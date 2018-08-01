/* ======== SourceHook ========
* Copyright (C) 2004-2010 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko, Scott "Damaged Soul" Ehlert
* Contributors: lancevorgin, XAD, theqizmo
* ============================
*/

#ifndef __SHINT_MEMORY_H__
#define __SHINT_MEMORY_H__

// Feb 17 / 2005:
//  Unprotect now sets to readwrite
//  The vtable doesn't need to be executable anyway

# if SH_XP == SH_XP_WINAPI
#		include <windows.h>
#		define SH_MEM_READ 1
#		define SH_MEM_WRITE 2
#		define SH_MEM_EXEC 4
# elif SH_XP == SH_XP_POSIX
#		include <sys/mman.h>
#		include <stdio.h>
#		include <signal.h>
#		include <setjmp.h>
#		include <stdint.h>
// http://www.die.net/doc/linux/man/man2/mprotect.2.html
#		include <limits.h>
#		ifndef PAGESIZE
#			define PAGESIZE 4096
#		endif
#		define SH_MEM_READ PROT_READ
#		define SH_MEM_WRITE PROT_WRITE
#		define SH_MEM_EXEC PROT_EXEC

// We need to align addr down to pagesize on linux
// We assume PAGESIZE is a power of two
#		define SH_LALIGN(x) (void*)((uintptr_t)(x) & ~(PAGESIZE-1))
#		define SH_LALDIF(x) ((uintptr_t)(x) % PAGESIZE)
# else
#		error Unsupported OS/Compiler
# endif
#if SH_SYS == SH_SYS_APPLE
# include <mach/mach.h>
# include <mach/vm_region.h>
# include <mach/mach_traps.h>
#endif

#include "sh_list.h"

namespace SourceHook
{
	static inline bool GetPageBits(void *addr, int *bits)
	{
#if SH_SYS == SH_SYS_LINUX
		// On linux, first check /proc/self/maps
		unsigned long laddr = reinterpret_cast<unsigned long>(addr);

		FILE *pF = fopen("/proc/self/maps", "r");
		if (pF) {
			// Linux /proc/self/maps -> parse
			// Format:
			// lower    upper    prot     stuff                 path
			// 08048000-0804c000 r-xp 00000000 03:03 1010107    /bin/cat
			unsigned long rlower, rupper;
			char r, w, x;
			while (fscanf(pF, "%lx-%lx %c%c%c", &rlower, &rupper, &r, &w, &x) != EOF) {
				// Check whether we're IN THERE!
				if (laddr >= rlower && laddr < rupper) {
					fclose(pF);
					*bits = 0;
					if (r == 'r')
						*bits |= SH_MEM_READ;
					if (w == 'w')
						*bits |= SH_MEM_WRITE;
					if (x == 'x')
						*bits |= SH_MEM_EXEC;
					return true;
				}
				// Read to end of line
				int c;
				while ((c = fgetc(pF)) != '\n') {
					if (c == EOF)
						break;
				}
				if (c == EOF)
					break;
			}
			fclose(pF);
			return false;
		}
		pF = fopen("/proc/curproc/map", "r");
		if (pF) {
			// FreeBSD /proc/curproc/map -> parse
			// 0x804800 0x805500 13 15 0xc6e18960 r-x 21 0x0 COW NC vnode
			unsigned long rlower, rupper, ignoreLong;
			int ignoreInt;
			char r, w, x;
			while (fscanf(pF, "0x%lx 0x%lx %d %d 0x%lx %c%c%c", &rlower, &rupper, &ignoreInt,
						  &ignoreInt, &ignoreLong, &r, &w, &x) != EOF) {
				// Check whether we're IN THERE!
				if (laddr >= rlower && laddr < rupper) {
					fclose(pF);
					*bits = 0;
					if (r == 'r')
						*bits |= SH_MEM_READ;
					if (r == 'w')
						*bits |= SH_MEM_WRITE;
					if (r == 'x')
						*bits |= SH_MEM_EXEC;
					return true;
				}
				// Read to end of line
				int c;
				while ((c = fgetc(pF)) != '\n') {
					if (c == EOF)
						break;
				}
				if (c == EOF)
					break;
			}
			fclose(pF);
			return false;
		}
		return false;
#elif SH_SYS == SH_SYS_APPLE
		vm_size_t ignoreSize;
		vm_address_t vmaddr = (vm_address_t)addr;
		vm_region_basic_info_data_t info;
		vm_region_flavor_t flavor = VM_REGION_BASIC_INFO;
		memory_object_name_t obj;

		mach_msg_type_number_t count = VM_REGION_BASIC_INFO_COUNT;
		kern_return_t kr = vm_region(mach_task_self(), &vmaddr, &ignoreSize, flavor,
		                             (vm_region_info_t)&info, &count, &obj);
		if (kr != KERN_SUCCESS)
			return false;
		*bits = 0;
		if (info.protection & VM_PROT_READ)
			*bits |= SH_MEM_READ;
		if (info.protection & VM_PROT_WRITE)
			*bits |= SH_MEM_WRITE;
		if (info.protection & VM_PROT_EXECUTE)
			*bits |= SH_MEM_EXEC;
		return true;
#elif SH_XP == SH_XP_WINAPI
		SYSTEM_INFO info;
		GetSystemInfo(&info);

		MEMORY_BASIC_INFORMATION mem;
		size_t base = size_t(addr) & ~size_t(info.dwPageSize - 1);
		if (!VirtualQuery((void *)base, &mem, sizeof(mem)))
			return false;
		switch (mem.Protect) {
			case PAGE_EXECUTE:
				*bits = SH_MEM_EXEC;
				break;
			case PAGE_EXECUTE_READ:
				*bits = SH_MEM_EXEC | SH_MEM_READ;
				break;
			case PAGE_EXECUTE_READWRITE:
			case PAGE_EXECUTE_WRITECOPY:
				*bits = SH_MEM_EXEC | SH_MEM_READ | SH_MEM_WRITE;
				break;
			case PAGE_NOACCESS:
				*bits = 0;
				break;
			case PAGE_READONLY:
				*bits = SH_MEM_READ;
				break;
			case PAGE_READWRITE:
			case PAGE_WRITECOPY:
				*bits = SH_MEM_READ | SH_MEM_WRITE;
				break;
			default:
				return false;
		}
		return true;
#endif
	}

	inline bool SetMemAccess(void *addr, size_t len, int access)
	{
# if SH_XP == SH_XP_POSIX
		return mprotect(SH_LALIGN(addr), len + SH_LALDIF(addr), access)==0 ? true : false;
# elif SH_XP == SH_XP_WINAPI
		DWORD tmp;
		DWORD prot;
		switch (access)
		{
		case SH_MEM_READ:
			prot = PAGE_READONLY; break;
		case SH_MEM_READ | SH_MEM_WRITE:
			prot = PAGE_READWRITE; break;
		case SH_MEM_READ | SH_MEM_EXEC:
			prot = PAGE_EXECUTE_READ; break;
		default:
		case SH_MEM_READ | SH_MEM_WRITE | SH_MEM_EXEC:
			prot = PAGE_EXECUTE_READWRITE; break;
		}
		return VirtualProtect(addr, len, prot, &tmp) ? true : false;
# endif
	}

	inline bool MakePageWritable(void *addr)
	{
		int bits;
		if (GetPageBits(addr, &bits)) {
			if (bits & SH_MEM_WRITE)
				return true;
			bits |= SH_MEM_WRITE;
		} else {
			bits = SH_MEM_READ | SH_MEM_WRITE | SH_MEM_EXEC;
		}
		return SetMemAccess(addr, sizeof(void *), bits);
	}

#if SH_XP == SH_XP_POSIX
	namespace
	{
		bool g_BadReadCalled;
		jmp_buf g_BadReadJmpBuf;

# if SH_SYS == SH_SYS_LINUX
		static void BadReadHandler(int sig)
		{
			if (g_BadReadCalled)
				longjmp(g_BadReadJmpBuf, 1);
		}
# elif SH_SYS == SH_SYS_APPLE
		static void BadReadHandler(int signal, siginfo_t* my_siginfo, void* my_context)	
		{
			if (g_BadReadCalled)
				longjmp(g_BadReadJmpBuf, 1);
		}
# endif
	}
#endif

	/**
	*	@brief Checks whether the specified memory region is (still) accessible
	*
	*	@param addr The lower boundary
	*	@param len Length of the region to be checked
	*/
	namespace
	{
		static inline bool ModuleInMemory(char *addr, size_t len)
		{
#if SH_SYS == SH_SYS_LINUX
			// On linux, first check /proc/self/maps
			long lower = reinterpret_cast<long>(addr);
			long upper = lower + len;

			FILE *pF = fopen("/proc/self/maps", "r");
			if (pF)
			{
				// Linux /proc/self/maps -> parse
				// Format:
				// lower    upper    prot     stuff                 path
				// 08048000-0804c000 r-xp 00000000 03:03 1010107    /bin/cat
				long rlower, rupper;
				while (fscanf(pF, "%lx-%lx", &rlower, &rupper) != EOF)
				{
					// Check whether we're IN THERE!
					if (lower >= rlower && upper <= rupper)
					{
						fclose(pF);
						return true;
					}
					// Read to end of line
					int c;
					while ((c = fgetc(pF)) != '\n')
					{
						if (c == EOF)
							break;
					}
					if (c == EOF)
						break;
				}
				fclose(pF);
				return false;
			}
			pF = fopen("/proc/curproc/map", "r");
			if (pF)
			{
				// FreeBSD /proc/curproc/map -> parse
				// 0x804800 0x805500 13 15 0xc6e18960 r-x 21 0x0 COW NC vnode
				long rlower, rupper;
				while (fscanf(pF, "0x%lx 0x%lx", &rlower, &rupper) != EOF)
				{
					// Check whether we're IN THERE!
					if (lower >= rlower && upper <= rupper)
					{
						fclose(pF);
						return true;
					}
					// Read to end of line
					int c;
					while ((c = fgetc(pF)) != '\n')
					{
						if (c == EOF)
							break;
					}
					if (c == EOF)
						break;
				}
				fclose(pF);
				return false;
			}

			// Both of the above failed, try to actually read and trap sigsegv (implemented by Damaged Soul)
			void(*prevHandler)(int sig);
			g_BadReadCalled = true;

			if (setjmp(g_BadReadJmpBuf))
				return true;

			prevHandler = signal(SIGSEGV, BadReadHandler);

			volatile const char *p = reinterpret_cast<const char*>(addr);
			char dummy;

			for (size_t i = 0; i < len; i++)
				dummy = p[i];
			(void)dummy; // silence unused var, we must read from p

			g_BadReadCalled = false;

			signal(SIGSEGV, prevHandler);

			return false;
#elif SH_SYS == SH_SYS_APPLE
			struct sigaction sa, osa;
			sa.sa_sigaction = BadReadHandler;
			sa.sa_flags = SA_SIGINFO | SA_RESTART;

			g_BadReadCalled = true;

			if (setjmp(g_BadReadJmpBuf))
				return false;

			if (sigaction(SIGBUS, &sa, &osa) == -1)
				return false;

			volatile const char *p = reinterpret_cast<const char *>(addr);
			char dummy;

			for (size_t i = 0; i < len; i++)
				dummy = p[i];
			(void)dummy; // silence unused var, we must read from p

			g_BadReadCalled = false;

			sigaction(SIGBUS, &osa, NULL);

			return true;
#elif SH_XP == SH_XP_WINAPI
			// On Win32, simply use IsBadReadPtr
			return !IsBadReadPtr(addr, len);
#endif
		}
	}
}

#endif
