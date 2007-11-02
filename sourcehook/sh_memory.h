/* ======== SourceHook ========
* Copyright (C) 2004-2007 Metamod:Source Development Team
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

# if	/********/ defined _WIN32
#		include <windows.h>
#		define SH_MEM_READ 1
#		define SH_MEM_WRITE 2
#		define SH_MEM_EXEC 4
# elif /******/ defined __linux__
#		include <sys/mman.h>
#		include <stdio.h>
namespace LinuxSignal
{
#		include <signal.h>
}
#		include <setjmp.h>
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
#		define SH_LALIGN(x) (void*)((intptr_t)(x) & ~(PAGESIZE-1))
#		define SH_LALDIF(x) ((intptr_t)(x) & (PAGESIZE-1))
# else
#		error Unsupported OS/Compiler
# endif

#include "sh_list.h"

namespace SourceHook
{
	inline bool SetMemAccess(void *addr, size_t len, int access)
	{
# ifdef __linux__
		return mprotect(SH_LALIGN(addr), len + SH_LALDIF(addr), access)==0 ? true : false;
# else
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

#ifdef __linux__
	namespace
	{
		bool g_BadReadCalled;
		jmp_buf g_BadReadJmpBuf;

		static void BadReadHandler(int sig)
		{
			if (g_BadReadCalled)
				longjmp(g_BadReadJmpBuf, 1);
		}
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
		bool ModuleInMemory(char *addr, size_t len)
		{
#ifdef __linux__
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

			prevHandler = LinuxSignal::signal(SIGSEGV, BadReadHandler);

			volatile const char *p = reinterpret_cast<const char*>(addr);
			char dummy;

			for (size_t i = 0; i < len; i++)
				dummy = p[i];

			g_BadReadCalled = false;

			LinuxSignal::signal(SIGSEGV, prevHandler);

			return false;
#else
			// On Win32, simply use IsBadReadPtr
			return !IsBadReadPtr(addr, len);
#endif
		}
	}

	/*
		Class which lets us allocate memory regions in special pages only meant for on the fly code generation.

		If we alloc with malloc and then set the page access type to read/exec only, other regions returned by
		malloc that are in the same page would lose their write access as well and the process could crash.

		Allocating one page per code generation session is usually a waste of memory and on some plattforms also
		a waste of virtual address space (Windows’ VirtualAlloc has a granularity of 64K).


		IMPORTANT: the memory that Alloc() returns is not a in a defined state!
		It could be in read+exec OR read+write mode.
		-> call SetRE() or SetRW() before using allocated memory!
	*/
	class CPageAlloc
	{
		struct AllocationUnit
		{
			size_t begin_offset;
			size_t size;

			AllocationUnit(size_t p_offs, size_t p_size) : begin_offset(p_offs), size(p_size)
			{
			}

			bool operator < (const AllocationUnit &other) const
			{
				return begin_offset < other.begin_offset;
			}
		};
		
		typedef List<AllocationUnit> AUList;

		struct AllocatedRegion
		{
			void *startPtr;
			size_t size;
			bool isolated;					// may contain only one AU
			AUList allocUnits;

			bool TryAlloc(size_t reqsize, void * &outAddr)
			{
				// Check for isolated
				if (isolated && !allocUnits.empty())
					return false;

				// Find the smallest gap where req fits
				size_t lastend = 0;
				size_t smallestgap_pos = size + 1;
				size_t smallestgap_size = size + 1;

				for (AUList::iterator iter = allocUnits.begin(); iter != allocUnits.end(); ++iter)
				{
					if (iter->begin_offset - lastend >= reqsize)
					{
						if (iter->begin_offset - lastend < smallestgap_size)
						{
							smallestgap_size = iter->begin_offset - lastend;
							smallestgap_pos = lastend;
						}
					}
					lastend = iter->begin_offset + iter->size;
				}

				if (size - lastend >= reqsize)
				{
					if (size - lastend < smallestgap_size)
					{
						smallestgap_size = size - lastend;
						smallestgap_pos = lastend;
					}
				}

				if (smallestgap_pos < size)
				{
					outAddr = reinterpret_cast<void*>(reinterpret_cast<char*>(startPtr) + smallestgap_pos);
					allocUnits.push_sorted( AllocationUnit(smallestgap_pos, reqsize) );
					return true;
				}
				else
				{
					return false;
				}
			}

			bool TryFree(void *addr)
			{
				if (addr < startPtr || addr >= reinterpret_cast<void*>(reinterpret_cast<char*>(startPtr) + size))
					return false;

				size_t offs = reinterpret_cast<char*>(addr) - reinterpret_cast<char*>(startPtr);

				for (AUList::iterator iter = allocUnits.begin(); iter != allocUnits.end(); ++iter)
				{
					if (iter->begin_offset == offs)
					{
						allocUnits.erase(iter);
						return true;
					}
				}

				return false;
			}

			bool Contains(void *addr)
			{
				return addr >= startPtr && addr < reinterpret_cast<void*>(reinterpret_cast<char*>(startPtr) + size);
			}

			void FreeRegion()
			{
#ifdef __linux__
				munmap(startPtr, size);
#else
				VirtualFree(startPtr, 0, MEM_RELEASE);
#endif
			}
		};

		typedef List<AllocatedRegion> ARList;

		size_t m_PageSize;
		ARList m_Regions;

		bool AddRegion(size_t minSize, bool isolated)
		{
			AllocatedRegion newRegion;
			newRegion.startPtr = 0;
			newRegion.isolated = isolated;

			// Compute real size -> align up to m_PageSize boundary

			newRegion.size = minSize - (minSize % m_PageSize);
			if (newRegion.size < minSize)
				newRegion.size += m_PageSize;

#ifdef __linux__
			newRegion.startPtr = mmap(0, newRegion.size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
#else
			newRegion.startPtr = VirtualAlloc(NULL, newRegion.size, MEM_COMMIT, PAGE_READWRITE);
#endif
			
			if (newRegion.startPtr)
			{
				m_Regions.push_back(newRegion);
				return true;
			}
			else
			{
				return false;
			}
			
		}

		void *AllocPriv(size_t size, bool isolated)
		{
			void *addr;

			if (!isolated)
			{
				for (ARList::iterator iter = m_Regions.begin(); iter != m_Regions.end(); ++iter)
				{
					if (iter->TryAlloc(size, addr))
						return addr;
				}
			}

			if (!AddRegion(size, isolated))
				return NULL;
			
			bool tmp = m_Regions.back().TryAlloc(size, addr);
			SH_ASSERT(tmp, ("TryAlloc fails after AddRegion"));
			return addr;
		}

	public:
		CPageAlloc()
		{
#ifdef __linux__
			m_PageSize = sysconf(_SC_PAGESIZE);
#else
			SYSTEM_INFO sysInfo;
			GetSystemInfo(&sysInfo);
			m_PageSize = sysInfo.dwPageSize;
#endif
		}

		~CPageAlloc()
		{
			// Free all regions
			for (ARList::iterator iter = m_Regions.begin(); iter != m_Regions.end(); ++iter)
			{
				iter->FreeRegion();
			}
		}

		void *Alloc(size_t size)
		{
			return AllocPriv(size, false);
		}

		void *AllocIsolated(size_t size)
		{
			return AllocPriv(size, true);
		}

		void Free(void *ptr)
		{
			for (ARList::iterator iter = m_Regions.begin(); iter != m_Regions.end(); ++iter)
			{
				if (iter->TryFree(ptr))
				{
					if (iter->allocUnits.empty())
					{
						iter->FreeRegion();
						m_Regions.erase(iter);
					}
					break;
				}
			}
		}

		void SetRE(void *ptr)
		{
			for (ARList::iterator iter = m_Regions.begin(); iter != m_Regions.end(); ++iter)
			{
				if (iter->Contains(ptr))
				{
					SetMemAccess(iter->startPtr, iter->size, SH_MEM_READ | SH_MEM_EXEC);
					break;
				}
			}
		}

		void SetRW(void *ptr)
		{
			for (ARList::iterator iter = m_Regions.begin(); iter != m_Regions.end(); ++iter)
			{
				if (iter->Contains(ptr))
				{
					SetMemAccess(iter->startPtr, iter->size, SH_MEM_READ | SH_MEM_WRITE);
					break;
				}
			}
		}

		size_t GetPageSize()
		{
			return m_PageSize;
		}
	};
}

#endif
