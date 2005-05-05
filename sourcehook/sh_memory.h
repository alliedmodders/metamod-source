/* ======== SourceHook ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
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

}

#endif

