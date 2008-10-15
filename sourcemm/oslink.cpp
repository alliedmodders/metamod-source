/* ======== SourceMM ========
 * Copyright (C) 2004-2008 Metamod:Source Development Team
 * No warranties of any kind
 *
 * License: zlib/libpng
 *
 * Author(s): David "BAILOPAN" Anderson
 * ============================
 */

/**
 * @brief Implements OS-dependant functions from oslink.h
 * @file oslink.cpp
 */

#include <stdio.h>
#include "oslink.h"
#ifdef __linux
#include <errno.h>
#endif

#if defined __WIN32__ || defined _WIN32 || defined WIN32
const char *dlerror()
{
	static char buf[1024];
	DWORD num;

	num = GetLastError();

	if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
						NULL,
						num,
						MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), 
						buf,
						sizeof(buf),
						NULL)
		== 0)
	{
		_snprintf(buf, sizeof(buf), "unknown error %x", num);
	}

	return buf;
}
#endif

#if defined __linux__
int GetLastError()
{
	return errno;
}

extern "C" void __cxa_guard_acquire(void)
{
}

extern "C" void __cxa_guard_release(void)
{
}
#endif

bool GetFileOfAddress(void *pAddr, char *buffer, size_t maxlength)
{
#if defined WIN32 || defined _WIN32
	MEMORY_BASIC_INFORMATION mem;
	if (!VirtualQuery(pAddr, &mem, sizeof(mem)))
		return false;
	if (mem.AllocationBase == NULL)
		return false;
	HMODULE dll = (HMODULE)mem.AllocationBase;
	GetModuleFileName(dll, (LPTSTR)buffer, maxlength);
#elif defined __linux__
	Dl_info info;
	if (!dladdr(pAddr, &info))
		return false;
	if (!info.dli_fbase || !info.dli_fname)
		return false;
	const char *dllpath = info.dli_fname;
	snprintf(buffer, maxlength, "%s", dllpath);
#endif
	return true;
}
