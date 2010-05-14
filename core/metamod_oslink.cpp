/**
 * vim: set ts=4 :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2008 AlliedModders LLC and authors.
 * All rights reserved.
 * ======================================================
 *
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the authors be held liable for any damages arising from 
 * the use of this software.
 * 
 * Permission is granted to anyone to use this software for any purpose, 
 * including commercial applications, and to alter it and redistribute it 
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not 
 * claim that you wrote the original software. If you use this software in a 
 * product, an acknowledgment in the product documentation would be 
 * appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 * misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 * Version: $Id$
 */

#include <stdio.h>
#include "metamod_oslink.h"
#include <stdlib.h>
#include <stdio.h>
#if defined __linux__ || defined __APPLE__
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

#if defined __linux_ || defined __APPLE___
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
#elif defined __linux__ || defined __APPLE__
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

#if defined __GNUC__ && (__GNUC__ == 3 || __GNUC__ == 4)
void * operator new(size_t size) {
	return malloc(size);
}

void * operator new[](size_t size) {
	return malloc(size);
}

void operator delete(void * ptr) {
	free(ptr);
}

void operator delete[](void * ptr) {
	free(ptr);
}
#endif

