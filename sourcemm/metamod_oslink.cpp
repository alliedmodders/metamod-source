/* ======== SourceMM ========
 * Copyright (C) 2004-2007 Metamod:Source Development Team
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

#include "metamod_oslink.h"
#include <malloc.h>
#ifdef __linux
#include <errno.h>
#include <stdio.h>
#endif

#if defined __WIN32__ || defined _WIN32 || defined WIN32
const char *dlerror()
{
	static char buf[1024];
	FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL, GetLastError(),
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), 
		(LPTSTR) &buf, 0, NULL);
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

