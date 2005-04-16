/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_OSLINK_H
#define _INCLUDE_OSLINK_H

/**
 * @brief Defines OS-independent information
 * @file oslink.h
 */

#if defined __WIN32__ || defined _WIN32 || defined WIN32
	#define WIN32_LEAN_AND_MEAN
	#define OS_WIN32
	#include <windows.h>
	#include <io.h>
	#include <direct.h>
	#define mkdir(a) _mkdir(a)
	#define		dlmount(x)		LoadLibrary(x)
	#define		dlsym(x, s)		GetProcAddress(x, s)
	#define		dlclose(x)		FreeLibrary(x)
	const char*	dlerror();
#elif defined __linux__
	#define OS_LINUX
	#include <dlfcn.h>
	#include <unistd.h>
	#include <sys/types.h>
	#include <dirent.h>
	#define		dlmount(x)		dlopen(x,RTLD_NOW)
	typedef		void*			HINSTANCE;
#endif

#if defined __linux__
	extern int errno;
	int GetLastError();
#endif

#if defined __WIN32__ || defined _WIN32 || defined WIN32
	#define SMM_API extern "C" __declspec(dllexport)
#elif defined __GNUC__
	#define SMM_API	extern "C"
#endif

#if defined __WIN32__ || defined _WIN32 || defined WIN32
	typedef __int64				int64_t;
	typedef unsigned __int64	uint64_t;
#elif defined __GNUC__
# if !__GLIBC_HAVE_LONG_LONG
	typedef long long			int64_t;
# endif
	typedef unsigned long long	uint64_t;
#endif

#ifndef __linux__
	#define snprintf	_snprintf
	#define vsnprintf	_vsnprintf
#endif

#endif //_INCLUDE_OSLINK_H
