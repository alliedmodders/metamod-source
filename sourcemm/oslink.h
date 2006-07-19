/* ======== SourceMM ========
* Copyright (C) 2004-2006 Metamod:Source Development Team
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
	#if defined _MSC_VER && _MSC_VER >= 1400
		#undef ARRAYSIZE
	#else
		#define mkdir _mkdir
	#endif
	#include <windows.h>
	#include <io.h>
	#include <direct.h>
	#define		dlmount(x)		LoadLibrary(x)
	#define		dlsym(x, s)		GetProcAddress(x, s)
	#define		dlclose(x)		FreeLibrary(x)
	const char*	dlerror();
	#define	PATH_SEP_STR	"\\"
	#define PATH_SEP_CHAR	'\\'
	#define ALT_SEP_CHAR	'/'
	#define SERVER_DLL		"server.dll"
#elif defined __linux__
	#define OS_LINUX
	#include <dlfcn.h>
	#include <unistd.h>
	#include <sys/types.h>
	#include <dirent.h>
	#define		dlmount(x)		dlopen(x,RTLD_NOW)
	typedef		void*			HINSTANCE;
	#define	PATH_SEP_STR	"/"
	#define PATH_SEP_CHAR	'/'
	#define ALT_SEP_CHAR	'\\'
	#define	stricmp				strcasecmp
	#define SERVER_DLL		"server_i486.so"
#endif

#if defined __linux__
	extern int errno;
	int GetLastError();
#endif

bool GetFileOfAddress(void *pAddr, char *buffer, size_t maxlength);

#if defined __WIN32__ || defined _WIN32 || defined WIN32
	#define SMM_API extern "C" __declspec(dllexport)
#elif defined __GNUC__
	#define SMM_API	extern "C"
#endif

#if defined __WIN32__ || defined _WIN32 || defined WIN32
	typedef __int64				int64_t;
	typedef unsigned __int64	uint64_t;
#elif defined __GNUC__
#include <stdint.h>
#if !__GLIBC_HAVE_LONG_LONG
	typedef long long			int64_t;
	typedef unsigned long long	uint64_t;
#endif
#endif

#ifndef __linux__
	#define snprintf	_snprintf
	#define vsnprintf	_vsnprintf
#endif

#endif //_INCLUDE_OSLINK_H
