#ifndef _INCLUDE_METAMOD_SOURCE_LOADER_H_
#define _INCLUDE_METAMOD_SOURCE_LOADER_H_

#define SH_COMP_GCC 	1
#define SH_COMP_MSVC	2

#if defined WIN32
#define WINDOWS_LEAN_AND_MEAN
#include <windows.h>
#include <direct.h>
#define PLATFORM_MAX_PATH	MAX_PATH
#define SH_COMP				SH_COMP_MSVC
#define	PATH_SEP_STR		"\\"
#define PATH_SEP_CHAR		'\\'
#define ALT_SEP_CHAR		'/'
#elif defined __linux__
#include <dlfcn.h>
#include <dirent.h>
#include <stdint.h>
#include <unistd.h>
typedef void *	HMODULE;
#define PLATFORM_MAX_PATH	PATH_MAX
#define SH_COMP				SH_COMP_GCC
#define	PATH_SEP_STR		"/"
#define PATH_SEP_CHAR		'/'
#define ALT_SEP_CHAR		'\\'
#else
#error "OS detection failed"
#endif

#include "loader_bridge.h"

#define SH_PTRSIZE sizeof(void*)

enum MetamodBackend
{
	MMBackend_Episode1 = 0,
	MMBackend_Episode2,
	MMBackend_Left4Dead,
	MMBackend_UNKNOWN
};

extern bool
mm_LoadMetamodLibrary(MetamodBackend backend, char *buffer, size_t maxlength);

extern void *
mm_GetProcAddress(const char *name);

extern void
mm_UnloadMetamodLibrary();

extern void
mm_LogFatal(const char *message, ...);

extern MetamodBackend
mm_DetermineBackend(QueryValveInterface qvi);

#endif /* _INCLUDE_METAMOD_SOURCE_LOADER_H_ */

