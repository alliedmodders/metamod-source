#ifndef _INCLUDE_METAMOD_SOURCE_LOADER_H_
#define _INCLUDE_METAMOD_SOURCE_LOADER_H_

#define SH_COMP_GCC 	1
#define SH_COMP_MSVC	2

#if defined WIN32
#define WINDOWS_LEAN_AND_MEAN
#include <windows.h>
#define PLATFORM_MAX_PATH	MAX_PATH
#define SH_COMP				SH_COMP_MSVC
#elif defined __linux__
#include <dlfcn.h>
#include <dirent.h>
#include <stdint.h>
typedef void *	HMODULE;
#define PLATFORM_MAX_PATH	PATH_MAX
#define SH_COMP				SH_COMP_GCC
#else
#error "OS detection failed"
#endif

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

#endif /* _INCLUDE_METAMOD_SOURCE_LOADER_H_ */

