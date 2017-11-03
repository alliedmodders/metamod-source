/**
 * vim: set ts=4 sw=4 tw=99 noet :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2009 AlliedModders LLC and authors.
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
 */

#ifndef _INCLUDE_METAMOD_SOURCE_LOADER_H_
#define _INCLUDE_METAMOD_SOURCE_LOADER_H_

// System
#define SH_SYS_WIN32	1
#define SH_SYS_LINUX	2
#define SH_SYS_APPLE	3

// Platform
#define SH_XP_POSIX		10
#define SH_XP_WINAPI	20

// Compiler
#define SH_COMP_GCC 	1
#define SH_COMP_MSVC	2

#if defined WIN32
#define SH_SYS              SH_SYS_WIN32
#define SH_XP               SH_XP_WINAPI
#define SH_COMP             SH_COMP_MSVC
#define WINDOWS_LEAN_AND_MEAN
#include <windows.h>
#include <direct.h>
#define PLATFORM_MAX_PATH	MAX_PATH
#define	PATH_SEP_STR		"\\"
#define PATH_SEP_CHAR		'\\'
#define ALT_SEP_CHAR		'/'
#elif defined __linux__ || defined __APPLE__
#if defined __linux__
#define SH_SYS				SH_SYS_LINUX
#elif defined __APPLE__
#define SH_SYS				SH_SYS_APPLE
#endif
#define SH_XP               SH_XP_POSIX
#define SH_COMP             SH_COMP_GCC
#include <dlfcn.h>
#include <dirent.h>
#include <stdint.h>
#include <unistd.h>
#if SH_SYS == SH_SYS_APPLE
#include <sys/syslimits.h>
#endif
typedef void *	HMODULE;
#define PLATFORM_MAX_PATH	PATH_MAX
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
	MMBackend_DarkMessiah,
	MMBackend_Episode2,
	MMBackend_BloodyGoodTime,
	MMBackend_EYE,
	MMBackend_CSS,
	MMBackend_Episode2Valve_OBSOLETE,
	MMBackend_Left4Dead,
	MMBackend_Left4Dead2,
	MMBackend_AlienSwarm,
	MMBackend_Portal2,
	MMBackend_CSGO,
	MMBackend_DOTA,
	MMBackend_HL2DM,
	MMBackend_DODS,
	MMBackend_TF2,
	MMBackend_NuclearDawn,
	MMBackend_SDK2013,
	MMBackend_Blade,
	MMBackend_Insurgency,
	MMBackend_Contagion,
	MMBackend_BMS,
	MMBackend_DOI,

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

extern void
mm_GetGameName(char *buffer, size_t size);

extern MetamodBackend
mm_DetermineBackend(QueryValveInterface engineFactory, QueryValveInterface serverFactory, const char *game_name);

extern MetamodBackend mm_backend;

#endif /* _INCLUDE_METAMOD_SOURCE_LOADER_H_ */

