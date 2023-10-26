/**
 * vim: set ts=4 :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2015 AlliedModders LLC and authors.
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

#ifndef _INCLUDE_METAMOD_SOURCE_LOADER_UTILITY_H_
#define _INCLUDE_METAMOD_SOURCE_LOADER_UTILITY_H_

#include <stddef.h>

extern size_t
mm_Format(char *buffer, size_t maxlength, const char *fmt, ...);

extern void *
mm_LoadLibrary(const char *path, char *buffer, size_t maxlength);

extern void *
mm_GetLibAddress(void *lib, const char *name);

extern void
mm_UnloadLibrary(void *lib);

extern bool
mm_ResolvePath(const char *path, char *buffer, size_t maxlength, bool bSource2);

extern size_t
mm_PathFormat(char *buffer, size_t len, const char *fmt, ...);

extern void
mm_TrimLeft(char *buffer);

extern void
mm_TrimRight(char *buffer);

extern void
mm_TrimComments(char *buffer);

extern void
mm_KeySplit(const char *str, char *buf1, size_t len1, char *buf2, size_t len2);

extern bool
mm_PathCmp(const char *path1, const char *path2);

extern bool
mm_GetFileOfAddress(void *pAddr, char *buffer, size_t maxlength);

extern void *
mm_FindPattern(const void *libPtr, const char *pattern, size_t len);

// True if arg is present, false if not. If arg has no value, buffer will be set to an empty string.
extern bool
mm_GetCommandArgument(const char *argName, char *buffer = nullptr, size_t maxlength = 0);

#endif /* _INCLUDE_METAMOD_SOURCE_LOADER_UTILITY_H_ */

