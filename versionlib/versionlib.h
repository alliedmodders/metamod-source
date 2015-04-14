/**
 * vim: set ts=4 sw=4 tw=99 noet :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2010 AlliedModders LLC and authors.
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
#ifndef _INCLUDE_METAMOD_VERSIONLIB_H_
#define _INCLUDE_METAMOD_VERSIONLIB_H_

#if !defined(MMS_USE_VERSIONLIB)
// These get defined in metamod_version.h since
// versionlib does not use versionlib.
# undef METAMOD_LOCAL_REV
# undef METAMOD_CSET
# undef METAMOD_VERSION
# undef METAMOD_BUILD_TIME
#endif

#ifdef __cplusplus
# define EXTERN_C extern "C"
#else
# define EXTERN_C extern
#endif
EXTERN_C const char *METAMOD_LOCAL_REV;
EXTERN_C const char *METAMOD_SHA;
EXTERN_C const char *METAMOD_VERSION;
EXTERN_C const char *METAMOD_BUILD_TIME;

#endif // _INCLUDE_METAMOD_VERSIONLIB_H_
