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

#ifndef _INCLUDE_UTIL_H
#define _INCLUDE_UTIL_H

#include <stdarg.h>

/**
 * @brief Utility functions
 * @file util.h
 */

/**
 * @brief Returns true is string is not blank, false otherwise.
 */
#define IS_STR_FILLED(var) (var != NULL && var[0] != '\0')

/**
 * @brief Returns a pointer to the extension in a file name.
 */
const char *UTIL_GetExtension(const char *file);

/**
 * @brief Removes whitespace characters from left side of string.
 */
void UTIL_TrimLeft(char *buffer);

/**
 * @brief Removes whitespace characters from right side of string.
 */
void UTIL_TrimRight(char *buffer);

/**
 * @brief Compares two file paths.
 */
bool UTIL_PathCmp(const char *path1, const char *path2);

/**
 * @brief Same as snprintf except that it ensures the string buffer is null terminated.
 */
size_t UTIL_Format(char *buffer, size_t maxlength, const char *fmt, ...);

/**
 * @brief Same as vsnprintf except that it ensures the string buffer is null terminated.
 */
size_t UTIL_FormatArgs(char *buffer, size_t maxlength, const char *fmt, va_list params);

/**
 * @brief Forms a relative path given two absolute paths.
 *
 * @param buffer		Buffer to store relative path in.
 * @param maxlength		Maximum length of the output buffer.
 * @param relTo			Destination folder to use as a working directory.
 *						Final folder name should not be pathchar-terminated.
 * @param relFrom		Source file or folder to use as a target.
 * @return				True on success, false on failure.
 */
bool UTIL_Relatize(char buffer[],
				   size_t maxlength,
				   const char *relTo,
				   const char *relFrom);

/**
 * @brief Compares memory address against a signature.
 *
 * @param addr			Memory address to check.
 * @param sig			Signature used to check against memory address. Accept 0x2A as wildcard.
 * @param len			Length of signature.
 * @return				True if signature was verified, false otherwise.
 */
bool UTIL_VerifySignature(const void *addr, const char *sig, size_t len);

#endif //_INCLUDE_UTIL_H

