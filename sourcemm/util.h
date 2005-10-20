/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_UTIL_H
#define _INCLUDE_UTIL_H

/**
 * @brief Utility functons
 * @file util.h
 */

const char *UTIL_GetExtension(const char *file);
void UTIL_TrimComments(char *buffer);
void UTIL_TrimLeft(char *buffer);
void UTIL_TrimRight(char *buffer);
void UTIL_KeySplit(const char *str, char *buf1, size_t len1, char *buf2, size_t len2);
void UTIL_PathFmt(char *buffer, size_t len, const char *fmt, ...);
bool UTIL_PathCmp(const char *path1, const char *path2);

#endif //_INCLUDE_UTIL_H
