/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include "util.h"
#include "oslink.h"

/**
 * @brief Utility functons
 * @file util.cpp
 */

/* UTIL_GetExtension
 * Returns a pointer to the extension in a file name
 */
const char *UTIL_GetExtension(const char *file)
{
	int len = strlen(file);
	int i = 0;

	for (i=len-1; i>=0; i--)
	{
		if (file[i] == '/' || file[i] == '\\')
			return NULL;

		if ((file[i] == '.') && (i != len-1))
		{
			return (const char *)&(file[i+1]);
		}
	}

	return NULL;
}

/* UTIL_TrimLeft
 * Removes whitespace characters from left side of string
 */
void UTIL_TrimLeft(char *buffer)
{
	// Let's think of this as our iterator
	char *i = buffer;

	// Make sure the buffer isn't null
	if (i && *i)
	{
		// Add up number of whitespace characters
		while(isspace(*i))
			i++;

		// If whitespace chars in buffer then adjust string so first non-whitespace char is at start of buffer
		// :TODO: change this to not use memcpy()!
		if (i != buffer)
			memcpy(buffer, i, (strlen(i) + 1) * sizeof(char));
	}
}

//:TODO: this should skip string literals
void UTIL_TrimComments(char *buffer)
{
	int num_sc = 0;
	size_t len = strlen(buffer);
	if (buffer)
	{
		for (int i=(int)len-1; i>=0; i--)
		{
			if (buffer[i] == '/')
			{
				if (++num_sc >= 2 && i==0)
				{
					buffer[i] = '\0';
					return;
				}
			} else {
				if (num_sc >= 2)
				{
					buffer[i] = '\0';
					return;
				}
				num_sc = 0;
			}
			//size_t won't go below 0, manually break out
			if (i == 0)
				break;
		}
	}
}

/* UTIL_TrimLeft
 * Removes whitespace characters from right side of string
 */
void UTIL_TrimRight(char *buffer)
{
	// Make sure buffer isn't null
	if (buffer)
	{
		// Loop through buffer backwards while replacing whitespace chars with null chars
		for (int i = (int)strlen(buffer) - 1; i >= 0; i--)
		{
			if (isspace(buffer[i]))
				buffer[i] = '\0';
			else
				break;
		}
	}
}

/* UTIL_KeySplit
 * Breaks a string at the first space until it reaches a nonspace
 */
void UTIL_KeySplit(const char *str, char *buf1, size_t len1, char *buf2, size_t len2)
{
	size_t start;
	size_t len = strlen(str);

	for (start=0; start<len; start++)
	{
		if (!isspace(str[start]))
			break;
	}

	size_t end;
	for (end=start; end<len; end++)
	{
		if (isspace(str[end]))
			break;
	}
	
	size_t i, c=0;
	for (i=start; i<end; i++,c++)
	{
		if (c >= len1)
			break;
		buf1[c] = str[i];
	}
	buf1[c] = '\0';

	for (start=end; start<len; start++)
	{
		if (!isspace(str[start]))
			break;
	}

	for (c=0; start<len; start++,c++)
	{
		if (c >= len2)
			break;
		buf2[c] = str[start];
	}
	buf2[c] = '\0';
}

/** 
 * Formats a path name for an OS
 */
void UTIL_PathFmt(char *buffer, size_t len, const char *fmt, ...)
{
	va_list ap;
	va_start(ap,fmt);
	size_t mylen = vsnprintf(buffer, len, fmt, ap);
	va_end(ap);

	for (size_t i=0; i<mylen; i++)
	{
		if (buffer[i] == ALT_SEP_CHAR)
			buffer[i] = PATH_SEP_CHAR;
	}
}

bool UTIL_PathCmp(const char *path1, const char *path2)
{
	size_t len1=strlen(path1);
	size_t len2=strlen(path2);
	size_t pos1=0,pos2=0;

	while (true)
	{
		if (path1[pos1] == '\0' || path2[pos2] == '\0')
		{
			return (path1[pos1] == path2[pos2]);
		}

		if (path1[pos1] == PATH_SEP_CHAR)
		{
			if (path2[pos2] != PATH_SEP_CHAR)
				return false;
			//look for extra path chars
			while (path1[++pos1])
			{
				if (path1[pos1] != PATH_SEP_CHAR)
					break;
			}
			while (path2[++pos2])
			{
				if (path2[pos2] != PATH_SEP_CHAR)
					break;
			}
			continue;
		}

		//if we're at a different non-alphanumeric, the next character MUST Match
		if (!isalpha(path1[pos1]) && (path1[pos1] != path2[pos2]))
			return false;

#ifdef WIN32
		if (toupper(path1[pos1]) != toupper(path2[pos2]))
#else
		if (path1[pos1] != path2[pos2])
#endif
			return false;
		pos1++;
		pos2++;
	}
}
