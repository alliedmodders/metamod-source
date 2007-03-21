/* ======== SourceMM ========
 * Copyright (C) 2004-2007 Metamod:Source Development Team
 * No warranties of any kind
 *
 * License: zlib/libpng
 *
 * Author(s): David "BAILOPAN" Anderson
 * ============================
 */

#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include "util.h"
#include "oslink.h"

/**
 * @brief Utility functions
 * @file util.cpp
 */

const char *UTIL_GetExtension(const char *file)
{
	int len = strlen(file);
	int i = 0;

	for (i = len - 1; i >= 0; i--)
	{
		if (file[i] == '/' || file[i] == '\\')
		{
			return NULL;
		}

		if ((file[i] == '.') && (i != len - 1))
		{
			return (const char *)&(file[i + 1]);
		}
	}

	return NULL;
}

void UTIL_TrimLeft(char *buffer)
{
	/* Let's think of this as our iterator */
	char *i = buffer;

	/* Make sure the buffer isn't null */
	if (i && *i)
	{
		/* Add up number of whitespace characters */
		while(isspace((unsigned char) *i))
		{
			i++;
		}

		/* If whitespace chars in buffer then adjust string so first non-whitespace char is at start of buffer */
		if (i != buffer)
		{
			memmove(buffer, i, (strlen(i) + 1) * sizeof(char));
		}
	}
}

void UTIL_TrimRight(char *buffer)
{
	/* Make sure buffer isn't null */
	if (buffer)
	{
		size_t len = strlen(buffer);

		/* Loop through buffer backwards while replacing whitespace chars with null chars */
		for (size_t i = len - 1; i >= 0; i--)
		{
			if (isspace((unsigned char) buffer[i]))
			{
				buffer[i] = '\0';
			} else {
				break;
			}
		}
	}
}

/* :TODO: this should skip string literals */
void UTIL_TrimComments(char *buffer)
{
	int num_sc = 0;
	size_t len = strlen(buffer);
	if (buffer)
	{
		for (int i = len - 1; i >= 0; i--)
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
			/* size_t won't go below 0, manually break out */
			if (i == 0)
			{
				break;
			}
		}
	}
}

void UTIL_KeySplit(const char *str, char *buf1, size_t len1, char *buf2, size_t len2)
{
	size_t start;
	size_t len = strlen(str);

	for (start = 0; start < len; start++)
	{
		if (!isspace(str[start]))
		{
			break;
		}
	}

	size_t end;
	for (end = start; end < len; end++)
	{
		if (isspace(str[end]))
		{
			break;
		}
	}
	
	size_t i, c = 0;
	for (i = start; i < end; i++, c++)
	{
		if (c >= len1)
		{
			break;
		}
		buf1[c] = str[i];
	}
	buf1[c] = '\0';

	for (start = end; start < len; start++)
	{
		if (!isspace(str[start]))
		{
			break;
		}
	}

	for (c = 0; start < len; start++, c++)
	{
		if (c >= len2)
		{
			break;
		}
		buf2[c] = str[start];
	}
	buf2[c] = '\0';
}

bool UTIL_PathCmp(const char *path1, const char *path2)
{
	size_t pos1 = 0, pos2 = 0;

	while (true)
	{
		if (path1[pos1] == '\0' || path2[pos2] == '\0')
		{
			return (path1[pos1] == path2[pos2]);
		}

		if (path1[pos1] == PATH_SEP_CHAR)
		{
			if (path2[pos2] != PATH_SEP_CHAR)
			{
				return false;
			}

			/* Look for extra path chars */
			while (path1[++pos1])
			{
				if (path1[pos1] != PATH_SEP_CHAR)
				{
					break;
				}
			}
			while (path2[++pos2])
			{
				if (path2[pos2] != PATH_SEP_CHAR)
				{
					break;
				}
			}
			continue;
		}

		/* If we're at a different non-alphanumeric, the next character MUST match */
		if (!isalpha(path1[pos1]) && (path1[pos1] != path2[pos2]))
		{
			return false;
		}

	#ifdef WIN32
		if (toupper(path1[pos1]) != toupper(path2[pos2]))
	#else
		if (path1[pos1] != path2[pos2])
	#endif
		{
			return false;
		}

		pos1++;
		pos2++;
	}
}

size_t UTIL_Format(char *buffer, size_t maxlength, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	size_t len = vsnprintf(buffer, maxlength, fmt, ap);
	va_end(ap);

	if (len >= maxlength)
	{
		len = maxlength - 1;
		buffer[len] = '\0';
	}

	return len;
}
