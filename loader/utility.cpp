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

#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>
#include "loader.h"
#include "utility.h"

#if defined _WIN32
static void
mm_GetPlatformError(char *buffer, size_t maxlength)
{
	DWORD dw = GetLastError();
	FormatMessageA(
		FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		dw,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPSTR)buffer,
		maxlength,
		NULL);
}
#endif


size_t
mm_FormatArgs(char *buffer, size_t maxlength, const char *fmt, va_list params)
{
	size_t len = vsnprintf(buffer, maxlength, fmt, params);

	if (len >= maxlength)
	{
		len = maxlength - 1;
		buffer[len] = '\0';
	}

	return len;
}

size_t
mm_Format(char *buffer, size_t maxlength, const char *fmt, ...)
{
	size_t len;
	va_list ap;

	va_start(ap, fmt);
	len = mm_FormatArgs(buffer, maxlength, fmt, ap);
	va_end(ap);

	return len;
}

size_t
mm_PathFormat(char *buffer, size_t maxlen, const char *fmt, ...)
{
	size_t len;
	va_list ap;

	va_start(ap, fmt);
	len = mm_FormatArgs(buffer, maxlen, fmt, ap);
	va_end(ap);

	for (size_t i = 0; i < len; i++)
	{
		if (buffer[i] == ALT_SEP_CHAR)
			buffer[i] = PATH_SEP_CHAR;
	}

	return len;
}

void
mm_TrimLeft(char *buffer)
{
	/* Let's think of this as our iterator */
	char *i = buffer;

	/* Make sure the buffer isn't null */
	if (i && *i)
	{
		/* Add up number of whitespace characters */
		while(isspace((unsigned char) *i))
			i++;

		/* If whitespace chars in buffer then adjust string so first non-whitespace char is at start of buffer */
		if (i != buffer)
			memmove(buffer, i, (strlen(i) + 1) * sizeof(char));
	}
}

void
mm_TrimRight(char *buffer)
{
	/* Make sure buffer isn't null */
	if (buffer)
	{
		size_t len = strlen(buffer);

		/* Loop through buffer backwards while replacing whitespace chars with null chars */
		for (size_t i = len - 1; i >= 0; i--)
		{
			if (isspace((unsigned char) buffer[i]))
				buffer[i] = '\0';
			else
				break;
		}
	}
}

/* :TODO: this should skip string literals */
void
mm_TrimComments(char *buffer)
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
			}
			else
			{
				if (num_sc >= 2)
				{
					buffer[i] = '\0';
					return;
				}
				num_sc = 0;
			}
			/* size_t won't go below 0, manually break out */
			if (i == 0)
				break;
			
		}
	}
}

void
mm_KeySplit(const char *str, char *buf1, size_t len1, char *buf2, size_t len2)
{
	size_t start;
	size_t len = strlen(str);

	for (start = 0; start < len; start++)
	{
		if (!isspace(str[start]))
			break;
	}

	size_t end;
	for (end = start; end < len; end++)
	{
		if (isspace(str[end]))
			break;
	}
	
	size_t i, c = 0;
	for (i = start; i < end; i++, c++)
	{
		if (c >= len1)
			break;
		buf1[c] = str[i];
	}
	buf1[c] = '\0';

	for (start = end; start < len; start++)
	{
		if (!isspace(str[start]))
			break;
	}

	for (c = 0; start < len; start++, c++)
	{
		if (c >= len2)
			break;
		buf2[c] = str[start];
	}
	buf2[c] = '\0';
}

bool
mm_PathCmp(const char *path1, const char *path2)
{
	size_t pos1 = 0, pos2 = 0;

	while (true)
	{
		if (path1[pos1] == '\0' || path2[pos2] == '\0')
			return (path1[pos1] == path2[pos2]);

		if (path1[pos1] == PATH_SEP_CHAR)
		{
			if (path2[pos2] != PATH_SEP_CHAR)
				return false;

			/* Look for extra path chars */
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

		/* If we're at a different non-alphanumeric, the next character MUST match */
		if ((((unsigned)path1[pos1] & 0x80) && path1[pos1] != path2[pos2])
			||
			!isalpha(path1[pos1]) && (path1[pos1] != path2[pos2])
			)
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

bool
mm_ResolvePath(const char *path, char *buffer, size_t maxlength)
{
#if defined _WIN32
	return _fullpath(buffer, path, maxlength) != NULL;
#elif defined __linux__ || defined __APPLE__
	assert(maxlength >= PATH_MAX);
	return realpath(path, buffer) != NULL;
#endif
}

void *
mm_LoadLibrary(const char *path, char *buffer, size_t maxlength)
{
	void *lib;

#if defined _WIN32
	lib = (void*)LoadLibrary(path);

	if (lib == NULL)
	{
		mm_GetPlatformError(buffer, maxlength);
		return NULL;
	}
#elif defined __linux__ || defined __APPLE__
	lib = dlopen(path, RTLD_NOW);

	if (lib == NULL)
	{
		mm_Format(buffer, maxlength, "%s", dlerror());
		return NULL;
	}
#endif

	return lib;
}

void *
mm_GetLibAddress(void *lib, const char *name)
{
#if defined _WIN32
	return GetProcAddress((HMODULE)lib, name);
#elif defined __linux__ || defined __APPLE__
	return dlsym(lib, name);
#endif
}

void
mm_UnloadLibrary(void *lib)
{
#if defined _WIN32
	FreeLibrary((HMODULE)lib);
#elif defined __linux__ || defined __APPLE__
	dlclose(lib);
#endif
}

bool
mm_GetFileOfAddress(void *pAddr, char *buffer, size_t maxlength)
{
#if defined _WIN32
	MEMORY_BASIC_INFORMATION mem;
	if (!VirtualQuery(pAddr, &mem, sizeof(mem)))
		return false;
	if (mem.AllocationBase == NULL)
		return false;
	HMODULE dll = (HMODULE)mem.AllocationBase;
	GetModuleFileName(dll, (LPTSTR)buffer, maxlength);
#elif defined __linux__ || defined __APPLE__
	Dl_info info;
	if (!dladdr(pAddr, &info))
		return false;
	if (!info.dli_fbase || !info.dli_fname)
		return false;
	const char *dllpath = info.dli_fname;
	snprintf(buffer, maxlength, "%s", dllpath);
#endif
	return true;
}

