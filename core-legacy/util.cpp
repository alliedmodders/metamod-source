/* ======== SourceMM ========
 * Copyright (C) 2004-2008 Metamod:Source Development Team
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
#include "sourcemm.h"
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
		for (size_t i = len - 1; i < len; i--)
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

inline bool pathchar_isalpha(char a)
{
	return (((a & 1<<7) == 0) && isalpha(a));
}

inline bool pathchar_sep(char a)
{
#if defined WIN32
	return (a == '/' || a == '\\');
#elif defined __linux__
	return (a == '/');
#endif
}

inline bool pathstr_isabsolute(const char *str)
{
#if defined WIN32
	return (pathchar_isalpha(str[0]) 
		&& str[1] == ':' 
		&& pathchar_sep(str[2]));
#elif defined __linux__
	return (str[0] == '/');
#endif
}

inline bool pathchar_cmp(char a, char b)
{
#if defined WIN32
	if (pathchar_isalpha(a) && pathchar_isalpha(b))
	{
		return (tolower(a) == tolower(b));
	}
	/* Either path separator is acceptable */
	if (pathchar_sep(a))
	{
		return pathchar_sep(b);
	}
#endif
	return (a == b);
}

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
				   const char *relFrom)
{
	/* We don't allow relative paths in here, force
	 * the user to resolve these himself!
	 */
	if (!pathstr_isabsolute(relTo)
		|| !pathstr_isabsolute(relFrom))
	{
		return false;
	}

#if defined WIN32
	/* Relative paths across drives are not possible */
	if (!pathchar_cmp(relTo[0], relFrom[0]))
	{
		return false;
	}
	/* Get rid of the drive and semicolon part */
	relTo = &relTo[2];
	relFrom = &relFrom[2];
#endif

	/* Eliminate the common root between the paths */
	const char *rootTo = NULL;
	const char *rootFrom = NULL;
	while (*relTo != '\0' && *relFrom != '\0')
	{
		/* If we get to a new path sequence, start over */
		if (pathchar_sep(*relTo)
			&& pathchar_sep(*relFrom))
		{
			rootTo = relTo;
			rootFrom = relFrom;
		/* If the paths don't compare, stop looking for a common root */
		} else if (!pathchar_cmp(*relTo, *relFrom)) {
			break;
		}
		relTo++;
		relFrom++;
	}

	/* NULLs shouldn't happen! */
	if (rootTo == NULL
		|| rootFrom == NULL)
	{
		return false;
	}

	size_t numLevels = 0;

	/* The root case is special! 
	 * Don't count anything from it.
	 */
	if (*(rootTo + 1) != '\0')
	{
		/* Search for how many levels we need to go up.
	 	 * Since the root pointer points to a '/', we increment
		 * the initial pointer by one.
		 */
		while (*rootTo != '\0')
		{
			if (pathchar_sep(*rootTo))
			{
				/* Check for an improper trailing slash,
				 * just to be nice even though the user 
				 * should NOT have done this!
				 */
				if (*(rootTo + 1) == '\0')
				{
					break;
				}
				numLevels++;
			}
			rootTo++;
		}
	}

	/* Now build the new relative path. */
	size_t len, total = 0;
	while (numLevels--)
	{
		len = _snprintf(&buffer[total], maxlength - total, ".." PATH_SEP_STR);
		if (len >= maxlength - total)
		{
			/* Not enough space in the buffer */
			return false;
		}
		total += len;
	}

	/* Add the absolute path. */
	len = _snprintf(&buffer[total], maxlength - total, "%s", &rootFrom[1]);
	if (len >= maxlength - total)
	{
		return false;
	}

	return true;
}

size_t UTIL_FormatArgs(char *buffer, size_t maxlength, const char *fmt, va_list params)
{
	size_t len = vsnprintf(buffer, maxlength, fmt, params);

	if (len >= maxlength)
	{
		len = maxlength - 1;
		buffer[len] = '\0';
	}

	return len;
}

bool UTIL_VerifySignature(const void *addr, const char *sig, size_t len)
{
	unsigned char *addr1 = (unsigned char *) addr;
	unsigned char *addr2 = (unsigned char *) sig;

	for (size_t i = 0; i < len; i++)
	{
		if (addr2[i] == '*')
			continue;
		if (addr1[i] != addr2[i])
			return false;
	}

	return true;
}
