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

#include <cctype>
#include <string.h>
#include <stdio.h>
#include <string>
#include <vector>
#include <algorithm>
#include "metamod_util.h"
#include "metamod_oslink.h"

#ifdef _WIN32
# include <io.h>
#else
# include <fcntl.h>
# include <unistd.h>
#endif

using namespace std::string_literals;

/**
 * @brief Utility functions
 * @file util.cpp
 */

const char *UTIL_GetExtension(const char *file)
{
	size_t len = strlen(file);
	size_t i = 0;

	for (i = len - 1; i + 1 > 0; i--)
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

// https://stackoverflow.com/a/217605
// trim from start (in place)
static inline void ltrim(std::string& s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) {
        return !std::isspace(ch);
        }));
}

// trim from end (in place)
static inline void rtrim(std::string& s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) {
        return !std::isspace(ch);
        }).base(), s.end());
}

void UTIL_TrimLeft(char *buffer)
{
    std::string s(buffer);
    ltrim(s);
    strcpy(buffer, s.c_str());
}

void UTIL_TrimRight(char *buffer)
{
    std::string s(buffer);
    rtrim(s);
    strcpy(buffer, s.c_str());
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
		if ((((unsigned)path1[pos1] & 0x80) && path1[pos1] != path2[pos2])
			||
			(!isalpha(path1[pos1]) && (path1[pos1] != path2[pos2]))
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

inline bool pathchar_isalpha(char a)
{
	return (((a & 1<<7) == 0) && isalpha(a));
}

inline bool pathchar_sep(char a)
{
#if defined WIN32
	return (a == '/' || a == '\\');
#else
	return (a == '/');
#endif
}

inline bool pathstr_isabsolute(const char *str)
{
#if defined WIN32
	return (pathchar_isalpha(str[0]) 
		&& str[1] == ':' 
		&& pathchar_sep(str[2]));
#else
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
bool UTIL_BadRelatize(char buffer[],
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
		len = snprintf(&buffer[total], maxlength - total, ".." PATH_SEP_STR);
		if (len >= maxlength - total)
		{
			/* Not enough space in the buffer */
			return false;
		}
		total += len;
	}

	/* Add the absolute path. */
	len = snprintf(&buffer[total], maxlength - total, "%s", &rootFrom[1]);
	if (len >= maxlength - total)
	{
		return false;
	}

	return true;
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

static bool ComparePathComponent(const std::string& a, const std::string& b) {
#ifdef _WIN32
	if (a.size() != b.size())
		return false;
	for (size_t i = 0; i < a.size(); i++) {
		if (!pathchar_cmp(a[i], b[i]))
			return false;
	}
	return true;
#else
	return a == b;
#endif
}

static std::vector<std::string> SplitPath(const char* path) {
	std::vector<std::string> parts;

	const char* iter = path;

#ifdef _WIN32
	if (isalpha(path[0]) && path[1] == ':' && pathchar_sep(path[2])) {
		// Append drive only (eg C:)
		parts.emplace_back(path, 2);
		iter += 2;
		while (pathchar_sep(*iter))
			iter++;
	}
#endif

	if (pathchar_sep(*iter)) {
		parts.emplace_back(PATH_SEP_STR);
		while (pathchar_sep(*iter))
			iter++;
	}

	while (*iter) {
		const char* start = iter;
		while (*iter && !pathchar_sep(*iter))
			iter++;
		if (iter != start)
			parts.emplace_back(start, iter - start);
		while (pathchar_sep(*iter))
			iter++;
	}
	return parts;
}

bool UTIL_Relatize2(char* buffer, size_t maxlen, const char* path1, const char* path2)
{
	auto parts1 = SplitPath(path1);
	auto parts2 = SplitPath(path2);

	// If this fails, paths were not relative or have different drives.
	if (parts1[0] != parts2[0])
		return false;

	// Skip past identical paths.
	size_t cursor = 1;
	while (true) {
		if (cursor >= parts1.size() || cursor >= parts2.size())
			break;
		if (!ComparePathComponent(parts1[cursor], parts2[cursor]))
			break;
		cursor++;
	}

	std::string new_path;
	for (size_t i = cursor; i < parts1.size(); i++)
		new_path += ".."s + PATH_SEP_STR;
	for (size_t i = cursor; i < parts2.size(); i++) {
		new_path += parts2[i];
		if (i != parts2.size() - 1)
			new_path += PATH_SEP_STR;
	}
	if (pathchar_sep(path2[strlen(path2) - 1]))
		new_path += PATH_SEP_STR;

	snprintf(buffer, maxlen, "%s", new_path.c_str());
	return true;
}

static inline bool PathExists(const char* path) {
#ifdef _WIN32
	return _access(path, 0) == 0 || errno != ENOENT;
#else
	return access(path, F_OK) == 0 || errno != ENOENT;
#endif
}

bool UTIL_Relatize(char buffer[], size_t maxlength, const char *relTo, const char *relFrom)
{
	if (UTIL_BadRelatize(buffer, maxlength, relTo, relFrom)) {
		if (PathExists(buffer))
			return true;
	}
	return UTIL_Relatize2(buffer, maxlength, relTo, relFrom);
}
