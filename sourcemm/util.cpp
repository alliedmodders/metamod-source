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
#include "util.h"

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
		if (i != buffer)
			memcpy(buffer, i, (strlen(i) + 1) * sizeof(char));
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
		for (unsigned int i = strlen(buffer) - 1; i >= 0; i--)
		{
			if (isspace(buffer[i]))
				buffer[i] = '\0';
			else
				break;
		}
	}
}

