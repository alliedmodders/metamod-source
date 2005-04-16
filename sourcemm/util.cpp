/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

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