/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

/**
 * @brief Implements OS-dependant functions from oslink.h
 * @file oslink.cpp
 */

#include "oslink.h"
#ifdef __linux
#include <errno.h>
#endif

#if defined __WIN32__ || defined _WIN32 || defined WIN32
const char *dlerror()
{
	static char buf[1024];
	FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL, GetLastError(),
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), 
		(LPTSTR) &buf, 0, NULL);
	return buf;
}
#endif

#if defined __linux__
int GetLastError()
{
	return errno;
}
#endif
