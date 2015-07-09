/**
 * vim: set ts=4 sw=4 tw=99 noet :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2015 AlliedModders LLC and authors.
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
 */

#include <time.h>
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "loader2.h"
#include "gamedll.h"
#include "utility.h"
#if defined __APPLE__
#include <crt_externs.h>
#endif

static HMODULE mm_library = NULL;
static char mm_fatal_logfile[PLATFORM_MAX_PATH] = "metamod-fatal.log";
MetamodBackend mm_backend = MMBackend_Source2;

extern void
mm_LogFatal(const char *message, ...)
{
	FILE *fp;
	time_t t;
	va_list ap;
	char header[256];

	fp = fopen(mm_fatal_logfile, "at");
	if (!fp && (fp = fopen("metamod-fatal.log", "at")) == NULL)
		return;

	t = time(NULL);
	strftime(header, sizeof(header), "%m/%d/%Y - %H:%M:%S", localtime(&t));
	fprintf(fp, "L %s: ", header);
	
	va_start(ap, message);
	vfprintf(fp, message, ap);
	va_end(ap);

	fprintf(fp, "\n");

	fclose(fp);	
}

#if defined _WIN32
#define LIBRARY_EXT		".dll"
#define LIBRARY_MINEXT	".dll"
#elif defined __APPLE__
#define LIBRARY_EXT		".dylib"
#define LIBRARY_MINEXT	".dylib"
#elif defined __linux__
#define LIBRARY_EXT		LIB_SUFFIX
#define LIBRARY_MINEXT	".so"
#endif

bool
mm_LoadMetamodLibrary(MetamodBackend backend, char *buffer, size_t maxlength)
{
	size_t len, temp_len;
	char mm_path[PLATFORM_MAX_PATH * 2];

	/* Get our path */
	if (!mm_GetFileOfAddress((void*)mm_GetFileOfAddress, mm_path, sizeof(mm_path)))
		return false;

	len = strlen(mm_path);
	temp_len = strlen("server" LIBRARY_EXT);
	if (len < temp_len)
		return false;

	/* Build log file name */
	mm_path[len - temp_len] = '\0';
	mm_Format(mm_fatal_logfile,
			  sizeof(mm_fatal_logfile),
			  "%smetamod-fatal.log",
			  mm_path);

	/* Replace server2.dll with the new binary we want */
	mm_Format(&mm_path[len - temp_len],
			  sizeof(mm_path) - (len - temp_len),
			  "metamod.2.s2" LIBRARY_MINEXT);

	mm_library = (HMODULE)mm_LoadLibrary(mm_path, buffer, maxlength);

	return (mm_library != NULL);
}

void
mm_UnloadMetamodLibrary()
{
	mm_UnloadLibrary(mm_library);
	mm_library = NULL;
}

#if defined _WIN32
#define EXPORT extern "C" __declspec(dllexport)
#elif defined __GNUC__
#if __GNUC__ == 4
#define EXPORT extern "C" __attribute__ ((visibility("default")))
#else
#define EXPORT extern "C"
#endif
#endif

EXPORT void *
CreateInterface(const char *name, int *ret)
{
	void *ptr;
	if (gamedll_bridge == NULL)
	{
		/* Load as gamedll */
		ptr = mm_GameDllRequest(name, ret);
	}
	else
	{
		/* If we've got a gamedll bridge, forward the request. */
		return gamedll_bridge->QueryInterface(name, ret);
	}

	if (ret != NULL)
		*ret = (ptr != NULL) ? 0 : 1;

	return ptr;
}

void *
mm_GetProcAddress(const char *name)
{
	return mm_GetLibAddress(mm_library, name);
}

void
mm_GetGameName(char *buffer, size_t size)
{
	buffer[0] = '\0';

#if defined _WIN32
	static char game[128];

	LPWSTR pCmdLine = GetCommandLineW();
	int argc;
	LPWSTR *wargv = CommandLineToArgvW(pCmdLine, &argc);
	for (int i = 0; i < argc; ++i)
	{
		if (wcscmp(wargv[i], L"-game") != 0)
			continue;

		if (++i >= argc)
			break;

		wcstombs(buffer, wargv[i], size);
		buffer[size-1] = '\0';
		break;
	}

	LocalFree(wargv);

#elif defined __APPLE__
	int argc = *_NSGetArgc();
	char **argv = *_NSGetArgv();
	for (int i = 0; i < argc; ++i)
	{
		if (strcmp(argv[i], "-game") != 0)
			continue;

		if (++i >= argc)
			break;

		strncpy(buffer, argv[i], size);
		buffer[size-1] = '\0';
		break;
	}

#elif defined __linux__
	FILE *pFile = fopen("/proc/self/cmdline", "rb");
	if (pFile)
	{
		char *arg = NULL;
		size_t argsize = 0;
		bool bNextIsGame = false;

		while (getdelim(&arg, &argsize, 0, pFile) != -1)
		{
			if (bNextIsGame)
			{
				strncpy(buffer, arg, size);
				buffer[size-1] = '\0';
				break;
			}

			if (strcmp(arg, "-game") == 0)
			{
				bNextIsGame = true;
			}
		}

		free(arg);
		fclose(pFile);
	}
#else
#error unsupported platform
#endif

	if (buffer[0] == 0)
	{
		strncpy(buffer, "dota", size);
	}
}

