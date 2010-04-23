/**
 * vim: set ts=4 sw=4 tw=99 noet :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2009 AlliedModders LLC and authors.
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
#include "loader.h"
#include "serverplugin.h"
#include "gamedll.h"
#include "utility.h"
#include "valve_commandline.h"

#undef GetCommandLine

typedef ICommandLine *(*GetCommandLine)();

static HMODULE mm_library = NULL;
static char mm_fatal_logfile[PLATFORM_MAX_PATH] = "metamod-fatal.log";
MetamodBackend mm_backend = MMBackend_UNKNOWN;

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

static const char *backend_names[] =
{
	"1.ep1",
	"2.darkm",
	"2.ep2",
	"2.ep2v",
	"2.l4d",
	"2.l4d2"
};

#if defined _WIN32
#define LIBRARY_EXT		".dll"
#define LIBRARY_MINEXT	".dll"
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

	/* Replace server.dll with the new binary we want */
	mm_Format(&mm_path[len - temp_len],
			  sizeof(mm_path) - (len - temp_len),
			  "metamod.%s" LIBRARY_MINEXT,
			  backend_names[backend]);

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
	/* If we've got a VSP bridge, do nothing. */
	if (vsp_bridge != NULL)
	{
		if (ret != NULL)
			*ret = 1;
		return NULL;
	}

	void *ptr;
	if (strncmp(name, "ISERVERPLUGINCALLBACKS", 22) == 0)
	{
		/* Either load as VSP or start VSP listener */
		ptr = mm_GetVspCallbacks(atoi(&name[22]));
	}
	else if (gamedll_bridge == NULL)
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

#if defined _WIN32
#define TIER0_NAME			"bin\\tier0.dll"
#define VSTDLIB_NAME		"bin\\vstdlib.dll"
#elif defined __linux__
#define TIER0_NAME			"bin/" LIB_PREFIX "tier0" LIB_SUFFIX
#define VSTDLIB_NAME		"bin/" LIB_PREFIX "vstdlib" LIB_SUFFIX
#endif

const char *
mm_GetGameName()
{
	void *lib;
	char error[255];
	GetCommandLine valve_cmdline;
	char lib_path[PLATFORM_MAX_PATH];
	const char *game_name;

	if (!mm_ResolvePath(TIER0_NAME, lib_path, sizeof(lib_path)))
	{
		mm_LogFatal("Could not find path for: " TIER0_NAME);
		return NULL;
	}

	if ((lib = mm_LoadLibrary(lib_path, error, sizeof(error))) == NULL)
	{
		mm_LogFatal("Could not load %s: %s", lib_path, error);
		return NULL;
	}

	valve_cmdline = (GetCommandLine)mm_GetLibAddress(lib, "CommandLine_Tier0");
	if (valve_cmdline == NULL)
	{
		/* We probably have a Ship engine. */
		mm_UnloadLibrary(lib);
		if (!mm_ResolvePath(VSTDLIB_NAME, lib_path, sizeof(lib_path)))
		{
			mm_LogFatal("Could not find path for: " VSTDLIB_NAME);
			return NULL;
		}

		if ((lib = mm_LoadLibrary(lib_path, error, sizeof(error))) == NULL)
		{
			mm_LogFatal("Could not load %s: %s", lib_path, error);
			return NULL;
		}

		valve_cmdline = (GetCommandLine)mm_GetLibAddress(lib, "CommandLine");
	}

	mm_UnloadLibrary(lib);

	if (valve_cmdline == NULL)
	{
		mm_LogFatal("Could not locate any command line functionality");
		return NULL;
	}

	game_name = valve_cmdline()->ParmValue("-game");

	/* This probably means that the game directory is actually the current directory */
	if (!game_name)
	{
		game_name = ".";
	}

	return game_name;
}

MetamodBackend
mm_DetermineBackend(QueryValveInterface engineFactory, const char *game_name)
{
	/* Check for L4D */
	if (engineFactory("VEngineServer022", NULL) != NULL &&
		engineFactory("VEngineCvar007", NULL) != NULL)
	{
		if (engineFactory("VPrecacheSystem001", NULL) != NULL)
		{
			return MMBackend_Left4Dead2;
		}
		return MMBackend_Left4Dead;
	}
	else if (engineFactory("VEngineServer021", NULL) != NULL)
	{
		/* Check for OB */
		if (engineFactory("VEngineCvar004", NULL) != NULL &&
			engineFactory("VModelInfoServer002", NULL) != NULL)
		{
			if (strcmp(game_name, "tf") == 0 || strcmp(game_name, "dod") == 0)
			{
				return MMBackend_Episode2Valve;
			}
			return MMBackend_Episode2;
		}
		/* Check for Episode One/Old Engine */
		else if (engineFactory("VModelInfoServer001", NULL) != NULL &&
				 (engineFactory("VEngineCvar003", NULL) != NULL ||
				  engineFactory("VEngineCvar002", NULL) != NULL))
		{
			/* Check for Dark Messiah which has a weird directory structure */
			if (strcmp(game_name, ".") == 0)
			{
				return MMBackend_DarkMessiah;
			}
			return MMBackend_Episode1;
		}
	}

	return MMBackend_UNKNOWN;
}

