/**
 * vim: set ts=4 sw=4 tw=99 noet :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2010 AlliedModders LLC and authors.
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
#if defined __APPLE__
#include <crt_externs.h>
#endif

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
	"2.bgt",
	"2.eye",
	"2.css",
	"2.ep2v",
	"2.l4d",
	"2.l4d2",
	"2.swarm",
	"2.portal2",
	"2.csgo",
	"2.dota",
	"2.hl2dm",
	"2.dods",	
	"2.tf2",
	"2.nd",
	"2.sdk2013",
	"2.blade",
	"2.insurgency",
	"2.contagion",
	"2.bms",
	"2.doi",
};

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
#if __GNUC__ >= 4
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
		strncpy(buffer, ".", size);
	}
}

MetamodBackend
mm_DetermineBackend(QueryValveInterface engineFactory, QueryValveInterface serverFactory, const char *game_name)
{
	if (engineFactory("VEngineServer024", NULL) != NULL)
	{
		return MMBackend_DOTA;
	}
	else if (engineFactory("VEngineServer023", NULL) != NULL)
	{
		if (engineFactory("EngineTraceServer004", NULL) == NULL)
		{
			goto TF2branch;
		}

		if (engineFactory("IEngineSoundServer004", NULL) != NULL)
		{
			void *lib = (void *)serverFactory;
			void *addr;
			if (strcmp(game_name, "doi") == 0
				|| (addr = mm_FindPattern(lib, "doi_gamerules_data", sizeof("doi_gamerules_data") - 1)))
			{
				return MMBackend_DOI;
			}
			
			return MMBackend_Insurgency;
		}
		
		if (serverFactory("ServerGameDLL010", NULL) != NULL)
		{
			return MMBackend_BMS;
		}
		
		return MMBackend_CSGO;
	}
	else if (engineFactory("VEngineServer022", NULL) != NULL &&
		engineFactory("VEngineCvar007", NULL) != NULL)
	{
		if (engineFactory("EngineTraceServer004", NULL) != NULL)
		{
			if (engineFactory("XboxSystemInterface001", NULL) != NULL)
			{
				return MMBackend_AlienSwarm;
			}
			
			void *lib = (void *)serverFactory;
			void *addr;
			if (strcmp(game_name, "portal2") == 0
				|| (addr = mm_FindPattern(lib, "baseportalcombatweapon", sizeof("baseportalcombatweapon") - 1)))
			{
				return MMBackend_Portal2;
			}

			return MMBackend_Blade;
		}
		else if (engineFactory("VPrecacheSystem001", NULL) != NULL)
		{
			if (serverFactory("ServerGameTags002", NULL) != NULL)
			{
				return MMBackend_NuclearDawn;
			}
			else
			{
				void *lib = (void *)serverFactory;
				void *addr;
				if (strcmp(game_name, "contagion") == 0
					|| (addr = mm_FindPattern(lib, "Contagion_Chat_All", sizeof("Contagion_Chat_All") - 1)))
				{
					return MMBackend_Contagion;
				}
				else
				{
					return MMBackend_Left4Dead2;
				}
			}
		}

		return MMBackend_Left4Dead;
	}
	else if (engineFactory("VEngineServer021", NULL) != NULL)
	{
		/* Check for OB */
		if (engineFactory("VEngineCvar004", NULL) != NULL)
		{
			if (engineFactory("VModelInfoServer002", NULL) != NULL)
			{
				/* BGT has same iface version numbers and libs as ep2 */
				void *lib = (void *)serverFactory;
				void *addr;
				if (strcmp(game_name, "pm") == 0
					|| (addr = mm_FindPattern(lib, "DT_PMPlayerResource", sizeof("DT_PMPlayerResource") - 1)))
				{
					return MMBackend_BloodyGoodTime;
				}
				else
				{
					return MMBackend_Episode2;
				}
			}
			else if (engineFactory("VModelInfoServer003", NULL) != NULL)
			{
				if (engineFactory("VFileSystem017", NULL) != NULL)
				{
					return MMBackend_EYE;
				}
				else
				{
	TF2branch:
					void *lib = (void *)serverFactory;
					void *addr;
					if (strcmp(game_name, "cstrike") == 0
						|| (addr = mm_FindPattern(lib, "DT_CSPlayerResource", sizeof("DT_CSPlayerResource") - 1)))
					{
						return MMBackend_CSS;
					}
					else if (strcmp(game_name, "tf") == 0
						|| (addr = mm_FindPattern(lib, "DT_TFPlayerResource", sizeof("DT_TFPlayerResource") - 1)))
					{
						return MMBackend_TF2;
					}
					else if (strcmp(game_name, "dod") == 0
						|| (addr = mm_FindPattern(lib, "DT_DODPlayerResource", sizeof("DT_DODPlayerResource") - 1)))
					{
						return MMBackend_DODS;
					}
					else if (strcmp(game_name, "hl2mp") == 0)
					{
						return MMBackend_HL2DM;
					}
					else
					{
						return MMBackend_SDK2013;
					}
				}
			}
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

