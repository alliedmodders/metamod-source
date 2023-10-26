/**
 * vim: set ts=4 sw=4 tw=99 noet :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2023 AlliedModders LLC and authors.
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
#include <cstdint>
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

	printf("MMS: Fatal error: ");
	va_start(ap, message);
	vprintf(message, ap);
	va_end(ap);
	printf("\n");

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
	"2.ep1",
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
	"2.mock",
	"2.pvkii",
	"2.mcv",
	"2.cs2",
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
	
	const char *pLastSlash = strrchr(mm_path, PATH_SEP_CHAR);
	if (!pLastSlash)
		return false;

	temp_len = strlen(&pLastSlash[1]);
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
#define EXPORT extern "C" __attribute__ ((visibility("default")))
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

typedef const char *(*GetGameInfoStringFn)(const char *pszKeyName, const char *pszDefaultValue, char *pszOut, uint64_t cbOut);

void
mm_GetGameName(char *buffer, size_t size)
{
	if (!mm_GetCommandArgument("-game", buffer, size))
	{
		char tier0_path[PLATFORM_MAX_PATH];
#ifdef _WIN32
		if (mm_ResolvePath("tier0.dll", tier0_path, sizeof(tier0_path), false))
#elif defined __linux__
		if (mm_ResolvePath("libtier0.so", tier0_path, sizeof(tier0_path), false))
#elif defined __APPLE__
		if (mm_ResolvePath("libtier0.dylib", tier0_path, sizeof(tier0_path), false))
#else
#error unsupported platform
#endif
		{
			char err[1024];
			void* pTier0 = mm_LoadLibrary(tier0_path, err, sizeof(err));
			if (pTier0)
			{
#ifdef _WIN32
				GetGameInfoStringFn func = (GetGameInfoStringFn)mm_GetLibAddress(pTier0, "?GetGameInfoString@@YAPEBDPEBD0PEAD_K@Z");
#else
				GetGameInfoStringFn func = (GetGameInfoStringFn)mm_GetLibAddress(pTier0, "_Z17GetGameInfoStringPKcS0_Pcm");
#endif
				if (func != nullptr)
				{
					static char szTmp[260];
					strncpy(buffer, func("FileSystem/SearchPaths/Mod", "", szTmp, sizeof(szTmp)), size);
				}
				else
				{
					mm_LogFatal("Failed to resolve GetGameInfoString in fallback gamedir lookup.");
				}

				mm_UnloadLibrary(pTier0);
			}
			else
			{
				mm_LogFatal("Failed to load tier0 from \"%s\" in fallback gamedir lookup: %s", tier0_path, err);
			}
		}
		else
		{
			mm_LogFatal("Failed to resolve tier0 path in fallback gamedir lookup.");
		}
	}

	if (buffer[0] == 0)
	{
		strncpy(buffer, ".", size);
	}
}

MetamodBackend
mm_DetermineBackendS1(QueryValveInterface engineFactory, QueryValveInterface serverFactory, const char *game_name)
{
	if (engineFactory("VEngineServer023", NULL) != NULL)
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

		if (mm_FindPattern((void *)engineFactory, " Blade Symphony ", sizeof(" Blade Symphony ") - 1))
		{
			return MMBackend_Blade;
		}

		if (mm_FindPattern((void *)engineFactory, "Military Conflict: Vietnam", sizeof("Military Conflict: Vietnam") - 1))
		{
			return MMBackend_MCV;
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

			return MMBackend_Blade; // Old Blade
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
					else if (strcmp(game_name, "pvkii") == 0)
					{
						return MMBackend_PVKII;
					}
					else if (strcmp(game_name, ".") == 0 && engineFactory("MOCK_ENGINE", NULL))
					{
						return MMBackend_Mock;
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

