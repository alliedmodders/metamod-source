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

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <stddef.h>
#include "loader.h"
#include <sh_memfuncinfo.h>
#include <sh_memory.h>
#include "utility.h"
#include "gamedll.h"

class IServerGameDLL;

#define MAX_GAMEDLL_PATHS	10

IGameDllBridge* gamedll_bridge = NULL;
static int game_info_detected = 0;
static char game_name[128];
static char gamedll_paths[MAX_GAMEDLL_PATHS][PLATFORM_MAX_PATH];
static void *gamedll_libs[MAX_GAMEDLL_PATHS];
static unsigned int gamedll_path_count = 0;
static void *gamedll_lib = NULL;
static IServerGameDLL *gamedll_iface = NULL;
static QueryValveInterface gamedll_qvi = NULL;
static int gamedll_version = 0;
static int isgd_shutdown_index = -1;
static char mm_path[PLATFORM_MAX_PATH];

#if defined _WIN32
#define SERVER_NAME			"server.dll"
#elif defined __APPLE__
#define SERVER_NAME			"server.dylib"
#elif defined __linux__
#define SERVER_NAME			"server" LIB_SUFFIX
#endif

static bool
mm_DetectGameInformation()
{
	char game_path[PLATFORM_MAX_PATH];

	if (game_info_detected)
		return game_info_detected == 1 ? true : false;

	game_info_detected = -1;

	mm_GetGameName(game_name, sizeof(game_name));

	if (!mm_GetFileOfAddress((void*)mm_DetectGameInformation, mm_path, sizeof(mm_path)))
	{
		mm_LogFatal("Could not locate Metamod loader library path");
		return false;
	}

	if (!mm_ResolvePath(game_name, game_path, sizeof(game_path)))
	{
		mm_LogFatal("Could not resolve path: %s", game_name);
		return false;
	}

	FILE *fp;
	char gameinfo_path[PLATFORM_MAX_PATH];

	mm_PathFormat(gameinfo_path, sizeof(gameinfo_path), "%s/gameinfo.txt", game_path);
	if ((fp = fopen(gameinfo_path, "rt")) == NULL)
	{
		mm_LogFatal("Could not read file: %s", gameinfo_path);
		return false;
	}

	char temp_path[PLATFORM_MAX_PATH];
	char cur_path[PLATFORM_MAX_PATH];
	getcwd(cur_path, sizeof(cur_path));

	char *ptr;
	const char *lptr;
	bool search = false;
	char buffer[255], key[128], val[128];
	while (!feof(fp) && fgets(buffer, sizeof(buffer), fp) != NULL)
	{
		mm_TrimComments(buffer);
		mm_TrimLeft(buffer);
		mm_TrimRight(buffer);

		if (stricmp(buffer, "SearchPaths") == 0)
			search = true;

		if (!search)
			continue;

		mm_KeySplit(buffer, key, sizeof(key) - 1, val, sizeof(val) - 1);
		if (stricmp(key, "Game") != 0 && stricmp(key, "GameBin") != 0)
			continue;

		if (strncmp(val, "|gameinfo_path|", sizeof("|gameinfo_path|") - 1) == 0)
		{
			ptr = &val[sizeof("|gameinfo_path|") - 1];
			if (ptr[0] == '.')
				ptr++;
			lptr = game_path;
		}
		else
		{
			ptr = val;
			lptr = cur_path;
		}

		if (stricmp(key, "GameBin") == 0)
			mm_PathFormat(temp_path, sizeof(temp_path), "%s/%s/" SERVER_NAME, lptr, ptr);
		else if (!ptr[0])
			mm_PathFormat(temp_path, sizeof(temp_path), "%s/bin/" SERVER_NAME, lptr);
		else
			mm_PathFormat(temp_path, sizeof(temp_path), "%s/%s/bin/" SERVER_NAME, lptr, ptr);

		if (mm_PathCmp(mm_path, temp_path))
			continue;

		FILE *exists = fopen(temp_path, "rb");
		if (!exists)
			continue;
		fclose(exists);

		/* exists is still non-NULL... use this as a flag */
		for (unsigned int i = 0; i < gamedll_path_count; i++)
		{
			if (mm_PathCmp(gamedll_paths[i], temp_path))
			{
				exists = NULL;
				break;
			}
		}

		if (!exists)
			continue;

		mm_Format(gamedll_paths[gamedll_path_count],
				  PLATFORM_MAX_PATH,
				  "%s",
				  temp_path);
		gamedll_path_count++;

		if (gamedll_path_count == MAX_GAMEDLL_PATHS)
			break;
	}
	fclose(fp);

	game_info_detected = 1;

	if (gamedll_path_count == 0)
	{
		mm_LogFatal("Could not detect any valid game paths in gameinfo.txt");
		return false;
	}

	return true;
}

static void
mm_FreeCachedLibraries()
{
	for (unsigned int i = 0; i < gamedll_path_count; i++)
	{
		if (gamedll_libs[i] == NULL)
			continue;
		mm_UnloadLibrary(gamedll_libs[i]);
	}
}

static void
mm_PatchDllInit(bool patch);

static void
mm_PatchDllShutdown();

static void *isgd_orig_init = NULL;
static void *isgd_orig_shutdown = NULL;

class VEmptyClass
{
};

class IServerGameDLL
{
public:
	virtual bool DLLInit(QueryValveInterface engineFactory, 
						 QueryValveInterface physicsFactory, 
						 QueryValveInterface fileSystemFactory, 
						 void *pGlobals)
	{
		mm_backend = mm_DetermineBackend(engineFactory, gamedll_qvi, game_name);

		char error[255];
		if (mm_backend == MMBackend_UNKNOWN)
		{
			mm_LogFatal("Could not detect engine version");
		}
		else
		{
			if (!mm_LoadMetamodLibrary(mm_backend, error, sizeof(error)))
			{
				mm_LogFatal("Detected engine %d but could not load: %s", mm_backend, error);
			}
			else
			{
				typedef IGameDllBridge *(*GetGameDllBridge)();
				GetGameDllBridge get_bridge = (GetGameDllBridge)mm_GetProcAddress("GetGameDllBridge");
				if (get_bridge == NULL)
				{
					mm_UnloadMetamodLibrary();
					mm_LogFatal("Detected engine %d but could not find GetGameDllBridge callback", mm_backend);
				}
				else
				{
					gamedll_bridge = get_bridge();
				}
			}
		}

		if (gamedll_bridge)
		{
			gamedll_bridge_info info;

			info.engineFactory = (QueryValveInterface)engineFactory;
			info.physicsFactory = (QueryValveInterface)physicsFactory;
			info.fsFactory = (QueryValveInterface)fileSystemFactory;
			info.pGlobals = pGlobals;
			info.dllVersion = gamedll_version;
			info.isgd = gamedll_iface;
			info.gsFactory = gamedll_qvi;
			info.vsp_listener_path = mm_path;

			strcpy(error, "Unknown error");
			if (!gamedll_bridge->DLLInit_Pre(&info, error, sizeof(error)))
			{
				gamedll_bridge = NULL;
				mm_UnloadMetamodLibrary();
				mm_LogFatal("Unknown error loading Metamod for engine %d: %s", mm_backend, error);	
			}
		}

		/* Call the original */
		bool result;
		{
			union
			{
				bool (VEmptyClass::*mfpnew)(QueryValveInterface engineFactory, 
										 	QueryValveInterface physicsFactory, 
										 	QueryValveInterface fileSystemFactory, 
										 	void *pGlobals);
#if defined _WIN32
				void *addr;
			} u;
			u.addr = isgd_orig_init;
#else
				struct
				{
					void *addr;
					intptr_t adjustor;
				} s;
			} u;
			u.s.addr = isgd_orig_init;
			u.s.adjustor = 0;
#endif
			result = (((VEmptyClass *)gamedll_iface)->*u.mfpnew)(engineFactory,
																 physicsFactory,
																 fileSystemFactory,
																 pGlobals);
		}

		/**
		 * :TODO: possible logic hole here, what happens if the gamedll REALLY returns false? 
		 * I'm pretty sure we'll die horribly.
		 */

		if (!result)
		{
			gamedll_bridge->Unload();
			mm_UnloadMetamodLibrary();
			gamedll_bridge = NULL;
		}
		else if (gamedll_bridge != NULL)
		{
			gamedll_bridge->DLLInit_Post(&isgd_shutdown_index);
			assert(isgd_shutdown_index != -1);
			mm_PatchDllShutdown();
		}

		mm_PatchDllInit(false);

		return result;
	}

	virtual void DLLShutdown()
	{
		gamedll_bridge->Unload();
		gamedll_bridge = NULL;
		mm_UnloadMetamodLibrary();

		/* Call original function */
		{
			union
			{
				void (VEmptyClass::*mfpnew)();
#if defined _WIN32
				void *addr;
			} u;
			u.addr = isgd_orig_shutdown;
#else
				struct
				{
					void *addr;
					intptr_t adjustor;
				} s;
			} u;
			u.s.addr = isgd_orig_shutdown;
			u.s.adjustor = 0;
#endif
			(((VEmptyClass *)gamedll_iface)->*u.mfpnew)();
		}

		mm_UnloadLibrary(gamedll_lib);
		gamedll_lib = NULL;
	}
};

static IServerGameDLL isgd_thunk;

static void
mm_PatchDllInit(bool patch)
{
	void **vtable_src;
	void **vtable_dest;
	SourceHook::MemFuncInfo mfp;

	SourceHook::GetFuncInfo(&IServerGameDLL::DLLInit, mfp);

	assert(mfp.isVirtual);
	assert(mfp.thisptroffs == 0);
	assert(mfp.vtbloffs == 0);

	vtable_src = (void **)*(void **)&isgd_thunk;
	vtable_dest = (void **)*(void **)gamedll_iface;

	SourceHook::SetMemAccess(&vtable_dest[mfp.vtblindex],
							 sizeof(void*),
							 SH_MEM_READ|SH_MEM_WRITE|SH_MEM_EXEC);

	if (patch)
	{
		assert(isgd_orig_init == NULL);
		isgd_orig_init = vtable_dest[mfp.vtblindex];
		vtable_dest[mfp.vtblindex] = vtable_src[mfp.vtblindex];
	}
	else
	{
		assert(isgd_orig_init != NULL);
		vtable_dest[mfp.vtblindex] = isgd_orig_init;
		isgd_orig_init = NULL;
	}
}

static void
mm_PatchDllShutdown()
{
	void **vtable_src;
	void **vtable_dest;
	SourceHook::MemFuncInfo mfp;

	mfp.isVirtual = false;
	SourceHook::GetFuncInfo(&IServerGameDLL::DLLShutdown, mfp);
	assert(mfp.isVirtual);
	assert(mfp.thisptroffs == 0);
	assert(mfp.vtbloffs == 0);

	vtable_src = (void **)*(void **)&isgd_thunk;
	vtable_dest = (void **)*(void **)gamedll_iface;

	isgd_orig_shutdown = vtable_dest[isgd_shutdown_index];
	vtable_dest[isgd_shutdown_index] = vtable_src[mfp.vtblindex];
}

static void
mm_PrepForGameLoad()
{
	mm_PatchDllInit(true);
}

void *
mm_GameDllRequest(const char *name, int *ret)
{
	if (gamedll_lib != NULL && gamedll_bridge == NULL)
	{
		return gamedll_qvi(name, ret);
	}

	if (strncmp(name, "ServerGameDLL", 13) == 0)
	{
		if (!mm_DetectGameInformation())
		{
			if (ret != NULL)
				*ret = 1;
			return NULL;
		}

		void *lib;
		char error[255];
		void *ptr = NULL;
		QueryValveInterface qvi;
		for (unsigned int i = 0; i < gamedll_path_count; i++)
		{
			if (gamedll_libs[i] == NULL)
			{
				lib = mm_LoadLibrary(gamedll_paths[i], error, sizeof(error));
				if (lib == NULL)
					continue;
				gamedll_libs[i] = lib;
			}
			lib = gamedll_libs[i];
			qvi = (QueryValveInterface)mm_GetLibAddress(lib, "CreateInterface");
			if (qvi == NULL)
				continue;
			ptr = qvi(name, ret);
			if (ptr != NULL)
			{
				gamedll_libs[i] = NULL;
				break;
			}
		}

		if (ptr != NULL)
		{
			mm_FreeCachedLibraries();	
			gamedll_lib = lib;
			gamedll_iface = (IServerGameDLL *)ptr;
			gamedll_qvi = qvi;
			gamedll_version = atoi(&name[13]);
			mm_PrepForGameLoad();

			if (ret != NULL)
				*ret = 0;
			return ptr;
		}
	}
	else if (game_info_detected == 0)
	{
		mm_LogFatal("Received interface request too early: %s", name);
	}

	if (ret != NULL)
		*ret = 1;
	return NULL;
}

