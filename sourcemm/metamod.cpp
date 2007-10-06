/**
 * vim: set ts=4 :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2007 AlliedModders LLC and authors.
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

#if defined _DEBUG
#define DEBUG2
#undef _DEBUG
#endif
#include <interface.h>
#include <eiface.h>
#include "metamod.h"
#include "metamod_provider.h"
#include "metamod_plugins.h"
#include "metamod_util.h"
#include "metamod_console.h"
#include "metamod_oslink.h"
#if defined DEBUG2
#undef DEBUG2
#define _DEBUG
#endif

using namespace SourceMM;
using namespace SourceHook;

/**
 * @brief Implementation of main SourceMM GameDLL functionality
 * @file sourcemm.cpp
 */

SH_DECL_MANUALHOOK4(SGD_DLLInit, 0, 0, 0, bool, CreateInterfaceFn, CreateInterfaceFn, CreateInterfaceFn, CGlobalVars *);
SH_DECL_MANUALHOOK0(SGD_GameInit, 0, 0, 0, bool);
SH_DECL_MANUALHOOK6(SGD_LevelInit, 0, 0, 0, bool, const char *, const char *, const char *, const char *, bool, bool);
SH_DECL_MANUALHOOK0_void(SGD_LevelShutdown, 0, 0, 0);
SH_DECL_MANUALHOOK0_void(SGD_DLLShutdown, 0, 0, 0);

bool Handler_DLLInit(CreateInterfaceFn engineFactory, CreateInterfaceFn physicsFactory, CreateInterfaceFn filesystemFactory, CGlobalVars *pGlobals);
bool Handler_DLLInit_Post(CreateInterfaceFn engineFactory, CreateInterfaceFn physicsFactory, CreateInterfaceFn filesystemFactory, CGlobalVars *pGlobals);
void Handler_DLLShutdown();
void Handler_LevelShutdown();
bool Handler_LevelInit(char const *pMapName, char const *pMapEntities, char const *pOldLevel, char const *pLandmarkName, bool loadGame, bool background);
bool Handler_GameInit();
void InitializeVSP();

struct game_dll_t
{
	HINSTANCE lib;
	CreateInterfaceFn factory;
};

String mod_path;
String metamod_path;
String full_bin_path;
bool parsed_game_info = false;
int vsp_version = 0;
int gamedll_version = 0;
int engine_build = SOURCE_ENGINE_UNKNOWN;
List<game_dll_t *> gamedll_list;
bool is_gamedll_loaded = false;
bool in_first_level = true;
bool is_game_init = false;
bool vsp_load_attempted = false;
bool vsp_load_requested = false;
bool vsp_loaded = false;
game_dll_t gamedll_info;
ConVar *metamod_version = NULL;
ConVar *mm_pluginsfile = NULL;
IServerGameDLL *server = NULL;
CreateInterfaceFn engine_factory = NULL;
CreateInterfaceFn physics_factory = NULL;
CreateInterfaceFn filesystem_factory = NULL;
CGlobalVars *gpGlobals = NULL;
SourceHook::CSourceHookImpl g_SourceHook;
SourceHook::ISourceHook *g_SHPtr = &g_SourceHook;
PluginId g_PLID = Pl_Console;
META_RES last_meta_res;
IServerPluginCallbacks *vsp_callbacks = NULL;

MetamodSource g_Metamod;

void ClearGamedllList();

/* Helper Macro */
#define	IFACE_MACRO(orig,nam) \
	CPluginManager::CPlugin *pl; \
	SourceHook::List<IMetamodListener *>::iterator event; \
	IMetamodListener *api; \
	int mret = 0; \
	void *val = NULL; \
	for (PluginIter iter = g_PluginMngr._begin(); iter != g_PluginMngr._end(); iter++) { \
		pl = (*iter); \
		for (event=pl->m_Events.begin(); event!=pl->m_Events.end(); event++) { \
			api = (*event); \
			mret = IFACE_FAILED; \
			if ( (val=api->On##nam##Query(iface, &mret)) != NULL ) { \
				if (ret) *ret = mret; \
				return val; \
			} \
		} \
	} \
	return (orig)(iface, ret);

#define ITER_EVENT(evn, args) \
	CPluginManager::CPlugin *pl; \
	SourceHook::List<IMetamodListener *>::iterator event; \
	IMetamodListener *api; \
	for (PluginIter iter = g_PluginMngr._begin(); iter != g_PluginMngr._end(); iter++) { \
		pl = (*iter); \
		for (event=pl->m_Events.begin(); event!=pl->m_Events.end(); event++) { \
			api = (*event); \
			api->evn args; \
		} \
	}

/* Initialize everything here */
void InitMainStates()
{
	char full_path[PATH_SIZE] = {0};
	GetFileOfAddress((void *)gamedll_info.factory, full_path, sizeof(full_path));
	full_bin_path.assign(full_path);

	/* Like Metamod, reload plugins at the end of the map.
	 * This is so plugins can hook everything on load, BUT, new plugins will be reloaded
	 * if the server is shut down (silly, but rare case).
	 */
	in_first_level = true;

	SourceHook::MemFuncInfo info;

	if (!provider->GetHookInfo(ProvidedHook_DLLInit, &info))
	{
		provider->DisplayError("Metamod:Source could not find a valid hook for IServerGameDLL::DLLInit");
	}
	SH_MANUALHOOK_RECONFIGURE(SGD_DLLInit, info.vtblindex, info.vtbloffs, info.thisptroffs);
	SH_ADD_MANUALHOOK_STATICFUNC(SGD_DLLInit, server, Handler_DLLInit, false);
	SH_ADD_MANUALHOOK_STATICFUNC(SGD_DLLInit, server, Handler_DLLInit_Post, true);

	if (!provider->GetHookInfo(ProvidedHook_GameInit, &info))
	{
		provider->DisplayError("Metamod:Source could not find a valid hook for IServerGameDLL::GameInit");
	}
	SH_MANUALHOOK_RECONFIGURE(SGD_GameInit, info.vtblindex, info.vtbloffs, info.thisptroffs);
	SH_ADD_MANUALHOOK_STATICFUNC(SGD_GameInit, server, Handler_GameInit, false);

	if (!provider->GetHookInfo(ProvidedHook_LevelInit, &info))
	{
		provider->DisplayError("Metamod:Source could not find a valid hook for IServerGameDLL::LevelInit");
	}
	SH_MANUALHOOK_RECONFIGURE(SGD_LevelInit, info.vtblindex, info.vtbloffs, info.thisptroffs);
	SH_ADD_MANUALHOOK_STATICFUNC(SGD_LevelInit, server, Handler_LevelInit, true);

	if (!provider->GetHookInfo(ProvidedHook_LevelShutdown, &info))
	{
		provider->DisplayError("Metamod:Source could not find a valid hook for IServerGameDLL::LevelShutdown");
	}
	SH_MANUALHOOK_RECONFIGURE(SGD_LevelShutdown, info.vtblindex, info.vtbloffs, info.thisptroffs);
	SH_ADD_MANUALHOOK_STATICFUNC(SGD_LevelShutdown, server, Handler_LevelShutdown, true);

	if (!provider->GetHookInfo(ProvidedHook_DLLShutdown, &info))
	{
		provider->DisplayError("Metamod:Source could not find a valid hook for IServerGameDLL::DLLShutdown");
	}
	SH_MANUALHOOK_RECONFIGURE(SGD_DLLShutdown, info.vtblindex, info.vtbloffs, info.thisptroffs);
	SH_ADD_MANUALHOOK_STATICFUNC(SGD_DLLShutdown, server, Handler_DLLShutdown, false);
}

/* This is where the magic happens */
SMM_API void *CreateInterface(const char *iface, int *ret)
{
	/* Prevent loading of self as a SourceMM plugin or Valve server plugin :x */
	if (strcmp(iface, PLAPI_NAME) == 0)
	{
		provider->DisplayWarning("Do not try loading Metamod:Source as a plugin.\n");

		if (ret)
		{
			*ret = IFACE_FAILED;
		}

		return NULL;
	}

	if (strncmp(iface, "ISERVERPLUGINCALLBACKS", 22) == 0)
	{
		vsp_callbacks = provider->GetVSPCallbacks(iface);

		if (vsp_callbacks != NULL && vsp_version == 0)
		{
			vsp_version = atoi(&iface[22]);
		}

		if (ret)
		{
			*ret = (vsp_callbacks != NULL) ? IFACE_OK : IFACE_FAILED;
		}

		return vsp_callbacks;
	}

	if (!parsed_game_info)
	{
		parsed_game_info = true;
		const char *game_dir = NULL;
		char game_path[PATH_SIZE];
		char mm_path[PATH_SIZE];

		/* Get path to SourceMM DLL */
		if (!GetFileOfAddress((void *)CreateInterface, mm_path, sizeof(mm_path)))
		{
			provider->DisplayError("GetFileOfAddress() failed! Metamod cannot load.\n");
			return NULL;
		}

		metamod_path.assign(mm_path);

		/* Get value of -game from command line, defaulting to hl2 as engine seems to do */
		game_dir = provider->GetCommandLineValue("-game", "hl2");

		engine_build = SOURCE_ENGINE_ORANGEBOX;

		/* Get absolute path */
		abspath(game_path, game_dir);
		mod_path.assign(game_path);

		char temp_path[PATH_SIZE];

		/* Path to gameinfo.txt */
		g_Metamod.PathFormat(temp_path, PATH_SIZE, "%s/%s", mod_path.c_str(), "gameinfo.txt");

		FILE *fp = fopen(temp_path, "rt");

		if (!fp)
		{
			provider->DisplayError("Unable to open gameinfo.txt!  Metamod cannot load.\n");
			return NULL;
		}

		char buffer[255];
		char key[128], val[128];
		bool search = false;
		bool gamebin = false;
		char *ptr;
		const char *lptr;
		char cur_path[PATH_SIZE];

		getcwd(cur_path, PATH_SIZE);

		while (!feof(fp) && fgets(buffer, sizeof(buffer), fp) != NULL)
		{
			UTIL_TrimComments(buffer);
			UTIL_TrimLeft(buffer);
			UTIL_TrimRight(buffer);

			if (stricmp(buffer, "SearchPaths") == 0)
			{
				search = true;
			}

			if (!search)
			{
				continue;
			}

			UTIL_KeySplit(buffer, key, sizeof(key) - 1, val, sizeof(val) - 1);
			if (stricmp(key, "Game") == 0 || stricmp(key, "GameBin") == 0)
			{
				if (stricmp(key, "Game") == 0)
				{
					gamebin = false;
				}
				else
				{
					gamebin = true;
				}

				if (strncmp(val, "|gameinfo_path|", sizeof("|gameinfo_path|") - 1) == 0)
				{
					ptr = &(val[sizeof("|gameinfo_path|") - 1]);
					if (ptr[0] == '.')
					{
						ptr++;
					}
					lptr = mod_path.c_str();
				} else {
					ptr = val;
					lptr = cur_path;
				}

				size_t ptr_len = strlen(ptr);
				if (ptr[ptr_len] == '/' || ptr[ptr_len] == '\\')
				{
					ptr[--ptr_len] = '\0';
				}

				/* No need to append "bin" if key is GameBin */
				if (gamebin)
				{
					g_Metamod.PathFormat(temp_path, PATH_SIZE, "%s/%s/%s", lptr, ptr, SERVER_DLL);
				}
				else if (!ptr[0])
				{
					g_Metamod.PathFormat(temp_path, PATH_SIZE, "%s/%s/%s", lptr, "bin", SERVER_DLL);
				}
				else
				{
					g_Metamod.PathFormat(temp_path, PATH_SIZE, "%s/%s/%s/%s", lptr, ptr, "bin", SERVER_DLL);
				}

				/* If not path to SourceMM... */
				if (!UTIL_PathCmp(mm_path, temp_path))
				{
					FILE *temp_fp = fopen(temp_path, "rb");
					if (!temp_fp)
					{
						continue;
					}
					fclose(temp_fp);

					/* Search for a matching game dll */
					List<game_dll_t *>::iterator iter;
					game_dll_t *pCheck;
					bool found = false;
					for (iter = gamedll_list.begin(); iter != gamedll_list.end(); iter++)
					{
						pCheck = (*iter);
						if (GetFileOfAddress((void *)pCheck->factory, buffer, sizeof(buffer)))
						{
							if (UTIL_PathCmp(temp_path, buffer))
							{
								found = true;
								break;
							}
						}
					}

					if (found)
					{
						continue;
					}
					
					HINSTANCE gamedll = dlmount(temp_path);
					if (gamedll == NULL)
					{
						continue;
					}

					CreateInterfaceFn fn = (CreateInterfaceFn)dlsym(gamedll, "CreateInterface");
					if (fn == NULL)
					{
						dlclose(gamedll);
						continue;
					}

					game_dll_t *pInfo = new game_dll_t;
					pInfo->factory = fn;
					pInfo->lib = gamedll;
					gamedll_list.push_back(pInfo);
					break;
				}
			}
		}
		fclose(fp);
	}

	if (!is_gamedll_loaded)
	{
		if (strncmp(iface, "ServerGameDLL", 13) == 0)
		{
			List<game_dll_t *>::iterator iter;
			game_dll_t *pInfo;
			for (iter=gamedll_list.begin(); iter!=gamedll_list.end(); iter++)
			{
				pInfo = (*iter);
				if ((server = (IServerGameDLL *)((pInfo->factory)(iface, ret))) != NULL)
				{
					if ((gamedll_version = provider->TryServerGameDLL(iface)) != 0)
					{
						/* We have a match.  Erase us from the list and save our info. */
						gamedll_list.erase(iter);
						gamedll_info = *pInfo;
						delete pInfo;
						is_gamedll_loaded = true;
						break;
					}
					/* :TODO: error otherwise? */
				}
			}

			if (is_gamedll_loaded)
			{
				ClearGamedllList();
				InitMainStates();
			}
			else
			{
				if (ret)
				{
					*ret = IFACE_FAILED;
				}
				return NULL;
			}
		}
		else
		{
			/* wtf do we do... */
			/* :TODO: .. something a bit more intelligent? */
			provider->DisplayError("Engine requested unknown interface before GameDLL was known!\n");
			return NULL;
		}
	}

	/* If we got here, there's definitely a GameDLL */
	IFACE_MACRO(gamedll_info.factory, GameDLL);
}

void ClearGamedllList()
{
	List<game_dll_t *>::iterator iter;

	game_dll_t *pInfo;
	for (iter = gamedll_list.begin(); iter != gamedll_list.end(); iter++)
	{
		pInfo = (*iter);
		dlclose(pInfo->lib);
		delete pInfo;
	}

	gamedll_list.clear();
}

int LoadPluginsFromFile(const char *_file)
{
	FILE *fp;
	int total = 0, skipped=0;
	PluginId id;
	bool already;

	fp = fopen(_file, "rt");
	if (!fp)
	{
		LogMessage("[META] Could not open plugins file %s\n", _file);
		return -1;
	}

	char buffer[255], error[255], full_path[255];
	const char *ptr, *ext, *file;
	size_t length;
	while (!feof(fp) && fgets(buffer, sizeof(buffer), fp) != NULL)
	{
		UTIL_TrimLeft(buffer);
		UTIL_TrimRight(buffer);

		length = strlen(buffer);
		if (!length)
		{
			continue;
		}

		if (buffer[0] == '\0' || buffer[0] == ';' || strncmp(buffer, "//", 2) == 0)
		{
			continue;
		}

		file = buffer;
		if (buffer[0] == '"')
		{
			char *cptr = buffer;
			file = ++cptr;

			while (*cptr)
			{
				if (*cptr == '"')
				{
					*cptr = '\0';
					break;
				}
				cptr++;
			}
		}
		else
		{
			char *cptr = buffer;
			while (*cptr)
			{
				if (isspace(*cptr))
				{
					char *optr = cptr;
					while (*cptr && isspace(*cptr))
					{
						cptr++;
					}
					*optr = '\0';
					UTIL_TrimRight(cptr);
					if (*cptr && isalpha(*cptr))
					{
						g_PluginMngr.SetAlias(buffer, cptr);
						file = cptr;
					}
					break;
				}
				cptr++;
			}
		}
		if (!file[0])
		{
			continue;
		}
		/* First find if it's an absolute path or not... */
		if (file[0] == '/' || strncmp(&(file[1]), ":\\", 2) == 0)
		{
			/* If we're in an absolute path, ignore our normal heuristics */
			id = g_PluginMngr.Load(file, Pl_File, already, error, sizeof(error));
			if (id < Pl_MinId || g_PluginMngr.FindById(id)->m_Status < Pl_Paused)
			{
				LogMessage("[META] Failed to load plugin %s.  %s", buffer, error);
			}
			else
			{
				if (already)
				{
					skipped++;
				}
				else
				{
					total++;
				}
			}
		}
		else
		{
			/* Attempt to find a file extension */
			ptr = UTIL_GetExtension(file);
			/* Add an extension if there's none there */
			if (!ptr)
			{
#if defined WIN32 || defined _WIN32
				ext = ".dll";
#else
				ext = "_i486.so";
#endif
			}
			else
			{
				ext = "";
			}
			/* Format the new path */
			g_Metamod.PathFormat(full_path, sizeof(full_path), "%s/%s%s", mod_path.c_str(), file, ext);
			id = g_PluginMngr.Load(full_path, Pl_File, already, error, sizeof(error));
			if (id < Pl_MinId || g_PluginMngr.FindById(id)->m_Status < Pl_Paused)
			{
				LogMessage("[META] Failed to load plugin %s.  %s", buffer, error);
			}
			else
			{
				if (already)
				{
					skipped++;
				}
				else
				{
					total++;
				}
			}
		}
	}
	fclose(fp);

	if (skipped)
	{
		LogMessage("[META] Loaded %d plugins from file (%d already loaded)", total, skipped);
	}
	else
	{
		LogMessage("[META] Loaded %d plugins from file.", total);
	}
	
	return total;
}

void InitializeVSP()
{
	size_t len;
	char engine_file[PATH_SIZE];
	char engine_path[PATH_SIZE];
	char rel_path[PATH_SIZE * 2];

	GetFileOfAddress((void *)engine_factory, engine_file, sizeof(engine_file));

	/* Chop off the "engine" file part */
	len = strlen(engine_file);
	for (size_t i = len - 1; i >= 0 && i < len; i--)
	{
		if (engine_file[i] == '/' || engine_file[i] == '\\')
		{
			engine_file[i] = '\0';
			break;
		}
	}
	abspath(engine_path, engine_file);

	const char *usepath = metamod_path.c_str();
	if (UTIL_Relatize(rel_path, sizeof(rel_path), engine_path, metamod_path.c_str()))
	{
		usepath = rel_path;
	}

	char command[PATH_SIZE * 2];
	UTIL_Format(command, sizeof(command), "plugin_load \"%s\"\n", usepath);
	provider->ServerCommand(command);
}

/* Wrapper function.  This is called when the GameDLL thinks it's using
 * the engine's real engineFactory.
 */
void *EngineFactory(const char *iface, int *ret)
{
	IFACE_MACRO(engine_factory, Engine);
}

/* Wrapper function.  This is called when the GameDLL thinks it's using
 * the engine's real physicsFactory.
 */
void *PhysicsFactory(const char *iface, int *ret)
{
	IFACE_MACRO(physics_factory, Physics);
}

/* Wrapper function.  This is called when the GameDLL thinks it's using
 * the engine's real fileSystemFactory.
 */
void *FileSystemFactory(const char *iface, int *ret)
{
	IFACE_MACRO(filesystem_factory, FileSystem);
}

void LogMessage(const char *msg, ...)
{
	va_list ap;
	static char buffer[2048];

	va_start(ap, msg);
	size_t len = vsnprintf(buffer, sizeof(buffer) - 2, msg, ap);
	va_end(ap);

	buffer[len++] = '\n';
	buffer[len] = '\0';

	if (!provider->LogMessage(buffer))
	{
		fprintf(stdout, "%s", buffer);
	}
}

bool Handler_DLLInit(CreateInterfaceFn engineFactory, CreateInterfaceFn physicsFactory, CreateInterfaceFn filesystemFactory, CGlobalVars *pGlobals)
{
	engine_factory = engineFactory;
	filesystem_factory = filesystemFactory;
	physics_factory = physicsFactory;
	gpGlobals = pGlobals;

	provider->Notify_DLLInit_Pre(server, engineFactory, gamedll_info.factory);

	metamod_version = provider->CreateConVar("metamod_version", 
		SOURCEMM_VERSION, 
		"Metamod:Source Version",
		ConVarFlag_Notify|ConVarFlag_Replicated|ConVarFlag_SpOnly);
	
	mm_pluginsfile = provider->CreateConVar("mm_pluginsfile", 
#if defined WIN32 || defined _WIN32
		"addons\\metamod\\metaplugins.ini", 
#else
		"addons/metamod/metaplugins.ini",
#endif
		"Metamod:Source Plugins File",
		ConVarFlag_SpOnly);

	const char *pluginFile = provider->GetCommandLineValue("mm_pluginsfile", NULL);
	if (!pluginFile) 
	{
		pluginFile = provider->GetConVarString(mm_pluginsfile);
		if (pluginFile == NULL)
		{
			pluginFile = "addons/metamod/metaplugins.ini";
		}
	}

	char full_path[260];
	g_Metamod.PathFormat(full_path, sizeof(full_path), "%s/%s", mod_path.c_str(), pluginFile);

	LoadPluginsFromFile(full_path);

	in_first_level = true;

	RETURN_META_VALUE(MRES_IGNORED, true);
}

bool Handler_GameInit()
{
	if (is_game_init)
	{
		return true;
	}

	if (vsp_load_requested)
	{
		InitializeVSP();
	}

	is_game_init = true;

	RETURN_META_VALUE(MRES_IGNORED, true);
}

bool Handler_DLLInit_Post(CreateInterfaceFn engineFactory, CreateInterfaceFn physicsFactory, CreateInterfaceFn filesystemFactory, CGlobalVars *pGlobals)
{
	g_PluginMngr.SetAllLoaded();
	RETURN_META_VALUE(MRES_IGNORED, true);
}

void Handler_DLLShutdown()
{
	/* Unload plugins */
	g_PluginMngr.UnloadAll();

	provider->Notify_DLLShutdown_Pre();

	SH_CALL(server, &IServerGameDLL::DLLShutdown)();

	g_SourceHook.CompleteShutdown();

	if (is_gamedll_loaded && gamedll_info.lib)
	{
		dlclose(gamedll_info.lib);
		is_gamedll_loaded = false;
	}

	RETURN_META(MRES_SUPERCEDE);
}

void Handler_LevelShutdown(void)
{
	if (!in_first_level)
	{
		char full_path[255];
		g_Metamod.PathFormat(full_path, 
			sizeof(full_path), 
			"%s/%s", 
			mod_path.c_str(),
			provider->GetConVarString(mm_pluginsfile));

		LoadPluginsFromFile(full_path);
	}
	else
	{
		in_first_level = false;
	}

	ITER_EVENT(OnLevelShutdown, ());

	RETURN_META(MRES_IGNORED);
}

bool Handler_LevelInit(char const *pMapName, char const *pMapEntities, char const *pOldLevel, char const *pLandmarkName, bool loadGame, bool background)
{
	ITER_EVENT(OnLevelInit, (pMapName, pMapEntities, pOldLevel, pLandmarkName, loadGame, background));

	RETURN_META_VALUE(MRES_IGNORED, false);
}

void MetamodSource::LogMsg(ISmmPlugin *pl, const char *msg, ...)
{
	va_list ap;
	char buffer[2048];
	
	va_start(ap, msg);
	UTIL_FormatArgs(buffer, sizeof(buffer), msg, ap);
	va_end(ap);

	LogMessage("[%s] %s", pl->GetLogTag(), buffer);
}

CreateInterfaceFn MetamodSource::GetEngineFactory(bool syn/* =true */)
{
	if (syn)
	{
		return EngineFactory;
	}

	return engine_factory;
}

CreateInterfaceFn MetamodSource::GetPhysicsFactory(bool syn/* =true */)
{
	if (syn)
	{
		return PhysicsFactory;
	}

	return physics_factory;
}

CreateInterfaceFn MetamodSource::GetFileSystemFactory(bool syn/* =true */)
{
	if (syn)
	{
		return FileSystemFactory;
	}

	return filesystem_factory;
}

CreateInterfaceFn MetamodSource::GetServerFactory(bool syn/* =true */)
{
	if (syn)
	{
		return CreateInterface;
	}

	return gamedll_info.factory;
}

CGlobalVars *MetamodSource::GetCGlobals()
{
	return gpGlobals;
}

void MetamodSource::SetLastMetaReturn(META_RES res)
{
	last_meta_res = res;
}

META_RES MetamodSource::GetLastMetaReturn()
{
	return last_meta_res;
}

void MetamodSource::ConPrint(const char *str)
{
	provider->ConsolePrint(str);
}

void MetamodSource::ConPrintf(const char *fmt, ...)
{
	va_list ap;
	char buffer[2048];

	va_start(ap, fmt);
	UTIL_FormatArgs(buffer, sizeof(buffer), fmt, ap);
	va_end(ap);

	provider->ConsolePrint(buffer);
}

void MetamodSource::GetApiVersions(int &major, int &minor, int &plvers, int &plmin)
{
	major = SM_VERS_API_MAJOR;
	minor = SM_VERS_API_MINOR;
	plvers = PLAPI_VERSION;
	plmin = PLAPI_MIN_VERSION;
}

void MetamodSource::GetShVersions(int &shvers, int &shimpl)
{
	shvers = SH_IFACE_VERSION;
	shimpl = SH_IMPL_VERSION;
}

int MetamodSource::FormatIface(char iface[], unsigned int maxlength)
{
	int length = (int)strlen(iface);
	int i;
	int num = 0;

	for (i = length - 1; i >= 0; i--)
	{
		if (!isdigit(iface[i]))
		{
			if (i != length - 1)
			{
				num = 1;
			}
			break;
		}
	}

	if ( (num && ((int)maxlength <= length)) || (!num && ((int)maxlength <= length + 3)) )
	{
		return -1;
	}

	if (i != length - 1)
	{
		num = atoi(&(iface[++i]));
	}

	num++;

	snprintf(&(iface[i]), 4, "%03d", num);

	return num;
}

void *MetamodSource::InterfaceSearch(CreateInterfaceFn fn, const char *iface, int max, int *ret)
{
	char _if[256];	/* assume no interface goes beyond this */
	size_t len = strlen(iface);
	int num = 0;
	void *pf = NULL;

	if (max > 999)
	{
		max = 999;
	}

	if (len + 4 > sizeof(_if))
	{
		if (ret)
		{
			*ret = IFACE_FAILED;
		}
		return NULL;
	}

	strcpy(_if, iface);

	do
	{
		if ((pf = (fn)(_if, ret)) != NULL)
		{
			break;
		}
		if (num > max)
		{
			break;
		}
	} while ((num = FormatIface(_if, len+1)));

	return pf;
}

void *MetamodSource::VInterfaceMatch(CreateInterfaceFn fn, const char *iface, int min)
{
	char buffer[256];	/* assume no interface will go beyond this */
	size_t len = strlen(iface);
	int ret;			/* just in case something doesn't handle NULL properly */

	if (len > sizeof(buffer) - 4)
	{
		return NULL;
	}

	strcpy(buffer, iface);

	if (min != -1)
	{
		char *ptr = &buffer[len - 1];
		int digits = 0;
		while (isdigit(*ptr) && digits <=3)
		{
			*ptr = '\0';
			digits++;
			ptr--;
		}
		if (digits != 3)
		{
			/* for now, assume this is an error */
			strcpy(buffer, iface);
		}
		else
		{
			char num[4];
			min = (min == 0) ? 1 : min;
			snprintf(num, sizeof(num), "%03d", min);
			strcat(buffer, num);
		}
	}

	return InterfaceSearch(fn, buffer, IFACE_MAXNUM, &ret);
}

const char *MetamodSource::GetBaseDir()
{
	return mod_path.c_str();
}

void MetamodSource::PathFormat(char *buffer, size_t len, const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	size_t mylen = UTIL_FormatArgs(buffer, len, fmt, ap);
	va_end(ap);

	for (size_t i = 0; i < mylen; i++)
	{
		if (buffer[i] == ALT_SEP_CHAR)
		{
			buffer[i] = PATH_SEP_CHAR;
		}
	}
}

void MetamodSource::ClientConPrintf(edict_t *client, const char *fmt, ...)
{
	va_list ap;
	char buffer[2048];

	va_start(ap, fmt);
	UTIL_FormatArgs(buffer, sizeof(buffer), fmt, ap);
	va_end(ap);

	provider->ClientConsolePrint(client, buffer);
}

void MetamodSource::EnableVSPListener()
{
	if (is_game_init && !vsp_load_requested && !vsp_loaded)
	{
		InitializeVSP();
	}

	vsp_load_requested = true;
}

int MetamodSource::GetVSPVersion()
{
	return vsp_version;
}

int MetamodSource::GetGameDLLVersion()
{
	return gamedll_version;
}

bool MetamodSource::RemotePrintingAvailable()
{
	return provider->IsRemotePrintingAvailable();
}

void *MetamodSource::MetaFactory(const char *iface, int *ret, PluginId *id)
{
	if (id)
	{
		*id = 0;
	}

	if (!iface)
	{
		return NULL;
	}

	/* First check ours... we get first chance! */
	if (strcmp(iface, MMIFACE_SOURCEHOOK) == 0)
	{
		if (ret)
		{
			*ret = IFACE_OK;
		}
		return static_cast<void *>(static_cast<SourceHook::ISourceHook *>(&g_SourceHook));
	}
	else if (strcmp(iface, MMIFACE_PLMANAGER) == 0)
	{
		if (ret)
		{
			*ret = IFACE_OK;
		}
		return static_cast<void *>(static_cast<ISmmPluginManager *>(&g_PluginMngr));
	}

	CPluginManager::CPlugin *pl;
	List<IMetamodListener *>::iterator event;
	IMetamodListener *api;
	void *value;
	
	int subret = 0;
	for (PluginIter iter = g_PluginMngr._begin();
		 iter != g_PluginMngr._end();
		 iter++)
	{
		pl = (*iter);
		for (event = pl->m_Events.begin(); event != pl->m_Events.end(); event++)
		{
			api = (*event);
			subret = IFACE_FAILED;
			if ((value = api->OnMetamodQuery(iface, &subret)) != NULL)
			{
				if (ret)
				{
					*ret = subret;
				}
				if (id)
				{
					*id = pl->m_Id;
				}
				return value;
			}
		}
	}

	if (ret)
	{
		*ret = IFACE_FAILED;
	}

	return NULL;
}

void MetamodSource::AddListener(ISmmPlugin *plugin, IMetamodListener *pListener)
{
	CPluginManager::CPlugin *pl = g_PluginMngr.FindByAPI(plugin);

	pl->m_Events.push_back(pListener);
}

const char *MetamodSource::GetGameBinaryPath()
{
	return full_bin_path.c_str();
}

const char *MetamodSource::GetPluginsFile()
{
	return provider->GetConVarString(mm_pluginsfile);
}

IConCommandBaseAccessor *MetamodSource::GetCvarBaseAccessor()
{
	return provider->GetConCommandBaseAccessor();
}

bool MetamodSource::RegisterConCommandBase(ISmmPlugin *plugin, ConCommandBase *pCommand)
{
	if (provider->IsConCommandBaseACommand(pCommand))
	{
		g_PluginMngr.AddPluginCmd(plugin, pCommand);
	}
	else
	{
		g_PluginMngr.AddPluginCvar(plugin, pCommand);
	}

	return provider->RegisterConCommandBase(pCommand);
}

void MetamodSource::UnregisterConCommandBase(ISmmPlugin *plugin, ConCommandBase *pCommand)
{
	if (provider->IsConCommandBaseACommand(pCommand))
	{
		g_PluginMngr.RemovePluginCmd(plugin, pCommand);
	}
	else
	{
		g_PluginMngr.RemovePluginCvar(plugin, pCommand);
	}

	CPluginManager::CPlugin *pOrig = g_PluginMngr.FindByAPI(plugin);
	UnregisterConCommandBase(pOrig ? pOrig->m_Id : 0, pCommand);
}

void MetamodSource::UnregisterConCommandBase(PluginId id, ConCommandBase *pCommand)
{
	PluginIter iter;
	CPluginManager::CPlugin *pPlugin;
	List<IMetamodListener *>::iterator event;
	IMetamodListener *pML;
	for (iter=g_PluginMngr._begin(); iter!=g_PluginMngr._end(); iter++)
	{
		pPlugin = (*iter);
		if (pPlugin->m_Status < Pl_Paused)
		{
			continue;
		}
		/* Only valid for plugins >= 12 (v1:6, SourceMM 1.5) */
		if (pPlugin->m_API->GetApiVersion() < 12)
		{
			continue;
		}
		for (event=pPlugin->m_Events.begin();
			event!=pPlugin->m_Events.end();
			event++)
		{
			pML = (*event);
			pML->OnUnlinkConCommandBase(id, pCommand);
		}
	}

	return provider->UnregisterConCommandBase(pCommand);
}

int MetamodSource::GetUserMessageCount()
{
	return provider->GetUserMessageCount();
}

int MetamodSource::FindUserMessage(const char *name, int *size/* =NULL */)
{
	return provider->FindUserMessage(name, size);
}

const char *MetamodSource::GetUserMessage(int index, int *size/* =NULL */)
{
	return provider->GetUserMessage(index, size);
}

int MetamodSource::GetSourceEngineBuild()
{
	return engine_build;
}

void MetamodSource::NotifyVSPListening(IServerPluginCallbacks *callbacks)
{
	ITER_EVENT(OnVSPListening, (callbacks));
}

IServerPluginCallbacks *MetamodSource::GetVSPInfo(int *pVersion)
{
	if (pVersion)
	{
		*pVersion = vsp_version;
	}

	return vsp_callbacks;
}

