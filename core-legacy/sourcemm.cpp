/* ======== SourceMM ========
 * Copyright (C) 2004-2010 Metamod:Source Development Team
 * No warranties of any kind
 *
 * License: zlib/libpng
 *
 * Author(s): David "BAILOPAN" Anderson
 * Contributor(s): Scott "Damaged Soul" Ehlert
 *				: Pavol "PM OnoTo" Marko
 * ============================
 */

#include <interface.h>
#include <eiface.h>
#include <tier0/icommandline.h>
#include "sourcemm.h"
#include "concommands.h"
#include "CSmmAPI.h"
#include "CPlugin.h"
#include "util.h"
#include "iplayerinfo.h"
#include <filesystem.h>

using namespace SourceMM;

/**
 * @brief Implementation of main SourceMM GameDLL functionality
 * @file sourcemm.cpp
 */

#undef CommandLine
DLL_IMPORT ICommandLine *CommandLine();

SH_DECL_HOOK0_void(IServerGameDLL, LevelShutdown, SH_NOATTRIB, false);
SH_DECL_HOOK6(IServerGameDLL, LevelInit, SH_NOATTRIB, false, bool, const char *, const char *, const char *, const char *, bool, bool);
SH_DECL_HOOK1_void(IServerGameClients, ClientCommand, SH_NOATTRIB, 0, edict_t *);
SH_DECL_HOOK0(IServerGameDLL, GameInit, SH_NOATTRIB, false, bool);

void LevelShutdown_handler();
bool LevelInit_handler(char const *pMapName,
					   char const *pMapEntities,
					   char const *pOldLevel,
					   char const *pLandmarkName,
					   bool loadGame,
					   bool background);
bool GameInit_handler();
int LoadPlugins(const char *filepath, const char *vdfpath);
int LoadVDFPluginsFromDir(const char *dir, int &skipped);
bool KVLoadFromFile(KeyValues *kv,
					IBaseFileSystem *filesystem,
					const char *resourceName,
					const char *pathID = NULL);

GameDllInfo g_GameDll = {false, NULL, NULL, NULL};
EngineInfo g_Engine;
SourceHook::CSourceHookImpl g_SourceHook;
SourceHook::ISourceHook *g_SHPtr = &g_SourceHook;
SourceHook::String g_ModPath;
SourceHook::String g_MetamodPath;
PluginId g_PLID = Pl_Console;			/* Technically, SourceMM is the "Console" plugin... :p */
static bool bInFirstLevel = true;
bool g_bGameInit = false;
int g_GameDllVersion = 0;
static const char VSPIFACE_001[] = "ISERVERPLUGINCALLBACKS001";
static const char VSPIFACE_002[] = "ISERVERPLUGINCALLBACKS002";
static const char GAMEINFO_PATH[] = "|gameinfo_path|";
IFileSystem *baseFs = NULL;
bool g_bLevelChanged = false;
IServerPluginCallbacks *g_pRealVspCallbacks = NULL;
unsigned int g_vsp_version = 0;

#define ITER_EVENT(evn, args) \
	CPluginManager::CPlugin *pl; \
	SourceHook::List<CPluginEventHandler>::iterator event; \
	IMetamodListener *api; \
	for (PluginIter iter = g_PluginMngr._begin(); iter != g_PluginMngr._end(); iter++) { \
		pl = (*iter); \
		for (event=pl->m_Events.begin(); event!=pl->m_Events.end(); event++) { \
			api = (*event).event; \
			api->evn args; \
		} \
	}

///////////////////////////////////
// Main code for HL2 Interaction //
///////////////////////////////////

/* Initialize everything here */
void InitMainStates()
{
	/* Like Metamod, reload plugins at the end of the map.
	 * This is so plugins can hook everything on load, BUT, new plugins will be reloaded
	 * if the server is shut down (silly, but rare case).
	 */
	bInFirstLevel = true;

	char smm_path[PATH_SIZE];
	const char *game_dir = CommandLine()->ParmValue("-game", "hl2");
	abspath(smm_path, game_dir);
	g_ModPath.assign(smm_path);

	SH_ADD_HOOK_STATICFUNC(IServerGameDLL, LevelShutdown, g_GameDll.pGameDLL, LevelShutdown_handler, true);
	SH_ADD_HOOK_STATICFUNC(IServerGameDLL, LevelInit, g_GameDll.pGameDLL, LevelInit_handler, true);
	SH_ADD_HOOK_STATICFUNC(IServerGameDLL, GameInit, g_GameDll.pGameDLL, GameInit_handler, false);
}

void DoInitialPluginLoads()
{
	const char *pluginFile = g_Engine.icvar->GetCommandLineValue("mm_pluginsfile");
	const char *mmBaseDir = g_Engine.icvar->GetCommandLineValue("mm_basedir");

	if (!pluginFile) 
	{
		pluginFile = GetPluginsFile();
	}
	if (!mmBaseDir)
	{
		mmBaseDir = GetMetamodBaseDir();
	}

	char filepath[PATH_SIZE], vdfpath[PATH_SIZE];

	g_SmmAPI.PathFormat(filepath, sizeof(filepath), "%s/%s", g_ModPath.c_str(), pluginFile);
	g_SmmAPI.PathFormat(vdfpath, sizeof(vdfpath), "%s/%s", g_ModPath.c_str(), mmBaseDir);
	LoadPlugins(filepath, vdfpath);
}

bool StartupMetamod(CreateInterfaceFn engineFactory, bool bWaitForGameInit)
{
	g_Engine.engine = (IVEngineServer *)((engineFactory)(INTERFACEVERSION_VENGINESERVER, NULL));

	if (!g_Engine.engine)
	{
		Error("Could not find IVEngineServer! Metamod cannot load.");
		return false;
	}
	g_Engine.icvar = (ICvar *)((engineFactory)(VENGINE_CVAR_INTERFACE_VERSION , NULL));
	if (!g_Engine.icvar)
	{
		Error("Could not find ICvar! Metamod cannot load.");
		return false;
	}

	g_Engine.loaded = true;

	/* The Ship is the only game known at this time that uses the pre-Episode One engine */
	g_Engine.original = strcmp(CommandLine()->ParmValue("-game", "hl2"), "ship") == 0;

	ConCommandBaseMgr::OneTimeInit(static_cast<IConCommandBaseAccessor *>(&g_SMConVarAccessor));

	if (g_GameDll.pGameClients)
	{
		SH_ADD_HOOK_STATICFUNC(IServerGameClients, ClientCommand, g_GameDll.pGameClients, ClientCommand_handler, false);
	}
	else
	{
		/* If IServerGameClients isn't found, this really isn't a fatal error so... */
		LogMessage("[META] Warning: Could not find IServerGameClients!");
		LogMessage("[META] Warning: The 'meta' command will not be available to clients.");
	}

	if (!g_SmmAPI.CacheCmds())
	{
		LogMessage("[META] Warning: Failed to initialize Con_Printf.  Defaulting to Msg().");
		LogMessage("[META] Warning: Console messages will not be redirected to rcon console.");
	}

	g_SmmAPI.CacheUserMessages();

	baseFs = (IFileSystem *)((engineFactory)(FILESYSTEM_INTERFACE_VERSION, NULL));
	if (baseFs == NULL)
	{
		LogMessage("[META] Failed to find filesystem interface, .vdf files will not be parsed.");
	}

	if (!g_SMConVarAccessor.InitConCommandBaseList())
	{
		/* This is very unlikely considering it's old engine */
		LogMessage("[META] Warning: Failed to find ConCommandBase list!");
		LogMessage("[META] Warning: ConVars and ConCommands cannot be unregistered properly! Please file a bug report.");
	}

	if (!bWaitForGameInit)
	{
		DoInitialPluginLoads();
		bInFirstLevel = true;
	}

	return true;
}

void LoadAsGameDLL(const gamedll_bridge_info *info)
{
	g_Engine.engineFactory = (CreateInterfaceFn)info->engineFactory;
	g_Engine.fileSystemFactory = (CreateInterfaceFn)info->fsFactory;
	g_Engine.physicsFactory = (CreateInterfaceFn)info->physicsFactory;
	g_Engine.pGlobals = (CGlobalVars*)info->pGlobals;
	g_GameDll.loaded = true;
	g_GameDll.factory = (CreateInterfaceFn)info->gsFactory;
	g_GameDll.pGameDLL = (IServerGameDLL*)info->isgd;
	g_GameDllVersion = info->dllVersion;
	g_MetamodPath.assign(info->vsp_listener_path);

	InitMainStates();

	StartupMetamod(g_Engine.engineFactory, false);
}

bool AlternatelyLoadMetamod(CreateInterfaceFn ifaceFactory, CreateInterfaceFn serverFactory)
{
	g_Engine.engineFactory = ifaceFactory;
	g_Engine.fileSystemFactory = ifaceFactory;
	g_Engine.physicsFactory = ifaceFactory;

	IPlayerInfoManager *playerInfoManager = (IPlayerInfoManager *)serverFactory("PlayerInfoManager002", NULL);
	if (playerInfoManager == NULL)
	{
		Error("Metamod:Source requires gameinfo.txt modification to load on this game.");
		return false;
	}

	g_Engine.pGlobals = playerInfoManager->GetGlobalVars();

	/* Now find the server */
	g_GameDll.factory = serverFactory;

	char gamedll_iface[] = "ServerGameDLL000";
	for (unsigned int i = 3; i <= 50; i++)
	{
		gamedll_iface[15] = '0' + i;
		g_GameDll.pGameDLL = (IServerGameDLL *)serverFactory(gamedll_iface, NULL);
		if (g_GameDll.pGameDLL != NULL)
		{
			g_GameDllVersion = i;
			break;
		}
	}

	if (g_GameDll.pGameDLL == NULL)
	{
		Error("Metamod:Source requires gameinfo.txt modification to load on this game.");
		return false;
	}

	char gameclients_iface[] = "ServerGameClients000";
	for (unsigned int i = 3; i <= 4; i++)
	{
		gameclients_iface[19] = '0' + i;
		g_GameDll.pGameClients = (IServerGameClients *)serverFactory(gameclients_iface, NULL);
		if (g_GameDll.pGameClients != NULL)
		{
			break;
		}
	}

	InitMainStates();

	if (!StartupMetamod(ifaceFactory, true))
	{
		return false;
	}

	return true;
}

bool GameInit_handler()
{
	if (g_bGameInit)
		RETURN_META_VALUE(MRES_IGNORED, true);

	if (g_SmmAPI.VSPEnabled() && !g_bIsBridgedAsVsp)
		g_SmmAPI.LoadAsVSP();

	if (g_bIsBridgedAsVsp)
	{
		DoInitialPluginLoads();
		g_PluginMngr.SetAllLoaded();
	}

	g_bGameInit = true;

	RETURN_META_VALUE(MRES_IGNORED, true);
}

void UnloadMetamod(bool shutting_down)
{
	/* Unload plugins */
	g_PluginMngr.UnloadAll();

	if (shutting_down)
	{
		/* Add the FCVAR_GAMEDLL flag to our cvars so the engine removes them properly */
		g_SMConVarAccessor.MarkCommandsAsGameDLL();
		g_Engine.icvar->UnlinkVariables(FCVAR_GAMEDLL);
	}

	g_SourceHook.CompleteShutdown();
}

bool LoadFromVDF(const char *file, bool &skipped)
{
	PluginId id;
	bool already, kvfileLoaded;
	KeyValues *pValues;
	const char *plugin_file, *alias;
	char full_path[256], error[256];
	
	pValues = new KeyValues("Metamod Plugin");

	if (g_Engine.original)
	{
		/* The Ship must use a special version of this function */
		kvfileLoaded = KVLoadFromFile(pValues, baseFs, file);
	}
	else
	{
		kvfileLoaded = pValues->LoadFromFile(baseFs, file);
	}

	if (!kvfileLoaded)
	{
		pValues->deleteThis();
		skipped = false;
		return false;
	}

	if ((plugin_file = pValues->GetString("file", NULL)) == NULL)
	{
		pValues->deleteThis();
		skipped = false;
		return false;
	}

	if ((alias = pValues->GetString("alias", NULL)) != NULL)
	{
		g_PluginMngr.SetAlias(alias, plugin_file);
	}

	g_SmmAPI.GetFullPluginPath(plugin_file, full_path, sizeof(full_path));

	id = g_PluginMngr.Load(full_path, Pl_File, already, error, sizeof(error));
	skipped = already;
	if (id < Pl_MinId || g_PluginMngr.FindById(id)->m_Status < Pl_Paused)
	{
		LogMessage("[META] Failed to load plugin %s: %s", plugin_file, error);
		return false;
	}

	pValues->deleteThis();
	return true;
}

int LoadVDFPluginsFromDir(const char *dir, int &skipped)
{
	bool success, skip;
	int total = 0;
	char path[MAX_PATH];

	skipped = 0;

#if defined _MSC_VER
	HANDLE hFind;
	WIN32_FIND_DATA fd;
	char error[255];

	g_SmmAPI.PathFormat(path, sizeof(path), "%s\\*.vdf", dir);
	if ((hFind = FindFirstFile(path, &fd)) == INVALID_HANDLE_VALUE)
	{
		DWORD dw = GetLastError();
		if (dw == ERROR_FILE_NOT_FOUND)
			return 0;

		FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
			NULL,
			dw,
			MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
			error,
			sizeof(error),
			NULL);
		LogMessage("[META] Could not open folder \"%s\" (%s)", dir, error);
		return 0;
	}

	do
	{
		g_SmmAPI.PathFormat(path, sizeof(path), "%s\\%s", dir, fd.cFileName);
		success = LoadFromVDF(path, skip);
		if (skip)
			skipped++;
		else if (success)
			total++;
	} while (FindNextFile(hFind, &fd));

	FindClose(hFind);
#else
	DIR *pDir;
	struct dirent *pEnt;
	int extidx;

	if ((pDir = opendir(dir)) == NULL)
	{
		LogMessage("[META] Could not open folder \"%s\" (%s)", dir, strerror(errno));
		return 0;
	}

	while ((pEnt = readdir(pDir)) != NULL)
	{
		if (strcmp(pEnt->d_name, ".") == 0
			|| strcmp(pEnt->d_name, "..") == 0)
		{
			continue;
		}
		extidx = strlen(pEnt->d_name) - 4;
		if (extidx < 0 || stricmp(&pEnt->d_name[extidx], ".vdf"))
		{
			continue;
		}
		g_SmmAPI.PathFormat(path, sizeof(path), "%s/%s", dir, pEnt->d_name);
		success = LoadFromVDF(path, skip);
		if (skip)
			skipped++;
		else if (success)
			total++;
	}

	closedir(pDir);
#endif

	return total;
}

bool KVLoadFromFile(KeyValues *kv, IBaseFileSystem *filesystem, const char *resourceName, const char *pathID)
{
	Assert(filesystem);
#ifdef _MSC_VER
	Assert(_heapchk() == _HEAPOK);
#endif

	FileHandle_t f = filesystem->Open(resourceName, "rb", pathID);
	if (!f)
		return false;

	// load file into a null-terminated buffer
	int fileSize = filesystem->Size(f);
	char *buffer = (char *)MemAllocScratch(fileSize + 1);
	
	Assert(buffer);
	
	filesystem->Read(buffer, fileSize, f); // read into local buffer

	buffer[fileSize] = 0; // null terminate file as EOF

	filesystem->Close( f );	// close file after reading

	bool retOK = kv->LoadFromBuffer( resourceName, buffer, filesystem );

	MemFreeScratch();

	return retOK;
}

int LoadPluginsFromFile(const char *filepath, int &skipped)
{
	FILE *fp;
	int total = 0;
	PluginId id;
	bool already;

	skipped = 0;

	fp = fopen(filepath, "rt");
	if (!fp)
	{
		return 0;
	}

	char buffer[255], error[255], full_path[PATH_SIZE];
	const char *file;
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
		g_SmmAPI.GetFullPluginPath(file, full_path, sizeof(full_path));
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
	fclose(fp);

	return total;
}

int LoadPlugins(const char *filepath, const char *vdfpath)
{
	int total, skipped, fskipped, vskipped;
	const char *s = "";

	total = LoadPluginsFromFile(filepath, fskipped);
	total += LoadVDFPluginsFromDir(vdfpath, vskipped);
	skipped = fskipped + vskipped;

	if (total == 0 || total > 1)
		s = "s";

	if (skipped)
		LogMessage("[META] Loaded %d plugin%s (%d already loaded)", total, s, skipped);
	else
		LogMessage("[META] Loaded %d plugin%s.", total, s);

	return total;
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

	if (!g_Engine.engine)
	{
		fprintf(stdout, "%s", buffer);
	} else {
		g_Engine.engine->LogPrint(buffer);
	}
}

void LevelShutdown_handler(void)
{
	if (!bInFirstLevel)
	{
		char filepath[PATH_SIZE], vdfpath[PATH_SIZE];

		g_SmmAPI.PathFormat(filepath, sizeof(filepath), "%s/%s", g_ModPath.c_str(), GetPluginsFile());
		g_SmmAPI.PathFormat(vdfpath, sizeof(vdfpath), "%s/%s", g_ModPath.c_str(), GetMetamodBaseDir());
		LoadPlugins(filepath, vdfpath);
	}
	else
	{
		bInFirstLevel = false;
	}

	g_bLevelChanged = true;

	ITER_EVENT(OnLevelShutdown, ());

	RETURN_META(MRES_IGNORED);
}

bool LevelInit_handler(char const *pMapName, char const *pMapEntities, char const *pOldLevel, char const *pLandmarkName, bool loadGame, bool background)
{
	if (!g_SmmAPI.CmdCacheSuccessful())
	{
		LogMessage("[META] Warning: Failed to initialize Con_Printf.  Defaulting to Msg().");
		LogMessage("[META] Warning: Console messages will not be redirected to rcon console.");
	}

	ITER_EVENT(OnLevelInit, (pMapName, pMapEntities, pOldLevel, pLandmarkName, loadGame, background));

	RETURN_META_VALUE(MRES_IGNORED, false);
}

#if defined __GNUC__ && (__GNUC__ == 3 || __GNUC__ == 4)
void * operator new(size_t size) {
	return(calloc(1, size)); 
}

void * operator new[](size_t size) {
	return(calloc(1, size)); 
}

void operator delete(void * ptr) {
	if(ptr)
		free(ptr);
}

void operator delete[](void * ptr) {
	if(ptr)
		free(ptr);
}
#endif
