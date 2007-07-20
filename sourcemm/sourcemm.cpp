/* ======== SourceMM ========
 * Copyright (C) 2004-2007 Metamod:Source Development Team
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
#include "vsp_listener.h"

using namespace SourceMM;

/**
 * @brief Implementation of main SourceMM GameDLL functionality
 * @file sourcemm.cpp
 */

#undef CommandLine
DLL_IMPORT ICommandLine *CommandLine();

SH_DECL_HOOK4(IServerGameDLL, DLLInit, SH_NOATTRIB, false, bool, CreateInterfaceFn, CreateInterfaceFn, CreateInterfaceFn, CGlobalVars *);
SH_DECL_HOOK0_void(IServerGameDLL, DLLShutdown, SH_NOATTRIB, false);
SH_DECL_HOOK0_void(IServerGameDLL, LevelShutdown, SH_NOATTRIB, false);
SH_DECL_HOOK6(IServerGameDLL, LevelInit, SH_NOATTRIB, false, bool, const char *, const char *, const char *, const char *, bool, bool);
SH_DECL_HOOK1_void(IServerGameClients, ClientCommand, SH_NOATTRIB, 0, edict_t *);
SH_DECL_HOOK0(IServerGameDLL, GameInit, SH_NOATTRIB, false, bool);

bool DLLInit(CreateInterfaceFn engineFactory, CreateInterfaceFn physicsFactory, CreateInterfaceFn filesystemFactory, CGlobalVars *pGlobals);
bool DLLInit_Post(CreateInterfaceFn engineFactory, CreateInterfaceFn physicsFactory, CreateInterfaceFn filesystemFactory, CGlobalVars *pGlobals);
void DLLShutdown_handler();
void LevelShutdown_handler();
bool LevelInit_handler(char const *pMapName, char const *pMapEntities, char const *pOldLevel, char const *pLandmarkName, bool loadGame, bool background);
bool GameInit_handler();

GameDllInfo g_GameDll = {false, NULL, NULL, NULL, NULL};
EngineInfo g_Engine;
SourceHook::CSourceHookImpl g_SourceHook;
SourceHook::ISourceHook *g_SHPtr = &g_SourceHook;
SourceHook::String g_ModPath;
SourceHook::String g_BinPath;
SourceHook::String g_SmmPath;
PluginId g_PLID = Pl_Console;			/* Technically, SourceMM is the "Console" plugin... :p */
bool bInFirstLevel = true;
bool gParsedGameInfo = false;
bool bGameInit = false;
SourceHook::List<GameDllInfo *> gamedll_list;
int g_GameDllVersion = 0;
int g_GameClientsVersion = 0;
int g_VspVersion = 0;
const char VSPIFACE[] = "ISERVERPLUGINCALLBACKS";
const char GAMEINFO_PATH[] = "|gameinfo_path|";

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

///////////////////////////////////
// Main code for HL2 Interaction //
///////////////////////////////////

/* Initialize everything here */
void InitMainStates()
{
	char full_path[PATH_SIZE] = {0};
	GetFileOfAddress((void *)g_GameDll.factory, full_path, sizeof(full_path));
	g_BinPath.assign(full_path);

	/* Like Metamod, reload plugins at the end of the map.
	 * This is so plugins can hook everything on load, BUT, new plugins will be reloaded
	 * if the server is shut down (silly, but rare case).
	 */
	bInFirstLevel = true;

	SH_ADD_HOOK_STATICFUNC(IServerGameDLL, DLLInit, g_GameDll.pGameDLL, DLLInit, false);
	SH_ADD_HOOK_STATICFUNC(IServerGameDLL, DLLInit, g_GameDll.pGameDLL, DLLInit_Post, true);
	SH_ADD_HOOK_STATICFUNC(IServerGameDLL, DLLShutdown, g_GameDll.pGameDLL, DLLShutdown_handler, false);
	SH_ADD_HOOK_STATICFUNC(IServerGameDLL, LevelShutdown, g_GameDll.pGameDLL, LevelShutdown_handler, true);
	SH_ADD_HOOK_STATICFUNC(IServerGameDLL, LevelInit, g_GameDll.pGameDLL, LevelInit_handler, true);
	SH_ADD_HOOK_STATICFUNC(IServerGameDLL, GameInit, g_GameDll.pGameDLL, GameInit_handler, false);
}

bool DLLInit(CreateInterfaceFn engineFactory, CreateInterfaceFn physicsFactory, CreateInterfaceFn filesystemFactory, CGlobalVars *pGlobals)
{
	g_Engine.engineFactory = engineFactory;
	g_Engine.fileSystemFactory = filesystemFactory;
	g_Engine.physicsFactory = physicsFactory;
	g_Engine.pGlobals = pGlobals;

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

	/* Initialize our console hooks */
	ConCommandBaseMgr::OneTimeInit(static_cast<IConCommandBaseAccessor *>(&g_SMConVarAccessor));

	if (g_GameDll.pGameClients)
	{
		SH_ADD_HOOK_STATICFUNC(IServerGameClients, ClientCommand, g_GameDll.pGameClients, ClientCommand_handler, false);
	} else {
		/* If IServerGameClients isn't found, this really isn't a fatal error so... */
		LogMessage("[META] Warning: Could not find IServerGameClients!");
		LogMessage("[META] Warning: The 'meta' command will not be available to clients.");
	}

	if (!g_SmmAPI.CacheCmds())
	{
		LogMessage("[META] Warning: Failed to initialize Con_Printf.  Defaulting to Msg().");
		LogMessage("[META] Warning: Console messages will not be redirected to rcon console.");
	}

	if (!g_SmmAPI.CacheUserMessages())
	{
		/* Don't know of a mod that has stripped out user messages completely, 
		 * but perhaps should do something different here?
		 */
		LogMessage("[META] Warning: Failed to get list of user messages.");
		LogMessage("[META] Warning: The 'meta game' command will not display user messages.");
	}

	const char *pluginFile = g_Engine.icvar->GetCommandLineValue("mm_pluginsfile");
	if (!pluginFile) 
	{
		pluginFile = GetPluginsFile();
	}

	char full_path[260];
	g_SmmAPI.PathFormat(full_path, sizeof(full_path), "%s/%s", g_ModPath.c_str(), pluginFile);

	LoadPluginsFromFile(full_path);

	bInFirstLevel = true;

	RETURN_META_VALUE(MRES_IGNORED, true);
}

bool GameInit_handler()
{
	if (bGameInit)
	{
		return true;
	}

	if (g_SmmAPI.VSPEnabled())
	{
		g_SmmAPI.LoadAsVSP();
	}

	bGameInit = true;

	RETURN_META_VALUE(MRES_IGNORED, true);
}

bool DLLInit_Post(CreateInterfaceFn engineFactory, CreateInterfaceFn physicsFactory, CreateInterfaceFn filesystemFactory, CGlobalVars *pGlobals)
{
	g_PluginMngr.SetAllLoaded();
	RETURN_META_VALUE(MRES_IGNORED, true);
}

/* This is where the magic happens */
SMM_API void *CreateInterface(const char *iface, int *ret)
{
	/* Prevent loading of self as a SourceMM plugin or Valve server plugin :x */
	if (strcmp(iface, PLAPI_NAME) == 0)
	{
		Warning("Do not try loading Metamod:Source as a SourceMM or Valve server plugin.\n");

		if (ret)
		{
			*ret = IFACE_FAILED;
		}

		return NULL;
	}

	if (strncmp(iface, VSPIFACE, 22) == 0)
	{
		g_VspVersion = atoi(&(iface[22]));

		if (g_VspVersion <= MAX_VSP_VERSION)
		{
			if (ret)
			{
				*ret = IFACE_OK;
			}
			return &g_VspListener;
		}
	}

	if (!gParsedGameInfo)
	{
		gParsedGameInfo = true;
		const char *gameDir = NULL;
		char gamePath[PATH_SIZE];
		char smmPath[PATH_SIZE];

		/* Get path to SourceMM DLL */
		if (!GetFileOfAddress((void *)CreateInterface, smmPath, sizeof(smmPath)))
		{
			Error("GetFileOfAddress() failed! Metamod cannot load.\n");
			return NULL;
		}

		g_SmmPath.assign(smmPath);

		/* Get value of -game from command line, defaulting to hl2 as engine seems to do */
		gameDir = CommandLine()->ParmValue("-game", "hl2");

		/* Get absolute path */
		abspath(gamePath, gameDir);
		g_ModPath.assign(gamePath);

		char tempPath[PATH_SIZE];

		/* Path to gameinfo.txt */
		g_SmmAPI.PathFormat(tempPath, PATH_SIZE, "%s/%s", g_ModPath.c_str(), "gameinfo.txt");

		FILE *fp = fopen(tempPath, "rt");

		if (!fp)
		{
			Error("Unable to open gameinfo.txt!  Metamod cannot load.\n");
			return NULL;
		}

		char buffer[255];
		char key[128], val[128];
		bool search = false;
		bool gamebin = false;
		char *ptr;
		const char *lptr;
		char curPath[PATH_SIZE];

		getcwd(curPath, PATH_SIZE);

		while (!feof(fp))
		{
			buffer[0] = '\0';
			fgets(buffer, sizeof(buffer), fp);
			UTIL_TrimComments(buffer);
			UTIL_TrimLeft(buffer);
			UTIL_TrimRight(buffer);
			if (stricmp(buffer, "SearchPaths") == 0)
				search = true;
			if (!search)
				continue;
			UTIL_KeySplit(buffer, key, sizeof(key) - 1, val, sizeof(val) - 1);
			if (stricmp(key, "Game") == 0 || stricmp(key, "GameBin") == 0)
			{
				if (stricmp(key, "Game") == 0)
					gamebin = false;
				else
					gamebin = true;

				if (strncmp(val, GAMEINFO_PATH, sizeof(GAMEINFO_PATH) - 1) == 0)
				{
					ptr = &(val[sizeof(GAMEINFO_PATH) - 1]);
					if (ptr[0] == '.')
						ptr++;
					lptr = g_ModPath.c_str();
				} else {
					ptr = val;
					lptr = curPath;
				}

				size_t ptr_len = strlen(ptr);
				if (ptr[ptr_len] == '/' || ptr[ptr_len] == '\\')
					ptr[--ptr_len] = '\0';

				/* No need to append "bin" if key is GameBin */
				if (gamebin)
				{
					g_SmmAPI.PathFormat(tempPath, PATH_SIZE, "%s/%s/%s", lptr, ptr, SERVER_DLL);
				} else if (!ptr[0]) {
					g_SmmAPI.PathFormat(tempPath, PATH_SIZE, "%s/%s/%s", lptr, "bin", SERVER_DLL);
				} else {
					g_SmmAPI.PathFormat(tempPath, PATH_SIZE, "%s/%s/%s/%s", lptr, ptr, "bin", SERVER_DLL);
				}

				/* If not path to SourceMM... */
				if (!UTIL_PathCmp(smmPath, tempPath))
				{
					FILE *fp = fopen(tempPath, "rb");
					if (!fp)
						continue;
					//:TODO: Optimize this a bit!
					SourceHook::List<GameDllInfo *>::iterator iter;
					GameDllInfo *pCheck;
					bool found = false;
					for (iter=gamedll_list.begin(); iter!=gamedll_list.end(); iter++)
					{
						pCheck = (*iter);
						if (GetFileOfAddress((void *)pCheck->factory, buffer, sizeof(buffer)))
						{
							if (UTIL_PathCmp(tempPath, buffer))
							{
								found = true;
								break;
							}
						}
					}
					if (found)
						continue;
					fclose(fp);
					HINSTANCE gamedll = dlmount(tempPath);
					if (gamedll == NULL)
						continue;
					CreateInterfaceFn fn = (CreateInterfaceFn)dlsym(gamedll, "CreateInterface");
					if (fn == NULL)
					{
						dlclose(gamedll);
						continue;
					}
					GameDllInfo *pInfo = new GameDllInfo;
					pInfo->factory = fn;
					pInfo->lib = gamedll;
					pInfo->loaded = true;
					pInfo->pGameDLL = NULL;
					gamedll_list.push_back(pInfo);
					break;
				}
			}
		}
		fclose(fp);
	}

	if (!g_GameDll.loaded)
	{
		const char *str = "ServerGameDLL";
		size_t len = strlen(str);

		if (strncmp(iface, str, len) == 0)
		{
			/* This is the interface we want!  Right now we support versions 3 through 8 */
			g_GameDllVersion = atoi(&(iface[len]));
			int sizeTooBig = 0;	//rename this to sizeWrong in the future!
			if (g_GameDllVersion < MIN_GAMEDLL_VERSION || g_GameDllVersion > MAX_GAMEDLL_VERSION)
			{
				/* Maybe this will get used in the future */
				sizeTooBig = g_GameDllVersion;
				if (ret)
				{
					*ret = IFACE_FAILED;
				}
			}
			SourceHook::List<GameDllInfo *>::iterator iter;
			GameDllInfo *pInfo = NULL;
			void *ptr;
			for (iter=gamedll_list.begin(); iter!=gamedll_list.end(); iter++)
			{
				pInfo = (*iter);
				ptr = (pInfo->factory)(iface, ret);
				if (ptr)
				{
					/* This is our GameDLL. Unload the others. */
					gamedll_list.erase(iter);
					ClearGamedllList();
					pInfo->pGameDLL = static_cast<IServerGameDLL *>(ptr);
					g_GameDll = *pInfo;
					delete pInfo;
					break;
				}
			}
			if (g_GameDll.loaded)
			{
				if (sizeTooBig)
				{
					Error("This mod version requires a SourceMM update (ServerGameDLL%03d)!\n", sizeTooBig);
					if (ret)
					{
						*ret = IFACE_FAILED;
					}
					return NULL;
				} else {
					InitMainStates();
				}
			} else {
				sizeTooBig = 0;
				if (ret)
					*ret = IFACE_FAILED;
				return NULL;
			}
		} else {
			/* wtf do we do... */
			/* :TODO: .. something a bit more intelligent? */
			Error("Engine requested unknown interface before GameDLL was known!\n");
			return NULL;
		}
	}

	/* We use this interface for responding to the meta client command */
	if (strncmp(iface, "ServerGameClients", 17) == 0)
	{
		void *ptr = (g_GameDll.factory)(iface, ret);
		g_GameDll.pGameClients = static_cast<IServerGameClients *>(ptr);
		g_GameClientsVersion = atoi(&iface[17]);

		return ptr;
	}

	/* If we got here, there's definitely a GameDLL */
	IFACE_MACRO(g_GameDll.factory, GameDLL);
}

void ClearGamedllList()
{
	SourceHook::List<GameDllInfo *>::iterator iter;

	GameDllInfo *pInfo;
	for (iter=gamedll_list.begin(); iter!=gamedll_list.end(); iter++)
	{
		pInfo = (*iter);
		dlclose(pInfo->lib);
		delete pInfo;
	}

	gamedll_list.clear();
}

void DLLShutdown_handler()
{
	/* Unload plugins */
	g_PluginMngr.UnloadAll();

	/* Add the FCVAR_GAMEDLL flag to our cvars so the engine removes them properly */
	g_SMConVarAccessor.MarkCommandsAsGameDLL();
	g_SMConVarAccessor.UnregisterGameDLLCommands();

	SH_CALL(g_GameDll.pGameDLL, &IServerGameDLL::DLLShutdown)();

	g_SourceHook.CompleteShutdown();

	if (g_GameDll.lib && g_GameDll.loaded)
		dlclose(g_GameDll.lib);
	memset(&g_GameDll, 0, sizeof(GameDllInfo));

	RETURN_META(MRES_SUPERCEDE);
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
	while (!feof(fp))
	{
		buffer[0] = '\0';
		fgets(buffer, sizeof(buffer), fp);
		length = strlen(buffer);
		if (!length)
			continue;
		if (buffer[length-1] == '\n')
			buffer[--length] = '\0';

		UTIL_TrimLeft(buffer);
		UTIL_TrimRight(buffer);

		if (buffer[0] == '\0' || buffer[0] == ';' || strncmp(buffer, "//", 2) == 0)
			continue;
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
		} else {
			char *cptr = buffer;
			while (*cptr)
			{
				if (isspace(*cptr))
				{
					char *optr = cptr;
					while (*cptr && isspace(*cptr))
						cptr++;
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
			} else {
				if (already)
					skipped++;
				else
					total++;
			}
		} else {
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
			} else {
				ext = "";
			}
			/* Format the new path */
			g_SmmAPI.PathFormat(full_path, sizeof(full_path), "%s/%s%s", g_ModPath.c_str(), file, ext);
			id = g_PluginMngr.Load(full_path, Pl_File, already, error, sizeof(error));
			if (id < Pl_MinId || g_PluginMngr.FindById(id)->m_Status < Pl_Paused)
			{
				LogMessage("[META] Failed to load plugin %s.  %s", buffer, error);
			} else {
				if (already)
					skipped++;
				else
					total++;
			}
		}
	}
	fclose(fp);

	if (skipped)
	{
		LogMessage("[META] Loaded %d plugins from file (%d already loaded)", total, skipped);
	} else {
		LogMessage("[META] Loaded %d plugins from file.", total);
	}
	
	return total;
}

/* Wrapper function.  This is called when the GameDLL thinks it's using
 * the engine's real engineFactory.
 */
void *EngineFactory(const char *iface, int *ret)
{
	IFACE_MACRO(g_Engine.engineFactory, Engine);
}

/* Wrapper function.  This is called when the GameDLL thinks it's using
 * the engine's real physicsFactory.
 */
void *PhysicsFactory(const char *iface, int *ret)
{
	IFACE_MACRO(g_Engine.physicsFactory, Physics);
}

/* Wrapper function.  This is called when the GameDLL thinks it's using
 * the engine's real fileSystemFactory.
 */
void *FileSystemFactory(const char *iface, int *ret)
{
	IFACE_MACRO(g_Engine.fileSystemFactory, FileSystem);
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
		char full_path[255];
		g_SmmAPI.PathFormat(full_path, sizeof(full_path), "%s/%s", g_ModPath.c_str(), GetPluginsFile());

		LoadPluginsFromFile(full_path);
	} else {
		bInFirstLevel = false;
	}

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
