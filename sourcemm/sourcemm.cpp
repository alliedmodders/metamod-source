/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
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
#include "sourcemm.h"
#include "concommands.h"
#include "CSmmAPI.h"
#include "util.h"

/**
 * @brief Implementation of main SourceMM GameDLL functionality
 * @file sourcemm.cpp
 */

SH_DECL_HOOK4(IServerGameDLL, DLLInit, SH_NOATTRIB, false, bool, CreateInterfaceFn, CreateInterfaceFn, CreateInterfaceFn, CGlobalVars *);
SH_DECL_HOOK0_void(IServerGameDLL, DLLShutdown, SH_NOATTRIB, false);
SH_DECL_HOOK0_void(IServerGameDLL, LevelShutdown, SH_NOATTRIB, false);
SH_DECL_HOOK6(IServerGameDLL, LevelInit, SH_NOATTRIB, false, bool, const char *, const char *, const char *, const char *, bool, bool);
bool DLLInit(CreateInterfaceFn engineFactory, CreateInterfaceFn physicsFactory, CreateInterfaceFn filesystemFactory, CGlobalVars *pGlobals);
void DLLShutdown_handler();
void LevelShutdown_handler();
bool LevelInit_handler(char const *pMapName, char const *pMapEntities, char const *pOldLevel, char const *pLandmarkName, bool loadGame, bool background);

GameDllInfo g_GameDll = {false, NULL, NULL, NULL};
EngineInfo g_Engine;
SourceHook::CSourceHookImpl g_SourceHook;
SourceHook::ISourceHook *g_SHPtr = &g_SourceHook;
SourceHook::String g_ModPath;
SourceHook::String g_BinPath;
PluginId g_PLID = Pl_Console;		//Technically, SourceMM is the "Console" plugin... :p
bool bInFirstLevel = true;
bool gParsedGameInfo = false;
SourceHook::List<GameDllInfo *> gamedll_list;
SourceHook::CallClass<IServerGameDLL> *dllExec;

void ClearGamedllList();

///////////////////////////////////
// Main code for HL2 Interaction //
///////////////////////////////////

//Initialize everything here
void InitMainStates()
{
	char full_path[260] = {0};
	GetFileOfAddress(g_GameDll.factory, full_path, sizeof(full_path)-1);
	g_BinPath.assign(full_path);

	UTIL_PathFmt(full_path, sizeof(full_path)-1, "%s/%s", g_ModPath.c_str(), GetPluginsFile());

	//Like metamod, reload plugins at the end of the map.
	//This is so plugins can hook everything on load, BUT, new plugins will be reloaded
	// if the server is shut down (silly, but rare case).
	bInFirstLevel = true;

	SH_ADD_HOOK_STATICFUNC(IServerGameDLL, DLLInit, g_GameDll.pGameDLL, DLLInit, false);
	SH_ADD_HOOK_STATICFUNC(IServerGameDLL, DLLShutdown, g_GameDll.pGameDLL, DLLShutdown_handler, false);
	SH_ADD_HOOK_STATICFUNC(IServerGameDLL, LevelShutdown, g_GameDll.pGameDLL, LevelShutdown_handler, true);
	SH_ADD_HOOK_STATICFUNC(IServerGameDLL, LevelInit, g_GameDll.pGameDLL, LevelInit_handler, true);
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

	//Initialize our console hooks
	ConCommandBaseMgr::OneTimeInit(static_cast<IConCommandBaseAccessor *>(&g_SMConVarAccessor));

	if (!g_SmmAPI.CacheCmds())
	{
		LogMessage("[META] Warning: Failed to initialize Con_Printf.  Defaulting to Msg().");
		LogMessage("[META] Warning: Console messages will not be redirected to rcon console.");
	}

	char full_path[260];
	UTIL_PathFmt(full_path, sizeof(full_path)-1, "%s/%s", g_ModPath.c_str(), GetPluginsFile());

	LoadPluginsFromFile(full_path);

	g_PluginMngr.SetAllLoaded();

	bInFirstLevel = true;

	dllExec = SH_GET_CALLCLASS(g_GameDll.pGameDLL);

	RETURN_META_VALUE(MRES_IGNORED, true);
}

//This is where the magic happens
SMM_API void *CreateInterface(const char *name, int *ret)
{
	if (!gParsedGameInfo)
	{
		gParsedGameInfo = true;
		char curpath[260] = {0};
		char dllpath[260] = {0};
		getcwd(curpath, sizeof(curpath)-1);
		if (!GetFileOfAddress((void *)CreateInterface, dllpath, sizeof(dllpath)-1))
		{
			Error("GetFileOfAddress() failed! Metamod cannot load.");
			return NULL;
		}
		SourceHook::String s_dllpath(dllpath);
		//begin the heuristics for searching for the mod path (these are quite ugly).
		//for OS compatibility purposes, we're going to do case insensitivity on windows.
		size_t path_len = strlen(curpath);
		size_t dll_len = strlen(dllpath);

		//strip the dll path off
		//:TODO: with path stuff - in Linux, \ can exist in a file path as a non-seperator!
		for (size_t i=dll_len-1; i>0; i--)
		{
			if (dllpath[i] == '\\' || dllpath[i] == '/')
			{
				if (i == dll_len-1)
					break;
				//save path by stripping off file name and ending terminator
				dllpath[i] = '\0';
				dll_len = i;
				break;
			}
		}

		//strip absolute path terminators if any!
		if (curpath[path_len-1] == '/' || curpath[path_len-1] == '\\')
			curpath[--path_len] = '\0';

		//if the base path doesn't fit into the module path, something is wrong!
		// ex: c:\games\srcds
		//     c:\gaben.dll
		if (path_len > dll_len)
		{
			Error("Could not detect GameDLL path! Metamod cannot load[1].");
			return NULL;
		}

		//we are now in such a position that the two dir names SHOULD MATCH!
		typedef int (*STRNCMP)(const char *str1, const char *str2, size_t n);
		STRNCMP cmp = 
#ifdef WIN32
			strnicmp;
#else
			strncmp;
#endif
		//are they equal?
		if ( ((cmp)(curpath, dllpath, path_len)) != 0 )
		{
			//:TODO: In this case, we should read /proc/self/maps and find srcds!
			Error("Could not detect GameDLL path! Metamod cannot load[2].");
			return NULL;
		}
		//this will skip past the dir and its separator char
		char *ptr = &(dllpath[path_len+1]);
		path_len = strlen(ptr);
		for (size_t i=0; i<path_len; i++)
		{
			if (ptr[i] == '/' || ptr[i] == '\\')
			{
				if (i == 0)
				{
					Error("Could not detect GameDLL path! Metamod cannot load[3].");
					return NULL;
				} else {
					ptr[i] = '\0';
					path_len = i+1;
					break;
				}
			}
		}
		//WE NOW HAVE A GUESS AT THE MOD DIR.  OH MY GOD.
		char temp_path[260];
		snprintf(temp_path, sizeof(temp_path)-1, 
			"%s%s%s", 
			curpath, 
			PATH_SEP_STR, 
			ptr);

		g_ModPath.assign(temp_path);

		snprintf(temp_path, sizeof(temp_path)-1, 
			"%s%s%s", 
			g_ModPath.c_str(),
			PATH_SEP_STR,
			"gameinfo.txt");

		FILE *fp = fopen(temp_path, "rt");
		if (!fp)
		{
			Error("Unable to open gameinfo.txt!  Metamod cannot load.");
			return NULL;
		}
		char buffer[255];
		char key[128], val[128];
		size_t len = 0;
		const char *gameinfo = "|gameinfo_path|";
		size_t gameinfo_len = strlen(gameinfo);
		bool search = false;
		bool gamebin = false;
		const char *lptr;
		while (!feof(fp))
		{
			buffer[0] = '\0';
			fgets(buffer, sizeof(buffer)-1, fp);
			len = strlen(buffer);
			if (buffer[len-1] == '\n')
				buffer[--len] = '\0';
			UTIL_TrimComments(buffer);
			UTIL_TrimLeft(buffer);
			UTIL_TrimRight(buffer);
			if (stricmp(buffer, "SearchPaths") == 0)
				search = true;
			if (!search)
				continue;
			UTIL_KeySplit(buffer, key, sizeof(key)-1, val, sizeof(val)-1);
			if (stricmp(key, "Game") == 0 || stricmp(key, "GameBin") == 0)
			{
				if (stricmp(key, "Game") == 0)
					gamebin = false;
				else
					gamebin = true;

				if (strncmp(val, gameinfo, gameinfo_len) == 0)
				{
					ptr = &(val[gameinfo_len]);
					if (ptr[0] == '.')
						ptr++;
					lptr = g_ModPath.c_str();
				} else {
					lptr = curpath;
					ptr = val;
				}
				size_t ptr_len = strlen(ptr);
				if (ptr[ptr_len] == '/' || ptr[ptr_len] == '\\')
					ptr[--ptr_len] = '\0';
				//no need to append "bin"
				if (gamebin)
				{
					UTIL_PathFmt(temp_path, sizeof(temp_path)-1, "%s/%s/%s", lptr, ptr, SERVER_DLL);
				} else {
					UTIL_PathFmt(temp_path, sizeof(temp_path)-1, "%s/%s/%s/%s", lptr, ptr, "bin", SERVER_DLL);
				}
				if (!UTIL_PathCmp(s_dllpath.c_str(), temp_path))
				{
					FILE *fp = fopen(temp_path, "rb");
					if (!fp)
						continue;
					//:TODO: Optimize this a bit!
					SourceHook::List<GameDllInfo *>::iterator iter;
					GameDllInfo *pCheck;
					bool found = false;
					for (iter=gamedll_list.begin(); iter!=gamedll_list.end(); iter++)
					{
						pCheck = (*iter);
						if (GetFileOfAddress(pCheck->factory, buffer, sizeof(buffer)-1))
						{
							if (UTIL_PathCmp(temp_path, buffer))
							{
								found = true;
								break;
							}
						}
					}
					if (found)
						continue;
					fclose(fp);
					HINSTANCE gamedll = dlmount(temp_path);
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
				}
			}
		}
		fclose(fp);
	}

	if (!g_GameDll.loaded)
	{
		const char *str = "ServerGameDLL";
		size_t len = strlen(str);

		if (strncmp(name, str, len) == 0)
		{
			//This is the interface we want!  Right now we support versions 3 and 4.
			int version = atoi(&(name[len]));
			if (version < MIN_GAMEDLL_VERSION || version > MAX_GAMEDLL_VERSION)
			{
				Error("GameDLL version %d is not supported by Metamod!", version);
				return NULL;
			}
			SourceHook::List<GameDllInfo *>::iterator iter;
			GameDllInfo *pInfo = NULL;
			void *ptr;
			for (iter=gamedll_list.begin(); iter!=gamedll_list.end(); iter++)
			{
				pInfo = (*iter);
				ptr = (pInfo->factory)(name, ret);
				if (ptr)
				{
					//this is our gamedll.  unload the others.
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
				InitMainStates();
			} else {
				if (ret)
					*ret = IFACE_FAILED;
				return NULL;
			}
		} else {
			//wtf do we do...
			//:TODO: .. something a bit more intelligent?
			Error("Engine requested unknown interface before GameDLL was known!");
			return NULL;
		}
	}

		SetUnhandledExceptionFilter(NULL);

	//if we got here, there's definitely a gamedll.
	//META_INTERFACE_MACRO(server, g_GameDll.factory);
	return (g_GameDll.factory)(name, ret);
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
	//Unload plugins
	g_PluginMngr.UnloadAll();

	// Add the FCVAR_GAMEDLL flag to our cvars so the engine removes them properly
	g_SMConVarAccessor.MarkCommandsAsGameDLL();
	g_SMConVarAccessor.UnregisterGameDLLCommands();

	SH_CALL(dllExec, &IServerGameDLL::DLLShutdown)();

	SH_RELEASE_CALLCLASS(dllExec);
	dllExec = NULL;

	//right now this will crash when the function returns!
	// :TODO: remove this warning once PM fixes it.
	g_SourceHook.CompleteShutdown();

	if (g_GameDll.lib && g_GameDll.loaded)
		dlclose(g_GameDll.lib);
	memset(&g_GameDll, 0, sizeof(GameDllInfo));

	RETURN_META(MRES_SUPERCEDE);
}

int LoadPluginsFromFile(const char *file)
{
	FILE *fp;
	int total = 0, skipped=0;
	PluginId id;
	bool already;

	fp = fopen(file, "rt");
	if (!fp)
	{
		LogMessage("[META] Could not open plugins file %s\n", file);
		return -1;
	}

	char buffer[255], error[255], full_path[128];
	const char *ptr, *ext;
	size_t length;
	while (!feof(fp))
	{
		buffer[0] = '\0';
		fgets(buffer, sizeof(buffer)-1, fp);
		length = strlen(buffer);
		if (!length)
			continue;
		if (buffer[length-1] == '\n')
			buffer[length-1] = '\0';

		UTIL_TrimLeft(buffer);
		UTIL_TrimRight(buffer);

		if (buffer[0] == '\0' || buffer[0] == ';' || strncmp(buffer, "//", 2) == 0)
			continue;
		//First find if it's an absolute path or not...
		if (buffer[0] == '/' || strncmp(&(buffer[1]), ":\\", 2) == 0)
		{
			//If we're in an absolute path, ignore our normal heuristics
			id = g_PluginMngr.Load(buffer, Pl_File, already, error, sizeof(error)-1);
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
			//Attempt to find a file extension
			ptr = UTIL_GetExtension(buffer);
			//Add an extension if there's none there
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
			//Format the new path
			UTIL_PathFmt(full_path, sizeof(full_path)-1, "%s/%s", g_ModPath.c_str(), buffer, ext);
			id = g_PluginMngr.Load(full_path, Pl_File, already, error, sizeof(error)-1);
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

//Wrapper function.  This is called when the GameDLL thinks it's using
// the engine's real engineFactory.
void *EngineFactory(const char *name, int *ret)
{
	META_INTERFACE_MACRO(engine, g_Engine.engineFactory);
}

//Wrapper function.  This is called when the GameDLL thinks it's using
// the engine's real physicsFactory.
void *PhysicsFactory(const char *name, int *ret)
{
	META_INTERFACE_MACRO(physics, g_Engine.physicsFactory);
}

//Wrapper function.  This is called when the GameDLL thinks it's using
// the engine's real fileSystemFactory.
void *FileSystemFactory(const char *name, int *ret)
{
	META_INTERFACE_MACRO(fileSystem, g_Engine.fileSystemFactory);
}

void LogMessage(const char *msg, ...)
{
	va_list ap;
	static char buffer[2048];

	buffer[0] = '\0';

	va_start(ap, msg);
	vsnprintf(buffer, sizeof(buffer)-5, msg, ap);
	strcat(buffer, "\n");
	va_end(ap);

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
#if defined WIN32 || defined _WIN32
		snprintf(full_path, sizeof(full_path)-1, "%s\\addons\\metamod\\%s", g_ModPath.c_str(), "metaplugins.ini");
#else
		snprintf(full_path, sizeof(full_path)-1, "%s/addons/metamod/%s", g_ModPath.c_str(), "metaplugins.ini");
#endif
		LoadPluginsFromFile(full_path);
	} else {
		bInFirstLevel = false;
	}
}

bool LevelInit_handler(char const *pMapName, char const *pMapEntities, char const *pOldLevel, char const *pLandmarkName, bool loadGame, bool background)
{ 
	if (!g_SmmAPI.CacheSuccessful())
	{
		LogMessage("[META] Warning: Failed to initialize Con_Printf.  Defaulting to Msg().");
		LogMessage("[META] Warning: Console messages will not be redirected to rcon console.");
	}

	RETURN_META_VALUE(MRES_IGNORED, false);
}

#if defined __GNUC__ && (__GNUC__ == 3)
void * ::operator new(size_t size) {
	return(calloc(1, size)); 
}

void * ::operator new[](size_t size) {
	return(calloc(1, size)); 
}

void ::operator delete(void * ptr) {
	if(ptr)
		free(ptr);
}

void ::operator delete[](void * ptr) {
	if(ptr)
		free(ptr);
}
#endif
