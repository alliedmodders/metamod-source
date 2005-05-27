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
#include "CServerGameDLL.h"
#include "CServerGameEnts.h"
#include "CServerGameClients.h"
#include "CHLTVDirector.h"
#include "sourcemm.h"
#include "concommands.h"
#include "CSmmAPI.h"
#include "util.h"

/**
 * @brief Implementation of main SourceMM GameDLL functionality
 * @file sourcemm.cpp
 */

CServerGameDLL g_TempGameDLL;
CServerGameEnts g_TempGameEnts;
CServerGameClients g_TempGameClients;
CHLTVDirector g_TempDirector;
GameDllInfo g_GameDll = {false, NULL, NULL};
EngineInfo g_Engine = {NULL, NULL, NULL, NULL};
SourceHook::CSourceHookImpl g_SourceHook;
SourceHook::ISourceHook *g_SHPtr;
std::string g_ModPath;
std::string g_BinPath;
PluginId g_PLID = Pl_Console;		//Technically, SourceMM is the "Console" plugin... :p
bool bInShutdown = false;
bool bInFirstLevel = true;

///////////////////////////////////
// Main code for HL2 Interaction //
///////////////////////////////////

//This is where the magic happens
SMM_API void *CreateInterface(const char *name, int *ret)
{
	if (!g_GameDll.loaded)
	{
		//State of the Mod:
		// Currently, HL2 Engine has loaded Metamod:Source
		// It is now asking it to get an interface.  We don't have one,
		//  so we're gonna try to give it a fake one to get the information we need.
		//  Then, the the interface will forward the calls to the original interface

		if (strcmp(name, INTERFACEVERSION_SERVERGAMEDLL) == 0)
		{
			//We're in.  Give the server our fake class as bait.
			if (ret)
				*ret = IFACE_OK;
			return static_cast<void *>(&g_TempGameDLL);
		} else if (strcmp(name, INTERFACEVERSION_SERVERGAMEENTS) == 0) {
			if (ret)
				*ret = IFACE_OK;

			return static_cast<void *>(&g_TempGameEnts);
		} else if (strcmp(name, INTERFACEVERSION_SERVERGAMECLIENTS) == 0) {
			if (ret)
				*ret = IFACE_OK;

			return static_cast<void *>(&g_TempGameClients);
		} else if (strcmp(name, INTERFACEVERSION_HLTVDIRECTOR) == 0) {
			if (ret)
				*ret = IFACE_OK;

			return static_cast<void *>(&g_TempDirector);
		} else {
			if (ret)
				*ret = IFACE_FAILED;

			return NULL;
		}
	} else {
		META_INTERFACE_MACRO(server, g_GameDll.factory);
	}
}

bool CServerGameDLL::DLLInit(CreateInterfaceFn engineFactory, CreateInterfaceFn physicsFactory, CreateInterfaceFn fileSystemFactory, CGlobalVars *pGlobals)
{
	if (m_pOrig)
		return m_pOrig->DLLInit(engineFactory, physicsFactory, fileSystemFactory, pGlobals);

	if (!g_GameDll.loaded)
	{
		//Initialize SourceHook
		g_SHPtr = static_cast<SourceHook::ISourceHook *>(&g_SourceHook);

		//The gamedll isn't loaded yet.  We need to find out where it's hiding.
		IVEngineServer *ive = (IVEngineServer *)((engineFactory)(INTERFACEVERSION_VENGINESERVER, NULL));
		if (!ive)
		{
			Error("Metamod:Source could not load %s", INTERFACEVERSION_VENGINESERVER);
			return false;
		}
		
		//Guess the file name
		//We'll have better heurestics[sp?] later on
		char mod_path[128], full_path[255];
		ive->GetGameDir(mod_path, sizeof(mod_path)-1);
		g_ModPath.assign(mod_path);
#if defined WIN32 || defined _WIN32
		snprintf(full_path, sizeof(full_path)-1, "%s\\bin\\server.dll", mod_path);
#else
		snprintf(full_path, sizeof(full_path)-1, "%s/bin/server_i486.so", mod_path);
#endif

		g_BinPath.assign(full_path);

		//See if the file even exists
		FILE *fp = fopen(g_BinPath.c_str(), "r");
		if (!fp)
		{
			Error("Metamod:Source could not read %s", g_BinPath.c_str());
			return false;
		}
		fclose(fp);
		fp = NULL;

		//Load the DLL
		g_GameDll.lib = dlmount(g_BinPath.c_str());
		if (!g_GameDll.lib)
		{
			Error("Metamod:Source could not load GameDLL: %s", dlerror());
			return false;
		} else {
			//Find its factory
			g_GameDll.factory = (CreateInterfaceFn)(dlsym(g_GameDll.lib, "CreateInterface"));
			if (!g_GameDll.factory)
			{
				Error("Metamod:Source could not find an entry point in GameDLL: %s", g_BinPath.c_str());
				dlclose(g_GameDll.lib);
				return false;
			}

			//Find the new IServerGameDLL pointer
			IServerGameDLL *serverDll;
			serverDll = (IServerGameDLL *)((g_GameDll.factory)(INTERFACEVERSION_SERVERGAMEDLL, NULL));
			if (!serverDll)
			{
				Error("Metamod:Source could not find %s in GameDLL: %s", INTERFACEVERSION_SERVERGAMEDLL, g_BinPath.c_str());
				dlclose(g_GameDll.lib);
				return false;
			}

			//Set this information early in case our wrappers are called somehow
			g_Engine.engineFactory = engineFactory;
			g_Engine.icvar = (ICvar *)(g_Engine.engineFactory)(VENGINE_CVAR_INTERFACE_VERSION, NULL);
			if (!g_Engine.icvar)
			{
				Error("Metamod:Source could not find %s in engine!", VENGINE_CVAR_INTERFACE_VERSION);
				dlclose(g_GameDll.lib);
				return false;
			}
			g_Engine.fileSystemFactory = fileSystemFactory;
			g_Engine.pGlobals = pGlobals;
			g_Engine.physicsFactory = physicsFactory;
			g_Engine.engine = ive;
			
			//Attempt to load the GameDLL
			// Note that nothing will be intercepting yet.
			// This is the one and only call that plugins have no chance of seeing.
			// Likewise, you won't be able to trick the Server DLL into loading random things.
			// Luckily, because of SourceHook, this really isn't a problem - you can change
			//  the virtual interfaces in anything it requests.
			if (!serverDll->DLLInit(EngineFactory, PhysicsFactory, FileSystemFactory, pGlobals))
			{
				//For some reason, the GameDLL failed to load. 
				Error("Metamod:Source: GameDLL %s refused to load.", g_BinPath.c_str());
				dlclose(g_GameDll.lib);
				return false;
			}

			//Retrieve the pointers we'll need from the GameDLL
			IServerGameEnts *serverEnts;
			IServerGameClients *serverClients;
			IHLTVDirector *serverHLTV;

			serverEnts = (IServerGameEnts *)((g_GameDll.factory)(INTERFACEVERSION_SERVERGAMEENTS, NULL));
			if (!serverEnts)
			{
				Error("Metamod:Source could not find %s in GameDLL: %s", INTERFACEVERSION_SERVERGAMEENTS, g_BinPath.c_str());
				dlclose(g_GameDll.lib);
				return false;
			}

			serverClients = (IServerGameClients *)((g_GameDll.factory)(INTERFACEVERSION_SERVERGAMECLIENTS, NULL));
			if (!serverClients)
			{
				Error("Metamod:Source could not find %s in GameDLL: %s", INTERFACEVERSION_SERVERGAMECLIENTS, g_BinPath.c_str());
				dlclose(g_GameDll.lib);
				return false;
			}

            serverHLTV = (IHLTVDirector *)((g_GameDll.factory)(INTERFACEVERSION_HLTVDIRECTOR, NULL));
			if (!serverHLTV)
			{
				Error("Metamod:Source could not find %s in GameDLL: %s", INTERFACEVERSION_HLTVDIRECTOR, g_BinPath.c_str());
				dlclose(g_GameDll.lib);
				return false;
			}
			
			// Now tell the global temp classes that they can call the original functions
			g_TempDirector.SetOrig(serverHLTV);
			g_TempGameClients.SetOrig(serverClients);
			g_TempGameEnts.SetOrig(serverEnts);
			g_TempGameDLL.SetOrig(serverDll);

			//Everything's done.
			g_GameDll.loaded = true;

			//Initialize our console hooks
			ConCommandBaseMgr::OneTimeInit(static_cast<IConCommandBaseAccessor *>(&g_SMConVarAccessor));
            
			//Now it's safe to load plugins.
#if defined WIN32 || defined _WIN32
			snprintf(full_path, sizeof(full_path)-1, "%s\\addons\\metamod\\%s", g_ModPath.c_str(), "metaplugins.ini");
#else
			snprintf(full_path, sizeof(full_path)-1, "%s/addons/metamod/%s", g_ModPath.c_str(), "metaplugins.ini");
#endif

			LoadPluginsFromFile(full_path);

			//All plugins are now loaded.
			g_PluginMngr.SetAllLoaded();

			//Like metamod, reload plugins at the end of the map.
			//This is so plugins can hook everything on load, BUT, new plugins will be reloaded
			// if the server is shut down (silly, but rare case).
			bInFirstLevel = true;

			return true;
		}
	}

	//Somehow, the function got here.  This should be impossible.
	Error("Metamod:Source fatal error - IServerGameDLL::DLLInit() called inappropriately");

	return false;
}

// The engine uses the DLL even after it has call DLLShutdown, so we unload it
// when it unloads us
#if defined _WIN32
	BOOL WINAPI DllMain(
						HINSTANCE hinstDLL,
						DWORD fdwReason,
						LPVOID lpvReserved
						)
	{
		if (fdwReason == DLL_PROCESS_DETACH)
		{
			if (g_GameDll.lib && g_GameDll.loaded)
				dlclose(g_GameDll.lib);
			memset(&g_GameDll, 0, sizeof(GameDllInfo));
		}
		return TRUE;
	}
#elif defined __linux__
	void __attribute__ ((destructor)) app_fini(void)
	{
		if (g_GameDll.lib && g_GameDll.loaded)
			dlclose(g_GameDll.lib);
		memset(&g_GameDll, 0, sizeof(GameDllInfo));
	}
#endif

void CServerGameDLL::DLLShutdown()
{
    //Call the original function
	m_pOrig->DLLShutdown();

	//Unload plugins
	g_PluginMngr.UnloadAll();

	// Shutdown sourcehook now
	g_SourceHook.CompleteShutdown();

	// Add the FCVAR_GAMEDLL flag to our cvars so the engine removes them properly
	g_SMConVarAccessor.MarkCommandsAsGameDLL();
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
		if (buffer[0] == ';' || strncmp(buffer, "//", 2) == 0)
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
#if defined WIN32 || defined _WIN32
			snprintf(full_path, sizeof(full_path)-1, "%s\\%s%s", g_ModPath.c_str(), buffer, ext);
#else
			snprintf(full_path, sizeof(full_path)-1, "%s/%s%s", g_ModPath.c_str(), buffer, ext);
#endif
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

	g_Engine.engine->LogPrint(buffer);
}

void CServerGameDLL::LevelShutdown(void)
{
	LevelShutdown_handler();
	m_pOrig->LevelShutdown();
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
