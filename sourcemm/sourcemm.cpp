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

#include "sourcemm.h"
#include "CServerGameDLL.h"
#include "CServerGameEnts.h"
#include "CServerGameClients.h"
#include "CHLTVDirector.h"
#include "concommands.h"
#include "CSmmAPI.h"
#include "util.h"

/**
 * @brief Implementation of main SourceMM GameDLL functionality
 * @file sourcemm.cpp
 */

CServerGameDLL *g_TempGameDLL = NULL;
CServerGameEnts *g_TempGameEnts = NULL;
CServerGameClients *g_TempGameClients = NULL;
CHLTVDirector *g_TempDirector = NULL;
GameDllInfo g_GameDll = {false, NULL, NULL};
EngineInfo g_Engine = {NULL, NULL, NULL, NULL};
SourceHook::CSourceHookImpl g_SourceHook;
SourceHook::ISourceHook *g_SHPtr;
std::string g_ModPath;
std::string g_BinPath;
PluginId g_PLID = Pl_Console;		//Technically, SourceMM is the "Console" plugin... :p

///////////////////////////////////
// Main code for HL2 Interaction //
///////////////////////////////////

void DLLShutdown_handler(void);
SH_DECL_HOOK0_void(IServerGameDLL, DLLShutdown, SH_NOATTRIB, 0);

//This is where the magic happens
SMM_API void *CreateInterface(const char *name, int *ret)
{
	if (!g_GameDll.loaded)
	{
		//State of the Mod:
		// Currently, HL2 Engine has loaded Metamod:Source
		// It is now asking it to get an interface.  We don't have one,
		//  so we're gonna try to give it a fake one to get the information we need.
		//  Then we'll swap the vtables with the real one from the real gamedll.

		if (strcmp(name, INTERFACEVERSION_SERVERGAMEDLL) == 0)
		{
			//We're in.  Give the server our fake class as bait.
			if (ret)
				*ret = IFACE_OK;

			g_TempGameDLL = new CServerGameDLL;

			return static_cast<void *>(g_TempGameDLL);
		} else if (strcmp(name, INTERFACEVERSION_SERVERGAMEENTS) == 0) {
			if (ret)
				*ret = IFACE_OK;

			g_TempGameEnts = new CServerGameEnts;

			return static_cast<void *>(g_TempGameEnts);
		} else if (strcmp(name, INTERFACEVERSION_SERVERGAMECLIENTS) == 0) {
			if (ret)
				*ret = IFACE_OK;

			g_TempGameClients = new CServerGameClients;

			return static_cast<void *>(g_TempGameClients);
		} else if (strcmp(name, INTERFACEVERSION_HLTVDIRECTOR) == 0) {
			if (ret)
				*ret = IFACE_OK;

			g_TempDirector = new CHLTVDirector;

			return static_cast<void *>(g_TempDirector);
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

			g_GameDll.serverGameDLL = serverDll;

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

			//The GameDLL has given the go - now we'll swap the vtables ( eww! )
			//This ugly hack will patch the pointer we've given the engine
			// so it will just be calling the real GameDLL instead.
			// Unlike Metamod:HL1, this is the effect we want, rather than duplicating
			//  the entire class (because SourceHook doesn't do that).
			void *vtableDst = ((void *)(g_TempGameDLL));
			void *vtableSrc = ((void *)(serverDll));

			SourceHook::SetMemAccess(vtableDst, sizeof(IServerGameDLL), SH_MEM_READ|SH_MEM_WRITE);
			memcpy(vtableDst, vtableSrc, sizeof(IServerGameDLL));

			//Now patch IServerGameEnts
			vtableDst = ((void *)(g_TempGameEnts));
			vtableSrc = ((void *)(serverEnts));
			SourceHook::SetMemAccess(vtableDst, sizeof(IServerGameEnts), SH_MEM_READ|SH_MEM_WRITE);
			memcpy(vtableDst, vtableSrc, sizeof(IServerGameEnts));

			//Now patch IServerGameClients
			vtableDst = ((void *)(g_TempGameClients));
			vtableSrc = ((void *)(serverClients));
			SourceHook::SetMemAccess(vtableDst, sizeof(IServerGameClients), SH_MEM_READ|SH_MEM_WRITE);
			memcpy(vtableDst, vtableSrc, sizeof(IServerGameClients));

			//Now patch IHLTVDirector
			vtableDst = ((void *)(g_TempDirector));
			vtableSrc = ((void *)(serverHLTV));
			SourceHook::SetMemAccess(vtableDst, sizeof(IHLTVDirector), SH_MEM_READ|SH_MEM_WRITE);
			memcpy(vtableDst, vtableSrc, sizeof(IHLTVDirector));

			//Everything's done.
			g_GameDll.loaded = true;

			//Initialize our shutdown hook
			SH_ADD_HOOK_STATICFUNC(IServerGameDLL, DLLShutdown, serverDll, DLLShutdown_handler, false);

			//Initialize our console hooks
			ConCommandBaseMgr::OneTimeInit(static_cast<IConCommandBaseAccessor *>(&g_SMConVarAccessor));
            
			//Now it's safe to load plugins.
#if defined WIN32 || defined _WIN32
			snprintf(full_path, sizeof(full_path)-1, "%s\\%s", g_ModPath.c_str(), "metaplugins.ini");
#else
			snprintf(full_path, sizeof(full_path)-1, "%s/%s", g_ModPath.c_str(), "metaplugins.ini");
#endif

			LoadPluginsFromFile(full_path);

			return true;
		}
	}

	//Somehow, the function got here.  This should be impossible, as not only is it 
	// only called once, but the vtables should be overridden.
	Error("Metamod:Source fatal error - IServerGameDLL::DLLInit() called inappropriately");

	return false;
}

void DLLShutdown_handler(void)
{
	//It's time for us to shut down too!
	g_PluginMngr.UnloadAll();

	//Call the DLL...
	g_GameDll.serverGameDLL->DLLShutdown();

	//Unload the DLL forcefully
	dlclose(g_GameDll.lib);
	memset(&g_GameDll, 0, sizeof(GameDllInfo));

	//For now, I'm not gonna bother freeing the memory allocated above.
	//Why?
	// 1.  We're exiting the application (I should hope!)
	// 2.  If we're not exiting, we just deallocated the gamedll, so we're about to crash out ANYWAY
	// 3.  We never saved the original vtable pointers, and we'd have to copy them back to get our destructors.
	//Soooo... we'll just accept our fate here.

	//DON'T CALL THE GAMEDLL! IT'S GONE!  pinin' for the fjords
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
	while (!feof(fp))
	{
		buffer[0] = '\0';
		fgets(buffer, sizeof(buffer)-1, fp);
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
