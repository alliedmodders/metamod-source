/* ======== sample_mm ========
 * Copyright (C) 2004-2007 Metamod:Source Development Team
 * No warranties of any kind
 *
 * License: zlib/libpng
 *
 * Author(s): David "BAILOPAN" Anderson
 * ============================
 */

#include <oslink.h>
#include "SamplePlugin.h"
#include "cvars.h"

//Declare the hooks we will be using in this file.  Hooking will not compile without these.
//The macro naming scheme is SH_DECL_HOOKn[_void].
//If you have 5 parameters, it would be HOOK5.  If the function is void, add _void.
//It stands for "SourceHook, Declare Hook".
SH_DECL_HOOK6(IServerGameDLL, LevelInit, SH_NOATTRIB, 0, bool, char const *, char const *, char const *, char const *, bool, bool);
SH_DECL_HOOK3_void(IServerGameDLL, ServerActivate, SH_NOATTRIB, 0, edict_t *, int, int);
SH_DECL_HOOK1_void(IServerGameDLL, GameFrame, SH_NOATTRIB, 0, bool);
SH_DECL_HOOK0_void(IServerGameDLL, LevelShutdown, SH_NOATTRIB, 0);
SH_DECL_HOOK2_void(IServerGameClients, ClientActive, SH_NOATTRIB, 0, edict_t *, bool);
SH_DECL_HOOK1_void(IServerGameClients, ClientDisconnect, SH_NOATTRIB, 0, edict_t *);
SH_DECL_HOOK2_void(IServerGameClients, ClientPutInServer, SH_NOATTRIB, 0, edict_t *, char const *);
SH_DECL_HOOK1_void(IServerGameClients, SetCommandClient, SH_NOATTRIB, 0, int);
SH_DECL_HOOK1_void(IServerGameClients, ClientSettingsChanged, SH_NOATTRIB, 0, edict_t *);
SH_DECL_HOOK5(IServerGameClients, ClientConnect, SH_NOATTRIB, 0, bool, edict_t *, const char*, const char *, char *, int);
SH_DECL_HOOK1_void(IServerGameClients, ClientCommand, SH_NOATTRIB, 0, edict_t *);
SH_DECL_HOOK2(IGameEventManager2, FireEvent, SH_NOATTRIB, 0, bool, IGameEvent *, bool);

SamplePlugin g_SamplePlugin;
MyListener g_Listener;

PLUGIN_EXPOSE(SamplePlugin, g_SamplePlugin);

bool SamplePlugin::LevelInit(const char *pMapName, const char *pMapEntities, const char *pOldLevel, const char *pLandmarkName, bool loadGame, bool background)
{
	META_LOG(g_PLAPI, "LevelInit() called: pMapName=%s", pMapName); 
	RETURN_META_VALUE(MRES_IGNORED, true);
}

void SamplePlugin::OnLevelShutdown()
{
	META_LOG(g_PLAPI, "OnLevelShutdown() called from listener");
}

void SamplePlugin::ServerActivate(edict_t *pEdictList, int edictCount, int clientMax)
{
	META_LOG(g_PLAPI, "ServerActivate() called: edictCount=%d, clientMax=%d", edictCount, clientMax);
	RETURN_META(MRES_IGNORED);
}

void SamplePlugin::GameFrame(bool simulating)
{
	//don't log this, it just pumps stuff to the screen ;]
	//META_LOG(g_PLAPI, "GameFrame() called: simulating=%d", simulating);
	RETURN_META(MRES_IGNORED);
}

void SamplePlugin::LevelShutdown( void )
{
	META_LOG(g_PLAPI, "LevelShutdown() called");
	RETURN_META(MRES_IGNORED);
}

void SamplePlugin::ClientActive(edict_t *pEntity, bool bLoadGame)
{
	META_LOG(g_PLAPI, "ClientActive called: pEntity=%d", pEntity ? m_Engine->IndexOfEdict(pEntity) : 0);
	RETURN_META(MRES_IGNORED);
}

void SamplePlugin::ClientDisconnect(edict_t *pEntity)
{
	META_LOG(g_PLAPI, "ClientDisconnect called: pEntity=%d", pEntity ? m_Engine->IndexOfEdict(pEntity) : 0);
	RETURN_META(MRES_IGNORED);
}

void SamplePlugin::ClientPutInServer(edict_t *pEntity, char const *playername)
{
	META_LOG(g_PLAPI, "ClientPutInServer called: pEntity=%d, playername=%s", pEntity ? m_Engine->IndexOfEdict(pEntity) : 0, playername);
	RETURN_META(MRES_IGNORED);
}

void SamplePlugin::SetCommandClient(int index)
{
	META_LOG(g_PLAPI, "SetCommandClient() called: index=%d", index);
	RETURN_META(MRES_IGNORED);
}

void SamplePlugin::ClientSettingsChanged(edict_t *pEdict)
{
	META_LOG(g_PLAPI, "ClientSettingsChanged called: pEdict=%d", pEdict ? m_Engine->IndexOfEdict(pEdict) : 0);
	RETURN_META(MRES_IGNORED);
}

bool SamplePlugin::ClientConnect(edict_t *pEntity, const char *pszName, const char *pszAddress, char *reject, int maxrejectlen)
{
	META_LOG(g_PLAPI, "ClientConnect called: pEntity=%d, pszName=%s, pszAddress=%s", pEntity ? m_Engine->IndexOfEdict(pEntity) : 0, pszName, pszAddress);
	RETURN_META_VALUE(MRES_IGNORED, true);
}

void SamplePlugin::ClientCommand(edict_t *pEntity)
{
	META_LOG(g_PLAPI, "ClientCommand called: pEntity=%d (commandString=%s)", pEntity ? m_Engine->IndexOfEdict(pEntity) : 0, m_Engine->Cmd_Args() ? m_Engine->Cmd_Args() : "");
	RETURN_META(MRES_IGNORED);
}

bool FireEvent_Handler(IGameEvent *event, bool bDontBroadcast)
{
	if (!event || !event->GetName())
		RETURN_META_VALUE(MRES_IGNORED, false);

	/**
	 * Note that this will only fire on game events that are already being listened to.
	 * For events that are not firing (such as item_pickup), you must actually
	 *  register an event listener in the event manager.  This hook is provided
	 *  as an example for non-eiface hooks only.
	 */

	const char *name = event->GetName();

	META_LOG(g_PLAPI, "FireGameEvent called: name=%s", name); 

	RETURN_META_VALUE(MRES_IGNORED, true);
}

bool SamplePlugin::Load(PluginId id, ISmmAPI *ismm, char *error, size_t maxlen, bool late)
{
	PLUGIN_SAVEVARS();

	GET_V_IFACE_ANY(serverFactory, m_ServerDll, IServerGameDLL, INTERFACEVERSION_SERVERGAMEDLL);
	GET_V_IFACE_CURRENT(engineFactory, m_Engine, IVEngineServer, INTERFACEVERSION_VENGINESERVER);
	GET_V_IFACE_ANY(serverFactory, m_ServerClients, IServerGameClients, INTERFACEVERSION_SERVERGAMECLIENTS);
	GET_V_IFACE_CURRENT(engineFactory, m_GameEventManager, IGameEventManager2, INTERFACEVERSION_GAMEEVENTSMANAGER2);

	META_LOG(g_PLAPI, "Starting plugin.\n");

	ismm->AddListener(this, &g_Listener);

	//Init our cvars/concmds
	ConCommandBaseMgr::OneTimeInit(&g_Accessor);

	//We're hooking the following things as POST, in order to seem like Server Plugins.
	//However, I don't actually know if Valve has done server plugins as POST or not.
	//Change the last parameter to 'false' in order to change this to PRE.
	//SH_ADD_HOOK_MEMFUNC means "SourceHook, Add Hook, Member Function".

	//Hook LevelInit to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameDLL, LevelInit, m_ServerDll, &g_SamplePlugin, &SamplePlugin::LevelInit, true);
	//Hook ServerActivate to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameDLL, ServerActivate, m_ServerDll, &g_SamplePlugin, &SamplePlugin::ServerActivate, true);
	//Hook GameFrame to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameDLL, GameFrame, m_ServerDll, &g_SamplePlugin, &SamplePlugin::GameFrame, true);
	//Hook LevelShutdown to our function -- this makes more sense as pre I guess
	SH_ADD_HOOK_MEMFUNC(IServerGameDLL, LevelShutdown, m_ServerDll, &g_SamplePlugin, &SamplePlugin::LevelShutdown, false);
	//Hook ClientActivate to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientActive, m_ServerClients, &g_SamplePlugin, &SamplePlugin::ClientActive, true);
	//Hook ClientDisconnect to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientDisconnect, m_ServerClients, &g_SamplePlugin, &SamplePlugin::ClientDisconnect, true);
	//Hook ClientPutInServer to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientPutInServer, m_ServerClients, &g_SamplePlugin, &SamplePlugin::ClientPutInServer, true);
	//Hook SetCommandClient to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, SetCommandClient, m_ServerClients, &g_SamplePlugin, &SamplePlugin::SetCommandClient, true);
	//Hook ClientSettingsChanged to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientSettingsChanged, m_ServerClients, &g_SamplePlugin, &SamplePlugin::ClientSettingsChanged, true);

	//The following functions are pre handled, because that's how they are in IServerPluginCallbacks

	//Hook ClientConnect to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientConnect, m_ServerClients, &g_SamplePlugin, &SamplePlugin::ClientConnect, false);
	//Hook ClientCommand to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientCommand, m_ServerClients, &g_SamplePlugin, &SamplePlugin::ClientCommand, false);

	//This hook is a static hook, no member function
	SH_ADD_HOOK_STATICFUNC(IGameEventManager2, FireEvent, m_GameEventManager, FireEvent_Handler, false); 

	//Get the call class for IVServerEngine so we can safely call functions without
	// invoking their hooks (when needed).
	m_Engine_CC = SH_GET_CALLCLASS(m_Engine);

	SH_CALL(m_Engine_CC, &IVEngineServer::LogPrint)("All hooks started!\n");

	return true;
}

bool SamplePlugin::Unload(char *error, size_t maxlen)
{
	//IT IS CRUCIAL THAT YOU REMOVE CVARS.
	//As of Metamod:Source 1.00-RC2, it will automatically remove them for you.
	//But this is only if you've registered them correctly!
    
	//Make sure we remove any hooks we did... this may not be necessary since
	//SourceHook is capable of unloading plugins' hooks itself, but just to be safe.

	SH_REMOVE_HOOK_STATICFUNC(IGameEventManager2, FireEvent, m_GameEventManager, FireEvent_Handler, false); 
	SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, LevelInit, m_ServerDll, &g_SamplePlugin, &SamplePlugin::LevelInit, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, ServerActivate, m_ServerDll, &g_SamplePlugin, &SamplePlugin::ServerActivate, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, GameFrame, m_ServerDll, &g_SamplePlugin, &SamplePlugin::GameFrame, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, LevelShutdown, m_ServerDll, &g_SamplePlugin, &SamplePlugin::LevelShutdown, false);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientActive, m_ServerClients, &g_SamplePlugin, &SamplePlugin::ClientActive, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientDisconnect, m_ServerClients, &g_SamplePlugin, &SamplePlugin::ClientDisconnect, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientPutInServer, m_ServerClients, &g_SamplePlugin, &SamplePlugin::ClientPutInServer, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, SetCommandClient, m_ServerClients, &g_SamplePlugin, &SamplePlugin::SetCommandClient, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientSettingsChanged, m_ServerClients, &g_SamplePlugin, &SamplePlugin::ClientSettingsChanged, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientConnect, m_ServerClients, &g_SamplePlugin, &SamplePlugin::ClientConnect, false);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientCommand, m_ServerClients, &g_SamplePlugin, &SamplePlugin::ClientCommand, false);

	//this, sourcehook does not keep track of.  we must do this.
	SH_RELEASE_CALLCLASS(m_Engine_CC);

	return true;
}

void SamplePlugin::AllPluginsLoaded()
{
	//we don't really need this for anything other than interplugin communication
	//and that's not used in this plugin.
	//If we really wanted, we could override the factories so other plugins can request
	// interfaces we make.  In this callback, the plugin could be assured that either
	// the interfaces it requires were either loaded in another plugin or not.
}

void *MyListener::OnMetamodQuery(const char *iface, int *ret)
{
	if (strcmp(iface, "SamplePlugin")==0)
	{
		if (ret)
			*ret = IFACE_OK;
		return static_cast<void *>(&g_SamplePlugin);
	}

	if (ret)
		*ret = IFACE_FAILED;

	return NULL;
}
