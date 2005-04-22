/* ======== sample_mm ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#include <oslink.h>
#include "SamplePlugin.h"

SamplePlugin g_SamplePlugin;

PLUGIN_EXPOSE(SamplePlugin, g_SamplePlugin);

//This has all of the necessary hook declarations.  Read it!
#include "meta_hooks.h"

bool SamplePlugin::LevelInit(const char *pMapName, const char *pMapEntities, const char *pOldLevel, const char *pLandmarkName, bool loadGame, bool background)
{
	META_LOG(g_PLAPI, "LevelInit() called: pMapName=%s", pMapName); 
	RETURN_META_VALUE(MRES_IGNORED, true);
}

void SamplePlugin::ServerActivate(edict_t *pEdictList, int edictCount, int clientMax)
{
	META_LOG(g_PLAPI, "ServerActivate() called: edictCount=%d, clientMax=%d", edictCount, clientMax);
	RETURN_META(MRES_IGNORED);
}

void SamplePlugin::GameFrame(bool simulating)
{
	META_LOG(g_PLAPI, "GameFrame() called: simulating=%d", simulating);
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

bool SamplePlugin::Load(PluginId id, ISmmAPI *ismm, factories *list, char *error, size_t maxlen)
{
	PLUGIN_SAVEVARS();

	m_ServerDll = (IServerGameDLL *)((ismm->serverFactory())(INTERFACEVERSION_SERVERGAMEDLL, NULL));
	m_Engine = (IVEngineServer *)((ismm->engineFactory())(INTERFACEVERSION_VENGINESERVER, NULL));
	m_ServerClients = (IServerGameClients *)((ismm->serverFactory())(INTERFACEVERSION_SERVERGAMECLIENTS, NULL));

	if (!m_ServerDll)
	{
		snprintf(error, maxlen, "Could not find interface %s", INTERFACEVERSION_SERVERGAMEDLL);
		return false;
	}
	if (!m_Engine)
	{
		snprintf(error, maxlen, "Could not find interface %s", INTERFACEVERSION_VENGINESERVER);
		return false;
	}
	if (!m_ServerClients)
	{
		snprintf(error, maxlen, "Could not find interface %s", INTERFACEVERSION_SERVERGAMECLIENTS);
		return false;
	}

	//We're hooking the following things as POST, in order to seem like Server Plugins.
	//However, I don't actually know if Valve has done server plugins as POST or not.
	//Change the last parameter to 'false' in order to change this to PRE.
	//SH_ADD_HOOK_MEMFUNC means "SourceHook, Add Hook, Member Function".

	//Hook LevelInit to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameDLL, LevelInit, m_ServerDll, &g_SamplePlugin, LevelInit, true);
	//Hook ServerActivate to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameDLL, ServerActivate, m_ServerDll, &g_SamplePlugin, ServerActivate, true);
	//Hook GameFrame to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameDLL, GameFrame, m_ServerDll, &g_SamplePlugin, GameFrame, true);
	//Hook LevelShutdown to our function -- this makes more sense as pre I guess
	SH_ADD_HOOK_MEMFUNC(IServerGameDLL, LevelShutdown, m_ServerDll, &g_SamplePlugin, LevelShutdown, false);
	//Hook ClientActivate to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientActive, m_ServerClients, &g_SamplePlugin, ClientActive, true);
	//Hook ClientDisconnect to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientDisconnect, m_ServerClients, &g_SamplePlugin, ClientDisconnect, true);
	//Hook ClientPutInServer to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientPutInServer, m_ServerClients, &g_SamplePlugin, ClientPutInServer, true);
	//Hook SetCommandClient to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, SetCommandClient, m_ServerClients, &g_SamplePlugin, SetCommandClient, true);
	//Hook ClientSettingsChanged to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientSettingsChanged, m_ServerClients, &g_SamplePlugin, ClientSettingsChanged, true);

	//The following functions are pre handled, because that's how they are in IServerPluginCallbacks

	//Hook ClientConnect to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientConnect, m_ServerClients, &g_SamplePlugin, ClientConnect, false);
	//Hook ClientCommand to our function
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientCommand, m_ServerClients, &g_SamplePlugin, ClientCommand, false);

	return true;
}

bool SamplePlugin::Unload(char *error, size_t maxlen)
{
	//Make sure we remove any hooks we did... this may not be necessary since
	// SourceHook is capable of unloading plugins' hooks itself, but just to be safe.

	SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, LevelInit, m_ServerDll, &g_SamplePlugin, LevelInit, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, ServerActivate, m_ServerDll, &g_SamplePlugin, ServerActivate, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, GameFrame, m_ServerDll, &g_SamplePlugin, GameFrame, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, LevelShutdown, m_ServerDll, &g_SamplePlugin, LevelShutdown, false);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientActive, m_ServerClients, &g_SamplePlugin, ClientActive, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientDisconnect, m_ServerClients, &g_SamplePlugin, ClientDisconnect, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientPutInServer, m_ServerClients, &g_SamplePlugin, ClientPutInServer, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, SetCommandClient, m_ServerClients, &g_SamplePlugin, SetCommandClient, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientSettingsChanged, m_ServerClients, &g_SamplePlugin, ClientSettingsChanged, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientConnect, m_ServerClients, &g_SamplePlugin, ClientConnect, false);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientCommand, m_ServerClients, &g_SamplePlugin, ClientCommand, false);

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
