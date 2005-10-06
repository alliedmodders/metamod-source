/* ======== sample_mm ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_SAMPLEPLUGIN_H
#define _INCLUDE_SAMPLEPLUGIN_H

#include <ISmmPlugin.h>
#include <sourcehook/sourcehook.h>
#include <igameevents.h>

#define SAMPLE_VERSION	"1.10"

class SamplePlugin : public ISmmPlugin
{
public:
	bool Load(PluginId id, ISmmAPI *ismm, factories *list, char *error, size_t maxlen, bool late);
	bool Unload(char *error, size_t maxlen);
	void AllPluginsLoaded();
	bool Pause(char *error, size_t maxlen)
	{
		return true;
	}
	bool Unpause(char *error, size_t maxlen)
	{
		return true;
	}
public:
	int GetApiVersion() { return PLAPI_VERSION; }
public:
	const char *GetAuthor()
	{
		return "BAILOPAN";
	}
	const char *GetName()
	{
		return "Sample Plugin";
	}
	const char *GetDescription()
	{
		return "Sample plugin that hooks basic things";
	}
	const char *GetURL()
	{
		return "http://www.sourcemm.net/";
	}
	const char *GetLicense()
	{
		return "zlib/libpng";
	}
	const char *GetVersion()
	{
		return SAMPLE_VERSION;
	}
	const char *GetDate()
	{
		return __DATE__;
	}
	const char *GetLogTag()
	{
		return "SAMPLE";
	}
public:
	//These functions are from IServerPluginCallbacks
	//Note, the parameters might be a little different to match the actual calls!

	//Called on LevelInit.  Server plugins only have pMapName
	bool LevelInit(const char *pMapName, char const *pMapEntities, char const *pOldLevel, char const *pLandmarkName, bool loadGame, bool background);

	//Called on ServerActivate.  Same definition as server plugins
	void ServerActivate(edict_t *pEdictList, int edictCount, int clientMax);

	//Called on a game tick.  Same definition as server plugins
	void GameFrame(bool simulating);

	//Called on level shutdown.  Same definition as server plugins 
	void LevelShutdown(void);

	//Client is activate (whatever that means).  We get an extra parameter...
	// "If bLoadGame is true, don't spawn the player because its state is already setup."
	void ClientActive(edict_t *pEntity, bool bLoadGame);

	//Client disconnects - same as server plugins
	void ClientDisconnect(edict_t *pEntity);

	//Client is put in server - same as server plugins
	void ClientPutInServer(edict_t *pEntity, char const *playername);

	//Sets the client index - same as server plugins
	void SetCommandClient(int index);

	//Called on client settings changed (duh) - same as server plugins
	void ClientSettingsChanged(edict_t *pEdict);

	//Called on client connect.  Unlike server plugins, we return whether the 
	// connection is allowed rather than set it through a pointer in the first parameter.
	// You can still supercede the GameDLL through RETURN_META_VALUE(MRES_SUPERCEDE, true/false)
	bool ClientConnect(edict_t *pEntity, const char *pszName, const char *pszAddress, char *reject, int maxrejectlen);

	//Called when a client uses a command.  Unlike server plugins, it's void.
	// You can still supercede the gamedll through RETURN_META(MRES_SUPERCEDE).
	void ClientCommand(edict_t *pEntity);

private:
	IGameEventManager2 *m_GameEventManager;	
	IVEngineServer *m_Engine;
	IServerGameDLL *m_ServerDll;
	IServerGameClients *m_ServerClients;
	SourceHook::CallClass<IVEngineServer> *m_Engine_CC;
};

extern SamplePlugin g_SamplePlugin;
PLUGIN_GLOBALVARS();

bool FireEvent_Handler(IGameEvent *event, bool bDontBroadcast);

#endif //_INCLUDE_SAMPLEPLUGIN_H
