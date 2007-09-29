#ifndef _INCLUDE_METAMOD_SOURCE_VSP_LISTENER_H_
#define _INCLUDE_METAMOD_SOURCE_VSP_LISTENER_H_

#include "iserverplugin.h"

class VSPListener : public IServerPluginCallbacks
{
public:
	VSPListener();
public:
	virtual bool Load(CreateInterfaceFn interfaceFactory, CreateInterfaceFn gameServerFactory);
	virtual void Unload();
	virtual void Pause();
	virtual void UnPause();
	virtual const char *GetPluginDescription();
	virtual void LevelInit(char const *pMapName);
	virtual void ServerActivate(edict_t *pEdictList, int edictCount, int clientMax);
	virtual void GameFrame(bool simulating);
	virtual void LevelShutdown(void);
	virtual void ClientActive(edict_t *pEntity);
	virtual void ClientDisconnect(edict_t *pEntity);
	virtual void ClientPutInServer(edict_t *pEntity, char const *playername);
	virtual void SetCommandClient(int index);
	virtual void ClientSettingsChanged(edict_t *pEdict);
	virtual PLUGIN_RESULT ClientConnect(bool *bAllowConnect, edict_t *pEntity, const char *pszName, const char *pszAddress, char *reject, int maxrejectlen);
	virtual PLUGIN_RESULT ClientCommand(edict_t *pEntity);
	virtual PLUGIN_RESULT NetworkIDValidated(const char *pszUserName, const char *pszNetworkID);
public:
	bool IsLoaded();
	void SetLoadable(bool loadable);
private:
	bool m_bLoaded;
	bool m_bLoadable;
};

#endif //_INCLUDE_METAMOD_SOURCE_VSP_LISTENER_H_

