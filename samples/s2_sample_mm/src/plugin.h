/**
 * vim: set ts=4 sw=4 tw=99 noet :
 * ======================================================
 * Metamod:Source Sample Plugin
 * Written by AlliedModders LLC.
 * ======================================================
 *
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the authors be held liable for any damages arising from 
 * the use of this software.
 *
 * This sample plugin is public domain.
 */

#ifndef _INCLUDE_METAMOD_SOURCE_PLUGIN_H_
#define _INCLUDE_METAMOD_SOURCE_PLUGIN_H_

#include <ISmmPlugin.h>
#include <igameevents.h>
#include "version_gen.h"


class MMSPlugin : public ISmmPlugin, public IMetamodListener
{
public:
	MMSPlugin();

	bool Load(PluginId id, ISmmAPI *ismm, char *error, size_t maxlen, bool late);
	bool Unload(char *error, size_t maxlen);
	void AllPluginsLoaded();
public: //hooks
	void OnLevelInit( char const *pMapName,
				 char const *pMapEntities,
				 char const *pOldLevel,
				 char const *pLandmarkName,
				 bool loadGame,
				 bool background );
	void OnLevelShutdown();
	KHook::Return<void> Hook_GameFrame(IServerGameDLL*, bool simulating, bool bFirstTick, bool bLastTick );
	KHook::Return<void> Hook_ClientActive(IServerGameClients*, CPlayerSlot slot, bool bLoadGame, const char *pszName, uint64 xuid );
	KHook::Return<void> Hook_ClientDisconnect(IServerGameClients*, CPlayerSlot slot, ENetworkDisconnectionReason reason, const char *pszName, uint64 xuid, const char *pszNetworkID );
	KHook::Return<void> Hook_ClientPutInServer(IServerGameClients*, CPlayerSlot slot, char const *pszName, int type, uint64 xuid );
	KHook::Return<void> Hook_ClientSettingsChanged(IServerGameClients*, CPlayerSlot slot );
	KHook::Return<void> Hook_OnClientConnected(IServerGameClients*, CPlayerSlot slot, const char *pszName, uint64 xuid, const char *pszNetworkID, const char *pszAddress, bool bFakePlayer );
	KHook::Return<bool> Hook_ClientConnect(IServerGameClients*, CPlayerSlot slot, const char *pszName, uint64 xuid, const char *pszNetworkID, bool unk1, CBufferString *pRejectReason );
	KHook::Return<void> Hook_ClientCommand(IServerGameClients*, CPlayerSlot nSlot, const CCommand &_cmd );
public:
	const char *GetAuthor() { return PLUGIN_AUTHOR; }
	const char *GetName() { return PLUGIN_DISPLAY_NAME; }
	const char *GetDescription() { return PLUGIN_DESCRIPTION; }
	const char *GetURL() { return PLUGIN_URL; }
	const char *GetLicense() { return PLUGIN_LICENSE; }
	const char *GetVersion() { return PLUGIN_FULL_VERSION; }
	const char *GetDate() { return __DATE__; }
	const char *GetLogTag() { return PLUGIN_LOGTAG; }
protected:
	KHook::Virtual<IServerGameDLL, void, bool, bool, bool> m_GameFrame;
	KHook::Virtual<IServerGameClients, void, CPlayerSlot, bool, const char *, uint64> m_ClientActive;
	KHook::Virtual<IServerGameClients, void, CPlayerSlot, ENetworkDisconnectionReason, const char *, uint64, const char *> m_ClientDisconnect;
	KHook::Virtual<IServerGameClients, void, CPlayerSlot, char const *, int, uint64> m_ClientPutInServer;
	KHook::Virtual<IServerGameClients, void, CPlayerSlot> m_ClientSettingsChanged;
	KHook::Virtual<IServerGameClients, void, CPlayerSlot, const char*, uint64, const char *, const char *, bool> m_OnClientConnected;
	KHook::Virtual<IServerGameClients, bool, CPlayerSlot, const char*, uint64, const char *, bool, CBufferString *> m_ClientConnect;
	KHook::Virtual<IServerGameClients, void, CPlayerSlot, const CCommand &> m_ClientCommand;
};

extern MMSPlugin g_ThisPlugin;

PLUGIN_GLOBALVARS();

#endif //_INCLUDE_METAMOD_SOURCE_PLUGIN_H_
