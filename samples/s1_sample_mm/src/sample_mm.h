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

#ifndef _INCLUDE_METAMOD_SOURCE_STUB_PLUGIN_H_
#define _INCLUDE_METAMOD_SOURCE_STUB_PLUGIN_H_

#include <ISmmPlugin.h>
#include <igameevents.h>
#include <iplayerinfo.h>
#include "engine_wrappers.h"

#if defined WIN32 && !defined snprintf
#define snprintf _snprintf
#endif

class SamplePlugin : public ISmmPlugin, public IMetamodListener
{
public:
	SamplePlugin();
	
	bool Load(PluginId id, ISmmAPI *ismm, char *error, size_t maxlen, bool late);
	bool Unload(char *error, size_t maxlen);
	bool Pause(char *error, size_t maxlen);
	bool Unpause(char *error, size_t maxlen);
	void AllPluginsLoaded();
public: //IMetamodListener stuff
	void OnVSPListening(IServerPluginCallbacks *iface);
public: //hooks
	KHook::Return<void> Hook_ServerActivate(IServerGameDLL*, edict_t *pEdictList, int edictCount, int clientMax);
	KHook::Return<bool> Hook_LevelInit(IServerGameDLL*, const char *pMapName,
		char const *pMapEntities,
		char const *pOldLevel,
		char const *pLandmarkName,
		bool loadGame,
		bool background);
	KHook::Return<void> Hook_GameFrame(IServerGameDLL*, bool simulating);
	KHook::Return<void> Hook_LevelShutdown(IServerGameDLL*);
	KHook::Return<void> Hook_ClientActive(IServerGameClients*, edict_t *pEntity, bool bLoadGame);
	KHook::Return<void> Hook_ClientDisconnect(IServerGameClients*, edict_t *pEntity);
	KHook::Return<void> Hook_ClientPutInServer(IServerGameClients*, edict_t *pEntity, char const *playername);
	KHook::Return<void> Hook_SetCommandClient(IServerGameClients*, int index);
	KHook::Return<void> Hook_ClientSettingsChanged(IServerGameClients*, edict_t *pEdict);
	KHook::Return<bool> Hook_ClientConnect(IServerGameClients*, edict_t *pEntity, 
		const char *pszName,
		const char *pszAddress,
		char *reject,
		int maxrejectlen);
#if SOURCE_ENGINE >= SE_ORANGEBOX
	KHook::Return<void> Hook_ClientCommand(IServerGameClients*, edict_t *pEntity, const CCommand &args);
#else
	KHook::Return<void> Hook_ClientCommand(IServerGameClients*, edict_t *pEntity);
#endif
public:
	const char *GetAuthor();
	const char *GetName();
	const char *GetDescription();
	const char *GetURL();
	const char *GetLicense();
	const char *GetVersion();
	const char *GetDate();
	const char *GetLogTag();

	KHook::Virtual<IServerGameDLL, bool, char const *, char const *, char const *, char const *, bool, bool> m_LevelInit;
	KHook::Virtual<IServerGameDLL, void, edict_t *, int, int> m_ServerActivate;
	KHook::Virtual<IServerGameDLL, void, bool> m_GameFrame;
	KHook::Virtual<IServerGameDLL, void> m_LevelShutdown;
	KHook::Virtual<IServerGameClients, void, edict_t *, bool> m_ClientActive;
	KHook::Virtual<IServerGameClients, void, edict_t *> m_ClientDisconnect;
	KHook::Virtual<IServerGameClients, void, edict_t *, char const *> m_ClientPutInServer;
	KHook::Virtual<IServerGameClients, void, int> m_SetCommandClient;
	KHook::Virtual<IServerGameClients, void, edict_t *> m_ClientSettingsChanged;
	KHook::Virtual<IServerGameClients, bool, edict_t *, const char*, const char *, char *, int> m_ClientConnect;
#if SOURCE_ENGINE >= SE_ORANGEBOX
	KHook::Virtual<IServerGameClients, void, edict_t *, const CCommand &> m_ClientCommand;
#else
	KHook::Virtual<IServerGameClients, void, edict_t *> m_ClientCommand;
#endif
};

extern SamplePlugin g_SamplePlugin;

PLUGIN_GLOBALVARS();

#endif //_INCLUDE_METAMOD_SOURCE_STUB_PLUGIN_H_
