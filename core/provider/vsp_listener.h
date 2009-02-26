/**
 * vim: set ts=4 :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2008 AlliedModders LLC and authors.
 * All rights reserved.
 * ======================================================
 *
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the authors be held liable for any damages arising from 
 * the use of this software.
 * 
 * Permission is granted to anyone to use this software for any purpose, 
 * including commercial applications, and to alter it and redistribute it 
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not 
 * claim that you wrote the original software. If you use this software in a 
 * product, an acknowledgment in the product documentation would be 
 * appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 * misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 * Version: $Id$
 */

#ifndef _INCLUDE_METAMOD_SOURCE_VSP_LISTENER_H_
#define _INCLUDE_METAMOD_SOURCE_VSP_LISTENER_H_

#if defined _DEBUG
#define DEBUG2
#undef _DEBUG
#endif
#include "iserverplugin.h"
#if defined DEBUG2
#undef DEBUG2
#define _DEBUG
#endif



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
#if SOURCE_ENGINE >= SE_ORANGEBOX
	virtual PLUGIN_RESULT ClientCommand(edict_t *pEntity, const CCommand &cmd);
#else
	virtual PLUGIN_RESULT ClientCommand(edict_t *pEntity);
#endif
	virtual PLUGIN_RESULT NetworkIDValidated(const char *pszUserName, const char *pszNetworkID);
#if SOURCE_ENGINE != SE_DARKMESSIAH
	virtual void OnQueryCvarValueFinished( QueryCvarCookie_t iCookie, edict_t *pPlayerEntity, EQueryCvarValueStatus eStatus, const char *pCvarName, const char *pCvarValue );
#endif
public:
	bool IsLoaded();
	void SetLoadable(bool loadable);
private:
	bool m_bLoaded;
	bool m_bLoadable;
};

#endif //_INCLUDE_METAMOD_SOURCE_VSP_LISTENER_H_

