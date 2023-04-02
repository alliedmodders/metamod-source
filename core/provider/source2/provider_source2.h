/**
 * vim: set ts=4 :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2023 AlliedModders LLC and authors.
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

#ifndef _INCLUDE_METAMOD_SOURCE_SOURCE2_PROVIDER_H_
#define _INCLUDE_METAMOD_SOURCE_SOURCE2_PROVIDER_H_

#include "../provider_base.h"
#include <tier1/utlvector.h>
#include <string>

// TODO: is this still needed for Dota or CS2 on any platform?
#if SOURCE_ENGINE == SE_DOTA && defined( _WIN32 )
#define SHOULD_OVERRIDE_ALLOWDEDICATED_SERVER
#endif

class INetworkGameClient;
class ISource2WorldSession;

class Source2Provider : public BaseProvider
{
public:
	virtual void Notify_DLLInit_Pre(CreateInterfaceFn engineFactory, CreateInterfaceFn serverFactory) override;
	virtual void Notify_DLLShutdown_Pre() override;
	virtual bool ProcessVDF(const char* file, char path[], size_t path_len, char alias[], size_t alias_len) override;
	virtual int DetermineSourceEngine() override;
	virtual const char* GetEngineDescription() const override;
	virtual void GetGamePath(char* pszBuffer, int len) override;
	virtual const char* GetGameDescription() override;
	virtual const char* GetCommandLineValue(const char* key, const char* defval) override;
	virtual void ConsolePrint(const char* msg) override;
	virtual void ClientConsolePrint(edict_t* client, const char* msg) override;
	virtual void ServerCommand(const char* cmd) override;
	virtual MetamodSourceConVar *CreateConVar(const char* name,
		const char* defval,
		const char* help,
		int flags) override;
	virtual const char* GetConVarString(MetamodSourceConVar *convar) override;
	virtual void SetConVarString(MetamodSourceConVar *convar, const char* str) override;
	virtual bool RegisterConCommandBase(ConCommandBase* pCommand) override;
	virtual void UnregisterConCommandBase(ConCommandBase* pCommand) override;
	virtual bool IsConCommandBaseACommand(ConCommandBase* pCommand) override;
public:
#ifdef SHOULD_OVERRIDE_ALLOWDEDICATED_SERVER
	bool Hook_AllowDedicatedServers(EUniverse universe) const;
#endif
	void Hook_StartupServer_Post(const GameSessionConfiguration_t &config, ISource2WorldSession *, const char *);
	void Hook_Init(const GameSessionConfiguration_t &config, const char* pszMapName);
	CUtlVector<INetworkGameClient *> *Hook_StartChangeLevel(const char*, const char*, void*);
	void Hook_SwitchToLoop(const char *pszLoopName, KeyValues *pKV, uint32 nId, const char *pszUnk, bool bUnk);
	void Hook_ClientCommand(CEntityIndex index, const CCommand& args);
private:
	bool KVLoadFromFile(KeyValues *kv, IFileSystem *filesystem, const char *resourceName, const char *pathID);
private:
	IFileSystem* baseFs = nullptr;
	std::string sLastMap;
};

#endif
