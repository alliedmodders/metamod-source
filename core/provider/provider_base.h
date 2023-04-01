/**
 * vim: set ts=4 sw=4 tw=99 noet :
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
 */

#ifndef _INCLUDE_METAMOD_SOURCE_BASE_PROVIDER_H_
#define _INCLUDE_METAMOD_SOURCE_BASE_PROVIDER_H_

#if defined _DEBUG
#define DEBUG2
#undef _DEBUG
#endif
#include <sourcehook.h>
#include <sh_memfuncinfo.h>
#if SOURCE_ENGINE != SE_DOTA
#include <iserverplugin.h>
#endif
#include "ISmmAPI.h"
#include "metamod_provider.h"
#include "metamod_oslink.h"
#if defined DEBUG2
#undef DEBUG2
#define _DEBUG
#endif

using namespace SourceMM;
using namespace SourceHook;

class INetworkGameServer;

class BaseProvider : public IMetamodSourceProvider
{
public:
	virtual bool IsSourceEngineBuildCompatible(int build) override;
	virtual bool GetHookInfo(ProvidedHooks hook, SourceHook::MemFuncInfo *pInfo) override;
	virtual bool LogMessage(const char *buffer) override;
	virtual const char *GetCommandLineValue(const char *key, const char *defval) override;
	virtual void ConsolePrint(const char *msg) override;
	virtual bool IsRemotePrintingAvailable() override;
	virtual void ClientConsolePrint(edict_t *client, const char *msg) override;
	virtual void DisplayError(const char *fmt, ...) override;
	virtual void DisplayWarning(const char *fmt, ...) override;
	virtual int TryServerGameDLL(const char *iface) override;
	virtual void Notify_DLLInit_Pre(CreateInterfaceFn engineFactory, CreateInterfaceFn serverFactory) override;
	void Notify_DLLShutdown_Pre() override;
	virtual void ServerCommand(const char *cmd) override;
	virtual ConVar *CreateConVar(const char *name, 
		const char *defval, 
		const char *help,
		int flags) override;
	virtual const char *GetConVarString(ConVar *convar) override;
	virtual void SetConVarString(ConVar *convar, const char *str) override;
	virtual void GetGamePath(char *pszBuffer, int len) override;
	virtual const char *GetGameDescription() override;
	virtual IConCommandBaseAccessor *GetConCommandBaseAccessor() override;
	virtual bool RegisterConCommandBase(ConCommandBase *pCommand) override;
	virtual void UnregisterConCommandBase(ConCommandBase *pCommand) override;
	virtual bool IsConCommandBaseACommand(ConCommandBase *pCommand) override;
	virtual int GetUserMessageCount() override;
	virtual int FindUserMessage(const char *name, int *size=NULL) override;
	virtual const char *GetUserMessage(int index, int *size=NULL) override;
	virtual int DetermineSourceEngine() override;
	virtual bool ProcessVDF(const char *file, char path[], size_t path_len, char alias[], size_t alias_len) override;
	virtual const char *GetEngineDescription() const override;
#if SOURCE_ENGINE == SE_DOTA && defined( _WIN32 )
	bool AllowDedicatedServers(EUniverse universe) const;
#endif
};

extern IVEngineServer *engine;
extern IServerGameDLL *server;
extern IServerGameClients *gameclients;
extern ICvar *icvar;
extern CGlobalVars *gpGlobals;
#if SOURCE_ENGINE == SE_DOTA
extern INetworkServerService *netservice;
extern IEngineServiceMgr *enginesvcmgr;
#endif

#endif //_INCLUDE_METAMOD_SOURCE_BASE_PROVIDER_H_

