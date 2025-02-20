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

#include "../metamod_convar.h"

#if defined _DEBUG
#define DEBUG2
#undef _DEBUG
#endif
#include <sourcehook.h>
#include <sh_memfuncinfo.h>
#if !defined META_IS_SOURCE2
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
public: // Must implement
	virtual void Notify_DLLInit_Pre(CreateInterfaceFn engineFactory, CreateInterfaceFn serverFactory) override = 0;
	virtual void Notify_DLLShutdown_Pre() override = 0;
	virtual int DetermineSourceEngine() override = 0;
	virtual const char *GetEngineDescription() const override = 0;
	virtual void GetGamePath(char *pszBuffer, int len) override = 0;
	virtual const char *GetGameDescription() override = 0;
	virtual bool ProcessVDF(const char* file, char path[], size_t path_len, char alias[], size_t alias_len) override = 0;
	virtual const char* GetCommandLineValue(const char* key, const char* defval) override = 0;
	virtual void ConsolePrint(const char* msg) override = 0;
	virtual void ClientConsolePrint(MMSPlayer_t player, const char* msg) override = 0;
	virtual void ServerCommand(const char* cmd) override = 0;
	virtual MetamodSourceConVar *CreateConVar(const char* name,
		const char* defval,
		const char* help,
		int flags) override = 0;
	virtual const char* GetConVarString(MetamodSourceConVar *convar) override = 0;
	virtual void SetConVarString(MetamodSourceConVar *convar, const char* str) override = 0;
	virtual bool RegisterConCommand(ProviderConCommand *pCommand) override = 0;
	virtual bool RegisterConVar(ProviderConVar *pVar) override = 0;
	virtual void UnregisterConCommand(ProviderConCommand *pCommand) override = 0;
	virtual void UnregisterConVar(ProviderConVar *pVar) override = 0;
	virtual bool IsConCommandBaseACommand(ConCommandBase* pCommand) override = 0;
public: // May implement/override (stubbed)
	virtual int GetUserMessageCount() override { return -1; }
	virtual int FindUserMessage(const char *name, int *size=nullptr) override { return -1;}
	virtual const char *GetUserMessage(int index, int *size=nullptr) override { return nullptr;}
public: // May implement/override
	virtual bool IsSourceEngineBuildCompatible(int build) override;
	virtual bool LogMessage(const char *buffer) override;
	virtual void DisplayError(const char *fmt, ...) override;
	virtual void DisplayWarning(const char *fmt, ...) override;
	virtual void DisplayDevMsg(const char* fmt, ...) override;
	virtual int TryServerGameDLL(const char *iface) override;
public:
	void SetCallbacks(IMetamodSourceProviderCallbacks* pCallbacks) override final
	{
		m_pCallbacks = pCallbacks;
	}
protected:
	IMetamodSourceProviderCallbacks* m_pCallbacks = nullptr;
};

extern IVEngineServer *engine;
extern IServerGameDLL *server;
extern IServerGameClients *gameclients;
extern ICvar *icvar;
extern CGlobalVars *gpGlobals;
#if defined META_IS_SOURCE2
extern INetworkServerService *netservice;
extern IEngineServiceMgr *enginesvcmgr;
#endif

#endif //_INCLUDE_METAMOD_SOURCE_BASE_PROVIDER_H_

