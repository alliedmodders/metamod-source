/**
 * vim: set ts=4 sw=4 tw=99 noet :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2009 AlliedModders LLC and authors.
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
#include <iserverplugin.h>
#include "ISmmAPI.h"
#include "metamod_provider.h"
#include "metamod_oslink.h"
#if defined DEBUG2
#undef DEBUG2
#define _DEBUG
#endif

using namespace SourceMM;
using namespace SourceHook;

class BaseProvider : public IMetamodSourceProvider
{
public:
	virtual bool IsSourceEngineBuildCompatible(int build);
	virtual bool GetHookInfo(ProvidedHooks hook, SourceHook::MemFuncInfo *pInfo);
	virtual bool LogMessage(const char *buffer);
	virtual const char *GetCommandLineValue(const char *key, const char *defval);
	virtual void ConsolePrint(const char *msg);
	virtual bool IsRemotePrintingAvailable();
	virtual void ClientConsolePrint(edict_t *client, const char *msg);
	virtual void DisplayError(const char *fmt, ...);
	virtual void DisplayWarning(const char *fmt, ...);
	virtual int TryServerGameDLL(const char *iface);
	virtual void Notify_DLLInit_Pre(CreateInterfaceFn engineFactory, CreateInterfaceFn serverFactory);
	void Notify_DLLShutdown_Pre();
	virtual void ServerCommand(const char *cmd);
	virtual ConVar *CreateConVar(const char *name, 
		const char *defval, 
		const char *help,
		int flags);
	virtual const char *GetConVarString(ConVar *convar);
	virtual void SetConVarString(ConVar *convar, const char *str);
	virtual void GetGamePath(char *pszBuffer, int len);
	virtual const char *GetGameDescription();
	virtual IConCommandBaseAccessor *GetConCommandBaseAccessor();
	virtual bool RegisterConCommandBase(ConCommandBase *pCommand);
	virtual void UnregisterConCommandBase(ConCommandBase *pCommand);
	virtual bool IsConCommandBaseACommand(ConCommandBase *pCommand);
	virtual int GetUserMessageCount();
	virtual int FindUserMessage(const char *name, int *size=NULL);
	virtual const char *GetUserMessage(int index, int *size=NULL);
	virtual int DetermineSourceEngine();
	virtual bool ProcessVDF(const char *file, char path[], size_t path_len, char alias[], size_t alias_len);
};

extern IVEngineServer *engine;
extern IServerGameDLL *server;
extern IServerGameClients *gameclients;
extern ICvar *icvar;

#endif //_INCLUDE_METAMOD_SOURCE_BASE_PROVIDER_H_

