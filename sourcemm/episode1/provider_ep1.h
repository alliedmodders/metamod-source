#ifndef _INCLUDE_METAMOD_SOURCE_BASE_PROVIDER_H_
#define _INCLUDE_METAMOD_SOURCE_BASE_PROVIDER_H_

#include <sourcehook.h>
#include <sh_memfuncinfo.h>
#include <iserverplugin.h>
#include "ISmmAPI.h"
#include "metamod_provider.h"
#include "metamod_oslink.h"

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
	virtual IServerPluginCallbacks *GetVSPCallbacks(const char *iface);
	virtual void DisplayError(const char *fmt, ...);
	virtual void DisplayWarning(const char *fmt, ...);
	virtual int TryServerGameDLL(const char *iface);
	virtual void Notify_DLLInit_Pre(void *gamedll,
		CreateInterfaceFn engineFactory, 
		CreateInterfaceFn serverFactory);
	void Notify_DLLShutdown_Pre();
	virtual void ServerCommand(const char *cmd);
	virtual ConVar *CreateConVar(const char *name, 
		const char *defval, 
		const char *help,
		int flags);
	virtual const char *GetConVarString(ConVar *convar);
	virtual const char *GetGameDescription();
	virtual IConCommandBaseAccessor *GetConCommandBaseAccessor();
	virtual bool RegisterConCommandBase(ConCommandBase *pCommand);
	virtual void UnregisterConCommandBase(ConCommandBase *pCommand);
	virtual bool IsConCommandBaseACommand(ConCommandBase *pCommand);
	virtual int GetUserMessageCount();
	virtual int FindUserMessage(const char *name, int *size=NULL);
	virtual const char *GetUserMessage(int index, int *size=NULL);
};

extern IVEngineServer *engine;
extern IServerGameDLL *server;
extern IServerGameClients *gameclients;
extern ICvar *icvar;

#endif //_INCLUDE_METAMOD_SOURCE_BASE_PROVIDER_H_

