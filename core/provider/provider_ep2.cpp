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

#include <stdio.h>
#include <setjmp.h>
#include "../metamod_oslink.h"
#include <sourcehook.h>
#include <convar.h>
#include <eiface.h>
#include <tier0/icommandline.h>
#include <sh_vector.h>
#include <sh_string.h>
#include "../metamod_util.h"
#include "provider_ep2.h"
#include "console.h"
#include "metamod_console.h"
#include <filesystem.h>
#include "metamod.h"
#include <tier1/KeyValues.h>
#if SOURCE_ENGINE == SE_DOTA
#include <iserver.h>
#endif

#if SOURCE_ENGINE == SE_DOTA && defined( _WIN32 )
SH_DECL_HOOK1(ISource2ServerConfig, AllowDedicatedServers, const, 0, bool, EUniverse);
bool BaseProvider::AllowDedicatedServers(EUniverse universe) const
{
	RETURN_META_VALUE(MRES_SUPERCEDE, true);
}
#endif

/* Types */
typedef void (*CONPRINTF_FUNC)(const char *, ...);
struct UsrMsgInfo
{
	UsrMsgInfo()
	{
	}
	UsrMsgInfo(int s, const char *t) : size(s), name(t)
	{
	}
	int size;
	String name;
};

/* Imports */
#if SOURCE_ENGINE < SE_ORANGEBOX
#undef CommandLine
DLL_IMPORT ICommandLine *CommandLine();
#endif

/* Functions */
void CacheUserMessages();
bool KVLoadFromFile(KeyValues *kv, IBaseFileSystem *filesystem, const char *resourceName, const char *pathID = NULL);
void Detour_Error(const tchar *pMsg, ...);

#if SOURCE_ENGINE == SE_DOTA
void ClientCommand(CEntityIndex index, const CCommand &args);
#elif SOURCE_ENGINE >= SE_ORANGEBOX
void ClientCommand(edict_t *pEdict, const CCommand &args);
#else
void ClientCommand(edict_t *pEdict);
#endif

#if SOURCE_ENGINE >= SE_ORANGEBOX
void LocalCommand_Meta(const CCommand &args);
#else
void LocalCommand_Meta();
#endif

void _ServerCommand();
/* Variables */
static BaseProvider g_Ep1Provider;
static List<ConCommandBase *> conbases_unreg;
static CVector<UsrMsgInfo> usermsgs_list;
static jmp_buf usermsg_end;
static bool g_bOriginalEngine = false;

ICvar *icvar = NULL;
IFileSystem *baseFs = NULL;
IServerGameDLL *server = NULL;
#if SOURCE_ENGINE == SE_DOTA
static ISource2ServerConfig *serverconfig = NULL;
INetworkServerService *netservice = NULL;
IEngineServiceMgr *enginesvcmgr = NULL;
#endif
IVEngineServer *engine = NULL;
IServerGameClients *gameclients = NULL;
CGlobalVars *gpGlobals = NULL;
IMetamodSourceProvider *provider = &g_Ep1Provider;
ConCommand meta_local_cmd("meta", LocalCommand_Meta, "Metamod:Source control options");

#if SOURCE_ENGINE == SE_DOTA
SH_DECL_HOOK2_void(IServerGameClients, ClientCommand, SH_NOATTRIB, 0, CEntityIndex, const CCommand &);
#elif SOURCE_ENGINE >= SE_ORANGEBOX
SH_DECL_HOOK2_void(IServerGameClients, ClientCommand, SH_NOATTRIB, 0, edict_t *, const CCommand &);
#else
SH_DECL_HOOK1_void(IServerGameClients, ClientCommand, SH_NOATTRIB, 0, edict_t *);
#endif

void BaseProvider::ConsolePrint(const char *str)
{
#if SOURCE_ENGINE >= SE_ORANGEBOX
	ConMsg("%s", str);
#else
	Msg("%s", str);
#endif
}

void BaseProvider::Notify_DLLInit_Pre(CreateInterfaceFn engineFactory, 
									  CreateInterfaceFn serverFactory)
{
#if SOURCE_ENGINE == SE_TF2 || SOURCE_ENGINE == SE_CSS || SOURCE_ENGINE == SE_DODS || SOURCE_ENGINE == SE_HL2DM || SOURCE_ENGINE == SE_SDK2013
	// Shim to avoid hooking shims
	engine = (IVEngineServer *)((engineFactory)("VEngineServer023", NULL));
	if (!engine)
	{
		engine = (IVEngineServer *)((engineFactory)("VEngineServer022", NULL));
		if (!engine)
		{
			engine = (IVEngineServer *)((engineFactory)("VEngineServer021", NULL));
		}
	}
#else
	engine = (IVEngineServer *)((engineFactory)(INTERFACEVERSION_VENGINESERVER, NULL));
#endif
	if (!engine)
	{
		DisplayError("Could not find IVEngineServer! Metamod cannot load.");
		return;
	}
#if SOURCE_ENGINE == SE_DOTA
	gpGlobals = engine->GetServerGlobals();
	serverconfig = (ISource2ServerConfig *) ((serverFactory) (INTERFACEVERSION_SERVERCONFIG, NULL));
	netservice = (INetworkServerService *) ((engineFactory) (NETWORKSERVERSERVICE_INTERFACE_VERSION, NULL));
	enginesvcmgr = (IEngineServiceMgr *) ((engineFactory) (ENGINESERVICEMGR_INTERFACE_VERSION, NULL));
#endif
#if SOURCE_ENGINE >= SE_ORANGEBOX
	icvar = (ICvar *)((engineFactory)(CVAR_INTERFACE_VERSION, NULL));
#else
	icvar = (ICvar *)((engineFactory)(VENGINE_CVAR_INTERFACE_VERSION, NULL));
#endif
	if (!icvar)
	{
		DisplayError("Could not find ICvar! Metamod cannot load.");
		return;
	}

#if SOURCE_ENGINE == SE_DOTA
	gameclients = (IServerGameClients *)(serverFactory(INTERFACEVERSION_SERVERGAMECLIENTS, NULL));
#else
	if ((gameclients = (IServerGameClients *)(serverFactory("ServerGameClients003", NULL)))
		== NULL)
	{
		gameclients = (IServerGameClients *)(serverFactory("ServerGameClients004", NULL));
	}
#endif

	baseFs = (IFileSystem *)((engineFactory)(FILESYSTEM_INTERFACE_VERSION, NULL));
	if (baseFs == NULL)
	{
		mm_LogMessage("Unable to find \"%s\": .vdf files will not be parsed", FILESYSTEM_INTERFACE_VERSION);
	}

#if SOURCE_ENGINE == SE_DOTA && 0
	// Since we have to be added as a Game path (cannot add GameBin directly), we
	// automatically get added to other paths as well, including having the MM:S
	// dir become the default write path for logs and more. We can fix some of these.

	char searchPath[260];
	baseFs->GetSearchPath("GAME", (GetSearchPathTypes_t)0, searchPath, sizeof(searchPath));
	for (size_t i = 0; i < sizeof(searchPath); ++i)
	{
		if (searchPath[i] == ';')
		{
			searchPath[i] = '\0';
			break;
		}
	}
	baseFs->RemoveSearchPath(searchPath, "GAME");

	// TODO: figure out why these calls get ignored and path remains
	//baseFs->RemoveSearchPath(searchPath, "CONTENT");
	//baseFs->RemoveSearchPath(searchPath, "SHADER_SOURCE");
	//baseFs->RemoveSearchPath(searchPath, "SHADER_SOURCE_MOD");

	baseFs->RemoveSearchPaths("DEFAULT_WRITE_PATH");
	baseFs->GetSearchPath("GAME", (GetSearchPathTypes_t)0, searchPath, sizeof(searchPath));
	for (size_t i = 0; i < sizeof(searchPath); ++i)
	{
		if (searchPath[i] == ';')
		{
			searchPath[i] = '\0';
			break;
		}
	}
	baseFs->AddSearchPath(searchPath, "DEFAULT_WRITE_PATH");
#endif

#if SOURCE_ENGINE >= SE_ORANGEBOX
	g_pCVar = icvar;
#endif

	g_SMConVarAccessor.RegisterConCommandBase(&meta_local_cmd);
	
#if SOURCE_ENGINE == SE_EPISODEONE
	/* The Ship is the only game known at this time that uses the pre-Episode One engine */
	g_bOriginalEngine = strcmp(CommandLine()->ParmValue("-game", "hl2"), "ship") == 0;
#endif

	CacheUserMessages();

#if SOURCE_ENGINE < SE_ORANGEBOX
	if (!g_SMConVarAccessor.InitConCommandBaseList())
	{
		/* This is very unlikely considering it's old engine */
		mm_LogMessage("[META] Warning: Failed to find ConCommandBase list!");
		mm_LogMessage("[META] Warning: ConVars and ConCommands cannot be unregistered properly! Please file a bug report.");
	}
#endif

	if (gameclients)
	{
		SH_ADD_HOOK_STATICFUNC(IServerGameClients, ClientCommand, gameclients, ClientCommand, false);
	}

#if SOURCE_ENGINE == SE_DOTA && defined( _WIN32 )
	SH_ADD_VPHOOK(ISource2ServerConfig, AllowDedicatedServers, serverconfig, SH_MEMBER(this, &BaseProvider::AllowDedicatedServers), false);
#endif
}

void BaseProvider::Notify_DLLShutdown_Pre()
{
	g_SMConVarAccessor.RemoveMetamodCommands();

#if SOURCE_ENGINE < SE_ORANGEBOX
	if (g_Metamod.IsLoadedAsGameDLL())
	{
		icvar->UnlinkVariables(FCVAR_GAMEDLL);
	}
#endif
}

bool BaseProvider::IsRemotePrintingAvailable()
{
	return true;
}

void BaseProvider::ClientConsolePrint(edict_t *pEdict, const char *message)
{
#if SOURCE_ENGINE == SE_DOTA
	int client = (int)(pEdict - gpGlobals->pEdicts);
	engine->ClientPrintf(client, message);
#else
	engine->ClientPrintf(pEdict, message);
#endif
}

void BaseProvider::ServerCommand(const char *cmd)
{
	engine->ServerCommand(cmd);
}

const char *BaseProvider::GetConVarString(ConVar *convar)
{
	if (convar == NULL)
	{
		return NULL;
	}

	return convar->GetString();
}

void BaseProvider::SetConVarString(ConVar *convar, const char *str)
{
	convar->SetValue(str);
}

bool BaseProvider::IsConCommandBaseACommand(ConCommandBase *pCommand)
{
	return pCommand->IsCommand();
}


bool BaseProvider::IsSourceEngineBuildCompatible(int build)
{
	return (build == SOURCE_ENGINE_ORIGINAL
			|| build == SOURCE_ENGINE_EPISODEONE);
}

const char *BaseProvider::GetCommandLineValue(const char *key, const char *defval)
{
	if (key[0] == '-' || key[0] == '+')
	{
		return CommandLine()->ParmValue(key, defval);
	}
	else if (icvar)
	{
		const char *val;
		if ((val = icvar->GetCommandLineValue(key)) == NULL)
		{
			return defval;
		}

		return val;
	}

	return NULL;
}

int BaseProvider::TryServerGameDLL(const char *iface)
{
	if (strncmp(iface, "ServerGameDLL", 13) != 0)
	{
		return 0;
	}

	return atoi(&iface[13]);
}

bool BaseProvider::LogMessage(const char *buffer)
{
	if (!engine)
	{
		return false;
	}

	engine->LogPrint(buffer);

	return true;
}

bool BaseProvider::GetHookInfo(ProvidedHooks hook, SourceHook::MemFuncInfo *pInfo)
{
#if SOURCE_ENGINE == SE_DOTA
	SourceHook::MemFuncInfo mfi = {true, -1, 0, 0};

	switch (hook)
	{
	case ProvidedHook_StartupServer:
		SourceHook::GetFuncInfo(&INetworkServerService::StartupServer, mfi);
		break;
	case ProvidedHook_StartChangeLevel:
		SourceHook::GetFuncInfo(&INetworkGameServer::StartChangeLevel, mfi);
		break;
	case ProvidedHook_Init:
		SourceHook::GetFuncInfo(&INetworkGameServer::Init, mfi);
		break;
	case ProvidedHook_SwitchToLoop:
		SourceHook::GetFuncInfo(&IEngineServiceMgr::SwitchToLoop, mfi);
		break;
	default:
		return false;
	}

	*pInfo = mfi;

	return (mfi.thisptroffs >= 0);
#else
	SourceHook::MemFuncInfo mfi = {true, -1, 0, 0};

	if (hook == ProvidedHook_LevelInit)
	{
		SourceHook::GetFuncInfo(&IServerGameDLL::LevelInit, mfi);
	}
	else if (hook == ProvidedHook_LevelShutdown)
	{
		SourceHook::GetFuncInfo(&IServerGameDLL::LevelShutdown, mfi);
	}
	else if (hook == ProvidedHook_GameInit)
	{
		SourceHook::GetFuncInfo(&IServerGameDLL::GameInit, mfi);
	}

	*pInfo = mfi;

	return (mfi.thisptroffs >= 0);
#endif
}

void BaseProvider::DisplayError(const char *fmt, ...)
{
	va_list ap;
	char buffer[2048];

	va_start(ap, fmt);
	UTIL_FormatArgs(buffer, sizeof(buffer), fmt, ap);
	va_end(ap);

	Error("%s", buffer);
}

void BaseProvider::DisplayWarning(const char *fmt, ...)
{
	va_list ap;
	char buffer[2048];

	va_start(ap, fmt);
	UTIL_FormatArgs(buffer, sizeof(buffer), fmt, ap);
	va_end(ap);

	Warning("%s", buffer);
}

IConCommandBaseAccessor *BaseProvider::GetConCommandBaseAccessor()
{
	return &g_SMConVarAccessor;
}

bool BaseProvider::RegisterConCommandBase(ConCommandBase *pCommand)
{
	return g_SMConVarAccessor.Register(pCommand);
}

void BaseProvider::UnregisterConCommandBase(ConCommandBase *pCommand)
{
	return g_SMConVarAccessor.Unregister(pCommand);
}

int BaseProvider::GetUserMessageCount()
{
#if SOURCE_ENGINE == SE_CSGO || SOURCE_ENGINE == SE_DOTA || SOURCE_ENGINE == SE_BLADE
	return -1;
#else
	return (int)usermsgs_list.size();
#endif
}

int BaseProvider::FindUserMessage(const char *name, int *size)
{
	for (size_t i = 0; i < usermsgs_list.size(); i++)
	{
		if (usermsgs_list[i].name.compare(name) == 0)
		{
			if (size)
			{
				*size = usermsgs_list[i].size;
			}
			return (int)i;
		}
	}
	
	return -1;
}

const char *BaseProvider::GetUserMessage(int index, int *size)
{
	if (index < 0 || index >= (int)usermsgs_list.size())
	{
		return NULL;
	}

	if (size)
	{
		*size = usermsgs_list[index].size;
	}

	return usermsgs_list[index].name.c_str();
}

void BaseProvider::GetGamePath(char *pszBuffer, int len)
{
	engine->GetGameDir(pszBuffer, len);
}

const char *BaseProvider::GetGameDescription()
{
#if SOURCE_ENGINE == SE_DOTA
	return serverconfig->GetGameDescription();
#else
	return server->GetGameDescription();
#endif
}

int BaseProvider::DetermineSourceEngine()
{
#if SOURCE_ENGINE == SE_BLOODYGOODTIME
	return SOURCE_ENGINE_BLOODYGOODTIME;
#elif SOURCE_ENGINE == SE_ALIENSWARM
	return SOURCE_ENGINE_ALIENSWARM;
#elif SOURCE_ENGINE == SE_LEFT4DEAD2
	return SOURCE_ENGINE_LEFT4DEAD2;
#elif SOURCE_ENGINE == SE_NUCLEARDAWN
	return SOURCE_ENGINE_NUCLEARDAWN;
#elif SOURCE_ENGINE == SE_CONTAGION
	return SOURCE_ENGINE_CONTAGION;
#elif SOURCE_ENGINE == SE_LEFT4DEAD
	return SOURCE_ENGINE_LEFT4DEAD;
#elif SOURCE_ENGINE == SE_ORANGEBOX
	return SOURCE_ENGINE_ORANGEBOX;
#elif SOURCE_ENGINE == SE_CSS
	return SOURCE_ENGINE_CSS;
#elif SOURCE_ENGINE == SE_HL2DM
	return SOURCE_ENGINE_HL2DM;
#elif SOURCE_ENGINE == SE_DODS
	return SOURCE_ENGINE_DODS;
#elif SOURCE_ENGINE == SE_SDK2013
	return SOURCE_ENGINE_SDK2013;
#elif SOURCE_ENGINE == SE_TF2
	return SOURCE_ENGINE_TF2;
#elif SOURCE_ENGINE == SE_DARKMESSIAH
	return SOURCE_ENGINE_DARKMESSIAH;
#elif SOURCE_ENGINE == SE_EYE
	return SOURCE_ENGINE_EYE;
#elif SOURCE_ENGINE == SE_PORTAL2
	return SOURCE_ENGINE_PORTAL2;
#elif SOURCE_ENGINE == SE_BLADE
	return SOURCE_ENGINE_BLADE;
#elif SOURCE_ENGINE == SE_INSURGENCY
	return SOURCE_ENGINE_INSURGENCY;
#elif SOURCE_ENGINE == SE_DOI
	return SOURCE_ENGINE_DOI;
#elif SOURCE_ENGINE == SE_CSGO
	return SOURCE_ENGINE_CSGO;
#elif SOURCE_ENGINE == SE_DOTA
	return SOURCE_ENGINE_DOTA;
#elif SOURCE_ENGINE == SE_BMS
	return SOURCE_ENGINE_BMS;
#elif SOURCE_ENGINE == SE_EPISODEONE
	return g_bOriginalEngine ? SOURCE_ENGINE_ORIGINAL : SOURCE_ENGINE_EPISODEONE;
#elif SOURCE_ENGINE == SE_MOCK
	return SOURCE_ENGINE_MOCK;
#else
#error "SOURCE_ENGINE not defined to a known value"
#endif
}

ConVar *BaseProvider::CreateConVar(const char *name,
								   const char *defval,
								   const char *help,
								   int flags)
{
	int newflags = 0;
	if (flags & ConVarFlag_Notify)
	{
		newflags |= FCVAR_NOTIFY;
	}
	if (flags & ConVarFlag_SpOnly)
	{
		newflags |= FCVAR_SPONLY;
	}

	ConVar *pVar = new ConVar(name, defval, newflags, help);

	g_SMConVarAccessor.RegisterConCommandBase(pVar);

	return pVar;
}

bool BaseProvider::ProcessVDF(const char *file, char path[], size_t path_len, char alias[], size_t alias_len)
{
	if (baseFs == NULL)
	{
		return false;
	}

	KeyValues *pValues;
	bool bKVLoaded = false;
	const char *plugin_file, *p_alias;

	pValues = new KeyValues("Metamod Plugin");

	if (g_bOriginalEngine)
	{
		/* The Ship must use a special version of this function */
		bKVLoaded = KVLoadFromFile(pValues, baseFs, file);
	}
	else
	{
		bKVLoaded = pValues->LoadFromFile(baseFs, file);
	}
	
	if (!bKVLoaded)
	{
		pValues->deleteThis();
		return false;
	}

	if ((plugin_file = pValues->GetString("file", NULL)) == NULL)
	{
		pValues->deleteThis();
		return false;
	}

	UTIL_Format(path, path_len, "%s", plugin_file);

	if ((p_alias = pValues->GetString("alias", NULL)) != NULL)
	{
		UTIL_Format(alias, alias_len, "%s", p_alias);
	}
	else
	{
		UTIL_Format(alias, alias_len, "");
	}

	pValues->deleteThis();

	return true;
}

const char *BaseProvider::GetEngineDescription() const
{
#if SOURCE_ENGINE == SE_BLOODYGOODTIME
	return "Bloody Good Time (2010)";
#elif SOURCE_ENGINE == SE_ALIENSWARM
	return "Alien Swarm (2010)";
#elif SOURCE_ENGINE == SE_LEFT4DEAD2
	return "Left 4 Dead 2 (2009)";
#elif SOURCE_ENGINE == SE_NUCLEARDAWN
	return "Nuclear Dawn (2011)";
#elif SOURCE_ENGINE == SE_CONTAGION
	return "Contagion (2013)";
#elif SOURCE_ENGINE == SE_LEFT4DEAD
	return "Left 4 Dead (2008)";
#elif SOURCE_ENGINE == SE_ORANGEBOX
	return "Episode 2 (Orange Box, 2007)";
#elif SOURCE_ENGINE == SE_CSS
	return "Counter-Strike: Source (Valve Orange Box)";
#elif SOURCE_ENGINE == SE_HL2DM
	return "Half-Life 2 Deathmatch (Valve Orange Box)";
#elif SOURCE_ENGINE == SE_DODS
	return "Day of Defeat: Source (Valve Orange Box)";
#elif SOURCE_ENGINE == SE_SDK2013
	return "Source SDK 2013 (2013)";
#elif SOURCE_ENGINE == SE_BMS
	return "Black Mesa (2015)";
#elif SOURCE_ENGINE == SE_TF2
	return "Team Fortress 2 (Valve Orange Box)";
#elif SOURCE_ENGINE == SE_DARKMESSIAH
	return "Dark Messiah (2006)";
#elif SOURCE_ENGINE == SE_EYE
	return "E.Y.E. Divine Cybermancy (2011)";
#elif SOURCE_ENGINE == SE_PORTAL2
	return "Portal 2 (2011)";
#elif SOURCE_ENGINE == SE_BLADE
	return "Blade Symphony (2013)";
#elif SOURCE_ENGINE == SE_INSURGENCY
	return "Insurgency (2013)";
#elif SOURCE_ENGINE == SE_DOI
	return "Day of Infamy (2016)";
#elif SOURCE_ENGINE == SE_CSGO
	return "Counter-Strike: Global Offensive (2012)";
#elif SOURCE_ENGINE == SE_DOTA
	return "Dota 2 (2013)";
#elif SOURCE_ENGINE == SE_EPISODEONE
	if (g_bOriginalEngine)
	{
		return "Original (pre-Episode 1)";
	}
	else
	{
		return "Episode 1 (2004)";
	}
#elif SOURCE_ENGINE == SE_MOCK
	return "Mock";
#else
#error "SOURCE_ENGINE not defined to a known value"
#endif
}

#if SOURCE_ENGINE >= SE_ORANGEBOX
class GlobCommand : public IMetamodSourceCommandInfo
{
public:
	GlobCommand(const CCommand *cmd) : m_cmd(cmd)
	{
	}
public:
	unsigned int GetArgCount()
	{
		return m_cmd->ArgC() - 1;
	}

	const char *GetArg(unsigned int num)
	{
		return m_cmd->Arg(num);
	}

	const char *GetArgString()
	{
		return m_cmd->ArgS();
	}
private:
	const CCommand *m_cmd;
};
#else
class GlobCommand : public IMetamodSourceCommandInfo
{
public:
	unsigned int GetArgCount()
	{
		return engine->Cmd_Argc() - 1;
	}

	const char *GetArg(unsigned int num)
	{
		return engine->Cmd_Argv(num);
	}

	const char *GetArgString()
	{
		return engine->Cmd_Args();
	}
};
#endif

#if SOURCE_ENGINE >= SE_ORANGEBOX
void LocalCommand_Meta(const CCommand &args)
{
	GlobCommand cmd(&args);
#else
void LocalCommand_Meta()
{
	GlobCommand cmd;
#endif
	Command_Meta(&cmd);
}

#if SOURCE_ENGINE == SE_DOTA
void ClientCommand(CEntityIndex index, const CCommand &_cmd)
{
	int client = index.Get();
	GlobCommand cmd(&_cmd);
#elif SOURCE_ENGINE >= SE_ORANGEBOX
void ClientCommand(edict_t *client, const CCommand &_cmd)
{
	GlobCommand cmd(&_cmd);
#else
void ClientCommand(edict_t *client)
{
	GlobCommand cmd;
#endif
	if (strcmp(cmd.GetArg(0), "meta") == 0)
	{
		Command_ClientMeta(client, &cmd);
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

#if SOURCE_ENGINE == SE_CSGO || SOURCE_ENGINE == SE_DOTA || SOURCE_ENGINE == SE_BLADE

void CacheUserMessages()
{
}

#else

/* This only gets called if IServerGameDLL::GetUserMessageInfo() triggers it */
void Detour_Error(const tchar *pMsg, ...)
{
	/* Jump back to setjmp() in CacheUserMessages() */
	longjmp(usermsg_end, 1);
}

#define IA32_JMP_IMM32 0xE9

/* IServerGameDLL::GetUserMessageInfo() crashes on games based on the old engine and
 * early Orange Box. This is because Error() from tier0 gets called when a bad index is
 * passed. This is all due to a bug in CUtlRBTree::IsValidIndex().
 *
 * So we detour Error() to fix this. Our detour then jumps back into CacheUserMessages()
 * to a point before GetUserMessageInfo() is called. The detour is then removed and we
 * exit.
 */
void CacheUserMessages()
{
	int q, size;
	char buffer[256];
	unsigned char *target, *detour;
	unsigned char orig_bytes[5];

	target = (unsigned char *)&Error;
	detour = (unsigned char *)&Detour_Error;

	/* Save bytes from target function */
	memcpy(orig_bytes, target, sizeof(orig_bytes));

	/* Patch in relative jump to our Error() detour */
	SetMemAccess(target, sizeof(orig_bytes), SH_MEM_READ|SH_MEM_WRITE|SH_MEM_EXEC);
	target[0] = IA32_JMP_IMM32;
	*(int32_t *)&target[1] = (int32_t)(detour - (target + 5));

	/* This is where longjmp() will end up */
	if (setjmp(usermsg_end))
	{
		/* Restore bytes and memory protection */
		memcpy(target, orig_bytes, sizeof(orig_bytes));
		SetMemAccess(target, sizeof(orig_bytes), SH_MEM_READ|SH_MEM_EXEC);
		return;
	}

	q = 0;

	/* If GetUserMessageInfo() calls Error(), we should end up in our detour */
	while (server->GetUserMessageInfo(q, buffer, sizeof(buffer), size))
	{
		usermsgs_list.push_back(UsrMsgInfo(size, buffer));
		q++;
	}

	/* Jump back to setjmp() */
	longjmp(usermsg_end, 1);
}

#endif

bool KVLoadFromFile(KeyValues *kv, IBaseFileSystem *filesystem, const char *resourceName, const char *pathID)
{
	Assert(filesystem);
#ifdef _MSC_VER
	Assert(_heapchk() == _HEAPOK);
#endif

	FileHandle_t f = filesystem->Open(resourceName, "rb", pathID);
	if (!f)
		return false;

	// load file into a null-terminated buffer
	int fileSize = filesystem->Size(f);
	char *buffer = (char *)MemAllocScratch(fileSize + 1);
	
	Assert(buffer);
	
	filesystem->Read(buffer, fileSize, f); // read into local buffer

	buffer[fileSize] = 0; // null terminate file as EOF

	filesystem->Close( f );	// close file after reading

	bool retOK = kv->LoadFromBuffer( resourceName, buffer, filesystem );

	MemFreeScratch();

	return retOK;
}
