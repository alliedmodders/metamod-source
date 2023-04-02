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

#include "provider_source2.h"
#include <metamod.h>
#include <metamod_util.h>
#include <metamod_console.h>
#include <amtl/am-string.h>
#include <eiface.h>
#include <KeyValues.h>
#include <filesystem.h>
#include <iserver.h>

void LocalCommand_Meta(const CCommand& args);

ConCommand meta_local_cmd("meta", LocalCommand_Meta, "Metamod:Source control options");

static ISource2ServerConfig* serverconfig = NULL;
INetworkServerService* netservice = NULL;
IEngineServiceMgr* enginesvcmgr = NULL;

// Hack to make hook decl compile when only having forward decl in header.
// (we have class structure but it requires protobuf which we don't want to include here)
class GameSessionConfiguration_t { };

SH_DECL_HOOK3_void(INetworkServerService, StartupServer, SH_NOATTRIB, 0, const GameSessionConfiguration_t &, ISource2WorldSession *, const char *);
SH_DECL_HOOK5_void(IEngineServiceMgr, SwitchToLoop, SH_NOATTRIB, 0, const char *, KeyValues *, uint32, const char *, bool);
SH_DECL_HOOK2_void(INetworkGameServer, Init, SH_NOATTRIB, 0, const GameSessionConfiguration_t &, const char *);
SH_DECL_HOOK3(INetworkGameServer, StartChangeLevel, SH_NOATTRIB, 0, CUtlVector<INetworkGameClient *> *, const char *, const char *, void *);
SH_DECL_HOOK2_void(IServerGameClients, ClientCommand, SH_NOATTRIB, 0, CEntityIndex, const CCommand&);

#ifdef SHOULD_OVERRIDE_ALLOWDEDICATED_SERVER
SH_DECL_HOOK1(ISource2ServerConfig, AllowDedicatedServers, const, 0, bool, EUniverse);
#endif

void Source2Provider::Notify_DLLInit_Pre(CreateInterfaceFn engineFactory,
	CreateInterfaceFn serverFactory)
{
	engine = (IVEngineServer*)((engineFactory)(INTERFACEVERSION_VENGINESERVER, NULL));
	if (!engine)
	{
		DisplayError("Could not find IVEngineServer! Metamod cannot load.");
		return;
	}

	gpGlobals = engine->GetServerGlobals();
	serverconfig = (ISource2ServerConfig*)((serverFactory)(INTERFACEVERSION_SERVERCONFIG, NULL));
	netservice = (INetworkServerService*)((engineFactory)(NETWORKSERVERSERVICE_INTERFACE_VERSION, NULL));
	enginesvcmgr = (IEngineServiceMgr*)((engineFactory)(ENGINESERVICEMGR_INTERFACE_VERSION, NULL));

	icvar = (ICvar*)((engineFactory)(CVAR_INTERFACE_VERSION, NULL));
	if (!icvar)
	{
		DisplayError("Could not find ICvar! Metamod cannot load.");
		return;
	}

	gameclients = (IServerGameClients*)(serverFactory(INTERFACEVERSION_SERVERGAMECLIENTS, NULL));
	baseFs = (IFileSystem*)((engineFactory)(FILESYSTEM_INTERFACE_VERSION, NULL));
	if (baseFs == NULL)
	{
		mm_LogMessage("Unable to find \"%s\": .vdf files will not be parsed", FILESYSTEM_INTERFACE_VERSION);
	}

#if 0
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

	g_pCVar = icvar;

#ifdef S2_CONVAR_UNFINISHED
	g_SMConVarAccessor.RegisterConCommandBase(&meta_local_cmd);
#endif

	if (gameclients)
	{
		SH_ADD_HOOK(IServerGameClients, ClientCommand, gameclients, SH_MEMBER(this, &Source2Provider::Hook_ClientCommand), false);
	}

#ifdef SHOULD_OVERRIDE_ALLOWDEDICATED_SERVER
	SH_ADD_VPHOOK(ISource2ServerConfig, AllowDedicatedServers, serverconfig, SH_MEMBER(this, &Source2Provider::Hook_AllowDedicatedServers), false);
#endif

	SH_ADD_HOOK(INetworkServerService, StartupServer, netservice, SH_MEMBER(this, &Source2Provider::Hook_StartupServer_Post), true);
	SH_ADD_HOOK(IEngineServiceMgr, SwitchToLoop, enginesvcmgr, SH_MEMBER(this, &Source2Provider::Hook_SwitchToLoop), false);
}

void Source2Provider::Notify_DLLShutdown_Pre()
{
#ifdef S2_CONVAR_UNFINISHED
	g_SMConVarAccessor.RemoveMetamodCommands();
#endif
}

bool Source2Provider::ProcessVDF(const char* file, char path[], size_t path_len, char alias[], size_t alias_len)
{
	if (baseFs == NULL)
	{
		return false;
	}

	KeyValues* pValues;
	bool bKVLoaded = false;
	const char* plugin_file, * p_alias;

	pValues = new KeyValues("Metamod Plugin");

	bKVLoaded = pValues->LoadFromFile(baseFs, file);
	if (!bKVLoaded)
	{
		delete pValues;
		return false;
	}

	if ((plugin_file = pValues->GetString("file", NULL)) == NULL)
	{
		delete pValues;
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

	delete pValues;

	return true;
}

int Source2Provider::DetermineSourceEngine()
{
#if SOURCE_ENGINE == SE_DOTA
	return SOURCE_ENGINE_DOTA;
#elif SOURCE_ENGINE == SE_CS2
	return SOURCE_ENGINE_CS2;
#else
#error "SOURCE_ENGINE not defined to a known value"
#endif
}

const char* Source2Provider::GetEngineDescription() const
{
#if SOURCE_ENGINE == SE_DOTA
	return "Dota 2 (2013)";
#elif SOURCE_ENGINE == SE_CS2
	return "Counter-Strike 2 (2023)";
#else
#error "SOURCE_ENGINE not defined to a known value"
#endif
}

void Source2Provider::GetGamePath(char* pszBuffer, int len)
{
	ke::SafeSprintf(pszBuffer, len, "%s", Plat_GetGameDirectory());
}

const char* Source2Provider::GetGameDescription()
{
	return serverconfig->GetGameDescription();
}

#ifdef SHOULD_OVERRIDE_ALLOWDEDICATED_SERVER
bool Source2Provider::Hook_AllowDedicatedServers(EUniverse universe) const
{
	RETURN_META_VALUE(MRES_SUPERCEDE, true);
}
#endif

void Source2Provider::ConsolePrint(const char* str)
{
	ConMsg("%s", str);
}

void Source2Provider::ClientConsolePrint(edict_t* pEdict, const char* message)
{
	int client = (int)(pEdict - gpGlobals->pEdicts);
	engine->ClientPrintf(client, message);
}

void Source2Provider::ServerCommand(const char* cmd)
{
	engine->ServerCommand(cmd);
}

const char* Source2Provider::GetConVarString(MetamodSourceConVar *convar)
{
#ifdef S2_CONVAR_UNFINISHED
	if (convar == NULL)
	{
		return NULL;
	}

	return convar->GetString();
#else
	return "";
#endif
}

void Source2Provider::SetConVarString(MetamodSourceConVar *convar, const char* str)
{
#ifdef S2_CONVAR_UNFINISHED
	convar->SetValue(str);
#endif
}

bool Source2Provider::IsConCommandBaseACommand(ConCommandBase* pCommand)
{
#ifdef S2_CONVAR_UNFINISHED
	return pCommand->IsCommand();
#else
	return false;
#endif
}

bool Source2Provider::RegisterConCommandBase(ConCommandBase* pCommand)
{
#ifdef S2_CONVAR_UNFINISHED
	return g_SMConVarAccessor.Register(pCommand);
#else
	return true;
#endif
}

void Source2Provider::UnregisterConCommandBase(ConCommandBase* pCommand)
{
#ifdef S2_CONVAR_UNFINISHED
	return g_SMConVarAccessor.Unregister(pCommand);
#endif
}

MetamodSourceConVar* Source2Provider::CreateConVar(const char* name,
	const char* defval,
	const char* help,
	int flags)
{
#ifdef S2_CONVAR_UNFINISHED
	int newflags = 0;
	if (flags & ConVarFlag_Notify)
	{
		newflags |= FCVAR_NOTIFY;
	}
	if (flags & ConVarFlag_SpOnly)
	{
		newflags |= FCVAR_SPONLY;
	}

	ConVar* pVar = new ConVar(name, defval, newflags, help);

	g_SMConVarAccessor.RegisterConCommandBase(pVar);

	return pVar;
#else
	return nullptr;
#endif
}

class GlobCommand : public IMetamodSourceCommandInfo
{
public:
	GlobCommand(const CCommand* cmd) : m_cmd(cmd)
	{
	}
public:
	unsigned int GetArgCount()
	{
		return m_cmd->ArgC() - 1;
	}

	const char* GetArg(unsigned int num)
	{
		return m_cmd->Arg(num);
	}

	const char* GetArgString()
	{
		return m_cmd->ArgS();
	}
private:
	const CCommand* m_cmd;
};

void LocalCommand_Meta(const CCommand& args)
{
	GlobCommand cmd(&args);
	Command_Meta(&cmd);
}

bool Source2Provider::KVLoadFromFile(KeyValues* kv, IFileSystem* filesystem, const char* resourceName, const char* pathID)
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
	char* buffer = (char*)MemAllocScratch(fileSize + 1);

	Assert(buffer);

	filesystem->Read(buffer, fileSize, f); // read into local buffer

	buffer[fileSize] = 0; // null terminate file as EOF

	filesystem->Close(f);	// close file after reading

	bool retOK = kv->LoadFromBuffer(resourceName, buffer, filesystem);

	MemFreeScratch();

	return retOK;
}

void Source2Provider::Hook_StartupServer_Post(const GameSessionConfiguration_t &config, ISource2WorldSession *, const char *)
{
	static bool bGameServerHooked = false;
	if (!bGameServerHooked)
	{
		INetworkGameServer* netserver = (META_IFACEPTR(INetworkServerService))->GetIGameServer();

		SH_ADD_VPHOOK(INetworkGameServer, Init, netserver, SH_MEMBER(this, &Source2Provider::Hook_Init), false);
		SH_ADD_VPHOOK(INetworkGameServer, StartChangeLevel, netserver, SH_MEMBER(this, &Source2Provider::Hook_StartChangeLevel), false);

		bGameServerHooked = true;
	}

	RETURN_META(MRES_IGNORED);
}

void Source2Provider::Hook_Init(const GameSessionConfiguration_t &config, const char *pszMapName)
{
	static char szLastMap[260] = "";
	if (nullptr != m_pCallbacks)
	{
		m_pCallbacks->OnLevelInit(pszMapName, "", sLastMap.c_str(), "", false, false);
	}

	sLastMap = pszMapName;

	RETURN_META(MRES_IGNORED);
}

CUtlVector<INetworkGameClient *> *Source2Provider::Hook_StartChangeLevel(const char *, const char *, void *)
{
	if (nullptr != m_pCallbacks)
	{
		m_pCallbacks->OnLevelShutdown();
	}

	RETURN_META_VALUE(MRES_IGNORED, nullptr);
}

void Source2Provider::Hook_SwitchToLoop(const char *pszLoopName, KeyValues *pKV, uint32 nId, const char *pszUnk, bool bUnk)
{
	if (nullptr != m_pCallbacks && strcmp(pszLoopName, "levelload") == 0)
	{
		m_pCallbacks->OnGameInit();
	}

	RETURN_META(MRES_IGNORED);
}

void Source2Provider::Hook_ClientCommand(CEntityIndex index, const CCommand& _cmd)
{
	int client = index.Get();
	GlobCommand cmd(&_cmd);

	if (strcmp(cmd.GetArg(0), "meta") == 0)
	{
		Command_ClientMeta(client, &cmd);
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

static Source2Provider g_Source2Provider;

IMetamodSourceProvider* provider = &g_Source2Provider;