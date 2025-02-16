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


static Source2Provider g_Source2Provider;

IMetamodSourceProvider* provider = &g_Source2Provider;

CON_COMMAND_EXTERN(meta, LocalCommand_Meta, "Metamod:Source control options");


static ISource2ServerConfig* serverconfig = NULL;
INetworkServerService* netservice = NULL;
IEngineServiceMgr* enginesvcmgr = NULL;

SH_DECL_HOOK2_void(IServerGameClients, ClientCommand, SH_NOATTRIB, 0, CPlayerSlot, const CCommand&);
SH_DECL_HOOK3_void(IEngineServiceMgr, RegisterLoopMode, SH_NOATTRIB, 0, const char *, ILoopModeFactory *, void **);
SH_DECL_HOOK3_void(IEngineServiceMgr, UnregisterLoopMode, SH_NOATTRIB, 0, const char*, ILoopModeFactory*, void**);
SH_DECL_HOOK0(ILoopModeFactory, CreateLoopMode, SH_NOATTRIB, 0, ILoopMode *);
SH_DECL_HOOK1_void(ILoopModeFactory, DestroyLoopMode, SH_NOATTRIB, 0, ILoopMode *);
SH_DECL_HOOK2(ILoopMode, LoopInit, SH_NOATTRIB, 0, bool, KeyValues*, ILoopModePrerequisiteRegistry *);
SH_DECL_HOOK0_void(ILoopMode, LoopShutdown, SH_NOATTRIB, 0);

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
	g_pFullFileSystem = baseFs = (IFileSystem*)((engineFactory)(FILESYSTEM_INTERFACE_VERSION, NULL));
	if (baseFs == NULL)
	{
		mm_LogMessage("Unable to find \"%s\": .vdf files will not be parsed", FILESYSTEM_INTERFACE_VERSION);
	}

	// Since we have to be added as a Game path (cannot add GameBin directly), we
	// automatically get added to other paths as well, including having the MM:S
	// dir become the default write path for logs and more. We can fix some of these.
	
	// GAMMACASE: This deals with the search paths where metamod could get added to, but there are still
	// problems with this as console.log file for example would be created way earlier than we alter these search paths,
	// and will be created in the metamod folder still, unsure what the solution to that could be tho!

	// NOTE: baseFs->PrintSearchPaths(); could be used to print out search paths to debug them.

	const char *pathIds[] = {
		"ADDONS",
		"CONTENT",
		"CONTENTADDONS",
		"CONTENTROOT",
		"EXECUTABLE_PATH",
		"GAME",
		"GAMEBIN",
		"GAMEROOT",
		"MOD",
		"PLATFORM",
		"SHADER_SOURCE",
		"SHADER_SOURCE_MOD",
		"SHADER_SOURCE_ROOT"
	};

	for(size_t id = 0; id < (sizeof(pathIds) / sizeof(pathIds[0])); id++)
	{
		CUtlVector<CUtlString> searchPaths;
		baseFs->GetSearchPathsForPathID(pathIds[id], (GetSearchPathTypes_t)0, searchPaths);

		FOR_EACH_VEC(searchPaths, i)
		{
			if(strstr(searchPaths[i].Get(), "metamod") != 0)
			{
				baseFs->RemoveSearchPath(searchPaths[i].Get(), pathIds[id]);
			}
		}
	}

	baseFs->RemoveSearchPaths("DEFAULT_WRITE_PATH");

	CBufferStringN<260> searchPath;
	baseFs->GetSearchPath("GAME", (GetSearchPathTypes_t)0, searchPath, 1);
	baseFs->AddSearchPath(searchPath.Get(), "DEFAULT_WRITE_PATH");

	g_pCVar = icvar;

	ConVar_Register(FCVAR_RELEASE);

	if (gameclients)
	{
		SH_ADD_HOOK(IServerGameClients, ClientCommand, gameclients, SH_MEMBER(this, &Source2Provider::Hook_ClientCommand), false);
	}

#ifdef SHOULD_OVERRIDE_ALLOWDEDICATED_SERVER
	SH_ADD_VPHOOK(ISource2ServerConfig, AllowDedicatedServers, serverconfig, SH_MEMBER(this, &Source2Provider::Hook_AllowDedicatedServers), false);
#endif

	SH_ADD_HOOK(IEngineServiceMgr, RegisterLoopMode, enginesvcmgr, SH_MEMBER(this, &Source2Provider::Hook_RegisterLoopMode), false);
	SH_ADD_HOOK(IEngineServiceMgr, UnregisterLoopMode, enginesvcmgr, SH_MEMBER(this, &Source2Provider::Hook_UnregisterLoopMode), false);
}

void Source2Provider::Notify_DLLShutdown_Pre()
{
	for(auto cvar : m_RegisteredConVars)
		delete cvar;

	ConVar_Unregister();

	SH_REMOVE_HOOK(IEngineServiceMgr, RegisterLoopMode, enginesvcmgr, SH_MEMBER(this, &Source2Provider::Hook_RegisterLoopMode), false);
	SH_REMOVE_HOOK(IEngineServiceMgr, UnregisterLoopMode, enginesvcmgr, SH_MEMBER(this, &Source2Provider::Hook_UnregisterLoopMode), false);

	if (gameclients)
	{
		SH_REMOVE_HOOK(IServerGameClients, ClientCommand, gameclients, SH_MEMBER(this, &Source2Provider::Hook_ClientCommand), false);
	}
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
#elif SOURCE_ENGINE == SE_DEADLOCK
	return SOURCE_ENGINE_DEADLOCK;
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
#elif SOURCE_ENGINE == SE_DEADLOCK
	return "Deadlock (2024)";
#else
#error "SOURCE_ENGINE not defined to a known value"
#endif
}

void Source2Provider::GetGamePath(char* pszBuffer, int len)
{
	CBufferStringN<MAX_PATH> buf;
	engine->GetGameDir(buf);
	ke::SafeSprintf(pszBuffer, len, "%s", buf.Get());
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

const char* Source2Provider::GetCommandLineValue(const char* key, const char* defval)
{
	return CommandLine()->ParmValue(key, defval);
}

void Source2Provider::ConsolePrint(const char* str)
{
	ConMsg("%s", str);
}

void Source2Provider::ClientConsolePrint(MMSPlayer_t client, const char* message)
{
	engine->ClientPrintf(client, message);
}

void Source2Provider::ServerCommand(const char* cmd)
{
	engine->ServerCommand(cmd);
}

const char* Source2Provider::GetConVarString(MetamodSourceConVar *convar)
{
	if (convar == NULL)
	{
		return NULL;
	}

	auto &value = reinterpret_cast<CConVar<CUtlString> *>(convar)->Get();
	return !value.IsEmpty() ? value.Get() : nullptr;
}

void Source2Provider::SetConVarString(MetamodSourceConVar *convar, const char* str)
{
	reinterpret_cast<CConVar<CUtlString> *>(convar)->Set( str );
}

bool Source2Provider::IsConCommandBaseACommand(ConCommandBase* pCommand)
{
	// This method shouldn't ever be called on s2 titles
	return false;
}

bool Source2Provider::RegisterConCommand(ProviderConCommand* pCommand)
{
	// Registration is handled by the plugins
	
	// If command has FCVAR_DEVELOPMENTONLY and is registered through our system
	// assume that it's previously plugin registered command that was hidden (on plugin reloads for example),
	// so remove that flag to make it available again
	if(pCommand->IsFlagSet( FCVAR_DEVELOPMENTONLY ))
		pCommand->RemoveFlags( FCVAR_DEVELOPMENTONLY );

	return true;
}

bool Source2Provider::RegisterConVar(ProviderConVar *pVar)
{
	// Registration is handled by the plugins
	return true;
}

void Source2Provider::UnregisterConCommand(ProviderConCommand *pCommand)
{
	icvar->UnregisterConCommandCallbacks(*pCommand);

	// Hide command from the search, as it would still be available otherwise
	pCommand->AddFlags( FCVAR_DEVELOPMENTONLY );
}

void Source2Provider::UnregisterConVar(ProviderConVar *pVar)
{
	icvar->UnregisterConVarCallbacks(*pVar);

	// Invalidate cvar for future use
	// This makes it hidden from the search and lets future convar registrations of that 
	// name to take over its setup
	pVar->GetConVarData()->Invalidate();
}

MetamodSourceConVar* Source2Provider::CreateConVar(const char* name,
	const char* defval,
	const char* help,
	int flags)
{
	uint64 newflags = 0ll;
	if (flags & ConVarFlag_Notify)
	{
		newflags |= FCVAR_NOTIFY;
	}
	if (flags & ConVarFlag_SpOnly)
	{
		newflags |= FCVAR_SPONLY;
	}

	CConVar<CUtlString> *pVar = new CConVar<CUtlString>( name, newflags, help, defval );
	
	m_RegisteredConVars.push_back( pVar );

	return reinterpret_cast<MetamodSourceConVar *>(pVar);
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

void LocalCommand_Meta(const CCommandContext &, const CCommand& args)
{
	if (nullptr != g_Source2Provider.m_pCallbacks)
	{
		GlobCommand cmd(&args);
		g_Source2Provider.m_pCallbacks->OnCommand_Meta(&cmd);
	}
}

void Source2Provider::Hook_RegisterLoopMode(const char *pszLoopModeName, ILoopModeFactory *pLoopModeFactory, void **ppGlobalPointer)
{
	if (!strcmp(pszLoopModeName, "game"))
	{
		SH_ADD_HOOK(ILoopModeFactory, CreateLoopMode, pLoopModeFactory, SH_MEMBER(this, &Source2Provider::Hook_CreateLoopModePost), true);
		SH_ADD_HOOK(ILoopModeFactory, DestroyLoopMode, pLoopModeFactory, SH_MEMBER(this, &Source2Provider::Hook_DestroyLoopMode), false);

		if (nullptr != m_pCallbacks)
		{
			m_pCallbacks->OnGameInit();
		}
	}
}

void Source2Provider::Hook_UnregisterLoopMode(const char* pszLoopModeName, ILoopModeFactory* pLoopModeFactory, void** ppGlobalPointer)
{
	if (!strcmp(pszLoopModeName, "game"))
	{
		SH_REMOVE_HOOK(ILoopModeFactory, CreateLoopMode, pLoopModeFactory, SH_MEMBER(this, &Source2Provider::Hook_CreateLoopModePost), true);
		SH_REMOVE_HOOK(ILoopModeFactory, DestroyLoopMode, pLoopModeFactory, SH_MEMBER(this, &Source2Provider::Hook_DestroyLoopMode), false);
	}

	RETURN_META(MRES_IGNORED);
}

ILoopMode *Source2Provider::Hook_CreateLoopModePost()
{
	ILoopMode *pLoopMode = META_RESULT_ORIG_RET(ILoopMode *);
	SH_ADD_HOOK(ILoopMode, LoopInit, pLoopMode, SH_MEMBER(this, &Source2Provider::Hook_LoopInitPost), true);
	SH_ADD_HOOK(ILoopMode, LoopShutdown, pLoopMode, SH_MEMBER(this, &Source2Provider::Hook_LoopShutdownPost), true);

	// Post-hook. Ignored
	return nullptr;
}

void Source2Provider::Hook_DestroyLoopMode(ILoopMode* pLoopMode)
{
	SH_REMOVE_HOOK(ILoopMode, LoopInit, pLoopMode, SH_MEMBER(this, &Source2Provider::Hook_LoopInitPost), true);
	SH_REMOVE_HOOK(ILoopMode, LoopShutdown, pLoopMode, SH_MEMBER(this, &Source2Provider::Hook_LoopShutdownPost), true);
}

bool Source2Provider::Hook_LoopInitPost(KeyValues* pKeyValues, ILoopModePrerequisiteRegistry *pRegistry)
{
	if (nullptr != m_pCallbacks)
	{
		m_pCallbacks->OnLevelInit(
			pKeyValues->GetString("levelname"),
			"",
			pKeyValues->GetString("previouslevel"),
			pKeyValues->GetString("landmarkname"),
			pKeyValues->GetBool("loadmap"),
			false
		);
	}
	
	// Post-hook. Ignored
	return true;
}

void Source2Provider::Hook_LoopShutdownPost()
{
	if (nullptr != m_pCallbacks)
	{
		m_pCallbacks->OnLevelShutdown();
	}
}

void Source2Provider::Hook_ClientCommand(CPlayerSlot nSlot, const CCommand& _cmd)
{
	GlobCommand cmd(&_cmd);

	if (strcmp(cmd.GetArg(0), "meta") == 0)
	{
		if (nullptr != m_pCallbacks)
		{
			m_pCallbacks->OnCommand_ClientMeta(nSlot, &cmd);
		}
		
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}
