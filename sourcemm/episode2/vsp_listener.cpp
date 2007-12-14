/**
 * vim: set ts=4 :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2007 AlliedModders LLC and authors.
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

#if defined _DEBUG
#define DEBUG2
#undef _DEBUG
#endif
#include "../metamod_oslink.h"
#include <sourcehook.h>
#include <convar.h>
#include <eiface.h>
#include "iplayerinfo.h"
#if defined DEBUG2
#undef DEBUG2
#define _DEBUG
#endif
#include "vsp_listener.h"
#include "svn_version.h"
#include "metamod.h"
#include "provider_ep2.h"

SH_DECL_HOOK1_void(ConCommand, Dispatch, SH_NOATTRIB, false, const CCommand &);

using namespace SourceMM;

ConCommand *g_plugin_unload = NULL;
bool g_bIsTryingToUnload;

void InterceptPluginUnloads(const CCommand &args)
{
	g_bIsTryingToUnload = true;
}

void InterceptPluginUnloads_Post(const CCommand &args)
{
	g_bIsTryingToUnload = false;
}

VSPListener::VSPListener()
{
	m_bLoaded = false;
	m_bLoadable = false;
	m_bIsRootLoadMethod = false;
}

void VSPListener::ClientActive(edict_t *pEntity)
{
}

PLUGIN_RESULT VSPListener::ClientCommand(edict_t *pEntity, const CCommand &cmd)
{
	return PLUGIN_CONTINUE;
}

PLUGIN_RESULT VSPListener::ClientConnect(bool *bAllowConnect, edict_t *pEntity, const char *pszName, const char *pszAddress, char *reject, int maxrejectlen)
{
	return PLUGIN_CONTINUE;
}

void VSPListener::ClientDisconnect(edict_t *pEntity)
{
}

void VSPListener::ClientPutInServer(edict_t *pEntity, char const *playername)
{
}

void VSPListener::ClientSettingsChanged(edict_t *pEdict)
{
}

void VSPListener::SetCommandClient(int index)
{
}

void VSPListener::GameFrame(bool simulating)
{
}

const char *VSPListener::GetPluginDescription()
{
	return "Metamod:Source Interface v" SVN_FILE_VERSION_STRING;
}

bool VSPListener::IsLoaded()
{
	return m_bLoaded;
}

void VSPListener::LevelInit(char const *pMapName)
{
}

void VSPListener::LevelShutdown()
{
}

PLUGIN_RESULT VSPListener::NetworkIDValidated(const char *pszUserName, const char *pszNetworkID)
{
	return PLUGIN_CONTINUE;
}

void VSPListener::Pause()
{
}

void VSPListener::UnPause()
{
}

void VSPListener::ServerActivate(edict_t *pEdictList, int edictCount, int clientMax)
{
}

void VSPListener::Unload()
{
	if (g_bIsTryingToUnload)
	{
		Error("Metamod:Source cannot be unloaded from VSP mode.  Use \"meta unload\" to unload specific plugins.\n");
		return;
	}
	if (IsRootLoadMethod())
	{
		if (g_plugin_unload != NULL)
		{
			SH_REMOVE_HOOK_STATICFUNC(ConCommand, Dispatch, g_plugin_unload, InterceptPluginUnloads, false);
			SH_REMOVE_HOOK_STATICFUNC(ConCommand, Dispatch, g_plugin_unload, InterceptPluginUnloads_Post, true);
			g_plugin_unload = NULL;
		}
		UnloadMetamod();
	}
	m_bLoaded = false;
	m_bLoadable = true;
	m_bIsRootLoadMethod = false;
}

void VSPListener::SetLoadable(bool set)
{
	m_bLoadable = set;
}

bool VSPListener::Load(CreateInterfaceFn interfaceFactory, CreateInterfaceFn gameServerFactory)
{
	if (!g_Metamod.IsLoadedAsGameDLL())
	{
		CGlobalVars *pGlobals;
		IPlayerInfoManager *playerInfoManager;

		playerInfoManager = (IPlayerInfoManager *)gameServerFactory("PlayerInfoManager002", NULL);
		if (playerInfoManager == NULL)
		{
			Msg("Metamod:Source requires gameinfo.txt modification to load on this game.\n");
			return false;
		}

		pGlobals = playerInfoManager->GetGlobalVars();

		char gamedll_iface[] = "ServerGameDLL000";
		for (unsigned int i = 5; i <= 50; i++)
		{
			gamedll_iface[15] = '0' + i;
			if ((server = (IServerGameDLL *)gameServerFactory(gamedll_iface, NULL)) != NULL)
			{
				g_Metamod.SetGameDLLInfo(gameServerFactory, i);
				break;
			}
		}

		if (server == NULL)
		{
			Msg("Metamod:Source could not load (GameDLL version not compatible).\n");
			return false;
		}

		char gameclients_iface[] = "ServerGameClients000";
		for (unsigned int i = 3; i <= 4; i++)
		{
			gameclients_iface[19] = '0' + i;
			if ((gameclients = (IServerGameClients *)gameServerFactory(gameclients_iface, NULL)) == NULL)
			{
				break;
			}
		}

		if (!DetectGameInformation())
		{
			Msg("Metamod:Source failed to detect game paths; cannot load.\n");
			return false;
		}

		m_bIsRootLoadMethod = true;
		m_bLoaded = true;
		SetLoadable(false);

		InitializeForLoad();
		InitializeGlobals(interfaceFactory, interfaceFactory, interfaceFactory, pGlobals);
		
		const ConCommandBase *pBase = icvar->GetCommands();
		while (pBase != NULL)
		{
			if (pBase->IsCommand() && strcmp(pBase->GetName(), "plugin_unload") == 0)
			{
				g_plugin_unload = (ConCommand *)pBase;
				break;
			}
			pBase = pBase->GetNext();
		}

		if (g_plugin_unload != NULL)
		{
			SH_ADD_HOOK_STATICFUNC(ConCommand, Dispatch, g_plugin_unload, InterceptPluginUnloads, false);
			SH_ADD_HOOK_STATICFUNC(ConCommand, Dispatch, g_plugin_unload, InterceptPluginUnloads_Post, true);
		}
	}

	m_bLoaded = true;
	SetLoadable(false);

	g_Metamod.NotifyVSPListening(this);

	return true;
}

void VSPListener::OnQueryCvarValueFinished(QueryCvarCookie_t iCookie, edict_t *pPlayerEntity, EQueryCvarValueStatus eStatus, const char *pCvarName, const char *pCvarValue )
{
}

bool VSPListener::IsRootLoadMethod()
{
	return m_bIsRootLoadMethod;
}
