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

#include <stdio.h>
#include "../metamod_oslink.h"
#include "metamod.h"
#include <sourcehook.h>
#include <convar.h>
#include <eiface.h>
#include "iplayerinfo.h"
#include "vsp_listener.h"
#include "version.h"
#include "provider_ep2.h"

using namespace SourceMM;

VSPListener::VSPListener()
{
	m_bLoaded = false;
	m_bLoadable = false;
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
	return "Metamod:Source Interface " SVN_FULL_VERSION;
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
	#if 0
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
	#endif
	m_bLoaded = false;
	m_bLoadable = true;
}

void VSPListener::SetLoadable(bool set)
{
	m_bLoadable = set;
}

bool VSPListener::Load(CreateInterfaceFn interfaceFactory, CreateInterfaceFn gameServerFactory)
{
	m_bLoaded = true;
	SetLoadable(false);

	g_Metamod.NotifyVSPListening(this);

	return true;
}

void VSPListener::OnQueryCvarValueFinished(QueryCvarCookie_t iCookie, edict_t *pPlayerEntity, EQueryCvarValueStatus eStatus, const char *pCvarName, const char *pCvarValue )
{
}

