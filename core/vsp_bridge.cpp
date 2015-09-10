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

#include "metamod.h"
#include "metamod_util.h"
#include <interface.h>
#include <eiface.h>
#include <iplayerinfo.h>
#include <assert.h>
#include <loader_bridge.h>
#include <versionlib.h>
#include <sh_string.h>
#include "provider/provider_ep2.h"

#if SOURCE_ENGINE == SE_DOTA
SH_DECL_HOOK2_void(ConCommand, Dispatch, SH_NOATTRIB, false, const CCommandContext &, const CCommand &);
#elif SOURCE_ENGINE >= SE_ORANGEBOX
SH_DECL_HOOK1_void(ConCommand, Dispatch, SH_NOATTRIB, false, const CCommand &);
#else
SH_DECL_HOOK0_void(ConCommand, Dispatch, SH_NOATTRIB, false);
#endif

ConCommand *g_plugin_unload = NULL;
bool g_bIsTryingToUnload;
SourceHook::String vsp_desc("Metamod:Source");

#if SOURCE_ENGINE == SE_DOTA
void InterceptPluginUnloads(const CCommandContext &context, const CCommand &args)
#elif SOURCE_ENGINE >= SE_ORANGEBOX
void InterceptPluginUnloads(const CCommand &args)
#else
void InterceptPluginUnloads()
#endif
{
	g_bIsTryingToUnload = true;
}

#if SOURCE_ENGINE == SE_DOTA
void InterceptPluginUnloads_Post(const CCommandContext &context, const CCommand &args)
#elif SOURCE_ENGINE >= SE_ORANGEBOX
void InterceptPluginUnloads_Post(const CCommand &args)
#else
void InterceptPluginUnloads_Post()
#endif
{
	g_bIsTryingToUnload = false;
}

class VspBridge : public IVspBridge
{
public:
	virtual bool Load(const vsp_bridge_info *info, char *error, size_t maxlength)
	{
		if (!g_Metamod.IsLoadedAsGameDLL())
		{
			vsp_desc.append(" ");
			vsp_desc.append(METAMOD_VERSION);

			CGlobalVars *pGlobals;
			IPlayerInfoManager *playerInfoManager;

			playerInfoManager = (IPlayerInfoManager *)info->gsFactory("PlayerInfoManager002", NULL);
			if (playerInfoManager == NULL)
			{
				UTIL_Format(error, maxlength, "Metamod:Source requires gameinfo.txt modification to load on this game");
				return false;
			}

			pGlobals = playerInfoManager->GetGlobalVars();

			char gamedll_iface[24];
			for (unsigned int i = 3; i <= 50; i++)
			{
				UTIL_Format(gamedll_iface, sizeof(gamedll_iface), "ServerGameDLL%03d", i);
				if ((server = (IServerGameDLL *)info->gsFactory(gamedll_iface, NULL)) != NULL)
				{
					g_Metamod.SetGameDLLInfo((CreateInterfaceFn)info->gsFactory, i, false);
					break;
				}
			}

			if (server == NULL)
			{
				UTIL_Format(error, maxlength, "Metamod:Source could not load (GameDLL version not compatible).");
				return false;
			}

			char gameclients_iface[] = "ServerGameClients000";
			for (unsigned int i = 3; i <= 4; i++)
			{
				gameclients_iface[19] = '0' + i;
				if ((gameclients = (IServerGameClients *)info->gsFactory(gameclients_iface, NULL)) == NULL)
					break;
			}

			mm_InitializeGlobals((CreateInterfaceFn) info->engineFactory,
				(CreateInterfaceFn) info->engineFactory,
				(CreateInterfaceFn) info->engineFactory,
				pGlobals);

			if (!mm_DetectGameInformation())
			{
				UTIL_Format(error, maxlength, "Metamod:Source failed to detect game paths; cannot load.");
				return false;
			}

			mm_InitializeForLoad();			
			g_Metamod.NotifyVSPListening(info->vsp_callbacks, info->vsp_version);
			mm_StartupMetamod(true);
		}
		else
		{
			vsp_desc.append(" Interface ");
			vsp_desc.append(METAMOD_VERSION);

			g_Metamod.NotifyVSPListening(info->vsp_callbacks, info->vsp_version);
		}

#if SOURCE_ENGINE >= SE_ORANGEBOX
		g_plugin_unload = icvar->FindCommand("plugin_unload");
#else
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
#endif

		if (g_plugin_unload != NULL)
		{
			SH_ADD_HOOK_STATICFUNC(ConCommand, Dispatch, g_plugin_unload, InterceptPluginUnloads, false);
			SH_ADD_HOOK_STATICFUNC(ConCommand, Dispatch, g_plugin_unload, InterceptPluginUnloads_Post, true);
		}

		return true;
	}

	virtual void Unload()
	{
		if (g_bIsTryingToUnload)
		{
			Error("Metamod:Source cannot be unloaded from VSP mode.  Use \"meta unload\" to unload specific plugins.\n");
			return;
		}
		if (g_plugin_unload != NULL)
		{
			SH_REMOVE_HOOK_STATICFUNC(ConCommand, Dispatch, g_plugin_unload, InterceptPluginUnloads, false);
			SH_REMOVE_HOOK_STATICFUNC(ConCommand, Dispatch, g_plugin_unload, InterceptPluginUnloads_Post, true);
			g_plugin_unload = NULL;
		}
		if (!g_Metamod.IsLoadedAsGameDLL())
		{
			mm_UnloadMetamod();
		}
	}

	virtual const char *GetDescription()
	{
		return vsp_desc.c_str();
	}
};

VspBridge mm16_vsp_bridge;

SMM_API IVspBridge *
GetVspBridge()
{
	return &mm16_vsp_bridge;
}

