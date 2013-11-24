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

#include <assert.h>
#include "sourcemm.h"
#include "concommands.h"
#include "util.h"
#include <loader_bridge.h>
#include <sh_string.h>
#include <versionlib.h>

SH_DECL_HOOK0_void(ConCommand, Dispatch, SH_NOATTRIB, false);

ConCommand *g_plugin_unload = NULL;
bool g_bIsTryingToUnload;
bool g_bIsBridgedAsVsp;
SourceHook::String vsp_desc("Metamod:Source");

void InterceptPluginUnloads()
{
	g_bIsTryingToUnload = true;
}

void InterceptPluginUnloads_Post()
{
	g_bIsTryingToUnload = false;
}

class VspBridge : public IVspBridge
{
	virtual bool Load(const vsp_bridge_info *info, char *error, size_t maxlength)
	{
		assert(!g_bIsBridgedAsVsp);

		if (!g_GameDll.loaded)
		{
			vsp_desc.append(" ");
			vsp_desc.append(METAMOD_VERSION);

			CreateInterfaceFn engineFactory = (CreateInterfaceFn)info->engineFactory;
			CreateInterfaceFn gsFactory = (CreateInterfaceFn)info->gsFactory;

			if (!AlternatelyLoadMetamod(engineFactory, gsFactory))
				return false;

			extern ConVar metamod_version;
			char buffer[255];

			UTIL_Format(buffer, sizeof(buffer), "%sV", metamod_version.GetString());
			metamod_version.SetValue(buffer);
		}
		else
		{
			vsp_desc.append(" Interface ");
			vsp_desc.append(METAMOD_VERSION);
		}

		ConCommandBase *pBase = g_Engine.icvar->GetCommands();
		while (pBase != NULL)
		{
			if (pBase->IsCommand() && strcmp(pBase->GetName(), "plugin_unload") == 0)
			{
				g_plugin_unload = (ConCommand *)pBase;
				break;
			}
			pBase = const_cast<ConCommandBase *>(pBase->GetNext());
		}

		if (g_plugin_unload != NULL)
		{
			SH_ADD_HOOK_STATICFUNC(ConCommand, Dispatch, g_plugin_unload, InterceptPluginUnloads, false);
			SH_ADD_HOOK_STATICFUNC(ConCommand, Dispatch, g_plugin_unload, InterceptPluginUnloads_Post, true);
		}

		g_bIsBridgedAsVsp = true;
		g_pRealVspCallbacks = info->vsp_callbacks;
		g_vsp_version = info->vsp_version;

		g_PluginMngr.SetVSPAsLoaded();

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
		if (!g_GameDll.loaded)
		{
			g_SMConVarAccessor.UnloadMetamodCommands();
			UnloadMetamod(false);
		}
	}

	virtual const char *GetDescription()
	{
		return vsp_desc.c_str();
	}
};

VspBridge mm14_vsp_bridge;

SMM_API IVspBridge *
GetVspBridge()
{
	return &mm14_vsp_bridge;
}
