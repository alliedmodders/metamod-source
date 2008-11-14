/* ======== SourceMM ========
 * Copyright (C) 2004-2008 Metamod:Source Development Team
 * No warranties of any kind
 *
 * License: zlib/libpng
 *
 * Author(s): David "BAILOPAN" Anderson
 * ============================
 */

#include "vsp_listener.h"
#include "CPlugin.h"
#include "concommands.h"

SH_DECL_HOOK0_void(ConCommand, Dispatch, SH_NOATTRIB, false);

using namespace SourceMM;

VSPListener g_VspListener;
ConCommand *g_plugin_unload = NULL;
bool g_bIsTryingToUnload;

void InterceptPluginUnloads()
{
	g_bIsTryingToUnload = true;
}

void InterceptPluginUnloads_Post()
{
	g_bIsTryingToUnload = false;
}

VSPListener::VSPListener()
{
	m_Loaded = false;
	m_Loadable = false;
	m_bIsRootLoadMethod = false;
}

void VSPListener::ClientActive(edict_t *pEntity)
{
}

PLUGIN_RESULT VSPListener::ClientCommand(edict_t *pEntity)
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
	return "Metamod:Source Interface v" SOURCEMM_VERSION;
}

bool VSPListener::IsLoaded()
{
	return m_Loaded;
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
		g_SMConVarAccessor.UnloadMetamodCommands();
		UnloadMetamod(false);
	}
	m_Loadable = true;
	m_Loaded = false;
	m_bIsRootLoadMethod = false;
}

void VSPListener::SetLoadable(bool set)
{
	m_Loadable = set;
}

bool VSPListener::IsRootLoadMethod()
{
	return m_bIsRootLoadMethod;
}

bool VSPListener::Load(CreateInterfaceFn interfaceFactory, CreateInterfaceFn gameServerFactory)
{
	if (m_Loaded)
	{
		return false;
	}

	if (!m_Loadable && !g_GameDll.loaded)
	{
		/* New loading mechanism, do a bunch o' stuff! */
		m_bIsRootLoadMethod = true;
		m_Loaded = true;
		SetLoadable(false);
		if (!AlternatelyLoadMetamod(interfaceFactory, gameServerFactory))
		{
			return false;
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

		/* Ho ho ho... if we get here, set a new cvar version. */
		extern ConVar metamod_version;
		char buffer[255];

		UTIL_Format(buffer, sizeof(buffer), "%sV", metamod_version.GetString());
		metamod_version.SetValue(buffer);
	}

	m_Loaded = true;
	SetLoadable(false);

	if (!m_bIsRootLoadMethod)
	{
		g_PluginMngr.SetVSPAsLoaded();
	}

	return true;
}
