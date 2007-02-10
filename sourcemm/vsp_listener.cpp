/* ======== SourceMM ========
 * Copyright (C) 2004-2007 Metamod:Source Development Team
 * No warranties of any kind
 *
 * License: zlib/libpng
 *
 * Author(s): David "BAILOPAN" Anderson
 * ============================
 */

#include "vsp_listener.h"

VSPListener g_VspListener;

VSPListener::VSPListener()
{
	m_Loaded = false;
	m_Loadable = false;
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
}

void VSPListener::SetLoadable(bool set)
{
	m_Loadable = set;
}

bool VSPListener::Load(CreateInterfaceFn interfaceFactory, CreateInterfaceFn gameServerFactory)
{
	if (!g_GameDll.loaded)
	{
		Error("Metamod:Source is not a Valve Server Plugin\n");

		return false;
	}

	if (!m_Loadable)
	{
		Warning("Do not manually load Metamod:Source as a Valve Server Plugin\n");

		return false;
	}

	if (m_Loaded)
	{
		return false;
	}

	m_Loaded = true;

	return true;
}
