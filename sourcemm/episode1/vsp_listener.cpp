#include "vsp_listener.h"
#include "svn_version.h"
#include "metamod.h"

using namespace SourceMM;

VSPListener::VSPListener()
{
	m_bLoaded = false;
	m_bLoadable = false;
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
}

void VSPListener::SetLoadable(bool set)
{
	m_bLoadable = set;
}

bool VSPListener::Load(CreateInterfaceFn interfaceFactory, CreateInterfaceFn gameServerFactory)
{
	if (!m_bLoadable)
	{
		Warning("Do not manually load Metamod:Source as a Valve Server Plugin\n");
		return false;
	}

	if (m_bLoaded)
	{
		return false;
	}

	m_bLoaded = true;
	SetLoadable(false);

	g_Metamod.NotifyVSPListening(this);

	return true;
}

