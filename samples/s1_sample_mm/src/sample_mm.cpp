/**
 * vim: set ts=4 sw=4 tw=99 noet :
 * ======================================================
 * Metamod:Source Sample Plugin
 * Written by AlliedModders LLC.
 * ======================================================
 *
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the authors be held liable for any damages arising from 
 * the use of this software.
 *
 * This sample plugin is public domain.
 */

#include <stdio.h>
#include "sample_mm.h"

SamplePlugin g_SamplePlugin;
IServerGameDLL *server = NULL;
IServerGameClients *gameclients = NULL;
IVEngineServer *engine = NULL;
IServerPluginHelpers *helpers = NULL;
IGameEventManager2 *gameevents = NULL;
IServerPluginCallbacks *vsp_callbacks = NULL;
IPlayerInfoManager *playerinfomanager = NULL;
ICvar *icvar = NULL;
CGlobalVars *gpGlobals = NULL;

ConVar sample_cvar("sample_cvar", "42", 0);

/** 
 * Something like this is needed to register cvars/CON_COMMANDs.
 */
class BaseAccessor : public IConCommandBaseAccessor
{
public:
	bool RegisterConCommandBase(ConCommandBase *pCommandBase)
	{
		/* Always call META_REGBASECMD instead of going through the engine. */
		return META_REGBASECMD(pCommandBase);
	}
} s_BaseAccessor;

SamplePlugin::SamplePlugin() :
	m_LevelInit(&IServerGameDLL::LevelInit, this, nullptr, &SamplePlugin::Hook_LevelInit),
	m_ServerActivate(&IServerGameDLL::ServerActivate, this, nullptr, &SamplePlugin::Hook_ServerActivate),
	m_GameFrame(&IServerGameDLL::GameFrame, this, nullptr, &SamplePlugin::Hook_GameFrame),
	m_LevelShutdown(&IServerGameDLL::LevelShutdown, this, &SamplePlugin::Hook_LevelShutdown, nullptr),
	m_ClientActive(&IServerGameClients::ClientActive, this, nullptr, &SamplePlugin::Hook_ClientActive),
	m_ClientDisconnect(&IServerGameClients::ClientDisconnect, this, nullptr, &SamplePlugin::Hook_ClientDisconnect),
	m_ClientPutInServer(&IServerGameClients::ClientPutInServer, this, nullptr, &SamplePlugin::Hook_ClientPutInServer),
	m_SetCommandClient(&IServerGameClients::SetCommandClient, this, nullptr, &SamplePlugin::Hook_SetCommandClient),
	m_ClientSettingsChanged(&IServerGameClients::ClientSettingsChanged, this, &SamplePlugin::Hook_ClientSettingsChanged, nullptr),
	m_ClientConnect(&IServerGameClients::ClientConnect, this, &SamplePlugin::Hook_ClientConnect, nullptr),
	m_ClientCommand(&IServerGameClients::ClientCommand, this, &SamplePlugin::Hook_ClientCommand, nullptr)
{
}

PLUGIN_EXPOSE(SamplePlugin, g_SamplePlugin);
bool SamplePlugin::Load(PluginId id, ISmmAPI *ismm, char *error, size_t maxlen, bool late)
{
	PLUGIN_SAVEVARS();

	GET_V_IFACE_CURRENT(GetEngineFactory, engine, IVEngineServer, INTERFACEVERSION_VENGINESERVER);
	GET_V_IFACE_CURRENT(GetEngineFactory, gameevents, IGameEventManager2, INTERFACEVERSION_GAMEEVENTSMANAGER2);
	GET_V_IFACE_CURRENT(GetEngineFactory, helpers, IServerPluginHelpers, INTERFACEVERSION_ISERVERPLUGINHELPERS);
	GET_V_IFACE_CURRENT(GetEngineFactory, icvar, ICvar, CVAR_INTERFACE_VERSION);
	GET_V_IFACE_ANY(GetServerFactory, server, IServerGameDLL, INTERFACEVERSION_SERVERGAMEDLL);
	GET_V_IFACE_ANY(GetServerFactory, gameclients, IServerGameClients, INTERFACEVERSION_SERVERGAMECLIENTS);
	GET_V_IFACE_ANY(GetServerFactory, playerinfomanager, IPlayerInfoManager, INTERFACEVERSION_PLAYERINFOMANAGER);

	gpGlobals = ismm->GetCGlobals();

	META_LOG(g_PLAPI, "Starting plugin.");

	/* Load the VSP listener.  This is usually needed for IServerPluginHelpers. */
	if ((vsp_callbacks = ismm->GetVSPInfo(NULL)) == NULL)
	{
		ismm->AddListener(this, this);
		ismm->EnableVSPListener();
	}

	META_LOG(g_PLAPI, "level init");
	m_LevelInit.Add(server);
	META_LOG(g_PLAPI, "server activate");
	m_ServerActivate.Add(server);
	META_LOG(g_PLAPI, "game frame");
	m_GameFrame.Add(server);
	META_LOG(g_PLAPI, "lvl shutdown");
	m_LevelShutdown.Add(server);
	META_LOG(g_PLAPI, "client active");
	m_ClientActive.Add(gameclients);
	META_LOG(g_PLAPI, "client disconnect");
	m_ClientDisconnect.Add(gameclients);
	META_LOG(g_PLAPI, "put in server");
	m_ClientPutInServer.Add(gameclients);
	META_LOG(g_PLAPI, "set cmd");
	m_SetCommandClient.Add(gameclients);
	META_LOG(g_PLAPI, "settings changed");
	m_ClientSettingsChanged.Add(gameclients);
	META_LOG(g_PLAPI, "client connect");
	m_ClientConnect.Add(gameclients);
	META_LOG(g_PLAPI, "client cmd");
	m_ClientCommand.Add(gameclients);

	META_LOG(g_PLAPI, "All hooks started!");

#if SOURCE_ENGINE >= SE_ORANGEBOX
	g_pCVar = icvar;
	ConVar_Register(0, &s_BaseAccessor);
#else
	ConCommandBaseMgr::OneTimeInit(&s_BaseAccessor);
#endif

	return true;
}

bool SamplePlugin::Unload(char *error, size_t maxlen)
{
	m_LevelInit.Remove(server);
	m_ServerActivate.Remove(server);
	m_GameFrame.Remove(server);
	m_LevelShutdown.Remove(server);
	m_ClientActive.Remove(gameclients);
	m_ClientDisconnect.Remove(gameclients);
	m_ClientPutInServer.Remove(gameclients);
	m_SetCommandClient.Remove(gameclients);
	m_ClientSettingsChanged.Remove(gameclients);
	m_ClientConnect.Remove(gameclients);
	m_ClientCommand.Remove(gameclients);

	return true;
}

void SamplePlugin::OnVSPListening(IServerPluginCallbacks *iface)
{
	vsp_callbacks = iface;
}

KHook::Return<void> SamplePlugin::Hook_ServerActivate(IServerGameDLL*, edict_t *pEdictList, int edictCount, int clientMax)
{
	META_LOG(g_PLAPI, "ServerActivate() called: edictCount = %d, clientMax = %d", edictCount, clientMax);

	return { KHook::Action::Ignore };
}

void SamplePlugin::AllPluginsLoaded()
{
	/* This is where we'd do stuff that relies on the mod or other plugins 
	 * being initialized (for example, cvars added and events registered).
	 */
}

KHook::Return<void> SamplePlugin::Hook_ClientActive(IServerGameClients*, edict_t *pEntity, bool bLoadGame)
{
	META_LOG(g_PLAPI, "Hook_ClientActive(%d, %d)", IndexOfEdict(pEntity), bLoadGame);

	return { KHook::Action::Ignore };
}

#if SOURCE_ENGINE >= SE_ORANGEBOX
KHook::Return<void> SamplePlugin::Hook_ClientCommand(IServerGameClients*, edict_t *pEntity, const CCommand &args)
#else
KHook::Return<void> SamplePlugin::Hook_ClientCommand(IServerGameClients*, edict_t *pEntity)
#endif
{
#if SOURCE_ENGINE <= SE_DARKMESSIAH
	CCommand args;
#endif

	if (!pEntity || pEntity->IsFree())
	{
		return { KHook::Action::Ignore };
	}

	const char *cmd = args.Arg(0);
	if (strcmp(cmd, "menu") == 0)
	{
		KeyValues *kv = new KeyValues("menu");
		kv->SetString("title", "You've got options, hit ESC");
		kv->SetInt("level", 1);
		kv->SetColor("color", Color(255, 0, 0, 255));
		kv->SetInt("time", 20);
		kv->SetString("msg", "Pick an option\nOr don't.");

		for (int i = 1; i < 9; i++)
		{
			char num[10], msg[10], cmd[10];
			MM_Format( num, sizeof(num), "%i", i );
			MM_Format( msg, sizeof(msg), "Option %i", i );
			MM_Format( cmd, sizeof(cmd), "option %i", i );

			KeyValues *item1 = kv->FindKey(num, true);
			item1->SetString("msg", msg);
			item1->SetString("command", cmd);
		}

		helpers->CreateMessage(pEntity, DIALOG_MENU, kv, vsp_callbacks);
		kv->deleteThis();
		return { KHook::Action::Supercede };
	}
	else if (strcmp(cmd, "rich") == 0)
	{
		KeyValues *kv = new KeyValues("menu");
		kv->SetString("title", "A rich message");
		kv->SetInt("level", 1);
		kv->SetInt("time", 20);
		kv->SetString("msg", "This is a long long long text string.\n\nIt also has line breaks.");

		helpers->CreateMessage(pEntity, DIALOG_TEXT, kv, vsp_callbacks);
		kv->deleteThis();
		return { KHook::Action::Supercede };
	}
	else if (strcmp(cmd, "msg") == 0)
	{
		KeyValues *kv = new KeyValues("menu");
		kv->SetString("title", "Just a simple hello");
		kv->SetInt("level", 1);
		kv->SetInt("time", 20);

		helpers->CreateMessage(pEntity, DIALOG_MSG, kv, vsp_callbacks);
		kv->deleteThis();
		return { KHook::Action::Supercede };
	}
	else if (strcmp(cmd, "entry") == 0)
	{
		KeyValues *kv = new KeyValues("entry");
		kv->SetString("title", "Stuff");
		kv->SetString("msg", "Enter something");
		kv->SetString("command", "say"); // anything they enter into the dialog turns into a say command
		kv->SetInt("level", 1);
		kv->SetInt("time", 20);

		helpers->CreateMessage(pEntity, DIALOG_ENTRY, kv, vsp_callbacks);
		kv->deleteThis();
		return { KHook::Action::Supercede };
	}
	return { KHook::Action::Ignore };
}

KHook::Return<void> SamplePlugin::Hook_ClientSettingsChanged(IServerGameClients*, edict_t *pEdict)
{
	if (playerinfomanager)
	{
		IPlayerInfo *playerinfo = playerinfomanager->GetPlayerInfo(pEdict);

		const char *name = engine->GetClientConVarValue(IndexOfEdict(pEdict), "name");

		if (playerinfo != NULL
			&& name != NULL
			&& strcmp(engine->GetPlayerNetworkIDString(pEdict), "BOT") != 0
			&& playerinfo->GetName() != NULL
			&& strcmp(name, playerinfo->GetName()) == 0)
		{
			char msg[128];
			MM_Format(msg, sizeof(msg), "Your name changed to \"%s\" (from \"%s\")\n", name, playerinfo->GetName());
			engine->ClientPrintf(pEdict, msg);
		}
	}
	return { KHook::Action::Ignore };
}

KHook::Return<bool> SamplePlugin::Hook_ClientConnect(IServerGameClients*, edict_t *pEntity,
									const char *pszName,
									const char *pszAddress,
									char *reject,
									int maxrejectlen)
{
	META_LOG(g_PLAPI, "Hook_ClientConnect(%d, \"%s\", \"%s\")", IndexOfEdict(pEntity), pszName, pszAddress);

	return { KHook::Action::Ignore };
}

KHook::Return<void> SamplePlugin::Hook_ClientPutInServer(IServerGameClients*, edict_t *pEntity, char const *playername)
{
	KeyValues *kv = new KeyValues( "msg" );
	kv->SetString( "title", "Hello" );
	kv->SetString( "msg", "Hello there" );
	kv->SetColor( "color", Color( 255, 0, 0, 255 ));
	kv->SetInt( "level", 5);
	kv->SetInt( "time", 10);
	helpers->CreateMessage(pEntity, DIALOG_MSG, kv, vsp_callbacks);
	kv->deleteThis();

	return { KHook::Action::Ignore };
}

KHook::Return<void> SamplePlugin::Hook_ClientDisconnect(IServerGameClients*, edict_t *pEntity)
{
	META_LOG(g_PLAPI, "Hook_ClientDisconnect(%d)", IndexOfEdict(pEntity));
	return { KHook::Action::Ignore };
}

KHook::Return<void> SamplePlugin::Hook_GameFrame(IServerGameDLL*, bool simulating)
{
	/**
	 * simulating:
	 * ***********
	 * true  | game is ticking
	 * false | game is not ticking
	 */
	return { KHook::Action::Ignore };
}

KHook::Return<bool> SamplePlugin::Hook_LevelInit(IServerGameDLL*, const char *pMapName,
								char const *pMapEntities,
								char const *pOldLevel,
								char const *pLandmarkName,
								bool loadGame,
								bool background)
{
	META_LOG(g_PLAPI, "Hook_LevelInit(%s)", pMapName);

	return { KHook::Action::Ignore };
}

KHook::Return<void> SamplePlugin::Hook_LevelShutdown(IServerGameDLL*)
{
	META_LOG(g_PLAPI, "Hook_LevelShutdown()");

	return { KHook::Action::Ignore };
}

KHook::Return<void> SamplePlugin::Hook_SetCommandClient(IServerGameClients*, int index)
{
	META_LOG(g_PLAPI, "Hook_SetCommandClient(%d)", index);

	return { KHook::Action::Ignore };
}

bool SamplePlugin::Pause(char *error, size_t maxlen)
{
	return true;
}

bool SamplePlugin::Unpause(char *error, size_t maxlen)
{
	return true;
}

const char *SamplePlugin::GetLicense()
{
	return "Public Domain";
}

const char *SamplePlugin::GetVersion()
{
	return "1.0.0.0";
}

const char *SamplePlugin::GetDate()
{
	return __DATE__;
}

const char *SamplePlugin::GetLogTag()
{
	return "SAMPLE";
}

const char *SamplePlugin::GetAuthor()
{
	return "AlliedModders LLC";
}

const char *SamplePlugin::GetDescription()
{
	return "Sample basic plugin";
}

const char *SamplePlugin::GetName()
{
	return "Sample Plugin";
}

const char *SamplePlugin::GetURL()
{
	return "http://www.sourcemm.net/";
}
