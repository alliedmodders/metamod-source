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

SH_DECL_HOOK6(IServerGameDLL, LevelInit, SH_NOATTRIB, 0, bool, char const *, char const *, char const *, char const *, bool, bool);
SH_DECL_HOOK3_void(IServerGameDLL, ServerActivate, SH_NOATTRIB, 0, edict_t *, int, int);
SH_DECL_HOOK1_void(IServerGameDLL, GameFrame, SH_NOATTRIB, 0, bool);
SH_DECL_HOOK0_void(IServerGameDLL, LevelShutdown, SH_NOATTRIB, 0);
SH_DECL_HOOK2_void(IServerGameClients, ClientActive, SH_NOATTRIB, 0, edict_t *, bool);
SH_DECL_HOOK1_void(IServerGameClients, ClientDisconnect, SH_NOATTRIB, 0, edict_t *);
SH_DECL_HOOK2_void(IServerGameClients, ClientPutInServer, SH_NOATTRIB, 0, edict_t *, char const *);
SH_DECL_HOOK1_void(IServerGameClients, SetCommandClient, SH_NOATTRIB, 0, int);
SH_DECL_HOOK1_void(IServerGameClients, ClientSettingsChanged, SH_NOATTRIB, 0, edict_t *);
SH_DECL_HOOK5(IServerGameClients, ClientConnect, SH_NOATTRIB, 0, bool, edict_t *, const char*, const char *, char *, int);
SH_DECL_HOOK2(IGameEventManager2, FireEvent, SH_NOATTRIB, 0, bool, IGameEvent *, bool);

#if SOURCE_ENGINE >= SE_ORANGEBOX
SH_DECL_HOOK2_void(IServerGameClients, NetworkIDValidated, SH_NOATTRIB, 0, const char *, const char *);
SH_DECL_HOOK2_void(IServerGameClients, ClientCommand, SH_NOATTRIB, 0, edict_t *, const CCommand &);
#else
SH_DECL_HOOK1_void(IServerGameClients, ClientCommand, SH_NOATTRIB, 0, edict_t *);
#endif

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
		/* Always call META_REGCVAR instead of going through the engine. */
		return META_REGCVAR(pCommandBase);
	}
} s_BaseAccessor;

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

	SH_ADD_HOOK_MEMFUNC(IServerGameDLL, LevelInit, server, this, &SamplePlugin::Hook_LevelInit, true);
	SH_ADD_HOOK_MEMFUNC(IServerGameDLL, ServerActivate, server, this, &SamplePlugin::Hook_ServerActivate, true);
	SH_ADD_HOOK_MEMFUNC(IServerGameDLL, GameFrame, server, this, &SamplePlugin::Hook_GameFrame, true);
	SH_ADD_HOOK_MEMFUNC(IServerGameDLL, LevelShutdown, server, this, &SamplePlugin::Hook_LevelShutdown, false);
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientActive, gameclients, this, &SamplePlugin::Hook_ClientActive, true);
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientDisconnect, gameclients, this, &SamplePlugin::Hook_ClientDisconnect, true);
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientPutInServer, gameclients, this, &SamplePlugin::Hook_ClientPutInServer, true);
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, SetCommandClient, gameclients, this, &SamplePlugin::Hook_SetCommandClient, true);
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientSettingsChanged, gameclients, this, &SamplePlugin::Hook_ClientSettingsChanged, false);
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientConnect, gameclients, this, &SamplePlugin::Hook_ClientConnect, false);
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientCommand, gameclients, this, &SamplePlugin::Hook_ClientCommand, false);

	ENGINE_CALL(LogPrint)("All hooks started!\n");

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
	SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, LevelInit, server, this, &SamplePlugin::Hook_LevelInit, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, ServerActivate, server, this, &SamplePlugin::Hook_ServerActivate, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, GameFrame, server, this, &SamplePlugin::Hook_GameFrame, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, LevelShutdown, server, this, &SamplePlugin::Hook_LevelShutdown, false);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientActive, gameclients, this, &SamplePlugin::Hook_ClientActive, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientDisconnect, gameclients, this, &SamplePlugin::Hook_ClientDisconnect, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientPutInServer, gameclients, this, &SamplePlugin::Hook_ClientPutInServer, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, SetCommandClient, gameclients, this, &SamplePlugin::Hook_SetCommandClient, true);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientSettingsChanged, gameclients, this, &SamplePlugin::Hook_ClientSettingsChanged, false);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientConnect, gameclients, this, &SamplePlugin::Hook_ClientConnect, false);
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientCommand, gameclients, this, &SamplePlugin::Hook_ClientCommand, false);

	return true;
}

void SamplePlugin::OnVSPListening(IServerPluginCallbacks *iface)
{
	vsp_callbacks = iface;
}

void SamplePlugin::Hook_ServerActivate(edict_t *pEdictList, int edictCount, int clientMax)
{
	META_LOG(g_PLAPI, "ServerActivate() called: edictCount = %d, clientMax = %d", edictCount, clientMax);
}

void SamplePlugin::AllPluginsLoaded()
{
	/* This is where we'd do stuff that relies on the mod or other plugins 
	 * being initialized (for example, cvars added and events registered).
	 */
}

void SamplePlugin::Hook_ClientActive(edict_t *pEntity, bool bLoadGame)
{
	META_LOG(g_PLAPI, "Hook_ClientActive(%d, %d)", IndexOfEdict(pEntity), bLoadGame);
}

#if SOURCE_ENGINE >= SE_ORANGEBOX
void SamplePlugin::Hook_ClientCommand(edict_t *pEntity, const CCommand &args)
#else
void SamplePlugin::Hook_ClientCommand(edict_t *pEntity)
#endif
{
#if SOURCE_ENGINE <= SE_DARKMESSIAH
	CCommand args;
#endif

	if (!pEntity || pEntity->IsFree())
	{
		return;
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
		RETURN_META(MRES_SUPERCEDE);
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
		RETURN_META(MRES_SUPERCEDE);
	}
	else if (strcmp(cmd, "msg") == 0)
	{
		KeyValues *kv = new KeyValues("menu");
		kv->SetString("title", "Just a simple hello");
		kv->SetInt("level", 1);
		kv->SetInt("time", 20);

		helpers->CreateMessage(pEntity, DIALOG_MSG, kv, vsp_callbacks);
		kv->deleteThis();
		RETURN_META(MRES_SUPERCEDE);
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
		RETURN_META(MRES_SUPERCEDE);
	}
}

void SamplePlugin::Hook_ClientSettingsChanged(edict_t *pEdict)
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
}

bool SamplePlugin::Hook_ClientConnect(edict_t *pEntity,
									const char *pszName,
									const char *pszAddress,
									char *reject,
									int maxrejectlen)
{
	META_LOG(g_PLAPI, "Hook_ClientConnect(%d, \"%s\", \"%s\")", IndexOfEdict(pEntity), pszName, pszAddress);

	return true;
}

void SamplePlugin::Hook_ClientPutInServer(edict_t *pEntity, char const *playername)
{
	KeyValues *kv = new KeyValues( "msg" );
	kv->SetString( "title", "Hello" );
	kv->SetString( "msg", "Hello there" );
	kv->SetColor( "color", Color( 255, 0, 0, 255 ));
	kv->SetInt( "level", 5);
	kv->SetInt( "time", 10);
	helpers->CreateMessage(pEntity, DIALOG_MSG, kv, vsp_callbacks);
	kv->deleteThis();
}

void SamplePlugin::Hook_ClientDisconnect(edict_t *pEntity)
{
	META_LOG(g_PLAPI, "Hook_ClientDisconnect(%d)", IndexOfEdict(pEntity));
}

void SamplePlugin::Hook_GameFrame(bool simulating)
{
	/**
	 * simulating:
	 * ***********
	 * true  | game is ticking
	 * false | game is not ticking
	 */
}

bool SamplePlugin::Hook_LevelInit(const char *pMapName,
								char const *pMapEntities,
								char const *pOldLevel,
								char const *pLandmarkName,
								bool loadGame,
								bool background)
{
	META_LOG(g_PLAPI, "Hook_LevelInit(%s)", pMapName);

	return true;
}

void SamplePlugin::Hook_LevelShutdown()
{
	META_LOG(g_PLAPI, "Hook_LevelShutdown()");
}

void SamplePlugin::Hook_SetCommandClient(int index)
{
	META_LOG(g_PLAPI, "Hook_SetCommandClient(%d)", index);
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
