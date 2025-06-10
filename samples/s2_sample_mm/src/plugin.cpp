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
#include "plugin.h"
#include "iserver.h"

SH_DECL_HOOK3_void(IServerGameDLL, GameFrame, SH_NOATTRIB, 0, bool, bool, bool);
SH_DECL_HOOK4_void(IServerGameClients, ClientActive, SH_NOATTRIB, 0, CPlayerSlot, bool, const char *, uint64);
SH_DECL_HOOK5_void(IServerGameClients, ClientDisconnect, SH_NOATTRIB, 0, CPlayerSlot, ENetworkDisconnectionReason, const char *, uint64, const char *);
SH_DECL_HOOK4_void(IServerGameClients, ClientPutInServer, SH_NOATTRIB, 0, CPlayerSlot, char const *, int, uint64);
SH_DECL_HOOK1_void(IServerGameClients, ClientSettingsChanged, SH_NOATTRIB, 0, CPlayerSlot );
SH_DECL_HOOK6_void(IServerGameClients, OnClientConnected, SH_NOATTRIB, 0, CPlayerSlot, const char*, uint64, const char *, const char *, bool);
SH_DECL_HOOK6(IServerGameClients, ClientConnect, SH_NOATTRIB, 0, bool, CPlayerSlot, const char*, uint64, const char *, bool, CBufferString *);
SH_DECL_HOOK2(IGameEventManager2, FireEvent, SH_NOATTRIB, 0, bool, IGameEvent *, bool);

SH_DECL_HOOK2_void( IServerGameClients, ClientCommand, SH_NOATTRIB, 0, CPlayerSlot, const CCommand & );

MMSPlugin g_ThisPlugin;
IServerGameDLL *server = NULL;
IServerGameClients *gameclients = NULL;
IVEngineServer *engine = NULL;
IGameEventManager2 *gameevents = NULL;
ICvar *icvar = NULL;

// Should only be called within the active game loop (i e map should be loaded and active)
// otherwise that'll be nullptr!
CGlobalVars *GetGameGlobals()
{
	INetworkGameServer *server = g_pNetworkServerService->GetIGameServer();

	if(!server)
		return nullptr;

	return g_pNetworkServerService->GetIGameServer()->GetGlobals();
}

void SampleCvarFChangeCB( CConVar<float> *cvar, CSplitScreenSlot slot, const float *new_val, const float *old_val )
{
	META_CONPRINTF( "Sample convar \"%s\" was changed from %f to %f [%s]\n",
					cvar->GetName(), *old_val, *new_val,
					// When convar is first initialised with a default value, it would have FCVAR_INITIAL_SETVALUE
					// flag set, so you can check for it if needed.
					cvar->IsFlagSet( FCVAR_INITIAL_SETVALUE ) ? "initialised" : "change" );
}

CConVar<int> sample_cvari("sample_cvari", FCVAR_NONE, "help string", 42);
CConVar<float> sample_cvarf("sample_cvarf", FCVAR_NONE, "help string", 69.69f, true, 10.0f, true, 100.0f, SampleCvarFChangeCB );

CON_COMMAND_F(sample_command, "Sample command", FCVAR_NONE)
{
	META_CONPRINTF( "Sample command called by %d. Command: %s\n", context.GetPlayerSlot(), args.GetCommandString() );
}

PLUGIN_EXPOSE(MMSPlugin, g_ThisPlugin);
bool MMSPlugin::Load(PluginId id, ISmmAPI *ismm, char *error, size_t maxlen, bool late)
{
	PLUGIN_SAVEVARS();

	GET_V_IFACE_CURRENT(GetEngineFactory, engine, IVEngineServer, INTERFACEVERSION_VENGINESERVER);
	GET_V_IFACE_CURRENT(GetEngineFactory, icvar, ICvar, CVAR_INTERFACE_VERSION);
	GET_V_IFACE_ANY(GetServerFactory, server, IServerGameDLL, INTERFACEVERSION_SERVERGAMEDLL);
	GET_V_IFACE_ANY(GetServerFactory, gameclients, IServerGameClients, INTERFACEVERSION_SERVERGAMECLIENTS);
	GET_V_IFACE_ANY(GetEngineFactory, g_pNetworkServerService, INetworkServerService, NETWORKSERVERSERVICE_INTERFACE_VERSION);

	// Currently doesn't work from within mm side, use GetGameGlobals() in the mean time instead
	// gpGlobals = ismm->GetCGlobals();

	// Required to get the IMetamodListener events
	g_SMAPI->AddListener( this, this );

	META_CONPRINTF( "Starting plugin.\n" );

	SH_ADD_HOOK(IServerGameDLL, GameFrame, server, SH_MEMBER(this, &MMSPlugin::Hook_GameFrame), true);
	SH_ADD_HOOK(IServerGameClients, ClientActive, gameclients, SH_MEMBER(this, &MMSPlugin::Hook_ClientActive), true);
	SH_ADD_HOOK(IServerGameClients, ClientDisconnect, gameclients, SH_MEMBER(this, &MMSPlugin::Hook_ClientDisconnect), true);
	SH_ADD_HOOK(IServerGameClients, ClientPutInServer, gameclients, SH_MEMBER(this, &MMSPlugin::Hook_ClientPutInServer), true);
	SH_ADD_HOOK(IServerGameClients, ClientSettingsChanged, gameclients, SH_MEMBER(this, &MMSPlugin::Hook_ClientSettingsChanged), false);
	SH_ADD_HOOK(IServerGameClients, OnClientConnected, gameclients, SH_MEMBER(this, &MMSPlugin::Hook_OnClientConnected), false);
	SH_ADD_HOOK(IServerGameClients, ClientConnect, gameclients, SH_MEMBER(this, &MMSPlugin::Hook_ClientConnect), false);
	SH_ADD_HOOK(IServerGameClients, ClientCommand, gameclients, SH_MEMBER(this, &MMSPlugin::Hook_ClientCommand), false);

	META_CONPRINTF( "All hooks started!\n" );

	g_pCVar = icvar;
	META_CONVAR_REGISTER( FCVAR_RELEASE | FCVAR_CLIENT_CAN_EXECUTE | FCVAR_GAMEDLL );

	// You can get a convar reference to an already existing cvar via CConVarRef.
	// This will pre-register it if it's not yet registered and would use default data until
	// the actual cvar is registered. You can assert data existance via IsConVarDataAvailable().
	// Make sure the type is correct here otherwise it might prevent actual convar being registered,
	// since you pre-registered it with a different type or if convar already exists you'd be left with 
	// an invalid ref, so a check for IsValidRef() is also nice to have.
	// Generally with these you should just know the type of a cvar you are referencing beforehand
	// and if not, refer to ConVarRefAbstract usage
	// 
	// Side Note: Always make sure you are working with a valid ref (IsValidRef()) before reading/writing to it
	// as otherwise you'd be reading/writing off of default convar data which is shared across
	// all the invalid convar refs.
	CConVarRef<int> ccvar_ref_example( "mp_limitteams" );

	if(ccvar_ref_example.IsValidRef() && ccvar_ref_example.IsConVarDataAvailable())
	{
		META_CONPRINTF( "CConVarRef \"%s\" got value pre = %d [float = %f, bool = %d, string = \"%s\"]\n",
						ccvar_ref_example.GetName(), ccvar_ref_example.Get(),
						ccvar_ref_example.GetFloat(), ccvar_ref_example.GetBool(),
						ccvar_ref_example.GetString() );
		
		// By default if you are using CConVar or CConVarRef you should be using Get()/Set()
		// methods to read/write values, as these are templated for the particular type the cvar is of.
		// It also is usually faster since it skips all the type conversion logic of non templated methods
		ccvar_ref_example.Set( 5 );

		// As noted above there are methods that support value conversion between certain types
		// so stuff like this is possible on an int typed cvar for example,
		// refer to ConVarRefAbstract declaration for more info on these methods
		ccvar_ref_example.SetFloat( 8.5f );

		META_CONPRINTF( "CConVarRef \"%s\" got value after = %d [float = %f, bool = %d, string = \"%s\"]\n",
						ccvar_ref_example.GetName(), ccvar_ref_example.Get(),
						ccvar_ref_example.GetFloat(), ccvar_ref_example.GetBool(),
						ccvar_ref_example.GetString() );
	}

	// You can also use ConVarRefAbstract class if you don't want typisation support
	// or don't know the actual type used, since you are responsible for picking the correct type there!
	// And ConVarRefAbstract won't pre-register the convar in the system, as it acts as a plain ref,
	// so make sure to check the ref for validity before usage via IsValidRef()
	ConVarRefAbstract cvar_ref_example( "mp_limitteams" );

	if(cvar_ref_example.IsValidRef())
	{
		META_CONPRINTF( "ConVarRefAbstract \"%s\" got value pre [float = %f, bool = %d, string = \"%s\"]\n",
						cvar_ref_example.GetName(), cvar_ref_example.GetFloat(), cvar_ref_example.GetBool(), cvar_ref_example.GetString() );

		// Since the ref is not typed, you can't use direct Get() and Set() methods,
		// instead you need to use methods with type conversion support.
		cvar_ref_example.SetFloat( 10.0f );

		// If you work with convars of non primitive types, you can also use SetAs() methods
		// to try to set the value as a specific type, if type mismatches it would try to do 
		// conversion if possible and if not it would do nothing.
		// There's also an equvialent methods for reading the value, GetAs()
		cvar_ref_example.SetAs<Vector>( Vector( 1.0f, 2.0f, 3.0f ) );

		// Alternatively you can "promote" plain ref to a typed variant by passing plain ref to a constructor
		// but be careful, as there's a type checker in place that would invalidate convar ref
		// if cast to a wrong type was attempted, you can check for that either via IsValidRef()
		// or IsConVarDataValid() (usually IsValidRef() is enough) afterwards, but generally you should
		// just know the correct type of the cvar you are casting to beforehand.
		CConVarRef<int> promoted_ref( cvar_ref_example );
		if(promoted_ref.IsValidRef() && promoted_ref.IsConVarDataValid())
		{
			// If the promoted ref is valid, you can use its templated methods like with CConVarRef/CConVar
			promoted_ref.Set( 5 );
		}

		META_CONPRINTF( "ConVarRefAbstract \"%s\" got value after [float = %f, bool = %d, string = \"%s\"]\n",
						cvar_ref_example.GetName(), cvar_ref_example.GetFloat(), cvar_ref_example.GetBool(), cvar_ref_example.GetString() );
	}

	return true;
}

bool MMSPlugin::Unload(char *error, size_t maxlen)
{
	SH_REMOVE_HOOK(IServerGameDLL, GameFrame, server, SH_MEMBER(this, &MMSPlugin::Hook_GameFrame), true);
	SH_REMOVE_HOOK(IServerGameClients, ClientActive, gameclients, SH_MEMBER(this, &MMSPlugin::Hook_ClientActive), true);
	SH_REMOVE_HOOK(IServerGameClients, ClientDisconnect, gameclients, SH_MEMBER(this, &MMSPlugin::Hook_ClientDisconnect), true);
	SH_REMOVE_HOOK(IServerGameClients, ClientPutInServer, gameclients, SH_MEMBER(this, &MMSPlugin::Hook_ClientPutInServer), true);
	SH_REMOVE_HOOK(IServerGameClients, ClientSettingsChanged, gameclients, SH_MEMBER(this, &MMSPlugin::Hook_ClientSettingsChanged), false);
	SH_REMOVE_HOOK(IServerGameClients, OnClientConnected, gameclients, SH_MEMBER(this, &MMSPlugin::Hook_OnClientConnected), false);
	SH_REMOVE_HOOK(IServerGameClients, ClientConnect, gameclients, SH_MEMBER(this, &MMSPlugin::Hook_ClientConnect), false);
	SH_REMOVE_HOOK(IServerGameClients, ClientCommand, gameclients, SH_MEMBER(this, &MMSPlugin::Hook_ClientCommand), false);

	return true;
}

void MMSPlugin::AllPluginsLoaded()
{
	/* This is where we'd do stuff that relies on the mod or other plugins 
	 * being initialized (for example, cvars added and events registered).
	 */
}

void MMSPlugin::Hook_ClientActive( CPlayerSlot slot, bool bLoadGame, const char *pszName, uint64 xuid )
{
	META_CONPRINTF( "Hook_ClientActive(%d, %d, \"%s\", %d)\n", slot, bLoadGame, pszName, xuid );
}

void MMSPlugin::Hook_ClientCommand( CPlayerSlot slot, const CCommand &args )
{
	META_CONPRINTF( "Hook_ClientCommand(%d, \"%s\")\n", slot, args.GetCommandString() );
}

void MMSPlugin::Hook_ClientSettingsChanged( CPlayerSlot slot )
{
	META_CONPRINTF( "Hook_ClientSettingsChanged(%d)\n", slot );
}

void MMSPlugin::Hook_OnClientConnected( CPlayerSlot slot, const char *pszName, uint64 xuid, const char *pszNetworkID, const char *pszAddress, bool bFakePlayer )
{
	META_CONPRINTF( "Hook_OnClientConnected(%d, \"%s\", %d, \"%s\", \"%s\", %d)\n", slot, pszName, xuid, pszNetworkID, pszAddress, bFakePlayer );
}

bool MMSPlugin::Hook_ClientConnect( CPlayerSlot slot, const char *pszName, uint64 xuid, const char *pszNetworkID, bool unk1, CBufferString *pRejectReason )
{
	META_CONPRINTF( "Hook_ClientConnect(%d, \"%s\", %d, \"%s\", %d, \"%s\")\n", slot, pszName, xuid, pszNetworkID, unk1, pRejectReason->Get() );

	RETURN_META_VALUE(MRES_IGNORED, true);
}

void MMSPlugin::Hook_ClientPutInServer( CPlayerSlot slot, char const *pszName, int type, uint64 xuid )
{
	META_CONPRINTF( "Hook_ClientPutInServer(%d, \"%s\", %d, %d)\n", slot, pszName, type, xuid );
}

void MMSPlugin::Hook_ClientDisconnect( CPlayerSlot slot, ENetworkDisconnectionReason reason, const char *pszName, uint64 xuid, const char *pszNetworkID )
{
	META_CONPRINTF( "Hook_ClientDisconnect(%d, %d, \"%s\", %d, \"%s\")\n", slot, reason, pszName, xuid, pszNetworkID );
}

void MMSPlugin::Hook_GameFrame( bool simulating, bool bFirstTick, bool bLastTick )
{
	/**
	 * simulating:
	 * ***********
	 * true  | game is ticking
	 * false | game is not ticking
	 */
}

void MMSPlugin::OnLevelInit( char const *pMapName,
									 char const *pMapEntities,
									 char const *pOldLevel,
									 char const *pLandmarkName,
									 bool loadGame,
									 bool background )
{
	META_CONPRINTF("OnLevelInit(%s)\n", pMapName);
}

void MMSPlugin::OnLevelShutdown()
{
	META_CONPRINTF("OnLevelShutdown()\n");
}
