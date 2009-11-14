///////////////////////////////////////////////////////////////////////////////
//
//  File:   [!output PLUGIN_CLASS]Hooks.cpp
//  Author: [!output PLUGIN_AUTHOR]
//  Date:   [!output CURRENT_DATE]
//  
//  License: [!output PLUGIN_LICENSE]
//
//  This software is provided 'as-is', without any express or
//  implied warranty. In no event will the authors be held liable
//  for any damages arising from the use of this software.
//
//  Based on code written by AlliedModders LLC. Sample Plugin
//
///////////////////////////////////////////////////////////////////////////////

#include "StdMMS.h"

/////////////////////////////////////////////////////////////////////////
// Declare the Hooks

[!if IServerGameDLL_ServerActivate]
SH_DECL_HOOK3_void(IServerGameDLL, ServerActivate, SH_NOATTRIB, 0, edict_t*, int, int);
[!endif]
[!if IServerGameDLL_GameInit]
SH_DECL_HOOK0(IServerGameDLL, GameInit, SH_NOATTRIB, 0, bool);
[!endif]
[!if IServerGameDLL_GameFrame]
SH_DECL_HOOK1_void(IServerGameDLL, GameFrame, SH_NOATTRIB, 0, bool);
[!endif]
[!if IServerGameDLL_GameShutdown]
SH_DECL_HOOK0_void(IServerGameDLL, GameShutdown, SH_NOATTRIB, 0);
[!endif]
[!if IServerGameDLL_LevelInit]
SH_DECL_HOOK6(IServerGameDLL, LevelInit, SH_NOATTRIB, 0, bool, char const*, char const*, char const*, char const*, bool, bool);
[!endif]
[!if IServerGameDLL_LevelShutdown]
SH_DECL_HOOK0_void(IServerGameDLL, LevelShutdown, SH_NOATTRIB, 0);
[!endif]
[!if IServerGameDLL_GetAllServerClasses]
SH_DECL_HOOK0(IServerGameDLL, GetAllServerClasses, SH_NOATTRIB, 0, ServerClass*);
[!endif]
[!if IServerGameDLL_GetTickInterval]
SH_DECL_HOOK0(IServerGameDLL, GetTickInterval, const, 0, float);
[!endif]
[!if IServerGameDLL_GetGameDescription]
SH_DECL_HOOK0(IServerGameDLL, GetGameDescription, SH_NOATTRIB, 0, const char*);
[!endif]

[!if IServerGameClients_ClientActive]
SH_DECL_HOOK2_void(IServerGameClients, ClientActive, SH_NOATTRIB, 0, edict_t*, bool);
[!endif]
[!if IServerGameClients_ClientConnect]
SH_DECL_HOOK5(IServerGameClients, ClientConnect, SH_NOATTRIB, 0, bool, edict_t*, const char*, const char *, char *, int);
[!endif]
[!if IServerGameClients_ClientDisconnect]
SH_DECL_HOOK1_void(IServerGameClients, ClientDisconnect, SH_NOATTRIB, 0, edict_t*);
[!endif]
[!if IServerGameClients_ClientPutInServer]
SH_DECL_HOOK2_void(IServerGameClients, ClientPutInServer, SH_NOATTRIB, 0, edict_t*, char const *);
[!endif]
[!if IServerGameClients_ClientSettingsChanged]
SH_DECL_HOOK1_void(IServerGameClients, ClientSettingsChanged, SH_NOATTRIB, 0, edict_t*);
[!endif]
[!if IServerGameClients_ClientSetupVisibility]
SH_DECL_HOOK4_void(IServerGameClients, ClientSetupVisibility, SH_NOATTRIB, 0, edict_t*, edict_t*, unsigned char*, int);
[!endif]
[!if IServerGameClients_ClientEarPosition]
SH_DECL_HOOK2_void(IServerGameClients, ClientEarPosition, SH_NOATTRIB, 0, edict_t*, Vector*);
[!endif]
[!if IServerGameClients_PostClientMessagesSent]
SH_DECL_HOOK0_void(IServerGameClients, PostClientMessagesSent, SH_NOATTRIB, 0);
[!endif]
[!if IServerGameClients_ProcessUsercmds]
SH_DECL_HOOK7(IServerGameClients, ProcessUsercmds, SH_NOATTRIB, 0, float, edict_t*, bf_read*, int, int, int, bool, bool);
[!endif]
[!if IServerGameClients_GetPlayerLimits]
SH_DECL_HOOK3_void(IServerGameClients, GetPlayerLimits, const, 0, int&, int&, int&);
[!endif]
[!if IServerGameClients_GetPlayerState]
SH_DECL_HOOK1(IServerGameClients, GetPlayerState, SH_NOATTRIB, 0, CPlayerState*, edict_t*);
[!endif]
[!if IServerGameClients_GetReplayDelay]
SH_DECL_HOOK2(IServerGameClients, GetReplayDelay, SH_NOATTRIB, 0, int, edict_t*, int&);
[!endif]
[!if IServerGameClients_GetBugReportInfo]
SH_DECL_HOOK2_void(IServerGameClients, GetBugReportInfo, SH_NOATTRIB, 0, char*, int);
[!endif]
[!if IServerGameClients_SetCommandClient]
SH_DECL_HOOK1_void(IServerGameClients, SetCommandClient, SH_NOATTRIB, 0, int);
[!endif]
SH_DECL_HOOK2(IGameEventManager2, FireEvent, SH_NOATTRIB, 0, bool, IGameEvent*, bool);

#if SOURCE_ENGINE >= SE_ORANGEBOX

[!if IServerGameClients_NetworkIDValidated]
SH_DECL_HOOK2_void(IServerGameClients, NetworkIDValidated, SH_NOATTRIB, 0, const char *, const char *);
[!endif]
[!if IServerGameClients_ClientCommand]
SH_DECL_HOOK2_void(IServerGameClients, ClientCommand, SH_NOATTRIB, 0, edict_t*, const CCommand&);
[!endif]

#else

[!if IServerGameClients_ClientCommand]
SH_DECL_HOOK1_void(IServerGameClients, ClientCommand, SH_NOATTRIB, 0, edict_t*);
[!endif]

#endif


/////////////////////////////////////////////////////////////////////////
// AddHooks

void [!output PLUGIN_CLASS]::AddHooks()
{
  // Add the ServerGameDLL hooks

[!if IServerGameDLL_ServerActivate]
  SH_ADD_HOOK_MEMFUNC(IServerGameDLL, ServerActivate, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedServerActivate, true);
[!endif]
[!if IServerGameDLL_GameInit]
  SH_ADD_HOOK_MEMFUNC(IServerGameDLL, GameInit, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedGameInit, true);
[!endif]
[!if IServerGameDLL_GameFrame]
  SH_ADD_HOOK_MEMFUNC(IServerGameDLL, GameFrame, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedGameFrame, true);
[!endif]
[!if IServerGameDLL_GameShutdown]
  SH_ADD_HOOK_MEMFUNC(IServerGameDLL, GameShutdown, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedGameShutdown, true);
[!endif]
[!if IServerGameDLL_LevelInit]
  SH_ADD_HOOK_MEMFUNC(IServerGameDLL, LevelInit, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedLevelInit, true);
[!endif]
[!if IServerGameDLL_LevelShutdown]
  SH_ADD_HOOK_MEMFUNC(IServerGameDLL, LevelShutdown, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedLevelShutdown, true);
[!endif]
[!if IServerGameDLL_GetAllServerClasses]
  SH_ADD_HOOK_MEMFUNC(IServerGameDLL, GetAllServerClasses, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedGetAllServerClasses, true);
[!endif]
[!if IServerGameDLL_GetTickInterval]
  SH_ADD_HOOK_MEMFUNC(IServerGameDLL, GetTickInterval, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedGetTickInterval, true);
[!endif]
[!if IServerGameDLL_GetGameDescription]
  SH_ADD_HOOK_MEMFUNC(IServerGameDLL, GetGameDescription, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedGetGameDescription, true);
[!endif]

  // Add the ServerGameClients hooks

[!if IServerGameClients_ClientActive]
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientActive, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedClientActive, true);
[!endif]
[!if IServerGameClients_ClientConnect]
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientConnect, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedClientConnect, true);
[!endif]
[!if IServerGameClients_ClientDisconnect]
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientDisconnect, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedClientDisconnect, true);
[!endif]
[!if IServerGameClients_ClientCommand]
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientCommand, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedClientCommand, true);
[!endif]
[!if IServerGameClients_ClientPutInServer]
  SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientPutInServer, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedClientPutInServer, true);
[!endif]
[!if IServerGameClients_ClientSettingsChanged]
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientSettingsChanged, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedClientSettingsChanged, true);
[!endif]
[!if IServerGameClients_ClientSetupVisibility]
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientSetupVisibility, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedClientSetupVisibility, true);
[!endif]
[!if IServerGameClients_ClientEarPosition]
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ClientEarPosition, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedClientEarPosition, true);
[!endif]
[!if IServerGameClients_PostClientMessagesSent]
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, PostClientMessagesSent, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedPostClientMessagesSent, true);
[!endif]
[!if IServerGameClients_ProcessUsercmds]
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, ProcessUsercmds, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedProcessUsercmds, false);
[!endif]
[!if IServerGameClients_GetPlayerLimits]
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, GetPlayerLimits, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedGetPlayerLimits, true);
[!endif]
[!if IServerGameClients_GetPlayerState]
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, GetPlayerState, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedGetPlayerState, true);
[!endif]
[!if IServerGameClients_GetReplayDelay]
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, GetReplayDelay, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedGetReplayDelay, true);
[!endif]
[!if IServerGameClients_GetBugReportInfo]
	SH_ADD_HOOK_MEMFUNC(IServerGameClients, GetBugReportInfo, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedGetBugReportInfo, true);
[!endif]
[!if IServerGameClients_SetCommandClient]
  SH_ADD_HOOK_MEMFUNC(IServerGameClients, SetCommandClient, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedSetCommandClient, true);
[!endif]

[!if IServerGameClients_NetworkIDValidated]
  #if SOURCE_ENGINE >= SE_ORANGEBOX

	SH_ADD_HOOK_MEMFUNC(IServerGameClients, NetworkIDValidated, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedNetworkIDValidated, true);

  #endif
[!endif]
}

/////////////////////////////////////////////////////////////////////////
// RemoveHooks

void [!output PLUGIN_CLASS]::RemoveHooks()
{
  // Remove the ServerGameDLL hooks

[!if IServerGameDLL_ServerActivate]
  SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, ServerActivate, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedServerActivate, true);
[!endif]
[!if IServerGameDLL_GameInit]
  SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, GameInit, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedGameInit, true);
[!endif]
[!if IServerGameDLL_GameFrame]
  SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, GameFrame, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedGameFrame, true);
[!endif]
[!if IServerGameDLL_GameShutdown]
  SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, GameShutdown, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedGameShutdown, true);
[!endif]
[!if IServerGameDLL_LevelInit]
  SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, LevelInit, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedLevelInit, true);
[!endif]
[!if IServerGameDLL_LevelShutdown]
  SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, LevelShutdown, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedLevelShutdown, true);
[!endif]
[!if IServerGameDLL_GetAllServerClasses]
  SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, GetAllServerClasses, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedGetAllServerClasses, true);
[!endif]
[!if IServerGameDLL_GetTickInterval]
  SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, GetTickInterval, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedGetTickInterval, true);
[!endif]
[!if IServerGameDLL_GetGameDescription]
  SH_REMOVE_HOOK_MEMFUNC(IServerGameDLL, GetGameDescription, [!output PLUGIN_CLASS]::GameServer, this, &[!output PLUGIN_CLASS]::HookedGetGameDescription, true);
[!endif]

  // Remove the ServerGameClients hooks

[!if IServerGameClients_ClientActive]
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientActive, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedClientActive, true);
[!endif]
[!if IServerGameClients_ClientConnect]
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientConnect, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedClientConnect, true);
[!endif]
[!if IServerGameClients_ClientDisconnect]
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientDisconnect, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedClientDisconnect, true);
[!endif]
[!if IServerGameClients_ClientCommand]
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientCommand, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedClientCommand, true);
[!endif]
[!if IServerGameClients_ClientPutInServer]
  SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientPutInServer, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedClientPutInServer, true);
[!endif]
[!if IServerGameClients_ClientSettingsChanged]
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientSettingsChanged, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedClientSettingsChanged, true);
[!endif]
[!if IServerGameClients_ClientSetupVisibility]
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientSetupVisibility, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedClientSetupVisibility, true);
[!endif]
[!if IServerGameClients_ClientEarPosition]
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ClientEarPosition, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedClientEarPosition, true);
[!endif]
[!if IServerGameClients_PostClientMessagesSent]
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, PostClientMessagesSent, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedPostClientMessagesSent, true);
[!endif]
[!if IServerGameClients_ProcessUsercmds]
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, ProcessUsercmds, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedProcessUsercmds, true);
[!endif]
[!if IServerGameClients_GetPlayerLimits]
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, GetPlayerLimits, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedGetPlayerLimits, true);
[!endif]
[!if IServerGameClients_GetPlayerState]
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, GetPlayerState, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedGetPlayerState, true);
[!endif]
[!if IServerGameClients_GetReplayDelay]
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, GetReplayDelay, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedGetReplayDelay, true);
[!endif]
[!if IServerGameClients_GetBugReportInfo]
	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, GetBugReportInfo, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedGetBugReportInfo, true);
[!endif]
[!if IServerGameClients_SetCommandClient]
  SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, SetCommandClient, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedSetCommandClient, true);
[!endif]

[!if IServerGameClients_NetworkIDValidated]
  #if SOURCE_ENGINE >= SE_ORANGEBOX

	SH_REMOVE_HOOK_MEMFUNC(IServerGameClients, NetworkIDValidated, [!output PLUGIN_CLASS]::GameClients, this, &[!output PLUGIN_CLASS]::HookedNetworkIDValidated, true);

  #endif
[!endif]
}


[!if IServerGameDLL_ServerActivate]
/////////////////////////////////////////////////////////////////////////
// HookedServerActivate

// The server is about to activate

void [!output PLUGIN_CLASS]::HookedServerActivate(edict_t* edict_list, int edict_count, int client_max)
{
  // TODO: Implement Method

  RETURN_META(MRES_IGNORED);
}

[!endif]
[!if IServerGameDLL_GameInit]
/////////////////////////////////////////////////////////////////////////
// HookedGameInit

// This is called when a new game is started. (restart, map)

bool [!output PLUGIN_CLASS]::HookedGameInit()
{
  // TODO: Implement Method

  RETURN_META_VALUE(MRES_IGNORED, true);
}

[!endif]
[!if IServerGameDLL_GameFrame]
/////////////////////////////////////////////////////////////////////////
// HookedGameFrame

// The server should run physics / think on all edicts

void [!output PLUGIN_CLASS]::HookedGameFrame(bool simulating)
{
  // TODO: Implement Method

  RETURN_META(MRES_IGNORED);
}

[!endif]
[!if IServerGameDLL_GameShutdown]
/////////////////////////////////////////////////////////////////////////
// HookedGameShutdown

// This is called when a game ends (server disconnect, death, 
// restart, load). NOT on level transitions within a game

void [!output PLUGIN_CLASS]::HookedGameShutdown()
{
  // TODO: Implement Method

  RETURN_META(MRES_IGNORED);
}

[!endif]
[!if IServerGameDLL_LevelInit]
/////////////////////////////////////////////////////////////////////////
// HookedLevelInit

// Called any time a new level is started (after 
// GameInit() also on level transitions within a game)

bool [!output PLUGIN_CLASS]::HookedLevelInit(const char* map_name, char const* map_entities, char const* old_level,
                                             char const* landmark_name, bool load_game, bool bkgnd)
{
  // TODO: Implement Method

  RETURN_META_VALUE(MRES_IGNORED, true);
}

[!endif]
[!if IServerGameDLL_LevelShutdown]
/////////////////////////////////////////////////////////////////////////
// HookedLevelShutdown

// Called when a level is shutdown (including changing levels)

void [!output PLUGIN_CLASS]::HookedLevelShutdown()
{
  // TODO: Implement Method

  RETURN_META(MRES_IGNORED);
}

[!endif]
[!if IServerGameDLL_GetAllServerClasses]
/////////////////////////////////////////////////////////////////////////
// HookedGetAllServerClasses

// Gets the list of datatable classes to the engine. The engine
// matches class names from here with edict_t::classname to figure
// out how to encode a class's data for networking

ServerClass* [!output PLUGIN_CLASS]::HookedGetAllServerClasses()
{
  // TODO: Implement Method

  RETURN_META_VALUE(MRES_IGNORED, NULL);
}

[!endif]
[!if IServerGameDLL_GetTickInterval]
/////////////////////////////////////////////////////////////////////////
// HookedGetTickInterval

// Get the simulation interval (must be compiled with identical values
// into both client and game .dll for MOD!!!). Right now this is only 
// requested at server startup time so it can't be changed on the fly, etc.

float	[!output PLUGIN_CLASS]::HookedGetTickInterval() const
{
  // TODO: Implement Method

  RETURN_META_VALUE(MRES_IGNORED, 0.0f);
}

[!endif]
[!if IServerGameDLL_GetGameDescription]
/////////////////////////////////////////////////////////////////////////
// HookedGetGameDescription

// Returns string describing current .dll. 
// e.g. TeamFortress 2, Half-Life 2 

const char* [!output PLUGIN_CLASS]::HookedGetGameDescription()
{
  // TODO: Implement Method

  RETURN_META_VALUE(MRES_IGNORED, "");
}

[!endif]
[!if IServerGameClients_ClientActive]
/////////////////////////////////////////////////////////////////////////
// HookedClientActive

// Client is going active. If load_game is true, don't 
// spawn the player because its state is already setup.

void [!output PLUGIN_CLASS]::HookedClientActive(edict_t* entity, bool load_game)
{
  // TODO: Implement Method

  RETURN_META(MRES_IGNORED);
}

[!endif]
[!if IServerGameClients_ClientConnect]
/////////////////////////////////////////////////////////////////////////
// HookedClientConnect

// Client is connecting to server (return false to reject the connection)
// You can specify a rejection message by writing it into reject

bool [!output PLUGIN_CLASS]::HookedClientConnect(edict_t* entity, const char* name, const char* address, char* reject, int reject_len)
{
  // TODO: Implement Method

  RETURN_META_VALUE(MRES_IGNORED, true);
}

[!endif]
[!if IServerGameClients_ClientDisconnect]
/////////////////////////////////////////////////////////////////////////
// HookedClientDisconnect

// Client is disconnecting from server

void [!output PLUGIN_CLASS]::HookedClientDisconnect(edict_t* entity)
{
  // TODO: Implement Method

  RETURN_META(MRES_IGNORED);
}

[!endif]
[!if IServerGameClients_ClientCommand]
/////////////////////////////////////////////////////////////////////////
// HookedClientCommand

// The client has typed a command at the console

#if SOURCE_ENGINE >= SE_ORANGEBOX
void [!output PLUGIN_CLASS]::HookedClientCommand(edict_t* entity, const CCommand& args)
#else
void [!output PLUGIN_CLASS]::HookedClientCommand(edict_t* entity)
#endif
{
  // Verify the parameters

  #if SOURCE_ENGINE <= SE_DARKMESSIAH
	CCommand args;
  #endif

	if(!entity || entity->IsFree())
	{
    RETURN_META(MRES_IGNORED);
	}

  // TODO: Implement Method
  
  RETURN_META(MRES_IGNORED);
}

[!endif]
[!if IServerGameClients_ClientPutInServer]
/////////////////////////////////////////////////////////////////////////
// HookedClientPutInServer

// Client is connected and should be put in the game

void [!output PLUGIN_CLASS]::HookedClientPutInServer(edict_t* entity, char const* name)
{
  // TODO: Implement Method

  RETURN_META(MRES_IGNORED);
}

[!endif]
[!if IServerGameClients_ClientSettingsChanged]
/////////////////////////////////////////////////////////////////////////
// HookedClientSettingsChanged

// A player changed one / several replicated cvars (name etc...)

void [!output PLUGIN_CLASS]::HookedClientSettingsChanged(edict_t* edict)
{
  // TODO: Implement Method

  RETURN_META(MRES_IGNORED);
}

[!endif]
[!if IServerGameClients_ClientSetupVisibility]
/////////////////////////////////////////////////////////////////////////
// HookedClientSetupVisibility

// Determine PVS origin and set PVS for the player / view entity

void [!output PLUGIN_CLASS]::HookedClientSetupVisibility(edict_t* view_entity, edict_t* client, unsigned char *pvs, int pvs_size)
{
  // TODO: Implement Method

  RETURN_META(MRES_IGNORED);
}

[!endif]
[!if IServerGameClients_ClientEarPosition]
/////////////////////////////////////////////////////////////////////////
// HookedClientEarPosition

// Get the ear position for a specified client

void [!output PLUGIN_CLASS]::HookedClientEarPosition(edict_t* entity, Vector* ear_origin)
{
  // TODO: Implement Method

  RETURN_META(MRES_IGNORED);
}

[!endif]
[!if IServerGameClients_PostClientMessagesSent]
/////////////////////////////////////////////////////////////////////////
// HookedPostClientMessagesSent

// Let the game .dll do stuff after messages have been sent
// to all of the clients once the server frame is complete

void [!output PLUGIN_CLASS]::HookedPostClientMessagesSent()
{
  // TODO: Implement Method

  RETURN_META(MRES_IGNORED);
}

[!endif]
[!if IServerGameClients_ProcessUsercmds]
/////////////////////////////////////////////////////////////////////////
// HookedProcessUsercmds

// A block of CUserCmds has arrived from the user, decode
// them and buffer for execution during player simulation

float [!output PLUGIN_CLASS]::HookedProcessUsercmds(edict_t* player, bf_read* buf, int num_cmds, int total_cmds, 
                                                    int dropped_packets, bool ignore, bool paused)
{
  // TODO: Implement Method

  RETURN_META_VALUE(MRES_IGNORED, 0.0f);
}

[!endif]
[!if IServerGameClients_NetworkIDValidated]
/////////////////////////////////////////////////////////////////////////
// HookedNetworkIDValidated

// A user has had their network id setup and validated

void [!output PLUGIN_CLASS]::HookedNetworkIDValidated(const char* user_name, const char* network_id)
{
  // TODO: Implement Method

  RETURN_META(MRES_IGNORED);
}

[!endif]
[!if IServerGameClients_GetPlayerLimits]
/////////////////////////////////////////////////////////////////////////
// HookedGetPlayerLimits

// Get server max players and lower bound for the same

void [!output PLUGIN_CLASS]::HookedGetPlayerLimits(int& min_players, int& max_players, int& default_max) const
{
  // TODO: Implement Method

  RETURN_META(MRES_IGNORED);
}

[!endif]
[!if IServerGameClients_GetPlayerState]
/////////////////////////////////////////////////////////////////////////
// HookedGetPlayerState

// For players, looks up the CPlayerState structure corresponding to the player

CPlayerState* [!output PLUGIN_CLASS]::HookedGetPlayerState(edict_t* player)
{
  // TODO: Implement Method

  RETURN_META_VALUE(MRES_IGNORED, NULL);
}

[!endif]
[!if IServerGameClients_GetReplayDelay]
/////////////////////////////////////////////////////////////////////////
// HookedGetReplayDelay

// Returns number of delay ticks if player is in Replay mode (0 = no delay)

int [!output PLUGIN_CLASS]::HookedGetReplayDelay(edict_t* player, int& entity)
{
  // TODO: Implement Method

  RETURN_META_VALUE(MRES_IGNORED, 0);
}

[!endif]
[!if IServerGameClients_GetBugReportInfo]
/////////////////////////////////////////////////////////////////////////
// HookedGetBugReportInfo

// Anything this game .dll wants to add to the bug reporter text
//(e.g., the entity/model under the picker crosshair) can be added here

void [!output PLUGIN_CLASS]::HookedGetBugReportInfo(char* buf, int size)
{
  // TODO: Implement Method

  RETURN_META(MRES_IGNORED);
}

[!endif]
[!if IServerGameClients_SetCommandClient]
/////////////////////////////////////////////////////////////////////////
// HookedSetCommandClient

// Sets the client index for the client who typed the command into his / her console

void [!output PLUGIN_CLASS]::HookedSetCommandClient(int index)
{
  // TODO: Implement Method

  RETURN_META(MRES_IGNORED);
}

[!endif]
