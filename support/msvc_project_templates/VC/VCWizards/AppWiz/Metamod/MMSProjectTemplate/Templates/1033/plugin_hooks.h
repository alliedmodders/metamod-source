///////////////////////////////////////////////////////////////////////////////
//
//  File:   [!output PLUGIN_CLASS]Hooks.h
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

#ifndef _PLUGIN_HOOKS_H
#define _PLUGIN_HOOKS_H

[!if IServerGameDLL_ServerActivate]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_SERVER_ACTIVATE

#define DECL_HOOK_SERVER_ACTIVATE                         \
                                                          \
	  void HookedServerActivate(edict_t* edict_list,        \
                              int edict_count,            \
                              int client_max)

[!endif]
[!if IServerGameDLL_GameInit]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_GAME_INIT

#define DECL_HOOK_GAME_INIT                               \
                                                          \
    bool HookedGameInit()

[!endif]
[!if IServerGameDLL_GameFrame]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_GAME_FRAME

#define DECL_HOOK_GAME_FRAME                              \
                                                          \
	  void HookedGameFrame(bool simulating)

[!endif]
[!if IServerGameDLL_GameShutdown]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_GAME_SHUTDOWN

#define DECL_HOOK_GAME_SHUTDOWN                           \
                                                          \
    void HookedGameShutdown()

[!endif]
[!if IServerGameDLL_LevelInit]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_LEVEL_INIT

#define DECL_HOOK_LEVEL_INIT                              \
                                                          \
	  bool HookedLevelInit(const char* map_name,            \
		                     char const* map_entities,        \
		                     char const* old_level,           \
		                     char const* landmark_name,       \
		                     bool load_game, bool bkgnd)

[!endif]
[!if IServerGameDLL_LevelShutdown]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_LEVEL_SHUTDOWN

#define DECL_HOOK_LEVEL_SHUTDOWN                          \
                                                          \
	  void HookedLevelShutdown();

[!endif]
[!if IServerGameDLL_GetAllServerClasses]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_GET_ALL_SERVER_CLASSES

#define DECL_HOOK_GET_ALL_SERVER_CLASSES                  \
                                                          \
	  ServerClass* HookedGetAllServerClasses()

[!endif]
[!if IServerGameDLL_GetTickInterval]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_GET_TICK_INTERVAL

#define DECL_HOOK_GET_TICK_INTERVAL                       \
                                                          \
	  float	HookedGetTickInterval() const

[!endif]
[!if IServerGameDLL_GetGameDescription]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_GET_GAME_DESC

#define DECL_HOOK_GET_GAME_DESC                           \
                                                          \
	  const char* HookedGetGameDescription()

[!endif]
[!if IServerGameClients_ClientActive]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_CLIENT_ACTIVE

#define DECL_HOOK_CLIENT_ACTIVE                           \
                                                          \
	  void HookedClientActive(edict_t* entity,              \
                            bool load_game)

[!endif]
[!if IServerGameClients_ClientConnect]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_CLIENT_CONNECT

#define DECL_HOOK_CLIENT_CONNECT                          \
                                                          \
    bool HookedClientConnect(edict_t* entity,             \
		                         const char* name,            \
		                         const char* address,         \
		                         char* reject,                \
		                         int reject_len)

[!endif]
[!if IServerGameClients_ClientDisconnect]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_CLIENT_DISCONNECT

#define DECL_HOOK_CLIENT_DISCONNECT                       \
                                                          \
    void HookedClientDisconnect(edict_t* entity)

[!endif]
[!if IServerGameClients_ClientCommand]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_CLIENT_COMMAND

#if SOURCE_ENGINE >= SE_ORANGEBOX
#define DECL_HOOK_CLIENT_COMMAND                          \
                                                          \
	  void HookedClientCommand(edict_t* entity,             \
                             const CCommand& args)
#else
#define DECL_HOOK_CLIENT_COMMAND                          \
                                                          \
	  void HookedClientCommand(edict_t* entity)
#endif

[!endif]
[!if IServerGameClients_ClientPutInServer]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_CLIENT_PUT_IN_SERVER

#define DECL_HOOK_CLIENT_PUT_IN_SERVER                    \
                                                          \
    void HookedClientPutInServer(edict_t* entity,         \
                                 char const* name)

[!endif]
[!if IServerGameClients_ClientSettingsChanged]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_CLIENT_SETTINGS_CHANGED

#define DECL_HOOK_CLIENT_SETTINGS_CHANGED                 \
                                                          \
    void HookedClientSettingsChanged(edict_t* edict)

[!endif]
[!if IServerGameClients_ClientSetupVisibility]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_CLIENT_SETUP_VISIBILITY

#define DECL_HOOK_CLIENT_SETUP_VISIBILITY                     \
                                                              \
    void HookedClientSetupVisibility(edict_t* view_entity,    \
                                     edict_t* client,         \
                                     unsigned char *pvs,      \
                                     int pvs_size)

[!endif]
[!if IServerGameClients_ClientEarPosition]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_CLIENT_EAR_POSITION

#define DECL_HOOK_CLIENT_EAR_POSITION                     \
                                                          \
    void HookedClientEarPosition(edict_t* entity,         \
                                 Vector* ear_origin)

[!endif]
[!if IServerGameClients_PostClientMessagesSent]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_POST_CLIENT_MESSAGES_SENT

#define DECL_HOOK_POST_CLIENT_MESSAGES_SENT               \
                                                          \
    void HookedPostClientMessagesSent()

[!endif]
[!if IServerGameClients_ProcessUsercmds]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_PROCESS_USER_CMDS

#define DECL_HOOK_PROCESS_USER_CMDS                       \
                                                          \
    float HookedProcessUsercmds(edict_t* player,          \
                                bf_read* buf,             \
                                int num_cmds,             \
                                int total_cmds,           \
                                int dropped_packets,      \
                                bool ignore,              \
                                bool paused)

[!endif]
[!if IServerGameClients_NetworkIDValidated]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_NETWORK_ID_VALIDATED

#define DECL_HOOK_NETWORK_ID_VALIDATED                    \
                                                          \
    void HookedNetworkIDValidated(const char* user_name,  \
                                  const char* network_id)

[!endif]
[!if IServerGameClients_GetPlayerLimits]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_GET_PLAYER_LIMITS

#define DECL_HOOK_GET_PLAYER_LIMITS                       \
                                                          \
    void HookedGetPlayerLimits(int& min_players,          \
                               int& max_players,          \
                               int& default_max) const

[!endif]
[!if IServerGameClients_GetPlayerState]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_GET_PLAYER_STATE

#define DECL_HOOK_GET_PLAYER_STATE                        \
                                                          \
    CPlayerState* HookedGetPlayerState(edict_t* player)

[!endif]
[!if IServerGameClients_GetReplayDelay]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_GET_REPLAY_DELAY

#define DECL_HOOK_GET_REPLAY_DELAY                        \
                                                          \
    int HookedGetReplayDelay(edict_t* player,             \
                             int& entity)

[!endif]
[!if IServerGameClients_GetBugReportInfo]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_GET_BUG_REPORT_INFO

#define DECL_HOOK_GET_BUG_REPORT_INFO                     \
                                                          \
    void HookedGetBugReportInfo(char* buf, int size)

[!endif]
[!if IServerGameClients_SetCommandClient]
/////////////////////////////////////////////////////////////////////////
// Define DECL_HOOK_SET_COMMAND_CLIENT

#define DECL_HOOK_SET_COMMAND_CLIENT                      \
                                                          \
    void HookedSetCommandClient(int index)

[!endif]
#endif // _PLUGIN_HOOKS_H
