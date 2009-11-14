///////////////////////////////////////////////////////////////////////////////
//
//  File:   [!output PLUGIN_CLASS].h
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

#ifndef _PLUGIN_H
#define _PLUGIN_H

/////////////////////////////////////////////////////////////////////////

class CCommand;

/////////////////////////////////////////////////////////////////////////
// Class [!output PLUGIN_CLASS]

class [!output PLUGIN_CLASS] : public ISmmPlugin, public IMetamodListener
{
  ////////////////////////////////////////
  // Plugin Interface

  public:

	  virtual bool Load(PluginId id, 
                      ISmmAPI* ismm,
                      char* error,
                      size_t maxlen,
                      bool late);

	  virtual bool Unload(char* error, size_t maxlen);
	  virtual void OnVSPListening(IServerPluginCallbacks* vsp_callbacks);

  ////////////////////////////////////////
  // Plugin Info

  public:

	  virtual const char* GetAuthor();
	  virtual const char* GetName();
	  virtual const char* GetDescription();
	  virtual const char* GetURL();
	  virtual const char* GetLicense();
	  virtual const char* GetVersion();
	  virtual const char* GetDate();
	  virtual const char* GetLogTag();

  ////////////////////////////////////////
  // Plugin Globals

  public:

    static IServerGameDLL*         GameServer;
    static IServerGameClients*     GameClients;
    static IVEngineServer*         EngineServer;
    static IServerPluginHelpers*   PluginHelpers;
    static IGameEventManager2*     GameEvents;
    static IServerPluginCallbacks* VspCallbacks;
    static IPlayerInfoManager*     PlayerInfoManager;
    static ICvar*                  CVar;
    static CGlobalVars*            GlobalVars;

    static [!output PLUGIN_CLASS] Global;

  ////////////////////////////////////////
  // Hooks

  private:

    void AddHooks();
    void RemoveHooks();

[!if IServerGameDLL_ServerActivate]
    DECL_HOOK_SERVER_ACTIVATE;
[!endif]
[!if IServerGameDLL_GameInit]
    DECL_HOOK_GAME_INIT;
[!endif]
[!if IServerGameDLL_GameFrame]
    DECL_HOOK_GAME_FRAME;
[!endif]
[!if IServerGameDLL_GameShutdown]
    DECL_HOOK_GAME_SHUTDOWN;
[!endif]
[!if IServerGameDLL_LevelInit]
    DECL_HOOK_LEVEL_INIT;
[!endif]
[!if IServerGameDLL_LevelShutdown]
    DECL_HOOK_LEVEL_SHUTDOWN;
[!endif]
[!if IServerGameDLL_GetAllServerClasses]
    DECL_HOOK_GET_ALL_SERVER_CLASSES;
[!endif]
[!if IServerGameDLL_GetTickInterval]
    DECL_HOOK_GET_TICK_INTERVAL;
[!endif]
[!if IServerGameDLL_GetGameDescription]
    DECL_HOOK_GET_GAME_DESC;
[!endif]
[!if IServerGameClients_ClientActive]
    DECL_HOOK_CLIENT_ACTIVE;
[!endif]
[!if IServerGameClients_ClientConnect]
    DECL_HOOK_CLIENT_CONNECT;
[!endif]
[!if IServerGameClients_ClientDisconnect]
    DECL_HOOK_CLIENT_DISCONNECT;
[!endif]
[!if IServerGameClients_ClientCommand]
    DECL_HOOK_CLIENT_COMMAND;
[!endif]
[!if IServerGameClients_ClientPutInServer]
    DECL_HOOK_CLIENT_PUT_IN_SERVER;
[!endif]
[!if IServerGameClients_ClientSettingsChanged]
    DECL_HOOK_CLIENT_SETTINGS_CHANGED;
[!endif]
[!if IServerGameClients_ClientSetupVisibility]
    DECL_HOOK_CLIENT_SETUP_VISIBILITY;
[!endif]
[!if IServerGameClients_ClientEarPosition]
    DECL_HOOK_CLIENT_EAR_POSITION;
[!endif]
[!if IServerGameClients_PostClientMessagesSent]
    DECL_HOOK_POST_CLIENT_MESSAGES_SENT;
[!endif]
[!if IServerGameClients_ProcessUsercmds]
    DECL_HOOK_PROCESS_USER_CMDS;
[!endif]
[!if IServerGameClients_NetworkIDValidated]
    DECL_HOOK_NETWORK_ID_VALIDATED;
[!endif]
[!if IServerGameClients_GetPlayerLimits]
    DECL_HOOK_GET_PLAYER_LIMITS;
[!endif]
[!if IServerGameClients_GetPlayerState]
    DECL_HOOK_GET_PLAYER_STATE;
[!endif]
[!if IServerGameClients_GetReplayDelay]
    DECL_HOOK_GET_REPLAY_DELAY;
[!endif]
[!if IServerGameClients_GetBugReportInfo]
    DECL_HOOK_GET_BUG_REPORT_INFO;
[!endif]
[!if IServerGameClients_SetCommandClient]
    DECL_HOOK_SET_COMMAND_CLIENT;
[!endif]
};

/////////////////////////////////////////////////////////////////////////
// Global Vars

PLUGIN_GLOBALVARS();

#endif // _PLUGIN_H
