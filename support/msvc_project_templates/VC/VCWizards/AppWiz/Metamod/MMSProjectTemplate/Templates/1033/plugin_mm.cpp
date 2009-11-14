///////////////////////////////////////////////////////////////////////////////
//
//  File:   [!output PLUGIN_CLASS].cpp
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
// Global Variables

IServerGameDLL*         [!output PLUGIN_CLASS]::GameServer(NULL);
IServerGameClients*     [!output PLUGIN_CLASS]::GameClients(NULL);
IVEngineServer*         [!output PLUGIN_CLASS]::EngineServer(NULL);
IServerPluginHelpers*   [!output PLUGIN_CLASS]::PluginHelpers(NULL);
IGameEventManager2*     [!output PLUGIN_CLASS]::GameEvents(NULL);
IServerPluginCallbacks* [!output PLUGIN_CLASS]::VspCallbacks(NULL);
IPlayerInfoManager*     [!output PLUGIN_CLASS]::PlayerInfoManager(NULL);
ICvar*                  [!output PLUGIN_CLASS]::CVar(NULL);
CGlobalVars*            [!output PLUGIN_CLASS]::GlobalVars(NULL);


/////////////////////////////////////////////////////////////////////////
// Class BaseAccessor

// This is needed to register cvars / CON_COMMANDs

class BaseAccessor : public IConCommandBaseAccessor
{
  public:

	  bool RegisterConCommandBase(ConCommandBase *pCommandBase)
	  {
		  // Always call META_REGCVAR instead
      // of going through the [!output PLUGIN_CLASS]::EngineServer

		  return META_REGCVAR(pCommandBase);
	  }
}
s_BaseAccessor;


///////////////////////////////////////////////////////////////////////////////
// Expose the Plugin

[!output PLUGIN_CLASS] [!output PLUGIN_CLASS]::Global;

PLUGIN_EXPOSE([!output PLUGIN_CLASS], [!output PLUGIN_CLASS]::Global);


/////////////////////////////////////////////////////////////////////////
// Load

bool [!output PLUGIN_CLASS]::Load(PluginId id, ISmmAPI* ismm, char* error, size_t maxlen, bool late)
{
	PLUGIN_SAVEVARS();

  // Get the current Enginefactory interfaces

	GET_V_IFACE_CURRENT(GetEngineFactory, [!output PLUGIN_CLASS]::EngineServer, IVEngineServer, INTERFACEVERSION_VENGINESERVER);
	GET_V_IFACE_CURRENT(GetEngineFactory, [!output PLUGIN_CLASS]::GameEvents, IGameEventManager2, INTERFACEVERSION_GAMEEVENTSMANAGER2);
	GET_V_IFACE_CURRENT(GetEngineFactory, [!output PLUGIN_CLASS]::PluginHelpers, IServerPluginHelpers, INTERFACEVERSION_ISERVERPLUGINHELPERS);
	GET_V_IFACE_CURRENT(GetEngineFactory, [!output PLUGIN_CLASS]::CVar, ICvar, CVAR_INTERFACE_VERSION);

  // Get the current ServerFactory interfaces

	GET_V_IFACE_ANY(GetServerFactory, [!output PLUGIN_CLASS]::GameServer, IServerGameDLL, INTERFACEVERSION_SERVERGAMEDLL);
	GET_V_IFACE_ANY(GetServerFactory, [!output PLUGIN_CLASS]::GameClients, IServerGameClients, INTERFACEVERSION_SERVERGAMECLIENTS);
	GET_V_IFACE_ANY(GetServerFactory, [!output PLUGIN_CLASS]::PlayerInfoManager, IPlayerInfoManager, INTERFACEVERSION_PLAYERINFOMANAGER);

  // Get the current Globals interfaces

	[!output PLUGIN_CLASS]::GlobalVars = ismm->GetCGlobals();

	// Load the VSP listener. This is usually needed for IServerPluginHelpers

	if (([!output PLUGIN_CLASS]::VspCallbacks = ismm->GetVSPInfo(NULL)) == NULL)
	{
		ismm->AddListener(this, this);
		ismm->EnableVSPListener();
	}

  // Add the hooks

  AddHooks();

  ENGINE_CALL(LogPrint)("All hooks started!\n");

  // Register the base accessor

  #if SOURCE_ENGINE >= SE_ORANGEBOX
	
  g_pCVar = [!output PLUGIN_CLASS]::CVar;
	ConVar_Register(0, &s_BaseAccessor);

  #else
	
  ConCommandBaseMgr::OneTimeInit(&s_BaseAccessor);
  
  #endif

	return true;
}

/////////////////////////////////////////////////////////////////////////
// Unload

bool [!output PLUGIN_CLASS]::Unload(char *error, size_t maxlen)
{
  // Remove the hooks

  RemoveHooks();

	return true;
}

/////////////////////////////////////////////////////////////////////////
// OnVSPListening

void [!output PLUGIN_CLASS]::OnVSPListening(IServerPluginCallbacks* vsp_callbacks)
{
	[!output PLUGIN_CLASS]::VspCallbacks = vsp_callbacks;
}

/////////////////////////////////////////////////////////////////////////
// GetVersion

const char* [!output PLUGIN_CLASS]::GetVersion()
{
	return "1.0.0.0";
}

/////////////////////////////////////////////////////////////////////////
// GetDate

const char* [!output PLUGIN_CLASS]::GetDate()
{
	return __DATE__;
}

/////////////////////////////////////////////////////////////////////////
// GetLogTag

const char* [!output PLUGIN_CLASS]::GetLogTag()
{
	return "[!output PLUGIN_NAME]Log";
}

/////////////////////////////////////////////////////////////////////////
// GetLicense

const char* [!output PLUGIN_CLASS]::GetLicense()
{
	return "[!output PLUGIN_LICENSE]";
}

/////////////////////////////////////////////////////////////////////////
// GetAuthor

const char* [!output PLUGIN_CLASS]::GetAuthor()
{
	return "[!output PLUGIN_AUTHOR]";
}

/////////////////////////////////////////////////////////////////////////
// GetDescription

const char* [!output PLUGIN_CLASS]::GetDescription()
{
	return "[!output PLUGIN_DESC]";
}

/////////////////////////////////////////////////////////////////////////
// GetName

const char* [!output PLUGIN_CLASS]::GetName()
{
	return "[!output PLUGIN_NAME]";
}

/////////////////////////////////////////////////////////////////////////
// GetURL

const char* [!output PLUGIN_CLASS]::GetURL()
{
	return "[!output PLUGIN_URL]";
}
