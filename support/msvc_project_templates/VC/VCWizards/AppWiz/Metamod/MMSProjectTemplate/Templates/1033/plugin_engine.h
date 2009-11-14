///////////////////////////////////////////////////////////////////////////////
//
//  File:   [!output PLUGIN_CLASS]Engine.h
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

#ifndef _MMS_ENGINE_H
#define _MMS_ENGINE_H

#include <eiface.h>

/////////////////////////////////////////////////////////////////////////
// Class [!output PLUGIN_CLASS]

class [!output PLUGIN_CLASS];

/////////////////////////////////////////////////////////////////////////
// Episode One

#if SOURCE_ENGINE == SE_EPISODEONE && defined METAMOD_PLAPI_VERSION

#error "Metamod:Source 1.6 API is not supported on the old engine."

#endif

/////////////////////////////////////////////////////////////////////////
// Wrap some API calls for legacy MM:S.

#if !defined METAMOD_PLAPI_VERSION

#define GetEngineFactory engineFactory
#define GetServerFactory serverFactory

#define MM_Format snprintf
#define	GetCGlobals	pGlobals

#else

#define MM_Format g_SMAPI->Format

#endif

/////////////////////////////////////////////////////////////////////////
// Wrap the CCommand class so our code looks the same for both engines.

#if SOURCE_ENGINE <= SE_DARKMESSIAH

class CCommand
{
  public:

	  const char *ArgS()
	  {
		  return [!output PLUGIN_CLASS]::EngineServer->Cmd_Args();
	  }

	  int ArgC()
	  {
		  return [!output PLUGIN_CLASS]::EngineServer->Cmd_Argc();
	  }

	  const char *Arg(int index)
	  {
		  return [!output PLUGIN_CLASS]::EngineServer->Cmd_Argv(index);
	  }
};

#define CVAR_INTERFACE_VERSION	VENGINE_CVAR_INTERFACE_VERSION

#endif

/////////////////////////////////////////////////////////////////////////
// Left 4 Dead engine removed these from IVEngineServer

#if SOURCE_ENGINE >= SE_LEFT4DEAD

inline int IndexOfEdict(const edict_t* edict)
{
  return (int)(edict - [!output PLUGIN_CLASS]::GlobalVars->baseEdict);
}

inline edict_t* PEntityOfEntIndex(int entity_index)
{
	if (entity_index >= 0 && entity_index < [!output PLUGIN_CLASS]::GlobalVars->maxEntities)
	{
		return (edict_t *)([!output PLUGIN_CLASS]::GlobalVars->baseEdict + entity_index);
	}
	return NULL;
}

#else

inline int IndexOfEdict(const edict_t* edict)
{
	return [!output PLUGIN_CLASS]::EngineServer->IndexOfEdict(edict);
}

inline edict_t* PEntityOfEntIndex(int entity_index)
{
	return [!output PLUGIN_CLASS]::EngineServer->PEntityOfEntIndex(entity_index);
}

#endif

/////////////////////////////////////////////////////////////////////////
// Define snprintf

#if defined WIN32 && !defined snprintf
#define snprintf _snprintf
#endif

/////////////////////////////////////////////////////////////////////////
// Define ENGINE_CALL

#define ENGINE_CALL(func) SH_CALL([!output PLUGIN_CLASS]::EngineServer, &IVEngineServer::func)

#endif // _MMS_ENGINE_H
