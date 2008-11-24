/**
 * vim: set ts=4 :
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
 *
 * Version: $Id$
 */

#ifndef _INCLUDE_SOURCE_ENGINE_WRAPPERS_
#define _INCLUDE_SOURCE_ENGINE_WRAPPERS_

#include <eiface.h>

extern IVEngineServer *engine;
extern CGlobalVars *gpGlobals;

/**
 * For non-OrangeBox builds, we have to make wrappers.
 */
#if SOURCE_ENGINE == SE_EPISODEONE

/**
 * MM:S 1.4.x needs older API calls.
 */
#if !defined METAMOD_PLAPI_VERSION
#define GetEngineFactory engineFactory
#define GetServerFactory serverFactory
#define MM_Format snprintf
#define	GetCGlobals	pGlobals
#else
#error "Metamod:Source 1.6 is not supported on the old engine."
#endif

/**
 * Wrap the CCommand class so our code looks the same for both engines.
 */
class CCommand
{
public:
	const char *ArgS()
	{
		return engine->Cmd_Args();
	}
	int ArgC()
	{
		return engine->Cmd_Argc();
	}

	const char *Arg(int index)
	{
		return engine->Cmd_Argv(index);
	}
};

#define CVAR_INTERFACE_VERSION				VENGINE_CVAR_INTERFACE_VERSION
#define ENGINE_CALL(func)					SH_CALL(m_EngineCC, func)

#elif SOURCE_ENGINE >= SE_ORANGEBOX

#define ENGINE_CALL(func)					SH_CALL(engine, func)
#define MM_Format							g_SMAPI->Format

#endif

/**
 * Left 4 Dead engine removed these from IVEngineServer.
 */
#if SOURCE_ENGINE >= SE_LEFT4DEAD

inline int IndexOfEdict(const edict_t *pEdict)
{
	return (int)(pEdict - gpGlobals->baseEdict);
}
inline edict_t *PEntityOfEntIndex(int iEntIndex)
{
	if (iEntIndex >= 0 && iEntIndex < gpGlobals->maxEntities)
	{
		return (edict_t *)(gpGlobals->baseEdict + iEntIndex);
	}
	return NULL;
}

#else

inline int IndexOfEdict(const edict_t *pEdict)
{
	return engine->IndexOfEdict(pEdict);
}
inline edict_t *PEntityOfEntIndex(int iEntIndex)
{
	return engine->PEntityOfEntIndex(iEntIndex);
}

#endif

#endif //_INCLUDE_SOURCE_ENGINE_WRAPPERS_

