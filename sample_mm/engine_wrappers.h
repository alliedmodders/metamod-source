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

/**
 * For non-OrangeBox builds, we have to make wrappers.
 */
#if defined ENGINE_ORIGINAL

/**
 * MM:S 1.4.x needs older API calls.
 */
#if !defined METAMOD_PLAPI_VERSION
#define GetEngineFactory engineFactory
#define GetServerFactory serverFactory
#define MM_Format snprintf
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
#define ENGINE_CALL(func) SH_CALL(m_EngineCC, func)

#elif defined ENGINE_ORANGEBOX

#define ENGINE_CALL(func) SH_CALL(engine, func)
#define MM_Format g_SMAPI->Format

#endif

#endif //_INCLUDE_SOURCE_ENGINE_WRAPPERS_

