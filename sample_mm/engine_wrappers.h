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

#if !defined ORANGEBOX_BUILD
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
#endif

#endif //_INCLUDE_SOURCE_ENGINE_WRAPPERS_
