/**
 * vim: set ts=4 sw=4 tw=99 noet :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2023 AlliedModders LLC and authors.
 * All rights reserved.
 * ======================================================
 *
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the authors be held liable for any damages arising from 
 * the use of this software.
 * 
 * Permission is granted to anyone to use this software for any purpose, 
 * including commercial applications, and to alter it and redistribute it 
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not 
 * claim that you wrote the original software. If you use this software in a 
 * product, an acknowledgment in the product documentation would be 
 * appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 * misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 */

#include <stdio.h>
#include <setjmp.h>
#include "../metamod_oslink.h"
#include <sourcehook.h>
#include <convar.h>
#include <eiface.h>
#include <tier0/icommandline.h>
#include "../metamod_util.h"
#include "provider_base.h"
#include "metamod_console.h"
#include <filesystem.h>
#include "metamod.h"
#include <tier1/KeyValues.h>


/* Imports */
#if SOURCE_ENGINE < SE_ORANGEBOX
#undef CommandLine
DLL_IMPORT ICommandLine *CommandLine();
#endif

void _ServerCommand();
/* Variables */
static List<ConCommandBase *> conbases_unreg;

ICvar *icvar = NULL;
IServerGameDLL *server = NULL;
IVEngineServer *engine = NULL;
IServerGameClients *gameclients = NULL;
CGlobalVars *gpGlobals = NULL;


bool BaseProvider::IsSourceEngineBuildCompatible(int build)
{
	return (build == SOURCE_ENGINE_ORIGINAL
			|| build == SOURCE_ENGINE_EPISODEONE);
}

int BaseProvider::TryServerGameDLL(const char *iface)
{
	if (strncmp(iface, "ServerGameDLL", 13) != 0)
	{
		return 0;
	}

	return atoi(&iface[13]);
}

bool BaseProvider::LogMessage(const char *buffer)
{
	if (!engine)
	{
		return false;
	}

	engine->LogPrint(buffer);

	return true;
}

void BaseProvider::DisplayError(const char *fmt, ...)
{
	va_list ap;
	char buffer[2048];

	va_start(ap, fmt);
	UTIL_FormatArgs(buffer, sizeof(buffer), fmt, ap);
	va_end(ap);

	Error("%s", buffer);
}

void BaseProvider::DisplayWarning(const char *fmt, ...)
{
	va_list ap;
	char buffer[2048];

	va_start(ap, fmt);
	UTIL_FormatArgs(buffer, sizeof(buffer), fmt, ap);
	va_end(ap);

	Warning("%s", buffer);
}

void BaseProvider::DisplayDevMsg(const char* fmt, ...)
{
	va_list ap;
	char buffer[2048];

	va_start(ap, fmt);
	UTIL_FormatArgs(buffer, sizeof(buffer), fmt, ap);
	va_end(ap);

	DevMsg("%s", buffer);
}
