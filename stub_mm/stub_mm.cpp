/**
 * vim: set ts=4 :
 * ======================================================
 * Metamod:Source Stub Plugin
 * Written by AlliedModders LLC.
 * ======================================================
 *
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the authors be held liable for any damages arising from 
 * the use of this software.
 *
 * This stub plugin is public domain.
 *
 * Version: $Id$
 */

#include <stdio.h>
#include "stub_mm.h"

SH_DECL_HOOK3_void(IServerGameDLL, ServerActivate, SH_NOATTRIB, 0, edict_t *, int, int);

StubPlugin g_StubPlugin;
IServerGameDLL *server = NULL;
int hidServerActivate = 0;

PLUGIN_EXPOSE(StubPlugin, g_StubPlugin);
bool StubPlugin::Load(PluginId id, ISmmAPI *ismm, char *error, size_t maxlen, bool late)
{
	PLUGIN_SAVEVARS();

	GET_V_IFACE_ANY(GetServerFactory, server, IServerGameDLL, INTERFACEVERSION_SERVERGAMEDLL);

	hidServerActivate = SH_ADD_HOOK(IServerGameDLL, ServerActivate, server, SH_STATIC(Hook_ServerActivate), true);

	return true;
}

bool StubPlugin::Unload(char *error, size_t maxlen)
{
	if (hidServerActivate != 0)
	{
		SH_REMOVE_HOOK_ID(hidServerActivate);
	}

	return true;
}

void Hook_ServerActivate(edict_t *pEdictList, int edictCount, int clientMax)
{
	META_LOG(g_PLAPI, "ServerActivate() called: edictCount = %d, clientMax = %d", edictCount, clientMax);
}

void StubPlugin::AllPluginsLoaded()
{
	/* This is where we'd do stuff that relies on the mod or other plugins 
	 * being initialized (for example, cvars added and events registered).
	 */
}

bool StubPlugin::Pause(char *error, size_t maxlen)
{
	return true;
}

bool StubPlugin::Unpause(char *error, size_t maxlen)
{
	return true;
}

const char *StubPlugin::GetLicense()
{
	return "Public Domain";
}

const char *StubPlugin::GetVersion()
{
	return "1.0.0.0";
}

const char *StubPlugin::GetDate()
{
	return __DATE__;
}

const char *StubPlugin::GetLogTag()
{
	return "STUB";
}

const char *StubPlugin::GetAuthor()
{
	return "AlliedModders LLC";
}

const char *StubPlugin::GetDescription()
{
	return "Sample empty plugin";
}

const char *StubPlugin::GetName()
{
	return "Stub Plugin";
}

const char *StubPlugin::GetURL()
{
	return "http://www.sourcemm.net/";
}
