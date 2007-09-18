/* ======== stub_mm ========
 * Copyright (C) 2004-2007 Metamod:Source Development Team
 * No warranties of any kind
 *
 * License: zlib/libpng
 *
 * Author(s): David "BAILOPAN" Anderson
 * ============================
 */

#include <oslink.h>
#include "stub_mm.h"

SH_DECL_HOOK3_void(IServerGameDLL, ServerActivate, SH_NOATTRIB, 0, edict_t *, int, int);

StubPlugin g_StubPlugin;

PLUGIN_EXPOSE(SamplePlugin, g_StubPlugin);

void ServerActivate_handler(edict_t *pEdictList, int edictCount, int clientMax)
{
	META_LOG(g_PLAPI, "ServerActivate() called: edictCount=%d, clientMax=%d", edictCount, clientMax);
	RETURN_META(MRES_IGNORED);
}

bool StubPlugin::Load(PluginId id, ISmmAPI *ismm, char *error, size_t maxlen, bool late)
{
	PLUGIN_SAVEVARS();

	GET_V_IFACE_ANY(serverFactory, m_ServerDll, IServerGameDLL, INTERFACEVERSION_SERVERGAMEDLL);

	SH_ADD_HOOK_STATICFUNC(IServerGameDLL, ServerActivate, m_ServerDll, ServerActivate_handler, true);

	return true;
}

bool StubPlugin::Unload(char *error, size_t maxlen)
{
	SH_REMOVE_HOOK_STATICFUNC(IServerGameDLL, ServerActivate, m_ServerDll, ServerActivate_handler, true);

	return true;
}

bool StubPlugin::Pause(char *error, size_t maxlen)
{
	return true;
}

bool StubPlugin::Unpause(char *error, size_t maxlen)
{
	return true;
}

void StubPlugin::AllPluginsLoaded()
{
	//This is an example of inter-plugin communication
	PluginId id;
	void *ptr = g_SMAPI->MetaFactory("SamplePlugin", NULL, &id);
	if (ptr)
	{
		META_LOG(g_PLAPI, "Found Sample Plugin[%d] at (%p)!", id, ptr);
	} else {
		META_LOG(g_PLAPI, "Did not find Sample Plugin!");
	}
}

const char *StubPlugin::GetAuthor()
{
	return "AUTHOR";
}

const char *StubPlugin::GetName()
{
	return "Stub Plugin";
}

const char *StubPlugin::GetDescription()
{
	return "Stub Plugin";
}

const char *StubPlugin::GetURL()
{
	return "http://www.mysite.com/";
}

const char *StubPlugin::GetLicense()
{
	return "zlib/libpng";
}

const char *StubPlugin::GetVersion()
{
	return "1.00";
}

const char *StubPlugin::GetDate()
{
	return __DATE__;
}

const char *StubPlugin::GetLogTag()
{
	return "STUB";
}
