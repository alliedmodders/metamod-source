/* ======== stub_mm ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#include <oslink.h>
#include "stub_mm.h"

StubPlugin g_StubPlugin;

PLUGIN_EXPOSE(SamplePlugin, g_StubPlugin);

//This has all of the necessary hook declarations.  Read it!
#include "meta_hooks.h"

void ServerActivate_handler(edict_t *pEdictList, int edictCount, int clientMax)
{
	META_LOG(g_PLAPI, "ServerActivate() called: edictCount=%d, clientMax=%d", edictCount, clientMax);
	RETURN_META(MRES_IGNORED);
}

bool StubPlugin::Load(PluginId id, ISmmAPI *ismm, factories *list, char *error, size_t maxlen)
{
	PLUGIN_SAVEVARS();

	m_ServerDll = (IServerGameDLL *)((ismm->serverFactory())(INTERFACEVERSION_SERVERGAMEDLL, NULL));

	if (!m_ServerDll)
	{
		snprintf(error, maxlen, "Could not find interface %s", INTERFACEVERSION_SERVERGAMEDLL);
		return false;
	}

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
