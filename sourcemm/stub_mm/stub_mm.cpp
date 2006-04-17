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

#define	FIND_IFACE(func, assn_var, num_var, name, type) \
	do { \
		if ( (assn_var=(type)((ismm->func())(name, NULL))) != NULL ) { \
			num_var = 0; \
			break; \
		} \
		if (num_var >= 999) \
			break; \
	} while ( num_var=ismm->FormatIface(name, sizeof(name)-1) ); \
	if (!assn_var) { \
		if (error) \
			snprintf(error, maxlen, "Could not find interface %s", name); \
		return false; \
	}

void ServerActivate_handler(edict_t *pEdictList, int edictCount, int clientMax)
{
	META_LOG(g_PLAPI, "ServerActivate() called: edictCount=%d, clientMax=%d", edictCount, clientMax);
	RETURN_META(MRES_IGNORED);
}

bool StubPlugin::Load(PluginId id, ISmmAPI *ismm, char *error, size_t maxlen, bool late)
{
	PLUGIN_SAVEVARS();

	char iface_buffer[255];
	int num = 0;

	strcpy(iface_buffer, INTERFACEVERSION_SERVERGAMEDLL);
	FIND_IFACE(serverFactory, m_ServerDll, num, iface_buffer, IServerGameDLL *);

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

