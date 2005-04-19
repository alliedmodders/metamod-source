/* ======== stub_mm ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_SAMPLEPLUGIN_H
#define _INCLUDE_SAMPLEPLUGIN_H

#include <ISmmPlugin.h>

class StubPlugin : public ISmmPlugin
{
public:
	bool Load(PluginId id, ISmmAPI *ismm, factories *list, char *error, size_t maxlen);
	bool Unload(char *error, size_t maxlen);
	bool Pause(char *error, size_t maxlen);
	bool Unpause(char *error, size_t maxlen);
	void AllPluginsLoaded();
public:
	const char *GetAuthor();
	const char *GetName();
	const char *GetDescription();
	const char *GetURL();
	const char *GetLicense();
	const char *GetVersion();
	const char *GetDate();
	const char *GetLogTag();
public:
	//Called on ServerActivate.  Same definition as server plugins
	void ServerActivate(edict_t *pEdictList, int edictCount, int clientMax);
private:
	IServerGameDLL *m_ServerDll;
	IServerGameDLL *m_ServerDll_CC;
};

extern StubPlugin g_StubPlugin;

PLUGIN_GLOBALVARS();

#endif //_INCLUDE_SAMPLEPLUGIN_H
