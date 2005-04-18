#ifndef _INCLUDE_SAMPLEPLUGIN_H
#define _INCLUDE_SAMPLEPLUGIN_H

#include <ISmmPlugin.h>

class SamplePlugin : public ISmmPlugin
{
public:
	bool Load(PluginId id, ISmmAPI *ismm, factories *list, char *error, size_t maxlen);
	bool Unload(char *error, size_t maxlen);
	void AllPluginsLoaded();
	bool Pause(char *error, size_t maxlen)
	{
		return true;
	}
	bool Unpause(char *error, size_t maxlen)
	{
		return true;
	}
public:
	int GetApiVersion() { return PLAPI_VERSION; }
public:
	const char *GetAuthor()
	{
		return "BAILOPAN";
	}
	const char *GetName()
	{
		return "Sample Plugin";
	}
	const char *GetDescription()
	{
		return "Sample plugin that hooks basic things";
	}
	const char *GetURL()
	{
		return "http://www.sourcemm.net/";
	}
	const char *GetLicense()
	{
		return "zlib/libpng";
	}
	const char *GetVersion()
	{
		return "1.00";
	}
	const char *GetDate()
	{
		return __DATE__;
	}
	const char *GetLogTag()
	{
		return "SAMPLE";
	}
};

extern SamplePlugin g_SamplePlugin;
PLUGIN_GLOBALVARS();

#endif //_INCLUDE_SAMPLEPLUGIN_H
