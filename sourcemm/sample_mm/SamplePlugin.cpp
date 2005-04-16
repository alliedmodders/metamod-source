#include "SamplePlugin.h"

SamplePlugin g_SamplePlugin;

PLUGIN_EXPOSE(SamplePlugin, g_SamplePlugin);

bool SamplePlugin::Load(PluginId id, ISmmAPI *ismm, factories *list, char *error, size_t maxlen)
{
	PLUGIN_SAVEVARS();

	return true;
}

bool SamplePlugin::Unload(char *error, size_t maxlen)
{
	return true;
}
