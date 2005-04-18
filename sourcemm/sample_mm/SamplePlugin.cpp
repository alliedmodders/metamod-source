#include <oslink.h>
#include "SamplePlugin.h"

SamplePlugin g_SamplePlugin;

PLUGIN_EXPOSE(SamplePlugin, g_SamplePlugin);

SH_DECL_HOOK6(IServerGameDLL, LevelInit, SH_NOATTRIB, 0, bool, char const *, char const *, char const *, char const *, bool, bool);

bool LevelInit_handler( char const *pMapName, char const *pMapEntities, char const *pOldLevel, char const *pLandmarkName, bool loadGame, bool background );

bool SamplePlugin::Load(PluginId id, ISmmAPI *ismm, factories *list, char *error, size_t maxlen)
{
	PLUGIN_SAVEVARS();

	IServerGameDLL *isgd = (IServerGameDLL *)((ismm->serverFactory())(INTERFACEVERSION_SERVERGAMEDLL, NULL));

	if (!isgd)
	{
		snprintf(error, maxlen, "Could not find interface %s", INTERFACEVERSION_SERVERGAMEDLL);
		return false;
	}

	SH_ADD_HOOK_STATICFUNC(IServerGameDLL, LevelInit, isgd, LevelInit_handler, false);

	return true;
}

bool SamplePlugin::Unload(char *error, size_t maxlen)
{
	return true;
}

void SamplePlugin::AllPluginsLoaded()
{
	//we don't really care
}

bool LevelInit_handler( char const *pMapName, char const *pMapEntities, char const *pOldLevel, char const *pLandmarkName, bool loadGame, bool background )
{
	FILE *fp = fopen("c:\\dump.txt", "at");
	if (!fp)
		RETURN_META_VALUE(MRES_IGNORED, false);

	fprintf(fp, "Map name: %s (old level: %s)\n", pMapName?pMapName:"", pOldLevel?pOldLevel:"");

	fclose(fp);

	RETURN_META_VALUE(MRES_IGNORED, false);
}
