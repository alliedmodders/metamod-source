#include <assert.h>
#include "sourcemm.h"
#include <loader_bridge.h>

SH_DECL_HOOK0_void(ConCommand, Dispatch, SH_NOATTRIB, false);

ConCommand *g_plugin_unload = NULL;
bool g_bIsTryingToUnload;
bool g_bIsBridgedAsVsp;

void InterceptPluginUnloads()
{
	g_bIsTryingToUnload = true;
}

void InterceptPluginUnloads_Post()
{
	g_bIsTryingToUnload = false;
}

class VspBridge : public IVspBridge
{
	virtual bool Load(const vsp_bridge_info *info, char *error, size_t maxlength)
	{
		assert(!g_GameDll.loaded && !g_bIsBridgedAsVsp);

		CreateInterfaceFn engineFactory = (CreateInterfaceFn)info->engineFactory;
		CreateInterfaceFn gsFactory = (CreateInterfaceFn)info->gsFactory;

		if (!AlternatelyLoadMetamod(engineFactory, gsFactory))
			return false;

		ConCommandBase *pBase = g_Engine.icvar->GetCommands();
		while (pBase != NULL)
		{
			if (pBase->IsCommand() && strcmp(pBase->GetName(), "plugin_unload") == 0)
			{
				g_plugin_unload = (ConCommand *)pBase;
				break;
			}
			pBase = const_cast<ConCommandBase *>(pBase->GetNext());
		}

		if (g_plugin_unload != NULL)
		{
			SH_ADD_HOOK_STATICFUNC(ConCommand, Dispatch, g_plugin_unload, InterceptPluginUnloads, false);
			SH_ADD_HOOK_STATICFUNC(ConCommand, Dispatch, g_plugin_unload, InterceptPluginUnloads_Post, true);
		}

		extern ConVar metamod_version;
		char buffer[255];

		UTIL_Format(buffer, sizeof(buffer), "%sV", metamod_version.GetString());
		metamod_version.SetValue(buffer);

		g_bIsBridgedAsVsp = true;
		g_pRealVspCallbacks = info->vsp_callbacks;

		g_PluginMngr.SetVSPAsLoaded();

		return true;
	}

	virtual void Unload()
	{
	}

	virtual const char *GetDescription()
	{
		return "Metamod:Source " SVN_FULL_VERSION;
	}
};

VspBridge mm14_vsp_bridge;

SMM_API IVspBridge *
GetVspBridge()
{
	return &mm14_vsp_bridge;
}
