#include "metamod.h"
#include "metamod_util.h"
#include <interface.h>
#include <eiface.h>
#include <iplayerinfo.h>
#include <assert.h>
#include <loader_bridge.h>
#include "provider/provider_ep2.h"

SH_DECL_HOOK1_void(ConCommand, Dispatch, SH_NOATTRIB, false, const CCommand &);

ConCommand *g_plugin_unload = NULL;
bool g_bIsTryingToUnload;

void InterceptPluginUnloads(const CCommand &args)
{
	g_bIsTryingToUnload = true;
}

void InterceptPluginUnloads_Post(const CCommand &args)
{
	g_bIsTryingToUnload = false;
}

class VspBridge : public IVspBridge
{
public:
	virtual bool Load(const vsp_bridge_info *info, char *error, size_t maxlength)
	{
		assert(!g_Metamod.IsLoadedAsGameDLL());

		CGlobalVars *pGlobals;
		IPlayerInfoManager *playerInfoManager;

		playerInfoManager = (IPlayerInfoManager *)info->gsFactory("PlayerInfoManager002", NULL);
		if (playerInfoManager == NULL)
		{
			UTIL_Format(error, maxlength, "Metamod:Source requires gameinfo.txt modification to load on this game");
			return false;
		}

		pGlobals = playerInfoManager->GetGlobalVars();

		char gamedll_iface[] = "ServerGameDLL000";
		for (unsigned int i = 5; i <= 50; i++)
		{
			gamedll_iface[15] = '0' + i;
			if ((server = (IServerGameDLL *)info->gsFactory(gamedll_iface, NULL)) != NULL)
			{
				g_Metamod.SetGameDLLInfo((CreateInterfaceFn)info->gsFactory, i);
				break;
			}
		}

		if (server == NULL)
		{
			UTIL_Format(error, maxlength, "Metamod:Source could not load (GameDLL version not compatible).");
			return false;
		}

		char gameclients_iface[] = "ServerGameClients000";
		for (unsigned int i = 3; i <= 4; i++)
		{
			gameclients_iface[19] = '0' + i;
			if ((gameclients = (IServerGameClients *)info->gsFactory(gameclients_iface, NULL)) == NULL)
				break;
		}

		if (!mm_DetectGameInformation())
		{
			UTIL_Format(error, maxlength, "Metamod:Source failed to detect game paths; cannot load.");
			return false;
		}

		mm_InitializeForLoad();
		mm_InitializeGlobals((CreateInterfaceFn)info->engineFactory,
							 (CreateInterfaceFn)info->engineFactory,
							 (CreateInterfaceFn)info->engineFactory,
							 pGlobals);
		mm_StartupMetamod(true);
		
		g_plugin_unload = icvar->FindCommand("plugin_unload");

		if (g_plugin_unload != NULL)
		{
			SH_ADD_HOOK_STATICFUNC(ConCommand, Dispatch, g_plugin_unload, InterceptPluginUnloads, false);
			SH_ADD_HOOK_STATICFUNC(ConCommand, Dispatch, g_plugin_unload, InterceptPluginUnloads_Post, true);
		}

		return true;
	}

	virtual void Unload()
	{
		if (g_bIsTryingToUnload)
		{
			Error("Metamod:Source cannot be unloaded from VSP mode.  Use \"meta unload\" to unload specific plugins.\n");
			return;
		}
		if (g_plugin_unload != NULL)
		{
			SH_REMOVE_HOOK_STATICFUNC(ConCommand, Dispatch, g_plugin_unload, InterceptPluginUnloads, false);
			SH_REMOVE_HOOK_STATICFUNC(ConCommand, Dispatch, g_plugin_unload, InterceptPluginUnloads_Post, true);
			g_plugin_unload = NULL;
		}
		mm_UnloadMetamod();
	}

	virtual const char *GetDescription()
	{
		return "Metamod:Source " SVN_FULL_VERSION;
	}
};

VspBridge mm16_vsp_bridge;

SMM_API IVspBridge *
GetVspBridge()
{
	return &mm16_vsp_bridge;
}

