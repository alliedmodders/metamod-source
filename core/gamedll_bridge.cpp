#include <assert.h>
#include "metamod.h"
#include "metamod_plugins.h"
#include "metamod_util.h"
#include <loader_bridge.h>
#include "provider/provider_ep2.h"

using namespace SourceMM;

class GameDllBridge : public IGameDllBridge
{
public:
	virtual bool DLLInit_Pre(const gamedll_bridge_info *info, char *buffer, size_t maxlength)
	{
		if (!mm_DetectGameInformation())
		{
			UTIL_Format(buffer, maxlength, "Metamod:Source failed to detect game paths; cannot load.");
			return false;
		}

		server = (IServerGameDLL *)info->isgd;
		g_Metamod.SetGameDLLInfo((CreateInterfaceFn)info->gsFactory,
								 info->dllVersion,
								 true);
		mm_InitializeGlobals((CreateInterfaceFn)info->engineFactory,
							 (CreateInterfaceFn)info->physicsFactory,
							 (CreateInterfaceFn)info->fsFactory,
							 (CGlobalVars*)info->pGlobals);
		mm_InitializeForLoad();
		mm_StartupMetamod(false);

		return true;
	}
	virtual void DLLInit_Post(int *isgdUnload)
	{
		SourceHook::MemFuncInfo mfi;

		mfi.isVirtual = false;
		SourceHook::GetFuncInfo(&IServerGameDLL::DLLShutdown, mfi);
		assert(mfi.isVirtual);
		assert(mfi.vtbloffs == 0);
		assert(mfi.thisptroffs == 0);
		*isgdUnload = mfi.vtblindex;

		g_PluginMngr.SetAllLoaded();
	}
	virtual void *QueryInterface(const char *iface, int *ret)
	{
		return g_Metamod.GetServerFactory(true)(iface, ret);
	}
	virtual void Unload()
	{
		mm_UnloadMetamod();
	}
};

GameDllBridge mm16_gamedll_bridge;

SMM_API IGameDllBridge *
GetGameDllBridge()
{
	return &mm16_gamedll_bridge;
}

