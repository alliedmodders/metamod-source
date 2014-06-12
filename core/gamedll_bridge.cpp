/**
 * vim: set ts=4 sw=4 tw=99 noet :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2009 AlliedModders LLC and authors.
 * All rights reserved.
 * ======================================================
 *
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the authors be held liable for any damages arising from 
 * the use of this software.
 * 
 * Permission is granted to anyone to use this software for any purpose, 
 * including commercial applications, and to alter it and redistribute it 
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not 
 * claim that you wrote the original software. If you use this software in a 
 * product, an acknowledgment in the product documentation would be 
 * appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 * misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 */

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
		server = (IServerGameDLL *) info->isgd;
		g_Metamod.SetGameDLLInfo((CreateInterfaceFn) info->gsFactory,
			info->dllVersion,
			true);
		g_Metamod.SetVSPListener(info->vsp_listener_path);
		mm_InitializeGlobals((CreateInterfaceFn) info->engineFactory,
			(CreateInterfaceFn) info->physicsFactory,
			(CreateInterfaceFn) info->fsFactory,
			(CGlobalVars*) info->pGlobals);

		if (!mm_DetectGameInformation())
		{
			UTIL_Format(buffer, maxlength, "Metamod:Source failed to detect game paths; cannot load.");
			return false;
		}

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

