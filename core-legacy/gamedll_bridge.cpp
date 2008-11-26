/**
 * vim: set ts=4 :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2008 AlliedModders LLC and authors.
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
 *
 * Version: $Id$
 */

#include <assert.h>
#include "sourcemm.h"
#include "CPlugin.h"
#include <loader_bridge.h>

using namespace SourceMM;

#define	IFACE_MACRO(orig,nam) \
	CPluginManager::CPlugin *pl; \
	SourceHook::List<CPluginEventHandler>::iterator event; \
	IMetamodListener *api; \
	int mret = 0; \
	void *val = NULL; \
	for (PluginIter iter = g_PluginMngr._begin(); iter != g_PluginMngr._end(); iter++) { \
		pl = (*iter); \
		for (event=pl->m_Events.begin(); event!=pl->m_Events.end(); event++) { \
			api = (*event).event; \
			mret = IFACE_FAILED; \
			if ( (val=api->On##nam##Query(iface, &mret)) != NULL ) { \
				if (ret) *ret = mret; \
				return val; \
			} \
		} \
	} \
	return (orig)(iface, ret);


class GameDllBridge : public IGameDllBridge
{
public:
	virtual bool DLLInit_Pre(const gamedll_bridge_info *info, char *buffer, size_t maxlength)
	{
		assert(!g_GameDll.loaded && !g_bIsBridgedAsVsp);
		LoadAsGameDLL(info);
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
		/* We use this interface for responding to the meta client command */
		if (strncmp(iface, "ServerGameClients", 17) == 0)
		{
			void *ptr = (g_GameDll.factory)(iface, ret);
			g_GameDll.pGameClients = static_cast<IServerGameClients *>(ptr);
			return ptr;
		}

		/* If we got here, there's definitely a GameDLL */
		IFACE_MACRO(g_GameDll.factory, GameDLL);
	}
	virtual void Unload()
	{
		UnloadMetamod(true);
	}
};

GameDllBridge mm14_gamedll_bridge;

SMM_API IGameDllBridge *
GetGameDllBridge()
{
	return &mm14_gamedll_bridge;
}

void *ServerFactory(const char *name, int *code)
{
	return mm14_gamedll_bridge.QueryInterface(name, code);
}

/* Wrapper function.  This is called when the GameDLL thinks it's using
 * the engine's real engineFactory.
 */
void *EngineFactory(const char *iface, int *ret)
{
	IFACE_MACRO(g_Engine.engineFactory, Engine);
}

/* Wrapper function.  This is called when the GameDLL thinks it's using
 * the engine's real physicsFactory.
 */
void *PhysicsFactory(const char *iface, int *ret)
{
	IFACE_MACRO(g_Engine.physicsFactory, Physics);
}

/* Wrapper function.  This is called when the GameDLL thinks it's using
 * the engine's real fileSystemFactory.
 */
void *FileSystemFactory(const char *iface, int *ret)
{
	IFACE_MACRO(g_Engine.fileSystemFactory, FileSystem);
}


