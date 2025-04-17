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

#include <cstdio>
#include <cstring>
#include <cassert>
#include <cstddef>

#include "loader.h"
#include "serverplugin.h"
#include "gamedll.h"
#include "khook/memory.hpp"
#include "khook.hpp"

typedef enum
{
	PLUGIN_CONTINUE = 0,
	PLUGIN_OVERRIDE,
	PLUGIN_STOP,
} PLUGIN_RESULT;

typedef enum
{
    eQueryCvarValueStatus_ValueIntact=0,
    eQueryCvarValueStatus_CvarNotFound=1,
    eQueryCvarValueStatus_NotACvar=2,
    eQueryCvarValueStatus_CvarProtected=3
} EQueryCvarValueStatus;

typedef int QueryCvarCookie_t;
class CCommand;
class IServerPluginCallbacks;
struct edict_t;

class IRandomThings
{
public:
	virtual PLUGIN_RESULT ClientCommand(edict_t *pEntity, const CCommand& args)
	{
		return PLUGIN_CONTINUE;
	}
};

IVspBridge *vsp_bridge = NULL;

/**
 * The vtable must match the general layout for ISPC.  We modify the vtable
 * based on what we get back.
 */
class ServerPlugin
{
	char game_name[128];
	unsigned int vsp_version;
	bool load_allowed;
public:
	ServerPlugin()
	{
		load_allowed = false;
	}
	virtual bool Load(QueryValveInterface engineFactory, QueryValveInterface gsFactory)
	{
		if (!load_allowed)
			return false;

		load_allowed = false;

		/* Backend should already filled in if loaded as gamedll */
		if (gamedll_bridge == NULL)
		{
			mm_GetGameName(game_name, sizeof(game_name));

			mm_backend = mm_DetermineBackendS1(engineFactory, gsFactory, game_name);
			if (mm_backend == MMBackend_Mock)
				strcpy(game_name, "mock");
		}

		if (mm_backend == MMBackend_UNKNOWN)
		{
			mm_LogFatal("Could not detect engine version");
			return false;
		}
		
		void **this_vtable;
		this_vtable = (void **)*(void **)this;

		if (mm_backend != MMBackend_Episode1
			&& mm_backend != MMBackend_DarkMessiah)
		{
			/* We need to insert the right type of call into this vtable */
			void **vtable_src;
			IRandomThings sample;

			auto mfp_dest = KHook::__GetMFPVtableIndex__(&ServerPlugin::ClientCommand);
			auto mfp_src = KHook::__GetMFPVtableIndex__(&IRandomThings::ClientCommand);

			assert(mfp_dest != -1);
			assert(mfp_src != -1);

			vtable_src = (void **)*(void **)&sample;
			KHook::Memory::SetAccess(&this_vtable[mfp_dest],
									 sizeof(void*),
									 KHook::Memory::Flags::READ | KHook::Memory::Flags::WRITE | KHook::Memory::Flags::EXECUTE);
			this_vtable[mfp_dest] = vtable_src[mfp_src];
		}

		/* AS inserted ClientFullyConnect into vtable, so move entries up on older engines */
		if (mm_backend != MMBackend_AlienSwarm
			&& mm_backend != MMBackend_Portal2
			&& mm_backend != MMBackend_Blade
			&& mm_backend != MMBackend_Insurgency
			&& mm_backend != MMBackend_DOI
			&& mm_backend != MMBackend_CSGO
			&& mm_backend != MMBackend_MCV)
		{
			auto mfp_fconnect = KHook::__GetMFPVtableIndex__(&ServerPlugin::ClientFullyConnect);

			assert(mfp_fconnect != -1);

			/* Shifting ClientDisconnect through OnQueryCvarValueFinished up into slot for
			 * ClientFullyConnect (8 entries)
			 */
			KHook::Memory::SetAccess(&this_vtable[mfp_fconnect],
									 sizeof(void *) * 8,
									 KHook::Memory::Flags::READ | KHook::Memory::Flags::WRITE | KHook::Memory::Flags::EXECUTE);
			memmove(&this_vtable[mfp_fconnect],
					&this_vtable[mfp_fconnect + 1],
					sizeof(void *) * 8);
		}

		char error[255];
		if (gamedll_bridge == NULL)
		{
			if (!mm_LoadMetamodLibrary(mm_backend, error, sizeof(error)))
			{
				mm_LogFatal("Detected engine %d but could not load: %s", mm_backend, error);
				return false;
			}
		}

		typedef IVspBridge *(*GetVspBridge)();
		GetVspBridge get_bridge = (GetVspBridge)mm_GetProcAddress("GetVspBridge");
		if (get_bridge == NULL)
		{
			if (gamedll_bridge == NULL)
			{
				mm_UnloadMetamodLibrary();
			}
			mm_LogFatal("Detected engine %d but could not find GetVspBridge callback", mm_backend);
			return false;
		}

		vsp_bridge = get_bridge();

		vsp_bridge_info info;

		info.engineFactory = engineFactory;
		info.gsFactory = gsFactory;
		info.vsp_callbacks = (IServerPluginCallbacks*)this;
		info.vsp_version = vsp_version;

		strcpy(error, "Unknown error");
		if (!vsp_bridge->Load(&info, error, sizeof(error)))
		{
			vsp_bridge = NULL;
			if (gamedll_bridge == NULL)
			{
				mm_UnloadMetamodLibrary();
			}
			mm_LogFatal("Unknown error loading Metamod for engine %d: %s", mm_backend, error);	
			return false;
		}

		return true;
	}
	virtual void Unload()
	{
		if (vsp_bridge == NULL)
			return;
		vsp_bridge->Unload();

		if (gamedll_bridge == NULL)
		{
			mm_UnloadMetamodLibrary();
		}
	}
	virtual void Pause()
	{
	}
	virtual void UnPause()
	{
	}
	virtual const char *GetPluginDescription()
	{
		if (vsp_bridge == NULL)
			return "Metamod:Source Loader Shim";
		return vsp_bridge->GetDescription();
	}
	virtual void LevelInit(char const *pMapName)
	{
	}
	virtual void ServerActivate(edict_t *pEdictList, int edictCount, int clientMax)
	{
	}
	virtual void GameFrame(bool simulating)
	{
	}
	virtual void LevelShutdown()
	{
	}
	virtual void ClientActive(edict_t *pEntity)
	{
	}
	virtual void ClientFullyConnect(edict_t *pEntity)
	{
	}
	virtual void ClientDisconnect(edict_t *pEntity)
	{
	}
	virtual void ClientPutInServer(edict_t *pEntity, char const *playername)
	{
	}
	virtual void SetCommandClient(int index)
	{
	}
	virtual void ClientSettingsChanged(edict_t *pEdict)
	{
	}
	virtual PLUGIN_RESULT ClientConnect(bool *bAllowConnect,
										edict_t *pEntity,
										const char *pszName,
										const char *pszAddress,
										char *reject,
										int maxrejectlen) 
	{
		return PLUGIN_CONTINUE;
	}
	virtual PLUGIN_RESULT ClientCommand(edict_t *pEntity)
	{
		return PLUGIN_CONTINUE;
	}
	virtual PLUGIN_RESULT NetworkIDValidated(const char *pszUserName, const char *pszNetworkID)
	{
		return PLUGIN_CONTINUE;
	}
	virtual void OnQueryCvarValueFinished(QueryCvarCookie_t iCookie,
										  edict_t *pPlayerEntity,
										  EQueryCvarValueStatus eStatus,
										  const char *pCvarName,
										  const char *pCvarValue)
	{
	}
	void PrepForLoad(unsigned int version)
	{
		vsp_version = version;
		load_allowed = true;
	}
};

ServerPlugin mm_vsp_callbacks;

void *mm_GetVspCallbacks(unsigned int version)
{
	if (vsp_bridge != NULL)
		return NULL;

	/* Only support versions 1 or 2 right now */
	if (version > 2)
		return NULL;

	mm_vsp_callbacks.PrepForLoad(version);

	return &mm_vsp_callbacks;
}

