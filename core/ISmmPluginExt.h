/*
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

#ifndef _INCLUDE_METAMOD_SOURCE_PLUGIN_EXT_H_
#define _INCLUDE_METAMOD_SOURCE_PLUGIN_EXT_H_

#include <stddef.h>

/**
 * @file ISmmPluginExt.h Provides an alternate method for loading plugins, 
 * without needing to include the default headers and all their Half-Life baggage.
 *
 * @brief Definitions for extended plugin loading.
 */

#define SOURCE_ENGINE_UNKNOWN			0				/**< Could not determine the engine version */
#define SOURCE_ENGINE_ORIGINAL			1				/**< Original Source Engine (used by The Ship) */
#define SOURCE_ENGINE_EPISODEONE		2				/**< Episode 1 Source Engine (second major SDK) */
#define SOURCE_ENGINE_ORANGEBOX			3				/**< Orange Box Source Engine (third major SDK) */
#define SOURCE_ENGINE_LEFT4DEAD			4				/**< Left 4 Dead */
#define SOURCE_ENGINE_DARKMESSIAH		5				/**< Dark Messiah Multiplayer (based on original engine) */
#define SOURCE_ENGINE_ORANGEBOXVALVE_DEPRECATED	6				/**< Orange Box Source Engine for Valve games (TF2/DOD:S/HL2DM) */
#define SOURCE_ENGINE_LEFT4DEAD2		7				/**< Left 4 Dead 2 */
#define SOURCE_ENGINE_ALIENSWARM		8				/**< Alien Swarm */
#define SOURCE_ENGINE_BLOODYGOODTIME	9				/**< Bloody Good Time */
#define SOURCE_ENGINE_EYE				10				/**< E.Y.E Divine Cybermancy */
#define SOURCE_ENGINE_PORTAL2			11				/**< Portal 2 */
#define SOURCE_ENGINE_CSGO				12				/**< Counter-Strike: Global Offensive */
#define SOURCE_ENGINE_CSS				13				/**< Counter-Strike: Source (sometimes older version of Orange Box Valve) */
#define SOURCE_ENGINE_DOTA				14				/**< Dota 2 */
#define SOURCE_ENGINE_HL2DM				15				/**< Half-Life 2 Deathmatch */
#define SOURCE_ENGINE_DODS				16				/**< Day of Defeat: Source */
#define SOURCE_ENGINE_TF2				17				/**< Team Fortress 2 */
#define SOURCE_ENGINE_NUCLEARDAWN		18				/**< Nuclear Dawn */
#define SOURCE_ENGINE_SDK2013			19				/**< Source SDK 2013 */
#define SOURCE_ENGINE_BLADE				20				/**< Blade Symphony */
#define SOURCE_ENGINE_INSURGENCY		21				/**< Insurgency */
#define SOURCE_ENGINE_CONTAGION			22				/**< Contagion */
#define SOURCE_ENGINE_BMS				23				/**< Black Mesa Multiplayer */
#define SOURCE_ENGINE_DOI				24				/**< Day of Infamy */

#define METAMOD_PLAPI_VERSION			15				/**< Version of this header file */
#define METAMOD_PLAPI_NAME				"ISmmPlugin"	/**< Name of the plugin interface */

namespace SourceMM
{
	class ISmmPlugin;
	class ISmmAPI;

	/**
	 * @brief Used to uniquely identify plugins.
	 */
	typedef int PluginId;

	#define METAMOD_FAIL_API_V1			7				/**< Minimum API version to detect for V1 */
	#define METAMOD_FAIL_API_V2			14				/**< Minimum API version to detect for V2 */

	/**
	 * Use this to instantiate a plugin that will always fail.
	 * This class definition works against major API versions 1 and 2.
	 */
	class ISmmFailPlugin
	{
	public:
		/**
		 * @brief You must return METAMOD_FAIL_API_V1 or METAMOD_FAIL_API_V2 here, 
		 * depending on which Metamod:Source version you detected.
		 */
		virtual int GetApiVersion() = 0;

		/**
		 * @brief Do not change.
		 */
		virtual ~ISmmFailPlugin()
		{
		}

		/**
		 * @brief Return false here -- fill in the error buffer appropriately.
		 *
		 * Do not ever return true.  If you do, MM:S will crash because the class layout is 
		 * incomplete against ISmmPlugin.
		 * 
		 * @param id			Ignore.
		 * @param ismm			Ignore.
		 * @param error			Error buffer (must be filled).
		 * @param maxlength		Maximum size of error buffer.
		 * @param late			Ignore.
		 * @return				Must return false.
		 */
		virtual bool Load(PluginId id, ISmmAPI *ismm, char *error, size_t maxlength, bool late) =0;
	};
}

typedef SourceMM::ISmmPlugin METAMOD_PLUGIN;

/**
 * @brief Contains version information.
 */
struct MetamodVersionInfo
{
	int api_major;			/**< ISmmAPI major version */
	int api_minor;			/**< ISmmAPI minor version */
	int sh_iface;			/**< SourceHook interface version */
	int sh_impl;			/**< SourceHook implementation version */
	int pl_min;				/**< Plugin API minimum version */
	int pl_max;				/**< Plugin API maximum version */
	int source_engine;		/**< Source Engine version (SOURCE_* constants) */
	const char *game_dir;	/**< Game directory name */

	/**
	 * @brief Returns the game folder.
	 *
	 * @return      Game folder, or NULL if not available on this version
	 *              of Metamod:Source.
	 */
	inline const char *GetGameDir() const
	{
		if (pl_max < 15)
			return NULL;
		return game_dir;
	}
};

/**
 * @brief Contains information about loading a plugin.
 */
struct MetamodLoaderInfo
{
	const char *pl_file;	/**< File path to the plugin being loaded. */
	const char *pl_path;	/**< Folder path containing the plugin. */
};

/**
 * @brief If a function of this type is exposed as "CreateInterface_MMS", then
 * Metamod:Source will attempt to call this function before calling 
 * CreateInterface.  If this function returns a valid ISmmPlugin pointer, then 
 * CreateInterface will not be called.
 *
 * This is useful for implementing a mini-loader plugin for multiple versions.
 *
 * @param mvi				MetamodVersionInfo structure.
 * @param mli				MetamodLoaderInfo structure.
 * @return					ISmmAPI pointer, or NULL if none.
 */
typedef METAMOD_PLUGIN *(*METAMOD_FN_LOAD)(const MetamodVersionInfo *mvi, 
										   const MetamodLoaderInfo *mli);

/**
 * @brief If a function of this type is exposed as "UnloadInterface_MMS", then
 * Metamod:Source will attempt to call this function after calling 
 * ISmmAPI::Unload(), and before closing the library.  This lets loader plugins 
 * clean up before exiting.
 *
 * Note: This function will be ignored unless CreateInterfce_MMS was exposed.
 * It may be called even if ISmmAPI::Unload() could not be called.
 */
typedef void (*METAMOD_FN_UNLOAD)();

/**
 * @brief Original type of load function.  CreateInterfaceFn from Valve.
 * 
 * Plugins will expose this as "CreateInterface".
 */
typedef void *(*METAMOD_FN_ORIG_LOAD)(const char *, int *);

#endif //INCLUDE_METAMOD_SOURCE_PLUGIN_EXT_H_

