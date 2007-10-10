/**
 * vim: set ts=4 :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2007 AlliedModders LLC and authors.
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

#ifndef _INCLUDE_METAMOD_SOURCE_PLUGIN_EXT_H_
#define _INCLUDE_METAMOD_SOURCE_PLUGIN_EXT_H_

#define SOURCE_ENGINE_UNKNOWN			0				/**< Could not determine the engine version */
#define SOURCE_ENGINE_ORIGINAL			1				/**< Original Source Engine (used by The Ship) */
#define SOURCE_ENGINE_EPISODEONE		2				/**< Episode 1 Source Engine (second major SDK) */
#define SOURCE_ENGINE_ORANGEBOX			3				/**< Orange Box Source Engine (third major SDK) */

#define METAMOD_PLAPI_VERSION			14				/**< Version of this header file */
#define METAMOD_PLAPI_NAME				"ISmmPlugin"	/**< Name of the plugin interface */

namespace SourceMM
{
	class ISmmPlugin;
}

typedef SourceMM::ISmmPlugin METAMOD_PLUGIN;

/**
 * @file ISmmPluginExt.h 	Definitions for extended plugin exposure syntax.
 * @brief Provides an alternate method for loading plugins, without needing to 
 * 		  include the default headers and all their Half-Life baggage.
 */

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

