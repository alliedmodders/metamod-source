/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_ISMM_PLUGIN_H
#define _INCLUDE_ISMM_PLUGIN_H

/**
 * @brief Plugin API interface
 * @file ISmmPlugin.h
 */

#include <interface.h>
#include <sourcehook/sourcehook.h>
#include "ISmmAPI.h"

#define PLAPI_VERSION	6
#define PLAPI_NAME		"ISmmPlugin"

struct factories
{
	CreateInterfaceFn engine;
	CreateInterfaceFn server;
	CreateInterfaceFn physics;
	CreateInterfaceFn fileSystem;
};

class ISmmAPI;
typedef int PluginId;

class ISmmPlugin
{
public:
	virtual int GetApiVersion() { return PLAPI_VERSION; }
public:
	/**
	 * @brief Called on plugin load.
	 *
	 * @param id Internal id of plugin.  Saved globally by PLUGIN_SAVEVARS()
	 * @param ismm External API for SourceMM.  Saved globally by PLUGIN_SAVEVARS()
	 * @param list Contains a list of factories.  Hook a factory call by setting one equal to your own function.
	 * @param error Error message buffer
	 * @param maxlen Size of error message buffer
	 * @return True on success, return false to request no load.
	 */
	virtual bool Load(PluginId id, ISmmAPI *ismm, factories *list, char *error, size_t maxlen) =0;

	/** @brief Called on plugin unload.
	 *
	 * @param error Error message buffer
	 * @param maxlen Size of error message buffer
	 * @return True on success, return false to request no unload.
	 */
	virtual bool Unload(char *error, size_t maxlen) =0;

	/** @brief Called on plugin pause.
	 *
	 * @param error Error message buffer
	 * @param maxlen Size of error message buffer
	 * @return True on success, return false to request no pause.
	 */
	virtual bool Pause(char *error, size_t maxlen) =0;

	/** @brief Called on plugin unpause.
	 *
	 * @param error Error message buffer
	 * @param maxlen Size of error message buffer
	 * @return True on success, return false to request no unpause.
	 */
	virtual bool Unpause(char *error, size_t maxlen) =0;
public:
	/** @brief Return author as string */
	virtual const char *GetAuthor() =0;

	/** @brief Return plugin name as string */
	virtual const char *GetName() =0;

	/** @brief Return a description as string */
	virtual const char *GetDescription() =0;

	/** @brief Return a URL as string */
	virtual const char *GetURL() =0;

	/** @brief Return quick license code as string */
	virtual const char *GetLicense() =0;

	/** @brief Return version as string */
	virtual const char *GetVersion() =0;

	/** @brief Return author as string */
	virtual const char *GetDate() =0;

	/** @brief Return author as string */
	virtual const char *GetLogTag() =0;
public:
	/**
	 * @brief Called when all plugins have been loaded - API version 4
         *
         * Ths is useful for knowing when it's safe to request things another plugin might have.
	 */
	virtual void AllPluginsLoaded() =0;
};

#define PL_EXPOSURE	CreateInterface
#define PL_EXPOSURE_C	"CreateInterface"

#define PLUGIN_EXPOSE(name, var) \
	ISmmAPI *g_SMAPI = NULL; \
	ISmmPlugin *g_PLAPI = NULL; \
	PluginId g_PLID = (PluginId)0; \
	SourceHook::ISourceHook *g_SHPtr = NULL; \
	SMM_API void *PL_EXPOSURE(const char *name, int *code) { \
		if (name && !strcmp(name, PLAPI_NAME)) { \
			return static_cast<void *>(&var); \
		} \
		return NULL; \
	}

#define PLUGIN_GLOBALVARS()	\
	extern SourceHook::ISourceHook *g_SHPtr; \
	extern ISmmAPI *g_SMAPI; \
	extern ISmmPlugin *g_PLAPI; \
	extern PluginId g_PLID; 

#define PLUGIN_SAVEVARS() \
	g_SMAPI = ismm; \
	g_SHPtr = ismm->SourceHook(); \
	g_PLAPI = static_cast<ISmmPlugin *>(this); \
	g_PLID = id;

#define FACTORY_RETURN(mres, value) \
	g_SMAPI->SetLastMetaReturn(mres); \
	return value;

#define META_LOG		g_SMAPI->LogMsg
#define META_REGCMD(name)		g_SMAPI->RegisterConCmdBase(g_PLAPI, name##_command)
#define META_REGCVAR(var)		g_SMAPI->RegisterConCmdBase(g_PLAPI, var)
#define META_UNREGCMD(name)		g_SMAPI->UnregisterConCmdBase(g_PLAPI, name##_command)
#define META_UNREGCVAR(var)		g_SMAPI->UnregisterConCmdBase(g_PLAPI, var)
#define	META_CONPRINT	g_SMAPI->ConPrint
#define META_CONPRINTF	g_SMAPI->ConPrintf

//probably should use this up above someday
#define CONCMD_VARNAME(name) name##_command

#if !defined SMM_API
#if defined __WIN32__ || defined _WIN32 || defined WIN32
	#define SMM_API extern "C" __declspec(dllexport)
#elif defined __GNUC__
	#define SMM_API	extern "C"
#endif
#endif //!defined SMM_API

#endif //_INCLUDE_ISMM_PLUGIN_H
