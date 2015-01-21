/* ======== SourceMM ========
* Copyright (C) 2004-2008 Metamod:Source Development Team
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

#define PLAPI_VERSION	11
#define PLAPI_NAME		"ISmmPlugin"

class ISmmAPI;

// Interface return status, binary-compatible with HL2SDK's IFACE_OK and IFACE_FAILED.
enum 
{
	META_IFACE_OK = 0,
	META_IFACE_FAILED
};

class ISmmPlugin
{
public:
	virtual int GetApiVersion() { return PLAPI_VERSION; }
	virtual ~ISmmPlugin() { }
public:
	/**
	 * @brief Called on plugin load.
	 * NOTE - As of API 7, this is called as DLLInit() executes - after the parameters are known, but before 
	 *  the original GameDLL function is called.  Therefore, you cannot hook it, but you don't need to - 
	 *  Load() is basically your hook.
	 * NOTE - As of API 7, you can override factories before the engine and gamedll exchange them.
	 *  However, take care to note that if your plugin is unloaded, and the gamedll/engine have cached
	 *  an interface you've passed, something will definitely crash.  Be careful.
	 *
	 * @param id	Internal id of plugin.  Saved globally by PLUGIN_SAVEVARS()
	 * @param ismm	External API for SourceMM.  Saved globally by PLUGIN_SAVEVARS()
	 * @param list	Contains a list of factories.  Hook a factory call by setting one equal to your own function.
	 * @param late	Set to true if your plugin was loaded late (not at server load).
	 * @param error Error message buffer
	 * @param maxlen Size of error message buffer
	 * @return		True if successful, return false to reject the load.
	 */
	virtual bool Load(PluginId id, ISmmAPI *ismm, char *error, size_t maxlength, bool late) =0;

	/**
	 * @brief Called when your plugin is "queried".  This is useful for rejecting a loaded
	 *  state.  For example, if your plugin wants to stop operating, it can simply return
	 *  false and copy an error message.  This will notify other plugins or MM:S of something
	 *  bad that happened.  NOTE - MM:S will not cache the return state, so if you return false,
	 *  your plugin will not actually be paused or unloaded.  This callback will be called when:
	 *  - Another plugin requests it
	 *  - Someone types "meta list", it will show up as "REFUSED"
	 *  - When Metamod need to re-check the plugin's status
	 *  - If the plugin does something like overload a factory, Metamod will make sure the Query() returns true
	 *    before calling it.
	 *  Also note that this query will only override Metamod when the plugin is running and not paused.
	 *
	 * @param error		Buffer for error message.  This can be NULL!
	 * @param maxlen	Maximum length of error buffer.
	 * @return			Status code - true for okay, false for badness.
	 */
	virtual bool QueryRunning(char *error, size_t maxlen)
	{
		return true;
	}

	/** 
	 * @brief Called on plugin unload.  You can return false if you know your plugin
	 *  is not capable of restoring critical states it modifies.
	 *
	 * @param error		Error message buffer
	 * @param maxlen	Size of error message buffer
	 * @return			True on success, return false to request no unload.
	 */
	virtual bool Unload(char *error, size_t maxlen)
	{
		return true;
	}

	/** @brief Called on plugin pause.
	 *
	 * @param	error Error message buffer
	 * @param	maxlen Size of error message buffer
	 * @return	True on success, return false to request no pause.
	 */
	virtual bool Pause(char *error, size_t maxlen)
	{
		return true;
	}

	/** @brief Called on plugin unpause.
	 *
	 * @param	error Error message buffer
	 * @param	maxlen Size of error message buffer
	 * @return	True on success, return false to request no unpause.
	 */
	virtual bool Unpause(char *error, size_t maxlen)
	{
		return true;
	}
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
	 * This is useful for knowing when it's safe to request things another plugin might have.
	 * NOTE for API 7 - This is called after DLLInit(). 
	 */
	virtual void AllPluginsLoaded()
	{
	}
};

/**
 * @brief Added in 1.1 so plugins could listen to specific events
 */
class IMetamodListener
{
public:
	virtual ~IMetamodListener() { }
public:
	/**
	 * @brief Called when a plugin is loaded.
	 *
	 * @param id		Id of the plugin.
	 */
	virtual void OnPluginLoad(PluginId id) { }

	/**
	 * @brief Called when a plugin is unloaded.
	 *
	 * @param id		Id of the plugin.
	 */
	virtual void OnPluginUnload(PluginId id) { }

	/**
	 * @brief Called when a plugin is paused.
	 *
	 * @param id		Id of the plugin.
	 */
	virtual void OnPluginPause(PluginId id) { }

	/**
	 * @brief Called when a plugin is unpaused.
	 *
	 * @param id		Id of the plugin.
	 */
	virtual void OnPluginUnpause(PluginId id) { }

	/**
	 * @brief Called when the level is loaded (after GameInit, before ServerActivate).
	 *
	 * To override this, hook IServerGameDLL::LevelInit().
	 *
	 * @param pMapName		Name of the map.
	 * @param pMapEntities	Lump string of the map entities, in KeyValue format.
	 * @param pOldLevel		Unknown.
	 * @param pLandmarkName	Unknown.
	 * @param loadGame		Unknown.
	 * @param background	Unknown.
	 */
	virtual void OnLevelInit(char const *pMapName, char const *pMapEntities, char const *pOldLevel, char const *pLandmarkName, bool loadGame, bool background) { }

	/**
	 * @brief Called when the level is shut down.  May be called more than once.
	 */
	virtual void OnLevelShutdown() { }

	/**
	 * @brief Called when engineFactory() is used through Metamod:Source's wrapper.
	 * This can be used to provide interfaces to other plugins or the GameDLL.
	 *
	 * If ret is passed, you should fill it with IFACE_OK or IFACE_FAILED.
	 *
	 * @param iface			Interface string.
	 * @param ret			Optional pointer to store return code.
	 * @return				Generic pointer to the interface, or NULL if not found.
	 */
	virtual void *OnEngineQuery(const char *iface, int *ret)
	{
		if (ret)
		{
			*ret = IFACE_FAILED;
		}

		return NULL; 
	}

	/**
	 * @brief Called when the physics factory is used through Metamod:Source's wrapper.
	 * This can be used to provide interfaces to other plugins.
	 *
	 * If ret is passed, you should fill it with IFACE_OK or IFACE_FAILED.
	 *
	 * @param iface			Interface string.
	 * @param ret			Optional pointer to store return code.
	 * @return				Generic pointer to the interface, or NULL if not found.
	 */
	virtual void *OnPhysicsQuery(const char *iface, int *ret)
	{
		if (ret)
		{
			*ret = IFACE_FAILED;
		}

		return NULL; 
	}

	/**
	 * @brief Called when the filesystem factory is used through Metamod:Source's wrapper.
	 * This can be used to provide interfaces to other plugins.
	 *
	 * If ret is passed, you should fill it with IFACE_OK or IFACE_FAILED.
	 *
	 * @param iface			Interface string.
	 * @param ret			Optional pointer to store return code.
	 * @return				Generic pointer to the interface, or NULL if not found.
	 */
	virtual void *OnFileSystemQuery(const char *iface, int *ret)
	{
		if (ret)
		{
			*ret = IFACE_FAILED;
		}
		
		return NULL; 
	}

	/**
	 * @brief Called when the server DLL's factory is used through Metamod:Source's wrapper.
	 * This can be used to provide interfaces to other plugins.
	 *
	 * If ret is passed, you should fill it with IFACE_OK or IFACE_FAILED.
	 *
	 * @param iface			Interface string.
	 * @param ret			Optional pointer to store return code.
	 * @return				Generic pointer to the interface, or NULL if not found.
	 */
	virtual void *OnGameDLLQuery(const char *iface, int *ret)
	{
		if (ret)
		{
			*ret = IFACE_FAILED;
		}

		return NULL; 
	}

	/**
	 * @brief Called when Metamod's own factory is invoked.  
	 * This can be used to provide interfaces to other plugins.
	 *
	 * If ret is passed, you should fill it with IFACE_OK or IFACE_FAILED.
	 *
	 * @param iface			Interface string.
	 * @param ret			Optional pointer to store return code.
	 * @return				Generic pointer to the interface, or NULL if not found.
	 */
	virtual void *OnMetamodQuery(const char *iface, int *ret)
	{
		if (ret)
		{
			*ret = IFACE_FAILED;
		}

		return NULL; 	
	}

	/**
	 * @brief Called when Metamod:Source acquires a valid IServerPluginCallbacks 
	 * pointer to be used for hooking by plugins.
	 *
	 * This will only be called after a call to ISmmAPI::EnableVSPListener().
	 * If called before GameInit, this callback will occur before LevelInit.
	 * Otherwise, it will be called on the first call after that.
	 *
	 * This callback is provided to all plugins regardless of which (or how many)
	 * called EnableVSPListener(), but only if at least one did in fact enable it.
	 *
	 * This callback is only available for plugins using API v1:5 (SourceMM 1.4+).
	 *
	 * @param iface			Interface pointer.  If NULL, then the VSP listening construct
	 *						failed to initialize and is not available.
	 */
	virtual void OnVSPListening(IServerPluginCallbacks *iface)
	{
	}

	/**
	 * @brief Called when Metamod:Source knows that a specific ConCommandBase is 
	 * about to be unregistered. This is only called for ConCommandBases 
	 * registered by Metamod:Source plugins.
	 *
	 * This is only invoked on Metamod:Source 1.8 or higher (PLAPI_VERSION >= 11).
	 *
	 * @param plugin        Plugin owning the ConCommandBase.
	 * @param base          ConCommandBase that is being unlinked.
	 */
	virtual void OnUnlinkConCommandBase(PluginId id, ConCommandBase *base)
	{
	}
};

#define PL_EXPOSURE		CreateInterface
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
	g_SHPtr = static_cast<SourceHook::ISourceHook *>(ismm->MetaFactory(MMIFACE_SOURCEHOOK, NULL, NULL)); \
	g_PLAPI = static_cast<ISmmPlugin *>(this); \
	g_PLID = id;

#define META_LOG				g_SMAPI->LogMsg
#define META_REGCMD(name)		g_SMAPI->RegisterConCmdBase(g_PLAPI, name##_command)
#define META_REGCVAR(var)		g_SMAPI->RegisterConCmdBase(g_PLAPI, var)
#define META_UNREGCMD(name)		g_SMAPI->UnregisterConCmdBase(g_PLAPI, name##_command)
#define META_UNREGCVAR(var)		g_SMAPI->UnregisterConCmdBase(g_PLAPI, var)
#define	META_CONPRINT			g_SMAPI->ConPrint
#define META_CONPRINTF			g_SMAPI->ConPrintf

/* Probably should use this up above someday */
#define CONCMD_VARNAME(name) name##_command

#if !defined SMM_API
#if defined __WIN32__ || defined _WIN32 || defined WIN32
	#define SMM_API extern "C" __declspec(dllexport)
#elif defined __GNUC__
	#if (__GNUC__ == 4) && (__GNUC_MINOR__ >= 1)
		#define SMM_API extern "C" __attribute__ ((visibility("default")))	
	#else
		#define SMM_API	extern "C" 
	#endif	
#endif
#endif //!defined SMM_API

/**
 * @brief Macro for automatically getting a current or newer Valve interface.
 *
 * @param v_factory		Factory method to use from ISmmAPI (such as engineFactory).
 * @param v_var			Variable name to store into.
 * @param v_type		Interface type (do not include the pointer/asterisk).
 * @param v_name		Interface name.
 */
#define GET_V_IFACE_CURRENT(v_factory, v_var, v_type, v_name) \
	v_var = (v_type *)ismm->VInterfaceMatch(ismm->v_factory(), v_name); \
	if (!v_var) \
	{ \
		if (error && maxlen) \
		{ \
			snprintf(error, maxlen, "Could not find interface: %s", v_name); \
		} \
		return false; \
	}

 /**
  * @brief Same as GET_V_IFACE, except searches for any.
  *
  * @param v_factory	Factory method to use from ISmmAPI (such as engineFactory).
  * @param v_var		Variable name to store into.
  * @param v_type		Interface type (do not include the pointer/asterisk).
  * @param v_name		Interface name.
  */
#define GET_V_IFACE_ANY(v_factory, v_var, v_type, v_name) \
	v_var = (v_type *)ismm->VInterfaceMatch(ismm->v_factory(), v_name, 0); \
	if (!v_var) \
	{ \
		if (error && maxlen) \
		{ \
			snprintf(error, maxlen, "Could not find interface: %s", v_name); \
		} \
		return false; \
	}

#endif //_INCLUDE_ISMM_PLUGIN_H
