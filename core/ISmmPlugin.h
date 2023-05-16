/*
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

#ifndef _INCLUDE_ISMM_PLUGIN_H
#define _INCLUDE_ISMM_PLUGIN_H

/**
 * @brief Plugin API interface.
 * @file ISmmPlugin.h These are callbacks that plugins can implement without hooks.
 * The pure-virtual ISmmPlugin callbacks must be implemented for the load to load.
 */

#include <sourcehook.h>
#include <IPluginManager.h>
#include <ISmmAPI.h>
#include <ISmmPluginExt.h>

#ifndef META_NO_HL2SDK
#if META_IS_SOURCE2
#include <interfaces/interfaces.h>
#else
#include <tier1/interface.h>
#endif
#endif

class IServerPluginCallbacks;

// Interface return status, binary-compatible with HL2SDK's IFACE_OK and IFACE_FAILED.
enum 
{
	META_IFACE_OK = 0,
	META_IFACE_FAILED
};

namespace SourceMM
{
	class ISmmAPI;

	/**
	 * @brief Callbacks that a plugin must expose.
	 */
	class ISmmPlugin
	{
	public:
		/**
		 * @brief Called to request the plugin's API version.
		 *
		 * This is the first callback invoked, and always remains at the top 
		 * of the virtual table.  
		 *
		 * @return			Plugin API version.
		 */
		virtual int GetApiVersion() 
		{ 
			return METAMOD_PLAPI_VERSION; 
		}

		/**
		 * @brief Virtual destructor so GCC doesn't complain.
		 */
		virtual ~ISmmPlugin()
		{
		}

	public:
		/**
		 * @brief Called on plugin load.
		 *
		 * This is called as DLLInit() executes - after the parameters are 
		 * known, but before the original GameDLL function is called.  
		 * Therefore, you cannot hook it, but you don't need to - Load() is 
		 * basically your hook.  You can override factories before the engine 
		 * and gamedll exchange them.  However, take care to note that if your 
		 * plugin is unloaded, and the gamedll/engine have cached an interface 
		 * you've passed, something will definitely crash.  Be careful.
		 *
		 * @param id		Internal id of plugin.  Saved globally by PLUGIN_SAVEVARS()
		 * @param ismm		External API for SourceMM.  Saved globally by PLUGIN_SAVEVARS()
		 * @param error		Error message buffer
		 * @param maxlength	Size of error message buffer
		 * @param late		Set to true if your plugin was loaded late (not at server load).
		 * @return			True if successful, return false to reject the load.
		 */
		virtual bool Load(PluginId id, ISmmAPI *ismm, char *error, size_t maxlength, bool late) =0;

		/**
		 * @brief Called when all plugins have been loaded.  
		 *
		 * This is called after DLLInit(), and thus the mod has been mostly initialized. 
		 * It is also safe to assume that all other (automatically loaded) plugins are now 
		 * ready to start interacting, because they are all loaded.
		 */
		virtual void AllPluginsLoaded()
		{
		}

		/**
		 * @brief Called when your plugin is "queried".  
		 * 
		 * This is useful for rejecting a loaded state.  For example, if your 
		 * plugin wants to stop operating, it can simply return false and copy 
		 * an error message.  This will notify other plugins or MM:S of 
		 * something bad that happened.  MM:S will not cache the return state, 
		 * so if you return false,  your plugin will not actually be paused or 
		 * unloaded.  This callback will be called when:
		 *  - Another plugin requests it
		 *  - Someone types "meta list", it will show up as "REFUSED"
		 *  - When Metamod need to re-check the plugin's status
		 *  - If the plugin does something like overload a factory, Metamod 
		 *    will make sure the Query() returns true before calling it.
		 *  Also note that this query will only override Metamod when the 
		 *  plugin is running and not paused.
		 *
		 * @param error		Buffer for error message, or NULL if none.
		 * @param maxlen	Maximum length of error buffer.
		 * @return			Status code - true for okay, false for badness.
		 */
		virtual bool QueryRunning(char *error, size_t maxlen)
		{
			return true;
		}

		/** 
		 * @brief Called on plugin unload.  You can return false if you know 
		 * your plugin is not capable of restoring critical states it modifies.
		 *
		 * @param error		Error message buffer
		 * @param maxlen	Size of error message buffer
		 * @return			True on success, return false to request no unload.
		 */
		virtual bool Unload(char *error, size_t maxlen)
		{
			return true;
		}

		/** 
		 * @brief Called on plugin pause.
		 *
		 * @param	error Error message buffer
		 * @param	maxlen Size of error message buffer
		 * @return	True on success, return false to request no pause.
		 */
		virtual bool Pause(char *error, size_t maxlen)
		{
			return true;
		}

		/** 
		 * @brief Called on plugin unpause.
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
	};

	/**
	 * @brief Various events that Metamod can fire.
	 */
	class IMetamodListener
	{
	public:
		/**
		 * @brief Called when a plugin is loaded.
		 *
		 * @param id		Id of the plugin.
		 */
		virtual void OnPluginLoad(PluginId id)
		{
		}

		/**
		 * @brief Called when a plugin is unloaded.
		 *
		 * @param id		Id of the plugin.
		 */
		virtual void OnPluginUnload(PluginId id)
		{
		}
	
		/**
		 * @brief Called when a plugin is paused.
		 *
		 * @param id		Id of the plugin.
		 */
		virtual void OnPluginPause(PluginId id)
		{
		}
	
		/**
		 * @brief Called when a plugin is unpaused.
		 *
		 * @param id		Id of the plugin.
		 */
		virtual void OnPluginUnpause(PluginId id)
		{
		}
	
		/**
		 * @brief Called when the level is loaded (after GameInit, before 
		 * ServerActivate).
		 *
		 * To override this, hook IServerGameDLL::LevelInit().
		 *
		 * @param pMapName		Name of the map.
		 * @param pMapEntities	Lump string of the map entities, in KeyValues
		 * 						format.
		 * @param pOldLevel		Unknown.
		 * @param pLandmarkName	Unknown.
		 * @param loadGame		Unknown.
		 * @param background	Unknown.
		 */
		virtual void OnLevelInit(char const *pMapName, 
								 char const *pMapEntities, 
								 char const *pOldLevel, 
								 char const *pLandmarkName, 
								 bool loadGame, 
								 bool background) 
		{ 
		}

		/**
		 * @brief Called when the level is shut down.  May be called more than 
		 * once.
		 */
		virtual void OnLevelShutdown()
		{
		}

		/**
		 * @brief Called when engineFactory() is used through Metamod:Source's 
		 * wrapper.  This can be used to provide interfaces to other plugins or
		 * the GameDLL.
		 *
		 * If ret is passed, you should fill it with META_IFACE_OK or META_IFACE_FAILED.
		 *
		 * @param iface			Interface string.
		 * @param ret			Optional pointer to store return code.
		 * @return				Generic pointer to the interface, or NULL if 
		 * 						not found.
		 */
		virtual void *OnEngineQuery(const char *iface, int *ret)
		{
			if (ret)
			{
				*ret = META_IFACE_FAILED;
			}

			return NULL; 
		}

		/**
		 * @brief Called when the physics factory is used through 
		 * Metamod:Source's wrapper. This can be used to provide interfaces to 
		 * other plugins.
		 *
		 * If ret is passed, you should fill it with META_IFACE_OK or META_IFACE_FAILED.
		 *
		 * @param iface			Interface string.
		 * @param ret			Optional pointer to store return code.
		 * @return				Generic pointer to the interface, or NULL if 
		 * 						not found.
		 */
		virtual void *OnPhysicsQuery(const char *iface, int *ret)
		{
			if (ret)
			{
				*ret = META_IFACE_FAILED;
			}
	
			return NULL; 
		}

		/**
		 * @brief Called when the filesystem factory is used through 
		 * Metamod:Source's wrapper.  This can be used to provide interfaces to 
		 * other plugins.
		 *
		 * If ret is passed, you should fill it with META_IFACE_OK or META_IFACE_FAILED.
		 *
		 * @param iface			Interface string.
		 * @param ret			Optional pointer to store return code.
		 * @return				Generic pointer to the interface, or NULL if not 
		 * 						found.
		 */
		virtual void *OnFileSystemQuery(const char *iface, int *ret)
		{
			if (ret)
			{
				*ret = META_IFACE_FAILED;
			}
			
			return NULL; 
		}

		/**
		 * @brief Called when the server DLL's factory is used through 
		 * Metamod:Source's wrapper.  This can be used to provide interfaces to 
		 * other plugins.
		 *
		 * If ret is passed, you should fill it with META_IFACE_OK or META_IFACE_FAILED.
		 *
		 * @param iface			Interface string.
		 * @param ret			Optional pointer to store return code.
		 * @return				Generic pointer to the interface, or NULL if not 
		 * 						found.
		 */
		virtual void *OnGameDLLQuery(const char *iface, int *ret)
		{
			if (ret)
			{
				*ret = META_IFACE_FAILED;
			}
	
			return NULL; 
		}

		/**
		 * @brief Called when Metamod's own factory is invoked.  
		 * This can be used to provide interfaces to other plugins.
		 *
		 * If ret is passed, you should fill it with META_IFACE_OK or META_IFACE_FAILED.
		 *
		 * @param iface			Interface string.
		 * @param ret			Optional pointer to store return code.
		 * @return				Generic pointer to the interface, or NULL if not 
		 * 						found.
		 */
		virtual void *OnMetamodQuery(const char *iface, int *ret)
		{
			if (ret)
			{
				*ret = META_IFACE_FAILED;
			}
	
			return NULL; 	
		}

		/**
		 * @brief Called when Metamod:Source acquires a valid 
		 * IServerPluginCallbacks pointer to be used for hooking by plugins.
		 *
		 * This will only be called after a call to ISmmAPI::EnableVSPListener().
		 * If called before GameInit, this callback will occur before LevelInit.
		 * Otherwise, it will be called on the first call after that.
		 *
		 * This callback is provided to all plugins regardless of which (or how 
		 * many) called EnableVSPListener(), but only if at least one did in 
		 * fact enable it, and only once for all plugins.  That is, a late 
		 * loading plugin should use ISmmAPI::GetVSPInfo() before relying on 
		 * this callback.
		 *
		 * This callback is never called if Metamod:Source is in VSP mode.  
		 * If in VSP mode, a VSP instance is automatically and always available 
		 * via ISmmAPI::GetVSPInfo(), which should be called anyway (to handle 
		 * late loading cases).
		 *
		 * @param iface			Interface pointer.  If NULL, then the VSP 
		 * 						listening construct failed to initialize and 
		 * 						is not available.
		 */
		virtual void OnVSPListening(IServerPluginCallbacks *iface) 
		{ 
		}

		/**
		 * @brief Called when Metamod:Source is about to remove a concommand or 
		 * convar.  This can also be called if ISmmAPI::UnregisterConCmdBase is 
		 * used by a plugin.
		 *
		 * @param id			Id of the plugin that created the concommand or 
		 * 						convar.
		 * @param pCommand		Pointer to concommand or convar that is being 
		 * 						removed.
		 */
		virtual void OnUnlinkConCommandBase(PluginId id, ConCommandBase *pCommand)
		{
		}
	};
}

#if !defined METAMOD_NO_AUTO_NAMESPACE
using namespace SourceMM;
#endif

#define PL_EXPOSURE		CreateInterface
#define PL_EXPOSURE_C	"CreateInterface"

/**
 * @brief Exposes the plugin to the MM:S loader.
 *
 * @param name		Deprecated - should be a variable name (like name).
 * @param var		Name of the variable that contains the singleton.
 *					This macro automatically takes the address of it, so 
 * 					you should not pass a pointer to your plugin's 
 *					singleton.
 */
#ifdef META_NO_HL2SDK
#define PL_EXPOSURE_FUNC(name, var) \
	SMM_API void *PL_EXPOSURE(const char *name, int *code) { \
		if (name && !strcmp(name, METAMOD_PLAPI_NAME)) { \
			return static_cast<void *>(&var); \
		} \
		return NULL; \
	}

#else
// First param should be actual classname, not iface name, but we don't have that and it doesn't matter here.
#define PL_EXPOSURE_FUNC(name, var)	EXPOSE_SINGLE_INTERFACE_GLOBALVAR(ISmmPlugin, ISmmPlugin, METAMOD_PLAPI_NAME, var);
#endif

#define PLUGIN_EXPOSE(name, var) \
	ISmmAPI *g_SMAPI = NULL; \
	ISmmPlugin *g_PLAPI = NULL; \
	PluginId g_PLID = (PluginId)0; \
	SourceHook::ISourceHook *g_SHPtr = NULL; \
	PL_EXPOSURE_FUNC(name, var)
	
	
	


/**
 * @brief This should be in one of your header files, if you wish 
 * to use values like g_SHPtr in other files.
 */
#define PLUGIN_GLOBALVARS()	\
	extern SourceHook::ISourceHook *g_SHPtr; \
	extern ISmmAPI *g_SMAPI; \
	extern ISmmPlugin *g_PLAPI; \
	extern PluginId g_PLID; 

/**
 * @brief This should be the first line in your Load callback.
 */
#define PLUGIN_SAVEVARS() \
	g_SMAPI = ismm; \
	g_SHPtr = static_cast<SourceHook::ISourceHook *>(ismm->MetaFactory(MMIFACE_SOURCEHOOK, NULL, NULL)); \
	g_PLAPI = static_cast<ISmmPlugin *>(this); \
	g_PLID = id;

#define META_LOG				g_SMAPI->LogMsg
#define META_REGCMD(name)		g_SMAPI->RegisterConCommandBase(g_PLAPI, name##_command)
#define META_REGCVAR(var)		g_SMAPI->RegisterConCommandBase(g_PLAPI, var)
#define META_UNREGCMD(name)		g_SMAPI->UnregisterConCommandBase(g_PLAPI, name##_command)
#define META_UNREGCVAR(var)		g_SMAPI->UnregisterConCommandBase(g_PLAPI, var)
#define	META_CONPRINT			g_SMAPI->ConPrint
#define META_CONPRINTF			g_SMAPI->ConPrintf

/* Probably should use this up above someday */
#define CONCMD_VARNAME(name) name##_command

#if !defined SMM_API
#if defined __WIN32__ || defined _WIN32 || defined WIN32
	#define SMM_API extern "C" __declspec(dllexport)
#elif defined __GNUC__
	#define SMM_API extern "C" __attribute__ ((visibility("default")))	
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
			ismm->Format(error, maxlen, "Could not find interface: %s", v_name); \
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
			ismm->Format(error, maxlen, "Could not find interface: %s", v_name); \
		} \
		return false; \
	}

#endif //_INCLUDE_ISMM_PLUGIN_H

