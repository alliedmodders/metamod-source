/* ======== SourceMM ========
* Copyright (C) 2004-2008 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_ISMM_API_H
#define _INCLUDE_ISMM_API_H

/**
 * @brief External API interface
 * @file ISmmAPI.h
 */

#include <interface.h>
#include <eiface.h>
#include <sourcehook/sourcehook.h>
#include "IPluginManager.h"

#if defined __GNUC__
#if ((__GNUC__ == 3) && (__GNUC_MINOR__ < 4)) || (__GNUC__ < 3)
#error "You must compile with at least GCC 3.4! If you know what you are doing, you can remove this message."
#endif //version check
#endif //__GNUC__

class IMetamodListener;
class ISmmPluginManager;
class ISmmPlugin;

#define	MMIFACE_SOURCEHOOK		"ISourceHook"			/**< ISourceHook Pointer */
#define	MMIFACE_PLMANAGER		"IPluginManager"		/**< SourceMM Plugin Functions */
#define IFACE_MAXNUM			999

#define SOURCE_ENGINE_UNKNOWN			0				/**< Could not determine the engine version */
#define SOURCE_ENGINE_ORIGINAL			1				/**< Original Source Engine (used by The Ship) */
#define SOURCE_ENGINE_EPISODEONE		2				/**< Episode 1 Source Engine (second major SDK) */

class ISmmAPI
{
public:
	/**
	 * @brief Logs a message through the HL2 log system.
	 * Note: Newlines are appended automatically.
	 *
	 * @param pl			Plugin API pointer (used for tagging message)
	 * @param msg			Formatted string.
	 */
	virtual void LogMsg(ISmmPlugin *pl, const char *msg, ...) =0;
public:
	/**
	 * @brief Returns an interface factory for the HL2 engine.
	 *
	 * @param syn			If syn is true, the synthetic wrapper is returned.  
	 *						If syn is false, the true function is returned.
	 * @return				CreateInterfaceFn function pointer.
	 */
	virtual CreateInterfaceFn engineFactory(bool syn=true) =0;

	/**
	 * @brief Returns an interface factory for the HL2 physics engine.
	 *
	 * @param syn			If syn is true, the synthetic wrapper is returned.  
	 *						If syn is false, the true function is returned.
	 * @return				CreateInterfaceFn function pointer.
	 */
	virtual CreateInterfaceFn physicsFactory(bool syn=true) =0;

	/**
	 * @brief Returns an interface factory for the HL2 file system.
	 *
	 * @param syn			If syn is true, the synthetic wrapper is returned.  
	 *						If syn is false, the true function is returned.
	 * @return				CreateInterfaceFn function pointer.
	 */
	virtual CreateInterfaceFn fileSystemFactory(bool syn=true) =0;

	/**
	 * @brief Returns an interface factory for the GameDLL.
	 *
	 * @param syn			If syn is true, the synthetic wrapper is returned.  
	 *						If syn is false, the true function is returned.
	 * @return				CreateInterfaceFn function pointer.
	 */
	virtual CreateInterfaceFn serverFactory(bool syn=true) =0;

	/**
	 * @brief Returns a CGlobalVars pointer from the HL2 Engine.
	 *
	 * @return				CGlobalVars pointer.
	 */
	virtual CGlobalVars *pGlobals() =0;

	/**
	 * @brief Used with SourceHook, sets teh last meta return value.
	 * Note: Do not call this directly, use the Metamod macros.
	 *
	 * @param res			META_RETURN value to set.
	 */
	virtual void SetLastMetaReturn(META_RES res) =0;

	/**
	 * @brief Used with SourceHook, returns the last meta return value.
	 * Note: This is only valid inside a hook function.
	 *
	 * @return				Last META_RETURN value set by a plugin.
	 */
	virtual META_RES GetLastMetaReturn() =0;

public:		// Added in 1.00-RC2 (0:0)
	/**
	 * @brief Allows access to Metamod's ConCommandBaseAccessor.
	 *
	 * @return				Returns IConCommandBaseAccessor pointer.
	 */
	virtual IConCommandBaseAccessor *GetCvarBaseAccessor() =0;

	/**
	 * @brief Registers a ConCommandBase.
	 *
	 * @param plugin		Parent plugin API pointer.
	 * @param pCommand		ConCommandBase to register.
	 * @return				True if successful, false otherwise.  Does not return false yet.
	 */
	virtual bool RegisterConCmdBase(ISmmPlugin *plugin, ConCommandBase *pCommand) =0;

	/**
	 * @brief Unregisters a ConCommandBase.
	 *
	 * @param plugin		Parent plugin API pointer.
	 * @param pCommand		ConCommandBase to unlink.
	 */
	virtual void UnregisterConCmdBase(ISmmPlugin *plugin, ConCommandBase *pCommand) =0;
	
	/**
	 * @brief Prints an unformatted string to the remote server console.
	 * Note: Newlines are not added automatically.
	 *
	 * @param str			Message string.
	 */
	virtual void ConPrint(const char *str) =0;

	/**
	 * @brief Prints a formatted message to the remote server console.  
	 * Note: Newlines are not added automatically.
	 *
	 * @param fmt			Formatted message.
	 */
	virtual void ConPrintf(const char *fmt, ...) =0;

public:		// Added in 1.1.0 (1:0)
	/**
	 * @brief Checks if ConPrint/ConPrintf will mirror to rcon.
	 *
	 * @return				True if remote printing available, false otherwise.
	 */
	virtual bool RemotePrintingAvailable() =0;

	/**
	 * @brief Returns the Metamod Version numbers as major version and minor (API) version.
	 * Changes to minor version are guaranteed to be backwards compatible.
	 * Changes to major version are not.
	 * 
	 * @param major			Filled with the major API version number.
	 * @param minor			Filled with the minor API version number.
	 * @param plvers		Filled with the current plugin API version number.
	 * @param plmin			Filled with the minimum plugin API version number supported.
	 */
	virtual void GetApiVersions(int &major, int &minor, int &plvers, int &plmin) =0;
	
	/** 
	 * @brief Returns sourcehook API version and implementation version.
	 *
	 * @param shvers		Filled with the SourceHook API version number.
	 * @param shimpl		Filled with the SourceHook implementation number.
	 */
	virtual void GetShVersions(int &shvers, int &shimpl) =0;
	
	/**
	 * @brief Adds a Metamod listener.
	 *
	 * @param plugin		Plugin interface pointer.
	 * @param pListener		Listener interface pointer to add.
	 */
	virtual void AddListener(ISmmPlugin *plugin, IMetamodListener *pListener) =0;

	/**
	  * @brief Queries the metamod factory
	  *
	  * @param iface		String containing interface name
	  * @param ret			Optional pointer to store return status
	  * @param id			Optional pointer to store id of plugin that overrode interface, 0 if none
	  * @return				Returned pointer
	  */
	virtual void *MetaFactory(const char *iface, int *ret, PluginId *id) =0;

public:		// Added in 1.1.2 (1:1)
	/**
	 * @brief Given a base interface name, such as ServerGameDLL or ServerGameDLL003, 
	 * reformats the string to increase the number, then returns the new number.
	 * This is the base function to InterfaceSearch() and VInterfaceMatch().
	 * 
	 * @param iface			Input/output interface name.  Must be writable.
	 * @param maxlength		Maximum length of iface buffer.  Must be at least strlen(iface)+4 chars.
	 * @return				The newly incremented iface version number.
	 */
	virtual int FormatIface(char iface[], unsigned int maxlength) =0;

public:		// Added in 1.2 (1:2)
	/**
	 * @brief Searches for an interface for you.
	 * 
	 * @param fn			InterfaceFactory function.
	 * @param iface			Interface string name.
	 * @param max			Maximum version to look up.
	 * @param ret			Last return code from interface factory function.
	 * @return				Interface pointer, or NULL if not found.
	 */
	virtual void *InterfaceSearch(CreateInterfaceFn fn, const char *iface, int max, int *ret) =0;

	/**
	 * @brief Returns the base directory of the game/server, equivalent to 
	 *  IVEngineServer::GetGameDir(), except the path is absolute.
	 *
	 * @return				Static pointer to game's absolute basedir.
	 */
	virtual const char *GetBaseDir() =0;

	/**
	 * @brief Formats a file path to the local OS.  Does not include any base directories.
	 * Note that all slashes and black slashes are reverted to the local OS's expectancy.
	 *
	 * @param buffer		Destination buffer to store path.
	 * @param len			Maximum length of buffer, including null terminator.
	 * @param fmt			Formatted string.
	 */
	virtual void PathFormat(char *buffer, size_t len, const char *fmt, ...) =0;

public:		// Added in 1.2.2 (1:3)
	/**
	 * @brief Prints text in the specified client's console. Same as IVEngineServer::ClientPrintf 
	 *  except that it allows for string formatting.
	 *
	 * @param client		Client edict pointer.
	 * @param fmt			Formatted string to print to the client.
	 */
	virtual void ClientConPrintf(edict_t *client, const char *fmt, ...) =0;

public:		// Added in 1.3 (1:4)
	/**
	 * @brief Wrapper around InterfaceSearch().  Assumes no maximum.
	 * This is designed to replace the fact that searches only went upwards.
	 * The "V" is intended to convey that this is for Valve formatted interface strings.
	 *
	 * @param fn			Interface factory function.
	 * @param iface			Interface string.
	 * @param min			Minimum value to search from.  If zero, searching begins from the
	 *                       first available version regardless of the interface.  
	 *                      Note that this can return interfaces EARLIER than the version specified.
	 *                      A value of -1 (default) specifies the string version as the minimum.
	 *                      Any other value specifices the minimum value to search from.
	 * @return				Interface pointer, or NULL if not found.
	 */
	virtual void *VInterfaceMatch(CreateInterfaceFn fn, const char *iface, int min=-1) =0;

public:		// Added in 1.4 (1:5)
	/**
	 * @brief Tells SourceMM to add VSP hooking capability to plugins.  
	 *
	 * Since this  potentially uses more resources than it would otherwise, plugins have to 
	 * explicitly enable the feature.  Whether requested or not, if it is enabled, all plugins 
	 * will get a pointer to the VSP listener through IMetamodListener.
	 */
	virtual void EnableVSPListener() =0;

	/**
	 * @brief Returns the interface version of the GameDLL's IServerGameDLL implementation.
	 *
	 * @return				Interface version of the loaded IServerGameDLL.
	 */
	virtual int GetGameDLLVersion() =0;

	/**
	 * @brief Returns the number of user messages in the GameDLL.
	 *
	 * @return				Number of user messages, or -1 if SourceMM has failed to get user message list.
	 */
	virtual int GetUserMessageCount() =0;

	/**
	 * @brief Returns the index of the specified user message.
	 *
	 * @param name			User message name.
	 * @param size			Optional pointer to store size of user message.
	 * @return				Message index, or -1 on failure.
	 */
	virtual int FindUserMessage(const char *name, int *size=NULL) =0;

	/**
	 * @brief Returns the name of the specified user message.
	 *
	 * @param index			User message index.
	 * @param size			Optional pointer to store size of user message.
	 * @return				Message name, or NULL on failure.
	 */
	virtual const char *GetUserMessage(int index, int *size=NULL) =0;

	/**
	 * @brief Returns the VSP listener loaded.
	 *
	 * This is useful for late-loading plugins which need to decide whether 
	 * to add a listener or not (or need to get the pointer at all).
	 *
	 * @param pVersion		Optional pointer to store the VSP version.
	 * @return				IServerPluginCallbacks pointer, or NULL if an
	 * 						IMetamodListener event has yet to occur for 
	 * 						EnableVSPListener().
	 */
	virtual IServerPluginCallbacks *GetVSPInfo(int *pVersion) =0;

	/**
	 * @brief Returns the engine interface that MM:S is using as a backend.
	 *
	 * The values will be one of the SOURCE_ENGINE_* constants from the top
	 * of this file.
	 *
	 * @return				A SOURCE_ENGINE_* constant value.
	 */
	virtual int GetSourceEngineBuild() =0;

};


/** 
 * Version History
 *
 * 1.1.0 Bumped API to 1:0. The breaking changes occurred in SourceHook and the plugin API.
 * 1.1.2 Added API call for generating iface names.
 * 1.2   Added API more helper functions and new SourceHook version.
 * 1.2.2 Added API for printing to client console (with string formatting).
 * 1.3   Added new interface search API.
 * 1.4	 Added VSP listener and user message API.
 * 1.8   Backported SH VP hooks and various "new API" calls.
 */

#endif //_INCLUDE_ISMM_API_H
