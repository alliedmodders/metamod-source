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

#ifndef _INCLUDE_ISMM_API_H
#define _INCLUDE_ISMM_API_H

/**
 * @brief Core API calls that are provided to plugins.
 * @file ISmmAPI.h
 */

#include <stdarg.h>
#include <sourcehook.h>
#include <IPluginManager.h>

#if defined META_NO_HL2SDK
class CGlobalVars;
struct edict_t;
class ConCommandBase;
#else
#include <eiface.h>
#endif

#include <ISmmPlugin.h>
#include <ISmmPluginExt.h>

#define	MMIFACE_SOURCEHOOK		"ISourceHook"			/**< ISourceHook Pointer */
#define	MMIFACE_PLMANAGER		"IPluginManager"		/**< SourceMM Plugin Functions */
#define MMIFACE_SH_HOOKMANAUTOGEN	"IHookManagerAutoGen"		/**< SourceHook::IHookManagerAutoGen Pointer */
#define IFACE_MAXNUM			999						/**< Maximum interface version */

#if defined META_IS_SOURCE2
typedef CPlayerSlot MMSPlayer_t;
static const MMSPlayer_t MMSPlayer_INVALID = CPlayerSlot(-1);
#else
typedef edict_t* MMSPlayer_t;
static const MMSPlayer_t MMSPlayer_INVALID = nullptr;
#endif

typedef void* (*CreateInterfaceFn)(const char *pName, int *pReturnCode);

class IServerPluginCallbacks;

namespace SourceMM
{
	class ISmmPlugin;
	class IMetamodListener;

	/**
	 * The core API that Metamod:Source provides to plugins.
	 */
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
	
		/**
		 * @brief Returns an interface factory for the HL2 engine.
		 *
		 * @param syn			If syn is true, the synthetic wrapper is returned.  
		 *						If syn is false, the true function is returned.
		 * @return				CreateInterfaceFn function pointer.
		 */
		virtual CreateInterfaceFn GetEngineFactory(bool syn=true) =0;

		/**
		 * @brief Returns an interface factory for the HL2 physics engine.
		 *
		 * @param syn			If syn is true, the synthetic wrapper is returned.  
		 *						If syn is false, the true function is returned.
		 * @return				CreateInterfaceFn function pointer.
		 */
		virtual CreateInterfaceFn GetPhysicsFactory(bool syn=true) =0;

		/**
		 * @brief Returns an interface factory for the HL2 file system.
		 *
		 * @param syn			If syn is true, the synthetic wrapper is returned.  
		 *						If syn is false, the true function is returned.
		 * @return				CreateInterfaceFn function pointer.
		 */
		virtual CreateInterfaceFn GetFileSystemFactory(bool syn=true) =0;

		/**
		 * @brief Returns an interface factory for the GameDLL.
		 *
		 * @param syn			If syn is true, the synthetic wrapper is returned.
		 *						If syn is false, the true function is returned.
		 * @return				CreateInterfaceFn function pointer.
		 */
		virtual CreateInterfaceFn GetServerFactory(bool syn=true) =0;

		/**
		 * @brief Returns a CGlobalVars pointer from the HL2 Engine.
		 *
		 * @return				CGlobalVars pointer.
		 */
		virtual CGlobalVars *GetCGlobals() =0;

		/**
		 * @brief Registers a ConCommandBase.
		 *
		 * @param plugin		Parent plugin API pointer.
		 * @param pCommand		ConCommandBase to register.
		 * @return				True if successful, false otherwise.
		 */
		virtual bool RegisterConCommandBase(ISmmPlugin *plugin, ConCommandBase *pCommand) =0;

		/**
		 * @brief Unregisters a ConCommandBase.
		 *
		 * @param plugin		Parent plugin API pointer.
		 * @param pCommand		ConCommandBase to unlink.
		 */
		virtual void UnregisterConCommandBase(ISmmPlugin *plugin, ConCommandBase *pCommand) =0;
	
		/**
		 * @brief Prints an unformatted string to the remote server console.
		 * 
		 * Note: Newlines are not added automatically.
		 *
		 * @param str			Message string.
		 */
		virtual void ConPrint(const char *str) =0;

		/**
		 * @brief Prints a formatted message to the remote server console.  
		 * 
		 * Note: Newlines are not added automatically.
		 *
		 * @param fmt			Formatted message.
		 */
		virtual void ConPrintf(const char *fmt, ...) =0;

		/**
		 * @brief Returns the Metamod Version numbers as major version and 
		 * minor (API) version.  Changes to minor version are guaranteed to be 
		 * backwards compatible.  Changes to major version are not.
		 * 
		 * @param major			Filled with the major API version number.
		 * @param minor			Filled with the minor API version number.
		 * @param plvers		Filled with the current plugin API version number.
		 * @param plmin			Filled with the minimum plugin API version number 
		 * 						supported.
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
		  * @param id			Optional pointer to store id of plugin that 
		  * 					overrode interface, 0 if none
		  * @return				Returned pointer
		  */
		virtual void *MetaFactory(const char *iface, int *ret, PluginId *id) =0;

		/**
		 * @brief Given a base interface name, such as ServerGameDLL or 
		 * ServerGameDLL003, reformats the string to increase the number, then 
		 * returns the new number. This is the base function to InterfaceSearch() 
		 * and VInterfaceMatch().
		 * 
		 * @param iface			Input/output interface name.  Must be writable.
		 * @param maxlength		Maximum length of iface buffer.  Must be at least 
		 * 						strlen(iface)+4 chars.
		 * @return				The newly incremented iface version number.
		 * @deprecated			Use InterfaceSearch() or VInterfaceMatch instead.
		 */
		virtual int FormatIface(char iface[], size_t maxlength) =0;

		/**
		 * @brief Searches for an interface, eliminating the need to loop 
		 * through FormatIface().
		 * 
		 * @param fn			InterfaceFactory function.
		 * @param iface			Interface string name.
		 * @param max			Maximum version to look up.
		 * @param ret			Last return code from interface factory function.
		 * @return				Interface pointer, or NULL if not found.
		 */
		virtual void *InterfaceSearch(CreateInterfaceFn fn, 
									  const char *iface, 
									  int max, 
									  int *ret) =0;

		/**
		 * @brief Returns the base directory of the game/server, equivalent to 
		 * IVEngineServer::GetGameDir(), except the path is absolute.
		 *
		 * @return				Static pointer to game's absolute basedir.
		 */
		virtual const char *GetBaseDir() =0;

		/**
		 * @brief Formats a file path to the local OS.  
		 *
		 * Does not include any base directories.  Note that all slashes and 
		 * black slashes are reverted to the local OS's expectancy.
		 *
		 * @param buffer		Destination buffer to store path.
		 * @param len			Maximum length of buffer, including null 
		 * 						terminator.
		 * @param fmt			Formatted string.
		 * @param ...			Arguments in the string.
		 * @return				Number of bytes written, not including the null 
		 *						terminator.
		 */
		virtual size_t PathFormat(char *buffer, size_t len, const char *fmt, ...) =0;

		/**
		 * @brief Prints text in the specified client's console. Same as 
		 * IVEngineServer::ClientPrintf except that it allows for string 
		 * formatting.
		 *
		 * @param client		Client edict pointer.
		 * @param fmt			Formatted string to print to the client.
		 */
		virtual void ClientConPrintf(MMSPlayer_t client, const char *fmt, ...) =0;

		/**
		 * @brief Wrapper around InterfaceSearch().  Assumes no maximum.
		 * This is designed to replace the fact that searches only went upwards.
		 * The "V" is intended to convey that this is for Valve formatted 
		 * interface strings.
		 *
		 * @param fn			Interface factory function.
		 * @param iface			Interface string.
		 * @param min			Minimum value to search from.  If zero, searching 
		 *						begins from the first available version regardless 
		 *						of the interface.  Note that this can return 
		 *						interfaces EARLIER than the version specified.  A 
		 *						value of -1 (default) specifies the string version 
		 *						as the minimum.  Any other value specifices the 
		 *						minimum value to search from.
		 * @return				Interface pointer, or NULL if not found.
		 */
		virtual void *VInterfaceMatch(CreateInterfaceFn fn, 
									  const char *iface, 
									  int min=-1) =0;

		/**
		 * @brief Tells SourceMM to add VSP hooking capability to plugins.  
		 *
		 * Since this  potentially uses more resources than it would otherwise, 
		 * plugins have to explicitly enable the feature.  Whether requested or 
		 * not, if it is enabled, all plugins will get a pointer to the VSP 
		 * listener through IMetamodListener.  This will not be called more than 
		 * once for a given plugin; if it is requested more than once, each 
		 * successive call will only give the pointer to plugins which have not 
		 * yet received it.
		 */
		virtual void EnableVSPListener() =0;

		/**
		 * @brief Returns the interface version of the GameDLL's IServerGameDLL 
		 * implementation.
		 *
		 * @return				Interface version of the loaded IServerGameDLL.
		 */
		virtual int GetGameDLLVersion() =0;

		/**
		 * @brief Returns the number of user messages in the GameDLL.
		 *
		 * @return				Number of user messages, or -1 if SourceMM has 
		 *						failed to get user message list.
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
		 * @brief Returns the highest interface version of IServerPluginCallbacks 
		 * that the engine supports.  This is useful for games that run on older 
		 * versions of the Source engine, such as The Ship.
		 *
		 * @return				Highest interface version of IServerPluginCallbacks.
		 *						Returns 0 if SourceMM's VSP listener isn't 
		 *						currently enabled.
		 * @deprecated			Use GetVSPInfo() instead.
		 */
		virtual int GetVSPVersion() =0;

		/**
		 * @brief Returns the engine interface that MM:S is using as a backend.
		 *
		 * The values will be one of the SOURCE_ENGINE_* constants from the top
		 * of this file.
	 	 *
	 	 * @return				A SOURCE_ENGINE_* constant value.
		 */
		virtual int GetSourceEngineBuild() =0;

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
		 * @brief Formats a string.  This is a platform safe wrapper around 
		 * snprintf/_snprintf.
		 *
		 * @param buffer		Buffer to write to.
		 * @param maxlength		Maximum length of the buffer.
		 * @param format		Format specifiers.
		 * @param ...			Format arguments.
		 * @return				Number of bytes actually written, not including 
		 * 						the null terminator.
		 */
		virtual size_t Format(char *buffer, 
						  	  size_t maxlength,
						  	  const char *format,
						  	  ...) =0;

		/**
		 * @brief Formats a string.  This is a platform safe wrapper around 
		 * vsnprintf/_vsnprintf.
		 *
		 * @param buffer		Buffer to write to.
		 * @param maxlength		Maximum length of the buffer.
		 * @param format		Format specifiers.
		 * @param ap			Format argument list.
		 * @return				Number of bytes actually written, not including the 
		 *						null terminator.
		 */
		virtual size_t FormatArgs(char *buffer,
								  size_t maxlength,
								  const char *format,
								  va_list ap) =0;
	};
}

#if !defined METAMOD_NO_AUTO_NAMESPACE
using namespace SourceMM;
#endif

/** 
 * Version History
 *
 * 1.1.0 Bumped API to 1:0. The breaking changes occurred in SourceHook and the plugin API.
 * 1.1.2 Added API call for generating iface names.
 * 1.2   Added API more helper functions and new SourceHook version.
 * 1.2.2 Added API for printing to client console (with string formatting).
 * 1.3   Added new interface search API.
 * 1.4	 Added VSP listener and user message API.
 * 1.5.0 Added API for getting highest supported version of IServerPluginCallbacks.
 * 1.6.0 Added API for Orange Box.  Broke backwards compatibility.
 */

#endif //_INCLUDE_ISMM_API_H

