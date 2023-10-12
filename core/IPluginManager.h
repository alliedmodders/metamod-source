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

#ifndef _INCLUDE_METAMOD_IPLUGINMANAGER_H
#define _INCLUDE_METAMOD_IPLUGINMANAGER_H

/**
 * @brief Plugin Manager interface
 * @file IPluginManager.h
 */

#include <ISmmPluginExt.h>
#include <string>

#if defined META_IS_SOURCE2
#include <icvar.h>
#include <tier1/convar.h>
class ConCommandBase
{
public:
	ConCommandBase(const ConVarCreation_t& creation, int64_t nAdditionalFlags, const ConVarHandle& handle) :
	m_bIsCvar(true),
	m_Name(creation.m_pszName),
	m_nAdditionalFlags(nAdditionalFlags),
	m_Handle(handle),
	m_Creation(creation)
	{}

	ConCommandBase(const ConCommandCreation_t& creation, int64_t nAdditionalFlags, const ConCommandHandle& handle) :
	m_bIsCvar(false),
	m_Name(creation.m_pszName),
	m_nAdditionalFlags(nAdditionalFlags),
	m_Handle(handle),
	m_Creation(creation)
	{}

	const char* GetName() const { return m_Name.c_str(); }
	bool IsConVar() const { return m_bIsCvar; }

	ConVarHandle GetConVar() { return m_Handle.cvar; }
	ConCommandHandle GetConCommand() { return m_Handle.cmd; }

	ConVarCreation_t& GetConVarCreation() { return m_Creation.cvar; }
	ConCommandCreation_t& GetConCommandCreation() { return m_Creation.cmd; }

	int64_t GetAdditionalFlags() const { return m_nAdditionalFlags; }
protected:
	bool m_bIsCvar;
	std::string m_Name;
	int64_t m_nAdditionalFlags;
	
	union ConCommandBaseHandle {
		ConCommandBaseHandle(ConVarHandle handle) :
		cvar(handle)
		{}

		ConCommandBaseHandle(ConCommandHandle handle) :
		cmd(handle)
		{}

		ConVarHandle cvar;
		ConCommandHandle cmd;
	} m_Handle;

	union ConCommandBaseCreation {
		ConCommandBaseCreation(const ConVarCreation_t& handle) :
		cvar(handle)
		{}

		ConCommandBaseCreation(const ConCommandCreation_t& handle) :
		cmd(handle)
		{}
	
		ConVarCreation_t cvar;
		ConCommandCreation_t cmd;
	} m_Creation;
};
#endif

namespace SourceMM
{
	/**
	 * @brief Load sources
	 */
	enum
	{
		Pl_BadLoad=0,
		Pl_Console=-1,
		Pl_File=-2,
		Pl_MinId=1,
	};
	
	/**
	 * @brief Status of a plugin at runtime
	 */
	enum Pl_Status
	{
		Pl_NotFound=-4,
		Pl_Error=-3,
		Pl_Refused=-2,
		Pl_Paused=-1,
		Pl_Running=0,
	};

	/**
	 * @brief Programmatic management of the "loaded plugin" list.
	 */
	class ISmmPluginManager
	{
	public:
		/**
		 * @brief Loads a plugin and returns its id.  If this is called before DLLInit(),
		 *  then the plugin is considered to be "hot" - it might refuse its own load later!
		 *  Also, a hot plugin might not have an error message.
		 *
		 * @param file 		String containing file name.
		 * @param source 	Specifies who loaded the plugin.
		 * @param already 	Whether or not the plugin was already loaded.
		 * @param error 	String buffer for error messages.
		 * @param maxlen 	Maximum length of buffer.
		 * @return 			Id of plugin.
		 */
		virtual PluginId Load(const char *file, PluginId source, bool &already, char *error, size_t maxlen) =0;

		/**
		 * @brief Unloads a plugin.
		 *
		 * @param id 		Id of plugin
		 * @param force		True to forcefully unload, false to let plugin opt-out.
		 * @param error 	String buffer for error messages
		 * @param maxlen 	Maximum length of buffer
		 * @return 			True on success, false otherwise
		 */
		virtual bool Unload(PluginId id, bool force, char *error, size_t maxlen) =0;
	
		/**
		 * @brief Pauses a plugin
		 *
		 * @param id 		Id of plugin
		 * @param error 	String buffer for error messages
		 * @param maxlen 	Maximum length of buffer
		 * @return 			True on success, false otherwise
		 */
		virtual bool Pause(PluginId id, char *error, size_t maxlen) =0;
	
		/**
		 * @brief Unpauses a plugin
		 *
		 * @param id 		Id of plugin
		 * @param error 	String buffer for error messages
		 * @param maxlen 	Maximum length of buffer
		 * @return 			True on success, false otherwise
		 */
		virtual bool Unpause(PluginId id, char *error, size_t maxlen) =0;
	
		/**
		 * @brief Unloads all plugins forcefully
		 * 
		 * @return 			True on success, false otherwise
		 */
		virtual bool UnloadAll() =0;
	
		/**
		 * @brief Returns information about a plugin
		 *
		 * @param id 		Id of plugin
		 * @param file 		Pointer to store filename pointer, or NULL to ignore.
		 * @param status 	Pointer to store status, or NULL to ignore.
		 * @param source 	Pointer to store source, or NULL to ignore.
		 * @return 			True on success, false if not found
		 */
		virtual bool Query(PluginId id, const char **file, Pl_Status *status, PluginId *source) =0;
	
		/** 
		 * @brief Checks another plugin's QueryRunning() status.
		 *
		 * @param id		Id of plugin
		 * @param error		Message buffer
		 * @param maxlength	Size of error buffer
		 * @return			Status value
		 */
		virtual bool QueryRunning(PluginId id, char *error, size_t maxlength) =0;
	
		/**
		 * @brief Returns the handle of a plugin (OS dependent meaning)
		 * 
		 * @param id		Id of plugin
		 * @param handle	Pointer to store handle pointer, or NULL to ignore.
		 * @return			True if plugin id is valid, false otherwise
		 */
		virtual bool QueryHandle(PluginId id, void **handle) =0;
	};
}

#if !defined METAMOD_NO_AUTO_NAMESPACE
using namespace SourceMM;
#endif

#endif //_INCLUDE_PLUGINMANAGER_H

