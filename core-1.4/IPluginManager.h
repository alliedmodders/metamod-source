/* ======== SourceMM ========
* Copyright (C) 2004-2008 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_PLUGINMANAGER_H
#define _INCLUDE_PLUGINMANAGER_H

/**
 * @brief Plugin Manager interface
 * @file IPluginManager.h
 */

typedef int PluginId;

#include "ISmmPlugin.h"

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

typedef int PluginId;
struct factories;

class ISmmPluginManager
{
public:
	/**
	 * @brief Loads a plugin and returns its id.  If this is called before DLLInit(),
	 *  then the plugin is considered to be "hot" - it might refuse its own load later!
	 *  Also, a hot plugin might not have an error message.
	 *
	 * @param file String containing file name
	 * @param source Specifies who loaded the plugin
	 * @param status Status of the plugin 
	 * @param ismm Pointer to Smm API
	 * @param error String buffer for error messages
	 * @param maxlen Maximum length of buffer
	 * @return Id of plugin
	 */
	virtual PluginId Load(const char *file, PluginId source, bool &already, char *error, size_t maxlen) =0;

	/**
	 * @brief Unloads a plugin
	 *
	 * @param id Id of plugin
	 * @param error String buffer for error messages
	 * @param maxlen Maximum length of buffer
	 * @return True on success, false otherwise
	 */
	virtual bool Unload(PluginId id, bool force, char *error, size_t maxlen) =0;

	/**
	 * @brief Pauses a plugin
	 *
	 * @param id Id of plugin
	 * @param error String buffer for error messages
	 * @param maxlen Maximum length of buffer
	 * @return True on success, false otherwise
	 */
	virtual bool Pause(PluginId id, char *error, size_t maxlen) =0;

	/**
	 * @brief Unpauses a plugin
	 *
	 * @param id Id of plugin
	 * @param force If true, forces the plugin to unload
	 * @param error String buffer for error messages
	 * @param maxlen Maximum length of buffer
	 * @return True on success, false otherwise
	 */
	virtual bool Unpause(PluginId id, char *error, size_t maxlen) =0;

	/**
	 * @brief Unloads all plugins forcefully
	 * 
	 * @return True on success, false otherwise
	 */
	virtual bool UnloadAll() =0;

	/**
	 * @brief Returns information about a plugin
	 *
	 * @param id Id of plugin
	 * @param file Pointer to file string by reference
	 * @param list Pointer to factories by reference
	 * @param status By reference status of plugin
	 * @param source By reference source of plugin
	 * @return True on success, false if not found
	 */
	virtual bool Query(PluginId id, const char *&file, Pl_Status &status, PluginId &source) =0;

	/** 
	 * @brief Checks another plugin's QueryRunning() status.
	 *
	 * @param id		Id of plugin
	 * @param error		Message buffer
	 * @param maxlen	Size of error buffer
	 * @return			Status value
	 */
	virtual bool QueryRunning(PluginId id, char *error, size_t maxlength) =0;

	/**
	 * @brief Returns the handle of a plugin (OS dependent meaning)
	 * 
	 * @param id		Id of plugin
	 * @param handle	By reference handle of plugin, if any
	 * @return			True if plugin id is valid, false otherwise
	 */
	virtual bool QueryHandle(PluginId id, void *&handle) =0;
};

#endif //_INCLUDE_PLUGINMANAGER_H
