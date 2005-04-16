#ifndef _INCLUDE_PLUGINMANAGER_H
#define _INCLUDE_PLUGINMANAGER_H

#include "ISmmPlugin.h"

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
 * @brief Load sources
 */
enum
{
	Pl_BadLoad=0,
	Pl_Console,
	Pl_File,
	Pl_MinId,
};

typedef unsigned int PluginId;
struct factories;

class ISmmPluginManager
{
public:
	/**
	 * @brief Loads a plugin and returns its id
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
	virtual bool Unload(PluginId id, char *error, size_t maxlen) =0;

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
	virtual bool Query(PluginId id, const char *&file, factories *&list, Pl_Status &status, PluginId &source) =0;
};

#endif //_INCLUDE_PLUGINMANAGER_H
