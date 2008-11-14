/**
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

#ifndef _INCLUDE_CPLUGIN_H
#define _INCLUDE_CPLUGIN_H

/**
 * @brief Implementation of Plugin API and Management routines
 * @file CPlugin.cpp
 */

#include <interface.h>
#include <eiface.h>
#include <convar.h>
#include <sh_list.h>
#include <sh_string.h>
#include <IPluginManager.h>
#include <ISmmPluginExt.h>
#include "metamod_oslink.h"

/**
 * History of plugin versions: (M=min, C=current)
 *     1: Initial interface
 *     2: Added GetLogTag()
 *     3: Moved GetApiVersion() to top  (2005-04-16)
 *     4: Added AllPluginsLoaded() callback (2005-04-18)
 *     5: Bumped version for SourceHook V4 (2005-05-01)
 *     6: Added functions for console printing (2005-05-26)
 *     7: Changed template libraries (2005-08-11)
 * 	      New loading structure mechanism
 *        New SourceHook version
 *     8: New SourceHook version (2005-12-23)
 *        New ISmmAPI additions
 *     9: New ISmmPluginManager additions
 *    10: Added VSP listen functions to ISmmAPI and IMetamodListener (2007-02-09)
 *    11: New SourceHook version v4.5 (May, 2007)
 *    12: Orange Box API
 *    13: Breaking of API for next SH version and other API changes
 * MC 14: ABI stability reached for 1.6.0 changes
 */

#define PLAPI_MIN_VERSION	14

struct CNameAlias
{
	SourceHook::String alias;
	SourceHook::String value;
};
/**
 * @brief Implements Plugin Manager API
 */
class CPluginManager : public ISmmPluginManager
{	
public:
	/**
	 * @brief Internal structure for holding plugin data
	 */
	class CPlugin
	{
	public:
		CPlugin();
	public:
		PluginId m_Id;
		SourceHook::String m_File;
		Pl_Status m_Status;
		PluginId m_Source;
		ISmmPlugin *m_API;
		HINSTANCE m_Lib;
		SourceHook::List<ConCommandBase *> m_Cvars;
		SourceHook::List<ConCommandBase *> m_Cmds;
		SourceHook::List<IMetamodListener *> m_Events;
		METAMOD_FN_UNLOAD m_UnloadFn;
	};
public:
	CPluginManager();
	~CPluginManager();
	void SetAllLoaded();
public:
	PluginId Load(const char *file, PluginId source, bool &already, char *error, size_t maxlen);
	bool Unload(PluginId id, bool force, char *error, size_t maxlen);
	bool Pause(PluginId id, char *error, size_t maxlen);
	bool Unpause(PluginId id, char *error, size_t maxlen);
	bool UnloadAll();
	void SetAlias(const char *alias, const char *value);
public:
	bool Query(PluginId id, const char **file, Pl_Status *status, PluginId *source);
	bool QueryRunning(PluginId id, char *error, size_t maxlength);
	bool QueryHandle(PluginId id, void **handle);

	void AddPluginCvar(ISmmPlugin *api, ConCommandBase *pCvar);
	void AddPluginCmd(ISmmPlugin *api, ConCommandBase *pCmd);
	void RemovePluginCvar(ISmmPlugin *api, ConCommandBase *pCvar);
	void RemovePluginCmd(ISmmPlugin *api, ConCommandBase *pCmd);

	/**
	 * @brief Finds a plugin by Id
	 *
	 * @param id Id of plugin
	 * @return CPlugin on success, NULL otherwise
	 */
	CPlugin *FindById(PluginId id);

	CPlugin *FindByAPI(ISmmPlugin *api);

	/**
	 * @brief Attempts to reload a failed plugin
	 *
	 * @param id Id of plugin
	 * @param error Error message buffer
	 * @param len Maximum length of buffer
	 * @return True on success, false otherwise
	 */
	bool Retry(PluginId id, char *error, size_t len);

	int GetPluginCount();
	const char *GetStatusText(CPlugin *pl);

	//get alias info
	const char *LookupAlias(const char *alias);
	SourceHook::List<CNameAlias *>::iterator _alias_begin();
	SourceHook::List<CNameAlias *>::iterator _alias_end();

	//Internal iterators
	SourceHook::List<CPluginManager::CPlugin *>::iterator _begin();
	SourceHook::List<CPluginManager::CPlugin *>::iterator _end();
private:
	//These are identical internal functions for the wrappers above.
	CPlugin *_Load(const char *file, PluginId source, char *error, size_t maxlen);
	bool _Unload(CPlugin *pl, bool force, char *error, size_t maxlen);
	bool _Pause(CPlugin *pl, char *error, size_t maxlen);
	bool _Unpause(CPlugin *pl, char *error, size_t maxlen);
	void UnregAllConCmds(CPlugin *pl);
private:
	PluginId m_LastId;
	SourceHook::List<CPlugin *> m_Plugins;
	SourceHook::List<CNameAlias *> m_Aliases;
	bool m_AllLoaded;
};

typedef SourceHook::List<CPluginManager::CPlugin *>::iterator PluginIter;

/** @brief Singleton for plugin manager */
extern CPluginManager g_PluginMngr;

#endif //_INCLUDE_CPLUGIN_H

