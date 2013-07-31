/**
 * vim: set ts=4 sw=4 tw=99 noet :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2009 AlliedModders LLC and authors.
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
 */

#include <stdio.h>
#include "metamod_oslink.h"
#include "metamod.h"
#include "metamod_plugins.h"
#include "metamod_util.h"

/** 
 * @brief Implements functions from CPlugin.h
 * @file CPlugin.cpp
 */

using namespace SourceMM;

#define ITER_PLEVENT(evn, plid) \
	CPluginManager::CPlugin *_Xpl; \
	SourceHook::List<IMetamodListener *>::iterator event; \
	IMetamodListener *api; \
	for (PluginIter iter = g_PluginMngr._begin(); iter != g_PluginMngr._end(); iter++) { \
		_Xpl = (*iter); \
		if (_Xpl->m_Id == plid) \
			continue; \
		for (event=_Xpl->m_Events.begin(); event!=_Xpl->m_Events.end(); event++) { \
			api = (*event); \
			api->evn(plid); \
		} \
	}

CPluginManager g_PluginMngr;

MetamodVersionInfo GlobVersionInfo = 
{
	METAMOD_API_MAJOR,
	METAMOD_API_MINOR,
	SH_IFACE_VERSION,
	SH_IMPL_VERSION,
	PLAPI_MIN_VERSION,
	METAMOD_PLAPI_VERSION,
	SOURCE_ENGINE_UNKNOWN,
	NULL
};

CPluginManager::CPluginManager()
{
	m_LastId = Pl_MinId;
	m_AllLoaded = false;
}

CPluginManager::~CPluginManager()
{
	SourceHook::List<CNameAlias *>::iterator iter;

	for (iter = m_Aliases.begin(); iter != m_Aliases.end(); iter++)
	{
		delete (*iter);
	}

	m_Aliases.clear();
}

const char *CPluginManager::LookupAlias(const char *alias)
{
	SourceHook::List<CNameAlias *>::iterator iter;
	CNameAlias *p;

	for (iter = m_Aliases.begin(); iter != m_Aliases.end(); iter++)
	{
		p = (*iter);
		if (p->alias.compare(alias) == 0)
		{
			return p->value.c_str();
		}
	}

	return NULL;
}

SourceHook::List<CNameAlias *>::iterator CPluginManager::_alias_begin()
{
	return m_Aliases.begin();
}

SourceHook::List<CNameAlias *>::iterator CPluginManager::_alias_end()
{
	return m_Aliases.end();
}

void CPluginManager::SetAlias(const char *alias, const char *value)
{
	SourceHook::List<CNameAlias *>::iterator iter;
	CNameAlias *p;

	for (iter = m_Aliases.begin(); iter != m_Aliases.end(); iter++)
	{
		p = (*iter);
		if (p->alias.compare(alias) == 0)
		{
			if (value[0] == '\0')
			{
				iter = m_Aliases.erase(iter);
				return;
			}
			else
			{
				p->value.assign(value);
				return;
			}
		}
	}

	if (value[0] != '\0')
	{
		p = new CNameAlias;
	
		p->alias.assign(alias);
		p->value.assign(value);
	
		m_Aliases.push_back(p);
	}
}

CPluginManager::CPlugin::CPlugin() : m_Id(0), m_Source(0), m_API(NULL), m_Lib(NULL), m_UnloadFn(NULL)
{

}

PluginId CPluginManager::Load(const char *file, PluginId source, bool &already, char *error, size_t maxlen)
{
	already = false;
	//Check if we're about to reload an old plugin
	PluginIter i = m_Plugins.begin();
	while (i != m_Plugins.end())
	{
		if ((*i) && UTIL_PathCmp(file, (*i)->m_File.c_str()))
		{
			if ((*i)->m_Status < Pl_Paused)
			{
				//Attempt to load the plugin again
				already = true;
				i = m_Plugins.erase(i);
				continue;
			}
			else
			{
				//No need to load it
				already = true;
				return (*i)->m_Id;
			}
		}
		i++;
	}

	CPlugin *pl = _Load(file, source, error, maxlen);

	if (!pl)
	{
		return Pl_BadLoad;
	}

	ITER_PLEVENT(OnPluginLoad, pl->m_Id);

	return pl->m_Id;
}

CPluginManager::CPlugin *CPluginManager::FindById(PluginId id)
{
	PluginIter i;

	for (i=m_Plugins.begin(); i!=m_Plugins.end(); i++)
	{
		if ( (*i)->m_Id == id )\
		{
			return (*i);
		}
	}

	return NULL;
}

void CPluginManager::SetAllLoaded()
{
	if (m_AllLoaded)
	{
		return;
	}

	m_AllLoaded = true;
	PluginIter i;

	for (i=m_Plugins.begin(); i!=m_Plugins.end(); i++)
	{
		if ( (*i) && (*i)->m_Status == Pl_Running && (*i)->m_API )
		{
			//API 4 is when we added this callback
			//Min version is now 5, so we ignore this check
			//if ( (*i)->m_API->GetApiVersion() >= 004 )
			(*i)->m_API->AllPluginsLoaded();
		}
	}
}

bool CPluginManager::Pause(PluginId id, char *error, size_t maxlen)
{
	CPlugin *pl = FindById(id);
	
	if (!pl)
	{
		UTIL_Format(error, maxlen, "Plugin id not found");
		return false;
	}

	bool ret;
	
	if ((ret=_Pause(pl, error, maxlen)) == true)
	{
		ITER_PLEVENT(OnPluginPause, pl->m_Id);
	}

	return ret;
}

bool CPluginManager::Unpause(PluginId id, char *error, size_t maxlen)
{
	CPlugin *pl = FindById(id);
	
	if (!pl)
	{
		UTIL_Format(error, maxlen, "Plugin id not found");
		return false;
	}

	bool ret;
	
	if ( (ret=_Unpause(pl, error, maxlen)) == true )
	{
		ITER_PLEVENT(OnPluginUnpause, pl->m_Id);
	}

	return ret;
}

bool CPluginManager::Unload(PluginId id, bool force, char *error, size_t maxlen)
{
	CPlugin *pl = FindById(id);
	
	if (!pl)
	{
		UTIL_Format(error, maxlen, "Plugin %d not found", id);
		return false;
	}

	bool ret;
	PluginId old_id = pl->m_Id;
	if ( (ret=_Unload(pl, force, error, maxlen)) == true )
	{
		ITER_PLEVENT(OnPluginUnload, old_id);
	}

	return ret;
}

bool CPluginManager::Retry(PluginId id, char *error, size_t len)
{
	PluginIter i;
	char buffer[64];
	for (i=m_Plugins.begin(); i!=m_Plugins.end(); i++)
	{
		if ( (*i) && (*i)->m_Id == id )
		{
			if ( (*i)->m_Status >= Pl_Paused)
			{
				UTIL_Format(error, len, "Plugin %d is already running.", id);
				return false;
			}
			CPlugin *pl = _Load((*i)->m_File.c_str(), Pl_Console, error, len);
			if (!pl)
			{
				return false;
			}
			if (pl->m_Status >= Pl_Paused)
			{
				//Now it gets crazy... unload the original copy.
				_Unload( (*i), true, buffer, sizeof(buffer)-1 );
				
				//Set the new copy's id
				pl->m_Id = id;

				//We just wasted an id... reclaim it
				m_LastId--;
				
				return true;
			}
			else
			{
				//don't really care about the buffer here
				_Unload(pl, true, buffer, sizeof(buffer)-1);

				//We just wasted an id... reclaim it
				m_LastId--;
				return false;
			}
		}
	}

	UTIL_Format(error, len, "Plugin %d not found,", id);
	return false;
}

CPluginManager::CPlugin *CPluginManager::FindByAPI(ISmmPlugin *api)
{
	PluginIter i;

	//don't find bad plugins!
	if (!api)
	{
		return NULL;
	}

	for (i=m_Plugins.begin(); i!=m_Plugins.end(); i++)
	{
		if ((*i)->m_API == api)
		{
			return (*i);
		}
	}

	return NULL;
}

int CPluginManager::GetPluginCount()
{
	return (int)m_Plugins.size();
}

const char *CPluginManager::GetStatusText(CPlugin *pl)
{
	switch (pl->m_Status)
	{
	case Pl_NotFound:
		return "NOFILE";
	case Pl_Error:
		return "ERROR";
	case Pl_Refused:
		return "FAILED";
	case Pl_Paused:
		return "PAUSED";
	case Pl_Running:
		{
			if (pl->m_API && pl->m_API->QueryRunning(NULL, 0))
			{
				return "STOPPED";
			} else {
				return "RUNNING";
			}
		}
	default:
		return "-";
	}
}

struct Unloader : public SourceHook::Impl::UnloadListener
{
	CPluginManager::CPlugin *plugin_;
	bool destroy_;

	Unloader(CPluginManager::CPlugin *plugin, bool destroy)
	  : plugin_(plugin), destroy_(destroy)
	{ }

	void ReadyToUnload(SourceHook::Plugin plug)
	{
		if (plugin_->m_UnloadFn != NULL)
			plugin_->m_UnloadFn();

		dlclose(plugin_->m_Lib);

		if (destroy_)
		{
			delete plugin_;
		}
		else
		{
			plugin_->m_Lib = NULL;
			plugin_->m_API = NULL;
		}

		delete this;
	}
};

CPluginManager::CPlugin *CPluginManager::_Load(const char *file, PluginId source, char *error, size_t maxlen)
{
	FILE *fp;
	CPlugin *pl;

	pl = new CPlugin();
	*error = '\0';
	
	//Add plugin to list
	pl->m_Id = m_LastId;
	pl->m_File.assign(file);
	m_Plugins.push_back(pl);
	m_LastId++;

	//Check if the file even exists
	fp = fopen(file, "r");
	if (!fp)
	{
		if (error)
		{
			UTIL_Format(error, maxlen, "File not found: %s", file);
		}
		pl->m_Status = Pl_NotFound;
	}

	if (fp)
	{
		fclose(fp);
		fp = NULL;
		
		//Load the file
		pl->m_Lib = dlmount(file);
		if (!pl->m_Lib)
		{
			if (error)
			{
				UTIL_Format(error, maxlen, "%s", dlerror());
			}
			pl->m_Status = Pl_Error;
		}
		else
		{
			pl->m_API = NULL;
			
			/**
			 * First, try the new "advanced" interface
			 */
			METAMOD_FN_LOAD fnLoad = (METAMOD_FN_LOAD)dlsym(pl->m_Lib, "CreateInterface_MMS");
			if (fnLoad != NULL)
			{
				if (GlobVersionInfo.source_engine == SOURCE_ENGINE_UNKNOWN)
				{
					GlobVersionInfo.source_engine = g_Metamod.GetSourceEngineBuild();
				}
				if (GlobVersionInfo.game_dir == NULL)
				{
					GlobVersionInfo.game_dir = strrchr(g_Metamod.GetBaseDir(), PATH_SEP_CHAR) + 1;
				}

				/* Build path information */
				char file_path[256];
				size_t len = g_Metamod.PathFormat(file_path, sizeof(file_path), "%s", file);

				for (size_t i = len - 1; i < len; i--)
				{
					if (_IsPathSepChar(file_path[i]))
					{
						file_path[i] = '\0';
						break;
					}
				}

				MetamodLoaderInfo info;
				info.pl_file = file;
				info.pl_path = file_path;

				pl->m_API = fnLoad(&GlobVersionInfo, &info);
#if SOURCE_ENGINE == SE_CSS || SOURCE_ENGINE == SE_HL2DM || SOURCE_ENGINE == SE_DODS || SOURCE_ENGINE == SE_TF2
				/* For plugin compat - try loading again using original OB if OB-Valve has failed */
				if (pl->m_API == NULL)
				{
					GlobVersionInfo.source_engine = SOURCE_ENGINE_ORANGEBOXVALVE_DEPRECATED;
					pl->m_API = fnLoad(&GlobVersionInfo, &info);
					if (pl->m_API == NULL)
					{
						GlobVersionInfo.source_engine = SOURCE_ENGINE_ORANGEBOX;
						pl->m_API = fnLoad(&GlobVersionInfo, &info);
					}
					GlobVersionInfo.source_engine = g_Metamod.GetSourceEngineBuild();
				}
#endif
				pl->m_UnloadFn = (METAMOD_FN_UNLOAD)dlsym(pl->m_Lib, "UnloadInterface_MMS");
			}

			/**
			 * If we didn't get anything, try the normal/simple interface.
			 */
			if (pl->m_API == NULL)
			{
				CreateInterfaceFn pfn = (CreateInterfaceFn)(dlsym(pl->m_Lib, PL_EXPOSURE_C));
				if (!pfn)
				{
					if (error)
					{
						UTIL_Format(error, maxlen, "Function %s not found", PL_EXPOSURE_C);
					}
					pl->m_Status = Pl_Error;
				}
				else
				{
					pl->m_API = static_cast<ISmmPlugin *>((pfn)(METAMOD_PLAPI_NAME, NULL));

					if (!pl->m_API)
					{
						if (error)
						{
							UTIL_Format(error, maxlen, "Failed to get API");
						}
						pl->m_Status = Pl_Error;
					}
				}
			}

			if (pl->m_API != NULL)
			{
				int api = pl->m_API->GetApiVersion();
				if (api < PLAPI_MIN_VERSION)
				{
					if (error)
					{
						if (api == 13)
						{
							UTIL_Format(error, maxlen, "Plugin uses experimental Metamod build, probably 1.6.x (%d < %d)", api, PLAPI_MIN_VERSION);
						}
						else if (api <= 12 && api >= 7)
						{
							UTIL_Format(error, maxlen, "Older Metamod version required, probably 1.4.x (%d < %d)", api, PLAPI_MIN_VERSION);
						}
						else
						{
							UTIL_Format(error, maxlen, "Older Metamod version required, probably 1.0 (%d < %d)", api, PLAPI_MIN_VERSION);
						}
					}
					pl->m_Status = Pl_Error;
				}
				else if (api > METAMOD_PLAPI_VERSION)
				{
					if (error)
					{
						UTIL_Format(error, maxlen, "Plugin requires newer Metamod version (%d > %d)", api, METAMOD_PLAPI_VERSION);
					}
					pl->m_Status = Pl_Error;
				}
				else
				{
					if (pl->m_API->Load(pl->m_Id, &g_Metamod, error, maxlen, m_AllLoaded))
					{
						pl->m_Status = Pl_Running;
						if (m_AllLoaded)
						{
							pl->m_API->AllPluginsLoaded();
						}
					}
					else
					{
						pl->m_Status = Pl_Refused;
					}
				}
			}
		}
	}

	if (pl->m_Lib && (pl->m_Status < Pl_Paused))
	{
		pl->m_Events.clear();
		UnregAllConCmds(pl);
		g_SourceHook.UnloadPlugin(pl->m_Id, new Unloader(pl, false));
	}

	return pl;
}

bool CPluginManager::_Unload(CPluginManager::CPlugin *pl, bool force, char *error, size_t maxlen)
{
	if (error)
	{
		*error = '\0';
	}

	if (pl->m_API && pl->m_Lib)
	{
		//Note, we'll always tell the plugin it will be unloading...
		if (pl->m_API->Unload(error, maxlen) || force)
		{
			pl->m_Events.clear();
			UnregAllConCmds(pl);

			//Remove the plugin from the list
			PluginIter i;
			for (i=m_Plugins.begin(); i!=m_Plugins.end(); i++)
			{
				if ( (*i)->m_Id == pl->m_Id )
				{
					i = m_Plugins.erase(i);
					break;
				}
			}

			//Make sure to detach it from sourcehook!
			g_SourceHook.UnloadPlugin(pl->m_Id, new Unloader(pl, true));
			return true;
		}
	} else {
		//The plugin is not valid, and let's just remove it from the list anyway
		PluginIter i;
		for (i=m_Plugins.begin(); i!=m_Plugins.end(); i++)
		{
			if ( (*i)->m_Id == pl->m_Id )
			{
				i = m_Plugins.erase(i);
				break;
			}
		}
		delete pl;

		return true;
	}

	return false;
}

bool CPluginManager::_Pause(CPluginManager::CPlugin *pl, char *error, size_t maxlen)
{
	if (error)
	{
		*error = '\0';
	}

	if (pl->m_Status != Pl_Running || !pl->m_API)
	{
		if (error)
		{
			UTIL_Format(error, maxlen, "Plugin cannot be paused");
		}
	}
	else
	{
		if (pl->m_API->Pause(error, maxlen))
		{
			g_SourceHook.PausePlugin(pl->m_Id);
			pl->m_Status = Pl_Paused;

			return true;
		}
	}

	return false;
}

bool CPluginManager::_Unpause(CPluginManager::CPlugin *pl, char *error, size_t maxlen)
{
	if (error)
	{
		*error = '\0';
	}

	if (pl->m_Status != Pl_Paused || !pl->m_API)
	{
		if (error)
		{
			UTIL_Format(error, maxlen, "Plugin cannot be unpaused");
		}
	}
	else
	{
		if (pl->m_API->Unpause(error, maxlen))
		{
			g_SourceHook.UnpausePlugin(pl->m_Id);
			pl->m_Status = Pl_Running;

			return true;
		}
	}

	return false;
}

bool CPluginManager::UnloadAll()
{
	PluginIter i;

	char error[128];
	bool status = true;

	while ((i = m_Plugins.begin()) != m_Plugins.end())
	{
		if ( !_Unload( (*i), true, error, sizeof(error)) )
		{
			status = false;
		}
	}

	return status;
}

bool CPluginManager::Query(PluginId id, const char **file, Pl_Status *status, PluginId *source)
{
	CPlugin *pl = FindById(id);

	if (!pl)
	{
		return false;
	}

	if (file != NULL)
	{
		*file = pl->m_File.c_str();
	}

	if (status != NULL)
	{
		*status = pl->m_Status;
	}

	if (source != NULL)
	{
		*source = pl->m_Source;
	}

	return true;
}

bool CPluginManager::QueryRunning(PluginId id, char *error, size_t maxlength)
{
	CPlugin *pl = FindById(id);

	if (!pl || !pl->m_API)
	{
		if (error)
		{
			UTIL_Format(error, maxlength, "Plugin not valid");
		}
		return false;
	}

	return pl->m_API->QueryRunning(error, maxlength);
}

bool CPluginManager::QueryHandle(PluginId id, void **handle)
{
	CPlugin *pl = FindById(id);

	if (!pl)
	{
		return false;
	}

	if (handle)
	{
		*handle = static_cast<void *>(pl->m_Lib);
	}

	return true;
}

PluginIter CPluginManager::_begin()
{
	return m_Plugins.begin();
}

PluginIter CPluginManager::_end()
{
	return m_Plugins.end();
}

void CPluginManager::AddPluginCvar(ISmmPlugin *api, ConCommandBase *pCvar)
{
	CPlugin *pl = FindByAPI(api);

	if (!pl)
	{
		return;
	}

	pl->m_Cvars.push_back(pCvar);
}

void CPluginManager::AddPluginCmd(ISmmPlugin *api, ConCommandBase *pCmd)
{
	CPlugin *pl = FindByAPI(api);

	if (!pl)
	{
		return;
	}

	pl->m_Cmds.push_back(pCmd);
}

void CPluginManager::RemovePluginCvar(ISmmPlugin *api, ConCommandBase *pCvar)
{
	CPlugin *pl = FindByAPI(api);

	if (!pl)
	{
		return;
	}

	pl->m_Cvars.remove(pCvar);
}

void CPluginManager::RemovePluginCmd(ISmmPlugin *api, ConCommandBase *pCmd)
{
	CPlugin *pl = FindByAPI(api);

	if (!pl)
	{
		return;
	}

	pl->m_Cmds.remove(pCmd);
}

void CPluginManager::UnregAllConCmds(CPlugin *pl)
{
	SourceHook::List<ConCommandBase *>::iterator i;

	/* :TODO: */
	for (i=pl->m_Cvars.begin(); i!=pl->m_Cvars.end(); i++)
	{
		g_Metamod.UnregisterConCommandBase(pl->m_Id, (*i) );
	}
	pl->m_Cvars.clear();

	for (i=pl->m_Cmds.begin(); i!=pl->m_Cmds.end(); i++)
	{
		g_Metamod.UnregisterConCommandBase(pl->m_Id, (*i) );
	}
	pl->m_Cmds.clear();
}

