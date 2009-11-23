/* ======== SourceMM ========
 * Copyright (C) 2004-2009 Metamod:Source Development Team
 * No warranties of any kind
 *
 * License: zlib/libpng
 *
 * Author(s): David "BAILOPAN" Anderson
 * ============================
 */

#include "CPlugin.h"
#include "CSmmAPI.h"
#include "sourcemm.h"
#include "concommands.h"
#include "util.h"

/** 
 * @brief Implements functions from CPlugin.h
 * @file CPlugin.cpp
 */

using namespace SourceMM;

#define ITER_PLEVENT(evn, plid) \
	CPluginManager::CPlugin *_Xpl; \
	SourceHook::List<CPluginEventHandler>::iterator event; \
	IMetamodListener *api; \
	for (PluginIter iter = g_PluginMngr._begin(); iter != g_PluginMngr._end(); iter++) { \
		_Xpl = (*iter); \
		if (_Xpl->m_Id == plid) \
			continue; \
		for (event=_Xpl->m_Events.begin(); event!=_Xpl->m_Events.end(); event++) { \
			api = (*event).event; \
			api->evn(plid); \
		} \
	}

CPluginManager g_PluginMngr;

void NotifyConCommandBaseDrop(PluginId id, ConCommandBase *base)
{
	CPluginManager::CPlugin *pl;
	SourceHook::List<CPluginEventHandler>::iterator event;
	IMetamodListener *api;
	for (PluginIter iter = g_PluginMngr._begin(); iter != g_PluginMngr._end(); iter++)
	{
		pl = (*iter);
		if (pl->m_Status < Pl_Paused)
			continue;
		if (pl->m_API->GetApiVersion() < 11)
			continue;
		for (event = pl->m_Events.begin(); event != pl->m_Events.end(); event++)
		{
			api = (*event).event;
			api->OnUnlinkConCommandBase(id, base);
		}
	}
}

CPluginManager::CPluginManager()
{
	m_LastId = Pl_MinId;
	m_AllLoaded = false;
}

CPluginManager::~CPluginManager()
{
	SourceHook::List<CNameAlias *>::iterator iter;

	for (iter=m_Aliases.begin(); iter!=m_Aliases.end(); iter++)
	{
		delete (*iter);
	}

	m_Aliases.clear();
}

const char *CPluginManager::LookupAlias(const char *alias)
{
	SourceHook::List<CNameAlias *>::iterator iter;
	CNameAlias *p;

	for (iter=m_Aliases.begin(); iter!=m_Aliases.end(); iter++)
	{
		p = (*iter);
		if (p->alias.compare(alias) == 0)
		{
			return p->value.c_str();
		}
	}

	return NULL;
}

SourceHook::List<SourceMM::CNameAlias *>::iterator CPluginManager::_alias_begin()
{
	return m_Aliases.begin();
}

SourceHook::List<SourceMM::CNameAlias *>::iterator CPluginManager::_alias_end()
{
	return m_Aliases.end();
}

void CPluginManager::SetAlias(const char *alias, const char *value)
{
	SourceHook::List<CNameAlias *>::iterator iter;
	CNameAlias *p;

	for (iter=m_Aliases.begin(); iter!=m_Aliases.end(); iter++)
	{
		p = (*iter);
		if (p->alias.compare(alias) == 0)
		{
			if (value[0] == '\0')
			{
				iter = m_Aliases.erase(iter);
				return;
			} else {
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

CPluginManager::CPlugin::CPlugin() : m_Id(0), m_Source(0), m_API(NULL), m_Lib(NULL)
{

}

PluginId CPluginManager::Load(const char *file, PluginId source, bool &already, char *error, size_t maxlen)
{
	already = false;
	//Check if we're about to reload an old plugin
	PluginIter i = m_Plugins.begin();
	while (i != m_Plugins.end())
	{
		if ( (*i) && UTIL_PathCmp(file, (*i)->m_File.c_str()) )
		{
			if ( (*i)->m_Status < Pl_Paused )
			{
				//Attempt to load the plugin again
				already = true;
				i = m_Plugins.erase(i);
				continue;
			} else {
				//No need to load it
				already = true;
				return (*i)->m_Id;
			}
		}
		i++;
	}

	CPlugin *pl = _Load(file, source, error, maxlen);

	if (!pl)
		return Pl_BadLoad;

	ITER_PLEVENT(OnPluginLoad, pl->m_Id);

	return pl->m_Id;
}

CPluginManager::CPlugin *CPluginManager::FindById(PluginId id)
{
	PluginIter i;

	for (i=m_Plugins.begin(); i!=m_Plugins.end(); i++)
	{
		if ( (*i)->m_Id == id )
			return (*i);
	}

	return NULL;
}

void CPluginManager::SetAllLoaded()
{
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

void CPluginManager::SetVSPAsLoaded()
{
	PluginIter i;
	CPlugin *pPlugin;
	SourceHook::List<CPluginEventHandler>::iterator event;

	for (i = m_Plugins.begin(); i != m_Plugins.end(); i++)
	{
		pPlugin = (*i);
		if (pPlugin->m_Status < Pl_Paused)
		{
			continue;
		}
		/* Only valid for plugins >= 10 (v1:5, SourceMM 1.4) */
		if (pPlugin->m_API->GetApiVersion() < 10)
		{
			continue;
		}
		for (event = pPlugin->m_Events.begin();
			 event != pPlugin->m_Events.end();
			 event++)
		{
			if ((*event).got_vsp)
			{
				continue;
			}
			(*event).got_vsp = true;
			(*event).event->OnVSPListening(g_pRealVspCallbacks);
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
	
	if ( (ret=_Pause(pl, error, maxlen)) == true )
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
				return false;
			if (pl->m_Status >= Pl_Paused)
			{
				//Now it gets crazy... unload the original copy.
				_Unload( (*i), true, buffer, sizeof(buffer)-1 );
				
				//Set the new copy's id
				pl->m_Id = id;

				//We just wasted an id... reclaim it
				m_LastId--;
				
				return true;
			} else {
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
		return NULL;

	for (i=m_Plugins.begin(); i!=m_Plugins.end(); i++)
	{
		if ( (*i)->m_API == api )
			return (*i);
	}

	return NULL;
}

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
			UTIL_Format(error, maxlen, "File not found: %s", file);
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
				UTIL_Format(error, maxlen, "[%d]", GetLastError());
			pl->m_Status = Pl_Error;
		} else {
			CreateInterfaceFn pfn = (CreateInterfaceFn)(dlsym(pl->m_Lib, PL_EXPOSURE_C));
			if (!pfn)
			{
				if (error)
					UTIL_Format(error, maxlen, "Function %s not found", PL_EXPOSURE_C);
				pl->m_Status = Pl_Error;
			} else {
				pl->m_API = static_cast<ISmmPlugin *>((pfn)(PLAPI_NAME, NULL));
				if (!pl->m_API)
				{
					if (error)
						UTIL_Format(error, maxlen, "Failed to get API");
					pl->m_Status = Pl_Error;
				} else {
					int api = pl->m_API->GetApiVersion();
					if (api < PLAPI_MIN_VERSION)
					{
						if (error)
							UTIL_Format(error, maxlen, "Plugin API %d is out of date with required minimum (%d)", api, PLAPI_MIN_VERSION);
						pl->m_Status = Pl_Error;
					} else if (api > PLAPI_VERSION) {
						if (error)
							UTIL_Format(error, maxlen, "Plugin API %d is newer than internal version (%d)", api, PLAPI_VERSION);
						pl->m_Status = Pl_Error;
					} else {
						if (pl->m_API->Load(pl->m_Id, static_cast<ISmmAPI *>(&g_SmmAPI), error, maxlen, m_AllLoaded))
						{
							pl->m_Status = Pl_Running;
							if (m_AllLoaded)
							{
								//API 4 is when we added this callback
								//Removing this code as the min version is now 5
								//if (pl->m_API->GetApiVersion() >= 4)
								pl->m_API->AllPluginsLoaded();
							}
							if (g_bIsBridgedAsVsp)
							{
								SourceHook::List<CPluginEventHandler>::iterator event;
								for (event = pl->m_Events.begin();
									 event != pl->m_Events.end(); 
									 event++)
								{
									if (pl->m_API->GetApiVersion() < 10 || (*event).got_vsp)
										continue;
									(*event).got_vsp = true;
									(*event).event->OnVSPListening(g_pRealVspCallbacks);
								}
							}
						} else {
							pl->m_Status = Pl_Refused;
						}
					}
				}
			}
		}
	}

	if (pl->m_Lib && (pl->m_Status < Pl_Paused))
	{
		pl->m_Events.clear();
		g_SourceHook.UnloadPlugin(pl->m_Id);
		UnregAllConCmds(pl);

		dlclose(pl->m_Lib);
		pl->m_Lib = NULL;
		pl->m_API = NULL;
	}

	return pl;
}

bool CPluginManager::_Unload(CPluginManager::CPlugin *pl, bool force, char *error, size_t maxlen)
{
	if (error)
		*error = '\0';
	if (pl->m_API && pl->m_Lib)
	{
		//Note, we'll always tell the plugin it will be unloading...
		if (pl->m_API->Unload(error, maxlen) || force)
		{
			//Make sure to detach it from sourcehook!
			g_SourceHook.UnloadPlugin(pl->m_Id);

			UnregAllConCmds(pl);

			//Clean up the DLL
			dlclose(pl->m_Lib);
			pl->m_Lib = NULL;
			pl->m_API = NULL;

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
			//Free its memory
			delete pl;

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
		*error = '\0';
	if (pl->m_Status != Pl_Running || !pl->m_API)
	{
		if (error)
			UTIL_Format(error, maxlen, "Plugin cannot be paused");
	} else {
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
		*error = '\0';

	if (pl->m_Status != Pl_Paused || !pl->m_API)
	{
		if (error)
			UTIL_Format(error, maxlen, "Plugin cannot be unpaused");
	} else {
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

bool CPluginManager::Query(PluginId id, const char *&file, Pl_Status &status, PluginId &source)
{
	CPlugin *pl = FindById(id);

	if (!pl)
		return false;

	file = pl->m_File.c_str();
	status = pl->m_Status;
	source = pl->m_Source;

	return true;
}

bool CPluginManager::QueryRunning(PluginId id, char *error, size_t maxlength)
{
	CPlugin *pl = FindById(id);

	if (!pl || !pl->m_API)
	{
		if (error)
			UTIL_Format(error, maxlength, "Plugin not valid");
		return false;
	}

	return pl->m_API->QueryRunning(error, maxlength);
}

bool CPluginManager::QueryHandle(PluginId id, void *&handle)
{
	CPlugin *pl = FindById(id);

	if (!pl)
	{
		return false;
	}

	handle = static_cast<void *>(pl->m_Lib);

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
		return;

	pl->m_Cvars.push_back(pCvar);
}

void CPluginManager::AddPluginCmd(ISmmPlugin *api, ConCommandBase *pCmd)
{
	CPlugin *pl = FindByAPI(api);

	if (!pl)
		return;

	pl->m_Cmds.push_back(pCmd);
}

void CPluginManager::RemovePluginCvar(ISmmPlugin *api, ConCommandBase *pCvar)
{
	CPlugin *pl = FindByAPI(api);

	if (!pl)
		return;

	pl->m_Cvars.remove(pCvar);
}

void CPluginManager::RemovePluginCmd(ISmmPlugin *api, ConCommandBase *pCmd)
{
	CPlugin *pl = FindByAPI(api);

	if (!pl)
		return;

	pl->m_Cmds.remove(pCmd);
}

void CPluginManager::UnregAllConCmds(CPlugin *pl)
{
	SourceHook::List<ConCommandBase *>::iterator i;

	for (i = pl->m_Cvars.begin(); i != pl->m_Cvars.end(); i++)
	{
		NotifyConCommandBaseDrop(pl->m_Id, (*i));
		g_SMConVarAccessor.Unregister((*i));
	}

	pl->m_Cvars.clear();

	for (i = pl->m_Cmds.begin(); i != pl->m_Cmds.end(); i++)
	{
		NotifyConCommandBaseDrop(pl->m_Id, (*i));
		g_SMConVarAccessor.Unregister((*i));
	}

	pl->m_Cmds.clear();
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

unsigned int CPluginManager::GetPluginCount()
{
	return (unsigned int)m_Plugins.size();
}
