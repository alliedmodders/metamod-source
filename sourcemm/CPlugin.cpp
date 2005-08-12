/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
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

/** 
 * @brief Implements functions from CPlugin.h
 * @file CPlugin.cpp
 */

using namespace SourceMM;

CPluginManager g_PluginMngr;

CPluginManager::CPluginManager()
{
	m_LastId = Pl_MinId;
	m_AllLoaded = false;
}

CPluginManager::~CPluginManager()
{
	/*if (m_Plugins.size())
	{
		UnloadAll();
		m_Plugins.clear();
	}*/
}

CPluginManager::CPlugin::CPlugin() : m_Lib(NULL), m_API(NULL), m_Id(0), m_Source(0)
{
	memset(&fac_list, 0, sizeof(factories));
}

PluginId CPluginManager::Load(const char *file, PluginId source, bool &already, char *error, size_t maxlen)
{
	PluginIter i;

	already = false;
	//Check if we're about to reload an old plugin
	for (i=m_Plugins.begin(); i!=m_Plugins.end(); i++)
	{
		if ( (*i) && (*i)->m_File.compare(file)==0 )
		{
			if ( (*i)->m_Status < Pl_Paused )
			{
				//Attempt to load the plugin again
				already = true;
				i = m_Plugins.erase(i);
			} else {
				//No need to load it
				already = true;
				return (*i)->m_Id;
			}
		}
	}

	CPlugin *pl = _Load(file, source, error, maxlen);

	if (!pl)
		return Pl_BadLoad;

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

bool CPluginManager::Pause(PluginId id, char *error, size_t maxlen)
{
	CPlugin *pl = FindById(id);
	
	if (!pl)
	{
		snprintf(error, maxlen, "Plugin id not found");
		return false;
	}

	return _Pause(pl, error, maxlen);
}

bool CPluginManager::Unpause(PluginId id, char *error, size_t maxlen)
{
	CPlugin *pl = FindById(id);
	
	if (!pl)
	{
		snprintf(error, maxlen, "Plugin id not found");
		return false;
	}

	return _Unpause(pl, error, maxlen);
}

bool CPluginManager::Unload(PluginId id, bool force, char *error, size_t maxlen)
{
	CPlugin *pl = FindById(id);
	
	if (!pl)
	{
		snprintf(error, maxlen, "Plugin %d not found", id);
		return false;
	}

	return _Unload(pl, force, error, maxlen);
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
				snprintf(error, len, "Plugin %d is already running.", id);
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

	snprintf(error, len, "Plugin %d not found,", id);
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
			snprintf(error, maxlen, "File not found: %s", file);
		pl->m_Status = Pl_NotFound;
	} else {
		fclose(fp);
		fp = NULL;
		
		//Load the file
		pl->m_Lib = dlmount(file);
		if (!pl->m_Lib)
		{
			if (error)
				snprintf(error, maxlen, "%s", dlerror());
			pl->m_Status = Pl_Error;
		} else {
			CreateInterfaceFn pfn = reinterpret_cast<CreateInterfaceFn>(dlsym(pl->m_Lib, PL_EXPOSURE_C));
			if (!pfn)
			{
				if (error)
					snprintf(error, maxlen, "Function %s not found", PL_EXPOSURE_C);
				pl->m_Status = Pl_Error;
			} else {
				pl->m_API = static_cast<ISmmPlugin *>((pfn)(PLAPI_NAME, NULL));
				if (!pl->m_API)
				{
					if (error)
						snprintf(error, maxlen, "Failed to get API");
					pl->m_Status = Pl_Error;
				} else {
					int api = pl->m_API->GetApiVersion();
					if (api < PLAPI_MIN_VERSION)
					{
						if (error)
							snprintf(error, maxlen, "Plugin API %d is out of date with required minimum (%d)", api, PLAPI_MIN_VERSION);
						pl->m_Status = Pl_Error;
					} else if (api > PLAPI_VERSION) {
						if (error)
							snprintf(error, maxlen, "Plugin API %d is newer than internal version (%d)", api, PLAPI_VERSION);
						pl->m_Status = Pl_Error;
					} else {
						if (pl->m_API->Load(pl->m_Id, static_cast<ISmmAPI *>(&g_SmmAPI), &(pl->fac_list), error, maxlen))
						{
							pl->m_Status = Pl_Running;
							if (m_AllLoaded)
							{
								//API 4 is when we added this callback
								//Removing this code as the min version is now 5
								//if (pl->m_API->GetApiVersion() >= 4)
								pl->m_API->AllPluginsLoaded();
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
			snprintf(error, maxlen, "Plugin cannot be paused");
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
			snprintf(error, maxlen, "Plugin cannot be unpaused");
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

	List<SourceMM::CPluginManager::CPlugin *> remqueue;

	for (i=m_Plugins.begin(); i!=m_Plugins.end(); i++)
		remqueue.push_back( (*i) );

	char error[128];
	bool status = true;

	for (i=remqueue.begin(); i!=remqueue.end(); i++)
	{
		if ( !_Unload( (*i), true, error, sizeof(error)-1) )
			status = false;
	}

	m_Plugins.clear();
	remqueue.clear();

	return status;
}

bool CPluginManager::Query(PluginId id, const char *&file, factories *&list, Pl_Status &status, PluginId &source)
{
	CPlugin *pl = FindById(id);

	if (!pl)
		return false;

	file = pl->m_File.c_str();
	list = &(pl->fac_list);
	status = pl->m_Status;
	source = pl->m_Source;

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
	List<ConCommandBase *>::iterator i;

	for (i=pl->m_Cvars.begin(); i!=pl->m_Cvars.end(); i++)
		g_SMConVarAccessor.Unregister( (*i) );

	pl->m_Cvars.clear();

	for (i=pl->m_Cmds.begin(); i!=pl->m_Cmds.end(); i++)
		g_SMConVarAccessor.Unregister( (*i) );

	pl->m_Cmds.clear();
}
