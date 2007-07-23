/* ======== SourceMM ========
 * Copyright (C) 2004-2007 Metamod:Source Development Team
 * No warranties of any kind
 *
 * License: zlib/libpng
 *
 * Author(s): David "BAILOPAN" Anderson
 * ============================
 */

#include <ctype.h>
#include "CSmmAPI.h"
#include "concommands.h"
#include "CPlugin.h"
#include "sh_string.h"
#include "sh_list.h"

/**
 * @brief Console Command Implementations
 * @file concommands.cpp
 */

CAlwaysRegisterableCommand g_EternalCommand;
SMConVarAccessor g_SMConVarAccessor;

bool SMConVarAccessor::RegisterConCommandBase(ConCommandBase *pCommand)
{
	// Add the FCVAR_GAMEDLL flag 
	// => No crash on exit!
	// UPDATE: Do _not_ add the FCVAR_GAMEDLL flag here, as it
	// causes the command to be unusable on listenservers until you load a map
	// We will set the FCVAR_GAMEDLL flag on all commands we have registered once we are being unloaded
	//pCommand->AddFlags(FCVAR_GAMEDLL);
	m_RegisteredCommands.push_back(pCommand);

	pCommand->SetNext( NULL );
	g_Engine.icvar->RegisterConCommandBase(pCommand);

	return true;
}

bool SMConVarAccessor::Register(ConCommandBase *pCommand)
{
	//simple, don't mark as part of sourcemm!
	pCommand->SetNext( NULL );
	g_Engine.icvar->RegisterConCommandBase(pCommand);

	return true;
}

void SMConVarAccessor::MarkCommandsAsGameDLL()
{
	for (SourceHook::List<ConCommandBase*>::iterator iter = m_RegisteredCommands.begin();
		iter != m_RegisteredCommands.end(); ++iter)
	{
		(*iter)->AddFlags(FCVAR_GAMEDLL);
	}
}

void SMConVarAccessor::Unregister(PluginId id, ConCommandBase *pCommand)
{
	/* Notify via IMetamodListener */
	PluginIter iter;
	SourceMM::CPluginManager::CPlugin *pPlugin;
	SourceHook::List<IMetamodListener *>::iterator event;
	IMetamodListener *pML;
	for (iter=g_PluginMngr._begin(); iter!=g_PluginMngr._end(); iter++)
	{
		pPlugin = (*iter);
		if (pPlugin->m_Status < Pl_Paused)
		{
			continue;
		}
		/* Only valid for plugins >= 12 (v1:6, SourceMM 1.5) */
		if (pPlugin->m_API->GetApiVersion() < 12)
		{
			continue;
		}
		for (event=pPlugin->m_Events.begin();
			event!=pPlugin->m_Events.end();
			event++)
		{
			pML = (*event);
			pML->OnUnlinkConCommandBase(id, pCommand);
		}
	}

	ICvar *cv = g_Engine.icvar;
	ConCommandBase *ptr = cv->GetCommands();

	if (ptr == pCommand)
	{
		/* First in list */
		g_EternalCommand.BringToFront();
		g_EternalCommand.SetNext(const_cast<ConCommandBase *>(pCommand->GetNext()));
	} else {
		/* Find us and unregister us */
		ConCommandBase *pPrev = NULL;
		while (ptr)
		{
			if (ptr == pCommand)
				break;
			pPrev = ptr;
			ptr = const_cast<ConCommandBase *>(ptr->GetNext());
		}
		if (pPrev && ptr == pCommand)
		{
			pPrev->SetNext(const_cast<ConCommandBase *>(pCommand->GetNext()));
		}
	}
}

void SMConVarAccessor::UnregisterGameDLLCommands()
{
	ConCommandBase *begin = g_Engine.icvar->GetCommands();
	ConCommandBase *iter = begin;
	ConCommandBase *prev = NULL;
	while (iter)
	{
		/* Watch out for the ETERNAL COMMAND! */
		if (iter != &g_EternalCommand && iter->IsBitSet(FCVAR_GAMEDLL))
		{
			/* Remove it! */
			if (iter == begin)
			{
				g_EternalCommand.BringToFront();
				iter = const_cast<ConCommandBase*>(iter->GetNext());
				g_EternalCommand.SetNext(iter);
				prev = &g_EternalCommand;
				continue;
			}
			else
			{
				iter = const_cast<ConCommandBase*>(iter->GetNext());
				prev->SetNext(iter);
				continue;
			}
		}
		prev = iter;
		iter = const_cast<ConCommandBase*>(iter->GetNext());
	}
}

ConVar metamod_version("metamod_version", SOURCEMM_VERSION, FCVAR_REPLICATED | FCVAR_SPONLY | FCVAR_NOTIFY, "Metamod:Source Version");
#if defined WIN32 || defined _WIN32
ConVar mm_pluginsfile("mm_pluginsfile", "addons\\metamod\\metaplugins.ini", FCVAR_SPONLY, "Metamod:Source Plugins File");
#else
ConVar mm_pluginsfile("mm_pluginsfile", "addons/metamod/metaplugins.ini", FCVAR_SPONLY, "Metamod:Source Plugins File");
#endif

CON_COMMAND(meta, "Metamod:Source Menu")
{
	IVEngineServer *e = g_Engine.engine;

	int args = e->Cmd_Argc();

	if (args >= 2)
	{
		const char *command = e->Cmd_Argv(1);
		if (strcmp(command, "credits") == 0)
		{
			CONMSG("Metamod:Source was developed by:\n");
			CONMSG("  SourceHook: Pavol \"PM OnoTo\" Marko\n");
			CONMSG("  GameDLL/Plugins: David \"BAILOPAN\" Anderson\n");
			CONMSG("  GameDLL: Scott \"Damaged Soul\" Ehlert\n");
			CONMSG("For more information, see the official website\n");
			CONMSG("http://www.sourcemm.net/\n");
			
			return;
		} else if (strcmp(command, "version") == 0) {
			CONMSG("Metamod:Source version %s\n", SOURCEMM_VERSION);
			CONMSG("Compiled on: %s\n", SOURCEMM_DATE);
			CONMSG("Plugin interface version: %d:%d\n", PLAPI_VERSION, PLAPI_MIN_VERSION);
			CONMSG("SourceHook version: %d:%d\n", g_SourceHook.GetIfaceVersion(), g_SourceHook.GetImplVersion());
			CONMSG("http://www.sourcemm.net/\n");

			return;
		} else if (strcmp(command, "game") == 0) {
			CONMSG("GameDLL Information\n");
			CONMSG("  Description: %s\n", g_GameDll.pGameDLL->GetGameDescription());
			CONMSG("  Mod Path: %s\n", g_ModPath.c_str());
			CONMSG("  DLL Path: %s\n", g_BinPath.c_str());
			CONMSG("  Interface: ServerGameDLL%03d, ServerGameClients%03d\n", g_GameDllVersion, g_GameClientsVersion);

			// Display user messages
			if (g_SmmAPI.MsgCacheSuccessful())
			{
				const char *msgname;
				int msgsize;
				int msgcount = g_SmmAPI.GetUserMessageCount();

				if (msgcount > 0)
				{
					CONMSG("  User Messages:  %-32.31s  %-5s  %-5s\n", "Name", "Index", "Size");

					for (int i = 0; i < msgcount; i++)
					{
						msgname = g_SmmAPI.GetUserMessage(i, &msgsize);

						CONMSG("                  %-32.31s  %-5d  %-5d\n", msgname, i, msgsize); 
					}

					CONMSG("  %d user message%s in total\n", msgcount, (msgcount > 1) ? "s" : "");
				} else {
					CONMSG("  User Messages: None\n");
				}
			} else {
				CONMSG("  User Messages: Failed to get list of user messages\n");
			}

			return;
		} else if (strcmp(command, "refresh") == 0) {
			char full_path[255];
			g_SmmAPI.PathFormat(full_path, sizeof(full_path), "%s/%s", g_ModPath.c_str(), GetPluginsFile());

			LoadPluginsFromFile(full_path);

			return;
		} else if (strcmp(command, "list") == 0) {
			SourceMM::CPluginManager::CPlugin *pl;
			PluginIter i;
			const char *status="";
			const char *version=NULL;
			const char *name=NULL;
			const char *author=NULL;

			CONMSG("-Id- %-20.19s  %-10.9s  %-20.19s %-8.7s\n", "Name", "Version", "Author", "Status");
			for (i=g_PluginMngr._begin(); i!=g_PluginMngr._end(); i++)
			{
				pl = (*i);
				if (!pl)
					break;
				if (pl->m_Status == Pl_Paused)
				{
					status = "PAUSE";
				} else if (pl->m_Status == Pl_Running) {
					if (pl->m_API && pl->m_API->QueryRunning(NULL, 0))
						status = "RUN";
					else
						status = "STOPPED";
				} else if (pl->m_Status == Pl_Refused) {
					status = "FAIL";
				} else if (pl->m_Status == Pl_Error) {
					status = "ERROR";
				} else if (pl->m_Status == Pl_NotFound) {
					status = "NOFILE";
				}

				if (pl->m_API)
				{
					version = pl->m_API->GetVersion();
					author = pl->m_API->GetAuthor();
					name = pl->m_API->GetName();
				} else {
					version = "-";
					author = "-";
					name = "-";
				}

				if (!version)
					version = "-";
				if (!author)
					author = "-";
				if (!name)
					name = pl->m_File.c_str();


				CONMSG("[%02d] %-20.19s  %-10.9s  %-20.19s %-8.7s\n", pl->m_Id, name, version, author, status);
			}

			//CONMSG("\n");

			return;
		} else if (strcmp(command, "cmds") == 0) {
			if (args >= 3)
			{
				int id = atoi(e->Cmd_Argv(2));
				SourceMM::CPluginManager::CPlugin *pl = g_PluginMngr.FindById(id);

				if (!pl)
				{
					CONMSG("Plugin %d not found.\n", id);
					return;
				}

				if (!pl->m_API)
				{
					CONMSG("Plugin %d is not loaded.\n", id);
				} else {
					CONMSG("Console commands for %s:\n", pl->m_API->GetName());
					SourceHook::List<ConCommandBase *>::iterator ci;
					size_t count = 0;

					for (ci=pl->m_Cmds.begin(); ci!=pl->m_Cmds.end(); ci++)
					{
						count++;
						CONMSG(" [%5d] %-s\n", count, (*ci)->GetName());
					}
				}
			} else {
				CONMSG("Usage: meta cmds <id>\n");
			}

			return;
		} else if (strcmp(command, "cvars") == 0) {
			if (args >= 3)
			{
				int id = atoi(e->Cmd_Argv(2));
				SourceMM::CPluginManager::CPlugin *pl = g_PluginMngr.FindById(id);

				if (!pl)
				{
					CONMSG("Plugin %d not found.\n", id);
					return;
				}

				if (!pl->m_API)
				{
					CONMSG("Plugin %d is not loaded.\n", id);
				} else {
					CONMSG("Registered cvars for %s:\n", pl->m_API->GetName());
					SourceHook::List<ConCommandBase *>::iterator ci;
					size_t count = 0;

					for (ci=pl->m_Cvars.begin(); ci!=pl->m_Cvars.end(); ci++)
					{
						count++;
						CONMSG(" [%5d] %-s\n", count, (*ci)->GetName());
					}
				}
			} else {
				CONMSG("Usage: meta cvars <id>\n");
			}

			return;
		} else if (strcmp(command, "info") == 0) {
			if (args >= 3)
			{
				int id = atoi(e->Cmd_Argv(2));
				SourceMM::CPluginManager::CPlugin *pl = g_PluginMngr.FindById(id);
				if (!pl)
				{
					CONMSG("Plugin %d not found.\n", id);
					return;
				}

				if (!pl->m_API)
				{
					CONMSG("Plugin %d is not loaded.\n", id);
				} else {
					if (pl->m_Status == Pl_Paused)
					{
						CONMSG("Plugin %d is paused.\n", id);
					} else if (pl->m_Status == Pl_Running) {
						char run_msg[255];
						bool run = false;
						if (pl->m_API && pl->m_API->QueryRunning(run_msg, sizeof(run_msg)-1))
							run = true;
						if (run)
						{
							CONMSG("Plugin %d is running.\n", id);
						} else {
							CONMSG("Plugin %d is stopped: %s\n", id, run_msg);
						}
					}
					CONMSG("  Name: \"%s\" by %s\n", pl->m_API->GetName(), pl->m_API->GetAuthor());
					CONMSG("  Version: %s\n", pl->m_API->GetVersion());
					CONMSG("  Description: %s\n", pl->m_API->GetDescription());
					CONMSG("  License: %s\n", pl->m_API->GetLicense());
					CONMSG("  URL: %s\n", pl->m_API->GetURL());
					CONMSG("  Details: API %03d, Date: %s\n", pl->m_API->GetApiVersion(), pl->m_API->GetDate());
				}
				CONMSG("File: %s\n\n", pl->m_File.c_str());

				return;
			} else {
				CONMSG("Usage: meta info <id>\n");

				return;
			}
		} else if (strcmp(command, "pause") == 0) {
			if (args >= 3)
			{
				int id = atoi(e->Cmd_Argv(2));

				char error[255];

				if (!g_PluginMngr.Pause(id, error, sizeof(error)))
				{
					CONMSG("Pause failed: %s\n", error);
					return;
				}
				
				CONMSG("Plugin %d has been paused.\n", id);

				return;
			} else {
				CONMSG("Usage: meta pause <id>\n");

				return;
			}
		} else if (strcmp(command, "unpause") == 0) {
			if (args >= 3)
			{
				int id = atoi(e->Cmd_Argv(2));
				char error[255];

				if (!g_PluginMngr.Unpause(id, error, sizeof(error)))
				{
					CONMSG("Unpause failed: %s\n", error);
					return;
				}

				CONMSG("Plugin %d has been unpaused.\n", id);

				return;
			} else {
				CONMSG("Usage: meta unpause <id>\n");

				return;
			}
		} else if (strcmp(command, "load") == 0) {
			if (args >= 3)
			{
				const char *file = e->Cmd_Argv(2);
				char full_path[255];

				const char *alias = g_PluginMngr.LookupAlias(file);
				if (alias)
				{
					file = alias;
				}

				if (file[0] == '/' || strcmp(&(file[1]), ":\\") == 0)
				{
					g_SmmAPI.PathFormat(full_path, sizeof(full_path), "%s", file);
				} else {
					const char *ext = UTIL_GetExtension(file);
#if defined WIN32 || defined _WIN32
					ext = ext ? "" : ".dll";
#else
					ext = ext ? "" : "_i486.so";
#endif
					g_SmmAPI.PathFormat(full_path, sizeof(full_path), "%s/%s%s", g_ModPath.c_str(), file, ext);
				}

				char error[255]={0};
				bool already;
				SourceMM::CPluginManager::CPlugin *pl;

				PluginId id = g_PluginMngr.Load(full_path, Pl_Console, already, error, sizeof(error));
				pl = g_PluginMngr.FindById(id);
				if (!pl || id < Pl_MinId || (pl->m_Status < Pl_Paused))
				{
					CONMSG("Failed to load plugin %s (%s).\n", file, error);
					return;
				}

				if (!already)
				{
					CONMSG("Plugin \"%s\" loaded with id %d.\n", pl->m_API->GetName(), pl->m_Id);
				} else {
					CONMSG("Plugin \"%s\" is already loaded as %d.\n", pl->m_API->GetName(), pl->m_Id);
				}
				
				return;
			} else {
				CONMSG("Usage: meta load <path>\n");

				return;
			}
		} else if ( (strcmp(command, "alias") == 0) ||
					(strcmp(command, "aliases") == 0) ) {
			if (args >= 4)
			{
				const char *alias = e->Cmd_Argv(2);
				const char *value = e->Cmd_Argv(3);

				g_PluginMngr.SetAlias(alias, value);
				if (value[0] == '\0')
				{
					CONMSG("Deleted alias: %s.\n", alias);
				} else {
					CONMSG("Set alias \"%s\" to: %s\n", alias, value);
				}
			} else if (args >= 3) {
				const char *alias = e->Cmd_Argv(2);
				const char *value = g_PluginMngr.LookupAlias(alias);
				if (value)
				{
					CONMSG("Alias \"%s\" is set to: %s\n", alias, value);
				} else {
					CONMSG("Alias \"%s\" was not found.\n", alias);
				}
			} else {
				SourceHook::List<SourceMM::CNameAlias *>::iterator iter, end;
				SourceMM::CNameAlias *p;

				iter = g_PluginMngr._alias_begin();
				end = g_PluginMngr._alias_end();
				size_t total = 0;
				if (iter != end)
				{
					CONMSG("%-10.9s %s\n", "Alias", "File");
					CONMSG(" --- \n");
					for (; iter!=end; iter++)
					{
						p = (*iter);
						CONMSG("%-10.9s %s\n", p->alias.c_str(), p->value.c_str());
						total++;
					}
					CONMSG(" --- \n");
					CONMSG("%d aliases total.\n", total);
				} else {
					CONMSG("No aliases found.\n");
				}
			}
			return;
		} else if (strcmp(command, "unload") == 0) {
			if (args >= 3)
			{
				const char *file = e->Cmd_Argv(2);
				int id = atoi(file);
				char error[255]={0};

				if (id == 0 && isalpha(file[0]))
				{
					char full_path[255];
					const char *alias = g_PluginMngr.LookupAlias(file);

					if (alias)
					{
						file = alias;
					}

					/* first check if it's a known filename */
					if (file[0] == '/' || strcmp(&(file[1]), ":\\") == 0)
					{
						g_SmmAPI.PathFormat(full_path, sizeof(full_path), "%s", file);
					} else {
						const char *ext = UTIL_GetExtension(file);
#if defined WIN32 || defined _WIN32
						ext = ext ? "" : ".dll";
#else
						ext = ext ? "" : "_i486.so";
#endif
						g_SmmAPI.PathFormat(full_path, sizeof(full_path), "%s/%s%s", g_ModPath.c_str(), file, ext);
					}

					SourceHook::List<SourceMM::CPluginManager::CPlugin *>::iterator iter, end;
					SourceMM::CPluginManager::CPlugin *pl;
					iter = g_PluginMngr._begin();
					end = g_PluginMngr._end();
					for (; iter!=end; iter++)
					{
						pl = (*iter);
						if (strcmp(pl->m_File.c_str(), full_path) == 0)
						{
							id = pl->m_Id;
							break;
						}
					}
					if (id == 0)
					{
						CONMSG("Plugin \"%s\" not found.\n", full_path);
						return;
					}
				}

				if (!g_PluginMngr.Unload(id, false, error, sizeof(error)))
				{
					CONMSG("Unload failed: %s\n", error);
					return;
				}

				CONMSG("Plugin %d unloaded.\n", id);
			} else {
				CONMSG("Usage: meta unload <id>\n");
			}
			return;
		} else if (strcmp(command, "force_unload") == 0) {
			if (args >= 3)
			{
				int id = atoi(e->Cmd_Argv(2));
				char error[255]={0};

				if (!g_PluginMngr.Unload(id, false, error, sizeof(error)))
				{
					CONMSG("Force unload failed: %s\n", error);
					return;
				}

				CONMSG("Plugin %d force unloaded.\n", id);

				return;
			} else {
				CONMSG("Usage: meta force_unload <id>\n");

				return;
			}
		} else if (strcmp(command, "clear") == 0) {
			if (!g_PluginMngr.UnloadAll())
			{
				CONMSG("One or more plugins resisted removal (cleaned anyway).\n");
				return;
			} 

			CONMSG("All plugins unloaded.\n");

			return;
		} else if (strcmp(command, "retry") == 0) {
			if (args >= 3)
			{
				int id = atoi(e->Cmd_Argv(2));
				char error[255];

				if (!g_PluginMngr.Retry(id, error, sizeof(error)))
				{
					CONMSG("Error reloading plugin: %s\n", error);
					return;
				}

				CONMSG("Plugin %d successfully reloaded.\n", id);

				return;
			} else {
				CONMSG("Usage: meta retry <id>\n");

				return;
			}
		}
	}

	CONMSG("Metamod:Source Menu\n");
	CONMSG("usage: meta <command> [arguments]\n");
	CONMSG("  alias        - List or set an alias\n");
	CONMSG("  clear        - Unload all plugins forcefully\n");
	CONMSG("  cmds         - Show plugin commands\n");
	CONMSG("  cvars        - Show plugin cvars\n");
	CONMSG("  credits      - About Metamod:Source\n");
	CONMSG("  force_unload - Forcefully unload a plugin\n");
	CONMSG("  game         - Information about GameDLL\n");
	CONMSG("  info         - Information about a plugin\n");
	CONMSG("  list         - List plugins\n");
	CONMSG("  load         - Load a plugin\n");
	CONMSG("  pause        - Pause a running plugin\n");
	CONMSG("  refresh      - Reparse plugins file\n");
	CONMSG("  retry        - Attempt to reload a plugin\n");
	CONMSG("  unload       - Unload a loaded plugin\n");
	CONMSG("  unpause      - Unpause a paused plugin\n");
	CONMSG("  version      - Version information\n");
}

CAlwaysRegisterableCommand::CAlwaysRegisterableCommand()
{
	Create("", NULL, FCVAR_UNREGISTERED|FCVAR_GAMEDLL);
	m_pICvar = NULL;
}

bool CAlwaysRegisterableCommand::IsRegistered( void ) const
{
	return false;
}

void CAlwaysRegisterableCommand::BringToFront()
{
	if (!m_pICvar)
		m_pICvar = g_Engine.icvar;

	// First, let's try to find us!
	ConCommandBase *pPtr = m_pICvar->GetCommands();

	if (pPtr == this)
	{
		// We are already at the beginning; Nothing to do
		return;
	}

	while (pPtr)
	{
		if (pPtr == this && pPtr->IsCommand() && stricmp(GetName(), pPtr->GetName()) == 0)
			break;
		ConCommandBase *pPrev = NULL;
		while (pPtr)
		{
			if (pPtr == this)
				break;
			pPrev = pPtr;
			pPtr = const_cast<ConCommandBase*>(pPtr->GetNext());
		}
		if (pPrev && pPtr == this)
		{
			pPrev->SetNext(m_pNext);		// Remove us from the list
		}
		// Now, register us
		SetNext(NULL);
		m_pICvar->RegisterConCommandBase(this);
	}
}

void ClientCommand_handler(edict_t *client)
{
	IVEngineServer *e = g_Engine.engine;
	const char *cmd = e->Cmd_Argv(0);

	if (strcmp(cmd, "meta") == 0)
	{
		int args = e->Cmd_Argc();
		if (args == 2)
		{
			const char *subcmd = e->Cmd_Argv(1);

			if (strcmp(subcmd, "credits") == 0)
			{
				CLIENT_CONMSG(client, "Metamod:Source was developed by:\n");
				CLIENT_CONMSG(client, "  SourceHook: Pavol \"PM OnoTo\" Marko\n");
				CLIENT_CONMSG(client, "  GameDLL/Plugins: David \"BAILOPAN\" Anderson\n");
				CLIENT_CONMSG(client, "  GameDLL: Scott \"Damaged Soul\" Ehlert\n");
				CLIENT_CONMSG(client, "For more information, see the official website\n");
				CLIENT_CONMSG(client, "http://www.sourcemm.net/\n");

				RETURN_META(MRES_SUPERCEDE);
			} else if(strcmp(subcmd, "version") == 0) {
				CLIENT_CONMSG(client, "Metamod:Source version %s\n", SOURCEMM_VERSION);
				CLIENT_CONMSG(client, "Compiled on: %s\n", SOURCEMM_DATE);
				CLIENT_CONMSG(client, "Plugin interface version: %d:%d\n", PLAPI_VERSION, PLAPI_MIN_VERSION);
				CLIENT_CONMSG(client, "SourceHook version: %d:%d\n", g_SourceHook.GetIfaceVersion(), g_SourceHook.GetImplVersion());
				CLIENT_CONMSG(client, "http://www.sourcemm.net/\n");

				RETURN_META(MRES_SUPERCEDE);
			} else if(strcmp(subcmd, "list") == 0) {
				SourceMM::CPluginManager::CPlugin *pl;
				Pl_Status st;
				PluginIter i;
				const char *version = NULL;
				const char *name = NULL;
				const char *author = NULL;
				const char *status = NULL;

				CLIENT_CONMSG(client, "-Id- %-20.19s  %-10.9s  %-20.19s  %6s\n", "Name", "Version", "Author", "Status");

				for (i=g_PluginMngr._begin(); i!=g_PluginMngr._end(); i++)
				{
					pl = (*i);
					if (!pl)
						break;
					
					st = pl->m_Status;

					/* Only show plugins that are running or paused */
					if (pl->m_API && (st == Pl_Running || st == Pl_Paused))
					{
						version = pl->m_API->GetVersion();
						author = pl->m_API->GetAuthor();
						name = pl->m_API->GetName();

						if (st == Pl_Running && pl->m_API->QueryRunning(NULL, 0))
						{
							status = "RUN";
						} else {
							status = "PAUSE";
						}

						if (!version || !author || !name)
							break;

						CLIENT_CONMSG(client, "[%02d] %-20.19s  %-10.9s  %-20.19s  %6s\n", pl->m_Id, name, version, author, status);
					}
				}

				RETURN_META(MRES_SUPERCEDE);
			}
		}

		CLIENT_CONMSG(client, "Metamod:Source Menu\n");
		CLIENT_CONMSG(client, "usage: meta <command>\n");
		CLIENT_CONMSG(client, "  credits - About Metamod:Source\n");
		CLIENT_CONMSG(client, "  list    - List plugins\n");
		CLIENT_CONMSG(client, "  version - Version information\n");

		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

const char *GetPluginsFile()
{
	return mm_pluginsfile.GetString();
}
