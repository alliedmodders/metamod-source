/* ======== SourceMM ========
 * Copyright (C) 2004-2010 Metamod:Source Development Team
 * No warranties of any kind
 *
 * License: zlib/libpng
 *
 * Author(s): David "BAILOPAN" Anderson
 * ============================
 */

#include <ctype.h>
#include "convar_smm.h"
#include "CSmmAPI.h"
#include "concommands.h"
#include "CPlugin.h"
#include "sh_string.h"
#include "sh_list.h"
#include "util.h"
#include <versionlib.h>

/**
 * @brief Console Command Implementations
 * @file concommands.cpp
 */

SMConVarAccessor g_SMConVarAccessor;

SMConVarAccessor::SMConVarAccessor()
{
	m_TopConCommandBase = NULL;
}

bool SMConVarAccessor::RegisterConCommandBase(ConCommandBase *pCommand)
{
	/* Add the FCVAR_GAMEDLL flag 
	 * => No crash on exit!
	 * UPDATE: Do _not_ add the FCVAR_GAMEDLL flag here, as it
	 * causes the command to be unusable on listen servers until you load a map
	 * We will set the FCVAR_GAMEDLL flag on all commands we have registered once we are being unloaded
	 */
	// pCommand->AddFlags(FCVAR_GAMEDLL);
	m_RegisteredCommands.push_back(pCommand);

	pCommand->SetNext(NULL);
	g_Engine.icvar->RegisterConCommandBase(pCommand);

	return true;
}

bool SMConVarAccessor::Register(ConCommandBase *pCommand)
{
	/* Simple, don't mark as part of sourcemm! */
	pCommand->SetNext(NULL);
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

void SMConVarAccessor::Unregister(ConCommandBase *pCommand)
{
	ConCommandBase *pCur = NULL;
	ConCommandBase *pPrev = NULL;

	if (!pCommand || !pCommand->IsRegistered())
	{
		return;
	}

	pCur = g_Engine.icvar->GetCommands();
	pCommand->SetRegistered(false);

	if (!m_TopConCommandBase || !pCur)
	{
		return;
	}

	if (pCur == pCommand)
	{
		*m_TopConCommandBase = const_cast<ConCommandBase *>(pCommand->GetNext());
		pCommand->SetNext(NULL);
		return;
	}
	
	pPrev = pCur;
	pCur = const_cast<ConCommandBase *>(pCur->GetNext());

	while (pCur)
	{
		if (pCur == pCommand)
		{
			pPrev->SetNext(const_cast<ConCommandBase *>(pCommand->GetNext()));
			pCommand->SetNext(NULL);
		}

		pPrev = pCur;
		pCur = const_cast<ConCommandBase *>(pCur->GetNext());
	}
}

ConVar metamod_version("metamod_version", METAMOD_VERSION, FCVAR_SPONLY | FCVAR_NOTIFY, "Metamod:Source Version");
#ifdef OS_WIN32
ConVar mm_pluginsfile("mm_pluginsfile", "addons\\metamod\\metaplugins.ini", FCVAR_SPONLY, "Metamod:Source Plugins File");
ConVar mm_basedir("mm_basedir", "addons\\metamod", FCVAR_SPONLY, "Metamod:Source base folder");
#else
ConVar mm_pluginsfile("mm_pluginsfile", "addons/metamod/metaplugins.ini", FCVAR_SPONLY, "Metamod:Source Plugins File");
ConVar mm_basedir("mm_basedir", "addons/metamod", FCVAR_SPONLY, "Metamod:Source base folder");
#endif

CON_COMMAND(meta, "Metamod:Source Menu")
{
	IVEngineServer *e = g_Engine.engine;

	int args = e->Cmd_Argc();

	if (!g_GameDll.loaded && !g_bLevelChanged)
	{
		CONMSG("WARNING: You must change the map to activate Metamod:Source.\n");
		return;
	}

	if (args >= 2)
	{
		const char *command = e->Cmd_Argv(1);
		if (strcmp(command, "credits") == 0)
		{
			CONMSG("Metamod:Source was developed by:\n");
			CONMSG("  SourceHook: Pavol \"PM OnoTo\" Marko\n");
			CONMSG("  GameDLL/Plugins: David \"BAILOPAN\" Anderson\n");
			CONMSG("  GameDLL: Scott \"DS\" Ehlert\n");
			CONMSG("For more information, see the official website\n");
			CONMSG("http://www.metamodsource.net/\n");
			
			return;
		} else if (strcmp(command, "version") == 0) {
			CONMSG("Metamod:Source version %s\n", METAMOD_VERSION);
#if defined(MMS_GENERATED_BUILD)
			CONMSG("Built from: https://github.com/alliedmodders/metamod-source/commit/%s\n", METAMOD_BUILD_SHA);
#endif
			CONMSG("Build ID: %s:%s\n", METAMOD_BUILD_LOCAL_REV, METAMOD_BUILD_SHA);
			if (g_GameDll.loaded)
			{
				CONMSG("Loaded As: GameDLL (gameinfo.txt)\n");
			}
			else
			{
				CONMSG("Loaded As: Valve Server Plugin\n");
			}
			CONMSG("Compiled on: %s\n", SOURCEMM_DATE);
			CONMSG("Plugin interface version: %d:%d\n", PLAPI_VERSION, PLAPI_MIN_VERSION);
			CONMSG("SourceHook version: %d:%d\n", g_SourceHook.GetIfaceVersion(), g_SourceHook.GetImplVersion());
			CONMSG("http://www.metamodsource.net/\n");

			return;
		} else if (strcmp(command, "game") == 0) {
			char bin_path[PATH_SIZE];
			GetFileOfAddress((void*)g_GameDll.factory, bin_path, sizeof(bin_path));

			CONMSG("GameDLL Information\n");
			CONMSG("  Description: %s\n", g_GameDll.pGameDLL->GetGameDescription());
			CONMSG("  Mod Path: %s\n", g_ModPath.c_str());
			CONMSG("  DLL Path: %s\n", bin_path);
			CONMSG("  Interface: ServerGameDLL%03d\n", g_GameDllVersion);

			if (g_Engine.original)
			{
				CONMSG("  Engine: Original (pre-Episode 1)\n");
			}
			else
			{
				CONMSG("  Engine: Episode 1 (2004)\n");
			}

			// Display user messages
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

			return;
		} else if (strcmp(command, "refresh") == 0) {
			char filepath[PATH_SIZE], vdfpath[PATH_SIZE];
			g_SmmAPI.PathFormat(filepath, sizeof(filepath), "%s/%s", g_ModPath.c_str(), GetPluginsFile());
			g_SmmAPI.PathFormat(vdfpath, sizeof(vdfpath), "%s/%s", g_ModPath.c_str(), GetMetamodBaseDir());

			LoadPlugins(filepath, vdfpath);

			return;
		} else if (strcmp(command, "list") == 0) {
			size_t len;
			PluginIter i;
			char buffer[255];
			ISmmPlugin *plapi;
			const char *plname;
			SourceMM::CPluginManager::CPlugin *pl;
			unsigned int plnum = g_PluginMngr.GetPluginCount();

#define IS_STR_FILLED(var) (var != NULL && var[0] != '\0')

			if (!plnum)
			{
				CONMSG("No plugins loaded.\n");
				return;
			}
			else
			{
				CONMSG("Listing %d plugin%s:\n", plnum, (plnum > 1) ? "s" : "");
			}

			for (i=g_PluginMngr._begin(); i!=g_PluginMngr._end(); i++)
			{
				pl = (*i);
				if (!pl)
				{
					break;
				}

				len = 0;

				if (pl->m_Status != Pl_Running)
				{
					len += UTIL_Format(buffer, sizeof(buffer), "  [%02d] <%s>", pl->m_Id, g_PluginMngr.GetStatusText(pl));
				}
				else
				{
					len += UTIL_Format(buffer, sizeof(buffer), "  [%02d]", pl->m_Id);
				}

				if ((plapi = pl->m_API))
				{
					plname = IS_STR_FILLED(plapi->GetName()) ? plapi->GetName() : pl->m_File.c_str();
					len += UTIL_Format(&buffer[len], sizeof(buffer)-len, " %s", plname);

					if (IS_STR_FILLED(plapi->GetVersion()))
					{
						len += UTIL_Format(&buffer[len], sizeof(buffer)-len, " (%s)", plapi->GetVersion());
					}
					if (IS_STR_FILLED(plapi->GetAuthor()))
					{
						UTIL_Format(&buffer[len], sizeof(buffer)-len, " by %s", plapi->GetAuthor());
					}
				}

				CONMSG("%s\n", buffer);
			}

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

				g_SmmAPI.GetFullPluginPath(file, full_path, sizeof(full_path));

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

					g_SmmAPI.GetFullPluginPath(file, full_path, sizeof(full_path));

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
	CONMSG("  refresh      - Reparse plugin files\n");
	CONMSG("  retry        - Attempt to reload a plugin\n");
	CONMSG("  unload       - Unload a loaded plugin\n");
	CONMSG("  unpause      - Unpause a paused plugin\n");
	CONMSG("  version      - Version information\n");
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
				CLIENT_CONMSG(client, "  Core: David \"BAILOPAN\" Anderson\n");
				CLIENT_CONMSG(client, "  Core: Scott \"DS\" Ehlert\n");
				CLIENT_CONMSG(client, "For more information, see the official website\n");
				CLIENT_CONMSG(client, "http://www.metamodsource.net/\n");

				RETURN_META(MRES_SUPERCEDE);
			} else if(strcmp(subcmd, "version") == 0) {
				CLIENT_CONMSG(client, "Metamod:Source version %s\n", METAMOD_VERSION);
				CLIENT_CONMSG(client, "Compiled on: %s\n", SOURCEMM_DATE);
				CLIENT_CONMSG(client, "Plugin interface version: %d:%d\n", PLAPI_VERSION, PLAPI_MIN_VERSION);
				CLIENT_CONMSG(client, "SourceHook version: %d:%d\n", g_SourceHook.GetIfaceVersion(), g_SourceHook.GetImplVersion());
				CLIENT_CONMSG(client, "http://www.metamodsource.net/\n");

				RETURN_META(MRES_SUPERCEDE);
			} else if(strcmp(subcmd, "list") == 0) {
				SourceMM::CPluginManager::CPlugin *pl;
				ISmmPlugin *plapi;
				const char *plname;
				PluginIter i;
				char buffer[256];
				int len = 0;
				int plnum = 0;

				for (i = g_PluginMngr._begin(); i != g_PluginMngr._end(); i++, len=0)
				{
					pl = (*i);
					if (pl && pl->m_Status == Pl_Running)
					{
						plapi = pl->m_API;
						if (!plapi || !plapi->QueryRunning(NULL, 0))
						{
							continue;
						}
						plnum++;

						len += UTIL_Format(buffer, sizeof(buffer), "  [%02d]", plnum);

						plname = IS_STR_FILLED(plapi->GetName()) ? plapi->GetName() : pl->m_File.c_str();
						len += UTIL_Format(&buffer[len], sizeof(buffer)-len, " %s", plname);

						if (IS_STR_FILLED(plapi->GetVersion()))
						{
							len += UTIL_Format(&buffer[len], sizeof(buffer)-len, " (%s)", plapi->GetVersion());
						}
						if (IS_STR_FILLED(plapi->GetAuthor()))
						{
							UTIL_Format(&buffer[len], sizeof(buffer)-len, " by %s", plapi->GetAuthor());
						}

						CLIENT_CONMSG(client, "%s\n", buffer);
					}
				}

				if (!plnum)
				{
					CLIENT_CONMSG(client, "No active plugins loaded.\n");
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

void SMConVarAccessor::UnloadMetamodCommands()
{
	Unregister(&metamod_version);
	Unregister(&mm_pluginsfile);
	Unregister(&mm_basedir);
	Unregister(&meta_command);
}

const char *GetPluginsFile()
{
	return mm_pluginsfile.GetString();
}

const char *GetMetamodBaseDir()
{
	return mm_basedir.GetString();
}

/* Signature for ICvar::GetCommands() in vstdlib for Win32 and Linux.
 *
 * 20226EE0 A1 50 5C 5A 20   mov         eax,dword ptr ds:[205A5C50h] <-- What we want
 * 20226EE5 C3               ret              
 */
#define CMDLIST_SIG "\xA1\x2A\x2A\x2A\x2A\xC3"
#define CMDLIST_SIGLEN 6

/* Linux symbol name of ConCommandBase list in vstdlib */
#define CMDLIST_SYMBOL "_ZN14ConCommandBase18s_pConCommandBasesE"

/* This function retrieves the address of the var that holds the top of the ConCommandBase list.
 * Having this allows us to change the beginning of this list with ease.
 *
 * This craziness eliminates the need for the eternal command/cvar used previously which
 * could have caused a crash as a result of registering commands/cvars more than once.
 */
bool SMConVarAccessor::InitConCommandBaseList()
{
	char *vfunc = UTIL_GetOrigFunction(&ICvar::GetCommands, g_Engine.icvar);

	if (!vfunc)
	{
		return false;
	}

#ifdef OS_WIN32
	if (UTIL_VerifySignature(vfunc, CMDLIST_SIG, CMDLIST_SIGLEN))
	{
		/* Skip past 0xA1 and get addr of ConCommandBase list var */
		m_TopConCommandBase = *reinterpret_cast<ConCommandBase ***>(vfunc + 1);
		return true;
	}
#elif defined OS_LINUX
	/* Try dlsym first */
	char path[PATH_SIZE];
	if (GetFileOfAddress((void *)g_Engine.icvar, path, sizeof(path)))
	{
		void *handle = dlopen(path, RTLD_NOW);
		if (handle)
		{
			m_TopConCommandBase = reinterpret_cast<ConCommandBase **>(dlsym(handle, CMDLIST_SYMBOL));
			dlclose(handle);
			return true;
		}
	}

	/* If dlsym failed, then verify signature of function */
	if (!m_TopConCommandBase && UTIL_VerifySignature(vfunc, CMDLIST_SIG, CMDLIST_SIGLEN))
	{
		/* Skip past 0xA1 and get addr of ConCommandBase list var */
		m_TopConCommandBase = *reinterpret_cast<ConCommandBase ***>(vfunc + 1);
		return true;
	}
#endif

	return false;
}
