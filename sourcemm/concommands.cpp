/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#include "concommands.h"
#include "CPlugin.h"

/**
 * @brief Console Command Implementations
 * @file concommands.cpp
 */

SMConVarAccessor g_SMConVarAccessor;

bool SMConVarAccessor::RegisterConCommandBase(ConCommandBase *pCommand)
{
	// Add the FCVAR_GAMEDLL flag 
	// => No crash on exit!
	pCommand->AddFlags(FCVAR_GAMEDLL);
	pCommand->SetNext( NULL );
	g_Engine.icvar->RegisterConCommandBase(pCommand);

	return true;
}

ConVar metamod_version("metamod_version", SOURCEMM_VERSION, FCVAR_REPLICATED | FCVAR_SPONLY, "Metamod:Source Version");

CON_COMMAND(meta, "Metamod:Source Menu")
{
	IVEngineServer *e = g_Engine.engine;

	int args = e->Cmd_Argc();

	if (args >= 2)
	{
		const char *command = e->Cmd_Argv(1);
		if (strcmp(command, "credits") == 0)
		{
			Msg("Metamod:Source was developed by:\n");
			Msg("  SourceHook: Pavol \"PM OnoTo\" Marko\n");
			Msg("  GameDLL/Plugins: David \"BAILOPAN\" Anderson\n");
			Msg("  GameDLL: Scott \"Damaged Soul\" Ehlert\n");
			Msg("For more information, see the official website\n");
			Msg("http://www.sourcemm.net/\n\n");
			
			return;
		} else if (strcmp(command, "version") == 0) {
			Msg("Metamod:Source version %s\n", SOURCEMM_VERSION);
			Msg("Compiled on: %s\n", SOURCEMM_DATE);
			Msg("Plugin interface version: %d:%d\n", PLAPI_VERSION, PLAPI_MIN_VERSION);
			Msg("http://www.sourcemm.net/\n\n");

			return;
		} else if (strcmp(command, "game") == 0) {
			Msg("GameDLL Information\n");
			Msg("  Mod path: %s\n", g_ModPath.c_str());
			Msg("  Dll path: %s\n", g_BinPath.c_str());

			return;
		} else if (strcmp(command, "refresh") == 0) {
			char full_path[255];
#if defined WIN32 || defined _WIN32
			snprintf(full_path, sizeof(full_path)-1, "%s\\%s", g_ModPath.c_str(), "metaplugins.ini");
#else
			snprintf(full_path, sizeof(full_path)-1, "%s/%s", g_ModPath.c_str(), "metaplugins.ini");
#endif
			LoadPluginsFromFile(full_path);

			return;
		} else if (strcmp(command, "list") == 0) {
			SourceMM::CPluginManager::CPlugin *pl;
			PluginIter i;
			const char *status="";
			const char *version=NULL;
			const char *name=NULL;
			const char *author=NULL;

			Msg("-Id- %-16.15s  %-8.7s  %-12.11s %-8.7s\n", "Name", "Version", "Author", "Status");
			for (i=g_PluginMngr._begin(); i!=g_PluginMngr._end(); i++)
			{
				pl = (*i);
				if (!pl)
					break;
				if (pl->m_Status == Pl_Paused)
				{
					status = "PAUSE";
				} else if (pl->m_Status == Pl_Running) {
					status = "RUN";
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


				Msg("[%02d] %-16.15s  %-8.7s  %-12.11s %-8.7s\n", pl->m_Id, name, version, author, status);
			}

			Msg("\n");

			return;
		} else if (strcmp(command, "info") == 0) {
			if (args >= 3)
			{
				int id = atoi(e->Cmd_Argv(2));
				SourceMM::CPluginManager::CPlugin *pl = g_PluginMngr.FindById(id);
				if (!pl)
				{
					Msg("Plugin %d not found.\n", id);
					return;
				}

				if (!pl->m_API)
				{
					Msg("Plugin %d is not loaded.\n", id);
				} else {
					if (pl->m_Status == Pl_Paused)
					{
						Msg("Plugin %d is paused.\n", id);
					} else if (pl->m_Status == Pl_Running) {
						Msg("Plugin %d is running.\n", id);
					}
					Msg("  Name: \"%s\" by %s\n", pl->m_API->GetName(), pl->m_API->GetAuthor());
					Msg("  Version: %s\n", pl->m_API->GetVersion());
					Msg("  Description: %s\n", pl->m_API->GetDescription());
					Msg("  License: %s\n", pl->m_API->GetLicense());
					Msg("  URL: %s\n", pl->m_API->GetURL());
					Msg("  Details: API %03d, Date: %s\n", pl->m_API->GetApiVersion(), pl->m_API->GetDate());
				}
				Msg("File: %s\n\n", pl->m_File.c_str());

				return;
			} else {
				Msg("Usage: meta info <id>\n");

				return;
			}
		} else if (strcmp(command, "pause") == 0) {
			if (args >= 3)
			{
				int id = atoi(e->Cmd_Argv(2));

				char error[255];

				if (!g_PluginMngr.Pause(id, error, sizeof(error)-1))
				{
					Msg("Pause failed: %s\n", error);
					return;
				}
				
				Msg("Plugin %d has been paused.\n", id);

				return;
			} else {
				Msg("Usage: meta pause <id>\n");

				return;
			}
		} else if (strcmp(command, "unpause") == 0) {
			if (args >= 3)
			{
				int id = atoi(e->Cmd_Argv(2));
				char error[255];

				if (!g_PluginMngr.Unpause(id, error, sizeof(error)-1))
				{
					Msg("Unpause failed: %s\n", error);
					return;
				}

				Msg("Plugin %d has been unpaused.\n", id);

				return;
			} else {
				Msg("Usage: meta unpause <id>\n");

				return;
			}
		} else if (strcmp(command, "load") == 0) {
			if (args >= 3)
			{
				const char *file = e->Cmd_Argv(2);
				char full_path[255];

				if (file[0] == '/' || strcmp(&(file[1]), ":\\") == 0)
				{
					snprintf(full_path, sizeof(full_path)-1, "%s", file);
				} else {
					const char *ext = UTIL_GetExtension(file);
#if defined WIN32 || defined _WIN32
					ext = ext ? "" : ".dll";
					snprintf(full_path, sizeof(full_path)-1, "%s\\%s%s", g_ModPath.c_str(), file, ext);
#else
					ext = ext ? "" : "_i486.so";
					snprintf(full_path, sizeof(full_path)-1, "%s/%s%s", g_ModPath.c_str(), file, ext);
#endif
				}

				char error[255]={0};
				bool already;
				SourceMM::CPluginManager::CPlugin *pl;

				PluginId id = g_PluginMngr.Load(full_path, Pl_Console, already, error, sizeof(error)-1);
				pl = g_PluginMngr.FindById(id);
				if (!pl || id < Pl_MinId || (pl->m_Status < Pl_Paused))
				{
					Msg("Failed to load plugin %s (%s).\n", file, error);
					return;
				}

				if (!already)
				{
					Msg("Plugin \"%s\" loaded with id %d.\n", pl->m_API->GetName(), pl->m_Id);
				} else {
					Msg("Plugin \"%s\" is already loaded as %d.\n", pl->m_API->GetName(), pl->m_Id);
				}
				
				return;
			} else {
				Msg("Usage: meta load <path>\n");

				return;
			}
		} else if (strcmp(command, "unload") == 0) {
			if (args >= 3)
			{
				int id = atoi(e->Cmd_Argv(2));
				char error[255]={0};

				if (!g_PluginMngr.Unload(id, false, error, sizeof(error)-1))
				{
					Msg("Unload failed: %s\n", error);
					return;
				}

				Msg("Plugin %d unloaded.\n", id);

				return;
			} else {
				Msg("Usage: meta unload <id>\n");

				return;
			}
		} else if (strcmp(command, "force_unload") == 0) {
			if (args >= 3)
			{
				int id = atoi(e->Cmd_Argv(2));
				char error[255]={0};

				if (!g_PluginMngr.Unload(id, false, error, sizeof(error)-1))
				{
					Msg("Force unload failed: %s\n", error);
					return;
				}

				Msg("Plugin %d force unloaded.\n", id);

				return;
			} else {
				Msg("Usage: meta force_unload <id>\n");

				return;
			}
		} else if (strcmp(command, "clear") == 0) {
			if (!g_PluginMngr.UnloadAll())
			{
				Msg("One or more plugins resisted removal (cleaned anyway).\n");
				return;
			} 

			Msg("All plugins unloaded.\n");

			return;
		} else if (strcmp(command, "retry") == 0) {
			if (args >= 3)
			{
				int id = atoi(e->Cmd_Argv(2));
				char error[255];

				if (!g_PluginMngr.Retry(id, error, sizeof(error)-1))
				{
					Msg("Error reloading plugin: %s\n", error);
					return;
				}

				Msg("Plugin %d successfully reloaded.\n", id);

				return;
			} else {
				Msg("Usage: meta retry <id>\n");

				return;
			}
		}
	}

	Msg("Metamod:Source Menu\n");
	Msg("usage: meta <command> [arguments]\n");
	Msg("  clear        - Unload all plugins forcefully\n");
	Msg("  credits      - About Metamod:Source\n");
	Msg("  force_unload - Forcefully unload a plugin\n");
	Msg("  game         - Information about GameDLL\n");
	Msg("  info         - Information about a plugin\n");
	Msg("  list         - List plugins\n");
	Msg("  load         - Load a plugin\n");
	Msg("  pause        - Pause a running plugin\n");
	Msg("  refresh      - Reparse plugins file\n");
	Msg("  retry        - Attempt to reload a plugin\n");
	Msg("  unload       - Unload a loaded plugin\n");
	Msg("  unpause      - Unpause a paused plugin\n");
	Msg("  version      - Version information\n");
	Msg("\n");
}
