/**
 * vim: set ts=4 sw=4 tw=99 noet :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2010 AlliedModders LLC and authors.
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
#include <ctype.h>
#include <versionlib.h>
#include "metamod.h"
#include "metamod_util.h"
#include "metamod_console.h"
#include "metamod_plugins.h"

using namespace SourceMM;
using namespace SourceHook;

/**
 * @brief Console Command Implementations
 * @file concommands.cpp
 */

#define CONMSG			g_Metamod.ConPrintf
#define CLIENT_CONMSG	g_Metamod.ClientConPrintf

bool Command_Meta(IMetamodSourceCommandInfo *info)
{
	unsigned int args = info->GetArgCount();

	if (mm_IsVspBridged() && !mm_IsVspLoadComplete())
	{
		CONMSG("You must change the map to activate Metamod:Source.\n");
		return true;
	}

	if (args >= 1)
	{
		const char *command = info->GetArg(1);
		if (strcmp(command, "credits") == 0)
		{
			CONMSG("Metamod:Source was developed by:\n");
			CONMSG("  SourceHook: Pavol \"PM OnoTo\" Marko\n");
			CONMSG("  GameDLL/Plugins: David \"BAILOPAN\" Anderson\n");
			CONMSG("  GameDLL: Scott \"DS\" Ehlert\n");
			CONMSG("For more information, see the official website\n");
			CONMSG("http://www.metamodsource.net/\n");
			
			return true;
		}
		else if (strcmp(command, "version") == 0)
		{
			CONMSG("Metamod:Source version %s\n", METAMOD_VERSION);
#if defined(MMS_GENERATED_BUILD)
			CONMSG("Built from: https://github.com/alliedmodders/metamod-source/commit/%s\n", METAMOD_BUILD_SHA);
#endif
			CONMSG("Build ID: %s:%s\n", METAMOD_BUILD_LOCAL_REV, METAMOD_BUILD_SHA);

			if (g_Metamod.IsLoadedAsGameDLL())
			{
				CONMSG("Loaded As: GameDLL (gameinfo.txt)\n");
			}
			else
			{
				CONMSG("Loaded As: Valve Server Plugin\n");
			}

			CONMSG("Compiled on: %s\n", SOURCEMM_DATE);
			CONMSG("Plugin interface version: %d:%d\n", METAMOD_PLAPI_VERSION, PLAPI_MIN_VERSION);
			CONMSG("SourceHook version: %d:%d\n", g_SHPtr->GetIfaceVersion(), g_SHPtr->GetImplVersion());
			CONMSG("http://www.metamodsource.net/\n");

			return true;
		}
		else if (strcmp(command, "game") == 0)
		{
			CONMSG("GameDLL Information\n");
			CONMSG("  Description: %s\n", provider->GetGameDescription());
			CONMSG("  Mod Path: %s\n", g_Metamod.GetBaseDir());
			CONMSG("  DLL Path: %s\n", g_Metamod.GetGameBinaryPath());
			CONMSG("  Interface: ServerGameDLL%03d\n", g_Metamod.GetGameDLLVersion());

#if 0
			int engine = g_Metamod.GetSourceEngineBuild();
			if (engine == SOURCE_ENGINE_ORIGINAL)
			{
				CONMSG("  Engine: Original (pre-Episode 1)\n");
			}
			else if (engine == SOURCE_ENGINE_EPISODEONE)
			{
				CONMSG("  Engine: Episode 1 (2004)\n");
			}
			else if (engine == SOURCE_ENGINE_ORANGEBOX)
			{
				CONMSG("  Engine: Episode 2 (Orange Box, 2007)\n");
			}
#endif

#if SOURCE_ENGINE == SE_BLOODYGOODTIME
			CONMSG("  Engine: Bloody Good Time (2010)\n");
#elif SOURCE_ENGINE == SE_ALIENSWARM
			CONMSG("  Engine: Alien Swarm (2010)\n");
#elif SOURCE_ENGINE == SE_LEFT4DEAD2
			CONMSG("  Engine: Left 4 Dead 2 (2009)\n");
#elif SOURCE_ENGINE == SE_NUCLEARDAWN
			CONMSG("  Engine: Nuclear Dawn (2011)\n");
#elif SOURCE_ENGINE == SE_CONTAGION
			CONMSG("  Engine: Contagion (2013)\n");
#elif SOURCE_ENGINE == SE_LEFT4DEAD
			CONMSG("  Engine: Left 4 Dead (2008)\n");
#elif SOURCE_ENGINE == SE_ORANGEBOX
			CONMSG("  Engine: Episode 2 (Orange Box, 2007)\n");
#elif SOURCE_ENGINE == SE_CSS
			CONMSG("  Engine: Counter-Strike: Source (Valve Orange Box)\n");
#elif SOURCE_ENGINE == SE_HL2DM
			CONMSG("  Engine: Half-Life 2 Deathmatch (Valve Orange Box)\n");
#elif SOURCE_ENGINE == SE_DODS
			CONMSG("  Engine: Day of Defeat: Source (Valve Orange Box)\n");
#elif SOURCE_ENGINE == SE_SDK2013
			CONMSG("  Engine: Source SDK 2013 (2013)\n");
#elif SOURCE_ENGINE == SE_BMS
			CONMSG("  Engine: Black Mesa (2015)\n");
#elif SOURCE_ENGINE == SE_TF2
			CONMSG("  Engine: Team Fortress 2 (Valve Orange Box)\n");
#elif SOURCE_ENGINE == SE_DARKMESSIAH
			CONMSG("  Engine: Dark Messiah (2006)\n");
#elif SOURCE_ENGINE == SE_EYE
			CONMSG("  Engine: E.Y.E. Divine Cybermancy (2011)\n");
#elif SOURCE_ENGINE == SE_PORTAL2
			CONMSG("  Engine: Portal 2 (2011)\n");
#elif SOURCE_ENGINE == SE_BLADE
			CONMSG("  Engine: Blade Symphony (2013)\n");
#elif SOURCE_ENGINE == SE_INSURGENCY
			CONMSG("  Engine: Insurgency (2013)\n");
#elif SOURCE_ENGINE == SE_DOI
			CONMSG("  Engine: Day of Infamy (2016)\n");
#elif SOURCE_ENGINE == SE_CSGO
			CONMSG("  Engine: Counter-Strike: Global Offensive (2012)\n");
#elif SOURCE_ENGINE == SE_DOTA
			CONMSG("  Engine: Dota 2 (2013)\n");
#else
#error "SOURCE_ENGINE not defined to a known value"
#endif

			// Display user messages
			const char *msgname;
			int msgsize;
			int messages = g_Metamod.GetUserMessageCount();

			if (messages > 0)
			{
				CONMSG("  User Messages:  %-32.31s  %-5s  %-5s\n", "Name", "Index", "Size");

				for (int i = 0; i < messages; i++)
				{
					msgname = g_Metamod.GetUserMessage(i, &msgsize);

					CONMSG("                  %-32.31s  %-5d  %-5d\n", msgname, i, msgsize); 
				}

				CONMSG("  %d user message%s in total\n", messages, (messages > 1) ? "s" : "");
			}
			else
			{
				CONMSG("  User Messages: None\n");
			}

			return true;
		}
		else if (strcmp(command, "refresh") == 0)
		{
			char filepath[PATH_SIZE], vdfpath[PATH_SIZE];
			g_Metamod.PathFormat(filepath,
				sizeof(filepath),
				"%s/%s",
				g_Metamod.GetBaseDir(), 
				g_Metamod.GetPluginsFile());
			g_Metamod.PathFormat(vdfpath,
				sizeof(vdfpath),
				"%s/%s",
				g_Metamod.GetBaseDir(), 
				g_Metamod.GetVDFDir());

			mm_LoadPlugins(filepath, vdfpath);

			return true;
		}
		else if (strcmp(command, "list") == 0)
		{
			CPluginManager::CPlugin *pl;
			ISmmPlugin *plapi;
			const char *plname;
			PluginIter i;
			char buffer[256];
			int len;
			int plnum = g_PluginMngr.GetPluginCount();

			if (!plnum)
			{
				CONMSG("No plugins loaded.\n");
				return true;
			} else {
				CONMSG("Listing %d plugin%s:\n", plnum, (plnum > 1) ? "s" : "");
			}

			for (i = g_PluginMngr._begin(); i != g_PluginMngr._end(); i++)
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
				} else {
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

			return true;
		}
		else if (strcmp(command, "cmds") == 0)
		{
			if (args >= 2)
			{
				int id = atoi(info->GetArg(2));
				CPluginManager::CPlugin *pl = g_PluginMngr.FindById(id);

				if (!pl)
				{
					CONMSG("Plugin %d not found.\n", id);
					return true;
				}

				if (!pl->m_API)
				{
					CONMSG("Plugin %d is not loaded.\n", id);
				}
				else
				{
					CONMSG("Console commands for %s:\n", pl->m_API->GetName());
					List<ConCommandBase *>::iterator ci;
					size_t count = 0;

					for (ci=pl->m_Cmds.begin(); ci!=pl->m_Cmds.end(); ci++)
					{
						count++;
						CONMSG(" [%5d] %-s\n", count, (*ci)->GetName());
					}
				}
			}
			else
			{
				CONMSG("Usage: meta cmds <id>\n");
			}

			return true;
		}
		else if (strcmp(command, "cvars") == 0)
		{
			if (args >= 2)
			{
				int id = atoi(info->GetArg(2));
				CPluginManager::CPlugin *pl = g_PluginMngr.FindById(id);

				if (!pl)
				{
					CONMSG("Plugin %d not found.\n", id);
					return true;
				}

				if (!pl->m_API)
				{
					CONMSG("Plugin %d is not loaded.\n", id);
				}
				else
				{
					CONMSG("Registered cvars for %s:\n", pl->m_API->GetName());
					List<ConCommandBase *>::iterator ci;
					size_t count = 0;

					for (ci=pl->m_Cvars.begin(); ci!=pl->m_Cvars.end(); ci++)
					{
						count++;
						CONMSG(" [%5d] %-s\n", count, (*ci)->GetName());
					}
				}
			}
			else
			{
				CONMSG("Usage: meta cvars <id>\n");
			}

			return true;
		}
		else if (strcmp(command, "info") == 0)
		{
			if (args >= 2)
			{
				int id = atoi(info->GetArg(2));
				CPluginManager::CPlugin *pl = g_PluginMngr.FindById(id);
				if (!pl)
				{
					CONMSG("Plugin %d not found.\n", id);
					return true;
				}

				if (!pl->m_API)
				{
					CONMSG("Plugin %d is not loaded.\n", id);
				}
				else
				{
					if (pl->m_Status == Pl_Paused)
					{
						CONMSG("Plugin %d is paused.\n", id);
					}
					else if (pl->m_Status == Pl_Running)
					{
						char run_msg[255];
						bool run = false;
						if (pl->m_API && pl->m_API->QueryRunning(run_msg, sizeof(run_msg)-1))
						{
							run = true;
						}
						if (run)
						{
							CONMSG("Plugin %d is running.\n", id);
						}
						else
						{
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

				return true;
			}
			else
			{
				CONMSG("Usage: meta info <id>\n");

				return true;
			}
		}
		else if (strcmp(command, "pause") == 0)
		{
			if (args >= 2)
			{
				int id = atoi(info->GetArg(2));

				char error[255];

				if (!g_PluginMngr.Pause(id, error, sizeof(error)))
				{
					CONMSG("Pause failed: %s\n", error);
					return true;
				}
				
				CONMSG("Plugin %d has been paused.\n", id);

				return true;
			}
			else
			{
				CONMSG("Usage: meta pause <id>\n");

				return true;
			}
		}
		else if (strcmp(command, "unpause") == 0)
		{
			if (args >= 2)
			{
				int id = atoi(info->GetArg(2));
				char error[255];

				if (!g_PluginMngr.Unpause(id, error, sizeof(error)))
				{
					CONMSG("Unpause failed: %s\n", error);
					return true;
				}

				CONMSG("Plugin %d has been unpaused.\n", id);

				return true;
			}
			else
			{
				CONMSG("Usage: meta unpause <id>\n");

				return true;
			}
		}
		else if (strcmp(command, "load") == 0)
		{
			if (args >= 2)
			{
				const char *file = info->GetArg(2);
				char full_path[255];

				const char *alias = g_PluginMngr.LookupAlias(file);
				if (alias)
				{
					file = alias;
				}

				g_Metamod.GetFullPluginPath(file, full_path, sizeof(full_path));

				char error[255]={0};
				bool already;
				CPluginManager::CPlugin *pl;

				// If we've recently tried to unload plugins, they might still
				// be in the unload queue. Force them out now. This is not
				// lowered to CPluginManager because it's not strictly safe
				// there.
				g_SourceHook.ResolvePendingUnloads(true);

				PluginId id = g_PluginMngr.Load(full_path, Pl_Console, already, error, sizeof(error));
				pl = g_PluginMngr.FindById(id);
				if (!pl || id < Pl_MinId || (pl->m_Status < Pl_Paused))
				{
					CONMSG("Failed to load plugin %s (%s).\n", file, error);
					return true;
				}

				if (!already)
				{
					CONMSG("Plugin \"%s\" loaded with id %d.\n", pl->m_API->GetName(), pl->m_Id);
				} else {
					CONMSG("Plugin \"%s\" is already loaded as %d.\n", pl->m_API->GetName(), pl->m_Id);
				}
				
				return true;
			}
			else
			{
				CONMSG("Usage: meta load <path>\n");

				return true;
			}
		}
		else if ( (strcmp(command, "alias") == 0) ||
					(strcmp(command, "aliases") == 0) )
		{
			if (args >= 3)
			{
				const char *alias = info->GetArg(2);
				const char *value = info->GetArg(3);

				g_PluginMngr.SetAlias(alias, value);
				if (value == NULL || value[0] == '\0')
				{
					CONMSG("Deleted alias: %s.\n", alias);
				} else {
					CONMSG("Set alias \"%s\" to: %s\n", alias, value);
				}
			}
			else if (args >= 2)
			{
				const char *alias = info->GetArg(2);
				const char *value = g_PluginMngr.LookupAlias(alias);
				if (value)
				{
					CONMSG("Alias \"%s\" is set to: %s\n", alias, value);
				} else {
					CONMSG("Alias \"%s\" was not found.\n", alias);
				}
			}
			else
			{
				List<CNameAlias *>::iterator iter, end;
				CNameAlias *p;

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
				}
				else
				{
					CONMSG("No aliases found.\n");
				}
			}
			return true;
		}
		else if (strcmp(command, "unload") == 0)
		{
			if (args >= 2)
			{
				const char *file = info->GetArg(2);
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

					g_Metamod.GetFullPluginPath(file, full_path, sizeof(full_path));

					List<CPluginManager::CPlugin *>::iterator iter, end;
					CPluginManager::CPlugin *pl;
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
						return true;
					}
				}

				if (!g_PluginMngr.Unload(id, false, error, sizeof(error)))
				{
					CONMSG("Unload failed: %s\n", error);
					return true;
				}

				CONMSG("Plugin %d unloaded.\n", id);
			}
			else
			{
				CONMSG("Usage: meta unload <id>\n");
			}
			return true;
		}
		else if (strcmp(command, "force_unload") == 0)
		{
			if (args >= 2)
			{
				int id = atoi(info->GetArg(2));
				char error[255]={0};

				if (!g_PluginMngr.Unload(id, false, error, sizeof(error)))
				{
					CONMSG("Force unload failed: %s\n", error);
					return true;
				}

				CONMSG("Plugin %d force unloaded.\n", id);

				return true;
			}
			else
			{
				CONMSG("Usage: meta force_unload <id>\n");

				return true;
			}
		}
		else if (strcmp(command, "clear") == 0)
		{
			if (!g_PluginMngr.UnloadAll())
			{
				CONMSG("One or more plugins resisted removal (cleaned anyway).\n");
				return true;
			} 

			CONMSG("All plugins unloaded.\n");

			return true;
		}
		else if (strcmp(command, "retry") == 0)
		{
			if (args >= 2)
			{
				int id = atoi(info->GetArg(2));
				char error[255];

				if (!g_PluginMngr.Retry(id, error, sizeof(error)))
				{
					CONMSG("Error reloading plugin: %s\n", error);
					return true;
				}

				CONMSG("Plugin %d successfully reloaded.\n", id);

				return true;
			}
			else
			{
				CONMSG("Usage: meta retry <id>\n");

				return true;
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

	return true;
}

#if SOURCE_ENGINE == SE_DOTA
bool Command_ClientMeta(int client, IMetamodSourceCommandInfo *info)
#else
bool Command_ClientMeta(edict_t *client, IMetamodSourceCommandInfo *info)
#endif
{
	const char *cmd = info->GetArg(0);

	if (strcmp(cmd, "meta") == 0)
	{
		unsigned int args = info->GetArgCount();
		if (args == 1)
		{
			const char *subcmd = info->GetArg(1);

			if (strcmp(subcmd, "credits") == 0)
			{
				CLIENT_CONMSG(client, "Metamod:Source was developed by:\n");
				CLIENT_CONMSG(client, "  SourceHook: Pavol \"PM OnoTo\" Marko\n");
				CLIENT_CONMSG(client, "  GameDLL/Plugins: David \"BAILOPAN\" Anderson\n");
				CLIENT_CONMSG(client, "  GameDLL: Scott \"DS\" Ehlert\n");
				CLIENT_CONMSG(client, "For more information, see the official website\n");
				CLIENT_CONMSG(client, "http://www.metamodsource.net/\n");

				return true;
			}
			else if(strcmp(subcmd, "version") == 0)
			{
				CLIENT_CONMSG(client, "Metamod:Source version %s\n", METAMOD_VERSION);
				CLIENT_CONMSG(client, "Compiled on: %s\n", SOURCEMM_DATE);
				CLIENT_CONMSG(client, "Plugin interface version: %d:%d\n", METAMOD_PLAPI_VERSION, PLAPI_MIN_VERSION);
				CLIENT_CONMSG(client, "SourceHook version: %d:%d\n", g_SHPtr->GetIfaceVersion(), g_SHPtr->GetImplVersion());
				CLIENT_CONMSG(client, "http://www.metamodsource.net/\n");

				return true;
			}
			else if(strcmp(subcmd, "list") == 0)
			{
				CPluginManager::CPlugin *pl;
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

				return true;
			}
		}

		CLIENT_CONMSG(client, "Metamod:Source Menu\n");
		CLIENT_CONMSG(client, "usage: meta <command>\n");
		CLIENT_CONMSG(client, "  credits - About Metamod:Source\n");
		CLIENT_CONMSG(client, "  list    - List plugins\n");
		CLIENT_CONMSG(client, "  version - Version information\n");

		return true;
	}

	return false;
}
