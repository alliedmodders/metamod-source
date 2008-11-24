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

#include <stdio.h>
#include "metamod_oslink.h"
#include "metamod.h"
#include <interface.h>
#include <eiface.h>
#include "metamod_provider.h"
#include "metamod_plugins.h"
#include "metamod_util.h"
#include "metamod_console.h"
#include "provider/provider_ep2.h"

using namespace SourceMM;
using namespace SourceHook;
using namespace SourceHook::Impl;

/**
 * @brief Implementation of main SourceMM GameDLL functionality
 * @file sourcemm.cpp
 */

SH_DECL_MANUALHOOK0(SGD_GameInit, 0, 0, 0, bool);
SH_DECL_MANUALHOOK6(SGD_LevelInit, 0, 0, 0, bool, const char *, const char *, const char *, const char *, bool, bool);
SH_DECL_MANUALHOOK0_void(SGD_LevelShutdown, 0, 0, 0);

static void
Handler_LevelShutdown();

static bool
Handler_LevelInit(char const *pMapName,
				  char const *pMapEntities,
				  char const *pOldLevel,
				  char const *pLandmarkName,
				  bool loadGame,
				  bool background);

static bool
Handler_GameInit();

static void
InitializeVSP();

static void
LookForVDFs(const char *dir);

struct game_dll_t
{
	CreateInterfaceFn factory;
};

static String mod_path;
static String metamod_path;
static String full_bin_path;
static int vsp_version = 0;
static int gamedll_version = 0;
static int engine_build = SOURCE_ENGINE_UNKNOWN;
static List<game_dll_t *> gamedll_list;
static bool is_gamedll_loaded = false;
static bool in_first_level = true;
static bool is_game_init = false;
static bool vsp_load_requested = false;
static bool vsp_loaded = false;
static game_dll_t gamedll_info;
static ConVar *metamod_version = NULL;
static ConVar *mm_pluginsfile = NULL;
static ConVar *mm_basedir = NULL;
static CreateInterfaceFn engine_factory = NULL;
static CreateInterfaceFn physics_factory = NULL;
static CreateInterfaceFn filesystem_factory = NULL;
static CGlobalVars *gpGlobals = NULL;
static CHookManagerAutoGen g_SH_HookManagerAutoGen(&g_SourceHook);
static META_RES last_meta_res;
static IServerPluginCallbacks *vsp_callbacks = NULL;
static bool were_plugins_loaded = false;
static bool g_bIsVspBridged = false;

MetamodSource g_Metamod;
PluginId g_PLID = Pl_Console;
CSourceHookImpl g_SourceHook;
ISourceHook *g_SHPtr = &g_SourceHook;
SourceMM::ISmmAPI *g_pMetamod = &g_Metamod;

/* Helper Macro */
#define	IFACE_MACRO(orig,nam) \
	CPluginManager::CPlugin *pl; \
	SourceHook::List<IMetamodListener *>::iterator event; \
	IMetamodListener *api; \
	int mret = 0; \
	void *val = NULL; \
	for (PluginIter iter = g_PluginMngr._begin(); iter != g_PluginMngr._end(); iter++) { \
		pl = (*iter); \
		for (event=pl->m_Events.begin(); event!=pl->m_Events.end(); event++) { \
			api = (*event); \
			mret = IFACE_FAILED; \
			if ( (val=api->On##nam##Query(iface, &mret)) != NULL ) { \
				if (ret) *ret = mret; \
				return val; \
			} \
		} \
	} \
	return (orig)(iface, ret);

#define ITER_EVENT(evn, args) \
	CPluginManager::CPlugin *pl; \
	SourceHook::List<IMetamodListener *>::iterator event; \
	IMetamodListener *api; \
	for (PluginIter iter = g_PluginMngr._begin(); iter != g_PluginMngr._end(); iter++) { \
		pl = (*iter); \
		for (event=pl->m_Events.begin(); event!=pl->m_Events.end(); event++) { \
			api = (*event); \
			api->evn args; \
		} \
	}

/* Initialize everything here */
void
mm_InitializeForLoad()
{
	char full_path[PATH_SIZE] = {0};
	GetFileOfAddress((void *)gamedll_info.factory, full_path, sizeof(full_path));
	full_bin_path.assign(full_path);

	/* Like Metamod, reload plugins at the end of the map.
	 * This is so plugins can hook everything on load, BUT, new plugins will be reloaded
	 * if the server is shut down (silly, but rare case).
	 */
	in_first_level = true;

	SourceHook::MemFuncInfo info;

	if (!provider->GetHookInfo(ProvidedHook_GameInit, &info))
	{
		provider->DisplayError("Metamod:Source could not find a valid hook for IServerGameDLL::GameInit");
	}
	SH_MANUALHOOK_RECONFIGURE(SGD_GameInit, info.vtblindex, info.vtbloffs, info.thisptroffs);
	SH_ADD_MANUALHOOK_STATICFUNC(SGD_GameInit, server, Handler_GameInit, false);

	if (!provider->GetHookInfo(ProvidedHook_LevelInit, &info))
	{
		provider->DisplayError("Metamod:Source could not find a valid hook for IServerGameDLL::LevelInit");
	}
	SH_MANUALHOOK_RECONFIGURE(SGD_LevelInit, info.vtblindex, info.vtbloffs, info.thisptroffs);
	SH_ADD_MANUALHOOK_STATICFUNC(SGD_LevelInit, server, Handler_LevelInit, true);

	if (!provider->GetHookInfo(ProvidedHook_LevelShutdown, &info))
	{
		provider->DisplayError("Metamod:Source could not find a valid hook for IServerGameDLL::LevelShutdown");
	}
	SH_MANUALHOOK_RECONFIGURE(SGD_LevelShutdown, info.vtblindex, info.vtbloffs, info.thisptroffs);
	SH_ADD_MANUALHOOK_STATICFUNC(SGD_LevelShutdown, server, Handler_LevelShutdown, true);
}

bool
mm_DetectGameInformation()
{
	char mm_path[PATH_SIZE];
	char game_path[PATH_SIZE];

	/* Get path to SourceMM DLL */
	if (!GetFileOfAddress((void *)mm_InitializeForLoad, mm_path, sizeof(mm_path)))
	{
		return false;
	}

	metamod_path.assign(mm_path);

	/* Get value of -game from command line, defaulting to hl2 as engine seems to do */
	const char *game_dir = provider->GetCommandLineValue("-game", "hl2");

	/* Get absolute path */
	abspath(game_path, game_dir);
	mod_path.assign(game_path);

	engine_build = provider->DetermineSourceEngine(game_dir);;

	return true;
}

void *
ServerFactory(const char *iface, int *ret)
{
	IFACE_MACRO(gamedll_info.factory, GameDLL);
}

SMM_API void *
CreateInterface(const char *iface, int *ret)
{
	void *ptr = NULL;

	if (!mm_IsVspBridged() && strncmp(iface, "ISERVERPLUGINCALLBACKS", 22) == 0)
	{
		if (vsp_callbacks != NULL)
		{
			if (ret != NULL)
				*ret = IFACE_FAILED;
			return NULL;
		}

		vsp_version = atoi(&iface[22]);
		ptr = provider->GetVSPCallbacks(vsp_version);

		if (ptr == NULL)
			vsp_version = 0;
	}

	if (ret)
		*ret = (ptr != NULL) ? IFACE_OK : IFACE_FAILED;

	return ptr;
}

static int
LoadPluginsFromFile(const char *filepath, int &skipped)
{
	FILE *fp;
	int total = 0;
	PluginId id;
	bool already;

	skipped = 0;

	fp = fopen(filepath, "rt");
	if (!fp)
	{
		return 0;
	}

	char buffer[255], error[255], full_path[PATH_SIZE];
	const char *ptr, *ext, *file;
	size_t length;
	while (!feof(fp) && fgets(buffer, sizeof(buffer), fp) != NULL)
	{
		UTIL_TrimLeft(buffer);
		UTIL_TrimRight(buffer);

		length = strlen(buffer);
		if (!length)
		{
			continue;
		}

		if (buffer[0] == '\0' || buffer[0] == ';' || strncmp(buffer, "//", 2) == 0)
		{
			continue;
		}

		file = buffer;
		if (buffer[0] == '"')
		{
			char *cptr = buffer;
			file = ++cptr;

			while (*cptr)
			{
				if (*cptr == '"')
				{
					*cptr = '\0';
					break;
				}
				cptr++;
			}
		}
		else
		{
			char *cptr = buffer;
			while (*cptr)
			{
				if (isspace(*cptr))
				{
					char *optr = cptr;
					while (*cptr && isspace(*cptr))
					{
						cptr++;
					}
					*optr = '\0';
					UTIL_TrimRight(cptr);
					if (*cptr && isalpha(*cptr))
					{
						g_PluginMngr.SetAlias(buffer, cptr);
						file = cptr;
					}
					break;
				}
				cptr++;
			}
		}
		if (!file[0])
		{
			continue;
		}
		/* First find if it's an absolute path or not... */
		if (file[0] == '/' || strncmp(&(file[1]), ":\\", 2) == 0)
		{
			/* If we're in an absolute path, ignore our normal heuristics */
			id = g_PluginMngr.Load(file, Pl_File, already, error, sizeof(error));
			if (id < Pl_MinId || g_PluginMngr.FindById(id)->m_Status < Pl_Paused)
			{
				mm_LogMessage("[META] Failed to load plugin %s.  %s", buffer, error);
			}
			else
			{
				if (already)
					skipped++;
				else
					total++;
			}
		}
		else
		{
			/* Attempt to find a file extension */
			ptr = UTIL_GetExtension(file);
			/* Add an extension if there's none there */
			if (!ptr)
			{
#if defined WIN32 || defined _WIN32
				ext = ".dll";
#else
				ext = "_i486.so";
#endif
			}
			else
			{
				ext = "";
			}
			/* Format the new path */
			g_Metamod.PathFormat(full_path, sizeof(full_path), "%s/%s%s", mod_path.c_str(), file, ext);
			id = g_PluginMngr.Load(full_path, Pl_File, already, error, sizeof(error));
			if (id < Pl_MinId || g_PluginMngr.FindById(id)->m_Status < Pl_Paused)
			{
				mm_LogMessage("[META] Failed to load plugin %s.  %s", buffer, error);
			}
			else
			{
				if (already)
					skipped++;
				else
					total++;
			}
		}
	}
	fclose(fp);
	
	return total;
}

void InitializeVSP()
{
	if (g_bIsVspBridged)
		return;

	size_t len;
	char engine_file[PATH_SIZE];
	char engine_path[PATH_SIZE];
	char rel_path[PATH_SIZE * 2];

	GetFileOfAddress((void *)engine_factory, engine_file, sizeof(engine_file));

	/* Chop off the "engine" file part */
	len = strlen(engine_file);
	for (size_t i = len - 1; i >= 0 && i < len; i--)
	{
		if (engine_file[i] == '/' || engine_file[i] == '\\')
		{
			engine_file[i] = '\0';
			break;
		}
	}
	abspath(engine_path, engine_file);

	const char *usepath = metamod_path.c_str();
	if (UTIL_Relatize(rel_path, sizeof(rel_path), engine_path, metamod_path.c_str()))
	{
		usepath = rel_path;
	}

	char command[PATH_SIZE * 2];
	UTIL_Format(command, sizeof(command), "plugin_load \"%s\"\n", usepath);
	provider->ServerCommand(command);
}

/* Wrapper function.  This is called when the GameDLL thinks it's using
 * the engine's real engineFactory.
 */
static void *
EngineFactory(const char *iface, int *ret)
{
	IFACE_MACRO(engine_factory, Engine);
}

/* Wrapper function.  This is called when the GameDLL thinks it's using
 * the engine's real physicsFactory.
 */
static void *
PhysicsFactory(const char *iface, int *ret)
{
	IFACE_MACRO(physics_factory, Physics);
}

/* Wrapper function.  This is called when the GameDLL thinks it's using
 * the engine's real fileSystemFactory.
 */
static void *
FileSystemFactory(const char *iface, int *ret)
{
	IFACE_MACRO(filesystem_factory, FileSystem);
}

void
mm_LogMessage(const char *msg, ...)
{
	va_list ap;
	static char buffer[2048];

	va_start(ap, msg);
	size_t len = vsnprintf(buffer, sizeof(buffer) - 2, msg, ap);
	va_end(ap);

	buffer[len++] = '\n';
	buffer[len] = '\0';

	if (!provider->LogMessage(buffer))
	{
		fprintf(stdout, "%s", buffer);
	}
}

static void
DoInitialPluginLoads()
{
	const char *pluginFile = provider->GetCommandLineValue("mm_pluginsfile", NULL);
	const char *mmBaseDir = provider->GetCommandLineValue("mm_basedir", NULL);
	if (!pluginFile) 
	{
		pluginFile = provider->GetConVarString(mm_pluginsfile);
		if (pluginFile == NULL)
		{
			pluginFile = "addons/metamod/metaplugins.ini";
		}
	}
	if (!mmBaseDir)
	{
		mmBaseDir = provider->GetConVarString(mm_basedir);
		if (mmBaseDir == NULL)
		{
			mmBaseDir = "addons/metamod";
		}
	}

	char filepath[PATH_SIZE], vdfpath[PATH_SIZE];

	g_Metamod.PathFormat(filepath, sizeof(filepath), "%s/%s", mod_path.c_str(), pluginFile);
	g_Metamod.PathFormat(vdfpath, sizeof(vdfpath), "%s/%s", mod_path.c_str(), mmBaseDir);
	mm_LoadPlugins(filepath, vdfpath);
}

void
mm_StartupMetamod(bool is_vsp_load)
{
	char buffer[255];

	UTIL_Format(buffer,
		sizeof(buffer),
		"%s%s",
		MMS_FULL_VERSION,
		is_vsp_load ? "V" : "");

	metamod_version = provider->CreateConVar("metamod_version", 
		MMS_FULL_VERSION, 
		"Metamod:Source Version",
		ConVarFlag_Notify|ConVarFlag_SpOnly);

	provider->SetConVarString(metamod_version, buffer);

	mm_pluginsfile = provider->CreateConVar("mm_pluginsfile", 
#if defined WIN32 || defined _WIN32
		"addons\\metamod\\metaplugins.ini", 
#else
		"addons/metamod/metaplugins.ini",
#endif
		"Metamod:Source Plugins File",
		ConVarFlag_SpOnly);

	mm_basedir = provider->CreateConVar("mm_basedir",
#if defined __linux__
		"addons/metamod",
#else
		"addons\\metamod",
#endif
		"Metamod:Source Base Folder",
		ConVarFlag_SpOnly);
	
	g_bIsVspBridged = is_vsp_load;

	if (!is_vsp_load)
	{
		DoInitialPluginLoads();
		in_first_level = true;
	}
}

void
mm_InitializeGlobals(CreateInterfaceFn engineFactory, 
					 CreateInterfaceFn physicsFactory,
					 CreateInterfaceFn filesystemFactory,
					 CGlobalVars *pGlobals)
{
	engine_factory = engineFactory;
	physics_factory = physicsFactory;
	filesystem_factory = filesystemFactory;
	gpGlobals = pGlobals;
	provider->Notify_DLLInit_Pre(engineFactory, gamedll_info.factory);
}

static bool
Handler_GameInit()
{
	if (is_game_init)
		return true;

	if (vsp_load_requested)
		InitializeVSP();

	if (g_bIsVspBridged && !were_plugins_loaded)
	{
		DoInitialPluginLoads();
		g_PluginMngr.SetAllLoaded();
		were_plugins_loaded = true;
	}

	is_game_init = true;

	RETURN_META_VALUE(MRES_IGNORED, true);
}

void
mm_UnloadMetamod()
{
	/* Unload plugins */
	g_PluginMngr.UnloadAll();

	provider->Notify_DLLShutdown_Pre();

	g_SourceHook.CompleteShutdown();
}

static void
Handler_LevelShutdown(void)
{
	if (g_bIsVspBridged && !were_plugins_loaded)
	{
		g_PluginMngr.SetAllLoaded();
		DoInitialPluginLoads();
		were_plugins_loaded = true;
		in_first_level = true;
	}

	if (!in_first_level)
	{
		char filepath[PATH_SIZE], vdfpath[PATH_SIZE];

		g_Metamod.PathFormat(filepath, 
			sizeof(filepath), 
			"%s/%s", 
			mod_path.c_str(),
			provider->GetConVarString(mm_pluginsfile));
		g_Metamod.PathFormat(vdfpath,
			sizeof(vdfpath),
			"%s/%s",
			mod_path.c_str(),
			provider->GetConVarString(mm_basedir));
		mm_LoadPlugins(filepath, vdfpath);
	}
	else
	{
		in_first_level = false;
	}

	ITER_EVENT(OnLevelShutdown, ());

	RETURN_META(MRES_IGNORED);
}

static bool
Handler_LevelInit(char const *pMapName,
				  char const *pMapEntities,
				  char const *pOldLevel,
				  char const *pLandmarkName,
				  bool loadGame,
				  bool background)
{
	ITER_EVENT(OnLevelInit, (pMapName, pMapEntities, pOldLevel, pLandmarkName, loadGame, background));

	RETURN_META_VALUE(MRES_IGNORED, false);
}

void MetamodSource::LogMsg(ISmmPlugin *pl, const char *msg, ...)
{
	va_list ap;
	char buffer[2048];
	
	va_start(ap, msg);
	UTIL_FormatArgs(buffer, sizeof(buffer), msg, ap);
	va_end(ap);

	mm_LogMessage("[%s] %s", pl->GetLogTag(), buffer);
}

CreateInterfaceFn MetamodSource::GetEngineFactory(bool syn/* =true */)
{
	if (syn)
		return EngineFactory;
	return engine_factory;
}

CreateInterfaceFn MetamodSource::GetPhysicsFactory(bool syn/* =true */)
{
	if (syn)
		return PhysicsFactory;
	return physics_factory;
}

CreateInterfaceFn MetamodSource::GetFileSystemFactory(bool syn/* =true */)
{
	if (syn)
		return FileSystemFactory;
	return filesystem_factory;
}

CreateInterfaceFn MetamodSource::GetServerFactory(bool syn/* =true */)
{
	if (syn)
		return ServerFactory;
	return gamedll_info.factory;
}

CGlobalVars *MetamodSource::GetCGlobals()
{
	return gpGlobals;
}

void MetamodSource::SetLastMetaReturn(META_RES res)
{
	last_meta_res = res;
}

META_RES MetamodSource::GetLastMetaReturn()
{
	return last_meta_res;
}

void MetamodSource::ConPrint(const char *str)
{
	provider->ConsolePrint(str);
}

void MetamodSource::ConPrintf(const char *fmt, ...)
{
	va_list ap;
	char buffer[2048];

	va_start(ap, fmt);
	UTIL_FormatArgs(buffer, sizeof(buffer), fmt, ap);
	va_end(ap);

	provider->ConsolePrint(buffer);
}

void MetamodSource::GetApiVersions(int &major, int &minor, int &plvers, int &plmin)
{
	major = METAMOD_API_MAJOR;
	minor = METAMOD_API_MINOR;
	plvers = METAMOD_PLAPI_VERSION;
	plmin = PLAPI_MIN_VERSION;
}

void MetamodSource::GetShVersions(int &shvers, int &shimpl)
{
	shvers = SH_IFACE_VERSION;
	shimpl = SH_IMPL_VERSION;
}

int MetamodSource::FormatIface(char iface[], unsigned int maxlength)
{
	int length = (int)strlen(iface);
	int i;
	int num = 0;

	for (i = length - 1; i >= 0; i--)
	{
		if (!isdigit(iface[i]))
		{
			if (i != length - 1)
			{
				num = 1;
			}
			break;
		}
	}

	if ( (num && ((int)maxlength <= length)) || (!num && ((int)maxlength <= length + 3)) )
	{
		return -1;
	}

	if (i != length - 1)
	{
		num = atoi(&(iface[++i]));
	}

	num++;

	snprintf(&(iface[i]), 4, "%03d", num);

	return num;
}

void *MetamodSource::InterfaceSearch(CreateInterfaceFn fn, const char *iface, int max, int *ret)
{
	char _if[256];	/* assume no interface goes beyond this */
	size_t len = strlen(iface);
	int num = 0;
	void *pf = NULL;

	if (max > 999)
	{
		max = 999;
	}

	if (len + 4 > sizeof(_if))
	{
		if (ret)
		{
			*ret = IFACE_FAILED;
		}
		return NULL;
	}

	strcpy(_if, iface);

	do
	{
		if ((pf = (fn)(_if, ret)) != NULL)
		{
			break;
		}
		if (num > max)
		{
			break;
		}
	} while ((num = FormatIface(_if, len+1)));

	return pf;
}

void *MetamodSource::VInterfaceMatch(CreateInterfaceFn fn, const char *iface, int min)
{
	char buffer[256];	/* assume no interface will go beyond this */
	size_t len = strlen(iface);
	int ret;			/* just in case something doesn't handle NULL properly */

	if (len > sizeof(buffer) - 4)
	{
		return NULL;
	}

	strcpy(buffer, iface);

	if (min != -1)
	{
		char *ptr = &buffer[len - 1];
		int digits = 0;
		while (isdigit(*ptr) && digits <=3)
		{
			*ptr = '\0';
			digits++;
			ptr--;
		}
		if (digits != 3)
		{
			/* for now, assume this is an error */
			strcpy(buffer, iface);
		}
		else
		{
			char num[4];
			min = (min == 0) ? 1 : min;
			snprintf(num, sizeof(num), "%03d", min);
			strcat(buffer, num);
		}
	}

	return InterfaceSearch(fn, buffer, IFACE_MAXNUM, &ret);
}

const char *MetamodSource::GetBaseDir()
{
	return mod_path.c_str();
}

size_t MetamodSource::PathFormat(char *buffer, size_t len, const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	size_t mylen = UTIL_FormatArgs(buffer, len, fmt, ap);
	va_end(ap);

	for (size_t i = 0; i < mylen; i++)
	{
		if (buffer[i] == ALT_SEP_CHAR)
		{
			buffer[i] = PATH_SEP_CHAR;
		}
	}

	return mylen;
}

void MetamodSource::ClientConPrintf(edict_t *client, const char *fmt, ...)
{
	va_list ap;
	char buffer[2048];

	va_start(ap, fmt);
	UTIL_FormatArgs(buffer, sizeof(buffer), fmt, ap);
	va_end(ap);

	provider->ClientConsolePrint(client, buffer);
}

void MetamodSource::EnableVSPListener()
{
	if (is_game_init && !vsp_load_requested && !vsp_loaded)
		InitializeVSP();

	vsp_load_requested = true;
}

int MetamodSource::GetVSPVersion()
{
	return vsp_version;
}

int MetamodSource::GetGameDLLVersion()
{
	return gamedll_version;
}

bool MetamodSource::RemotePrintingAvailable()
{
	return provider->IsRemotePrintingAvailable();
}

void *MetamodSource::MetaFactory(const char *iface, int *ret, PluginId *id)
{
	if (id)
	{
		*id = 0;
	}

	if (!iface)
	{
		return NULL;
	}

	/* First check ours... we get first chance! */
	if (strcmp(iface, MMIFACE_SOURCEHOOK) == 0)
	{
		if (ret)
		{
			*ret = IFACE_OK;
		}
		return static_cast<void *>(static_cast<SourceHook::ISourceHook *>(&g_SourceHook));
	}
	else if (strcmp(iface, MMIFACE_PLMANAGER) == 0)
	{
		if (ret)
		{
			*ret = IFACE_OK;
		}
		return static_cast<void *>(static_cast<ISmmPluginManager *>(&g_PluginMngr));
	}
	else if (strcmp(iface, MMIFACE_SH_HOOKMANAUTOGEN) == 0)
	{
		if (ret)
		{
			*ret = IFACE_OK;
		}
		return static_cast<void *>(static_cast<SourceHook::IHookManagerAutoGen *>(&g_SH_HookManagerAutoGen));
	}

	CPluginManager::CPlugin *pl;
	List<IMetamodListener *>::iterator event;
	IMetamodListener *api;
	void *value;
	
	int subret = 0;
	for (PluginIter iter = g_PluginMngr._begin();
		 iter != g_PluginMngr._end();
		 iter++)
	{
		pl = (*iter);
		for (event = pl->m_Events.begin(); event != pl->m_Events.end(); event++)
		{
			api = (*event);
			subret = IFACE_FAILED;
			if ((value = api->OnMetamodQuery(iface, &subret)) != NULL)
			{
				if (ret)
				{
					*ret = subret;
				}
				if (id)
				{
					*id = pl->m_Id;
				}
				return value;
			}
		}
	}

	if (ret)
	{
		*ret = IFACE_FAILED;
	}

	return NULL;
}

void MetamodSource::AddListener(ISmmPlugin *plugin, IMetamodListener *pListener)
{
	CPluginManager::CPlugin *pl = g_PluginMngr.FindByAPI(plugin);

	pl->m_Events.push_back(pListener);
}

const char *MetamodSource::GetGameBinaryPath()
{
	return full_bin_path.c_str();
}

const char *MetamodSource::GetPluginsFile()
{
	return provider->GetConVarString(mm_pluginsfile);
}

const char *MetamodSource::GetVDFDir()
{
	return provider->GetConVarString(mm_basedir);
}

IConCommandBaseAccessor *MetamodSource::GetCvarBaseAccessor()
{
	return provider->GetConCommandBaseAccessor();
}

bool MetamodSource::RegisterConCommandBase(ISmmPlugin *plugin, ConCommandBase *pCommand)
{
	if (provider->IsConCommandBaseACommand(pCommand))
	{
		g_PluginMngr.AddPluginCmd(plugin, pCommand);
	}
	else
	{
		g_PluginMngr.AddPluginCvar(plugin, pCommand);
	}

	return provider->RegisterConCommandBase(pCommand);
}

void MetamodSource::UnregisterConCommandBase(ISmmPlugin *plugin, ConCommandBase *pCommand)
{
	if (provider->IsConCommandBaseACommand(pCommand))
	{
		g_PluginMngr.RemovePluginCmd(plugin, pCommand);
	}
	else
	{
		g_PluginMngr.RemovePluginCvar(plugin, pCommand);
	}

	CPluginManager::CPlugin *pOrig = g_PluginMngr.FindByAPI(plugin);
	UnregisterConCommandBase(pOrig ? pOrig->m_Id : 0, pCommand);
}

void MetamodSource::UnregisterConCommandBase(PluginId id, ConCommandBase *pCommand)
{
	PluginIter iter;
	CPluginManager::CPlugin *pPlugin;
	List<IMetamodListener *>::iterator event;
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

	return provider->UnregisterConCommandBase(pCommand);
}

int MetamodSource::GetUserMessageCount()
{
	return provider->GetUserMessageCount();
}

int MetamodSource::FindUserMessage(const char *name, int *size/* =NULL */)
{
	return provider->FindUserMessage(name, size);
}

const char *MetamodSource::GetUserMessage(int index, int *size/* =NULL */)
{
	return provider->GetUserMessage(index, size);
}

int MetamodSource::GetSourceEngineBuild()
{
	return engine_build;
}

void MetamodSource::NotifyVSPListening(IServerPluginCallbacks *callbacks, int version)
{
	if (version != -1)
		vsp_version = version;
	
	vsp_callbacks = callbacks;
	ITER_EVENT(OnVSPListening, (callbacks));
}

IServerPluginCallbacks *MetamodSource::GetVSPInfo(int *pVersion)
{
	if (pVersion)
	{
		*pVersion = vsp_version;
	}

	return vsp_callbacks;
}

size_t MetamodSource::Format(char *buffer, size_t maxlength, const char *format, ...)
{
	va_list ap;
	size_t result;

	va_start(ap, format);
	result = FormatArgs(buffer, maxlength, format, ap);
	va_end(ap);

	return result;
}

size_t MetamodSource::FormatArgs(char *buffer, size_t maxlength, const char *format, va_list ap)
{
	return UTIL_FormatArgs(buffer, maxlength, format, ap);
}

bool MetamodSource::IsLoadedAsGameDLL()
{
	return is_gamedll_loaded;
}

void MetamodSource::SetGameDLLInfo(CreateInterfaceFn serverFactory, int version, bool loaded)
{
	gamedll_info.factory = serverFactory;
	gamedll_version = version;
	is_gamedll_loaded = loaded;
}

static bool
ProcessVDF(const char *path, bool &skipped)
{
	PluginId id;
	bool already;
	char alias[24], file[255], error[255];

	if (!provider->ProcessVDF(path, file, sizeof(file), alias, sizeof(alias)))
	{
		skipped = false;
		return false;
	}

	if (alias[0] != '\0')
		g_PluginMngr.SetAlias(alias, file);

	id = g_PluginMngr.Load(file, Pl_File, already, error, sizeof(error));
	skipped = already;
	if (id < Pl_MinId || g_PluginMngr.FindById(id)->m_Status < Pl_Paused)
	{
		mm_LogMessage("[META] Failed to load plugin %s: %s", file, error);
		return false;
	}

	return true;
}

static int
LoadVDFPluginsFromDir(const char *dir, int &skipped)
{
	bool success, skip;
	int total = 0;
	char path[MAX_PATH];
	char relpath[MAX_PATH * 2];

	skipped = 0;

#if defined _MSC_VER
	HANDLE hFind;
	WIN32_FIND_DATA fd;
	char error[255];

	g_Metamod.PathFormat(path, sizeof(path), "%s\\*.vdf", dir);
	if ((hFind = FindFirstFile(path, &fd)) == INVALID_HANDLE_VALUE)
	{
		DWORD dw = GetLastError();
		if (dw == ERROR_FILE_NOT_FOUND)
			return 0;

		FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
			NULL,
			dw,
			MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
			error,
			sizeof(error),
			NULL);
		mm_LogMessage("[META] Could not open folder \"%s\" (%s)", dir, error);
		return 0;
	}

	do
	{
		g_Metamod.PathFormat(path, sizeof(path), "%s\\%s", dir, fd.cFileName);
		UTIL_Relatize(relpath, sizeof(relpath), mod_path.c_str(), path);
		success = ProcessVDF(relpath, skip);
		if (skip)
			skipped++;
		else if (success)
			total++;
	} while (FindNextFile(hFind, &fd));

	FindClose(hFind);
#else
	DIR *pDir;
	struct dirent *pEnt;
	int extidx;

	if ((pDir = opendir(dir)) == NULL)
	{
		mm_LogMessage("[META] Could not open folder \"%s\" (%s)", dir, strerror(errno));
		return 0;
	}

	while ((pEnt = readdir(pDir)) != NULL)
	{
		if (strcmp(pEnt->d_name, ".") == 0
			|| strcmp(pEnt->d_name, "..") == 0)
		{
			continue;
		}
		extidx = strlen(pEnt->d_name) - 4;
		if (extidx < 0 || stricmp(&pEnt->d_name[extidx], ".vdf"))
		{
			continue;
		}
		g_Metamod.PathFormat(path, sizeof(path), "%s/%s", dir, pEnt->d_name);
		UTIL_Relatize(relpath, sizeof(relpath), mod_path.c_str(), path);
		success = ProcessVDF(relpath, skip);
		if (skip)
			skipped++;
		else if (success)
			total++;
	}

	closedir(pDir);
#endif

	return total;
}

int
mm_LoadPlugins(const char *filepath, const char *vdfpath)
{
	int total, skipped, fskipped, vskipped;
	const char *s = "";

	total = LoadPluginsFromFile(filepath, fskipped);
	total += LoadVDFPluginsFromDir(vdfpath, vskipped);
	skipped = fskipped + vskipped;

	if (total == 0 || total > 1)
		s = "s";

	if (skipped)
		mm_LogMessage("[META] Loaded %d plugin%s (%d already loaded)", total, s, skipped);
	else
		mm_LogMessage("[META] Loaded %d plugin%s.", total, s);

	return total;
}

bool
mm_IsVspBridged()
{
	return g_bIsVspBridged;
}

bool
mm_IsVspLoadComplete()
{
	return were_plugins_loaded;
}

