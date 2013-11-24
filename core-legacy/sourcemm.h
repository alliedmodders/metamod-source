/* ======== SourceMM ========
 * Copyright (C) 2004-2009 Metamod:Source Development Team
 * No warranties of any kind
 *
 * License: zlib/libpng
 *
 * Author(s): David "BAILOPAN" Anderson
 * ============================
 */

#ifndef _INCLUDE_SOURCEMM_H
#define _INCLUDE_SOURCEMM_H

/**
 * @brief SourceMM main functionality for GameDLL interception
 * @file sourcemm.h
 */

#include <interface.h>
#include <eiface.h>
#include <sourcehook/sourcehook_impl.h>
#include <sourcehook/sourcehook.h>
#include "ISmmAPI.h"
#include "CPlugin.h"
#include "oslink.h"
#include "loader_bridge.h"

/**
 * Versioning
 *   increase api_major when API breaks
 *   increase api_minor when new functions are added (non-breaking)
 */
#define		SOURCEMM_VERSION	SVN_FILE_VERSION_STRING
#define		SOURCEMM_DATE		__DATE__
#define		SM_VERS_API_MAJOR	1		//increase this on a breaking change
#define		SM_VERS_API_MINOR	5		//increase this on a non-breaking API change

//We need a good CServerGameDLL version to work properly.  We support these inclusively.
#define	MIN_GAMEDLL_VERSION	3
#define	MAX_GAMEDLL_VERSION	8


void *ServerFactory(const char *name, int *code);
void *EngineFactory(const char *name, int *code);
void *PhysicsFactory(const char *name, int *code);
void *FileSystemFactory(const char *name, int *code);

/** @brief Loads all plugins found from the mm_pluginsfile file and from VDFs in mm_basedir */
int LoadPlugins(const char *filepath, const char *vdfdir);

/** @brief Logs a message to the standard log file */
void LogMessage(const char *msg, ...);

/** @brief Stores information about the GameDLL */
struct GameDllInfo
{
	bool loaded;
	CreateInterfaceFn factory;
	IServerGameDLL *pGameDLL;
	IServerGameClients *pGameClients;
};

/** @brief Stores information about the HL2 Engine pointers */
struct EngineInfo
{
	EngineInfo() : loaded(false), original(false),
		engineFactory(NULL), physicsFactory(NULL), fileSystemFactory(NULL),
		pGlobals(NULL), icvar(NULL), engine(NULL)
	{ };
	bool loaded;
	bool original;
	CreateInterfaceFn engineFactory;
	CreateInterfaceFn physicsFactory;
	CreateInterfaceFn fileSystemFactory;
	CGlobalVars *pGlobals;
	ICvar *icvar;
	IVEngineServer *engine;
};

bool AlternatelyLoadMetamod(CreateInterfaceFn ifaceFactory, CreateInterfaceFn serverFactory);

extern IServerPluginCallbacks *g_pRealVspCallbacks;
extern bool g_bIsBridgedAsVsp;

/** @brief Global variable for GameDLL info */
extern GameDllInfo g_GameDll;

/** @brief Global variable for Engine info */
extern EngineInfo g_Engine;

/** @brief Global singleton for SourceHook */
extern SourceHook::CSourceHookImpl g_SourceHook;

/** @brief Mod path (important!)*/
extern SourceHook::String g_ModPath;

/** @brief Path to Metamod binary */
extern SourceHook::String g_MetamodPath;

/** @brief Global variable for SourceHook macros */
extern SourceHook::ISourceHook *g_SHPtr;

/** @brief We have our own internal plugin id... */
extern PluginId g_PLID;

/** @brief ServerGameDLL version that is currently loaded */
extern int g_GameDllVersion;

extern bool g_bGameInit;
extern bool g_bLevelChanged;

extern unsigned int g_vsp_version;

void UnloadMetamod(bool shutting_down);

void LoadAsGameDLL(const gamedll_bridge_info *info);

#endif //_INCLUDE_SOURCEMM_H

