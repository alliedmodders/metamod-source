/* ======== SourceMM ========
 * Copyright (C) 2004-2007 Metamod:Source Development Team
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

#include <string>
#include <interface.h>
#include <eiface.h>
#include <sourcehook/sourcehook_impl.h>
#include <sourcehook/sourcehook.h>
#include "ISmmAPI.h"
#include "CPlugin.h"
#include "oslink.h"
#include "util.h"
#include "svn_version.h"

/**
 * Versioning
 *   increase api_major when API breaks
 *   increase api_minor when new functions are added (non-breaking)
 */
#define SOURCEMM_VERSION	SVN_FILE_VERSION_STRING
#define SOURCEMM_DATE		__DATE__
#define SM_VERS_API_MAJOR	1		//increase this on a breaking change
#define SM_VERS_API_MINOR	7		//increase this on a non-breaking API change

/* We need a good CServerGameDLL version to work properly.  We support these inclusively. */
#define	MIN_GAMEDLL_VERSION	3

/* Maximum version of IServerPluginCallbacks that SourceMM supports */
#define MAX_VSP_VERSION 2

/** @brief Entry point for HL2 Engine */
SMM_API void *CreateInterface(const char *name, int *code);

/** @brief Wrapper to catch GameDLL calls */
void *EngineFactory(const char *name, int *code);

/** @brief Wrapper to catch GameDLL calls */
void *PhysicsFactory(const char *name, int *code);

/** @brief Wrapper to catch GameDLL calls */
void *FileSystemFactory(const char *name, int *code);

/** @brief Loads all plugins found in a file */
int LoadPluginsFromFile(const char *file);

/** @brief Logs a message to the standard log file */
void LogMessage(const char *msg, ...);

/** @brief Stores information about the GameDLL */
struct GameDllInfo
{
	bool loaded;
	HINSTANCE lib;
	CreateInterfaceFn factory;
	IServerGameDLL *pGameDLL;
	IServerGameClients *pGameClients;
};

/** @brief Stores information about the HL2 Engine pointers */
struct EngineInfo
{
	EngineInfo() : loaded(false), 
		engineFactory(NULL), physicsFactory(NULL), fileSystemFactory(NULL),
		pGlobals(NULL), icvar(NULL), engine(NULL)
	{ };
	bool loaded;
	CreateInterfaceFn engineFactory;
	CreateInterfaceFn physicsFactory;
	CreateInterfaceFn fileSystemFactory;
	CGlobalVars *pGlobals;
	ICvar *icvar;
	IVEngineServer *engine;
};

/** @brief Global variable for GameDLL info */
extern GameDllInfo g_GameDll;

/** @brief Global variable for Engine info */
extern EngineInfo g_Engine;

/** @brief Global singleton for SourceHook */
extern SourceHook::CSourceHookImpl g_SourceHook;

/** @brief Mod path (important!)*/
extern SourceHook::String g_ModPath;

/** @brief Path to server binary */
extern SourceHook::String g_BinPath;

/** @brief Path to SourceMM binary */
extern SourceHook::String g_SmmPath;

/** @brief Global variable for SourceHook macros */
extern SourceHook::ISourceHook *g_SHPtr;

/** @brief We have our own internal plugin id... */
extern PluginId g_PLID;

/** @brief ServerGameDLL version that is currently loaded */
extern int g_GameDllVersion;

/** @brief Highest IServerPluginCallbacks version that is supported by engine */
extern int g_VspVersion;

/** @brief IServerGameClients version the mod uses */
extern int g_GameClientsVersion;

/** @brief Source Engine version */
extern int g_SourceEngineVersion;

extern bool bGameInit;

/** @brief Global CallClass for IServerGameDLL */
extern SourceHook::CallClass<IServerGameDLL> *g_GameDllPatch;

#endif //_INCLUDE_SOURCEMM_H
