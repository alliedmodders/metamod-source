/* ======== SourceMM ========
* Copyright (C) 2004-2006 Metamod:Source Development Team
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

/**
 * Versioning
 *   ?what the hell was I smoking when I wrote this
 *   increase api_major when API breaks
 *   increase api_minor when new functions are added (non-breaking)
 *   increase vers_release when bug fix releases are made
 *   never increase major
 */
#define		SOURCEMM_VERSION	"1.3a"
#define		SOURCEMM_DATE		__DATE__
#define		SM_MAJOR_VERSION	1		//never need to increase this
#define		SM_VERS_API_MAJOR	1		//increase this on a breaking change
#define		SM_VERS_API_MINOR	4		//increase this on a non-breaking API change
#define		SM_VERS_RELEASE		4		//increase this on a bug-fix release.

//We need a good CServerGameDLL version to work properly.  We support these inclusively.
#define	MIN_GAMEDLL_VERSION	3
#define	MAX_GAMEDLL_VERSION	5

/**
 * @brief Entry point for HL2 Engine
 */
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

/** @brief Global variable for SourceHook macros */
extern SourceHook::ISourceHook *g_SHPtr;

/** @brief We have our own internal plugin id... */
extern PluginId g_PLID;

/** @brief ServerGameDLL version that is currently loaded */
extern int g_GameDllVersion;

#endif //_INCLUDE_SOURCEMM_H
