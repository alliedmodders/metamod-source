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

#ifndef _INCLUDE_SOURCEMM_H
#define _INCLUDE_SOURCEMM_H

/**
 * @brief SourceMM main functionality for GameDLL interception
 * @file sourcemm.h
 */

#include <interface.h>
#include <eiface.h>
#include <sourcehook/sourcehook_impl.h>
#include <sourcehook/sourcehook_hookmangen.h>
#include <sourcehook/sourcehook.h>
#include <ISmmPlugin.h>
#include "metamod_provider.h"

using namespace SourceMM;

/**
 * Versioning
 *   increase api_major when API breaks
 *   increase api_minor when new functions are added (non-breaking)
 */
#define SOURCEMM_VERSION	SVN_FILE_VERSION_STRING
#define SOURCEMM_DATE		__DATE__
#define METAMOD_API_MAJOR	2		/* increase this on a breaking change */
#define METAMOD_API_MINOR	0		/* increase this on a non-breaking API change */

class MetamodSource : public ISmmAPI
{
public:
	void LogMsg(ISmmPlugin *pl, const char *msg, ...);
	CreateInterfaceFn GetEngineFactory(bool syn=true);
	CreateInterfaceFn GetPhysicsFactory(bool syn=true);
	CreateInterfaceFn GetFileSystemFactory(bool syn=true);
	CreateInterfaceFn GetServerFactory(bool syn=true);
	CGlobalVars *GetCGlobals();
	void SetLastMetaReturn(META_RES res);
	META_RES GetLastMetaReturn();
	IConCommandBaseAccessor *GetCvarBaseAccessor();
	bool RegisterConCommandBase(ISmmPlugin *plugin, ConCommandBase *pCommand);
	void UnregisterConCommandBase(ISmmPlugin *plugin, ConCommandBase *pCommand);
	void ConPrint(const char *str);
	void ConPrintf(const char *fmt, ...);
	bool RemotePrintingAvailable();
	void GetApiVersions(int &major, int &minor, int &plvers, int &plmin);
	void GetShVersions(int &shvers, int &shimpl);
	void AddListener(ISmmPlugin *plugin, IMetamodListener *pListener);
	void *MetaFactory(const char *iface, int *ret, PluginId *id);
	int FormatIface(char iface[], size_t maxlength);
	void *InterfaceSearch(CreateInterfaceFn fn, const char *iface, int max, int *ret);
	const char *GetBaseDir();
	size_t PathFormat(char *buffer, size_t len, const char *fmt, ...);
#if SOURCE_ENGINE == SE_DOTA
	// Shim
	void ClientConPrintf(int clientIndex, const char *fmt, ...);
#endif
	void ClientConPrintf(edict_t *client, const char *fmt, ...);
	void *VInterfaceMatch(CreateInterfaceFn fn, const char *iface, int min=-1);
	void EnableVSPListener();
	int GetGameDLLVersion();
	int GetUserMessageCount();
	int FindUserMessage(const char *name, int *size=NULL);
	const char *GetUserMessage(int index, int *size=NULL);
	int GetVSPVersion();
	int GetSourceEngineBuild();
	IServerPluginCallbacks *GetVSPInfo(int *pVersion);
	size_t Format(char *buffer, size_t maxlength, const char *format, ...);
	size_t FormatArgs(char *buffer, size_t maxlength, const char *format, va_list ap);
public:
	bool IsLoadedAsGameDLL();
	const char *GetGameBinaryPath();
	const char *GetPluginsFile();
	const char *GetVDFDir();
	void UnregisterConCommandBase(PluginId id, ConCommandBase *pCommand);
	void NotifyVSPListening(IServerPluginCallbacks *callbacks, int version);
	void SetGameDLLInfo(CreateInterfaceFn serverFactory, int version, bool loaded);
	void SetVSPListener(const char *path);
	size_t GetFullPluginPath(const char *plugin, char *buffer, size_t len);
};

bool
mm_DetectGameInformation();

void
mm_LogMessage(const char *msg, ...);

int
mm_LoadPlugins(const char *filepath, const char *vdfpath);

void
mm_InitializeForLoad();

void
mm_InitializeGlobals(CreateInterfaceFn engineFactory, 
					 CreateInterfaceFn physicsFactory,
					 CreateInterfaceFn filesystemFactory,
					 CGlobalVars *pGlobals);
void
mm_StartupMetamod(bool is_vsp_load);

void
mm_UnloadMetamod();

bool
mm_IsVspBridged();

bool
mm_IsVspLoadComplete();

extern MetamodSource g_Metamod;
extern SourceHook::Impl::CSourceHookImpl g_SourceHook;

#endif //_INCLUDE_SOURCEMM_H

