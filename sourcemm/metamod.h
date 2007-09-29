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
#include "metamod_provider.h"
#include "svn_version.h"

using namespace SourceMM;

/**
 * Versioning
 *   increase api_major when API breaks
 *   increase api_minor when new functions are added (non-breaking)
 */
#define SOURCEMM_VERSION	SVN_FILE_VERSION_STRING
#define SOURCEMM_DATE		__DATE__
#define SM_VERS_API_MAJOR	1		//increase this on a breaking change
#define SM_VERS_API_MINOR	7		//increase this on a non-breaking API change

class MetamodSource : public ISmmAPI
{
public:
	void LogMsg(ISmmPlugin *pl, const char *msg, ...);
	CreateInterfaceFn engineFactory(bool syn=true);
	CreateInterfaceFn physicsFactory(bool syn=true);
	CreateInterfaceFn fileSystemFactory(bool syn=true);
	CreateInterfaceFn serverFactory(bool syn=true);
	CGlobalVars *pGlobals();
	void SetLastMetaReturn(META_RES res);
	META_RES GetLastMetaReturn();
	IConCommandBaseAccessor *GetCvarBaseAccessor();
	bool RegisterConCmdBase(ISmmPlugin *plugin, ConCommandBase *pCommand);
	void UnregisterConCmdBase(ISmmPlugin *plugin, ConCommandBase *pCommand);
	void ConPrint(const char *str);
	void ConPrintf(const char *fmt, ...);
	bool RemotePrintingAvailable();
	void GetApiVersions(int &major, int &minor, int &plvers, int &plmin);
	void GetShVersions(int &shvers, int &shimpl);
	void AddListener(ISmmPlugin *plugin, IMetamodListener *pListener);
	void *MetaFactory(const char *iface, int *ret, PluginId *id);
	int FormatIface(char iface[], unsigned int maxlength);
	void *InterfaceSearch(CreateInterfaceFn fn, const char *iface, int max, int *ret);
	const char *GetBaseDir();
	void PathFormat(char *buffer, size_t len, const char *fmt, ...);
	void ClientConPrintf(edict_t *client, const char *fmt, ...);
	void *VInterfaceMatch(CreateInterfaceFn fn, const char *iface, int min=-1);
	void EnableVSPListener();
	int GetGameDLLVersion();
	int GetUserMessageCount();
	int FindUserMessage(const char *name, int *size=NULL);
	const char *GetUserMessage(int index, int *size=NULL);
	int GetVSPVersion();
	int GetSourceEngineBuild();
public:
	const char *GetGameBinaryPath();
	const char *GetPluginsFile();
	void UnregisterConCommandBase(PluginId id, ConCommandBase *pCommand);
};

void LogMessage(const char *msg, ...);
int LoadPluginsFromFile(const char *_file);

extern SourceHook::CSourceHookImpl g_SourceHook;
extern MetamodSource g_Metamod;

#endif //_INCLUDE_SOURCEMM_H
