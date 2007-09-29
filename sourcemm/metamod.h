/**
 * vim: set ts=4 :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2007 AlliedModders LLC and authors.
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
	IServerPluginCallbacks *GetVSPInfo(int *pVersion);
public:
	const char *GetGameBinaryPath();
	const char *GetPluginsFile();
	void UnregisterConCommandBase(PluginId id, ConCommandBase *pCommand);
	void NotifyVSPListening(IServerPluginCallbacks *callbacks);
};

void LogMessage(const char *msg, ...);
int LoadPluginsFromFile(const char *_file);

extern SourceHook::CSourceHookImpl g_SourceHook;
extern MetamodSource g_Metamod;

#endif //_INCLUDE_SOURCEMM_H
