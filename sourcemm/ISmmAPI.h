/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_ISMM_API_H
#define _INCLUDE_ISMM_API_H

/**
 * @brief External API interface
 * @file ISmmAPI.h
 */

#include <interface.h>
#include <eiface.h>
#include <convar.h>
#include <sourcehook/sourcehook.h>
#include "IPluginManager.h"

class ISmmPluginManager;
class ISmmPlugin;

class ISmmAPI
{
public:
	virtual ISmmPluginManager *PluginManager() =0;
	virtual SourceHook::ISourceHook *SourceHook() =0;
	virtual void LogMsg(ISmmPlugin *pl, const char *msg, ...) =0;
public:
	virtual CreateInterfaceFn engineFactory(bool syn=true) =0;
	virtual CreateInterfaceFn physicsFactory(bool syn=true) =0;
	virtual CreateInterfaceFn fileSystemFactory(bool syn=true) =0;
	virtual CreateInterfaceFn serverFactory(bool syn=true) =0;
	virtual CGlobalVars *pGlobals() =0;
	virtual void SetLastMetaReturn(META_RES res) =0;
	virtual META_RES GetLastMetaReturn() =0;
public:
	//Added in 1.00-RC2 to solve concommand problems
	virtual IConCommandBaseAccessor *GetCvarBaseAccessor() =0;
	virtual bool RegisterConCmdBase(ConCommandBase *pCommand) =0;
	virtual void UnregisterConCmdBase(ConCommandBase *pCommand) =0;
};

#endif //_INCLUDE_ISMM_API_H
