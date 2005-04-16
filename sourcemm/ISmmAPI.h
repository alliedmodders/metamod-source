#ifndef _INCLUDE_ISMM_API_H
#define _INCLUDE_ISMM_API_H

#include <interface.h>
#include <eiface.h>
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
};

#endif //_INCLUDE_ISMM_API_H
