#ifndef _INCLUDE_CSMM_API_H
#define _INCLUDE_CSMM_API_H

#include "ISmmAPI.h"

class CSmmAPI : public ISmmAPI
{
public:
	ISmmPluginManager *PluginManager();
	SourceHook::ISourceHook *SourceHook();
	void LogMsg(ISmmPlugin *pl, const char *msg, ...);
public:
	CreateInterfaceFn engineFactory(bool syn=true);
	CreateInterfaceFn physicsFactory(bool syn=true);
	CreateInterfaceFn fileSystemFactory(bool syn=true);
	CreateInterfaceFn serverFactory(bool syn=true);
	CGlobalVars *pGlobals();
	void SetLastMetaReturn(META_RES res);
	META_RES GetLastMetaReturn();
private:
	META_RES m_Res;
};

extern CSmmAPI g_SmmAPI;

#endif //_INCLUDE_CSMM_API_H
