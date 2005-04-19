/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_CSMM_API_H
#define _INCLUDE_CSMM_API_H

/**
 * @brief Header for CSmmAPI implementation
 * @file CSmmAPI.h
 */

#include "ISmmAPI.h"

namespace SourceMM
{
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
};

extern SourceMM::CSmmAPI g_SmmAPI;

#endif //_INCLUDE_CSMM_API_H
