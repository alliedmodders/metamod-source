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

typedef void (*CONPRINTF_FUNC)(const char *, ...);

namespace SourceMM
{
	class CSmmAPI : public ISmmAPI
	{
	public:
		CSmmAPI::CSmmAPI();
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
		IConCommandBaseAccessor *GetCvarBaseAccessor();
		bool RegisterConCmdBase(ISmmPlugin *plugin, ConCommandBase *pCommand);
		void UnregisterConCmdBase(ISmmPlugin *plugin, ConCommandBase *pCommand);
		void ConPrint(const char *fmt);
		void ConPrintf(const char *fmt, ...);
		bool CacheSuccessful();
		bool RemotePrintingAvailable()
		{
			return CacheSuccessful();
		}
		virtual void GetApiVersions(int &major, int &minor, int &plvers, int &plmin);
		virtual void GetShVersions(int &shvers, int &shimpl);
	public:
		bool CacheCmds();
	private:
		META_RES m_Res;
		CONPRINTF_FUNC m_ConPrintf;
		bool m_Cache;
	};
};

extern SourceMM::CSmmAPI g_SmmAPI;

#define	CONMSG	g_SmmAPI.ConPrintf

#endif //_INCLUDE_CSMM_API_H
