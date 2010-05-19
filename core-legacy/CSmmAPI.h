/* ======== SourceMM ========
 * Copyright (C) 2004-2010 Metamod:Source Development Team
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
		CSmmAPI();
	public:
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
		bool RemotePrintingAvailable()
		{
			return CmdCacheSuccessful();
		}
		virtual void GetApiVersions(int &major, int &minor, int &plvers, int &plmin);
		virtual void GetShVersions(int &shvers, int &shimpl);
		virtual void AddListener(ISmmPlugin *plugin, IMetamodListener *pListener);
		virtual void *MetaFactory(const char *iface, int *ret, PluginId *id);
		virtual int FormatIface(char buffer[], unsigned int maxlength);
		virtual void *InterfaceSearch(CreateInterfaceFn fn, const char *iface, int max, int *ret);
		virtual const char *GetBaseDir();
		virtual void PathFormat(char *buffer, size_t len, const char *fmt, ...);
		virtual IServerPluginCallbacks *GetVSPInfo(int *pVersion);
		void ClientConPrintf(edict_t *client, const char *fmt, ...);
		void *VInterfaceMatch(CreateInterfaceFn fn, const char *iface, int min=-1);
		void EnableVSPListener();
		int GetGameDLLVersion();
		int GetUserMessageCount();
		int FindUserMessage(const char *name, int *size=NULL);
		const char *GetUserMessage(int index, int *size=NULL);
		int GetSourceEngineBuild();
	public:
		bool CacheCmds();
		bool CmdCacheSuccessful();
		void LoadAsVSP();
		bool VSPEnabled()
		{
			return m_VSP;
		}
		void CacheUserMessages();
		void GetFullPluginPath(const char *plugin, char *buffer, size_t len);
	private:
		META_RES m_Res;
		CONPRINTF_FUNC m_ConPrintf;
		bool m_CmdCache;
		bool m_VSP;
	};
};

extern SourceMM::CSmmAPI g_SmmAPI;

#define	CONMSG			g_SmmAPI.ConPrintf
#define CLIENT_CONMSG	g_SmmAPI.ClientConPrintf

#endif //_INCLUDE_CSMM_API_H
