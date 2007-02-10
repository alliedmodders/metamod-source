/* ======== SourceMM ========
 * Copyright (C) 2004-2007 Metamod:Source Development Team
 * No warranties of any kind
 *
 * License: zlib/libpng
 *
 * Author(s): David "BAILOPAN" Anderson
 * ============================
 */

#include "convar_smm.h"
#include "CSmmAPI.h"
#include "sourcemm.h"
#include "concommands.h"
#include "CPlugin.h"
#include "vsp_listener.h"

/**
 * @brief Implementation of main API interface
 * @file CSmmAPI.cpp
 */

using namespace SourceMM;

CSmmAPI g_SmmAPI;

CSmmAPI::CSmmAPI()
{
	m_ConPrintf = NULL;
	m_Cache = false;
	m_VSP = false;
}

void CSmmAPI::LogMsg(ISmmPlugin *pl, const char *msg, ...)
{
	va_list ap;
	static char buffer[2048];

	buffer[0] = '\0';

	va_start(ap, msg);
	vsnprintf(buffer, sizeof(buffer)-1, msg, ap);
	va_end(ap);

	LogMessage("[%s] %s", pl->GetLogTag(), buffer);
}

CreateInterfaceFn CSmmAPI::engineFactory(bool syn)
{
	if (syn)
		return EngineFactory;
	return g_Engine.engineFactory;
}

CreateInterfaceFn CSmmAPI::physicsFactory(bool syn)
{
	if (syn)
		return PhysicsFactory;
	return g_Engine.physicsFactory;
}

CreateInterfaceFn CSmmAPI::fileSystemFactory(bool syn)
{
	if (syn)
		return FileSystemFactory;
	return g_Engine.fileSystemFactory;
}

CreateInterfaceFn CSmmAPI::serverFactory(bool syn)
{
	if (syn)
		return CreateInterface;
	return g_GameDll.factory;
}

CGlobalVars *CSmmAPI::pGlobals()
{
	return g_Engine.pGlobals;
}

void CSmmAPI::SetLastMetaReturn(META_RES res)
{
	m_Res = res;
}

META_RES CSmmAPI::GetLastMetaReturn()
{
	return m_Res;
}

IConCommandBaseAccessor *CSmmAPI::GetCvarBaseAccessor()
{
	return static_cast<IConCommandBaseAccessor *>(&g_SMConVarAccessor);
}

bool CSmmAPI::RegisterConCmdBase(ISmmPlugin *plugin, ConCommandBase *pCommand)
{
	if (pCommand->IsCommand())
	{
		g_PluginMngr.AddPluginCmd(plugin, pCommand);
	} else {
		g_PluginMngr.AddPluginCvar(plugin, pCommand);
	}

	return g_SMConVarAccessor.Register(pCommand);
}

void CSmmAPI::UnregisterConCmdBase(ISmmPlugin *plugin, ConCommandBase *pCommand)
{
	if (pCommand->IsCommand())
	{
		g_PluginMngr.RemovePluginCmd(plugin, pCommand);
	} else {
		g_PluginMngr.RemovePluginCvar(plugin, pCommand);
	}

	g_SMConVarAccessor.Unregister(pCommand);
}

void CSmmAPI::ConPrint(const char *fmt)
{
	(m_ConPrintf)("%s", fmt);
}

void CSmmAPI::ConPrintf(const char *fmt, ...)
{
	va_list ap;
	static char buf[4096];
	va_start(ap, fmt);
	vsnprintf(buf, sizeof(buf)-1, fmt, ap);
	ConPrint(buf);
	va_end(ap);
}

void CSmmAPI::AddListener(ISmmPlugin *plugin, IMetamodListener *pListener)
{
	CPluginManager::CPlugin *pl = g_PluginMngr.FindByAPI(plugin);

	pl->m_Events.push_back(pListener);
}

void *CSmmAPI::MetaFactory(const char *iface, int *_ret, PluginId *id)
{
	if (id)
	{
		*id = 0;
	}

	if (!iface)
	{
		return NULL;
	}

	//first check ours... we get first chance!
	if (strcmp(iface, MMIFACE_SOURCEHOOK) == 0)
	{
		if (_ret)
		{
			*_ret = IFACE_OK;
		}
		return static_cast<void *>(static_cast<SourceHook::ISourceHook *>(&g_SourceHook));
	} else if (strcmp(iface, MMIFACE_PLMANAGER) == 0) {
		if (_ret)
		{
			*_ret = IFACE_OK;
		}
		return static_cast<void *>(static_cast<ISmmPluginManager *>(&g_PluginMngr));
	}

	CPluginManager::CPlugin *pl;
	SourceHook::List<IMetamodListener *>::iterator event;
	IMetamodListener *api;
	int ret = 0;
	void *val = NULL;

	for (PluginIter iter = g_PluginMngr._begin(); iter != g_PluginMngr._end(); iter++)
	{
		pl = (*iter);
		for (event=pl->m_Events.begin(); event!=pl->m_Events.end(); event++)
		{
			api = (*event);
			ret = IFACE_FAILED;
			if ( (val=api->OnMetamodQuery(iface, &ret)) != NULL )
			{
				if (_ret)
					*_ret = ret;
				if (id)
					*id = pl->m_Id;
				return val;
			}
		}
	}

	if (_ret)
		*_ret = IFACE_FAILED;

	return NULL;
}

//////////////////////////////////////////////////////////////////////////
//THERE BE HAX HERE!!!! DON'T TELL ALFRED, BUT GABE WANTED IT THAT WAY. //
// (note: you can find the offset by looking for the text               //
//   "Echo text to console", you'll find the callback cmd pushed on the //
//   stack.)                                                            //
//////////////////////////////////////////////////////////////////////////

#define SIGLEN			8
#define ENGINE486_SIG	"\x55\x89\xE5\x53\x83\xEC\x14\xBB"
#define ENGINE486_OFFS	40
#define ENGINE686_SIG	"\x53\x83\xEC\x08\xBB\x01\x00\x00"
#define ENGINE686_OFFS	50
#define	ENGINEAMD_SIG	"\x53\x51\xBB\x01\x00\x00\x00\x51"
#define	ENGINEAMD_OFFS	47
#define ENGINEW32_SIG	"\xA1\x2A\x2A\x2A\x2A\x56\xBE\x01"
#define ENGINEW32_OFFS	38
#define IA32_CALL		0xE8

bool vcmp(const void *_addr1, const void *_addr2, size_t len)
{
	unsigned char *addr1 = (unsigned char *)_addr1;
	unsigned char *addr2 = (unsigned char *)_addr2;

	for (size_t i=0; i<len; i++)
	{
		if (addr2[i] == '*')
			continue;
		if (addr1[i] != addr2[i])
			return false;
	}

	return true;
}

//Thanks to fysh for the idea of extracting info from "echo" and for
// having the original offsets at hand!
bool CSmmAPI::CacheCmds()
{
	ICvar *pCvar = g_Engine.icvar;

	ConCommandBase *pBase = pCvar->GetCommands();
	unsigned char *ptr = NULL;
	FnCommandCallback callback = NULL;
	int offs = 0;

	while (pBase)
	{
		if ( strcmp(pBase->GetName(), "echo") == 0 )
		{
			//callback = //*((FnCommandCallback *)((char *)pBase + offsetof(ConCommand, m_fnCommandCallback)));
			callback = ((ConCommand *)pBase)->GetCallback();
			ptr = (unsigned char *)callback;
		#ifdef OS_LINUX
			if (vcmp(ptr, ENGINE486_SIG, SIGLEN))
			{
				offs = ENGINE486_OFFS;
			}
			else if (vcmp(ptr, ENGINE686_SIG, SIGLEN))
			{
				offs = ENGINE686_OFFS;
			}
			else if (vcmp(ptr, ENGINEAMD_SIG, SIGLEN))
			{
				offs = ENGINEAMD_OFFS;
			}
		#elif defined OS_WIN32 // Only one Windows engine binary so far...
			if (vcmp(ptr, ENGINEW32_SIG, SIGLEN))
			{
				offs = ENGINEW32_OFFS;
			}
		#endif

			if (!offs || ptr[offs-1] != IA32_CALL)
			{
				m_ConPrintf = (CONPRINTF_FUNC)Msg;
				return false;
			}
			//get the relative offset
			m_ConPrintf = *((CONPRINTF_FUNC *)(ptr + offs));
			//add the base offset, to the ip (which is the address+offset + 4 bytes for next instruction)
			m_ConPrintf = (CONPRINTF_FUNC)((unsigned long)m_ConPrintf + (unsigned long)(ptr + offs) + 4);

			m_Cache = true;

			return true;
		}
		pBase = const_cast<ConCommandBase *>(pBase->GetNext());
	}

	m_ConPrintf = (CONPRINTF_FUNC)Msg;

	return false;
}

bool CSmmAPI::CacheSuccessful()
{
	return m_Cache;
}

void CSmmAPI::GetApiVersions(int &major, int &minor, int &plvers, int &plmin)
{
	major = SM_VERS_API_MAJOR;
	minor = SM_VERS_API_MINOR;
	plvers = PLAPI_VERSION;
	plmin = PLAPI_MIN_VERSION;
}

void CSmmAPI::GetShVersions(int &shvers, int &shimpl)
{
	shvers = SH_IFACE_VERSION;
	shimpl = SH_IMPL_VERSION;
}

int CSmmAPI::FormatIface(char iface[], unsigned int maxlength)
{
	int length = (int)strlen(iface);
	int i;
	int num = 0;

	for (i=length-1; i>=0; i--)
	{
		if (!isdigit(iface[i]))
		{
			if (i != length-1)
			{
				num = 1;
			}
			break;
		}
	}

	if ( (num && ((int)maxlength <= length)) || (!num && ((int)maxlength <= length+3)) )
	{
		return -1;
	}

	if (i != length-1)
		num = atoi(&(iface[++i]));

	num++;

	snprintf(&(iface[i]), 4, "%03d", num);

	return num;
}

void *CSmmAPI::InterfaceSearch(CreateInterfaceFn fn, const char *iface, int max, int *ret)
{
	char _if[256];	/* assume no interface goes beyond this */
	size_t len = strlen(iface);
	int num = 0;
	void *pf = NULL;

	if (max > 999)
		max = 999;

	if (len + 4 > sizeof(_if))
	{
		if (ret)
		{
			*ret = IFACE_FAILED;
		}
		return NULL;
	}

	strcpy(_if, iface);

	do
	{
		if ( (pf = (fn)(_if, ret)) != NULL )
			break;
		if (num > max)
			break;
	} while (( num = FormatIface(_if, len+1) ));

	return pf;
}

void *CSmmAPI::VInterfaceMatch(CreateInterfaceFn fn, const char *iface, int min)
{
	char buffer[256];	/* assume no interface will go beyond this */
	int len = static_cast<int>(strlen(iface));
	int ret;			/* just in case something doesn't handle NULL properly */

	if (len > static_cast<int>(sizeof(buffer) - 4))
	{
		return NULL;
	}

	strcpy(buffer, iface);

	if (min != -1)
	{
		char *ptr = &buffer[len-1];
		int digits = 0;
		while (isdigit(*ptr) && digits <=3)
		{
			*ptr = '\0';
			digits++;
			ptr--;
		}
		if (digits != 3)
		{
			/* for now, assume this is an error */
			strcpy(buffer, iface);
		} else {
			char num[4];
			min = (min == 0) ? 1 : min;
			snprintf(num, sizeof(num), "%03d", min);
			strcat(buffer, num);
		}
	}

	return InterfaceSearch(fn, buffer, IFACE_MAXNUM, &ret);
}

const char *CSmmAPI::GetBaseDir()
{
	return g_ModPath.c_str();
}

void CSmmAPI::PathFormat(char *buffer, size_t len, const char *fmt, ...)
{
	va_list ap;
	va_start(ap,fmt);
	size_t mylen = vsnprintf(buffer, len, fmt, ap);
	va_end(ap);

	if (mylen == 0xFFFFFFFF || mylen >= len)
	{
		mylen = len - 1;
	}

	for (size_t i=0; i<mylen; i++)
	{
		if (buffer[i] == ALT_SEP_CHAR)
			buffer[i] = PATH_SEP_CHAR;
	}
}

void CSmmAPI::ClientConPrintf(edict_t *client, const char *fmt, ...)
{
	va_list ap;
	static char buf[4096];
	va_start(ap, fmt);
	vsnprintf(buf, sizeof(buf) - 1, fmt, ap);
	g_Engine.engine->ClientPrintf(client, buf);
	va_end(ap);
}

void CSmmAPI::LoadAsVSP()
{
	g_Engine.engine->ServerCommand("plugin_load \n");
	g_Engine.engine->ServerExecute();

	IServerPluginCallbacks *iface = NULL;
	if (g_VspListener.IsLoaded())
	{
		iface = &g_VspListener;
	}

	PluginIter iter;
	CPluginManager::CPlugin *pPlugin;
	SourceHook::List<IMetamodListener *>::iterator event;
	IMetamodListener *pML;
	for (iter=g_PluginMngr._begin(); iter!=g_PluginMngr._end(); iter++)
	{
		pPlugin = (*iter);
		if (pPlugin->m_Status < Pl_Paused)
		{
			continue;
		}
		/* Only valid for plugins >= 10 (v1:5, SourceMM 1.4) */
		if (pPlugin->m_API->GetApiVersion() < 10)
		{
			continue;
		}
		for (event=pPlugin->m_Events.begin();
			 event!=pPlugin->m_Events.end();
			 event++)
		{
			pML = (*event);
			pML->OnVSPListening(iface);
		}
	}
}

void CSmmAPI::EnableVSPListener()
{
	/* If GameInit already passed and we're not already enabled or loaded, go ahead and LoadAsVSP load */
	if (bGameInit && !m_VSP && !g_VspListener.IsLoaded())
	{
		LoadAsVSP();
	}
	m_VSP = true;
}

int CSmmAPI::GetGameDLLVersion()
{
	return g_GameDllVersion;
}
