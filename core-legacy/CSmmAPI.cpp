/* ======== SourceMM ========
 * Copyright (C) 2004-2008 Metamod:Source Development Team
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
	m_CmdCache = false;
	m_MsgCount = -1;
	m_VSP = false;
}

CSmmAPI::~CSmmAPI()
{
	m_UserMessages.RemoveAll();
}

void CSmmAPI::LogMsg(ISmmPlugin *pl, const char *msg, ...)
{
	va_list ap;
	static char buffer[2048];

	va_start(ap, msg);
	UTIL_FormatArgs(buffer, sizeof(buffer), msg, ap);
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
		return ServerFactory;
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

	CPluginManager::CPlugin *pPlugin = g_PluginMngr.FindByAPI(plugin);
	PluginId id = (pPlugin != NULL) ? pPlugin->m_Id : Pl_BadLoad;

	NotifyConCommandBaseDrop(id, pCommand);

	g_SMConVarAccessor.Unregister(pCommand);
}

void CSmmAPI::ConPrint(const char *fmt)
{
	(m_ConPrintf)("%s", fmt);
}

void CSmmAPI::ConPrintf(const char *fmt, ...)
{
	va_list ap;
	static char buffer[4096];

	va_start(ap, fmt);
	UTIL_FormatArgs(buffer, sizeof(buffer), fmt, ap);
	va_end(ap);

	(m_ConPrintf)("%s", buffer);
}

void CSmmAPI::AddListener(ISmmPlugin *plugin, IMetamodListener *pListener)
{
	CPluginManager::CPlugin *pl = g_PluginMngr.FindByAPI(plugin);
	CPluginEventHandler cpeh;

	cpeh.event = pListener;
	cpeh.got_vsp = false;

	pl->m_Events.push_back(cpeh);
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
	SourceHook::List<CPluginEventHandler>::iterator event;
	IMetamodListener *api;
	int ret = 0;
	void *val = NULL;

	for (PluginIter iter = g_PluginMngr._begin(); iter != g_PluginMngr._end(); iter++)
	{
		pl = (*iter);
		for (event=pl->m_Events.begin(); event!=pl->m_Events.end(); event++)
		{
			api = (*event).event;
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
			if (UTIL_VerifySignature(ptr, ENGINE486_SIG, SIGLEN))
			{
				offs = ENGINE486_OFFS;
			}
			else if (UTIL_VerifySignature(ptr, ENGINE686_SIG, SIGLEN))
			{
				offs = ENGINE686_OFFS;
			}
			else if (UTIL_VerifySignature(ptr, ENGINEAMD_SIG, SIGLEN))
			{
				offs = ENGINEAMD_OFFS;
			}
		#elif defined OS_WIN32 // Only one Windows engine binary so far...
			if (UTIL_VerifySignature(ptr, ENGINEW32_SIG, SIGLEN))
			{
				offs = ENGINEW32_OFFS;
			}
		#endif

			if (!offs || ptr[offs - 1] != IA32_CALL)
			{
				m_ConPrintf = (CONPRINTF_FUNC)Msg;
				return false;
			}
			//get the relative offset
			m_ConPrintf = *((CONPRINTF_FUNC *)(ptr + offs));
			//add the base offset, to the ip (which is the address+offset + 4 bytes for next instruction)
			m_ConPrintf = (CONPRINTF_FUNC)((unsigned long)m_ConPrintf + (unsigned long)(ptr + offs) + 4);

			m_CmdCache = true;

			return true;
		}
		pBase = const_cast<ConCommandBase *>(pBase->GetNext());
	}

	m_ConPrintf = (CONPRINTF_FUNC)Msg;

	return false;
}

bool CSmmAPI::CmdCacheSuccessful()
{
	return m_CmdCache;
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

	for (i = length - 1; i >= 0; i--)
	{
		if (!isdigit(iface[i]))
		{
			if (i != length - 1)
			{
				num = 1;
			}
			break;
		}
	}

	if ( (num && ((int)maxlength <= length)) || (!num && ((int)maxlength <= length + 3)) )
	{
		return -1;
	}

	if (i != length - 1)
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
	size_t len = strlen(iface);
	int ret;			/* just in case something doesn't handle NULL properly */

	if (len > sizeof(buffer) - 4)
	{
		return NULL;
	}

	strcpy(buffer, iface);

	if (min != -1)
	{
		char *ptr = &buffer[len - 1];
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
	va_start(ap, fmt);
	size_t mylen = UTIL_FormatArgs(buffer, len, fmt, ap);
	va_end(ap);

	for (size_t i = 0; i < mylen; i++)
	{
		if (buffer[i] == ALT_SEP_CHAR)
		{
			buffer[i] = PATH_SEP_CHAR;
		}
	}
}

void CSmmAPI::ClientConPrintf(edict_t *client, const char *fmt, ...)
{
	va_list ap;
	static char buffer[4096];

	va_start(ap, fmt);
	UTIL_FormatArgs(buffer, sizeof(buffer), fmt, ap);
	va_end(ap);

	g_Engine.engine->ClientPrintf(client, buffer);
}

void CSmmAPI::LoadAsVSP()
{
	size_t len;
	char engine_file[PATH_SIZE];
	char rel_path[PATH_SIZE * 2];

	GetFileOfAddress(g_Engine.engine, engine_file, sizeof(engine_file));

	/* Chop off the "engine" file part */
	len = strlen(engine_file);
	for (size_t i = len - 1; i >= 0 && i < len; i--)
	{
		if (engine_file[i] == '/'
			|| engine_file[i] == '\\')
		{
			engine_file[i] = '\0';
			break;
		}
	}

	char our_path[PATH_SIZE];
	GetFileOfAddress((void*)LoadAsGameDLL, our_path, sizeof(our_path));

	const char *usepath = our_path;
	if (UTIL_Relatize(rel_path, sizeof(rel_path), engine_file, our_path))
		usepath = rel_path;
	
	char command[PATH_SIZE * 2];
	g_VspListener.SetLoadable(true);
	UTIL_Format(command, sizeof(command), "plugin_load \"%s\"\n", usepath);
	g_Engine.engine->ServerCommand(command);
}

void CSmmAPI::EnableVSPListener()
{
	/* If GameInit already passed and we're not already enabled or loaded, go ahead and LoadAsVSP load */
	if (g_bGameInit && !m_VSP && !g_VspListener.IsLoaded() && !g_bIsBridgedAsVsp)
	{
		LoadAsVSP();
	}
	m_VSP = true;
}

int CSmmAPI::GetGameDLLVersion()
{
	return g_GameDllVersion;
}

//////////////////////////////////////////////////////////////////////
// EVEN MORE HACKS HERE! YOU HAVE BEEN WARNED!                      //
// Signatures necessary in finding the pointer to the CUtlDict that //
//   stores user message information.                               //
// IServerGameDLL::GetUserMessageInfo() normally crashes with bad   //
//   message indices. This is our answer to it. Yuck! <:-(          //
//////////////////////////////////////////////////////////////////////
#ifdef OS_WIN32
	/* General Windows sig */
	#define MSGCLASS_SIGLEN		7
	#define MSGCLASS_SIG		"\x8B\x0D\x2A\x2A\x2A\x2A\x56"
	#define MSGCLASS_OFFS		2

	/* Dystopia Wimdows hack */
	#define MSGCLASS2_SIGLEN	16
	#define MSGCLASS2_SIG		"\x56\x8B\x74\x24\x2A\x85\xF6\x7C\x2A\x3B\x35\x2A\x2A\x2A\x2A\x7D"
	#define MSGCLASS2_OFFS		11

	/* Windows frame pointer sig */
	#define MSGCLASS3_SIGLEN	18
	#define MSGCLASS3_SIG		"\x55\x8B\xEC\x51\x89\x2A\x2A\x8B\x2A\x2A\x50\x8B\x0D\x2A\x2A\x2A\x2A\xE8"
	#define MSGCLASS3_OFFS		13
#elif defined OS_LINUX
	/* No frame pointer sig */
	#define MSGCLASS_SIGLEN		14
	#define MSGCLASS_SIG		"\x53\x83\xEC\x2A\x8B\x2A\x2A\x2A\xA1\x2A\x2A\x2A\x2A\x89"
	#define MSGCLASS_OFFS		9

	/* Frame pointer sig */
	#define MSGCLASS2_SIGLEN	16
	#define MSGCLASS2_SIG		"\x55\x89\xE5\x53\x83\xEC\x2A\x8B\x2A\x2A\xA1\x2A\x2A\x2A\x2A\x89"
	#define MSGCLASS2_OFFS		11
#endif

/* This is the ugliest function in all of SourceMM */
/* :TODO: Make this prettier */
bool CSmmAPI::CacheUserMessages()
{
	UserMsgDict *dict = NULL;
	char *vfunc = UTIL_GetOrigFunction(&IServerGameDLL::GetUserMessageInfo, g_GameDll.pGameDLL);

	if (!vfunc)
	{
		return false;
	}

	if (UTIL_VerifySignature(vfunc, MSGCLASS_SIG, MSGCLASS_SIGLEN))
	{
		/* Get address of CUserMessages instance */
		char **userMsgClass = *reinterpret_cast<char ***>(vfunc + MSGCLASS_OFFS);

		/* Get address of CUserMessages::m_UserMessages */
		dict = reinterpret_cast<UserMsgDict *>(*userMsgClass);
	} else if (UTIL_VerifySignature(vfunc, MSGCLASS2_SIG, MSGCLASS2_SIGLEN)) {
	#ifdef OS_WIN32
		/* If we get here, the code is possibly inlined like in Dystopia */

		/* Get the address of the CUtlRBTree */
		char *rbtree = *reinterpret_cast<char **>(vfunc + MSGCLASS2_OFFS);

		/* CUtlDict should be 8 bytes before the CUtlRBTree (hacktacular!) */
		dict = reinterpret_cast<UserMsgDict *>(rbtree - 8);
	#elif defined OS_LINUX
		/* Get address of CUserMessages instance */
		char **userMsgClass = *reinterpret_cast<char ***>(vfunc + MSGCLASS2_OFFS);

		/* Get address of CUserMessages::m_UserMessages */
		dict = reinterpret_cast<UserMsgDict *>(*userMsgClass);
	#endif
	#ifdef OS_WIN32
	} else if (UTIL_VerifySignature(vfunc, MSGCLASS3_SIG, MSGCLASS3_SIGLEN)) {
		/* Get address of CUserMessages instance */
		char **userMsgClass = *reinterpret_cast<char ***>(vfunc + MSGCLASS3_OFFS);

		/* Get address of CUserMessages::m_UserMessages */
		dict = reinterpret_cast<UserMsgDict *>(*userMsgClass);
	#endif
	}

	if (dict)
	{
		m_MsgCount = dict->Count();

		/* Ensure that count is within bounds of an unsigned byte, because that's what engine supports */
		if (m_MsgCount < 0 || m_MsgCount > 255)
		{
			m_MsgCount = -1;
			return false;
		}

		UserMessage *msg;

		/* Cache messages in our CUtlDict */
		for (int i = 0; i < m_MsgCount; i++)
		{
			msg = dict->Element(i);
			m_UserMessages.Insert(msg->name, msg);
		}

		return true;
	}

	return false;
}

bool CSmmAPI::MsgCacheSuccessful()
{
	return m_MsgCount > -1;
}

int CSmmAPI::GetUserMessageCount()
{
	return m_MsgCount;
}

int CSmmAPI::FindUserMessage(const char *name, int *size)
{
	int index = m_UserMessages.Find(name);

	if (size && index > -1)
	{
		UserMessage *msg = m_UserMessages.Element(index);
		*size = msg->size;
	}

	return index;
}

const char *CSmmAPI::GetUserMessage(int index, int *size)
{
	if (m_MsgCount <= 0 || index < 0 || index >= m_MsgCount)
	{
		return NULL;
	}

	UserMessage *msg = m_UserMessages.Element(index);

	if (size)
	{
		*size = msg->size;
	}

	return msg->name;
}
