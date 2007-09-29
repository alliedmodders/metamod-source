#include <sourcehook.h>
#include "convar_smm.h"
#include <eiface.h>
#include <tier0/icommandline.h>
#include <tier1/utldict.h>
#include <sh_vector.h>
#include <sh_string.h>
#include "../metamod_util.h"
#include "provider_ep1.h"
#include "console.h"
#include "metamod_console.h"
#include "vsp_listener.h"

/* Types */
typedef void (*CONPRINTF_FUNC)(const char *, ...);
struct UsrMsgInfo
{
	int size;
	String name;
};
/* Imports */
#undef CommandLine
DLL_IMPORT ICommandLine *CommandLine();
/* Functions */
CONPRINTF_FUNC ExtractRemotePrinter();
bool CacheUserMessages();
void ClientCommand(edict_t *pEdict);
void _ServerCommand();
/* Variables */
bool usermsgs_extracted = false;
CVector<UsrMsgInfo> usermsgs_list;
CONPRINTF_FUNC echo_msg_func = NULL;
ICvar *icvar = NULL;
ISmmAPI *metamod_api = NULL;
IVEngineServer *engine = NULL;
IServerGameClients *gameclients = NULL;
VSPListener g_VspListener;
BaseProvider g_Ep1Provider;
IMetamodSourceProvider *provider = &g_Ep1Provider;

SH_DECL_HOOK1_void(IServerGameClients, ClientCommand, SH_NOATTRIB, 0, edict_t *);

void BaseProvider::ConsolePrint(const char *str)
{
	if (echo_msg_func != NULL)
	{
		echo_msg_func("%s", str);
	}
	else
	{
		Msg("%s", str);
	}
}

void BaseProvider::Notify_DLLInit_Pre(void *gamedll,
									  CreateInterfaceFn engineFactory, 
									  CreateInterfaceFn serverFactory)
{
	server = (IServerGameDLL *)gamedll;
	engine = (IVEngineServer *)((engineFactory)(INTERFACEVERSION_VENGINESERVER, NULL));
	if (!engine)
	{
		Error("Could not find IVEngineServer! Metamod cannot load.");
		return;
	}
	icvar = (ICvar *)((engineFactory)(VENGINE_CVAR_INTERFACE_VERSION, NULL));
	if (!icvar)
	{
		Error("Could not find ICvar! Metamod cannot load.");
		return;
	}

	if ((gameclients = (IServerGameClients *)(serverFactory("ServerGameClients003", NULL)))
		== NULL)
	{
		gameclients = (IServerGameClients *)(serverFactory("ServerGameClients004", NULL));
	}

	echo_msg_func = ExtractRemotePrinter();	
	usermsgs_extracted = CacheUserMessages();

	if (gameclients)
	{
		SH_ADD_HOOK_STATICFUNC(IServerGameClients, ClientCommand, gameclients, ClientCommand, false);
	}

	ConCommandBaseMgr::OneTimeInit(&g_SMConVarAccessor);
}

void BaseProvider::Notify_DLLShutdown_Pre()
{
	g_SMConVarAccessor.MarkCommandsAsGameDLL();
	g_SMConVarAccessor.UnregisterGameDLLCommands();
}

bool BaseProvider::IsRemotePrintingAvailable()
{
	return (echo_msg_func != NULL);
}

void BaseProvider::ClientConsolePrint(edict_t *client, const char *message)
{
	engine->ClientPrintf(client, message);
}

void BaseProvider::ServerCommand(const char *cmd)
{
	engine->ServerCommand(cmd);
}

const char *BaseProvider::GetConVarString(ConVar *convar)
{
	return convar->GetString();
}

bool BaseProvider::IsConCommandBaseACommand(ConCommandBase *pCommand)
{
	return pCommand->IsCommand();
}


bool BaseProvider::IsSourceEngineBuildCompatible(int build)
{
	return (build == SOURCE_ENGINE_ORIGINAL
			|| build == SOURCE_ENGINE_EPISODEONE);
}

const char *BaseProvider::GetCommandLineValue(const char *key, const char *defval)
{
	if (key[0] == '-' || key[0] == '+')
	{
		return CommandLine()->ParmValue(key, defval);
	}
	else
	{
		const char *val;
		if ((val = icvar->GetCommandLineValue(key)) == NULL)
		{
			return defval;
		}

		return val;
	}
}

int BaseProvider::TryServerGameDLL(const char *iface)
{
	if (strncmp(iface, "ServerGameDLL", 13) != 0)
	{
		return 0;
	}

	return atoi(&iface[13]);
}

bool BaseProvider::LogMessage(const char *buffer)
{
	if (!engine)
	{
		return false;
	}

	engine->LogPrint(buffer);

	return true;
}

bool BaseProvider::GetHookInfo(ProvidedHooks hook, SourceHook::MemFuncInfo *pInfo)
{
	SourceHook::MemFuncInfo mfi = {true, -1, 0, 0};

	if (hook == ProvidedHook_LevelInit)
	{
		SourceHook::GetFuncInfo(&IServerGameDLL::LevelInit, mfi);
	}
	else if (hook == ProvidedHook_LevelShutdown)
	{
		SourceHook::GetFuncInfo(&IServerGameDLL::LevelShutdown, mfi);
	}
	else if (hook == ProvidedHook_GameInit)
	{
		SourceHook::GetFuncInfo(&IServerGameDLL::GameInit, mfi);
	}
	else if (hook == ProvidedHook_DLLShutdown)
	{
		SourceHook::GetFuncInfo(&IServerGameDLL::DLLShutdown, mfi);
	}
	else if (hook == ProvidedHook_DLLInit)
	{
		SourceHook::GetFuncInfo(&IServerGameDLL::DLLInit, mfi);
	}

	*pInfo = mfi;

	return (mfi.thisptroffs >= 0);
}

void BaseProvider::DisplayError(const char *fmt, ...)
{
	va_list ap;
	char buffer[2048];

	va_start(ap, fmt);
	UTIL_FormatArgs(buffer, sizeof(buffer), fmt, ap);
	va_end(ap);

	Error(buffer);
}

void BaseProvider::DisplayWarning(const char *fmt, ...)
{
	va_list ap;
	char buffer[2048];

	va_start(ap, fmt);
	UTIL_FormatArgs(buffer, sizeof(buffer), fmt, ap);
	va_end(ap);

	Warning(buffer);
}

IConCommandBaseAccessor *BaseProvider::GetConCommandBaseAccessor()
{
	return &g_SMConVarAccessor;
}

bool BaseProvider::RegisterConCommandBase(ConCommandBase *pCommand)
{
	return g_SMConVarAccessor.Register(pCommand);
}

void BaseProvider::UnregisterConCommandBase(ConCommandBase *pCommand)
{
	return g_SMConVarAccessor.Unregister(pCommand);
}

int BaseProvider::GetUserMessageCount()
{
	if (!usermsgs_extracted)
	{
		return -1;
	}

	return (int)usermsgs_list.size();
}

int BaseProvider::FindUserMessage(const char *name, int *size)
{
	for (size_t i = 0; i < usermsgs_list.size(); i++)
	{
		if (usermsgs_list[i].name.compare(name) == 0)
		{
			if (size)
			{
				*size = usermsgs_list[i].size;
			}
			return (int)i;
		}
	}
	
	return -1;
}

const char *BaseProvider::GetUserMessage(int index, int *size)
{
	if (!usermsgs_extracted || index < 0 || index >= (int)usermsgs_list.size())
	{
		return NULL;
	}

	if (size)
	{
		*size = usermsgs_list[index].size;
	}

	return usermsgs_list[index].name.c_str();
}

const char *BaseProvider::GetGameDescription()
{
	return server->GetGameDescription();
}

ConVar *BaseProvider::CreateConVar(const char *name,
								   const char *defval,
								   const char *help,
								   int flags)
{
	int newflags = 0;
	if (flags & ConVarFlag_Notify)
	{
		newflags |= FCVAR_NOTIFY;
	}
	if (flags & ConVarFlag_Replicated)
	{
		newflags |= FCVAR_REPLICATED;
	}
	if (flags & ConVarFlag_SpOnly)
	{
		newflags |= FCVAR_SPONLY;
	}

	return new ConVar(name, defval, newflags, help);
}

IServerPluginCallbacks *BaseProvider::GetVSPCallbacks(const char *iface)
{
	g_VspListener.SetLoadable(true);
	return &g_VspListener;
}

class GlobCommand : public IMetamodSourceCommandInfo
{
public:
	unsigned int GetArgCount()
	{
		return engine->Cmd_Argc() - 1;
	}

	const char *GetArg(unsigned int num)
	{
		return engine->Cmd_Argv(num);
	}

	const char *GetArgString()
	{
		return engine->Cmd_Args();
	}
};

CON_COMMAND(meta, "Metamod:Source control commands")
{
	GlobCommand cmd;
	Command_Meta(&cmd);
}

void ClientCommand(edict_t *pEdict)
{
	GlobCommand cmd;

	if (strcmp(cmd.GetArg(0), "meta") == 0)
	{
		Command_ClientMeta(pEdict, &cmd);
	}
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
		{
			continue;
		}
		if (addr1[i] != addr2[i])
		{
			return false;
		}
	}

	return true;
}

//Thanks to fysh for the idea of extracting info from "echo" and for
// having the original offsets at hand!
CONPRINTF_FUNC ExtractRemotePrinter()
{
	ConCommandBase *pBase = icvar->GetCommands();
	unsigned char *ptr = NULL;
	FnCommandCallback callback = NULL;
	int offs = 0;

	while (pBase)
	{
		if (strcmp(pBase->GetName(), "echo") == 0)
		{
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

			if (!offs || ptr[offs - 1] != IA32_CALL)
			{
				return NULL;
			}
			//get the relative offset
			void *addr = *((void **)(ptr + offs));
			//add the base offset, to the ip (which is the address+offset + 4 bytes for next instruction)
			return (CONPRINTF_FUNC)((unsigned long)addr + (unsigned long)(ptr + offs) + 4);
		}
		pBase = const_cast<ConCommandBase *>(pBase->GetNext());
	}

	return NULL;
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

struct UserMessage
{
	int size;
	const char *name;
};

typedef CUtlDict<UserMessage *, int> UserMsgDict;

/* This is the ugliest function in all of SourceMM */
bool CacheUserMessages()
{
	/* Get address of original GetUserMessageInfo() */
	char *vfunc = (char *)SH_GET_ORIG_VFNPTR_ENTRY(server, &IServerGameDLL::GetUserMessageInfo);

	/* Oh dear, we have a relative jump on our hands
	 * PVK II on Windows made me do this, but I suppose it doesn't hurt to check this on Linux too...
	 */
	if (*vfunc == '\xE9')
	{
		/* Get address from displacement...
		 *
		 * Add 5 because it's relative to next instruction:
		 * Opcode <1 byte> + 32-bit displacement <4 bytes> 
		 */
		vfunc += *reinterpret_cast<int *>(vfunc + 1) + 5;
	}

	CUtlDict<UserMessage *, int> *dict = NULL;

	if (vcmp(vfunc, MSGCLASS_SIG, MSGCLASS_SIGLEN))
	{
		/* Get address of CUserMessages instance */
		char **userMsgClass = *reinterpret_cast<char ***>(vfunc + MSGCLASS_OFFS);

		/* Get address of CUserMessages::m_UserMessages */
		dict = reinterpret_cast<UserMsgDict *>(*userMsgClass);
	} 
	else if (vcmp(vfunc, MSGCLASS2_SIG, MSGCLASS2_SIGLEN)) 
	{
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
	} 
	else if (vcmp(vfunc, MSGCLASS3_SIG, MSGCLASS3_SIGLEN)) 
	{
		/* Get address of CUserMessages instance */
		char **userMsgClass = *reinterpret_cast<char ***>(vfunc + MSGCLASS3_OFFS);

		/* Get address of CUserMessages::m_UserMessages */
		dict = reinterpret_cast<UserMsgDict *>(*userMsgClass);
	#endif
	}

	if (dict)
	{
		int msg_count = dict->Count();

		/* Ensure that count is within bounds of an unsigned byte, because that's what engine supports */
		if (msg_count < 0 || msg_count > 255)
		{
			return false;
		}

		UserMessage *msg;
		UsrMsgInfo u_msg;

		/* Cache messages in our CUtlDict */
		for (int i = 0; i < msg_count; i++)
		{
			msg = dict->Element(i);
			u_msg.name = msg->name;
			u_msg.size = msg->size;
			usermsgs_list.push_back(u_msg);
		}

		return true;
	}

	return false;
}

