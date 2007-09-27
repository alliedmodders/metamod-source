#include "convar_smm.h"
#include <eiface.h>
#include <tier0/icommandline.h>
#include <sourcehook/sourcehook.h>
#include "provider_util.h"
#include "provider_ep1.h"
#include "console.h"

/* Types */
typedef void (*CONPRINTF_FUNC)(const char *, ...);
/* Imports */
#undef CommandLine
DLL_IMPORT ICommandLine *CommandLine();
/* Functions */
CONPRINTF_FUNC ExtractRemotePrinter();
/* Variables */
CONPRINTF_FUNC echo_msg_func = NULL;
IVEngineServer *engine = NULL;
IServerGameDLL *server = NULL;
ICvar *icvar = NULL;

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

void BaseProvider::Notify_DLLInit_Pre()
{
	echo_msg_func = ExtractRemotePrinter();	
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
CONPRINTF_FUNC ExtractRemotePrinter()
{
	ConCommandBase *pBase = icvar->GetCommands();
	unsigned char *ptr = NULL;
	FnCommandCallback callback = NULL;
	int offs = 0;

	while (pBase)
	{
		if ( strcmp(pBase->GetName(), "echo") == 0 )
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


