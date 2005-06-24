/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#include "CSmmAPI.h"
#include "sourcemm.h"
#include "concommands.h"

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
}

ISmmPluginManager *CSmmAPI::PluginManager()
{
	return static_cast<ISmmPluginManager *>(&g_PluginMngr);
}

SourceHook::ISourceHook *CSmmAPI::SourceHook()
{
	return static_cast<SourceHook::ISourceHook *>(&g_SourceHook);
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

//////////////////////////////////////////////////////////////////////////
//THERE BE HAX HERE!!!! DON'T TELL ALFRED, BUT GABE WANTED IT THAT WAY. //
// (note: you can find the windows offset by looking for the text       //
//   "Echo text to console", you'll find the callback cmd pushed on the //
//   stack.)                                                            //
//////////////////////////////////////////////////////////////////////////

#define SIGLEN			8
#define ENGINE486_SIG	"\x55\x89\xE5\x53\x83\xEC\x14\xBB"
#define ENGINE486_OFFS	40
#define ENGINE686_SIG	"\x53\x83\xEC\x08\xBB\x01\x00\x00"
#define ENGINE686_OFFS	50
#define	ENGINEAMD_SIG	"\x53\x31\xBB\x01\x00\x00\x00\x51"
#define	ENGINEAMD_OFFS	47
#define ENGINEW32_SIG	"\xA1\x2A\x2A\x2A\x2A\x56\xBE\x01"
#define ENGINEW32_OFFS	38
#define IA32_CALL		0xE8

bool vcmp(void *_addr1, void *_addr2, size_t len)
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
	char *addr = NULL;
	FnCommandCallback callback = NULL;
	int offs = 0;

	while (pBase)
	{
		if ( strcmp(pBase->GetName(), "echo") == 0 )
		{
			//callback = //*((FnCommandCallback *)((char *)pBase + offsetof(ConCommand, m_fnCommandCallback)));
			callback = ((ConCommand *)pBase)->GetCallback();
			ptr = (unsigned char *)callback;
			if (vcmp(ptr, ENGINE486_SIG, SIGLEN))
			{
				offs = ENGINE486_OFFS;
			} else if (vcmp(ptr, ENGINE686_SIG, SIGLEN)) {
				offs = ENGINE686_OFFS;
			} else if (vcmp(ptr, ENGINEAMD_SIG, SIGLEN)) {
				offs = ENGINEAMD_OFFS;
			} else if (vcmp(ptr, ENGINEW32_SIG, SIGLEN)) {
				offs = ENGINEW32_OFFS;
			}
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
