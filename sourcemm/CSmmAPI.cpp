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

void CSmmAPI::UnregisterCvar(ConCommandBase *pCvar)
{
	g_SMConVarAccessor.Unregister(pCvar);
}

