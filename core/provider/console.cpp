/**
 * vim: set ts=4 :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2008 AlliedModders LLC and authors.
 * All rights reserved.
 * ======================================================
 *
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the authors be held liable for any damages arising from 
 * the use of this software.
 * 
 * Permission is granted to anyone to use this software for any purpose, 
 * including commercial applications, and to alter it and redistribute it 
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not 
 * claim that you wrote the original software. If you use this software in a 
 * product, an acknowledgment in the product documentation would be 
 * appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 * misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 * Version: $Id$
 */

#include "console.h"
#include "provider_ep2.h"
#include "metamod_util.h"

using namespace SourceHook;

SMConVarAccessor g_SMConVarAccessor;

#if SOURCE_ENGINE >= SE_ORANGEBOX
#else
#define RegisterConCommand RegisterConCommandBase
#endif

bool SMConVarAccessor::RegisterConCommandBase(ConCommandBase *pCommand)
{
	m_RegisteredCommands.push_back(pCommand);
#if SOURCE_ENGINE < SE_ALIENSWARM
	pCommand->SetNext(NULL);
#endif
	icvar->RegisterConCommand(pCommand);

	return true;
}

bool SMConVarAccessor::Register(ConCommandBase *pCommand)
{
#if SOURCE_ENGINE < SE_ALIENSWARM
	pCommand->SetNext(NULL);
#endif
	icvar->RegisterConCommand(pCommand);

	return true;
}

void SMConVarAccessor::RemoveMetamodCommands()
{
	List<ConCommandBase *>::iterator iter;

	for (iter = m_RegisteredCommands.begin(); iter != m_RegisteredCommands.end(); iter++)
	{
		Unregister(*iter);
	}
}

#if SOURCE_ENGINE == SE_DARKMESSIAH
/* Signature for ICvar::GetCommands() in vstdlib for Win32 and Linux.
 *
 * 20226EE0 A1 50 5C 5A 20   mov         eax,dword ptr ds:[205A5C50h] <-- What we want
 * 20226EE5 C3               ret              
 */
#define CMDLIST_SIG "\xA1\x2A\x2A\x2A\x2A\xC3"
#define CMDLIST_SIGLEN 6

/* Linux symbol name of ConCommandBase list in vstdlib */
#define CMDLIST_SYMBOL "_ZN14ConCommandBase18s_pConCommandBasesE"

/* This function retrieves the address of the var that holds the top of the ConCommandBase list.
 * Having this allows us to change the beginning of this list with ease.
 *
 * This craziness eliminates the need for the eternal command/cvar used previously which
 * could have caused a crash as a result of registering commands/cvars more than once.
 */
bool SMConVarAccessor::InitConCommandBaseList()
{
	char *vfunc = (char *)SH_GET_ORIG_VFNPTR_ENTRY(icvar, &ICvar::GetCommands);

	if (*vfunc == '\xE9')
	{
		/* Get address from displacement...
		*
		* Add 5 because it's relative to next instruction:
		* Opcode <1 byte> + 32-bit displacement <4 bytes> 
		*/
		vfunc += *reinterpret_cast<int *>(vfunc + 1) + 5;
	}

	if (!vfunc)
	{
		return false;
	}

#ifdef OS_WIN32
	if (UTIL_VerifySignature(vfunc, CMDLIST_SIG, CMDLIST_SIGLEN))
	{
		/* Skip past 0xA1 and get addr of ConCommandBase list var */
		m_TopConCommandBase = *reinterpret_cast<ConCommandBase ***>(vfunc + 1);
		return true;
	}
#elif defined OS_LINUX
	/* Try dlsym first */
	char path[PATH_SIZE];
	if (GetFileOfAddress((void *)icvar, path, sizeof(path)))
	{
		void *handle = dlopen(path, RTLD_NOW);
		if (handle)
		{
			m_TopConCommandBase = reinterpret_cast<ConCommandBase **>(dlsym(handle, CMDLIST_SYMBOL));
			dlclose(handle);
			return true;
		}
	}

	/* If dlsym failed, then verify signature of function */
	if (!m_TopConCommandBase && UTIL_VerifySignature(vfunc, CMDLIST_SIG, CMDLIST_SIGLEN))
	{
		/* Skip past 0xA1 and get addr of ConCommandBase list var */
		m_TopConCommandBase = *reinterpret_cast<ConCommandBase ***>(vfunc + 1);
		return true;
	}
#endif

	return false;
}
#endif

void SMConVarAccessor::Unregister(ConCommandBase *pCommand)
{
#if SOURCE_ENGINE >= SE_ORANGEBOX
	icvar->UnregisterConCommand(pCommand);
#else
	ConCommandBase *pCur = NULL;
	ConCommandBase *pPrev = NULL;

	if (!pCommand)
	{
		return;
	}

	pCur = icvar->GetCommands();

	if (!m_TopConCommandBase || !pCur)
	{
		return;
	}

	if (pCur == pCommand)
	{
		*m_TopConCommandBase = const_cast<ConCommandBase *>(pCommand->GetNext());
		pCommand->SetNext(NULL);
		return;
	}
	
	pPrev = pCur;
	pCur = const_cast<ConCommandBase *>(pCur->GetNext());

	while (pCur)
	{
		if (pCur == pCommand)
		{
			pPrev->SetNext(const_cast<ConCommandBase *>(pCommand->GetNext()));
			pCommand->SetNext(NULL);
		}

		pPrev = pCur;
		pCur = const_cast<ConCommandBase *>(pCur->GetNext());
	}
#endif
}

