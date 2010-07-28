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

#ifndef _INCLUDE_CONSOLE_MMS_H_
#define _INCLUDE_CONSOLE_MMS_H_

#include <interface.h>
#include "convar.h"
#include <eiface.h>
#include <sh_list.h>

class SMConVarAccessor : public IConCommandBaseAccessor
{
public:
	bool RegisterConCommandBase(ConCommandBase *pCommand);
	bool Register(ConCommandBase *pCommand);
	void Unregister(ConCommandBase *pCommand);
	void RemoveMetamodCommands();
#if SOURCE_ENGINE == SE_DARKMESSIAH
	bool InitConCommandBaseList();
private:
	ConCommandBase **m_TopConCommandBase;
#endif
private:
	SourceHook::List<ConCommandBase *> m_RegisteredCommands;
};

extern SMConVarAccessor g_SMConVarAccessor;

#endif //_INCLUDE_CONSOLE_MMS_H_

