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

using namespace SourceHook;

SMConVarAccessor g_SMConVarAccessor;

bool SMConVarAccessor::RegisterConCommandBase(ConCommandBase *pCommand)
{
	pCommand->SetNext(NULL);
	icvar->RegisterConCommand(pCommand);

	return true;
}

bool SMConVarAccessor::Register(ConCommandBase *pCommand)
{
	pCommand->SetNext(NULL);
	icvar->RegisterConCommand(pCommand);

	return true;
}

void SMConVarAccessor::Unregister(ConCommandBase *pCommand)
{
	icvar->UnregisterConCommand(pCommand);
}

