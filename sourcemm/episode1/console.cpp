/**
 * vim: set ts=4 :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2007 AlliedModders LLC and authors.
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
#include "provider_ep1.h"

using namespace SourceHook;

SMConVarAccessor g_SMConVarAccessor;

class CAlwaysRegisterableCommand : public ConCommandBase
{
public:
	CAlwaysRegisterableCommand()
	{
		Create("metamod_eternal", NULL, FCVAR_UNREGISTERED|FCVAR_GAMEDLL);
	}
	bool IsRegistered( void ) const
	{
		return false;
	}
	void BringToFront()
	{
		// First, let's try to find us!
		ConCommandBase *pPtr = icvar->GetCommands();
	
		if (pPtr == this)
		{
			// We are already at the beginning; Nothing to do
			return;
		}
	
		while (pPtr)
		{
			if (pPtr == this)
			{
				break;
			}
			ConCommandBase *pPrev = NULL;
			while (pPtr)
			{
				if (pPtr == this)
				{
					break;
				}
				pPrev = pPtr;
				pPtr = const_cast<ConCommandBase*>(pPtr->GetNext());
			}
			if (pPrev && pPtr == this)
			{
				pPrev->SetNext(m_pNext);		// Remove us from the list
			}
			// Now, register us
			SetNext(NULL);
			icvar->RegisterConCommandBase(this);
		}	
	}
} s_EternalCommand;

bool SMConVarAccessor::RegisterConCommandBase(ConCommandBase *pCommand)
{
	m_RegisteredCommands.push_back(pCommand);
	pCommand->SetNext(NULL);
	icvar->RegisterConCommandBase(pCommand);

	return true;
}

bool SMConVarAccessor::Register(ConCommandBase *pCommand)
{
	pCommand->SetNext(NULL);
	icvar->RegisterConCommandBase(pCommand);

	return true;
}

void SMConVarAccessor::MarkCommandsAsGameDLL()
{
	for (List<ConCommandBase*>::iterator iter = m_RegisteredCommands.begin();
		iter != m_RegisteredCommands.end(); ++iter)
	{
		(*iter)->AddFlags(FCVAR_GAMEDLL);
	}
}

void SMConVarAccessor::UnregisterGameDLLCommands()
{
	ConCommandBase *begin = icvar->GetCommands();
	ConCommandBase *iter = begin;
	ConCommandBase *prev = NULL;
	while (iter)
	{
		/* Watch out for the ETERNAL COMMAND! */
		if (iter != &s_EternalCommand && iter->IsBitSet(FCVAR_GAMEDLL))
		{
			/* Remove it! */
			if (iter == begin)
			{
				s_EternalCommand.BringToFront();
				iter = const_cast<ConCommandBase*>(iter->GetNext());
				s_EternalCommand.SetNext(iter);
				prev = &s_EternalCommand;
				continue;
			}
			else
			{
				iter = const_cast<ConCommandBase*>(iter->GetNext());
				prev->SetNext(iter);
				continue;
			}
		}
		prev = iter;
		iter = const_cast<ConCommandBase*>(iter->GetNext());
	}
}

void SMConVarAccessor::Unregister(ConCommandBase *pCommand)
{
	ConCommandBase *ptr = icvar->GetCommands();

	if (ptr == pCommand)
	{
		s_EternalCommand.BringToFront();
		s_EternalCommand.SetNext(const_cast<ConCommandBase *>(pCommand->GetNext()));
	}
	else
	{
		/* Find us and unregister us */
		ConCommandBase *pPrev = NULL;
		while (ptr)
		{
			if (ptr == pCommand)
			{
				break;
			}
			pPrev = ptr;
			ptr = const_cast<ConCommandBase *>(ptr->GetNext());
		}
		if (pPrev && ptr == pCommand)
		{
			pPrev->SetNext(const_cast<ConCommandBase *>(pCommand->GetNext()));
		}		
	}
}

