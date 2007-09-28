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

