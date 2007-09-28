#ifndef _INCLUDE_CONSOLE_MMS_H_
#define _INCLUDE_CONSOLE_MMS_H_

#include <interface.h>
#include <eiface.h>
#include "convar_smm.h"
#include <sh_list.h>

class SMConVarAccessor : public IConCommandBaseAccessor
{
public:
	bool RegisterConCommandBase(ConCommandBase *pCommand);
	bool Register(ConCommandBase *pCommand);
	void MarkCommandsAsGameDLL();
	void Unregister(ConCommandBase *pCommand);
	void UnregisterGameDLLCommands();
private:
	SourceHook::List<ConCommandBase*> m_RegisteredCommands;
};

extern SMConVarAccessor g_SMConVarAccessor;

#endif //_INCLUDE_CONSOLE_MMS_H_

