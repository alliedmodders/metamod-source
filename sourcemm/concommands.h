/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_CONCOMMANDS_H
#define _INCLUDE_CONCOMMANDS_H

/**
 * @brief Header for console commands
 * @file concommands.h
 */

#include <interface.h>
#include <eiface.h>
#include "sourcemm.h"
#include <convar.h>
#include "sh_list.h"

class SMConVarAccessor : public IConCommandBaseAccessor
{
	SourceHook::List<ConCommandBase*> m_RegisteredCommands;
public:
	virtual bool RegisterConCommandBase(ConCommandBase *pCommand);
	bool Register(ConCommandBase *pCommand);
	void MarkCommandsAsGameDLL();
	void Unregister(ConCommandBase *pCommand);
	void UnregisterGameDLLCommands();
};

class CAlwaysRegisterableCommand : public ConCommandBase
{
	ICvar *m_pICvar;
public:
	CAlwaysRegisterableCommand();
	bool IsRegistered( void ) const;
	// If already registered, removes us
	// Then it registers us again
	void BringToFront();
};

const char *GetPluginsFile();

extern SMConVarAccessor g_SMConVarAccessor;

#endif //_INCLUDE_CONCOMMANDS_H
