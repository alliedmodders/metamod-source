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
#include <list>

class SMConVarAccessor : public IConCommandBaseAccessor
{
	std::list<ConCommandBase*> m_RegisteredCommands;
public:
	virtual bool RegisterConCommandBase(ConCommandBase *pCommand);
	void MarkCommandsAsGameDLL();
	void Unregister(ConCommandBase *pCvar);
};

extern SMConVarAccessor g_SMConVarAccessor;

#endif //_INCLUDE_CONCOMMANDS_H
