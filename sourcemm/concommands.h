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

#include "sourcemm.h"
#include <convar.h>

class SMConVarAccessor : public IConCommandBaseAccessor
{
public:
	virtual bool RegisterConCommandBase(ConCommandBase *pCommand);
};

extern SMConVarAccessor g_SMConVarAccessor;

#endif //_INCLUDE_CONCOMMANDS_H
