/* ======== SourceMM ========
 * Copyright (C) 2004-2008 Metamod:Source Development Team
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

#include "convar_smm.h"
#include "sourcemm.h"
#include "sh_list.h"

class SMConVarAccessor : public IConCommandBaseAccessor
{
	SourceHook::List<ConCommandBase*> m_RegisteredCommands;
	ConCommandBase **m_TopConCommandBase;
public:
	SMConVarAccessor();
	virtual bool RegisterConCommandBase(ConCommandBase *pCommand);
	bool Register(ConCommandBase *pCommand);
	void MarkCommandsAsGameDLL();
	bool InitConCommandBaseList();
	void Unregister(ConCommandBase *pCommand);
	void UnloadMetamodCommands();
};

void ClientCommand_handler(edict_t *client);

const char *GetPluginsFile();
const char *GetMetamodBaseDir();

extern SMConVarAccessor g_SMConVarAccessor;

#endif //_INCLUDE_CONCOMMANDS_H
