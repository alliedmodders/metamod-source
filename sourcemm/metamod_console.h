/* ======== SourceMM ========
 * Copyright (C) 2004-2007 Metamod:Source Development Team
 * No warranties of any kind
 *
 * License: zlib/libpng
 *
 * Author(s): David "BAILOPAN" Anderson
 * ============================
 */

#ifndef _INCLUDE_CONCOMMANDS_H
#define _INCLUDE_CONCOMMANDS_H

#include "metamod_provider.h"

bool Command_Meta(IMetamodSourceCommandInfo *info);
bool Command_ClientMeta(edict_t *client, IMetamodSourceCommandInfo *info);

#endif //_INCLUDE_CONCOMMANDS_H
