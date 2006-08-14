/* ======== sample_mm ========
 * Copyright (C) 2004-2006 Metamod:Source Development Team
 * No warranties of any kind
 *
 * License: zlib/libpng
 *
 * Author(s): David "BAILOPAN" Anderson
 * ============================
 */

#ifndef _INCLUDE_CVARS_H
#define _INCLUDE_CVARS_H

#include <convar.h>

class SampleAccessor : public IConCommandBaseAccessor
{
public:
	virtual bool RegisterConCommandBase(ConCommandBase *pVar);
};

extern SampleAccessor g_Accessor;

#endif //_INCLUDE_CVARS_H
