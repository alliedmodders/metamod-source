/* ======== sample_mm ========
* Copyright (C) 2004-2006 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#include "SamplePlugin.h"
#include "cvars.h"

SampleAccessor g_Accessor;

ConVar g_MyConVar("sample_version", SAMPLE_VERSION, FCVAR_SPONLY, "Sample Plugin version");

bool SampleAccessor::RegisterConCommandBase(ConCommandBase *pVar)
{
	//this will work on any type of concmd!
	return META_REGCVAR(pVar);
}

CON_COMMAND(sample_cmd, "Sample Plugin command")
{
	META_LOG(g_PLAPI, "This sentence is in Spanish when you're not looking.");
}
