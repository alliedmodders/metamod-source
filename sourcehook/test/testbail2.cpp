// TESTBAIL
//  Different compilation unit

#include "testbail.h"

void ___TestBail2()
{
	SourceHook::CSourceHookImpl g_SHImpl;
	g_SHPtr = &g_SHImpl;
	g_PLID = 2;

	g_Gabgab = (IGaben*)___testbail_gabgab;

	g_Gabgab->EatYams();

	SH_ADD_HOOK_STATICFUNC(IGaben, EatYams, g_Gabgab, EatYams0_Handler, false);

	g_Gabgab->EatYams();
	
	SH_REMOVE_HOOK_STATICFUNC(IGaben, EatYams, g_Gabgab, EatYams0_Handler, false);

	g_Gabgab->EatYams();
}