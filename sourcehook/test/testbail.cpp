// TESTBAIL
// Test for a bug Bail has found

#include "testbail.h"

void *___testbail_gabgab;

bool TestBail(std::string &error)
{
	SourceHook::CSourceHookImpl g_SHImpl;
	g_SHPtr = &g_SHImpl;
	g_PLID = 1;

	g_Gabgab = new IGaben;
	___testbail_gabgab = (void*)g_Gabgab;

	g_Gabgab->EatYams();

	SH_ADD_HOOK_STATICFUNC(IGaben, EatYams, g_Gabgab, EatYams0_Handler, false);

	g_Gabgab->EatYams();

	___TestBail2();
	
	g_Gabgab->EatYams();

	SH_REMOVE_HOOK_STATICFUNC(IGaben, EatYams, g_Gabgab, EatYams0_Handler, false);

	g_Gabgab->EatYams();

	delete g_Gabgab;

	// If it didn't crash, it's ok

	return true;
}

