// TESTBAIL
//  Different compilation unit

#include "sourcehook.h"
#include "sourcehook_test.h"
#include "testbail.h"

// :TODO: Test new-old proto system compa

bool ___TestBail2(std::string &error)
{
	g_SHPtr = ___testbail_shptr;
	g_PLID = 2;

	g_Gabgab = (IGaben*)___testbail_gabgab;

	SH_ADD_HOOK_STATICFUNC(IGaben, EatYams, g_Gabgab, ___testbail_EatYams_Handler2, false);
	SH_ADD_HOOK_STATICFUNC(IGaben, EatYams, g_Gabgab, ___testbail_EatYams_Handler3, false);

	int ret = g_Gabgab->EatYams(0xDEAD);
	
	CHECK_COND(ret == 6, "Part 2.1");
	
	return true;
}
