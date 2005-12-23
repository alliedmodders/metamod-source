// TESTBAIL
// This test used to be a test for a bug BAIL found.
// That bug is now fixed so I've granted BAIL the pleasure of being a test for
// the correct priority ordering of hook managers based on their version.

/*
	THE PROBLEM:
		Old hook funcs don't work right when you combine override returns and recalls.
	THE SOLUTION:
		Always use a new hook func when possible. For this, hook funcs have to be able to say
			" HELLO I'M NEW! "

	This file tests that functionality.

	How it works:
		testbail.cpp compiles with old version of sourcehook.h.
		It sets everything up, adds a hook on a function
		Then testbail2.cpp which has the new version adds a hook on the same function and 
		does a recall and overrides the value in it.
*/

#include "sourcehook_test.h"
#include "testbail.h"

void *___testbail_gabgab;
SourceHook::ISourceHook *___testbail_shptr;

namespace
{
	int EatYams_Handler1(int a)
	{
		ADD_STATE(State_EatYams_Handler1_Called(a));
		RETURN_META_VALUE(MRES_IGNORED, 0);
	}
}

// These are here so they can access this CU's g_States
int ___testbail_EatYams_Handler2(int a)
{
	ADD_STATE(State_EatYams_Handler2_Called(a));
	RETURN_META_VALUE_NEWPARAMS(MRES_OVERRIDE, 6, IGaben, EatYams, (0xBEEF));
}

int ___testbail_EatYams_Handler3(int a)
{
	ADD_STATE(State_EatYams_Handler3_Called(a));
	RETURN_META_VALUE(MRES_IGNORED, 0);
}

bool TestBail(std::string &error)
{
	GET_SHPTR(g_SHPtr);
	g_PLID = 1;

	g_Gabgab = new IGaben;
	___testbail_gabgab = (void*)g_Gabgab;
	___testbail_shptr = g_SHPtr;

	SH_ADD_HOOK_STATICFUNC(IGaben, EatYams, g_Gabgab, EatYams_Handler1, false);

	ADD_STATE(State_EatYams_Return(g_Gabgab->EatYams(0xDEAD)));

	CHECK_STATES((&g_States,
		new State_EatYams_Handler1_Called(0xDEAD),
		new State_EatYams_Called(0xDEAD),
		new State_EatYams_Return(5),
		NULL), "Part 1");

	if (!___TestBail2(error))
		return false;

	CHECK_STATES((&g_States,
		new State_EatYams_Handler1_Called(0xDEAD),
		new State_EatYams_Handler2_Called(0xDEAD),
		new State_EatYams_Handler3_Called(0xBEEF),
		new State_EatYams_Called(0xBEEF),
		NULL), "Part 2.1");

	// WHAT IF NOW SOMEONE UNLOADS PLUGIN 2 !?!?!?!?
	Test_UnloadPlugin(g_SHPtr, 2);

	ADD_STATE(State_EatYams_Return(g_Gabgab->EatYams(0xDEAD)));

	CHECK_STATES((&g_States,
		new State_EatYams_Handler1_Called(0xDEAD),
		new State_EatYams_Called(0xDEAD),
		new State_EatYams_Return(5),
		NULL), "Part 3");

	SH_REMOVE_HOOK_STATICFUNC(IGaben, EatYams, g_Gabgab, EatYams_Handler1, false);

	ADD_STATE(State_EatYams_Return(g_Gabgab->EatYams(0xDEAD)));

	CHECK_STATES((&g_States,
		new State_EatYams_Called(0xDEAD),
		new State_EatYams_Return(5),
		NULL), "Part 4");

	delete g_Gabgab;

	return true;
}

