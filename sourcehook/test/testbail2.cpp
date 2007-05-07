// TESTBAIL
//  Different compilation unit

#include "sourcehook.h"
#include "sourcehook_test.h"
#include "testbail.h"


int EatYams_Handler2(int a)
{
	ADD_STATE(State_EatYams_Handler2_Called(a));
	RETURN_META_VALUE_NEWPARAMS(MRES_OVERRIDE, 6, &IGaben::EatYams, (0xBEEF));
}

int EatYams_Handler3(int a)
{
	ADD_STATE(State_EatYams_Handler3_Called(a));
	RETURN_META_VALUE(MRES_IGNORED, 0);
}

namespace N_TestBail
{
	bool TestBail2(std::string &error)
	{
		g_PLID = 2;

		SH_ADD_HOOK(IGaben, EatYams, g_Gabgab, SH_STATIC(EatYams_Handler2), false);
		SH_ADD_HOOK(IGaben, EatYams, g_Gabgab, SH_STATIC(EatYams_Handler3), false);

		int ret = g_Gabgab->EatYams(0xDEAD);

		CHECK_COND(ret == 6, "Part 2.1");

		return true;
	}
}
