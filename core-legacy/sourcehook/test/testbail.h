//////////////////////////////////////////////////////////////////////////
// Shared data for testbail

#include <string>
#include "testevents.h"

namespace N_TestBail
{
	extern StateList g_States;
	extern SourceHook::ISourceHook *g_SHPtr;
	
	MAKE_STATE_1(State_EatYams_Called, int);
	MAKE_STATE_1(State_EatYams_Handler1_Called, int);
	MAKE_STATE_1(State_EatYams_Handler2_Called, int);
	MAKE_STATE_1(State_EatYams_Handler3_Called, int);
	MAKE_STATE_1(State_EatYams_Return, int);

	class IGaben
	{
	public:
		virtual int EatYams(int a)
		{
			ADD_STATE(State_EatYams_Called(a));
			return 5;
		}
	};

	extern IGaben *g_Gabgab;

	bool TestBail2(std::string &error);
}

using namespace N_TestBail;

namespace
{
	SourceHook::Plugin g_PLID;
	SH_DECL_HOOK1(IGaben, EatYams, SH_NOATTRIB, 0, int, int);
}
