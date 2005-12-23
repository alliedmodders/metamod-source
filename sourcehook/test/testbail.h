//////////////////////////////////////////////////////////////////////////
// Shared data for testbail

#include <string>
#include "testevents.h"

bool ___TestBail2(std::string &error);

namespace
{
	StateList g_States;
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

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

	SH_DECL_HOOK1(IGaben, EatYams, SH_NOATTRIB, 0, int, int);

	IGaben *g_Gabgab;
}

extern void *___testbail_gabgab;
extern SourceHook::ISourceHook *___testbail_shptr;
extern int ___testbail_EatYams_Handler2(int a);
extern int ___testbail_EatYams_Handler3(int a);