//////////////////////////////////////////////////////////////////////////
// Shared data for testbail

#include <string>
#include "sourcehook_impl.h"
#include "testevents.h"
#include <stdarg.h>
#include <list>
#include <algorithm>

void ___TestBail2();

namespace
{
	StateList g_States;
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

	MAKE_STATE(State_EatYams_Called);
	MAKE_STATE(State_EatYams_Handler_Called);


	class IGaben
	{
	public:
		virtual void EatYams()
		{
			ADD_STATE(State_EatYams_Called);
		}
	};

	SH_DECL_HOOK0_void(IGaben, EatYams, SH_NOATTRIB, 0);

	void EatYams0_Handler()
	{
		ADD_STATE(State_EatYams_Handler_Called);
	}

	IGaben *g_Gabgab;
}

extern void *___testbail_gabgab;
