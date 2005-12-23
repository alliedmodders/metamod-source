#include <string>
#include "sourcehook.h"
#include "sourcehook_test.h"
#include "testevents.h"

// TESTRECALL
// Test modifying parameters from hook handlers

namespace
{
	StateList g_States;
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

	MAKE_STATE_1(State_Func1, int);
	MAKE_STATE_1(State_H1_Func1, int);
	MAKE_STATE_1(State_H2_Func1, int);
	MAKE_STATE_2(State_HP_Func1, int, void*);

	MAKE_STATE_1(State_Func2, int);
	MAKE_STATE_1(State_H1_Func2, int);
	MAKE_STATE_2(State_HP_Func2, int, int);

	struct Test
	{
		virtual void Func1(int a)
		{
			ADD_STATE(State_Func1(a));
		}

		virtual int Func2(int a)
		{
			ADD_STATE(State_Func2(a));
			return 1000;
		}
	};

	void Handler1_Func1(int a)
	{
		ADD_STATE(State_H1_Func1(a));
		RETURN_META_NEWPARAMS(MRES_IGNORED, Test, Func1, (5));
	}
	void Handler2_Func1(int a)
	{
		ADD_STATE(State_H2_Func1(a));
		RETURN_META_NEWPARAMS(MRES_IGNORED, Test, Func1, (a - 5));
	}
	void HandlerPost_Func1(int a)
	{
		ADD_STATE(State_HP_Func1(a, META_IFACEPTR(void)));
	}


	int Handler1_Func2(int a)
	{
		ADD_STATE(State_H1_Func2(a));
		RETURN_META_VALUE_NEWPARAMS(MRES_OVERRIDE, 500, Test, Func2, (a - 10));
	}

	int HandlerPost_Func2(int a)
	{
		ADD_STATE(State_HP_Func2(a, META_RESULT_ORIG_RET(int)));
		RETURN_META_VALUE(MRES_IGNORED, 0);
	}

	SH_DECL_HOOK1_void(Test, Func1, SH_NOATTRIB, 0, int);
	SH_DECL_HOOK1(Test, Func2, SH_NOATTRIB, 0, int, int);
}

bool TestRecall(std::string &error)
{
	GET_SHPTR(g_SHPtr);
	g_PLID = 1337;

	Test inst;
	Test *ptr = &inst;

	SH_ADD_HOOK_STATICFUNC(Test, Func1, ptr, Handler1_Func1, false);
	SH_ADD_HOOK_STATICFUNC(Test, Func1, ptr, Handler2_Func1, false);
	SH_ADD_HOOK_STATICFUNC(Test, Func1, ptr, HandlerPost_Func1, true);

	ptr->Func1(77);

	CHECK_STATES((&g_States,
		new State_H1_Func1(77),
		new State_H2_Func1(5),
		new State_Func1(0),
		new State_HP_Func1(0, ptr),
		NULL), "Part 1");

	SH_REMOVE_HOOK_STATICFUNC(Test, Func1, ptr, Handler1_Func1, false);
	SH_ADD_HOOK_STATICFUNC(Test, Func1, ptr, Handler2_Func1, false);
	SH_ADD_HOOK_STATICFUNC(Test, Func1, ptr, Handler2_Func1, false);
	SH_ADD_HOOK_STATICFUNC(Test, Func1, ptr, Handler2_Func1, false);

	ptr->Func1(77);

	CHECK_STATES((&g_States,
		new State_H2_Func1(77),
		new State_H2_Func1(72),
		new State_H2_Func1(67),
		new State_H2_Func1(62),
		new State_Func1(57),
		new State_HP_Func1(57, ptr),
		NULL), "Part 2");

	SH_REMOVE_HOOK_STATICFUNC(Test, Func1, ptr, Handler2_Func1, false);
	SH_REMOVE_HOOK_STATICFUNC(Test, Func1, ptr, HandlerPost_Func1, true);

	ptr->Func1(77);

	CHECK_STATES((&g_States,
		new State_Func1(77),
		NULL), "Part 3");

	// Func2

	SH_ADD_HOOK_STATICFUNC(Test, Func2, ptr, Handler1_Func2, false);
	SH_ADD_HOOK_STATICFUNC(Test, Func2, ptr, HandlerPost_Func2, true);

	int a = ptr->Func2(77);
	CHECK_STATES((&g_States,
		new State_H1_Func2(77),
		new State_Func2(67),
		new State_HP_Func2(67, 1000),			// 1000 because it's the ORIG_RET
		NULL), "Part 4");

	CHECK_COND(a == 500, "Part 4.1");

	return true;
}
