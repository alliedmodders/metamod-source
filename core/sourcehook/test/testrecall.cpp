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
	MAKE_STATE_1(State_H2_Func2, int);
	MAKE_STATE_2(State_HP_Func2, int, int);

	MAKE_STATE_2(State_Func22, int, int);
	MAKE_STATE_2(State_H1_Func22, int, int);
	MAKE_STATE_2(State_HP1_Func22, int, int);
	MAKE_STATE_2(State_HP2_Func22, int, int);

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

		// Overloaded version
		virtual int Func2(int a, int b)
		{
			ADD_STATE(State_Func22(a, b));
			return 0xDEADFC;
		}
	};

	void Handler1_Func1(int a)
	{
		ADD_STATE(State_H1_Func1(a));
		RETURN_META_NEWPARAMS(MRES_IGNORED, &Test::Func1, (5));
	}
	void Handler2_Func1(int a)
	{
		ADD_STATE(State_H2_Func1(a));
		RETURN_META_NEWPARAMS(MRES_IGNORED, &Test::Func1, (a - 5));
	}
	void HandlerPost_Func1(int a)
	{
		ADD_STATE(State_HP_Func1(a, META_IFACEPTR(void)));
	}


	int Handler1_Func2(int a)
	{
		ADD_STATE(State_H1_Func2(a));
		// Pfeeehhh, ugly, I know, but I had to test it :)
		RETURN_META_VALUE_NEWPARAMS(MRES_OVERRIDE, 500, 
			static_cast<int (Test::*)(int)>(&Test::Func2), (a - 10));
	}

	int Handler2_Func2(int a)
	{
		ADD_STATE(State_H2_Func2(a));
		RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, 0, 
			static_cast<int (Test::*)(int)>(&Test::Func2), (a - 10));
	}

	int HandlerPost_Func2(int a)
	{
		ADD_STATE(State_HP_Func2(a, META_RESULT_ORIG_RET(int)));
		RETURN_META_VALUE(MRES_IGNORED, 0);
	}

	int Handler1_Func22(int a, int b)
	{
		ADD_STATE(State_H1_Func22(a, b));
		RETURN_META_VALUE(MRES_IGNORED, 0);
	}

	int HandlerPost1_Func22(int a, int b)
	{
		ADD_STATE(State_HP1_Func22(a, b));
		RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, 0, static_cast<int (Test::*)(int, int)>(&Test::Func2), (1, 2));
	}

	int HandlerPost1A_Func22(int a, int b)
	{
		ADD_STATE(State_HP1_Func22(a, b));
		RETURN_META_VALUE_NEWPARAMS(MRES_OVERRIDE, 0, static_cast<int (Test::*)(int, int)>(&Test::Func2), (1, 2));
	}

	int HandlerPost2_Func22(int a, int b)
	{
		ADD_STATE(State_HP2_Func22(a, b));
		RETURN_META_VALUE(MRES_IGNORED, 0);
	}

	SH_DECL_HOOK1_void(Test, Func1, SH_NOATTRIB, 0, int);
	SH_DECL_HOOK1(Test, Func2, SH_NOATTRIB, 0, int, int);
	SH_DECL_HOOK2(Test, Func2, SH_NOATTRIB, 1, int, int, int);

	Test *MyTestFactory()
	{
		return new Test;
	}
}

bool TestRecall(std::string &error)
{
	GET_SHPTR(g_SHPtr);
	g_PLID = 1337;

	Test *ptr = MyTestFactory();
	CAutoPtrDestruction<Test> apd(ptr);

	SH_ADD_HOOK(Test, Func1, ptr, SH_STATIC(Handler1_Func1), false);
	SH_ADD_HOOK(Test, Func1, ptr, SH_STATIC(Handler2_Func1), false);
	SH_ADD_HOOK(Test, Func1, ptr, SH_STATIC(HandlerPost_Func1), true);

	ptr->Func1(77);

	CHECK_STATES((&g_States,
		new State_H1_Func1(77),
		new State_H2_Func1(5),
		new State_Func1(0),
		new State_HP_Func1(0, ptr),
		NULL), "Part 1");

	SH_REMOVE_HOOK(Test, Func1, ptr, SH_STATIC(Handler1_Func1), false);
	SH_ADD_HOOK(Test, Func1, ptr, SH_STATIC(Handler2_Func1), false);
	SH_ADD_HOOK(Test, Func1, ptr, SH_STATIC(Handler2_Func1), false);
	SH_ADD_HOOK(Test, Func1, ptr, SH_STATIC(Handler2_Func1), false);

	ptr->Func1(77);

	CHECK_STATES((&g_States,
		new State_H2_Func1(77),
		new State_H2_Func1(72),
		new State_H2_Func1(67),
		new State_H2_Func1(62),
		new State_Func1(57),
		new State_HP_Func1(57, ptr),
		NULL), "Part 2");

	SH_REMOVE_HOOK(Test, Func1, ptr, SH_STATIC(Handler2_Func1), false);
	SH_REMOVE_HOOK(Test, Func1, ptr, SH_STATIC(HandlerPost_Func1), true);

	ptr->Func1(77);

	CHECK_STATES((&g_States,
		new State_Func1(77),
		NULL), "Part 3");

	// Func2

	SH_ADD_HOOK(Test, Func2, ptr, SH_STATIC(Handler1_Func2), false);
	SH_ADD_HOOK(Test, Func2, ptr, SH_STATIC(HandlerPost_Func2), true);

	int a = ptr->Func2(77);
	CHECK_STATES((&g_States,
		new State_H1_Func2(77),
		new State_Func2(67),
		new State_HP_Func2(67, 1000),			// 1000 because it's the ORIG_RET
		NULL), "Part 4");

	CHECK_COND(a == 500, "Part 4.1");

	// Func2, with other handler
	SH_REMOVE_HOOK(Test, Func2, ptr, SH_STATIC(Handler1_Func2), false);
	SH_ADD_HOOK(Test, Func2, ptr, SH_STATIC(Handler2_Func2), false);

	a = ptr->Func2(77);
	CHECK_STATES((&g_States,
		new State_H2_Func2(77),
		new State_Func2(67),
		new State_HP_Func2(67, 1000),			// 1000 because it's the ORIG_RET
		NULL), "Part 4.2");

	CHECK_COND(a == 1000, "Part 4.2.1");		// Should return 1000 as well.

	// Func22 -> post recalls

	// 1) WITH OVERRIDE

	SH_ADD_HOOK(Test, Func2, ptr, SH_STATIC(Handler1_Func22), false);
	SH_ADD_HOOK(Test, Func2, ptr, SH_STATIC(HandlerPost1A_Func22), true);
	SH_ADD_HOOK(Test, Func2, ptr, SH_STATIC(HandlerPost2_Func22), true);
	
	a = ptr->Func2(10, 11);
	CHECK_STATES((&g_States,
		new State_H1_Func22(10, 11),
		new State_Func22(10, 11),
		new State_HP1_Func22(10, 11),
		new State_HP2_Func22(1, 2),
		NULL), "Part 5");

	CHECK_COND(a == 0, "Part 5.1");

	// 2) WITH IGNORE
	SH_REMOVE_HOOK(Test, Func2, ptr, SH_STATIC(HandlerPost1A_Func22), true);
	SH_REMOVE_HOOK(Test, Func2, ptr, SH_STATIC(HandlerPost2_Func22), true);

	SH_ADD_HOOK(Test, Func2, ptr, SH_STATIC(HandlerPost1_Func22), true);
	SH_ADD_HOOK(Test, Func2, ptr, SH_STATIC(HandlerPost2_Func22), true);

	a = ptr->Func2(10, 11);
	CHECK_STATES((&g_States,
		new State_H1_Func22(10, 11),
		new State_Func22(10, 11),
		new State_HP1_Func22(10, 11),
		new State_HP2_Func22(1, 2),
		NULL), "Part 5");

	CHECK_COND(a == 0xDEADFC, "Part 5.1");

	return true;
}
