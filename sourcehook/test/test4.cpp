#include <string>
#include "sourcehook_test.h"
#include "testevents.h"

// TEST4
// Tests of plugin management system

namespace
{
	StateList g_States;
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

	MAKE_STATE(State_Func1_Called);
	MAKE_STATE(State_Func2_Called);
	MAKE_STATE(State_Func3_Called);

	MAKE_STATE(State_Func1H_Called);
	MAKE_STATE(State_Func2H_Called);
	MAKE_STATE(State_Func3H_Called);

	MAKE_STATE_2(State_PluginInUse, int, bool);

	class Test
	{
	public:
		virtual void Func1()
		{
			ADD_STATE(State_Func1_Called);
		}

		virtual void Func2()
		{
			ADD_STATE(State_Func2_Called);
		}

		virtual void Func3()
		{
			ADD_STATE(State_Func3_Called);
		}
	};

	// GCC's optimizer is too good. I had to add this in order to make it execute a virtual table lookup!
	class Whatever : public Test
	{
	};

	SH_DECL_HOOK0_void(Test, Func1, SH_NOATTRIB, 0);
	SH_DECL_HOOK0_void(Test, Func2, SH_NOATTRIB, 0);
	SH_DECL_HOOK0_void(Test, Func3, SH_NOATTRIB, 0);

	void Handler_Func1()
	{
		ADD_STATE(State_Func1H_Called);
	}
	void Handler_Func2()
	{
		ADD_STATE(State_Func2H_Called);
	}
	void Handler_Func3()
	{
		ADD_STATE(State_Func3H_Called);
	}
}

bool TestPlugSys(std::string &error)
{
	GET_SHPTR(g_SHPtr);
	g_PLID = 1;

	Whatever inst;
	Test *pInst = &inst;

	// 1)
	//   Add hooks, then issue a complete shutdown
	SH_ADD_HOOK(Test, Func1, pInst, SH_STATIC(Handler_Func1), false);
	SH_ADD_HOOK(Test, Func2, pInst, SH_STATIC(Handler_Func2), true);
	SH_ADD_HOOK(Test, Func3, pInst, SH_STATIC(Handler_Func3), false);

	pInst->Func1();
	pInst->Func2();
	pInst->Func3();

	CHECK_STATES((&g_States,
		new State_Func1H_Called,
		new State_Func1_Called,
		new State_Func2_Called,
		new State_Func2H_Called,
		new State_Func3H_Called,
		new State_Func3_Called,
		NULL), "Part 1.1");

	Test_CompleteShutdown(g_SHPtr);

	pInst->Func1();
	pInst->Func2();
	pInst->Func3();

	CHECK_STATES((&g_States,
		new State_Func1_Called,
		new State_Func2_Called,
		new State_Func3_Called,
		NULL), "Part 1.2");

	// 2)
	//   Add hooks from "different plugins", then shutdown the plugins

	g_PLID = 1;
	SH_ADD_HOOK(Test, Func1, pInst, SH_STATIC(Handler_Func1), false);
	g_PLID = 2;
	SH_ADD_HOOK(Test, Func2, pInst, SH_STATIC(Handler_Func2), true);
	g_PLID = 3;
	SH_ADD_HOOK(Test, Func3, pInst, SH_STATIC(Handler_Func3), false);

	g_PLID = 1;
	SH_ADD_HOOK(Test, Func2, pInst, SH_STATIC(Handler_Func2), true);
	SH_ADD_HOOK(Test, Func3, pInst, SH_STATIC(Handler_Func3), false);

	g_PLID = 2;
	SH_ADD_HOOK(Test, Func1, pInst, SH_STATIC(Handler_Func1), false);
	SH_ADD_HOOK(Test, Func3, pInst, SH_STATIC(Handler_Func3), false);

	g_PLID = 3;
	SH_ADD_HOOK(Test, Func1, pInst, SH_STATIC(Handler_Func1), false);
	SH_ADD_HOOK(Test, Func2, pInst, SH_STATIC(Handler_Func2), true);

	pInst->Func1();
	pInst->Func2();
	pInst->Func3();

	CHECK_STATES((&g_States,
		new State_Func1H_Called,
		new State_Func1H_Called,
		new State_Func1H_Called,
		new State_Func1_Called,
		new State_Func2_Called,
		new State_Func2H_Called,
		new State_Func2H_Called,
		new State_Func2H_Called,
		new State_Func3H_Called,
		new State_Func3H_Called,
		new State_Func3H_Called,
		new State_Func3_Called,
		NULL), "Part 2.1");

	ADD_STATE(State_PluginInUse(1, Test_IsPluginInUse(g_SHPtr, 1)));
	ADD_STATE(State_PluginInUse(2, Test_IsPluginInUse(g_SHPtr, 2)));
	ADD_STATE(State_PluginInUse(3, Test_IsPluginInUse(g_SHPtr, 3)));
	ADD_STATE(State_PluginInUse(4, Test_IsPluginInUse(g_SHPtr, 4)));

	CHECK_STATES((&g_States,
		new State_PluginInUse(1, true),
		new State_PluginInUse(2, true),
		new State_PluginInUse(3, true),
		new State_PluginInUse(4, false),
		NULL), "Part 2.2");

	// Unload plugins one by one
	Test_UnloadPlugin(g_SHPtr, 3);

	pInst->Func1();
	pInst->Func2();
	pInst->Func3();

	CHECK_STATES((&g_States,
		new State_Func1H_Called,
		new State_Func1H_Called,
		new State_Func1_Called,
		new State_Func2_Called,
		new State_Func2H_Called,
		new State_Func2H_Called,
		new State_Func3H_Called,
		new State_Func3H_Called,
		new State_Func3_Called,
		NULL), "Part 2.3.1");

	ADD_STATE(State_PluginInUse(1, Test_IsPluginInUse(g_SHPtr, 1)));
	ADD_STATE(State_PluginInUse(2, Test_IsPluginInUse(g_SHPtr, 2)));
	ADD_STATE(State_PluginInUse(3, Test_IsPluginInUse(g_SHPtr, 3)));
	ADD_STATE(State_PluginInUse(4, Test_IsPluginInUse(g_SHPtr, 4)));

	CHECK_STATES((&g_States,
		new State_PluginInUse(1, true),
		new State_PluginInUse(2, true),
		new State_PluginInUse(3, false),
		new State_PluginInUse(4, false),
		NULL), "Part 2.3.2");

	Test_UnloadPlugin(g_SHPtr, 2);

	pInst->Func1();
	pInst->Func2();
	pInst->Func3();

	CHECK_STATES((&g_States,
		new State_Func1H_Called,
		new State_Func1_Called,
		new State_Func2_Called,
		new State_Func2H_Called,
		new State_Func3H_Called,
		new State_Func3_Called,
		NULL), "Part 2.4.1");

	ADD_STATE(State_PluginInUse(1, Test_IsPluginInUse(g_SHPtr, 1)));
	ADD_STATE(State_PluginInUse(2, Test_IsPluginInUse(g_SHPtr, 2)));
	ADD_STATE(State_PluginInUse(3, Test_IsPluginInUse(g_SHPtr, 3)));
	ADD_STATE(State_PluginInUse(4, Test_IsPluginInUse(g_SHPtr, 4)));

	CHECK_STATES((&g_States,
		new State_PluginInUse(1, true),
		new State_PluginInUse(2, false),
		new State_PluginInUse(3, false),
		new State_PluginInUse(4, false),
		NULL), "Part 2.4.2");

	Test_UnloadPlugin(g_SHPtr, 1);

	pInst->Func1();
	pInst->Func2();
	pInst->Func3();

	CHECK_STATES((&g_States,
		new State_Func1_Called,
		new State_Func2_Called,
		new State_Func3_Called,
		NULL), "Part 2.5.1");

	ADD_STATE(State_PluginInUse(1, Test_IsPluginInUse(g_SHPtr, 1)));
	ADD_STATE(State_PluginInUse(2, Test_IsPluginInUse(g_SHPtr, 2)));
	ADD_STATE(State_PluginInUse(3, Test_IsPluginInUse(g_SHPtr, 3)));
	ADD_STATE(State_PluginInUse(4, Test_IsPluginInUse(g_SHPtr, 4)));

	CHECK_STATES((&g_States,
		new State_PluginInUse(1, false),
		new State_PluginInUse(2, false),
		new State_PluginInUse(3, false),
		new State_PluginInUse(4, false),
		NULL), "Part 2.5.2");


	// 3)
	//   Add hooks from "different plugins", then pause the plugins

	g_PLID = 1;
	SH_ADD_HOOK(Test, Func1, pInst, SH_STATIC(Handler_Func1), false);
	g_PLID = 2;
	SH_ADD_HOOK(Test, Func2, pInst, SH_STATIC(Handler_Func2), true);
	g_PLID = 3;
	SH_ADD_HOOK(Test, Func3, pInst, SH_STATIC(Handler_Func3), false);

	g_PLID = 1;
	SH_ADD_HOOK(Test, Func2, pInst, SH_STATIC(Handler_Func2), true);
	SH_ADD_HOOK(Test, Func3, pInst, SH_STATIC(Handler_Func3), false);

	g_PLID = 2;
	SH_ADD_HOOK(Test, Func1, pInst, SH_STATIC(Handler_Func1), false);
	SH_ADD_HOOK(Test, Func3, pInst, SH_STATIC(Handler_Func3), false);

	g_PLID = 3;
	SH_ADD_HOOK(Test, Func1, pInst, SH_STATIC(Handler_Func1), false);
	SH_ADD_HOOK(Test, Func2, pInst, SH_STATIC(Handler_Func2), true);

	pInst->Func1();
	pInst->Func2();
	pInst->Func3();

	CHECK_STATES((&g_States,
		new State_Func1H_Called,
		new State_Func1H_Called,
		new State_Func1H_Called,
		new State_Func1_Called,
		new State_Func2_Called,
		new State_Func2H_Called,
		new State_Func2H_Called,
		new State_Func2H_Called,
		new State_Func3H_Called,
		new State_Func3H_Called,
		new State_Func3H_Called,
		new State_Func3_Called,
		NULL), "Part 3.1");

	ADD_STATE(State_PluginInUse(1, Test_IsPluginInUse(g_SHPtr, 1)));
	ADD_STATE(State_PluginInUse(2, Test_IsPluginInUse(g_SHPtr, 2)));
	ADD_STATE(State_PluginInUse(3, Test_IsPluginInUse(g_SHPtr, 3)));
	ADD_STATE(State_PluginInUse(4, Test_IsPluginInUse(g_SHPtr, 4)));

	CHECK_STATES((&g_States,
		new State_PluginInUse(1, true),
		new State_PluginInUse(2, true),
		new State_PluginInUse(3, true),
		new State_PluginInUse(4, false),
		NULL), "Part 3.2");

	// Unload plugins one by one
	Test_PausePlugin(g_SHPtr, 3);

	pInst->Func1();
	pInst->Func2();
	pInst->Func3();

	CHECK_STATES((&g_States,
		new State_Func1H_Called,
		new State_Func1H_Called,
		new State_Func1_Called,
		new State_Func2_Called,
		new State_Func2H_Called,
		new State_Func2H_Called,
		new State_Func3H_Called,
		new State_Func3H_Called,
		new State_Func3_Called,
		NULL), "Part 3.3.1");

	ADD_STATE(State_PluginInUse(1, Test_IsPluginInUse(g_SHPtr, 1)));
	ADD_STATE(State_PluginInUse(2, Test_IsPluginInUse(g_SHPtr, 2)));
	ADD_STATE(State_PluginInUse(3, Test_IsPluginInUse(g_SHPtr, 3)));
	ADD_STATE(State_PluginInUse(4, Test_IsPluginInUse(g_SHPtr, 4)));

	CHECK_STATES((&g_States,
		new State_PluginInUse(1, true),
		new State_PluginInUse(2, true),
		new State_PluginInUse(3, true),
		new State_PluginInUse(4, false),
		NULL), "Part 3.3.2");

	Test_PausePlugin(g_SHPtr, 2);

	pInst->Func1();
	pInst->Func2();
	pInst->Func3();

	CHECK_STATES((&g_States,
		new State_Func1H_Called,
		new State_Func1_Called,
		new State_Func2_Called,
		new State_Func2H_Called,
		new State_Func3H_Called,
		new State_Func3_Called,
		NULL), "Part 3.4.1");

	ADD_STATE(State_PluginInUse(1, Test_IsPluginInUse(g_SHPtr, 1)));
	ADD_STATE(State_PluginInUse(2, Test_IsPluginInUse(g_SHPtr, 2)));
	ADD_STATE(State_PluginInUse(3, Test_IsPluginInUse(g_SHPtr, 3)));
	ADD_STATE(State_PluginInUse(4, Test_IsPluginInUse(g_SHPtr, 4)));

	CHECK_STATES((&g_States,
		new State_PluginInUse(1, true),
		new State_PluginInUse(2, true),
		new State_PluginInUse(3, true),
		new State_PluginInUse(4, false),
		NULL), "Part 3.4.2");

	Test_PausePlugin(g_SHPtr, 1);

	pInst->Func1();
	pInst->Func2();
	pInst->Func3();

	CHECK_STATES((&g_States,
		new State_Func1_Called,
		new State_Func2_Called,
		new State_Func3_Called,
		NULL), "Part 3.5.1");

	ADD_STATE(State_PluginInUse(1, Test_IsPluginInUse(g_SHPtr, 1)));
	ADD_STATE(State_PluginInUse(2, Test_IsPluginInUse(g_SHPtr, 2)));
	ADD_STATE(State_PluginInUse(3, Test_IsPluginInUse(g_SHPtr, 3)));
	ADD_STATE(State_PluginInUse(4, Test_IsPluginInUse(g_SHPtr, 4)));

	CHECK_STATES((&g_States,
		new State_PluginInUse(1, true),
		new State_PluginInUse(2, true),
		new State_PluginInUse(3, true),
		new State_PluginInUse(4, false),
		NULL), "Part 3.5.2");

	Test_UnpausePlugin(g_SHPtr, 1);
	Test_UnpausePlugin(g_SHPtr, 2);
	Test_UnpausePlugin(g_SHPtr, 3);

	ADD_STATE(State_PluginInUse(1, Test_IsPluginInUse(g_SHPtr, 1)));
	ADD_STATE(State_PluginInUse(2, Test_IsPluginInUse(g_SHPtr, 2)));
	ADD_STATE(State_PluginInUse(3, Test_IsPluginInUse(g_SHPtr, 3)));
	ADD_STATE(State_PluginInUse(4, Test_IsPluginInUse(g_SHPtr, 4)));

	CHECK_STATES((&g_States,
		new State_PluginInUse(1, true),
		new State_PluginInUse(2, true),
		new State_PluginInUse(3, true),
		new State_PluginInUse(4, false),
		NULL), "Part 3.6");

	pInst->Func1();
	pInst->Func2();
	pInst->Func3();

	CHECK_STATES((&g_States,
		new State_Func1H_Called,
		new State_Func1H_Called,
		new State_Func1H_Called,
		new State_Func1_Called,
		new State_Func2_Called,
		new State_Func2H_Called,
		new State_Func2H_Called,
		new State_Func2H_Called,
		new State_Func3H_Called,
		new State_Func3H_Called,
		new State_Func3H_Called,
		new State_Func3_Called,
		NULL), "Part 3.7");

	// 4) Shutdown :)
	Test_CompleteShutdown(g_SHPtr);

	pInst->Func1();
	pInst->Func2();
	pInst->Func3();

	CHECK_STATES((&g_States,
		new State_Func1_Called,
		new State_Func2_Called,
		new State_Func3_Called,
		NULL), "Part 4");

	return true;
}
