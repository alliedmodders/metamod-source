#include <string>
#include "sourcehook_impl.h"
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
	SourceHook::CSourceHookImpl g_SHImpl;
	g_SHPtr = &g_SHImpl;
	g_PLID = 1;

	Test inst;
	Test *pInst = &inst;

	// 1)
	//   Add hooks, then issue a complete shutdown
	SH_ADD_HOOK_STATICFUNC(Test, Func1, pInst, Handler_Func1, false);
	SH_ADD_HOOK_STATICFUNC(Test, Func2, pInst, Handler_Func2, true);
	SH_ADD_HOOK_STATICFUNC(Test, Func3, pInst, Handler_Func3, false);

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

	g_SHImpl.CompleteShutdown();

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
	SH_ADD_HOOK_STATICFUNC(Test, Func1, pInst, Handler_Func1, false);
	g_PLID = 2;
	SH_ADD_HOOK_STATICFUNC(Test, Func2, pInst, Handler_Func2, true);
	g_PLID = 3;
	SH_ADD_HOOK_STATICFUNC(Test, Func3, pInst, Handler_Func3, false);

	g_PLID = 1;
	SH_ADD_HOOK_STATICFUNC(Test, Func2, pInst, Handler_Func2, true);
	SH_ADD_HOOK_STATICFUNC(Test, Func3, pInst, Handler_Func3, false);

	g_PLID = 2;
	SH_ADD_HOOK_STATICFUNC(Test, Func1, pInst, Handler_Func1, false);
	SH_ADD_HOOK_STATICFUNC(Test, Func3, pInst, Handler_Func3, false);

	g_PLID = 3;
	SH_ADD_HOOK_STATICFUNC(Test, Func1, pInst, Handler_Func1, false);
	SH_ADD_HOOK_STATICFUNC(Test, Func2, pInst, Handler_Func2, true);

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

	ADD_STATE(State_PluginInUse(1, g_SHImpl.IsPluginInUse(1)));
	ADD_STATE(State_PluginInUse(2, g_SHImpl.IsPluginInUse(2)));
	ADD_STATE(State_PluginInUse(3, g_SHImpl.IsPluginInUse(3)));
	ADD_STATE(State_PluginInUse(4, g_SHImpl.IsPluginInUse(4)));

	CHECK_STATES((&g_States,
		new State_PluginInUse(1, true),
		new State_PluginInUse(2, true),
		new State_PluginInUse(3, true),
		new State_PluginInUse(4, false),
		NULL), "Part 2.2");

	// Unload plugins one by one
	g_SHImpl.UnloadPlugin(3);

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

	ADD_STATE(State_PluginInUse(1, g_SHImpl.IsPluginInUse(1)));
	ADD_STATE(State_PluginInUse(2, g_SHImpl.IsPluginInUse(2)));
	ADD_STATE(State_PluginInUse(3, g_SHImpl.IsPluginInUse(3)));
	ADD_STATE(State_PluginInUse(4, g_SHImpl.IsPluginInUse(4)));

	CHECK_STATES((&g_States,
		new State_PluginInUse(1, true),
		new State_PluginInUse(2, true),
		new State_PluginInUse(3, false),
		new State_PluginInUse(4, false),
		NULL), "Part 2.3.2");

	g_SHImpl.UnloadPlugin(2);

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

	ADD_STATE(State_PluginInUse(1, g_SHImpl.IsPluginInUse(1)));
	ADD_STATE(State_PluginInUse(2, g_SHImpl.IsPluginInUse(2)));
	ADD_STATE(State_PluginInUse(3, g_SHImpl.IsPluginInUse(3)));
	ADD_STATE(State_PluginInUse(4, g_SHImpl.IsPluginInUse(4)));

	CHECK_STATES((&g_States,
		new State_PluginInUse(1, true),
		new State_PluginInUse(2, false),
		new State_PluginInUse(3, false),
		new State_PluginInUse(4, false),
		NULL), "Part 2.4.2");

	g_SHImpl.UnloadPlugin(1);

	pInst->Func1();
	pInst->Func2();
	pInst->Func3();

	CHECK_STATES((&g_States,
		new State_Func1_Called,
		new State_Func2_Called,
		new State_Func3_Called,
		NULL), "Part 2.5.1");

	ADD_STATE(State_PluginInUse(1, g_SHImpl.IsPluginInUse(1)));
	ADD_STATE(State_PluginInUse(2, g_SHImpl.IsPluginInUse(2)));
	ADD_STATE(State_PluginInUse(3, g_SHImpl.IsPluginInUse(3)));
	ADD_STATE(State_PluginInUse(4, g_SHImpl.IsPluginInUse(4)));

	CHECK_STATES((&g_States,
		new State_PluginInUse(1, false),
		new State_PluginInUse(2, false),
		new State_PluginInUse(3, false),
		new State_PluginInUse(4, false),
		NULL), "Part 2.5.2");


	// 3)
	//   Add hooks from "different plugins", then pause the plugins

	g_PLID = 1;
	SH_ADD_HOOK_STATICFUNC(Test, Func1, pInst, Handler_Func1, false);
	g_PLID = 2;
	SH_ADD_HOOK_STATICFUNC(Test, Func2, pInst, Handler_Func2, true);
	g_PLID = 3;
	SH_ADD_HOOK_STATICFUNC(Test, Func3, pInst, Handler_Func3, false);

	g_PLID = 1;
	SH_ADD_HOOK_STATICFUNC(Test, Func2, pInst, Handler_Func2, true);
	SH_ADD_HOOK_STATICFUNC(Test, Func3, pInst, Handler_Func3, false);

	g_PLID = 2;
	SH_ADD_HOOK_STATICFUNC(Test, Func1, pInst, Handler_Func1, false);
	SH_ADD_HOOK_STATICFUNC(Test, Func3, pInst, Handler_Func3, false);

	g_PLID = 3;
	SH_ADD_HOOK_STATICFUNC(Test, Func1, pInst, Handler_Func1, false);
	SH_ADD_HOOK_STATICFUNC(Test, Func2, pInst, Handler_Func2, true);

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

	ADD_STATE(State_PluginInUse(1, g_SHImpl.IsPluginInUse(1)));
	ADD_STATE(State_PluginInUse(2, g_SHImpl.IsPluginInUse(2)));
	ADD_STATE(State_PluginInUse(3, g_SHImpl.IsPluginInUse(3)));
	ADD_STATE(State_PluginInUse(4, g_SHImpl.IsPluginInUse(4)));

	CHECK_STATES((&g_States,
		new State_PluginInUse(1, true),
		new State_PluginInUse(2, true),
		new State_PluginInUse(3, true),
		new State_PluginInUse(4, false),
		NULL), "Part 3.2");

	// Unload plugins one by one
	g_SHImpl.PausePlugin(3);

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

	ADD_STATE(State_PluginInUse(1, g_SHImpl.IsPluginInUse(1)));
	ADD_STATE(State_PluginInUse(2, g_SHImpl.IsPluginInUse(2)));
	ADD_STATE(State_PluginInUse(3, g_SHImpl.IsPluginInUse(3)));
	ADD_STATE(State_PluginInUse(4, g_SHImpl.IsPluginInUse(4)));

	CHECK_STATES((&g_States,
		new State_PluginInUse(1, true),
		new State_PluginInUse(2, true),
		new State_PluginInUse(3, true),
		new State_PluginInUse(4, false),
		NULL), "Part 3.3.2");

	g_SHImpl.PausePlugin(2);

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

	ADD_STATE(State_PluginInUse(1, g_SHImpl.IsPluginInUse(1)));
	ADD_STATE(State_PluginInUse(2, g_SHImpl.IsPluginInUse(2)));
	ADD_STATE(State_PluginInUse(3, g_SHImpl.IsPluginInUse(3)));
	ADD_STATE(State_PluginInUse(4, g_SHImpl.IsPluginInUse(4)));

	CHECK_STATES((&g_States,
		new State_PluginInUse(1, true),
		new State_PluginInUse(2, true),
		new State_PluginInUse(3, true),
		new State_PluginInUse(4, false),
		NULL), "Part 3.4.2");

	g_SHImpl.PausePlugin(1);

	pInst->Func1();
	pInst->Func2();
	pInst->Func3();

	CHECK_STATES((&g_States,
		new State_Func1_Called,
		new State_Func2_Called,
		new State_Func3_Called,
		NULL), "Part 3.5.1");

	ADD_STATE(State_PluginInUse(1, g_SHImpl.IsPluginInUse(1)));
	ADD_STATE(State_PluginInUse(2, g_SHImpl.IsPluginInUse(2)));
	ADD_STATE(State_PluginInUse(3, g_SHImpl.IsPluginInUse(3)));
	ADD_STATE(State_PluginInUse(4, g_SHImpl.IsPluginInUse(4)));

	CHECK_STATES((&g_States,
		new State_PluginInUse(1, true),
		new State_PluginInUse(2, true),
		new State_PluginInUse(3, true),
		new State_PluginInUse(4, false),
		NULL), "Part 3.5.2");

	g_SHImpl.UnpausePlugin(1);
	g_SHImpl.UnpausePlugin(2);
	g_SHImpl.UnpausePlugin(3);

	ADD_STATE(State_PluginInUse(1, g_SHImpl.IsPluginInUse(1)));
	ADD_STATE(State_PluginInUse(2, g_SHImpl.IsPluginInUse(2)));
	ADD_STATE(State_PluginInUse(3, g_SHImpl.IsPluginInUse(3)));
	ADD_STATE(State_PluginInUse(4, g_SHImpl.IsPluginInUse(4)));

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
	g_SHImpl.CompleteShutdown();

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

