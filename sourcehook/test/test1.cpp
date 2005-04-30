#include <string>
#include "sourcehook_impl.h"
#include "testevents.h"

namespace
{
	StateList g_States;
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

	// TEST1
	// Basic tests
	// Hooking and callclass

	struct State_F1_Called : State
	{
	};

	struct State_F1_PreHandler_Called : State
	{
	};

	struct State_F1_PostHandler_Called : State
	{
	};

	struct State_F1_HookAdded : State
	{
		bool m_Successfully;
		State_F1_HookAdded(bool successfully) : m_Successfully(successfully)
		{
		}

		bool IsEqual(State *other)
		{
			State_F1_HookAdded *other2 = dynamic_cast<State_F1_HookAdded*>(other);
			if (!other2)
				return false;
			return other2->m_Successfully == m_Successfully;
		}
	};

	struct State_F1_HookRemoved : State
	{
	};

	struct State_F1_CallClassGenerated : State
	{
	};

	struct State_F1_CallClassReleased : State
	{
	};

	struct State_F299_Called : State
	{
		std::string param;
		State_F299_Called(const char *p) : param(p)
		{
		}
		bool IsEqual(State *other)
		{
			State_F299_Called *other2 = dynamic_cast<State_F299_Called*>(other);
			if (!other2)
				return false;
			return other2->param == param;
		}
	};

	struct State_F299_PreHandlerCalled : State_F299_Called
	{
		State_F299_PreHandlerCalled(const char *p) : State_F299_Called(p)
		{
		}
	};

	struct State_F299_PostHandlerCalled : State_F299_Called
	{
		State_F299_PostHandlerCalled(const char *p) : State_F299_Called(p)
		{
		}
	};

	struct State_F299Ret : State
	{
		bool m_Ret;
		State_F299Ret(bool ret) : m_Ret(ret)
		{
		}

		bool IsEqual(State *other)
		{
			State_F299Ret *other2 = dynamic_cast<State_F299Ret*>(other);
			if (!other2)
				return false;
			return other2->m_Ret == m_Ret;
		}
	};

	class Test
	{
	public:
		virtual void F1()
		{
			ADD_STATE(State_F1_Called);
		}
		virtual void F2(){}
		virtual void F3(){}
		virtual void F4(){}
		virtual void F5(){}
		virtual void F6(){}
		virtual void F7(){}
		virtual void F8(){}
		virtual void F9(){}
		virtual void F10(){}
		virtual void F11(){}
		virtual void F12(){}
		virtual void F13(){}
		virtual void F14(){}
		virtual void F15(){}
		virtual void F16(){}
		virtual void F17(){}
		virtual void F18(){}
		virtual void F19(){}
		virtual void F20(){}
		virtual void F21(){}
		virtual void F22(){}
		virtual void F23(){}
		virtual void F24(){}
		virtual void F25(){}
		virtual void F26(){}
		virtual void F27(){}
		virtual void F28(){}
		virtual void F29(){}
		virtual void F30(){}
		virtual void F31(){}
		virtual void F32(){}
		virtual void F33(){}
		virtual void F34(){}
		virtual void F35(){}
		virtual void F36(){}
		virtual void F37(){}
		virtual void F38(){}
		virtual void F39(){}
		virtual void F40(){}
		virtual void F41(){}
		virtual void F42(){}
		virtual void F43(){}
		virtual void F44(){}
		virtual void F45(){}
		virtual void F46(){}
		virtual void F47(){}
		virtual void F48(){}
		virtual void F49(){}
		virtual void F50(){}
		virtual void F51(){}
		virtual void F52(){}
		virtual void F53(){}
		virtual void F54(){}
		virtual void F55(){}
		virtual void F56(){}
		virtual void F57(){}
		virtual void F58(){}
		virtual void F59(){}
		virtual void F60(){}
		virtual void F61(){}
		virtual void F62(){}
		virtual void F63(){}
		virtual void F64(){}
		virtual void F65(){}
		virtual void F66(){}
		virtual void F67(){}
		virtual void F68(){}
		virtual void F69(){}
		virtual void F70(){}
		virtual void F71(){}
		virtual void F72(){}
		virtual void F73(){}
		virtual void F74(){}
		virtual void F75(){}
		virtual void F76(){}
		virtual void F77(){}
		virtual void F78(){}
		virtual void F79(){}
		virtual void F80(){}
		virtual void F81(){}
		virtual void F82(){}
		virtual void F83(){}
		virtual void F84(){}
		virtual void F85(){}
		virtual void F86(){}
		virtual void F87(){}
		virtual void F88(){}
		virtual void F89(){}
		virtual void F90(){}
		virtual void F91(){}
		virtual void F92(){}
		virtual void F93(){}
		virtual void F94(){}
		virtual void F95(){}
		virtual void F96(){}
		virtual void F97(){}
		virtual void F98(){}
		virtual void F99(){}
		virtual void F100(){}
		virtual void F101(){}
		virtual void F102(){}
		virtual void F103(){}
		virtual void F104(){}
		virtual void F105(){}
		virtual void F106(){}
		virtual void F107(){}
		virtual void F108(){}
		virtual void F109(){}
		virtual void F110(){}
		virtual void F111(){}
		virtual void F112(){}
		virtual void F113(){}
		virtual void F114(){}
		virtual void F115(){}
		virtual void F116(){}
		virtual void F117(){}
		virtual void F118(){}
		virtual void F119(){}
		virtual void F120(){}
		virtual void F121(){}
		virtual void F122(){}
		virtual void F123(){}
		virtual void F124(){}
		virtual void F125(){}
		virtual void F126(){}
		virtual void F127(){}
		virtual void F128(){}
		virtual void F129(){}
		virtual void F130(){}
		virtual void F131(){}
		virtual void F132(){}
		virtual void F133(){}
		virtual void F134(){}
		virtual void F135(){}
		virtual void F136(){}
		virtual void F137(){}
		virtual void F138(){}
		virtual void F139(){}
		virtual void F140(){}
		virtual void F141(){}
		virtual void F142(){}
		virtual void F143(){}
		virtual void F144(){}
		virtual void F145(){}
		virtual void F146(){}
		virtual void F147(){}
		virtual void F148(){}
		virtual void F149(){}
		virtual void F150(){}
		virtual void F151(){}
		virtual void F152(){}
		virtual void F153(){}
		virtual void F154(){}
		virtual void F155(){}
		virtual void F156(){}
		virtual void F157(){}
		virtual void F158(){}
		virtual void F159(){}
		virtual void F160(){}
		virtual void F161(){}
		virtual void F162(){}
		virtual void F163(){}
		virtual void F164(){}
		virtual void F165(){}
		virtual void F166(){}
		virtual void F167(){}
		virtual void F168(){}
		virtual void F169(){}
		virtual void F170(){}
		virtual void F171(){}
		virtual void F172(){}
		virtual void F173(){}
		virtual void F174(){}
		virtual void F175(){}
		virtual void F176(){}
		virtual void F177(){}
		virtual void F178(){}
		virtual void F179(){}
		virtual void F180(){}
		virtual void F181(){}
		virtual void F182(){}
		virtual void F183(){}
		virtual void F184(){}
		virtual void F185(){}
		virtual void F186(){}
		virtual void F187(){}
		virtual void F188(){}
		virtual void F189(){}
		virtual void F190(){}
		virtual void F191(){}
		virtual void F192(){}
		virtual void F193(){}
		virtual void F194(){}
		virtual void F195(){}
		virtual void F196(){}
		virtual void F197(){}
		virtual void F198(){}
		virtual void F199(){}
		virtual void F200(){}
		virtual void F201(){}
		virtual void F202(){}
		virtual void F203(){}
		virtual void F204(){}
		virtual void F205(){}
		virtual void F206(){}
		virtual void F207(){}
		virtual void F208(){}
		virtual void F209(){}
		virtual void F210(){}
		virtual void F211(){}
		virtual void F212(){}
		virtual void F213(){}
		virtual void F214(){}
		virtual void F215(){}
		virtual void F216(){}
		virtual void F217(){}
		virtual void F218(){}
		virtual void F219(){}
		virtual void F220(){}
		virtual void F221(){}
		virtual void F222(){}
		virtual void F223(){}
		virtual void F224(){}
		virtual void F225(){}
		virtual void F226(){}
		virtual void F227(){}
		virtual void F228(){}
		virtual void F229(){}
		virtual void F230(){}
		virtual void F231(){}
		virtual void F232(){}
		virtual void F233(){}
		virtual void F234(){}
		virtual void F235(){}
		virtual void F236(){}
		virtual void F237(){}
		virtual void F238(){}
		virtual void F239(){}
		virtual void F240(){}
		virtual void F241(){}
		virtual void F242(){}
		virtual void F243(){}
		virtual void F244(){}
		virtual void F245(){}
		virtual void F246(){}
		virtual void F247(){}
		virtual void F248(){}
		virtual void F249(){}
		virtual void F250(){}
		virtual void F251(){}
		virtual void F252(){}
		virtual void F253(){}
		virtual void F254(){}
		virtual void F255(){}
		virtual void F256(){}
		virtual void F257(){}
		virtual void F258(){}
		virtual void F259(){}
		virtual void F260(){}
		virtual void F261(){}
		virtual void F262(){}
		virtual void F263(){}
		virtual void F264(){}
		virtual void F265(){}
		virtual void F266(){}
		virtual void F267(){}
		virtual void F268(){}
		virtual void F269(){}
		virtual void F270(){}
		virtual void F271(){}
		virtual void F272(){}
		virtual void F273(){}
		virtual void F274(){}
		virtual void F275(){}
		virtual void F276(){}
		virtual void F277(){}
		virtual void F278(){}
		virtual void F279(){}
		virtual void F280(){}
		virtual void F281(){}
		virtual void F282(){}
		virtual void F283(){}
		virtual void F284(){}
		virtual void F285(){}
		virtual void F286(){}
		virtual void F287(){}
		virtual void F288(){}
		virtual void F289(){}
		virtual void F290(){}
		virtual void F291(){}
		virtual void F292(){}
		virtual void F293(){}
		virtual void F294(){}
		virtual void F295(){}
		virtual void F296(){}
		virtual void F297(){}
		virtual void F298(){}
		virtual bool F299(const char *mwah)
		{
			ADD_STATE(State_F299_Called(mwah));
			return true;
		}
	};

	SH_DECL_HOOK1(Test, F299, SH_NOATTRIB, 0, bool, const char *);
	SH_DECL_HOOK0_void(Test, F1, SH_NOATTRIB, 0);
	SH_DECL_HOOK0_void(Test, F2, SH_NOATTRIB, 0);
	SH_DECL_HOOK0_void(Test, F3, SH_NOATTRIB, 0);
	SH_DECL_HOOK0_void(Test, F4, SH_NOATTRIB, 0);
	SH_DECL_HOOK0_void(Test, F5, SH_NOATTRIB, 0);
	SH_DECL_HOOK0_void(Test, F6, SH_NOATTRIB, 0);
	SH_DECL_HOOK0_void(Test, F7, SH_NOATTRIB, 0);
	SH_DECL_HOOK0_void(Test, F8, SH_NOATTRIB, 0);
	SH_DECL_HOOK0_void(Test, F9, SH_NOATTRIB, 0);
	SH_DECL_HOOK0_void(Test, F10, SH_NOATTRIB, 0);

	META_RES g_F1Pre_WhatToDo;
	META_RES g_F1Post_WhatToDo;

	struct HandlersF1
	{
		void Pre()
		{
			ADD_STATE(State_F1_PreHandler_Called);
			RETURN_META(g_F1Pre_WhatToDo);
		}

		void Post()
		{
			ADD_STATE(State_F1_PostHandler_Called);
			RETURN_META(g_F1Post_WhatToDo);
		}
	};

	META_RES g_F299Pre_WhatToDo;
	bool g_F299Pre_WhatToRet;

	bool F299_Pre(const char *mwah)
	{
		ADD_STATE(State_F299_PreHandlerCalled(mwah));
		RETURN_META_VALUE(g_F299Pre_WhatToDo, g_F299Pre_WhatToRet);
	}

	bool F299_Post(const char *mwah)
	{
		ADD_STATE(State_F299_PostHandlerCalled(mwah));
		RETURN_META_VALUE(MRES_OVERRIDE, META_RESULT_STATUS >= MRES_OVERRIDE ? !META_RESULT_OVERRIDE_RET(bool) :
			!META_RESULT_ORIG_RET(bool));
	}

}

bool TestBasic(std::string &error)
{
	SourceHook::CSourceHookImpl g_SHImpl;
	g_SHPtr = &g_SHImpl;
	g_PLID = 1337;

	HandlersF1 f1_handlers;
	Test test;
	Test *pTest = &test;

	// 1) Get a call class and call the member through it and normally
	SourceHook::CallClass<Test> *cc = SH_GET_CALLCLASS(pTest);
	ADD_STATE(State_F1_CallClassGenerated);

	SH_CALL(cc, &Test::F1)();
	pTest->F1();

	CHECK_STATES((&g_States, 
		new State_F1_CallClassGenerated,
		new State_F1_Called,
		new State_F1_Called,
		NULL), "Part 1");

	// 2) Request a call class again
	SourceHook::CallClass<Test> *cc2 = SH_GET_CALLCLASS(pTest);
	ADD_STATE(State_F1_CallClassGenerated);

	SH_CALL(cc, &Test::F1)();
	SH_CALL(cc2, &Test::F1)();
	pTest->F1();

	SH_RELEASE_CALLCLASS(cc2);
	ADD_STATE(State_F1_CallClassReleased);

	SH_CALL(cc, &Test::F1)();
	pTest->F1();

	CHECK_STATES((&g_States, 
		new State_F1_CallClassGenerated,
		new State_F1_Called,
		new State_F1_Called,
		new State_F1_Called,
		new State_F1_CallClassReleased,
		new State_F1_Called,
		new State_F1_Called,
		NULL), "Part 2");

	// 3) Add a pre hook
	g_F1Pre_WhatToDo = MRES_SUPERCEDE;
	ADD_STATE(State_F1_HookAdded(SH_ADD_HOOK_MEMFUNC(Test, F1, pTest, &f1_handlers, &HandlersF1::Pre, false)));

	SH_CALL(cc, &Test::F1)();
	pTest->F1();
	
	CHECK_STATES((&g_States, 
		new State_F1_HookAdded(true),
		new State_F1_Called,
		new State_F1_PreHandler_Called,
		NULL), "Part 3");

	// 4) Rerequest the callclass
	SH_RELEASE_CALLCLASS(cc);
	ADD_STATE(State_F1_CallClassReleased);
	cc2 = SH_GET_CALLCLASS(pTest);
	ADD_STATE(State_F1_CallClassGenerated);

	SH_CALL(cc, &Test::F1)();
	pTest->F1();

	CHECK_STATES((&g_States, 
		new State_F1_CallClassReleased,
		new State_F1_CallClassGenerated,
		new State_F1_Called,
		new State_F1_PreHandler_Called, 
		NULL), "Part 4");

	// 5) Check ignore / supercede
	g_F1Pre_WhatToDo = MRES_SUPERCEDE;
	SH_CALL(cc, &Test::F1)();
	pTest->F1();

	g_F1Pre_WhatToDo = MRES_IGNORED;
	SH_CALL(cc, &Test::F1)();
	pTest->F1();

	CHECK_STATES((&g_States,
		new State_F1_Called,
		new State_F1_PreHandler_Called,
		new State_F1_Called,
		new State_F1_PreHandler_Called,
		new State_F1_Called,
		NULL), "Part 5");

	// 6) remove the hook again
	SH_REMOVE_HOOK_MEMFUNC(Test, F1, pTest, &f1_handlers, &HandlersF1::Pre, false);
	ADD_STATE(State_F1_HookRemoved);

	SH_CALL(cc, &Test::F1)();
	pTest->F1();

	CHECK_STATES((&g_States, 
		new State_F1_HookRemoved,
		new State_F1_Called,
		new State_F1_Called,
		NULL), "Part 6");

	// 7) add a post hook now
	g_F1Post_WhatToDo = MRES_IGNORED;
	ADD_STATE(State_F1_HookAdded(SH_ADD_HOOK_MEMFUNC(Test, F1, pTest, &f1_handlers, &HandlersF1::Post, true)));

	SH_CALL(cc, &Test::F1)();
	pTest->F1();

	CHECK_STATES((&g_States, 
		new State_F1_HookAdded(true),
		new State_F1_Called,
		new State_F1_Called,
		new State_F1_PostHandler_Called,
		NULL), "Part 7");

	// 8) And a pre hook again
	g_F1Pre_WhatToDo = MRES_IGNORED;
	ADD_STATE(State_F1_HookAdded(SH_ADD_HOOK_MEMFUNC(Test, F1, pTest, &f1_handlers, &HandlersF1::Pre, false)));

	SH_CALL(cc, &Test::F1)();
	pTest->F1();

	g_F1Pre_WhatToDo = MRES_SUPERCEDE;
	SH_CALL(cc, &Test::F1)();
	pTest->F1();

	CHECK_STATES((&g_States, 
		new State_F1_HookAdded(true),
		new State_F1_Called,
		new State_F1_PreHandler_Called,
		new State_F1_Called,
		new State_F1_PostHandler_Called,
		new State_F1_Called,
		new State_F1_PreHandler_Called,
		new State_F1_PostHandler_Called,
		NULL), "Part 8");

	// 9) Remove all hooks
	SH_REMOVE_HOOK_MEMFUNC(Test, F1, pTest, &f1_handlers, &HandlersF1::Pre, false);
	ADD_STATE(State_F1_HookRemoved);
	SH_REMOVE_HOOK_MEMFUNC(Test, F1, pTest, &f1_handlers, &HandlersF1::Post, true);
	ADD_STATE(State_F1_HookRemoved);

	SH_CALL(cc, &Test::F1)();
	pTest->F1();

	CHECK_STATES((&g_States, 
		new State_F1_HookRemoved,
		new State_F1_HookRemoved,
		new State_F1_Called,
		new State_F1_Called,
		NULL), "Part 9");

	// 10) Some checks on F299
	g_F299Pre_WhatToDo = MRES_IGNORED;
	g_F299Pre_WhatToRet = false;

	ADD_STATE(State_F299Ret(pTest->F299("hi")));
	ADD_STATE(State_F299Ret(SH_CALL(cc, &Test::F299)("hi")));

	CHECK_STATES((&g_States, 
		new State_F299_Called("hi"),
		new State_F299Ret(true),
		new State_F299_Called("hi"),
		new State_F299Ret(true),
		NULL), "Part 10.1");

	SH_ADD_HOOK_STATICFUNC(Test, F299, pTest, F299_Pre, false);
	ADD_STATE(State_F299Ret(pTest->F299("hi")));
	ADD_STATE(State_F299Ret(SH_CALL(cc, &Test::F299)("hi")));

	CHECK_STATES((&g_States, 
		new State_F299_PreHandlerCalled("hi"),
		new State_F299_Called("hi"),
		new State_F299Ret(true),
		new State_F299_Called("hi"),
		new State_F299Ret(true),
		NULL), "Part 10.2");

	SH_ADD_HOOK_STATICFUNC(Test, F299, pTest, F299_Post, true);
	ADD_STATE(State_F299Ret(pTest->F299("hi")));
	ADD_STATE(State_F299Ret(SH_CALL(cc, &Test::F299)("hi")));

	CHECK_STATES((&g_States, 
		new State_F299_PreHandlerCalled("hi"),
		new State_F299_Called("hi"),
		new State_F299_PostHandlerCalled("hi"),
		new State_F299Ret(false),
		new State_F299_Called("hi"),
		new State_F299Ret(true),
		NULL), "Part 10.3");

	g_F299Pre_WhatToDo = MRES_OVERRIDE;
	ADD_STATE(State_F299Ret(pTest->F299("hi")));
	ADD_STATE(State_F299Ret(SH_CALL(cc, &Test::F299)("hi")));

	CHECK_STATES((&g_States, 
		new State_F299_PreHandlerCalled("hi"),
		new State_F299_Called("hi"),
		new State_F299_PostHandlerCalled("hi"),
		new State_F299Ret(true),
		new State_F299_Called("hi"),
		new State_F299Ret(true),
		NULL), "Part 10.4");

	g_F299Pre_WhatToDo = MRES_SUPERCEDE;
	ADD_STATE(State_F299Ret(pTest->F299("hi")));
	ADD_STATE(State_F299Ret(SH_CALL(cc, &Test::F299)("hi")));

	CHECK_STATES((&g_States, 
		new State_F299_PreHandlerCalled("hi"),
		new State_F299_PostHandlerCalled("hi"),
		new State_F299Ret(true),
		new State_F299_Called("hi"),
		new State_F299Ret(true),
		NULL), "Part 10.5");

	SH_REMOVE_HOOK_STATICFUNC(Test, F299, pTest, F299_Pre, false);
	ADD_STATE(State_F299Ret(pTest->F299("hi")));
	ADD_STATE(State_F299Ret(SH_CALL(cc, &Test::F299)("hi")));

	CHECK_STATES((&g_States, 
		new State_F299_Called("hi"),
		new State_F299_PostHandlerCalled("hi"),
		new State_F299Ret(false),
		new State_F299_Called("hi"),
		new State_F299Ret(true),
		NULL), "Part 10.6");

	SH_REMOVE_HOOK_STATICFUNC(Test, F299, pTest, F299_Post, true);
	ADD_STATE(State_F299Ret(pTest->F299("hi")));
	ADD_STATE(State_F299Ret(SH_CALL(cc, &Test::F299)("hi")));

	CHECK_STATES((&g_States, 
		new State_F299_Called("hi"),
		new State_F299Ret(true),
		new State_F299_Called("hi"),
		new State_F299Ret(true),
		NULL), "Part 10.7");

	// 11) Release callclass
	SH_RELEASE_CALLCLASS(cc);
	ADD_STATE(State_F1_CallClassReleased);


	CHECK_STATES((&g_States, 
		new State_F1_CallClassReleased,
		NULL), "Part 11");

	// 12) Test? Test.
	SH_ADD_HOOK_MEMFUNC(Test, F1, pTest, &f1_handlers, &HandlersF1::Pre, true);
	SH_ADD_HOOK_MEMFUNC(Test, F2, pTest, &f1_handlers, &HandlersF1::Pre, true);
	SH_ADD_HOOK_MEMFUNC(Test, F3, pTest, &f1_handlers, &HandlersF1::Pre, false);
	SH_ADD_HOOK_MEMFUNC(Test, F4, pTest, &f1_handlers, &HandlersF1::Pre, true);
	SH_ADD_HOOK_MEMFUNC(Test, F5, pTest, &f1_handlers, &HandlersF1::Pre, false);
	SH_ADD_HOOK_MEMFUNC(Test, F6, pTest, &f1_handlers, &HandlersF1::Pre, true);
	SH_ADD_HOOK_MEMFUNC(Test, F7, pTest, &f1_handlers, &HandlersF1::Pre, false);
	SH_ADD_HOOK_MEMFUNC(Test, F8, pTest, &f1_handlers, &HandlersF1::Pre, false);
	SH_ADD_HOOK_MEMFUNC(Test, F9, pTest, &f1_handlers, &HandlersF1::Pre, false);
	SH_ADD_HOOK_MEMFUNC(Test, F10, pTest, &f1_handlers, &HandlersF1::Pre, false);

	SH_REMOVE_HOOK_MEMFUNC(Test, F1, pTest, &f1_handlers, &HandlersF1::Pre, true);
	SH_REMOVE_HOOK_MEMFUNC(Test, F2, pTest, &f1_handlers, &HandlersF1::Pre, true);
	SH_REMOVE_HOOK_MEMFUNC(Test, F3, pTest, &f1_handlers, &HandlersF1::Pre, false);
	SH_REMOVE_HOOK_MEMFUNC(Test, F4, pTest, &f1_handlers, &HandlersF1::Pre, true);
	SH_REMOVE_HOOK_MEMFUNC(Test, F5, pTest, &f1_handlers, &HandlersF1::Pre, false);
	SH_REMOVE_HOOK_MEMFUNC(Test, F6, pTest, &f1_handlers, &HandlersF1::Pre, true);
	SH_REMOVE_HOOK_MEMFUNC(Test, F7, pTest, &f1_handlers, &HandlersF1::Pre, false);
	SH_REMOVE_HOOK_MEMFUNC(Test, F8, pTest, &f1_handlers, &HandlersF1::Pre, false);
	SH_REMOVE_HOOK_MEMFUNC(Test, F9, pTest, &f1_handlers, &HandlersF1::Pre, false);
	SH_REMOVE_HOOK_MEMFUNC(Test, F10, pTest, &f1_handlers, &HandlersF1::Pre, false);
	return true;
}