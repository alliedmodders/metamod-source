#include "testevents.h"
#include "sourcehook_test.h"

// Tests support for functions which return references

namespace
{
	StateList g_States;
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

	MAKE_STATE_1(State_Func1_Pre1, int*);			// p1: the ref Func1_Pre1 is going to return
	MAKE_STATE_3(State_Func1_Pre2, META_RES, int*, int*);	// p1: current status
															// p2: override ret
															// p3: what this handler is going to supercede with
	MAKE_STATE_1(State_Func1, int*);				// p1: the ref Func1 is going to return
	MAKE_STATE_2(State_Func1_Post1, int*, int*);	// p1: orig_ret; p2: override_ret
	MAKE_STATE_1(State_Func1_Post2, int*);			// p1: what it's going to return
	MAKE_STATE_1(State_Func1_Ret, int*);			// p1: the ref it returned


	MAKE_STATE_2(State_Func2_Pre1, int, const int*);				// p1: func's p1; p2: what it's going to ret
	MAKE_STATE_2(State_Func2, int, const int*);						// p1: func's p1; p2: what it's going to ret
	MAKE_STATE_3(State_Func2_Post1, int, const int*, const int*);	// p1: func's p1; p2: orig ret; p3: override ret
	MAKE_STATE_1(State_Func2_Ret, const int*);						// p1: ret

	int g_Var;

	class Test
	{
	public:
		int m_Var1;
		int m_Var2;

		Test() : m_Var1(87)
		{
		}

		virtual int& Func1()
		{
			ADD_STATE(State_Func1(&m_Var1));
			return m_Var1;
		}
		virtual const int& Func2(int p1)
		{
			ADD_STATE(State_Func2(p1, &m_Var2));
			m_Var2 = p1;
			return m_Var2;
		}
	};

	class Whatever : public Test
	{
	};

	class CHook
	{
	public:
		int m_Var;
		virtual int& Func1_Pre1()
		{
			ADD_STATE(State_Func1_Pre1(&m_Var));
			RETURN_META_VALUE(MRES_OVERRIDE, m_Var);
		}
		virtual int &Func1_Pre2()
		{
			int &overrideret = META_RESULT_OVERRIDE_RET(int&);
			overrideret = 1337;
			ADD_STATE(State_Func1_Pre2(META_RESULT_STATUS, &overrideret, &g_Var));
			RETURN_META_VALUE(MRES_SUPERCEDE, g_Var);
		}
		virtual int& Func1_Post1()
		{
			ADD_STATE(State_Func1_Post1(&META_RESULT_ORIG_RET(int&), &META_RESULT_OVERRIDE_RET(int&)));
			RETURN_META_NOREF(MRES_IGNORED, int&);
		}
		virtual int& Func1_Post2()
		{
			ADD_STATE(State_Func1_Post2(&m_Var));
			RETURN_META_VALUE(MRES_OVERRIDE, m_Var);
		}

		virtual const int& Func2_Pre1(int p1)
		{
			ADD_STATE(State_Func2_Pre1(p1, &m_Var));
			RETURN_META_VALUE_NEWPARAMS(MRES_OVERRIDE, m_Var, static_cast<const int& (Test::*)(int)>(&Test::Func2), (1337));
		}

		virtual const int& Func2_Post1(int p1)
		{
			ADD_STATE(State_Func2_Post1(p1, &META_RESULT_ORIG_RET(int&), &META_RESULT_OVERRIDE_RET(int&)));
			RETURN_META_NOREF(MRES_IGNORED, const int&);
		}
	};

	SH_DECL_HOOK0(Test, Func1, SH_NOATTRIB, 0, int&);
	SH_DECL_HOOK1(Test, Func2, SH_NOATTRIB, 0, const int&, int);
}

bool TestRefRet(std::string &error)
{
	GET_SHPTR(g_SHPtr);
	g_PLID = 1;

	Whatever test;
	Test *pTest = &test;
	CHook hook;

	int &ret1 = pTest->Func1();
	ADD_STATE(State_Func1_Ret(&ret1));

	CHECK_STATES((&g_States,
		new State_Func1(&test.m_Var1),
		new State_Func1_Ret(&test.m_Var1),
		NULL), "Part 1");

	// Now add Func1_Pre1, which supercedes and returns hook.m_Var
	SH_ADD_HOOK(Test, Func1, &test, SH_MEMBER(&hook, &CHook::Func1_Pre1), false);

	int &ret2 = pTest->Func1();
	ADD_STATE(State_Func1_Ret(&ret2));
	
	CHECK_STATES((&g_States,
		new State_Func1_Pre1(&hook.m_Var),		// Pre1 says that it's going to override with hook.m_Var
		new State_Func1(&test.m_Var1),			// Function says that it's going to return test.m_Var1
		new State_Func1_Ret(&hook.m_Var),		// hook.m_Var is returned
		NULL), "Part 2");

	// Now add Func1_Post1, which only reports orig ret and override ret
	SH_ADD_HOOK(Test, Func1, &test, SH_MEMBER(&hook, &CHook::Func1_Post1), true);

	int &ret3 = pTest->Func1();
	ADD_STATE(State_Func1_Ret(&ret3));
	
	CHECK_STATES((&g_States,
		new State_Func1_Pre1(&hook.m_Var),		// Pre1 says that it's going to override with hook.m_Var
		new State_Func1(&test.m_Var1),			// Function says that it's going to return test.m_Var1
		new State_Func1_Post1(&test.m_Var1, &hook.m_Var),	// origret(=p1) is what it wanted to
															// return, overrideret(=p2) is pre1's var
		new State_Func1_Ret(&hook.m_Var),		// hook.m_Var is returned
		NULL), "Part 3");

	// Now add Func1_Pre2, which supercedes and returns g_Var (it also sets the override ret from pre1 to 1337)
	// and add Func1_Post2, which overrides and returns hook.m_Var again.

	SH_ADD_HOOK(Test, Func1, &test, SH_MEMBER(&hook, &CHook::Func1_Pre2), false);
	SH_ADD_HOOK(Test, Func1, &test, SH_MEMBER(&hook, &CHook::Func1_Post2), true);

	int &ret4 = pTest->Func1();
	ADD_STATE(State_Func1_Ret(&ret4));

	CHECK_STATES((&g_States,
		new State_Func1_Pre1(&hook.m_Var),		// Pre1 says that it's going to override with hook.m_Var
		new State_Func1_Pre2(MRES_OVERRIDE,			// current status
							&hook.m_Var,			// override ret (which it set to 1337)
							&g_Var),				// what it's going to ret, AND supercede with

		new State_Func1_Post1(&g_Var, &g_Var),	// origret(=p1) is what pre2 superceded with,
												//  so  overrideret(=p2) has to be the same
		new State_Func1_Post2(&hook.m_Var),			// post2 is going to override with hook.m_Var again
		new State_Func1_Ret(&hook.m_Var),		// ==>>> hook.m_Var is returned
		NULL), "Part 4");

	CHECK_COND(hook.m_Var == 1337, "Part 4.1");
	
	// Through a callclass
	int &ret5 = SH_CALL(pTest, &Test::Func1)();
	ADD_STATE(State_Func1_Ret(&ret5));

	CHECK_STATES((&g_States,
		new State_Func1(&test.m_Var1),
		new State_Func1_Ret(&test.m_Var1),
		NULL), "Part 5");


	////////////////////////////////////////////////////////////////////////////////////////
	// Func2 tests
	const int &ret21 = pTest->Func2(500);
	ADD_STATE(State_Func2_Ret(&ret21));

	CHECK_STATES((&g_States,
		new State_Func2(500, &test.m_Var2),
		new State_Func2_Ret(&test.m_Var2),
		NULL), "Part 6");

	SH_ADD_HOOK(Test, Func2, &test, SH_MEMBER(&hook, &CHook::Func2_Pre1), false);
	SH_ADD_HOOK(Test, Func2, &test, SH_MEMBER(&hook, &CHook::Func2_Post1), true);

	const int &ret22 = pTest->Func2(500);
	ADD_STATE(State_Func2_Ret(&ret22));

	CHECK_STATES((&g_States,
		new State_Func2_Pre1(500, &hook.m_Var),		// p1 was 500; it's going to override with hook.m_Var; and also change p1 to 1337
		new State_Func2(1337, &test.m_Var2),		// p1 was 1337; it's going to ret test.m_Var2
		new State_Func2_Post1(1337,					// p1 was 1337
			&test.m_Var2,							// orig ret was test.m_Var2
			&hook.m_Var),							// override ret was hook.m_Var
		new State_Func2_Ret(&hook.m_Var),			// really returned hook.m_Var
		NULL), "Part 7");
	
   	return true;
}
