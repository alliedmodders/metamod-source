#include "testevents.h"
#include "sourcehook_test.h"

// References and private copy constructors and stuff like that!

/*
BAILOPAN, 29.04.2006 21:51 :
onoto-san, interesting error from VS
we have this function:
void GabHaben(ISomething &sth);

SomethingDerived sth;
and SH_CALL(cc, &WHAT::GabHaben)(sth);
"No copy constructor available for class 'SomethingDerived'"
*/


namespace
{
	StateList g_States;
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

	MAKE_STATE_1(State_Result, int);
	MAKE_STATE_1(State_Result_InHook, int);

	MAKE_STATE_2(State_F2_Called, int, int);
	MAKE_STATE_2(State_F2_Handler_Called, int, int);

	struct POD
	{
		int a;
		int b;
		int c;
		int d;
		int e;
		int f;
	};
	
	class CBase
	{
		// Private copy constructor!
		CBase(CBase &other)
		{
		}
	public:
		CBase()
		{
		}

		virtual int Func()
		{
			return 0;
		}
	};

	class CDerived : public CBase
	{
		virtual int Func()
		{
			return 10;
		}
	};

	// Complicated derived shit !
	class COtherWeirdBase
	{
		int member;
	};
	class CDerived2 : private COtherWeirdBase, public CBase
	{
		int m_Return;
	public:
		CDerived2(int a) : m_Return(a)
		{
		}

		virtual int Func()
		{
			return m_Return;
		}
	};

	class CHello
	{
	public:
		virtual int Func(CBase &sth)
		{
			return sth.Func();
		}

		virtual void F2(POD a)
		{
		}
	};


	class Test2
	{
	public:
		virtual void F1(){}
		virtual void F2(POD &a)
		{
			ADD_STATE(State_F2_Called(a.a, a.b));
		}
	};

	class CHook
	{
	public:
		virtual int Func(CBase &sth)
		{
			ADD_STATE(State_Result_InHook(sth.Func()));
			RETURN_META_VALUE(MRES_SUPERCEDE, 20);
		}

		virtual void F2(POD &a)
		{
			ADD_STATE(State_F2_Handler_Called(a.a, a.b));
		}

		virtual void F21(POD a)
		{
		}
	};

	SH_DECL_HOOK1(CHello, Func, SH_NOATTRIB, 0, int, CBase&);
	SH_DECL_HOOK1_void(CHello, F2, SH_NOATTRIB, 0, POD);
	SH_DECL_HOOK1_void(Test2, F2, SH_NOATTRIB, 0, POD&);

	CHello *MyInstanceFactory()
	{
		return new CHello;
	}
}


bool TestRef(std::string &error)
{
	GET_SHPTR(g_SHPtr);
	g_PLID = 1;

	CBase base;
	CDerived der;
	CDerived2 der2(11);
	CDerived2 der3(12);
	CHello *pHello = MyInstanceFactory();
	Test2 *pTest2 = new Test2;
	CAutoPtrDestruction<CHello> apd(pHello);
	CAutoPtrDestruction<Test2> apd2(pTest2);
	CHook hook;

	SourceHook::CallClass<CHello> *cc = SH_GET_CALLCLASS(pHello);

	ADD_STATE(State_Result(pHello->Func(base)));
	ADD_STATE(State_Result(pHello->Func(der)));
	ADD_STATE(State_Result(pHello->Func(der2)));
	ADD_STATE(State_Result(pHello->Func(der3)));

	CHECK_STATES((&g_States,
		new State_Result(0),
		new State_Result(10),
		new State_Result(11),
		new State_Result(12),
		NULL), "Part 1");

	ADD_STATE(State_Result(SH_CALL(cc, &CHello::Func)(base)));
	ADD_STATE(State_Result(SH_CALL(cc, &CHello::Func)(der)));
	ADD_STATE(State_Result(SH_CALL(cc, &CHello::Func)(der2)));
	ADD_STATE(State_Result(SH_CALL(cc, &CHello::Func)(der3)));

	CHECK_STATES((&g_States,
		new State_Result(0),
		new State_Result(10),
		new State_Result(11),
		new State_Result(12),
		NULL), "Part 2");

	SH_ADD_HOOK(CHello, Func, pHello, SH_MEMBER(&hook, &CHook::Func), false);

	ADD_STATE(State_Result(pHello->Func(base)));
	ADD_STATE(State_Result(pHello->Func(der)));
	ADD_STATE(State_Result(pHello->Func(der2)));
	ADD_STATE(State_Result(pHello->Func(der3)));

	CHECK_STATES((&g_States,
		new State_Result_InHook(0),
		new State_Result(20),
		new State_Result_InHook(10),
		new State_Result(20),
		new State_Result_InHook(11),
		new State_Result(20),
		new State_Result_InHook(12),
		new State_Result(20),
		NULL), "Part 3");

	ADD_STATE(State_Result(SH_CALL(cc, &CHello::Func)(base)));
	ADD_STATE(State_Result(SH_CALL(cc, &CHello::Func)(der)));
	ADD_STATE(State_Result(SH_CALL(cc, &CHello::Func)(der2)));
	ADD_STATE(State_Result(SH_CALL(cc, &CHello::Func)(der3)));

	CHECK_STATES((&g_States,
		new State_Result(0),
		new State_Result(10),
		new State_Result(11),
		new State_Result(12),
		NULL), "Part 4");

	// Check for correct ref proto handling

	SH_ADD_HOOK(CHello, F2, pHello, SH_MEMBER(&hook, &CHook::F21), false);
	SH_ADD_HOOK(Test2, F2, pTest2, SH_MEMBER(&hook, &CHook::F2), false);

	POD pod = { 1, 2 };
	pTest2->F2(pod);

	int a = 5;

	pHello->F2(pod);

	CHECK_STATES((&g_States,
		new State_F2_Handler_Called(1, 2),
		new State_F2_Called(1, 2),
		NULL), "Part 5");

   	return true;
}
