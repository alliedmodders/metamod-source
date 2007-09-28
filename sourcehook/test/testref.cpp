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
	};
	class CHelloDerived : public CHello { };

	class CHook
	{
	public:
		virtual int Func(CBase &sth)
		{
			ADD_STATE(State_Result_InHook(sth.Func()));
			RETURN_META_VALUE(MRES_SUPERCEDE, 20);
		}
	};
	SH_DECL_HOOK1(CHello, Func, SH_NOATTRIB, 0, int, CBase&);
}


bool TestRef(std::string &error)
{
	GET_SHPTR(g_SHPtr);
	g_PLID = 1;

	CBase base;
	CDerived der;
	CDerived2 der2(11);
	CDerived2 der3(12);
	CHelloDerived hello;
	CHello *pHello = &hello;
	CHook hook;

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

	ADD_STATE(State_Result(SH_CALL(pHello, &CHello::Func)(base)));
	ADD_STATE(State_Result(SH_CALL(pHello, &CHello::Func)(der)));
	ADD_STATE(State_Result(SH_CALL(pHello, &CHello::Func)(der2)));
	ADD_STATE(State_Result(SH_CALL(pHello, &CHello::Func)(der3)));

	CHECK_STATES((&g_States,
		new State_Result(0),
		new State_Result(10),
		new State_Result(11),
		new State_Result(12),
		NULL), "Part 2");

	SH_ADD_HOOK(CHello, Func, &hello, SH_MEMBER(&hook, &CHook::Func), false);

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

	ADD_STATE(State_Result(SH_CALL(pHello, &CHello::Func)(base)));
	ADD_STATE(State_Result(SH_CALL(pHello, &CHello::Func)(der)));
	ADD_STATE(State_Result(SH_CALL(pHello, &CHello::Func)(der2)));
	ADD_STATE(State_Result(SH_CALL(pHello, &CHello::Func)(der3)));

	CHECK_STATES((&g_States,
		new State_Result(0),
		new State_Result(10),
		new State_Result(11),
		new State_Result(12),
		NULL), "Part 4");

   	return true;
}
