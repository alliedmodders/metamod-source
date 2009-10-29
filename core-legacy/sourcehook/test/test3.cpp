#include <string>
#include "sourcehook_test.h"
#include "testevents.h"

// TEST3
// Tests with inheritance / thisptroffsets

namespace
{
	StateList g_States;
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

	MAKE_STATE_1(State_Func1_Called, void*);			// param1: This pointer
	MAKE_STATE_1(State_Func2_Called, void*);			// param1: This pointer
	MAKE_STATE_1(State_Func3_Called, void*);			// param1: This pointer

	MAKE_STATE_1(State_Func1H_Called, void*);
	MAKE_STATE_1(State_Func2H_Called, void*);
	MAKE_STATE_1(State_Func3H_Called, void*);

	class Base1
	{
		int a;
	public:
		virtual void Func1()
		{
			ADD_STATE(State_Func1_Called(reinterpret_cast<void*>(this)));
		}
	};
	class Base2
	{
		int b;
	public:
		virtual void Func2()
		{
			ADD_STATE(State_Func2_Called(reinterpret_cast<void*>(this)));
		}
	};
	class Derived : public Base1, public Base2
	{
		int c;
	public:
		virtual void Func3()
		{
			ADD_STATE(State_Func3_Called(reinterpret_cast<void*>(this)));
		}
	};

	class DerivedDerived : public Derived { };

	SH_DECL_HOOK0_void(Derived, Func1, SH_NOATTRIB, 0);
	SH_DECL_HOOK0_void(Derived, Func2, SH_NOATTRIB, 0);
	SH_DECL_HOOK0_void(Derived, Func3, SH_NOATTRIB, 0);
	SH_DECL_HOOK0_void(Base1, Func1, SH_NOATTRIB, 0);
	SH_DECL_HOOK0_void(Base2, Func2, SH_NOATTRIB, 0);

	void Handler_Func1()
	{
		ADD_STATE(State_Func1H_Called(META_IFACEPTR(void)));
	}
	void Handler_Func2()
	{
		ADD_STATE(State_Func2H_Called(META_IFACEPTR(void)));
	}
	void Handler_Func3()
	{
		ADD_STATE(State_Func3H_Called(META_IFACEPTR(void)));
	}
}

bool TestThisPtrOffs(std::string &error)
{
	GET_SHPTR(g_SHPtr);
	g_PLID = 1337;

	DerivedDerived inst;
	Derived *pD = &inst;
	Base1 *pB1 = pD;
	Base2 *pB2 = pD;

	// It should be:
	// pB1 = pD
	// pB2 > pB1

	// 1)
	//  Get a callclass for pD
	//  Verify whether the this pointers are correct
	//  Also call them normally to make sure that we aren't messing it up ;)

	SH_CALL(pD, &Derived::Func1)();
	SH_CALL(pD, &Derived::Func2)();
	SH_CALL(pD, &Derived::Func3)();
	pD->Func1();
	pD->Func2();
	pD->Func3();

	CHECK_STATES((&g_States,
		new State_Func1_Called(pB1),
		new State_Func2_Called(pB2),
		new State_Func3_Called(pD),
		new State_Func1_Called(pB1),
		new State_Func2_Called(pB2),
		new State_Func3_Called(pD),
		NULL), "Part 1");

	SH_CALL(pD, &Base1::Func1)();
	SH_CALL(pD, &Base2::Func2)();

	CHECK_STATES((&g_States,
		new State_Func1_Called(pB1),
		new State_Func2_Called(pB2),
		NULL), "Part 1.1");

	// 2)
	//   Get callclasses for the other ones and verify it as well

	SH_CALL(pB1, &Base1::Func1)();
	SH_CALL(pB2, &Base2::Func2)();

	CHECK_STATES((&g_States,
		new State_Func1_Called(pB1),
		new State_Func2_Called(pB2),
		NULL), "Part 2");


	// 3) Add hooks on them (referring to them through pD1 / Derived)
	//   Check whether the hooks are called with the correct this pointer

	SH_ADD_HOOK(Derived, Func1, pD, SH_STATIC(Handler_Func1), false);
	SH_ADD_HOOK(Derived, Func2, pD, SH_STATIC(Handler_Func2), false);
	SH_ADD_HOOK(Derived, Func3, pD, SH_STATIC(Handler_Func3), false);

	pD->Func1();
	pD->Func2();
	pD->Func3();
	pB1->Func1();
	pB2->Func2();

	// The handlers should always be called with the pointer to Derived
	CHECK_STATES((&g_States,
		new State_Func1H_Called(pD),
		new State_Func1_Called(pB1),
		new State_Func2H_Called(pD),
		new State_Func2_Called(pB2),
		new State_Func3H_Called(pD),
		new State_Func3_Called(pD),
		new State_Func1H_Called(pD),
		new State_Func1_Called(pB1),
		new State_Func2H_Called(pD),
		new State_Func2_Called(pB2),
		NULL), "Part 3");

	SH_REMOVE_HOOK(Derived, Func1, pD, SH_STATIC(Handler_Func1), false);
	SH_REMOVE_HOOK(Derived, Func2, pD, SH_STATIC(Handler_Func2), false);
	SH_REMOVE_HOOK(Derived, Func3, pD, SH_STATIC(Handler_Func3), false);

	// 4)
	//   Now add the hooks on Base1 and Base2 and check again

	// Note that the new implicit_cast should convert the pD to Base1*/Base2* :)
	SH_ADD_HOOK(Base1, Func1, pD, SH_STATIC(Handler_Func1), false);
	SH_ADD_HOOK(Base2, Func2, pD, SH_STATIC(Handler_Func2), false);
	SH_ADD_HOOK(Derived, Func3, pD, SH_STATIC(Handler_Func3), false);

	pD->Func1();
	pD->Func2();
	pD->Func3();
	pB1->Func1();
	pB2->Func2();

	// This time, the handlers for Func1 should be called with pB1 and the handlers
	// for Func2 should be called with pB2
	CHECK_STATES((&g_States,
		new State_Func1H_Called(pB1),
		new State_Func1_Called(pB1),
		new State_Func2H_Called(pB2),
		new State_Func2_Called(pB2),
		new State_Func3H_Called(pD),
		new State_Func3_Called(pD),
		new State_Func1H_Called(pB1),
		new State_Func1_Called(pB1),
		new State_Func2H_Called(pB2),
		new State_Func2_Called(pB2),
		NULL), "Part 4");

	SH_REMOVE_HOOK(Base1, Func1, pD, SH_STATIC(Handler_Func1), false);
	SH_REMOVE_HOOK(Base2, Func2, pD, SH_STATIC(Handler_Func2), false);
	SH_REMOVE_HOOK(Derived, Func3, pD, SH_STATIC(Handler_Func3), false);


	// 5)
	//   Add some hooks, and use callclasses

	// 5.1) First off, add all of them on pD
	SH_ADD_HOOK(Derived, Func1, pD, SH_STATIC(Handler_Func1), false);
	SH_ADD_HOOK(Derived, Func2, pD, SH_STATIC(Handler_Func2), false);
	SH_ADD_HOOK(Derived, Func3, pD, SH_STATIC(Handler_Func3), false);

	pD->Func1();
	pD->Func2();
	pD->Func3();

	CHECK_STATES((&g_States,
		new State_Func1H_Called(pD),
		new State_Func1_Called(pB1),
		new State_Func2H_Called(pD),
		new State_Func2_Called(pB2),
		new State_Func3H_Called(pD),
		new State_Func3_Called(pD),
		NULL), "Part 5.1");

	SH_CALL(pD, &Derived::Func1)();
	SH_CALL(pD, &Derived::Func2)();
	SH_CALL(pD, &Derived::Func3)();
	SH_CALL(pB1, &Base1::Func1)();
	SH_CALL(pB2, &Base2::Func2)();

	CHECK_STATES((&g_States,
		new State_Func1_Called(pB1),
		new State_Func2_Called(pB2),
		new State_Func3_Called(pD),
		new State_Func1_Called(pB1),
		new State_Func2_Called(pB2),
		NULL), "Part 5.2");

	SH_REMOVE_HOOK(Derived, Func1, pD, SH_STATIC(Handler_Func1), false);
	SH_REMOVE_HOOK(Derived, Func2, pD, SH_STATIC(Handler_Func2), false);
	SH_REMOVE_HOOK(Derived, Func3, pD, SH_STATIC(Handler_Func3), false);

	return true;
}
