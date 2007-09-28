// Reentrancy test
// = calling hooks from hook handlers, etc

#include <string>
#include "sourcehook_test.h"
#include "testevents.h"

namespace
{
	StateList g_States;
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

	struct C1;
	struct C2;
	struct C3;
	struct C4;
	struct C5;
	struct C6;
	struct C7;
	struct C8;

	C1 *g_pC1;
	C2 *g_pC2;
	C3 *g_pC3;
	C4 *g_pC4;
	C5 *g_pC5;
	C6 *g_pC6;
	C7 *g_pC7;
	C8 *g_pC8;

	// param1: This pointer
	MAKE_STATE_1(State_C1_F, C1*);
	MAKE_STATE_1(State_C2_F, C2*);
	MAKE_STATE_1(State_C3_F, C3*);
	MAKE_STATE_1(State_C4_F, C4*);
	MAKE_STATE_1(State_C5_F, C5*);
	MAKE_STATE_1(State_C6_F, C6*);
	MAKE_STATE_1(State_C7_F, C7*);
	MAKE_STATE_1(State_C8_F, C8*);

	MAKE_STATE_1(State_C1_G, C1*);
	MAKE_STATE_1(State_C2_G, C2*);
	MAKE_STATE_1(State_C3_G, C3*);
	MAKE_STATE_1(State_C4_G, C4*);
	MAKE_STATE_1(State_C5_G, C5*);
	MAKE_STATE_1(State_C6_G, C6*);
	MAKE_STATE_1(State_C7_G, C7*);
	MAKE_STATE_1(State_C8_G, C8*);

	MAKE_STATE_1(State_H_C1_F, C1*);
	MAKE_STATE_1(State_H_C2_F, C2*);
	MAKE_STATE_1(State_H_C3_F, C3*);
	MAKE_STATE_1(State_H_C4_F, C4*);
	MAKE_STATE_1(State_H_C5_F, C5*);
	MAKE_STATE_1(State_H_C6_F, C6*);
	MAKE_STATE_1(State_H_C7_F, C7*);
	MAKE_STATE_1(State_H_C8_F, C8*);

	MAKE_STATE_1(State_H_C1_G, C1*);
	MAKE_STATE_1(State_H_C2_G, C2*);
	MAKE_STATE_1(State_H_C3_G, C3*);
	MAKE_STATE_1(State_H_C4_G, C4*);
	MAKE_STATE_1(State_H_C5_G, C5*);
	MAKE_STATE_1(State_H_C6_G, C6*);
	MAKE_STATE_1(State_H_C7_G, C7*);
	MAKE_STATE_1(State_H_C8_G, C8*);

	MAKE_STATE_1(State_H2_C4_G, C4*);

	int g_TestID;

	struct C1
	{
		virtual void F()
		{
			ADD_STATE(State_C1_F(this));
		}
		virtual int G()
		{
			ADD_STATE(State_C1_G(this));
			return 1;
		}
	};

	struct C2
	{
		virtual void F()
		{
			ADD_STATE(State_C2_F(this));
		}
		virtual int G()
		{
			ADD_STATE(State_C2_G(this));
			return 2;
		}
	};
	struct C3
	{
		virtual void F()
		{
			ADD_STATE(State_C3_F(this));
		}
		virtual int G()
		{
			ADD_STATE(State_C3_G(this));
			return 3;
		}
	};
	struct C4
	{
		virtual void F()
		{
			ADD_STATE(State_C4_F(this));
		}
		virtual int G()
		{
			ADD_STATE(State_C4_G(this));
			return 4;
		}
	};
	struct C5
	{
		virtual void F()
		{
			ADD_STATE(State_C5_F(this));
		}
		virtual int G()
		{
			ADD_STATE(State_C5_G(this));
			return 5;
		}
	};
	struct C6
	{
		virtual void F()
		{
			ADD_STATE(State_C6_F(this));
		}
		virtual int G()
		{
			ADD_STATE(State_C6_G(this));
			return 6;
		}
	};
	struct C7
	{
		virtual void F()
		{
			ADD_STATE(State_C7_F(this));
		}
		virtual int G()
		{
			ADD_STATE(State_C7_G(this));
			return 7;
		}
	};
	struct C8
	{
		virtual void F()
		{
			ADD_STATE(State_C8_F(this));
		}
		virtual int G()
		{
			ADD_STATE(State_C8_G(this));
			return 8;
		}
	};

	struct C1_Derived : public C1 {};
	struct C2_Derived : public C2 {};
	struct C3_Derived : public C3 {};
	struct C4_Derived : public C4 {};
	struct C5_Derived : public C5 {};
	struct C6_Derived : public C6 {};
	struct C7_Derived : public C7 {};
	struct C8_Derived : public C8 {};

	SH_DECL_HOOK0_void(C1, F, SH_NOATTRIB, 0);
	SH_DECL_HOOK0(C1, G, SH_NOATTRIB, 0, int);
	SH_DECL_HOOK0_void(C2, F, SH_NOATTRIB, 0);
	SH_DECL_HOOK0(C2, G, SH_NOATTRIB, 0, int);
	SH_DECL_HOOK0_void(C3, F, SH_NOATTRIB, 0);
	SH_DECL_HOOK0(C3, G, SH_NOATTRIB, 0, int);
	SH_DECL_HOOK0_void(C4, F, SH_NOATTRIB, 0);
	SH_DECL_HOOK0(C4, G, SH_NOATTRIB, 0, int);
	SH_DECL_HOOK0_void(C5, F, SH_NOATTRIB, 0);
	SH_DECL_HOOK0(C5, G, SH_NOATTRIB, 0, int);
	SH_DECL_HOOK0_void(C6, F, SH_NOATTRIB, 0);
	SH_DECL_HOOK0(C6, G, SH_NOATTRIB, 0, int);
	SH_DECL_HOOK0_void(C7, F, SH_NOATTRIB, 0);
	SH_DECL_HOOK0(C7, G, SH_NOATTRIB, 0, int);
	SH_DECL_HOOK0_void(C8, F, SH_NOATTRIB, 0);
	SH_DECL_HOOK0(C8, G, SH_NOATTRIB, 0, int);

	void Handler_C1_F()
	{
		ADD_STATE(State_H_C1_F(META_IFACEPTR(C1)));
		g_pC1->G();
	}
	int Handler_C1_G()
	{
		ADD_STATE(State_H_C1_G(META_IFACEPTR(C1)));
		g_pC2->F();
		return 1;
	}
	void Handler_C2_F()
	{
		ADD_STATE(State_H_C2_F(META_IFACEPTR(C2)));
		g_pC2->G();
	}
	int Handler_C2_G()
	{
		ADD_STATE(State_H_C2_G(META_IFACEPTR(C2)));
		g_pC3->F();
		return 2;
	}
	void Handler_C3_F()
	{
		ADD_STATE(State_H_C3_F(META_IFACEPTR(C3)));
		g_pC3->G();
	}
	int Handler_C3_G()
	{
		ADD_STATE(State_H_C3_G(META_IFACEPTR(C3)));
		g_pC4->F();
		return 3;
	}
	void Handler_C4_F()
	{
		ADD_STATE(State_H_C4_F(META_IFACEPTR(C4)));
		g_pC4->G();
	}
	int Handler_C4_G()
	{
		ADD_STATE(State_H_C4_G(META_IFACEPTR(C4)));
		g_pC5->F();
		return 4;
	}
	int Handler2_C4_G()
	{
		ADD_STATE(State_H2_C4_G(META_IFACEPTR(C4)));
		return 4;
	}
	void Handler_C5_F()
	{
		ADD_STATE(State_H_C5_F(META_IFACEPTR(C5)));
		g_pC5->G();
	}
	int Handler_C5_G()
	{
		ADD_STATE(State_H_C5_G(META_IFACEPTR(C5)));
		g_pC6->F();
		return 5;
	}
	void Handler_C6_F()
	{
		ADD_STATE(State_H_C6_F(META_IFACEPTR(C6)));
		g_pC6->G();
	}
	int Handler_C6_G()
	{
		ADD_STATE(State_H_C6_G(META_IFACEPTR(C6)));
		g_pC7->F();
		return 6;
	}
	void Handler_C7_F()
	{
		if (g_TestID == 1 || g_TestID == 2)
			SH_REMOVE_HOOK(C4, G, g_pC4, SH_STATIC(Handler2_C4_G), false);
		if (g_TestID == 2)
			SH_REMOVE_HOOK(C4, G, g_pC4, SH_STATIC(Handler_C4_G), false);

		ADD_STATE(State_H_C7_F(META_IFACEPTR(C7)));
		g_pC7->G();
	}
	int Handler_C7_G()
	{
		ADD_STATE(State_H_C7_G(META_IFACEPTR(C7)));
		g_pC8->F();
		return 7;
	}
	void Handler_C8_F()
	{
		ADD_STATE(State_H_C8_F(META_IFACEPTR(C8)));
		g_pC8->G();
	}
	int Handler_C8_G()
	{
		ADD_STATE(State_H_C8_G(META_IFACEPTR(C8)));
		return 8;
	}

	C1_Derived g_C1;
	C2_Derived g_C2;
	C3_Derived g_C3;
	C4_Derived g_C4;
	C5_Derived g_C5;
	C6_Derived g_C6;
	C7_Derived g_C7;
	C8_Derived g_C8;
}

bool TestReentr(std::string &error)
{
	GET_SHPTR(g_SHPtr);
	g_PLID = 1337;

	g_pC1 = &g_C1;
	g_pC2 = &g_C2;
	g_pC3 = &g_C3;
	g_pC4 = &g_C4;
	g_pC5 = &g_C5;
	g_pC6 = &g_C6;
	g_pC7 = &g_C7;
	g_pC8 = &g_C8;

	SH_ADD_HOOK(C1, F, g_pC1, SH_STATIC(Handler_C1_F), false);
	SH_ADD_HOOK(C1, G, g_pC1, SH_STATIC(Handler_C1_G), false);
	SH_ADD_HOOK(C2, F, g_pC2, SH_STATIC(Handler_C2_F), false);
	SH_ADD_HOOK(C2, G, g_pC2, SH_STATIC(Handler_C2_G), false);
	SH_ADD_HOOK(C3, F, g_pC3, SH_STATIC(Handler_C3_F), false);
	SH_ADD_HOOK(C3, G, g_pC3, SH_STATIC(Handler_C3_G), false);
	SH_ADD_HOOK(C4, F, g_pC4, SH_STATIC(Handler_C4_F), false);
	SH_ADD_HOOK(C4, G, g_pC4, SH_STATIC(Handler_C4_G), false);
	SH_ADD_HOOK(C5, F, g_pC5, SH_STATIC(Handler_C5_F), false);
	SH_ADD_HOOK(C5, G, g_pC5, SH_STATIC(Handler_C5_G), false);
	SH_ADD_HOOK(C6, F, g_pC6, SH_STATIC(Handler_C6_F), false);
	SH_ADD_HOOK(C6, G, g_pC6, SH_STATIC(Handler_C6_G), false);
	SH_ADD_HOOK(C7, F, g_pC7, SH_STATIC(Handler_C7_F), false);
	SH_ADD_HOOK(C7, G, g_pC7, SH_STATIC(Handler_C7_G), false);
	SH_ADD_HOOK(C8, F, g_pC8, SH_STATIC(Handler_C8_F), false);
	SH_ADD_HOOK(C8, G, g_pC8, SH_STATIC(Handler_C8_G), false);

	g_pC1->F();

	CHECK_STATES((&g_States,
		new State_H_C1_F(g_pC1),
		new State_H_C1_G(g_pC1),
		new State_H_C2_F(g_pC2),
		new State_H_C2_G(g_pC2),
		new State_H_C3_F(g_pC3),
		new State_H_C3_G(g_pC3),
		new State_H_C4_F(g_pC4),
		new State_H_C4_G(g_pC4),
		new State_H_C5_F(g_pC5),
		new State_H_C5_G(g_pC5),
		new State_H_C6_F(g_pC6),
		new State_H_C6_G(g_pC6),
		new State_H_C7_F(g_pC7),
		new State_H_C7_G(g_pC7),
		new State_H_C8_F(g_pC8),
		new State_H_C8_G(g_pC8),
		new State_C8_G(g_pC8),
		new State_C8_F(g_pC8),
		new State_C7_G(g_pC7),
		new State_C7_F(g_pC7),
		new State_C6_G(g_pC6),
		new State_C6_F(g_pC6),
		new State_C5_G(g_pC5),
		new State_C5_F(g_pC5),
		new State_C4_G(g_pC4),
		new State_C4_F(g_pC4),
		new State_C3_G(g_pC3),
		new State_C3_F(g_pC3),
		new State_C2_G(g_pC2),
		new State_C2_F(g_pC2),
		new State_C1_G(g_pC1),
		new State_C1_F(g_pC1),
		NULL), "1");


	SH_ADD_HOOK(C4, G, g_pC4, SH_STATIC(Handler2_C4_G), false);

	g_pC1->F();

	CHECK_STATES((&g_States,
		new State_H_C1_F(g_pC1),
		new State_H_C1_G(g_pC1),
		new State_H_C2_F(g_pC2),
		new State_H_C2_G(g_pC2),
		new State_H_C3_F(g_pC3),
		new State_H_C3_G(g_pC3),
		new State_H_C4_F(g_pC4),
		new State_H_C4_G(g_pC4),
		new State_H_C5_F(g_pC5),
		new State_H_C5_G(g_pC5),
		new State_H_C6_F(g_pC6),
		new State_H_C6_G(g_pC6),
		new State_H_C7_F(g_pC7),
		new State_H_C7_G(g_pC7),
		new State_H_C8_F(g_pC8),
		new State_H_C8_G(g_pC8),
		new State_C8_G(g_pC8),
		new State_C8_F(g_pC8),
		new State_C7_G(g_pC7),
		new State_C7_F(g_pC7),
		new State_C6_G(g_pC6),
		new State_C6_F(g_pC6),
		new State_C5_G(g_pC5),
		new State_C5_F(g_pC5),
		new State_H2_C4_G(g_pC4),
		new State_C4_G(g_pC4),
		new State_C4_F(g_pC4),
		new State_C3_G(g_pC3),
		new State_C3_F(g_pC3),
		new State_C2_G(g_pC2),
		new State_C2_F(g_pC2),
		new State_C1_G(g_pC1),
		new State_C1_F(g_pC1),
		NULL), "2");

	g_pC1->F();

	CHECK_STATES((&g_States,
		new State_H_C1_F(g_pC1),
		new State_H_C1_G(g_pC1),
		new State_H_C2_F(g_pC2),
		new State_H_C2_G(g_pC2),
		new State_H_C3_F(g_pC3),
		new State_H_C3_G(g_pC3),
		new State_H_C4_F(g_pC4),
		new State_H_C4_G(g_pC4),
		new State_H_C5_F(g_pC5),
		new State_H_C5_G(g_pC5),
		new State_H_C6_F(g_pC6),
		new State_H_C6_G(g_pC6),
		new State_H_C7_F(g_pC7),
		new State_H_C7_G(g_pC7),
		new State_H_C8_F(g_pC8),
		new State_H_C8_G(g_pC8),
		new State_C8_G(g_pC8),
		new State_C8_F(g_pC8),
		new State_C7_G(g_pC7),
		new State_C7_F(g_pC7),
		new State_C6_G(g_pC6),
		new State_C6_F(g_pC6),
		new State_C5_G(g_pC5),
		new State_C5_F(g_pC5),
		new State_H2_C4_G(g_pC4),
		new State_C4_G(g_pC4),
		new State_C4_F(g_pC4),
		new State_C3_G(g_pC3),
		new State_C3_F(g_pC3),
		new State_C2_G(g_pC2),
		new State_C2_F(g_pC2),
		new State_C1_G(g_pC1),
		new State_C1_F(g_pC1),
		NULL), "3");

	g_TestID = 1;

	g_pC1->F();

	CHECK_STATES((&g_States,
		new State_H_C1_F(g_pC1),
		new State_H_C1_G(g_pC1),
		new State_H_C2_F(g_pC2),
		new State_H_C2_G(g_pC2),
		new State_H_C3_F(g_pC3),
		new State_H_C3_G(g_pC3),
		new State_H_C4_F(g_pC4),
		new State_H_C4_G(g_pC4),
		new State_H_C5_F(g_pC5),
		new State_H_C5_G(g_pC5),
		new State_H_C6_F(g_pC6),
		new State_H_C6_G(g_pC6),
		new State_H_C7_F(g_pC7),
		new State_H_C7_G(g_pC7),
		new State_H_C8_F(g_pC8),
		new State_H_C8_G(g_pC8),
		new State_C8_G(g_pC8),
		new State_C8_F(g_pC8),
		new State_C7_G(g_pC7),
		new State_C7_F(g_pC7),
		new State_C6_G(g_pC6),
		new State_C6_F(g_pC6),
		new State_C5_G(g_pC5),
		new State_C5_F(g_pC5),
		new State_C4_G(g_pC4),
		new State_C4_F(g_pC4),
		new State_C3_G(g_pC3),
		new State_C3_F(g_pC3),
		new State_C2_G(g_pC2),
		new State_C2_F(g_pC2),
		new State_C1_G(g_pC1),
		new State_C1_F(g_pC1),
		NULL), "4");

	g_pC1->F();

	CHECK_STATES((&g_States,
		new State_H_C1_F(g_pC1),
		new State_H_C1_G(g_pC1),
		new State_H_C2_F(g_pC2),
		new State_H_C2_G(g_pC2),
		new State_H_C3_F(g_pC3),
		new State_H_C3_G(g_pC3),
		new State_H_C4_F(g_pC4),
		new State_H_C4_G(g_pC4),
		new State_H_C5_F(g_pC5),
		new State_H_C5_G(g_pC5),
		new State_H_C6_F(g_pC6),
		new State_H_C6_G(g_pC6),
		new State_H_C7_F(g_pC7),
		new State_H_C7_G(g_pC7),
		new State_H_C8_F(g_pC8),
		new State_H_C8_G(g_pC8),
		new State_C8_G(g_pC8),
		new State_C8_F(g_pC8),
		new State_C7_G(g_pC7),
		new State_C7_F(g_pC7),
		new State_C6_G(g_pC6),
		new State_C6_F(g_pC6),
		new State_C5_G(g_pC5),
		new State_C5_F(g_pC5),
		new State_C4_G(g_pC4),
		new State_C4_F(g_pC4),
		new State_C3_G(g_pC3),
		new State_C3_F(g_pC3),
		new State_C2_G(g_pC2),
		new State_C2_F(g_pC2),
		new State_C1_G(g_pC1),
		new State_C1_F(g_pC1),
		NULL), "5");



	SH_ADD_HOOK(C4, G, g_pC4, SH_STATIC(Handler2_C4_G), false);

	g_TestID = 2;

	g_pC1->F();

	CHECK_STATES((&g_States,
		new State_H_C1_F(g_pC1),
		new State_H_C1_G(g_pC1),
		new State_H_C2_F(g_pC2),
		new State_H_C2_G(g_pC2),
		new State_H_C3_F(g_pC3),
		new State_H_C3_G(g_pC3),
		new State_H_C4_F(g_pC4),
		new State_H_C4_G(g_pC4),
		new State_H_C5_F(g_pC5),
		new State_H_C5_G(g_pC5),
		new State_H_C6_F(g_pC6),
		new State_H_C6_G(g_pC6),
		new State_H_C7_F(g_pC7),
		new State_H_C7_G(g_pC7),
		new State_H_C8_F(g_pC8),
		new State_H_C8_G(g_pC8),
		new State_C8_G(g_pC8),
		new State_C8_F(g_pC8),
		new State_C7_G(g_pC7),
		new State_C7_F(g_pC7),
		new State_C6_G(g_pC6),
		new State_C6_F(g_pC6),
		new State_C5_G(g_pC5),
		new State_C5_F(g_pC5),
		new State_C4_G(g_pC4),
		new State_C4_F(g_pC4),
		new State_C3_G(g_pC3),
		new State_C3_F(g_pC3),
		new State_C2_G(g_pC2),
		new State_C2_F(g_pC2),
		new State_C1_G(g_pC1),
		new State_C1_F(g_pC1),
		NULL), "6");

	g_pC1->F();

	CHECK_STATES((&g_States,
		new State_H_C1_F(g_pC1),
		new State_H_C1_G(g_pC1),
		new State_H_C2_F(g_pC2),
		new State_H_C2_G(g_pC2),
		new State_H_C3_F(g_pC3),
		new State_H_C3_G(g_pC3),
		new State_H_C4_F(g_pC4),
		new State_C4_G(g_pC4),
		new State_C4_F(g_pC4),
		new State_C3_G(g_pC3),
		new State_C3_F(g_pC3),
		new State_C2_G(g_pC2),
		new State_C2_F(g_pC2),
		new State_C1_G(g_pC1),
		new State_C1_F(g_pC1),
		NULL), "7");

	SH_REMOVE_HOOK(C1, F, g_pC1, SH_STATIC(Handler_C1_F), false);
	SH_REMOVE_HOOK(C1, G, g_pC1, SH_STATIC(Handler_C1_G), false);
	SH_REMOVE_HOOK(C2, F, g_pC2, SH_STATIC(Handler_C2_F), false);
	SH_REMOVE_HOOK(C2, G, g_pC2, SH_STATIC(Handler_C2_G), false);
	SH_REMOVE_HOOK(C3, F, g_pC3, SH_STATIC(Handler_C3_F), false);
	SH_REMOVE_HOOK(C3, G, g_pC3, SH_STATIC(Handler_C3_G), false);
	SH_REMOVE_HOOK(C4, F, g_pC4, SH_STATIC(Handler_C4_F), false);
	SH_REMOVE_HOOK(C4, G, g_pC4, SH_STATIC(Handler_C4_G), false);
	SH_REMOVE_HOOK(C5, F, g_pC5, SH_STATIC(Handler_C5_F), false);
	SH_REMOVE_HOOK(C5, G, g_pC5, SH_STATIC(Handler_C5_G), false);
	SH_REMOVE_HOOK(C6, F, g_pC6, SH_STATIC(Handler_C6_F), false);
	SH_REMOVE_HOOK(C6, G, g_pC6, SH_STATIC(Handler_C6_G), false);
	SH_REMOVE_HOOK(C7, F, g_pC7, SH_STATIC(Handler_C7_F), false);
	SH_REMOVE_HOOK(C7, G, g_pC7, SH_STATIC(Handler_C7_G), false);
	SH_REMOVE_HOOK(C8, F, g_pC8, SH_STATIC(Handler_C8_F), false);
	SH_REMOVE_HOOK(C8, G, g_pC8, SH_STATIC(Handler_C8_G), false);

	return true;
}
