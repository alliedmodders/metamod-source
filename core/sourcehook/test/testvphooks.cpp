#include <string>
#include "sourcehook.h"
#include "sourcehook_test.h"
#include "testevents.h"

// TEST VP HOOKS
// Test vfnptr-wide hooks

namespace
{
	StateList g_States;
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

	class IBase;

	MAKE_STATE_1(State_D1_Func1, IBase *);
	MAKE_STATE_1(State_D2_Func1, IBase *);
	MAKE_STATE_1(State_Func1_Pre, IBase *);
	MAKE_STATE_1(State_Func1_Post, IBase *);

	MAKE_STATE_1(State_D1_Func2, IBase *);
	MAKE_STATE_1(State_D2_Func2, IBase *);
	MAKE_STATE_1(State_Func2_Pre, IBase *);
	MAKE_STATE_1(State_Func2_Post, IBase *);

	MAKE_STATE_2(State_D1_Func3, IBase *, int);
	MAKE_STATE_2(State_D2_Func3, IBase *, int);
	MAKE_STATE_2(State_Func3_Pre, IBase *, int);
	MAKE_STATE_2(State_Func3_Post, IBase *, int);

	class IBase
	{
	public:
		virtual void Func1() = 0;
		virtual void Func2() = 0;
		virtual void Func3(int x) = 0;
	};

	class CDerived1 : public IBase
	{
	public:
		virtual void Func1()
		{
			ADD_STATE(State_D1_Func1(this));
		}
		virtual void Func2()
		{
			ADD_STATE(State_D1_Func2(this));
		}
		virtual void Func3(int x)
		{
			ADD_STATE(State_D1_Func3(this, x));
		}
	};

	class CDerived2 : public IBase
	{
	public:
		virtual void Func1()
		{
			ADD_STATE(State_D2_Func1(this));
		}
		virtual void Func2()
		{
			ADD_STATE(State_D2_Func2(this));
		}
		virtual void Func3(int x)
		{
			ADD_STATE(State_D2_Func3(this, x));
		}
	};

	void Handler_Func1_Pre()
	{
		ADD_STATE(State_Func1_Pre(META_IFACEPTR(IBase)));
	}
	void Handler_Func1_Post()
	{
		ADD_STATE(State_Func1_Post(META_IFACEPTR(IBase)));
	}
	int g_F2_Pre_HookToRemove = 0;
	void Handler_Func2_Pre()
	{
		ADD_STATE(State_Func2_Pre(META_IFACEPTR(IBase)));
		SH_REMOVE_HOOK_ID(g_F2_Pre_HookToRemove);
	}
	void Handler_Func2_Post()
	{
		ADD_STATE(State_Func2_Post(META_IFACEPTR(IBase)));
	}


	void Handler_Func3_Pre(int x)
	{
		ADD_STATE(State_Func3_Pre(META_IFACEPTR(IBase), x));

		RETURN_META_NEWPARAMS(MRES_IGNORED, &IBase::Func3, (x+1));
	}
	void Handler_Func3_Post(int x)
	{
		ADD_STATE(State_Func3_Post(META_IFACEPTR(IBase), x));
	}

	SH_DECL_HOOK0_void(IBase, Func1, SH_NOATTRIB, 0);
	SH_DECL_HOOK0_void(IBase, Func2, SH_NOATTRIB, 0);
	SH_DECL_HOOK1_void(IBase, Func3, SH_NOATTRIB, 0, int);

	SH_DECL_MANUALHOOK1_void(IBase_Func3_Manual, 2, 0, 0, int);
}

bool TestVPHooks(std::string &error)
{
	GET_SHPTR(g_SHPtr);
	g_PLID = 1337;

	CDerived1 d1i1;
	CDerived1 d1i2;
	CDerived2 d2i1;

	IBase *p_d1i1 = &d1i1;
	IBase *p_d1i2 = &d1i2;
	IBase *p_d2i1 = &d2i1;
	
	int hook1 = SH_ADD_VPHOOK(IBase, Func1, p_d1i1, SH_STATIC(Handler_Func1_Pre), false);

	p_d1i1->Func1();
	p_d1i2->Func1();
	p_d2i1->Func1();

	CHECK_STATES((&g_States,
		new State_Func1_Pre(p_d1i1),
		new State_D1_Func1(p_d1i1),
		
		new State_Func1_Pre(p_d1i2),
		new State_D1_Func1(p_d1i2),

		new State_D2_Func1(p_d2i1),
		NULL), "Part 1");

	SH_REMOVE_HOOK_ID(hook1);

	p_d1i1->Func1();
	p_d1i2->Func1();
	p_d2i1->Func1();

	CHECK_STATES((&g_States,
		new State_D1_Func1(p_d1i1),

		new State_D1_Func1(p_d1i2),

		new State_D2_Func1(p_d2i1),
		NULL), "Part 2");


	// Normal hook, then vp hook

	int hook2 = SH_ADD_HOOK(IBase, Func1, p_d1i1, SH_STATIC(Handler_Func1_Pre), false);
	hook1 = SH_ADD_VPHOOK(IBase, Func1, p_d1i1, SH_STATIC(Handler_Func1_Pre), false);

	p_d1i1->Func1();
	p_d1i2->Func1();
	p_d2i1->Func1();

	CHECK_STATES((&g_States,
		new State_Func1_Pre(p_d1i1),
		new State_Func1_Pre(p_d1i1),
		new State_D1_Func1(p_d1i1),

		new State_Func1_Pre(p_d1i2),
		new State_D1_Func1(p_d1i2),

		new State_D2_Func1(p_d2i1),
		NULL), "Part 3");

	SH_REMOVE_HOOK_ID(hook1);

	p_d1i1->Func1();
	p_d1i2->Func1();
	p_d2i1->Func1();

	CHECK_STATES((&g_States,
		new State_Func1_Pre(p_d1i1),
		new State_D1_Func1(p_d1i1),

		new State_D1_Func1(p_d1i2),

		new State_D2_Func1(p_d2i1),
		NULL), "Part 4");

	SH_REMOVE_HOOK_ID(hook2);

	p_d1i1->Func1();
	p_d1i2->Func1();
	p_d2i1->Func1();

	CHECK_STATES((&g_States,
		new State_D1_Func1(p_d1i1),

		new State_D1_Func1(p_d1i2),

		new State_D2_Func1(p_d2i1),
		NULL), "Part 5");

	// Test this:
	// Normal hook AND vp hook on Func2
	// Func2's pre handler removes the VP hook.

	hook1 = SH_ADD_VPHOOK(IBase, Func2, p_d1i1, SH_STATIC(Handler_Func2_Pre), false);
	hook2 = SH_ADD_HOOK(IBase, Func2, p_d1i1, SH_STATIC(Handler_Func2_Pre), false);

	g_F2_Pre_HookToRemove = hook1;
	p_d1i1->Func2();
	p_d1i1->Func2();

	CHECK_STATES((&g_States,
		new State_Func2_Pre(p_d1i1),
		new State_D1_Func2(p_d1i1),

		new State_Func2_Pre(p_d1i1),
		new State_D1_Func2(p_d1i1),

		NULL), "Part 6");

	SH_REMOVE_HOOK_ID(hook1);

	// Hook function 3:
	//  Using manualhook, VP
	hook1 = SH_ADD_MANUALVPHOOK(IBase_Func3_Manual, p_d1i1, SH_STATIC(Handler_Func3_Pre), false);

	//  Normally, VP
	hook2 = SH_ADD_VPHOOK(IBase, Func3, p_d1i1, SH_STATIC(Handler_Func3_Pre), false);

	// Normally, no VP
	int hook3 = SH_ADD_HOOK(IBase, Func3, p_d1i1, SH_STATIC(Handler_Func3_Pre), false);

	p_d1i1->Func3(1);

	CHECK_STATES((&g_States,
		new State_Func3_Pre(p_d1i1, 1),		// manual vp hook
		new State_Func3_Pre(p_d1i1, 2),		// normal vp hook
		new State_Func3_Pre(p_d1i1, 3),		// normal non-vp hook

		new State_D1_Func3(p_d1i1, 4),		// function

		NULL), "Part 7.1");

	p_d1i2->Func3(1);

	CHECK_STATES((&g_States,
		new State_Func3_Pre(p_d1i2, 1),		// manual vp hook
		new State_Func3_Pre(p_d1i2, 2),		// normal vp hook

		new State_D1_Func3(p_d1i2, 3),		// function

		NULL), "Part 7.2");

	return true;
}

