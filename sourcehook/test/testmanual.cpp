#include <string>
#include "sourcehook.h"
#include "sourcehook_test.h"
#include "testevents.h"

// TESTMANUAL
// Test manual hooks
// :TODO: test more extensively

namespace
{
	StateList g_States;
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

	MAKE_STATE_1(State_Func1_Called, void*);				// param1: This pointer
	MAKE_STATE_2(State_Func2_Called, void*, int);			// param1: This pointer; param2: parameter
	MAKE_STATE_1(State_Func3_Called, void*);				// param1: This pointer
	MAKE_STATE_2(State_Func4_Called, void*, int);			// param1: This pointer; param2: parameter
	MAKE_STATE_1(State_Func5_Called, void*);				// param1: This pointer

	MAKE_STATE_1(State_Func1H_Called, void*);
	MAKE_STATE_2(State_Func2H_Called, void*, int);
	MAKE_STATE_1(State_Func3H_Called, void*);
	MAKE_STATE_2(State_Func4H_Called, void*, int);

	MAKE_STATE_1(State_Return, short);

	class TheWall
	{
	public:
		virtual void Func1()
		{
			ADD_STATE(State_Func1_Called(reinterpret_cast<void*>(this)));
		}
		virtual void Func2(int x)
		{
			ADD_STATE(State_Func2_Called(reinterpret_cast<void*>(this), x));
		}

		virtual short Func3()
		{
			ADD_STATE(State_Func3_Called(reinterpret_cast<void*>(this)));
			return 3;
		}
		virtual short Func4(int x)
		{
			ADD_STATE(State_Func4_Called(reinterpret_cast<void*>(this), x));
			return 4;
		}
		virtual void Func5()
		{
			ADD_STATE(State_Func5_Called(reinterpret_cast<void*>(this)));
		}

		virtual const char *Func6()
		{
			return 0;
		}
		virtual char *Func7()
		{
			return 0;
		}
	};

	SH_DECL_HOOK0_void(TheWall, Func1, SH_NOATTRIB, 0);
	SH_DECL_HOOK1_void(TheWall, Func2, SH_NOATTRIB, 0, int);
	SH_DECL_HOOK0(TheWall, Func3, SH_NOATTRIB, 0, short);
	SH_DECL_HOOK1(TheWall, Func4, SH_NOATTRIB, 0, short, int);

	SH_DECL_MANUALHOOK0_void(TheWall_Func1, 0, 0, 0);
	SH_DECL_MANUALHOOK1_void(TheWall_Func2, 1, 0, 0, int);
	SH_DECL_MANUALHOOK0(TheWall_Func3, 2, 0, 0, short);
	SH_DECL_MANUALHOOK1(TheWall_Func4, 3, 0, 0, short, int);

	SH_DECL_MANUALHOOK0(TheWall_Func6, 5, 0, 0, const char *);
	SH_DECL_MANUALEXTERN0(TheWall_Func6, const char *);

	SH_DECL_MANUALHOOK0(TheWall_Func7, 6, 0, 0, char *);
	SH_DECL_MANUALEXTERN0(TheWall_Func7, char *);

	SH_DECL_MANUALEXTERN0_void(TheWall_Func1);
	SH_DECL_MANUALEXTERN1(TheWall_Func4, short, int);

	typedef void (SourceHook::EmptyClass::*MFP_Func1)();
	typedef void (SourceHook::EmptyClass::*MFP_Func2)(int);
	typedef short (SourceHook::EmptyClass::*MFP_Func3)();
	typedef short (SourceHook::EmptyClass::*MFP_Func4)(int);

	void Handler_Func1()
	{
		ADD_STATE(State_Func1H_Called(META_IFACEPTR(void)));
	}
	void Handler_Func2(int x)
	{
		ADD_STATE(State_Func2H_Called(META_IFACEPTR(void), x));
		RETURN_META_MNEWPARAMS(MRES_IGNORED, TheWall_Func2, (0x87654321));
	}
	short Handler_Func3()
	{
		ADD_STATE(State_Func3H_Called(META_IFACEPTR(void)));
		return 0;
	}
	short Handler_Func4(int x)
	{
		ADD_STATE(State_Func4H_Called(META_IFACEPTR(void), x));

		// newparams (recall) using the manual hook declaration!
		RETURN_META_VALUE_MNEWPARAMS(MRES_IGNORED, 0, TheWall_Func4, (0x12345678));
	}

	struct AnotherBrick
	{
		void Handler_Func1()
		{
			ADD_STATE(State_Func1H_Called(META_IFACEPTR(void)));
			RETURN_META(MRES_SUPERCEDE);
		}
	};

	TheWall *MyInstanceFactory()
	{
		return new TheWall;
	}
}

bool TestManual(std::string &error)
{
	GET_SHPTR(g_SHPtr);
	g_PLID = 1337;

	TheWall *p = MyInstanceFactory();
	CAutoPtrDestruction<TheWall> apd(p);

	SourceHook::ManualCallClass *cc = SH_GET_MCALLCLASS(p, sizeof(void*));

	// 1)
	// Call each function
	p->Func1();
	p->Func2(200);
	ADD_STATE(State_Return(p->Func3()));
	ADD_STATE(State_Return(p->Func4(400)));

	CHECK_STATES((&g_States,
		new State_Func1_Called(p),
		new State_Func2_Called(p, 200),
		new State_Func3_Called(p),
		new State_Return(3),
		new State_Func4_Called(p, 400),
		new State_Return(4),
		NULL), "Part 1");

	// 1.1)
	// Now call each function through the manual call class, using the hook decl and manually
	SH_MCALL(cc, TheWall_Func1)();
	SH_MCALL2(cc, MFP_Func1(), 0, 0, 0)();
	SH_MCALL(cc, TheWall_Func2)(200);
	SH_MCALL2(cc, MFP_Func2(), 1, 0, 0)(200);
	ADD_STATE(State_Return(SH_MCALL(cc, TheWall_Func3)()));
	ADD_STATE(State_Return(SH_MCALL2(cc, MFP_Func3(), 2, 0, 0)()));
	ADD_STATE(State_Return(SH_MCALL(cc, TheWall_Func4)(400)));
	ADD_STATE(State_Return(SH_MCALL2(cc, MFP_Func4(), 3, 0, 0)(400)));

	CHECK_STATES((&g_States,
		new State_Func1_Called(p),
		new State_Func1_Called(p),
		new State_Func2_Called(p, 200),
		new State_Func2_Called(p, 200),
		new State_Func3_Called(p),
		new State_Return(3),
		new State_Func3_Called(p),
		new State_Return(3),
		new State_Func4_Called(p, 400),
		new State_Return(4),
		new State_Func4_Called(p, 400),
		new State_Return(4),
		NULL), "Part 1.1");

	// 2)
	// Hook each function normally, call them
	SH_ADD_HOOK(TheWall, Func1, p, SH_STATIC(Handler_Func1), false);
	SH_ADD_HOOK(TheWall, Func2, p, SH_STATIC(Handler_Func2), false);
	SH_ADD_HOOK(TheWall, Func3, p, SH_STATIC(Handler_Func3), false);
	SH_ADD_HOOK(TheWall, Func4, p, SH_STATIC(Handler_Func4), false);

	p->Func1();
	p->Func2(200);
	ADD_STATE(State_Return(p->Func3()));
	ADD_STATE(State_Return(p->Func4(400)));

	CHECK_STATES((&g_States,
		new State_Func1H_Called(p),
		new State_Func1_Called(p),
		new State_Func2H_Called(p, 200),
		new State_Func2_Called(p, 0x87654321),
		new State_Func3H_Called(p),
		new State_Func3_Called(p),
		new State_Return(3),
		new State_Func4H_Called(p, 400),
		new State_Func4_Called(p, 0x12345678),
		new State_Return(4),
		NULL), "Part 2");

	// Call them through the mcallclass
	// 2.1)
	// Now call each function through the manual call class, using the hook decl and manually
	SH_MCALL(cc, TheWall_Func1)();
	SH_MCALL2(cc, MFP_Func1(), 0, 0, 0)();
	SH_MCALL(cc, TheWall_Func2)(200);
	SH_MCALL2(cc, MFP_Func2(), 1, 0, 0)(200);
	ADD_STATE(State_Return(SH_MCALL(cc, TheWall_Func3)()));
	ADD_STATE(State_Return(SH_MCALL2(cc, MFP_Func3(), 2, 0, 0)()));
	ADD_STATE(State_Return(SH_MCALL(cc, TheWall_Func4)(400)));
	ADD_STATE(State_Return(SH_MCALL2(cc, MFP_Func4(), 3, 0, 0)(400)));

	CHECK_STATES((&g_States,
		new State_Func1_Called(p),
		new State_Func1_Called(p),
		new State_Func2_Called(p, 200),
		new State_Func2_Called(p, 200),
		new State_Func3_Called(p),
		new State_Return(3),
		new State_Func3_Called(p),
		new State_Return(3),
		new State_Func4_Called(p, 400),
		new State_Return(4),
		new State_Func4_Called(p, 400),
		new State_Return(4),
		NULL), "Part 2.1");

	// Unhook them
	SH_REMOVE_HOOK(TheWall, Func1, p, SH_STATIC(Handler_Func1), false);
	SH_REMOVE_HOOK(TheWall, Func2, p, SH_STATIC(Handler_Func2), false);
	SH_REMOVE_HOOK(TheWall, Func3, p, SH_STATIC(Handler_Func3), false);
	SH_REMOVE_HOOK(TheWall, Func4, p, SH_STATIC(Handler_Func4), false);

	// 3)
	// Hook each function manually, call them

	SH_ADD_MANUALHOOK(TheWall_Func1, p, SH_STATIC(Handler_Func1), false);
	SH_ADD_MANUALHOOK(TheWall_Func2, p, SH_STATIC(Handler_Func2), false);
	SH_ADD_MANUALHOOK(TheWall_Func3, p, SH_STATIC(Handler_Func3), false);
	SH_ADD_MANUALHOOK(TheWall_Func4, p, SH_STATIC(Handler_Func4), false);

	p->Func1();
	p->Func2(200);
	ADD_STATE(State_Return(p->Func3()));
	ADD_STATE(State_Return(p->Func4(400)));

	CHECK_STATES((&g_States,
		new State_Func1H_Called(p),
		new State_Func1_Called(p),
		new State_Func2H_Called(p, 200),
		new State_Func2_Called(p, 0x87654321),
		new State_Func3H_Called(p),
		new State_Func3_Called(p),
		new State_Return(3),
		new State_Func4H_Called(p, 400),
		new State_Func4_Called(p, 0x12345678),
		new State_Return(4),
		NULL), "Part 3");

	// Now through the mcallclass

	// Call them through the mcallclass
	// 3.1)
	// Now call each function through the manual call class, using the hook decl and manually
	SH_MCALL(cc, TheWall_Func1)();
	SH_MCALL2(cc, MFP_Func1(), 0, 0, 0)();
	SH_MCALL(cc, TheWall_Func2)(200);
	SH_MCALL2(cc, MFP_Func2(), 1, 0, 0)(200);
	ADD_STATE(State_Return(SH_MCALL(cc, TheWall_Func3)()));
	ADD_STATE(State_Return(SH_MCALL2(cc, MFP_Func3(), 2, 0, 0)()));
	ADD_STATE(State_Return(SH_MCALL(cc, TheWall_Func4)(400)));
	ADD_STATE(State_Return(SH_MCALL2(cc, MFP_Func4(), 3, 0, 0)(400)));

	CHECK_STATES((&g_States,
		new State_Func1_Called(p),
		new State_Func1_Called(p),
		new State_Func2_Called(p, 200),
		new State_Func2_Called(p, 200),
		new State_Func3_Called(p),
		new State_Return(3),
		new State_Func3_Called(p),
		new State_Return(3),
		new State_Func4_Called(p, 400),
		new State_Return(4),
		new State_Func4_Called(p, 400),
		new State_Return(4),
		NULL), "Part 3.1");

	// Unhook them
	SH_REMOVE_MANUALHOOK(TheWall_Func1, p, SH_STATIC(Handler_Func1), false);
	SH_REMOVE_MANUALHOOK(TheWall_Func2, p, SH_STATIC(Handler_Func2), false);
	SH_REMOVE_MANUALHOOK(TheWall_Func3, p, SH_STATIC(Handler_Func3), false);
	SH_REMOVE_MANUALHOOK(TheWall_Func4, p, SH_STATIC(Handler_Func4), false);

	// 4)
	// Hook each function manually, then normally, call, unhook

	AnotherBrick handler_inst;

	// Why this?
	// 1) tests sh_add_manualhook
	// 2) in my tests, the proto of the manual hook was not equal to the proto of the auto hook
	//    (because there are no attribs for manual hooks).
	//    sourcehook.cpp did a !strcmp(..), so it assigned a new hook manager even though there
	//    already was one for this vfnptr. This hook manager stored the pointer of the original
	//    hook manager's hookfunc as the orig pointer - everything seemingly worked.
	//    The problem with this is that returning MRES_SUPERCEDE (as AnotherBrick::Handler_Func1
	//    does) will supercede the second hook func from being called - thus bypassing the call
	//    of the auto hook here.
	SH_ADD_MANUALHOOK(TheWall_Func1, p, SH_MEMBER(&handler_inst, &AnotherBrick::Handler_Func1), false);
	SH_ADD_MANUALHOOK(TheWall_Func2, p, SH_STATIC(Handler_Func2), false);
	SH_ADD_MANUALHOOK(TheWall_Func3, p, SH_STATIC(Handler_Func3), false);
	SH_ADD_MANUALHOOK(TheWall_Func4, p, SH_STATIC(Handler_Func4), false);

	SH_ADD_HOOK(TheWall, Func1, p, SH_STATIC(Handler_Func1), false);
	SH_ADD_HOOK(TheWall, Func2, p, SH_STATIC(Handler_Func2), false);
	SH_ADD_HOOK(TheWall, Func3, p, SH_STATIC(Handler_Func3), false);
	SH_ADD_HOOK(TheWall, Func4, p, SH_STATIC(Handler_Func4), false);

	p->Func1();
	p->Func2(200);
	ADD_STATE(State_Return(p->Func3()));
	ADD_STATE(State_Return(p->Func4(400)));

	CHECK_STATES((&g_States,
		new State_Func1H_Called(p),
		new State_Func1H_Called(p),
		//new State_Func1_Called(p),
		new State_Func2H_Called(p, 200),
		new State_Func2H_Called(p, 0x87654321),
		new State_Func2_Called(p, 0x87654321),
		new State_Func3H_Called(p),
		new State_Func3H_Called(p),
		new State_Func3_Called(p),
		new State_Return(3),
		new State_Func4H_Called(p, 400),
		new State_Func4H_Called(p, 0x12345678),
		new State_Func4_Called(p, 0x12345678),
		new State_Return(4),
		NULL), "Part 4");

	SH_REMOVE_MANUALHOOK(TheWall_Func1, p, SH_MEMBER(&handler_inst, &AnotherBrick::Handler_Func1), false);
	SH_REMOVE_MANUALHOOK(TheWall_Func2, p, SH_STATIC(Handler_Func2), false);
	SH_REMOVE_MANUALHOOK(TheWall_Func3, p, SH_STATIC(Handler_Func3), false);
	SH_REMOVE_MANUALHOOK(TheWall_Func4, p, SH_STATIC(Handler_Func4), false);

	SH_REMOVE_HOOK(TheWall, Func1, p, SH_STATIC(Handler_Func1), false);
	SH_REMOVE_HOOK(TheWall, Func2, p, SH_STATIC(Handler_Func2), false);
	SH_REMOVE_HOOK(TheWall, Func3, p, SH_STATIC(Handler_Func3), false);
	SH_REMOVE_HOOK(TheWall, Func4, p, SH_STATIC(Handler_Func4), false);

	// 5) Reconfigure TheWall_Func1 to actually hook Func5:
	SH_MANUALHOOK_RECONFIGURE(TheWall_Func1, 4, 0, 0);
	SH_MANUALHOOK_RECONFIGURE(TheWall_Func1, 4, 0, 0);
	SH_ADD_MANUALHOOK(TheWall_Func1, p, SH_STATIC(Handler_Func1), false);

	p->Func5();

	SH_MCALL(p, TheWall_Func1)();

	CHECK_STATES((&g_States,
		new State_Func1H_Called(p),
		new State_Func5_Called(p),
		new State_Func5_Called(p),
		NULL), "Part 5");

	// 6) Test auto-remove on reconfig
	SH_MANUALHOOK_RECONFIGURE(TheWall_Func1, 0, 0, 0);

	p->Func1();
	p->Func5();
	
	CHECK_STATES((&g_States,
		new State_Func1_Called(p),
		new State_Func5_Called(p),
		NULL), "Part 6");

	return true;
}
