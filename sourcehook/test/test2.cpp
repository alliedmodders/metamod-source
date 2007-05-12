#include <string>
#include "sourcehook_test.h"
#include "testevents.h"
#include <stdarg.h>

// TEST2
// Vafmt and Overloaded functions
namespace
{
	StateList g_States;
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

	MAKE_STATE_1(State_EatYams_Called, int);
	MAKE_STATE_1(State_EatYams_Handler_Called, int);
	MAKE_STATE_2(State_Vafmt_Called, int, std::string);
	MAKE_STATE_2(State_Vafmt_PreHandler_Called, int, std::string);
	MAKE_STATE_2(State_Vafmt_PostHandler_Called, int, std::string);

	class IGaben
	{
	public:
		virtual void EatYams()
		{
			ADD_STATE(State_EatYams_Called(0));
		}
		virtual bool EatYams(const char *location) const
		{
			ADD_STATE(State_EatYams_Called(1));
			return true;
		}
		virtual void Vafmt1(bool param1, int x, const char *fmt, ...)
		{
			va_list ap;
			va_start(ap, fmt);
			char buffer[512];
			vsprintf(buffer, fmt, ap);
			va_end(ap);
			ADD_STATE(State_Vafmt_Called(1, std::string(buffer)));
		}
		virtual float Vafmt2(const char *fmt, ...)
		{
			va_list ap;
			va_start(ap, fmt);
			char buffer[512];
			vsprintf(buffer, fmt, ap);
			va_end(ap);
			ADD_STATE(State_Vafmt_Called(2, std::string(buffer)));
			return 0.0f;
		}
	};

	// GCC's optimizer is too good. I had to add this in order to make it execute a virtual table lookup!
	class Whatever : public IGaben
	{
	};

	SH_DECL_HOOK0_void(IGaben, EatYams, SH_NOATTRIB, 0);
	SH_DECL_HOOK1(IGaben, EatYams, const, 1, bool, const char *);
	SH_DECL_HOOK2_void_vafmt(IGaben, Vafmt1, SH_NOATTRIB, 0, bool, int);
	SH_DECL_HOOK0_vafmt(IGaben, Vafmt2, SH_NOATTRIB, 0, float);

	void EatYams0_Handler()
	{
		ADD_STATE(State_EatYams_Handler_Called(0));
	}

	bool EatYams1_Handler(const char *loc)
	{
		ADD_STATE(State_EatYams_Handler_Called(1));
		return true;
	}

	void Vafmt1_PreHandler(bool param1, int x, const char *in)
	{
		ADD_STATE(State_Vafmt_PreHandler_Called(1, std::string(in)));
	}
	void Vafmt1_PostHandler(bool param1, int x, const char *in)
	{
		ADD_STATE(State_Vafmt_PostHandler_Called(1, std::string(in)));
	}
	float Vafmt2_PreHandler(const char *in)
	{
		ADD_STATE(State_Vafmt_PreHandler_Called(2, std::string(in)));
		return 0.0f;
	}
	float Vafmt2_PostHandler(const char *in)
	{
		ADD_STATE(State_Vafmt_PostHandler_Called(2, std::string(in)));
		return 0.0f;
	}
}


bool TestVafmtAndOverload(std::string &error)
{
	GET_SHPTR(g_SHPtr);
	g_PLID = 1337;	

	Whatever gabgab;
	IGaben *pGab = &gabgab;

	// Part 1
	SH_CALL(pGab, static_cast<void (IGaben::*)()>(&IGaben::EatYams))();
	SH_CALL(pGab, static_cast<bool (IGaben::*)(const char *) const>(&IGaben::EatYams))("Here!");

	SH_ADD_HOOK(IGaben, EatYams, pGab, EatYams0_Handler, false);
	SH_ADD_HOOK(IGaben, EatYams, pGab, EatYams1_Handler, false);

	pGab->EatYams();
	pGab->EatYams("Here!");

	SH_REMOVE_HOOK(IGaben, EatYams, pGab, EatYams0_Handler, false);
	SH_REMOVE_HOOK(IGaben, EatYams, pGab, EatYams1_Handler, false);

	CHECK_STATES((&g_States, 
		new State_EatYams_Called(0),
		new State_EatYams_Called(1),
		new State_EatYams_Handler_Called(0),
		new State_EatYams_Called(0),
		new State_EatYams_Handler_Called(1),
		new State_EatYams_Called(1),
		NULL),"Part 1");

	// Part 2
	pGab->Vafmt1(true, 55, "Hello %s%d%s", "BA", 1, "L");
	SH_CALL(pGab, &IGaben::Vafmt1)(true, 55, "Hello %s%d%s", "BA", 1, "L");
	pGab->Vafmt2("Hello %s%d%s", "BA", 1, "LOPAN");
	SH_CALL(pGab, &IGaben::Vafmt2)("Hello %s%d%s", "BA", 1, "LOPAN");

	CHECK_STATES((&g_States,
		new State_Vafmt_Called(1, "Hello BA1L"),
		new State_Vafmt_Called(1, "Hello BA1L"),
		new State_Vafmt_Called(2, "Hello BA1LOPAN"),
		new State_Vafmt_Called(2, "Hello BA1LOPAN"),
		NULL), "Part 2");

	// Part 3
	SH_ADD_HOOK(IGaben, Vafmt1, pGab, Vafmt1_PreHandler, false);
	SH_ADD_HOOK(IGaben, Vafmt1, pGab, Vafmt1_PostHandler, true);
	SH_ADD_HOOK(IGaben, Vafmt2, pGab, Vafmt2_PreHandler, false);
	SH_ADD_HOOK(IGaben, Vafmt2, pGab, Vafmt2_PostHandler, true);

	pGab->Vafmt1(true, 55, "Hello %s%d%s", "BA", 1, "L");
	pGab->Vafmt2("Hello %s%d%s", "BA", 1, "LOPAN");

	CHECK_STATES((&g_States,
		new State_Vafmt_PreHandler_Called(1, std::string("Hello BA1L")),
		new State_Vafmt_Called(1, std::string("Hello BA1L")),
		new State_Vafmt_PostHandler_Called(1, std::string("Hello BA1L")),

		new State_Vafmt_PreHandler_Called(2, std::string("Hello BA1LOPAN")),
		new State_Vafmt_Called(2, std::string("Hello BA1LOPAN")),
		new State_Vafmt_PostHandler_Called(2, std::string("Hello BA1LOPAN")),
		NULL), "Part 3");

	// Part 4
	SH_REMOVE_HOOK(IGaben, Vafmt1, pGab, Vafmt1_PreHandler, false);
	SH_REMOVE_HOOK(IGaben, Vafmt1, pGab, Vafmt1_PostHandler, true);
	SH_REMOVE_HOOK(IGaben, Vafmt2, pGab, Vafmt2_PreHandler, false);
	SH_REMOVE_HOOK(IGaben, Vafmt2, pGab, Vafmt2_PostHandler, true);

	pGab->Vafmt1(true, 55, "Hello %s%d%s", "BA", 1, "L");
	pGab->Vafmt2("Hello %s%d%s", "BA", 1, "LOPAN");

	CHECK_STATES((&g_States,
		new State_Vafmt_Called(1, "Hello BA1L"),
		new State_Vafmt_Called(2, "Hello BA1LOPAN"),
		NULL), "Part 4");

	return true;
}
