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
			char buffer[9999];
			vsnprintf(buffer, 9998, fmt, ap);
			buffer[9998] = 0;
			va_end(ap);
			ADD_STATE(State_Vafmt_Called(1, std::string(buffer)));
		}
		virtual float Vafmt2(const char *fmt, ...)
		{
			va_list ap;
			va_start(ap, fmt);
			char buffer[9999];
			vsnprintf(buffer, 9998, fmt, ap);
			buffer[9998] = 0;
			va_end(ap);
			ADD_STATE(State_Vafmt_Called(2, std::string(buffer)));
			return 0.0f;
		}
	};

	SH_DECL_HOOK0_void(IGaben, EatYams, SH_NOATTRIB, 0);
	SH_DECL_HOOK1(IGaben, EatYams, const, 1, bool, const char *);
	SH_DECL_HOOK2_void_vafmt(IGaben, Vafmt1, SH_NOATTRIB, 0, bool, int);
	SH_DECL_HOOK0_vafmt(IGaben, Vafmt2, SH_NOATTRIB, 0, float);

	SH_DECL_EXTERN2_void_vafmt(IGaben, Vafmt1, SH_NOATTRIB, 0, bool, int);
	SH_DECL_EXTERN0_vafmt(IGaben, Vafmt2, SH_NOATTRIB, 0, float);

	// Hook also using manual hooks!
	SH_DECL_MANUALHOOK0_vafmt(IGaben_Vafmt2, 3, 0, 0, float);
	SH_DECL_MANUALEXTERN0_vafmt(IGaben_Vafmt2, float);

	SH_DECL_MANUALHOOK2_void_vafmt(IGaben_Vafmt1, 2, 0, 0, bool, int);
	SH_DECL_MANUALEXTERN2_void_vafmt(IGaben_Vafmt1, bool, int);

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

	IGaben *MyInstanceFactory()
	{
		return new IGaben;
	}
}


bool TestVafmtAndOverload(std::string &error)
{
	int h1, h2, h3, h4;
	GET_SHPTR(g_SHPtr);
	g_PLID = 1337;	

	IGaben *pGab = MyInstanceFactory();
	CAutoPtrDestruction<IGaben> apd(pGab);

	// Part 1
	SH_CALL(pGab, static_cast<void (IGaben::*)()>(&IGaben::EatYams))();
	SH_CALL(pGab, static_cast<bool (IGaben::*)(const char *) const>(&IGaben::EatYams))("Here!");

	h1 = SH_ADD_HOOK(IGaben, EatYams, pGab, EatYams0_Handler, false);
	h2 = SH_ADD_HOOK(IGaben, EatYams, pGab, EatYams1_Handler, false);

	pGab->EatYams();
	pGab->EatYams("Here!");

	SH_REMOVE_HOOK_ID(h1);
	SH_REMOVE_HOOK_ID(h2);

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
	h1 = SH_ADD_HOOK(IGaben, Vafmt1, pGab, Vafmt1_PreHandler, false);
	h2 = SH_ADD_HOOK(IGaben, Vafmt1, pGab, Vafmt1_PostHandler, true);
	h3 = SH_ADD_HOOK(IGaben, Vafmt2, pGab, Vafmt2_PreHandler, false);
	h4 = SH_ADD_HOOK(IGaben, Vafmt2, pGab, Vafmt2_PostHandler, true);

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

	
	// Check LARGE stuff
	char tmpbuf[501];
	for (int i = 0; i < 500; ++i)
		tmpbuf[i] = (i % 10) + '0';
	tmpbuf[500] = 0;

	pGab->Vafmt2("%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
		tmpbuf, tmpbuf, tmpbuf, tmpbuf, tmpbuf, tmpbuf, tmpbuf, tmpbuf, tmpbuf, tmpbuf,
		tmpbuf, tmpbuf, tmpbuf, tmpbuf, tmpbuf, tmpbuf, tmpbuf, tmpbuf, tmpbuf, tmpbuf);

	char refbuf[SourceHook::STRBUF_LEN]; 
	for (int i = 0; i < (SourceHook::STRBUF_LEN - 1); ++i)
		refbuf[i] = (i % 10) + '0';
	refbuf[SourceHook::STRBUF_LEN - 1] = 0;

	CHECK_STATES((&g_States,
		new State_Vafmt_PreHandler_Called(2, std::string(refbuf)),
		new State_Vafmt_Called(2, std::string(refbuf)),
		new State_Vafmt_PostHandler_Called(2, std::string(refbuf)),
		NULL
		), "Part 3.1");

	// Part 4
	SH_REMOVE_HOOK_ID(h1);
	SH_REMOVE_HOOK_ID(h2);
	SH_REMOVE_HOOK_ID(h3);
	SH_REMOVE_HOOK_ID(h4);

	pGab->Vafmt1(true, 55, "Hello %s%d%s", "BA", 1, "L");
	pGab->Vafmt2("Hello %s%d%s", "BA", 1, "LOPAN");

	CHECK_STATES((&g_States,
		new State_Vafmt_Called(1, "Hello BA1L"),
		new State_Vafmt_Called(2, "Hello BA1LOPAN"),
		NULL), "Part 4");

	// Part5: Part3 but with manualhooks
	h1 = SH_ADD_MANUALHOOK(IGaben_Vafmt1, pGab, SH_STATIC(Vafmt1_PreHandler), false);
	h2 = SH_ADD_MANUALHOOK(IGaben_Vafmt1, pGab, SH_STATIC(Vafmt1_PostHandler), true);
	h3 = SH_ADD_MANUALHOOK(IGaben_Vafmt2, pGab, SH_STATIC(Vafmt2_PreHandler), false);
	h4 = SH_ADD_MANUALHOOK(IGaben_Vafmt2, pGab, SH_STATIC(Vafmt2_PostHandler), true);

	pGab->Vafmt1(true, 55, "Hello %s%d%s", "BA", 1, "L");
	SH_CALL(pGab, &IGaben::Vafmt1)(true, 55, "Hello %s%d%s", "BA", 1, "L");
	SH_MCALL(pGab, IGaben_Vafmt1)(true, 55, "Hello %s%d%s", "BA", 1, "L");

	CHECK_STATES((&g_States,
		new State_Vafmt_PreHandler_Called(1, std::string("Hello BA1L")),
		new State_Vafmt_Called(1, std::string("Hello BA1L")),
		new State_Vafmt_PostHandler_Called(1, std::string("Hello BA1L")),

		// SH_CALL and SH_MCALL
		new State_Vafmt_Called(1, std::string("Hello BA1L")),
		new State_Vafmt_Called(1, std::string("Hello BA1L")),
		NULL), "Part 5.1");

	pGab->Vafmt2("Hello %s%d%s", "BA", 1, "LOPAN");
	SH_CALL(pGab, &IGaben::Vafmt2)("Hello %s%d%s", "BA", 1, "LOPAN");
	SH_MCALL(pGab, IGaben_Vafmt2)("Hello %s%d%s", "BA", 1, "LOPAN");

	CHECK_STATES((&g_States,
		new State_Vafmt_PreHandler_Called(2, std::string("Hello BA1LOPAN")),
		new State_Vafmt_Called(2, std::string("Hello BA1LOPAN")),
		new State_Vafmt_PostHandler_Called(2, std::string("Hello BA1LOPAN")),

		// SH_CALL and SH_MCALL
		new State_Vafmt_Called(2, "Hello BA1LOPAN"),
		new State_Vafmt_Called(2, "Hello BA1LOPAN"),
		NULL), "Part 5.2");

	return true;
}
