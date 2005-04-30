#include <string>
#include "sourcehook_impl.h"
#include "testevents.h"

namespace
{
	StateList g_States;
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

	struct State_EatYams_Called : State
	{
		int m_Overload;
		State_EatYams_Called(int ovrld) : m_Overload(ovrld)
		{
		}
		bool IsEqual(State *other)
		{
			State_EatYams_Called *other2 = dynamic_cast<State_EatYams_Called*>(other);
			if (!other2)
				return false;
			return other2->m_Overload == m_Overload;
		}
	};

	struct State_EatYams_Handler_Called : State
	{
		int m_Overload;
		State_EatYams_Handler_Called(int ovrld) : m_Overload(ovrld)
		{
		}
		bool IsEqual(State *other)
		{
			State_EatYams_Handler_Called *other2 = dynamic_cast<State_EatYams_Handler_Called*>(other);
			if (!other2)
				return false;
			return other2->m_Overload == m_Overload;
		}
	};


	// TEST1
	// Basic tests
	// Hooking and callclass
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
	};

	SH_DECL_HOOK0_void(IGaben, EatYams, SH_NOATTRIB, 0);
	SH_DECL_HOOK1(IGaben, EatYams, const, 1, bool, const char *);
}

void EatYams0_Handler()
{
	ADD_STATE(State_EatYams_Handler_Called(0));
}

bool EatYams1_Handler(const char *loc)
{
	ADD_STATE(State_EatYams_Handler_Called(1));
	return true;
}

bool TestVafmtAndOverload(std::string &error)
{
	SourceHook::CSourceHookImpl g_SHImpl;
	g_SHPtr = &g_SHImpl;
	g_PLID = 1337;	

	IGaben gabgab;
	IGaben *pGab = &gabgab;

	SourceHook::CallClass<IGaben> *cc = SH_GET_CALLCLASS(pGab);

	SH_CALL(cc, static_cast<void (IGaben::*)()>(&IGaben::EatYams))();
	SH_CALL(cc, static_cast<bool (IGaben::*)(const char *) const>(&IGaben::EatYams))("Here!");

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

	SH_RELEASE_CALLCLASS(cc);

	return true;
}