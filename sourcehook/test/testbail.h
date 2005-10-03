//////////////////////////////////////////////////////////////////////////
// Shared data for testbail

#include <string>
#include "sourcehook_impl.h"
#include "testevents.h"
#include <stdarg.h>
#include <list>
#include <algorithm>

void ___TestBail2();

namespace
{
	StateList g_States;
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

	MAKE_STATE(State_EatYams_Called);
	MAKE_STATE(State_EatYams_Handler_Called);


	class IGaben
	{
	public:
		virtual void EatYams()
		{
			ADD_STATE(State_EatYams_Called);
		}
	};

	SH_DECL_HOOK0_void(IGaben, EatYams, SH_NOATTRIB, 0);

	/*
	SHINT_MAKE_GENERICSTUFF_BEGIN(IGaben, EatYams, 0, (static_cast<void (IGaben::*)() SH_NOATTRIB>
		(&IGaben::EatYams)))
		typedef fastdelegate::FastDelegate0<> FD;
		virtual void Func()
		{
			// SH_HANDLEFUNC_void(IGaben, EatYams, (), ());

			SH_SETUPCALLS_void((), ())
			SH_CALL_HOOKS_void(pre, ())
			if (status != MRES_SUPERCEDE)
			{
				void (EmptyClass::*mfp)();
				SH_SETUP_MFP(mfp);
				(reinterpret_cast<EmptyClass*>(ifinfo->GetPtr())->*mfp)();
			}
			SH_CALL_HOOKS_void(post, ())
			SH_RETURN_void()
		}
	};
	SH_FHCls(IGaben,EatYams,0) SH_FHCls(IGaben,EatYams,0)::ms_Inst;
	::SourceHook::MemFuncInfo SH_FHCls(IGaben,EatYams,0)::ms_MFI;
	::SourceHook::IHookManagerInfo *SH_FHCls(IGaben,EatYams,0)::ms_HI;
	const char *SH_FHCls(IGaben,EatYams,0)::ms_Proto = "SH_NOATTRIB";
	bool __SourceHook_FHAddIGabenEatYams(void *iface, bool post,
		SH_FHCls(IGaben,EatYams,0)::FD handler)
	{
		using namespace ::SourceHook;
		MemFuncInfo mfi;
		GetFuncInfo((static_cast<void (IGaben::*)() SH_NOATTRIB>(&IGaben::EatYams)), mfi);
		if (mfi.thisptroffs < 0)
			return false;

		return SH_GLOB_SHPTR->AddHook(SH_GLOB_PLUGPTR, iface, mfi.thisptroffs,
			SH_FHCls(IGaben,EatYams,0)::HookManPubFunc,
			new CSHDelegate<SH_FHCls(IGaben,EatYams,0)::FD>(handler), post);
	}
	bool __SourceHook_FHRemoveIGabenEatYams(void *iface, bool post,
		SH_FHCls(IGaben,EatYams,0)::FD handler)
	{
		using namespace ::SourceHook;
		MemFuncInfo mfi;
		GetFuncInfo((static_cast<void (IGaben::*)() SH_NOATTRIB>(&IGaben::EatYams)), mfi);
		if (mfi.thisptroffs < 0)
			return false;

		CSHDelegate<SH_FHCls(IGaben,EatYams,0)::FD> tmp(handler);
		return SH_GLOB_SHPTR->RemoveHook(SH_GLOB_PLUGPTR, iface, mfi.thisptroffs,
			SH_FHCls(IGaben,EatYams,0)::HookManPubFunc, &tmp, post);
	}
	*/

	void EatYams0_Handler()
	{
		ADD_STATE(State_EatYams_Handler_Called);
	}

	IGaben *g_Gabgab;
}

extern void *___testbail_gabgab;
