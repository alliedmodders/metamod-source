// TESTBAIL
// Test for a bug Bail has found

#include "testbail.h"

void *___testbail_gabgab;

namespace
{
	class zomg_lolz
	{
	public:
		virtual void zomg()
		{
		}
	};
	SH_DECL_HOOK0_void(zomg_lolz, zomg, SH_NOATTRIB, 0);
	void Handler()
	{
		SH_REMOVE_HOOK_STATICFUNC(zomg_lolz, zomg, reinterpret_cast<zomg_lolz*>(META_IFACEPTR),
			Handler, false);
	}
	void Handler2()
	{
	}
}

bool TestBail(std::string &error)
{
	SourceHook::CSourceHookImpl g_SHImpl;
	g_SHPtr = &g_SHImpl;
	g_PLID = 1;

	g_Gabgab = new IGaben;
	___testbail_gabgab = (void*)g_Gabgab;

	g_Gabgab->EatYams();

	SH_ADD_HOOK_STATICFUNC(IGaben, EatYams, g_Gabgab, EatYams0_Handler, false);

	g_Gabgab->EatYams();

	___TestBail2();
	
	g_Gabgab->EatYams();

	SH_REMOVE_HOOK_STATICFUNC(IGaben, EatYams, g_Gabgab, EatYams0_Handler, false);

	g_Gabgab->EatYams();

	delete g_Gabgab;

	// If it didn't crash, it's ok

	// NEW TEST: Remove hook from handler
	zomg_lolz inst;
	SH_ADD_HOOK_STATICFUNC(zomg_lolz, zomg, &inst, Handler, false);
	SH_ADD_HOOK_STATICFUNC(zomg_lolz, zomg, &inst, Handler2, false);

	zomg_lolz *mwah = &inst;
	mwah->zomg();
	mwah->zomg();

	SH_ADD_HOOK_STATICFUNC(zomg_lolz, zomg, &inst, Handler, false);
	SH_REMOVE_HOOK_STATICFUNC(zomg_lolz, zomg, &inst, Handler2, false);

	mwah->zomg();
	mwah->zomg();

	// Shouldn't crash again...

	return true;
}

