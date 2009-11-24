#include <string>
#include "sourcehook.h"
#include "sourcehook_test.h"
#include "testevents.h"

// This should probably be done the other way round!
#if SH_XP == SH_XP_POSIX
#define _snprintf snprintf
#endif

namespace
{
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

	unsigned int g_callcount[10];

	class VMultiTest
	{
	public:
		VMultiTest(unsigned int index) : m_idx(index)
		{
		}
	public:
		virtual void HookTarget()
		{
		}
		virtual void Notify()
		{
			g_callcount[this->m_idx]++;
		}
	private:
		unsigned int m_idx;
	};

	void HookFunction()
	{
		VMultiTest *pv = META_IFACEPTR(VMultiTest);
		pv->Notify();
	}

	SH_DECL_HOOK0_void(VMultiTest, HookTarget, SH_NOATTRIB, false);
};

bool TestMulti(std::string &error)
{
	GET_SHPTR(g_SHPtr);
	g_PLID = 1337;

	VMultiTest **pv = new VMultiTest *[10];
	for (unsigned int i=0; i<10; i++)
		pv[i] = new VMultiTest(i);
	

	for (unsigned int i=0; i<10; i++)
		SH_ADD_HOOK(VMultiTest, HookTarget, pv[i], SH_STATIC(HookFunction), false);

	pv[0]->HookTarget();

	if (g_callcount[0] != 1)
	{
		error.assign("g_callcount[0] != 0");
		return false;
	}

	for (unsigned int i=1; i<10; i++)
	{
		if (g_callcount[i])
		{
			error.assign("g_callcount[n] != 0");
			return false;
		}
	}

	SH_REMOVE_HOOK(VMultiTest, HookTarget, pv[0], SH_STATIC(HookFunction), false);

	for (unsigned int i=1; i<10; i++)
		pv[i]->HookTarget();

	if (g_callcount[0] != 1)
	{
		error.assign("g_callcount[0] != 0");
		return false;
	}

	for (unsigned int i=1; i<10; i++)
	{
		if (g_callcount[i] != 1)
		{
			char err[256];
			_snprintf(err, sizeof(err)-1, "g_callcount[%d] != 1", i);
			error.assign(err);
			return false;
		}
	}

	for (unsigned int i=0; i<10; i++)
	{
		SH_REMOVE_HOOK(VMultiTest, HookTarget, pv[1], SH_STATIC(HookFunction), false);
		delete pv[i];
	}

	delete [] pv;

	return true;
}
