#include <string>
#include "sourcehook_test.h"
#include "sh_pagealloc.h"
#include "testevents.h"

namespace
{
	StateList g_States;
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

	MAKE_STATE(State_Func_Called);
	MAKE_STATE(State_Pre1_Called);
	MAKE_STATE(State_Pre2_Called);

	int g_CallNumber;

	class Test
	{
	public:
		virtual void Func()
		{
			ADD_STATE(State_Func_Called);
		}
	};

	Test *g_pInst;

	void Handler_Func_Pre1()
	{
		ADD_STATE(State_Pre1_Called);
	}

	SH_DECL_HOOK0_void(Test, Func, SH_NOATTRIB, 0);

	void Handler_Func_Pre2()
	{
		ADD_STATE(State_Pre2_Called);

		++g_CallNumber;
		if (g_CallNumber == 2)
		{
			// REMOVE ourselves
			SH_REMOVE_HOOK(Test, Func, g_pInst, SH_STATIC(Handler_Func_Pre1), false);
			SH_REMOVE_HOOK(Test, Func, g_pInst, SH_STATIC(Handler_Func_Pre2), false);
		}

		// Call again.
		g_pInst->Func();
	}

	Test *MyInstanceFactory()
	{
		return new Test;
	}

	SourceHook::CPageAlloc g_ThunkAllocator(2);
	void *g_OddThunkMemory = NULL;
	void *g_OddThunk = NULL;

	// Guarantees that Func()
	// has a vtable entry showing to an ODD address
	void PatchFuncWithOddThunk()
	{
		g_OddThunkMemory = g_ThunkAllocator.Alloc(10);
		unsigned char* base = reinterpret_cast<unsigned char*>(g_OddThunkMemory);

		if (((ptrdiff_t)base) & 1)
		{
			// ODD, ok.
		}
		else
		{
			// EVEN. make odd.
			base += 1;
		}

		// Get vtable entry pointer
		SourceHook::MemFuncInfo info = {true, -1, 0, 0};
		SourceHook::GetFuncInfo(g_pInst, &Test::Func, info);

		void *adjustediface = NULL;
		void **cur_vtptr = NULL;
		void **cur_vfnptr = NULL;

		adjustediface = reinterpret_cast<void*>(reinterpret_cast<char*>(g_pInst) + info.thisptroffs);

		cur_vtptr = *reinterpret_cast<void***>(
			reinterpret_cast<char*>(adjustediface) + info.vtbloffs);
		cur_vfnptr = reinterpret_cast<void**>(cur_vtptr + info.vtblindex);

		
		// Original function
		void *origEntry = *cur_vfnptr;

		// Now generate the jump code
		g_ThunkAllocator.SetRW(g_OddThunkMemory);

		*(base + 0) = 0xE9;		// offset jump, immediate operand
		ptrdiff_t *offsetAddr = reinterpret_cast<ptrdiff_t*>(base + 1);
				
		// destination = src + offset + 5
		// <=>  offset = destination - src - 5
		*offsetAddr =
			(reinterpret_cast<unsigned char*>(origEntry) - base) - 5;

		g_ThunkAllocator.SetRE(g_OddThunkMemory);
		
		g_OddThunk = reinterpret_cast<void*>(base);

		// Now set the odd thunk as new vtable entry
		SourceHook::SetMemAccess((void*)cur_vfnptr, sizeof(void*), SH_MEM_READ | SH_MEM_WRITE);
		*cur_vfnptr = g_OddThunk;
	}

	void FreeOddThunk()
	{
		g_ThunkAllocator.Free(g_OddThunkMemory);
		g_OddThunk = NULL;
	}
}

bool TestOddThunks(std::string &error)
{
	GET_SHPTR(g_SHPtr);
	g_PLID = 1337;

	g_CallNumber = 0;

	g_pInst = MyInstanceFactory();

	PatchFuncWithOddThunk();

	SH_ADD_HOOK(Test, Func, g_pInst, SH_STATIC(Handler_Func_Pre1), false);
	SH_ADD_HOOK(Test, Func, g_pInst, SH_STATIC(Handler_Func_Pre2), false);

	g_pInst->Func();

	CHECK_STATES((&g_States,
		// FIRST func() call
		new State_Pre1_Called(),
		new State_Pre2_Called(),		// calls Func() again

		// SECOND func() call
		new State_Pre1_Called(),
		new State_Pre2_Called(),		// removes hooks and calls Func() again

		// THIRD func() call
		new State_Func_Called(),

		// SECOND func() call
		new State_Func_Called(),

		// FIRST func() call
		new State_Func_Called(),
		NULL), "Part 1");

	delete g_pInst;
	FreeOddThunk();

	return true;
}

