#include <string>
#include "sourcehook.h"
#include "sourcehook_test.h"
#include "testevents.h"
#include "sourcehook_hookmangen.h"

// TESTHOOKMANGEN
// Test automatic hookman generation
//  Tests void and non-void functions
//  0 to 6 params:
//   integer-type, float-type, plain-old-data struct and objects with ctors/dtors
//   both byval and byref
//  also tests ignore/supercede
//  also tests recalls

// :TODO: test override as well

namespace
{
	#include "testhookmangen.h"

	StateList g_States;
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

	// PtrBuf(ptr) gives ptrs unique numbers
	// in the order they appear
	SourceHook::List<const void*> g_PtrHash;

	bool g_Inside_LeafFunc = false;			// inside a hook or a func

	template <class T>
	int PtrBuf(T ptr)
	{
		int a = 0;

		const void *vptr = reinterpret_cast<const void*>(ptr);
		for (SourceHook::List<const void*>::iterator iter = g_PtrHash.begin(); iter != g_PtrHash.end(); ++iter)
		{
			if (*iter == vptr)
				return a;
			else
				++a;
		}
		g_PtrHash.push_back(vptr);
		return static_cast<int>(g_PtrHash.size()) - 1;
	}

	template <class T>
	T PtrBufPtr(T ptr)
	{
		PtrBuf(ptr);
		return ptr;
	}

	void PtrBuf_Clear(int leave_in = 0)
	{
		for (SourceHook::List<const void*>::iterator iter = g_PtrHash.begin(); iter != g_PtrHash.end();)
		{
			if (--leave_in < 0)
				iter = g_PtrHash.erase(iter);
			else
				++iter;
		}
	}

	// POD / Object types
	template <int MYSIZE>
	struct POD
	{
		char x[MYSIZE];

		bool operator==(const POD<MYSIZE> &other)
		{
			return memcmp(reinterpret_cast<void*>(x), reinterpret_cast<const void*>(other.x), MYSIZE) == 0;
		}
	};

	template <int MYSIZE>
	std::ostream& operator <<(std::ostream &os, const POD<MYSIZE> &obj)
	{
		os << "Some POD!";
		return os;
	}

	MAKE_STATE_1(State_ObjOCtor_Called, int /*MYSIZE*/);
	MAKE_STATE_1(State_ObjCCtor_Called, int /*MYSIZE*/);
	MAKE_STATE_1(State_ObjODtor_Called, int /*MYSIZE*/);

	template <int MYSIZE>
	struct Object
	{
		char x[MYSIZE];

		Object(char initch)
		{
			memset(reinterpret_cast<void*>(x), initch, MYSIZE);
			if (!g_Inside_LeafFunc)
				ADD_STATE(State_ObjOCtor_Called(MYSIZE));
		}

		Object()
		{
			if (!g_Inside_LeafFunc)
				ADD_STATE(State_ObjOCtor_Called(MYSIZE));
		}

		Object(const Object<MYSIZE> & other)
		{
			memcpy(reinterpret_cast<void*>(x), reinterpret_cast<const void*>(other.x), MYSIZE);
			if (!g_Inside_LeafFunc)
				ADD_STATE(State_ObjCCtor_Called(MYSIZE));
		}

		~Object()
		{
			if (!g_Inside_LeafFunc)
				ADD_STATE(State_ObjODtor_Called(MYSIZE));
		}

		bool operator==(const Object<MYSIZE> &other)
		{
			return memcmp(reinterpret_cast<void*>(x), reinterpret_cast<const void*>(other.x), MYSIZE) == 0;
		}
	};

	template <int MYSIZE>
	std::ostream& operator <<(std::ostream &os, const Object<MYSIZE> &obj)
	{
		os << "Some Obj!";
		return os;
	}


	// "Increment" something:  (reproducible change for recall tests)
	//  integer: ++
	//  float: += 1.3
	//  pod/object: x[i]++, 0 <= i < MYSIZE
	template <class T>
	struct Increment
	{
		static void Incr(T &what)	
		{
			++what;
		}
	};

	template<>
	struct Increment<float>
	{
		static void Incr(float &what)	
		{
			what += 1.3f;
		}
	};

	template<>
	struct Increment<double>
	{
		static void Incr(double &what)	
		{
			what += 1.3;
		}
	};

	template<int MYSIZE>
	struct Increment< POD<MYSIZE> >
	{
		static void Incr(POD<MYSIZE> &what)	
		{
			for (int i = 0; i < MYSIZE; ++i)
				++ what.x[i];
		}
	};

	template<int MYSIZE>
	struct Increment< Object<MYSIZE> >
	{
		static void Incr(Object<MYSIZE> &what)	
		{
			for (int i = 0; i < MYSIZE; ++i)
				++ what.x[i];
		}
	};

	// MyDelegate base class for other delegates
	class MyDelegate : public SourceHook::ISHDelegate
	{
		// Unneeded
		//  because we don't use old SH_REMOVE_HOOK syntax
		virtual bool IsEqual(SourceHook::ISHDelegate *pOtherDeleg)
		{
			return false;
		}

		virtual void DeleteThis()
		{
			delete this;
		}
	};

	THGM_MAKE_TEST0_void(0);
	THGM_SETUP_PI0(0);

	THGM_MAKE_TEST1_void(1, char);
	THGM_SETUP_PI1(1, char, SourceHook::PassInfo::PassType_Basic, SourceHook::PassInfo::PassFlag_ByVal);

	THGM_MAKE_TEST1_void(2, short);
	THGM_SETUP_PI1(2, short, SourceHook::PassInfo::PassType_Basic, SourceHook::PassInfo::PassFlag_ByVal);

	THGM_MAKE_TEST1_void(3, int);
	THGM_SETUP_PI1(3, int, SourceHook::PassInfo::PassType_Basic, SourceHook::PassInfo::PassFlag_ByVal);

	THGM_MAKE_TEST1_void(4, float);
	THGM_SETUP_PI1(4, float, SourceHook::PassInfo::PassType_Float, SourceHook::PassInfo::PassFlag_ByVal);

	THGM_MAKE_TEST1_void(5, double);
	THGM_SETUP_PI1(5, double, SourceHook::PassInfo::PassType_Float, SourceHook::PassInfo::PassFlag_ByVal);
	
	THGM_MAKE_TEST5_void(6, char, short, int, float, double);
	THGM_SETUP_PI5(6,
		char, SourceHook::PassInfo::PassType_Basic, SourceHook::PassInfo::PassFlag_ByVal,
		short, SourceHook::PassInfo::PassType_Basic, SourceHook::PassInfo::PassFlag_ByVal,
		int, SourceHook::PassInfo::PassType_Basic, SourceHook::PassInfo::PassFlag_ByVal,
		float, SourceHook::PassInfo::PassType_Float, SourceHook::PassInfo::PassFlag_ByVal,
		double, SourceHook::PassInfo::PassType_Float, SourceHook::PassInfo::PassFlag_ByVal
		);

	THGM_MAKE_TEST2_void(7, char&, double&);
	THGM_SETUP_PI2(7,
		char&, SourceHook::PassInfo::PassType_Basic, SourceHook::PassInfo::PassFlag_ByRef,
		double&, SourceHook::PassInfo::PassType_Float, SourceHook::PassInfo::PassFlag_ByRef
		);

	THGM_MAKE_TEST1_void(8, POD<7>);
	THGM_SETUP_PI1(8, POD<7>, SourceHook::PassInfo::PassType_Object, SourceHook::PassInfo::PassFlag_ByVal);

	THGM_MAKE_TEST1_void(9, POD<600>);
	THGM_SETUP_PI1(9, POD<600>, SourceHook::PassInfo::PassType_Object, SourceHook::PassInfo::PassFlag_ByVal);

	THGM_MAKE_TEST1_void(10, POD<600> &);
	THGM_SETUP_PI1(10, POD<600> &, SourceHook::PassInfo::PassType_Object, SourceHook::PassInfo::PassFlag_ByRef);

	THGM_MAKE_TEST2_void(11, Object<3>, Object<600>&);
	THGM_SETUP_PI2(11,
		Object<3>, SourceHook::PassInfo::PassType_Object, SourceHook::PassInfo::PassFlag_ByVal | SourceHook::PassInfo::PassFlag_OCtor | SourceHook::PassInfo::PassFlag_ODtor | SourceHook::PassInfo::PassFlag_CCtor,
		Object<600> &, SourceHook::PassInfo::PassType_Object, SourceHook::PassInfo::PassFlag_ByRef | SourceHook::PassInfo::PassFlag_OCtor | SourceHook::PassInfo::PassFlag_ODtor | SourceHook::PassInfo::PassFlag_CCtor
		);

	THGM_MAKE_TEST0(101, char);
	THGM_SETUP_PI0(101);
	THGM_SETUP_RI(101, char, SourceHook::PassInfo::PassType_Basic, SourceHook::PassInfo::PassFlag_ByVal);

	THGM_MAKE_TEST0(102, short);
	THGM_SETUP_PI0(102);
	THGM_SETUP_RI(102, short, SourceHook::PassInfo::PassType_Basic, SourceHook::PassInfo::PassFlag_ByVal);

	THGM_MAKE_TEST0(103, int);
	THGM_SETUP_PI0(103);
	THGM_SETUP_RI(103, int, SourceHook::PassInfo::PassType_Basic, SourceHook::PassInfo::PassFlag_ByVal);

	THGM_MAKE_TEST0(104, float);
	THGM_SETUP_PI0(104);
	THGM_SETUP_RI(104, float, SourceHook::PassInfo::PassType_Float, SourceHook::PassInfo::PassFlag_ByVal);

	THGM_MAKE_TEST0(105, double);
	THGM_SETUP_PI0(105);
	THGM_SETUP_RI(105, double, SourceHook::PassInfo::PassType_Float, SourceHook::PassInfo::PassFlag_ByVal);
}


bool TestHookManGen(std::string &error)
{
	GET_SHPTR(g_SHPtr);
	g_PLID = 1337;

	THGM_DO_TEST_void(0, ());

	THGM_DO_TEST_void(1, (100));
	
	THGM_DO_TEST_void(2, (0x1F00));

	THGM_DO_TEST_void(3, (0x1F000000));

	THGM_DO_TEST_void(4, (0.5f));

	THGM_DO_TEST_void(5, (5.5));

	THGM_DO_TEST_void(6, (100, 0x1f00, 0x1f000000, 0.5f, 5.5));

	char a = 5;
	double b = 233.33;
	THGM_DO_TEST_void(7, (a, b));

	POD<7> pod7 = {78};
	THGM_DO_TEST_void(8, (pod7));

	POD<600> pod600 = {34};
	THGM_DO_TEST_void(9, (pod600));
	
	THGM_DO_TEST_void(10, (pod600));

	// Test11: Special: constructors/destructors
	PtrBuf_Clear();
	Object<3> obj3(33);
	Object<600> obj600(21);

	CHECK_STATES((&g_States,
		new State_ObjOCtor_Called(3),
		new State_ObjOCtor_Called(600),
		NULL), "Test11 Part0");

	setuppi_11();
	g_Genc11 = new SourceHook::Impl::GenContext(&protoinfo_11, 0, 0, g_SHPtr);
	g_Genc_ad11.set(g_Genc11);
	SourceHook::HookManagerPubFunc myhookman11 = g_Genc11->Generate();
	int hook1_11, hook2_11, hook3_11, hook4_11;
	
	TestClass11 *pTest11 = new TestClass11;
	CAutoPtrDestruction<TestClass11> apd11(pTest11);
	
	/* no hooks - no hooks */
	PtrBuf_Clear();
	pTest11->Func(obj3, obj600);

	g_Inside_LeafFunc = true;
	CHECK_STATES((&g_States,
		new State_ObjCCtor_Called(3),
		new State_Func11(pTest11, ParamState_m11 (obj3, obj600)),
		new State_ObjODtor_Called(3),
		NULL), "Test" "11" " Part1");
	g_Inside_LeafFunc = false;

	/* hook1 - no hooks */
	THGM_ADD_HOOK(11, 1);
	
	PtrBuf_Clear();
	pTest11->Func(obj3, obj600);
	g_Inside_LeafFunc = true;
	CHECK_STATES((&g_States,
		new State_ObjCCtor_Called(3),

		new State_ObjCCtor_Called(3),
		new State_Deleg1_11(pTest11, 0, ParamState_m11 (obj3, obj600)),
		new State_ObjODtor_Called(3),

		new State_ObjCCtor_Called(3),
		new State_Func11(pTest11, ParamState_m11 (obj3, obj600)),
		new State_ObjODtor_Called(3),

		new State_ObjODtor_Called(3),
		NULL), "Test" "11" " Part2");
	g_Inside_LeafFunc = false;

	THGM_REMOVE_HOOK(11, 1);

	/* hook1, hook2 - hook3, hook4 */
	THGM_ADD_HOOK(11, 1);
	THGM_ADD_HOOK(11, 2);
	THGM_ADD_HOOK(11, 3);
	THGM_ADD_HOOK(11, 4);
	PtrBuf_Clear();
	pTest11->Func(obj3, obj600);
	g_Inside_LeafFunc = true;
	CHECK_STATES((&g_States,
		new State_ObjCCtor_Called(3),

		new State_ObjCCtor_Called(3),
		new	State_Deleg1_11(pTest11, 0, ParamState_m11 (obj3, obj600)),
		new State_ObjODtor_Called(3),

		new State_ObjCCtor_Called(3),
		new State_Deleg2_11(pTest11, 1, ParamState_m11 (obj3, obj600)),
		new State_ObjODtor_Called(3),

		new State_ObjCCtor_Called(3),
		new State_Deleg3_11(pTest11, 2, ParamState_m11 (obj3, obj600)),
		new State_ObjODtor_Called(3),

		new State_ObjCCtor_Called(3),
		new State_Deleg4_11(pTest11, 3, ParamState_m11 (obj3, obj600)),
		new State_ObjODtor_Called(3),

		new State_ObjODtor_Called(3),
	NULL), "Test" "11" " Part3");
	g_Inside_LeafFunc = false;

	/* hook1 - hook3, hook4 WITH RECALLS */
	THGM_REMOVE_HOOK(11, 2);
	PtrBuf_Clear();
	TestClass11::ms_DoRecall = true;
	pTest11->Func(obj3, obj600);
	g_Inside_LeafFunc = true;
	CHECK_STATES((&g_States,
		new State_ObjCCtor_Called(3),

		new State_ObjCCtor_Called(3),
		new	State_Deleg1_11(pTest11, 0 /* first deleg ptr */, ParamState_m11 (obj3, obj600)),

		// recall !
			// second hookfunc -> new copy
			new State_ObjCCtor_Called(3),

			// in second hookfunc now
			//   -> calls orig func

			new State_ObjCCtor_Called(3),
			new State_Func11(pTest11, ParamState_m11 (obj3, obj600)(1)),
			new State_ObjODtor_Called(3),

			// calls first posthook
			new State_ObjCCtor_Called(3),
			new State_Deleg3_11(pTest11, 1 /* second deleg ptr */, ParamState_m11 (obj3, obj600)(1)),

			// recall!
				// third hookfunc -> new copy
				new State_ObjCCtor_Called(3),

				// calls second posthook

				new State_ObjCCtor_Called(3),
				new State_Deleg4_11(pTest11, 2 /* third deleg ptr */, ParamState_m11 (obj3, obj600)(2)),

				// recall!
					// fourth hookfunc -> new copy
					new State_ObjCCtor_Called(3),

					// has nothing to do though!

					// fourth hookfunc done -> ret
					new State_ObjODtor_Called(3),

				// third hookfunc done -> ret
				new State_ObjODtor_Called(3),
				// ret from hookhandler which did the recall
				new State_ObjODtor_Called(3),

			// second hookfunc done -> ret
			new State_ObjODtor_Called(3),
			// ret from hookhandler which did the recall
			new State_ObjODtor_Called(3),

		// deleg1's instance
		new State_ObjODtor_Called(3),
		// first hookfunc done -> ret
		new State_ObjODtor_Called(3),
		NULL), "Test" "11" " Part4");
	g_Inside_LeafFunc = false;

	THGM_REMOVE_HOOK(11, 1);
	THGM_REMOVE_HOOK(11, 3);
	THGM_REMOVE_HOOK(11, 4);

	THGM_DO_TEST(101, ());

	THGM_DO_TEST(102, ());

	THGM_DO_TEST(103, ());

	THGM_DO_TEST(104, ());

	THGM_DO_TEST(105, ());

	// Shutdown now!
	// If we don't SH will auto-shutdown _after_ genc's destructor is called
	//  -> crash

	Test_CompleteShutdown(g_SHPtr);
	return true;
}
