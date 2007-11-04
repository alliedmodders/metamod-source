#include <string>
#include "sourcehook.h"
#include "sourcehook_test.h"
#include "testevents.h"
#include "sourcehook_hookmangen.h"
#include "sh_memory.h"
#include "sh_pagealloc.h"

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
	StateList g_States;
	SourceHook::ISourceHook *g_SHPtr;
	SourceHook::Plugin g_PLID;

	// PtrBuf(ptr) gives ptrs unique numbers
	// in the order they appear
	SourceHook::List<const void*> g_PtrHash;

	bool g_Inside_LeafFunc = false;			// inside a hook or a func


	// POD / Object types
	template <int MYSIZE>
	struct POD
	{
		char x[MYSIZE];

		bool operator==(const POD<MYSIZE> &other)
		{
			return memcmp(reinterpret_cast<void*>(x), reinterpret_cast<const void*>(other.x), MYSIZE) == 0;
		}

		bool operator==(char other)
		{
			for (int i = 0; i < MYSIZE; ++i)
			{
				if (x[i] != other)
					return false;
			}
			return true;
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
	MAKE_STATE_1(State_ObjAssignOp_Called, int /*MYSIZE*/);

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

		Object & operator = (const Object &other)
		{
			if (!g_Inside_LeafFunc)
				ADD_STATE(State_ObjAssignOp_Called(MYSIZE));

			memcpy(reinterpret_cast<void*>(x), reinterpret_cast<const void*>(other.x), MYSIZE);

			return *this;
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

	// Because of some weird bug in MSVC < 1400
#define MAKE_PODRET(size) \
	struct PodRet##size \
	{ \
		POD<size> actPod; \
		bool operator==(const PodRet##size &other) { return actPod == other.actPod; } \
		bool operator==(char other) { return actPod == other; } \
	}; \
	std::ostream& operator <<(std::ostream &os, const PodRet##size &obj) \
	{ \
		os << obj.actPod; \
		return os; \
	} \
	template <> struct MakeRet< PodRet##size > \
	{ \
		static PodRet##size Do(int a) \
		{ \
			PodRet##size x; \
			memset(reinterpret_cast<void*>(x.actPod.x), a, size); \
			return x; \
		} \
	};

#define MAKE_OBJRET(size) \
	struct ObjRet##size \
	{ \
		Object<size> actObj; \
		bool operator==(const ObjRet##size &other) { return actObj == other.actObj; } \
		bool operator==(char other) { return actObj == other; } \
	}; \
	std::ostream& operator <<(std::ostream &os, const ObjRet##size &obj) \
	{ \
		os << obj.actObj; \
		return os; \
	} \
	template <> struct MakeRet< ObjRet##size > \
	{ \
		static ObjRet##size Do(int a) \
		{ \
			ObjRet##size x; \
			memset(reinterpret_cast<void*>(x.actObj.x), a, size); \
			return x; \
		} \
	};



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

	#include "testhookmangen.h"

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

	// pod 1-13
	MAKE_PODRET(1);
	THGM_MAKE_TEST1(106, PodRet1, int);
	THGM_SETUP_PI1(106,
		int, SourceHook::PassInfo::PassType_Basic, SourceHook::PassInfo::PassFlag_ByVal
		);
	THGM_SETUP_RI(106, PodRet1, SourceHook::PassInfo::PassType_Object, SourceHook::PassInfo::PassFlag_ByVal);

	MAKE_PODRET(4);
	THGM_MAKE_TEST1(107, PodRet4, int);
	THGM_SETUP_PI1(107,
		int, SourceHook::PassInfo::PassType_Basic, SourceHook::PassInfo::PassFlag_ByVal
		);
	THGM_SETUP_RI(107, PodRet4, SourceHook::PassInfo::PassType_Object, SourceHook::PassInfo::PassFlag_ByVal);

	MAKE_PODRET(8);
	THGM_MAKE_TEST1(108, PodRet8, int);
	THGM_SETUP_PI1(108,
		int, SourceHook::PassInfo::PassType_Basic, SourceHook::PassInfo::PassFlag_ByVal
		);
	THGM_SETUP_RI(108, PodRet8, SourceHook::PassInfo::PassType_Object, SourceHook::PassInfo::PassFlag_ByVal);

	MAKE_PODRET(13);
	THGM_MAKE_TEST1(109, PodRet13, int);
	THGM_SETUP_PI1(109,
		int, SourceHook::PassInfo::PassType_Basic, SourceHook::PassInfo::PassFlag_ByVal
		);
	THGM_SETUP_RI(109, PodRet13, SourceHook::PassInfo::PassType_Object, SourceHook::PassInfo::PassFlag_ByVal);

	MAKE_OBJRET(13);
	THGM_MAKE_TEST1(110, ObjRet13, int);
	THGM_SETUP_PI1(110,
		int, SourceHook::PassInfo::PassType_Basic, SourceHook::PassInfo::PassFlag_ByVal
		);
	THGM_SETUP_RI(110, ObjRet13, SourceHook::PassInfo::PassType_Object,
		SourceHook::PassInfo::PassFlag_ByVal | SourceHook::PassInfo::PassFlag_OCtor | SourceHook::PassInfo::PassFlag_ODtor |
		SourceHook::PassInfo::PassFlag_CCtor | SourceHook::PassInfo::PassFlag_AssignOp);

	MAKE_OBJRET(111);
	ObjRet111 g_O111_0;
	ObjRet111 g_O111_1;
	ObjRet111 g_O111_2;
	ObjRet111 g_O111_3;
	ObjRet111 g_O111_4;

	template <>
	struct MakeRet< ObjRet111& >
	{
		static ObjRet111 &Do(int a)
		{
			switch (a)
			{
			case 0:
				return g_O111_0;
			case 1:
				return g_O111_1;
			case 2:
				return g_O111_2;
			case 3:
				return g_O111_3;
			default:
				return g_O111_4;
			}
		}
	};

	THGM_MAKE_TEST0(111, ObjRet111& );
	THGM_SETUP_PI0(111);
	THGM_SETUP_RI(111, ObjRet111& , SourceHook::PassInfo::PassType_Object,
		SourceHook::PassInfo::PassFlag_ByRef | SourceHook::PassInfo::PassFlag_OCtor | SourceHook::PassInfo::PassFlag_ODtor |
		SourceHook::PassInfo::PassFlag_CCtor | SourceHook::PassInfo::PassFlag_AssignOp);
}

bool TestHookManGen(std::string &error)
{
	GET_SHPTR(g_SHPtr);
	g_PLID = 1337;

	// 5 Global constructors (g_O111_*)
	CHECK_STATES((&g_States,
		new State_ObjOCtor_Called(111),
		new State_ObjOCtor_Called(111),
		new State_ObjOCtor_Called(111),
		new State_ObjOCtor_Called(111),
		new State_ObjOCtor_Called(111),
		NULL), "GlobCtors");

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

	POD<7> pod7 = MakeRet< POD<7> >::Do(78);
	THGM_DO_TEST_void(8, (pod7));

	POD<600> pod600 = MakeRet< POD<600> >::Do(34);
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

	// pod returns
	THGM_DO_TEST(106, (5));
	THGM_DO_TEST(107, (5));
	THGM_DO_TEST(108, (5));
	THGM_DO_TEST(109, (5));

	// Test110: Special: constructors/destructors on return
	PtrBuf_Clear();
	ObjRet13 obj13;

	CHECK_STATES((&g_States,
		new State_ObjOCtor_Called(13),
		NULL), "Test110 Part0");

	setuppi_110();
	setupri_110();

	g_Genc110 = new SourceHook::Impl::GenContext(&protoinfo_110, 0, 0, g_SHPtr);
	g_Genc_ad110.set(g_Genc110);
	SourceHook::HookManagerPubFunc myhookman110 = g_Genc110->Generate();
	int hook1_110, hook2_110, hook3_110, hook4_110;

	TestClass110 *pTest110 = new TestClass110;
	CAutoPtrDestruction<TestClass110> apd110(pTest110);

	/* no hooks - no hooks */
	PtrBuf_Clear();
	obj13 = pTest110->Func(5);

	g_Inside_LeafFunc = true;
	CHECK_STATES((&g_States,
		new State_Func110(pTest110, ParamState_m110 (5)),
		new State_ObjOCtor_Called(13),			// MakeRet: Construction of x
		new State_ObjCCtor_Called(13),			// Return from MakeRet	-> construct temporary object in our stack
		new State_ObjODtor_Called(13),			// MakeRet: Destruction of x

		new State_ObjAssignOp_Called(13),		// assign: obj13 = temporary object in our stack
		new State_ObjODtor_Called(13),			// Func110: destruction of temporary object
		NULL), "Test" "110" " Part1");
	g_Inside_LeafFunc = false;

	/* hook1 - no hooks */
	THGM_ADD_HOOK(110, 1);

	PtrBuf_Clear();
	obj13 = pTest110->Func(5);
	g_Inside_LeafFunc = true;
	CHECK_STATES((&g_States,
		// HookFunc: construct orig_ret/override_ret/plugin_ret objects
		new State_ObjOCtor_Called(13),
		new State_ObjOCtor_Called(13),
		new State_ObjOCtor_Called(13),

		// Calling delegate
			new State_Deleg1_110(pTest110, 0, ParamState_m110 (5)),

			new State_ObjOCtor_Called(13),	// MakeRet: Construction of x
			new State_ObjCCtor_Called(13),	// Return from MakeRet	-> construct temporary object in HookFunc's stack
			new State_ObjODtor_Called(13),	// MakeRet: Destruction of x

		// back in hookfunc
		new State_ObjAssignOp_Called(13),	// assign: plugin_ret = temporary object in HookFunc's stack
		new State_ObjODtor_Called(13),		// destruction of temporary object in HookFunc's stack

		// Calling orig function Func110
			new State_Func110(pTest110, ParamState_m110 (5)),
			new State_ObjOCtor_Called(13),	// MakeRet: Construction of x
			new State_ObjCCtor_Called(13),	// Return from MakeRet	-> construct temporary object in HookFunc's stack
			new State_ObjODtor_Called(13),	// MakeRet: Destruction of x

		// back in hookfunc
		new State_ObjAssignOp_Called(13),	// assign: orig_ret = temporary object in HookFunc's stack
		new State_ObjODtor_Called(13),		// destruction of temporary object in HookFunc's stack

		// hookfunc is returning:
		new State_ObjCCtor_Called(13),		// copy to temp object in our stack

		// hookfunc cleans up its stack -> destroys plugin_ret/override_ret/orig_ret
		new State_ObjODtor_Called(13),
		new State_ObjODtor_Called(13),
		new State_ObjODtor_Called(13),
		
		// we are in our function: assign 
		new State_ObjAssignOp_Called(13),	// assign: obj13 = temporary object in our stack
		new State_ObjODtor_Called(13),		// Func110: destruction of temporary object

		NULL), "Test" "11" " Part2");
	
	CHECK_COND(obj13 == 0, "Test" "11" " Part 2.1");
	g_Inside_LeafFunc = false;

	THGM_REMOVE_HOOK(11, 1);

	/* hook1, hook2 - hook3, hook4 */
	THGM_ADD_HOOK(110, 1);
	THGM_ADD_HOOK(110, 2);
	THGM_ADD_HOOK(110, 3);
	THGM_ADD_HOOK(110, 4);

	PtrBuf_Clear();
	obj13 = pTest110->Func(5);
	g_Inside_LeafFunc = true;
	CHECK_STATES((&g_States,
		// HookFunc: construct orig_ret/override_ret/plugin_ret objects
		new State_ObjOCtor_Called(13),
		new State_ObjOCtor_Called(13),
		new State_ObjOCtor_Called(13),

		// Calling delegate1
			new State_Deleg1_110(pTest110, 0, ParamState_m110 (5)),

			new State_ObjOCtor_Called(13),	// MakeRet: Construction of x
			new State_ObjCCtor_Called(13),	// Return from MakeRet	-> construct temporary object in HookFunc's stack
			new State_ObjODtor_Called(13),	// MakeRet: Destruction of x

		// back in hookfunc
		new State_ObjAssignOp_Called(13),	// assign: plugin_ret = temporary object in HookFunc's stack
		new State_ObjODtor_Called(13),		// destruction of temporary object in HookFunc's stack


		// Calling delegate2
			new State_Deleg2_110(pTest110, 1, ParamState_m110 (5)),

			new State_ObjOCtor_Called(13),	// MakeRet: Construction of x
			new State_ObjCCtor_Called(13),	// Return from MakeRet	-> construct temporary object in HookFunc's stack
			new State_ObjODtor_Called(13),	// MakeRet: Destruction of x

		// back in hookfunc
		new State_ObjAssignOp_Called(13),	// assign: plugin_ret = temporary object in HookFunc's stack
		new State_ObjODtor_Called(13),		// destruction of temporary object in HookFunc's stack

		// hookfunc finds out that the hook wanted to SUPERCEDE --> copy to override_Ret
		new State_ObjAssignOp_Called(13),

		// SUPERCEDE -> orig function is not called
		// instead: orig_ret = override_ret
		new State_ObjAssignOp_Called(13),

		// Calling delegate3
			new State_Deleg3_110(pTest110, 2, ParamState_m110 (5)),

			new State_ObjOCtor_Called(13),	// MakeRet: Construction of x
			new State_ObjCCtor_Called(13),	// Return from MakeRet	-> construct temporary object in HookFunc's stack
			new State_ObjODtor_Called(13),	// MakeRet: Destruction of x

		// back in hookfunc
		new State_ObjAssignOp_Called(13),	// assign: plugin_ret = temporary object in HookFunc's stack
		new State_ObjODtor_Called(13),		// destruction of temporary object in HookFunc's stack

		// Calling delegate4
			new State_Deleg4_110(pTest110, 3, ParamState_m110 (5)),

			new State_ObjOCtor_Called(13),	// MakeRet: Construction of x
			new State_ObjCCtor_Called(13),	// Return from MakeRet	-> construct temporary object in HookFunc's stack
			new State_ObjODtor_Called(13),	// MakeRet: Destruction of x

		// back in hookfunc
		new State_ObjAssignOp_Called(13),	// assign: plugin_ret = temporary object in HookFunc's stack
		new State_ObjODtor_Called(13),		// destruction of temporary object in HookFunc's stack

		// hookfunc finds out that the hook wanted to SUPERCEDE --> copy to override_Ret (yes really, we overwrite the old value!)
		new State_ObjAssignOp_Called(13),

		// hookfunc is returning:
		new State_ObjCCtor_Called(13),		// copy to temp object in our stack

		// hookfunc cleans up its stack -> destroys plugin_ret/override_ret/orig_ret
		new State_ObjODtor_Called(13),
		new State_ObjODtor_Called(13),
		new State_ObjODtor_Called(13),

		// we are in our function: assign 
		new State_ObjAssignOp_Called(13),	// assign: obj13 = temporary object in our stack
		new State_ObjODtor_Called(13),		// Func110: destruction of temporary object

		NULL), "Test" "11" " Part3");

	CHECK_COND(obj13 == 4, "Test" "11" " Part 3.1");
	g_Inside_LeafFunc = false;

	THGM_REMOVE_HOOK(110, 1);
	THGM_REMOVE_HOOK(110, 2);
	THGM_REMOVE_HOOK(110, 3);
	THGM_REMOVE_HOOK(110, 4);


	// RefRet
	THGM_DO_TEST(111, ());

	// Shutdown now!
	// If we don't SH will auto-shutdown _after_ genc's destructor is called
	//  -> crash

	Test_CompleteShutdown(g_SHPtr);

	CHECK_COND(sizeof(PodRet1) == 1, "WTF!");
	CHECK_COND(sizeof(PodRet4) == 4, "WTF!");
	CHECK_COND(sizeof(PodRet8) == 8, "WTF!");
	CHECK_COND(sizeof(PodRet13) == 13, "WTF!");

	return true;
}

bool TestCPageAlloc(std::string &error)
{
	using namespace SourceHook;

	CPageAlloc alloc;
	int i;
	size_t ps = alloc.GetPageSize();

	char *test1[8];

	for (i = 0; i < 8; ++i)
		test1[i] = (char*) alloc.Alloc(ps / 4);

	CHECK_COND(test1[1] == test1[0] + ps/4, "Part 1.1");
	CHECK_COND(test1[2] == test1[1] + ps/4, "Part 1.2");
	CHECK_COND(test1[3] == test1[2] + ps/4, "Part 1.3");

	CHECK_COND(test1[5] == test1[4] + ps/4, "Part 1.4");
	CHECK_COND(test1[6] == test1[5] + ps/4, "Part 1.5");
	CHECK_COND(test1[7] == test1[6] + ps/4, "Part 1.6");

	void *test2 = alloc.Alloc(ps * 3);

	alloc.SetRW(test2);
	memset(test2, 0, ps * 3);			// should not crash :)
	alloc.SetRE(test2);

	alloc.Free(test2);

	// Dealloc a ps/4 block and place two ps/8 blocks into it
	alloc.Free(test1[2]);

	char *test3[2];
	test3[0] = (char*) alloc.Alloc(ps / 8);
	test3[1] = (char*) alloc.Alloc(ps / 8);

	CHECK_COND(test3[0] == test1[2], "Part 2.1");
	CHECK_COND(test3[1] == test1[2] + ps/8, "Part 2.2");

	// Isolated
	char *test4[8];
	for (i = 0; i < 8; ++i)
		test4[i] = (char*) alloc.AllocIsolated(ps / 4);

	// -> The difference is at least one page
	CHECK_COND(static_cast<size_t>(abs(test4[1] - test4[0])) >= ps, "Part 3.1");
	CHECK_COND(static_cast<size_t>(abs(test4[2] - test4[1])) >= ps, "Part 3.2");
	CHECK_COND(static_cast<size_t>(abs(test4[3] - test4[2])) >= ps, "Part 3.3");

	CHECK_COND(static_cast<size_t>(abs(test4[5] - test4[4])) >= ps, "Part 3.4");
	CHECK_COND(static_cast<size_t>(abs(test4[6] - test4[5])) >= ps, "Part 3.5");
	CHECK_COND(static_cast<size_t>(abs(test4[7] - test4[6])) >= ps, "Part 3.6");

	// Thus i can set everything except for test4[2] to RE and still write to test4[2]

	alloc.SetRW(test4[2]);

	alloc.SetRE(test4[0]);
	alloc.SetRE(test4[1]);
	alloc.SetRE(test4[3]);
	alloc.SetRE(test4[4]);
	alloc.SetRE(test4[5]);
	alloc.SetRE(test4[6]);
	alloc.SetRE(test4[7]);

	memset((void*)test4[2], 0, ps / 4);

	return true;
}
