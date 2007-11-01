// Strip &
template <class T> struct StripRef
{
	typedef T type;
};

template <class T> struct StripRef<T&>
{
	typedef T type;
};

// Address of constructor/destructor
// (using wrappers)
template <class T>
class Ctor_Thunk
{
public:
	void NormalConstructor()
	{
		new(this) T;
	}

	void CopyConstructor(const T &other)
	{
		new(this) T(other);
	}

	void Destructor()
	{
		reinterpret_cast<T*>(this)->~T();
	}
};


template <class T>
void *FindFuncAddr(T mfp)
{
	if (sizeof(mfp) != sizeof(void*))
		return NULL;
	else
		return fastdelegate::detail::horrible_cast<void*>(mfp);
}

// Reference carrier
template <class T> struct MyRefCarrier
{
	typedef T type;
};

template <class T> struct MyRefCarrier<T&>
{
	class type
	{
		T *m_StoredRef;
	public:
		type() : m_StoredRef(NULL)
		{
		}
		type(T& ref) : m_StoredRef(&ref)
		{
		}

		T& operator= (T& ref)
		{
			m_StoredRef = &ref;
			return ref;
		}

		operator T& () const
		{
			return *m_StoredRef;
		}
		
		bool operator== (const typename MyRefCarrier<T&>::type &other)
		{
			return m_StoredRef == other.m_StoredRef;
		}
		
		friend std::ostream& operator <<(std::ostream &os,const typename MyRefCarrier<T&>::type &obj)
		{
			os << *obj.m_StoredRef;
			return os;
		}
	};
};

// Return value maker
template <class T>
struct MakeRet
{
	static T Do(int a)
	{
		return a;
	}
};

template <int SIZE>
struct MakeRet< POD<SIZE> >
{
	static POD<SIZE> Do(int a)
	{
		POD x;
		memset(reinterpret_cast<void*>(x.x), a, SIZE);
		return x;
	}
};

// Stores parameter status
@[$1,0,$a:

template<int dummy@[$2,1,$1:, class p$2@]>
struct ParamState$1
{
	@[$2,1,$1:typename MyRefCarrier<p$2>::type m_$2; @]
	
	bool operator==(const ParamState$1<dummy@[$2,1,$1:, p$2@]> &other)
	{
		return true
			@[$2,1,$1: && m_$1 == other.m_$1@]
			;
	}
	ParamState$1(@[$2,1,$1|, :p$2 a$2@]) @[$1!=0::@] @[$2,1,$1|, :m_$2(a$2)@]
	{
	}
	
	ParamState$1<dummy@[$2,1,$1:, p$2@]> & operator() (int incrsteps)
	{
		@[$1!=0:int i;@]
		@[$2,1,$1:
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p$2 >::type>::Incr(m_$2);
		@]
		
		return *this;
	}
};

@[$1!=0:template<@[$2,1,$1|, :class p$2@]>@]
std::ostream& operator <<(std::ostream &os,const ParamState$1<0@[$2,1,$1:, p$2@]> &obj)
{
	@[$1!=0:os@] @[$2,1,$1:<< obj.m_$2@];
	return os;
}
	
@]

#define CAT2(a, b) a##b
#define CAT3(a, b, c) a##b##c
#define CAT4(a, b, c, d) a##b##c##d

// hook1: pre ignore
// hook2: pre supercede
// hook3: post ignore
// hook4: post supercede


@[$1,0,$a:

#define THGM_MAKE_TEST$1_void(id@[$2,1,$1:, param$2@]) \
	struct TestClass##id; \
	typedef ParamState$1<0@[$2,1,$1:, param$2@] > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_3(State_Deleg1_##id, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	MAKE_STATE_3(State_Deleg2_##id, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	MAKE_STATE_3(State_Deleg3_##id, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	MAKE_STATE_3(State_Deleg4_##id, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	\
	struct TestClass##id \
	{ \
		static bool ms_DoRecall; \
		\
		virtual void Func(@[$2,1,$1|, :param$2 p$2@]) \
		{ \
			g_Inside_LeafFunc = true; \
			ADD_STATE(State_Func##id(this, ParamState_m##id(@[$2,1,$1|, :p$2@]))); \
			g_Inside_LeafFunc = false; \
		} \
		\
		struct Delegate1 : public MyDelegate \
		{ \
			virtual void Call(@[$2,1,$1|, :param$2 p$2@]) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg1_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(@[$2,1,$1|, :p$2@]))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					@[$2,1,$1:Increment<StripRef< param$2 >::type>::Incr(p$2);@] \
					RETURN_META_NEWPARAMS(MRES_IGNORED, &TestClass##id::Func, (@[$2,1,$1|, :p$2@])); \
				} \
				else \
					RETURN_META(MRES_IGNORED); \
			} \
		}; \
		struct Delegate2 : public MyDelegate \
		{ \
			virtual void Call(@[$2,1,$1|, :param$2 p$2@]) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg2_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(@[$2,1,$1|, :p$2@]))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					@[$2,1,$1:Increment<StripRef< param$2 >::type>::Incr(p$2);@] \
					RETURN_META_NEWPARAMS(MRES_SUPERCEDE, &TestClass##id::Func, (@[$2,1,$1|, :p$2@])); \
				} \
				else \
					RETURN_META(MRES_SUPERCEDE); \
			} \
		}; \
		struct Delegate3 : public MyDelegate \
		{ \
			virtual void Call(@[$2,1,$1|, :param$2 p$2@]) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg3_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(@[$2,1,$1|, :p$2@]))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					@[$2,1,$1:Increment<StripRef< param$2 >::type>::Incr(p$2);@] \
					RETURN_META_NEWPARAMS(MRES_IGNORED, &TestClass##id::Func, (@[$2,1,$1|, :p$2@])); \
				} \
				else \
					RETURN_META(MRES_IGNORED); \
			} \
		}; \
		struct Delegate4 : public MyDelegate \
		{ \
			virtual void Call(@[$2,1,$1|, :param$2 p$2@]) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg4_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(@[$2,1,$1|, :p$2@]))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					@[$2,1,$1:Increment<StripRef< param$2 >::type>::Incr(p$2);@] \
					RETURN_META_NEWPARAMS(MRES_SUPERCEDE, &TestClass##id::Func, (@[$2,1,$1|, :p$2@])); \
				} \
				else \
					RETURN_META(MRES_SUPERCEDE); \
			}; \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	\
	SourceHook::PassInfo paraminfos_##id[$1+1]; \
	SourceHook::PassInfo::V2Info paraminfos2_##id[$1+1]; \
	SourceHook::ProtoInfo protoinfo_##id = { $1, {0, 0, 0}, paraminfos_##id, \
		SourceHook::ProtoInfo::CallConv_ThisCall, __SH_EPI, paraminfos2_##id }; \
	\
	SourceHook::Impl::GenContext *g_Genc##id = NULL; \
	CAutoPtrDestruction<SourceHook::Impl::GenContext> g_Genc_ad##id(NULL); 


#define THGM_MAKE_TEST$1(id, ret_type@[$2,1,$1:, param$2@]) \
	struct TestClass##id; \
	typedef ParamState$1<0@[$2,1,$1:, param$2@] > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_3(State_Deleg1_##id, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	MAKE_STATE_3(State_Deleg2_##id, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	MAKE_STATE_3(State_Deleg3_##id, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	MAKE_STATE_3(State_Deleg4_##id, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	\
	struct TestClass##id \
	{ \
		static bool ms_DoRecall; \
		\
		virtual ret_type Func(@[$2,1,$1|, :param$2 p$2@]) \
		{ \
			g_Inside_LeafFunc = true; \
			ADD_STATE(State_Func##id(this, ParamState_m##id(@[$2,1,$1|, :p$2@]))); \
			g_Inside_LeafFunc = false; \
			\
			return MakeRet<ret_type>::Do(0); \
		} \
		\
		struct Delegate1 : public MyDelegate \
		{ \
			virtual ret_type Call(@[$2,1,$1|, :param$2 p$2@]) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg1_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(@[$2,1,$1|, :p$2@]))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					@[$2,1,$1:Increment<StripRef< param$2 >::type>::Incr(p$2);@] \
					RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, MakeRet<ret_type>::Do(1), &TestClass##id::Func, (@[$2,1,$1|, :p$2@])); \
				} \
				else \
					RETURN_META_VALUE(MRES_IGNORED, MakeRet<ret_type>::Do(1)); \
			} \
		}; \
		struct Delegate2 : public MyDelegate \
		{ \
			virtual ret_type Call(@[$2,1,$1|, :param$2 p$2@]) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg2_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(@[$2,1,$1|, :p$2@]))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					@[$2,1,$1:Increment<StripRef< param$2 >::type>::Incr(p$2);@] \
					RETURN_META_VALUE_NEWPARAMS(MRES_SUPERCEDE, MakeRet<ret_type>::Do(2), &TestClass##id::Func, (@[$2,1,$1|, :p$2@])); \
				} \
				else \
					RETURN_META_VALUE(MRES_SUPERCEDE, MakeRet<ret_type>::Do(2)); \
			} \
		}; \
		struct Delegate3 : public MyDelegate \
		{ \
			virtual ret_type Call(@[$2,1,$1|, :param$2 p$2@]) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg3_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(@[$2,1,$1|, :p$2@]))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					@[$2,1,$1:Increment<StripRef< param$2 >::type>::Incr(p$2);@] \
					RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, MakeRet<ret_type>::Do(3), &TestClass##id::Func, (@[$2,1,$1|, :p$2@])); \
				} \
				else \
					RETURN_META_VALUE(MRES_IGNORED, MakeRet<ret_type>::Do(3)); \
			} \
		}; \
		struct Delegate4 : public MyDelegate \
		{ \
			virtual ret_type Call(@[$2,1,$1|, :param$2 p$2@]) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg4_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(@[$2,1,$1|, :p$2@]))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					@[$2,1,$1:Increment<StripRef< param$2 >::type>::Incr(p$2);@] \
					RETURN_META_VALUE_NEWPARAMS(MRES_SUPERCEDE, MakeRet<ret_type>::Do(4), &TestClass##id::Func, (@[$2,1,$1|, :p$2@])); \
				} \
				else \
					RETURN_META_VALUE(MRES_SUPERCEDE, MakeRet<ret_type>::Do(4)); \
			}; \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	\
	SourceHook::PassInfo paraminfos_##id[$1+1]; \
	SourceHook::PassInfo::V2Info paraminfos2_##id[$1+1]; \
	SourceHook::ProtoInfo protoinfo_##id = { $1, {0, 0, 0}, paraminfos_##id, \
		SourceHook::ProtoInfo::CallConv_ThisCall, __SH_EPI, paraminfos2_##id }; \
	\
	SourceHook::Impl::GenContext *g_Genc##id = NULL; \
	CAutoPtrDestruction<SourceHook::Impl::GenContext> g_Genc_ad##id(NULL); 
	
#define THGM_SETUP_PI$1(id@[$2,1,$1:, p$2_type, p$2_passtype, p$2_flags@]) \
	void setuppi_##id() \
	{ \
		paraminfos_##id[0].size = 1; paraminfos_##id[0].type = 0; paraminfos_##id[0].flags = 0; \
		\
		@[$2,1,$1: paraminfos_##id[$2].size = sizeof(p$2_type); paraminfos_##id[$2].type = p$2_passtype; paraminfos_##id[$2].flags = p$2_flags; @] \
		\
		@[$2,1,$1: paraminfos2_##id[$2].pNormalCtor = (paraminfos_##id[$2].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p$2_type >::type>::NormalConstructor) : NULL; @] \
		@[$2,1,$1: paraminfos2_##id[$2].pCopyCtor = (paraminfos_##id[$2].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p$2_type >::type>::CopyConstructor) : NULL; @] \
		@[$2,1,$1: paraminfos2_##id[$2].pDtor = (paraminfos_##id[$2].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p$2_type >::type>::Destructor) : NULL; @] \
	}
	
@]

#define THGM_SETUP_RI(id, ret_type, ret_passtype, ret_flags) \
	void setupri_##id() \
	{ \
		protoinfo_##id.retPassInfo.size = sizeof(ret_type); \
		protoinfo_##id.retPassInfo.type = ret_passtype; \
		protoinfo_##id.retPassInfo.flags = ret_flags; \
		\
		protoinfo_##id.retPassInfo2.pNormalCtor = (protoinfo_##id.retPassInfo.flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< ret_type >::type>::NormalConstructor) : NULL; \
		protoinfo_##id.retPassInfo2.pCopyCtor = (protoinfo_##id.retPassInfo.flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< ret_type >::type>::CopyConstructor) : NULL; \
		protoinfo_##id.retPassInfo2.pDtor = (protoinfo_##id.retPassInfo.flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< ret_type >::type>::Destructor) : NULL; \
	}
	
#define THGM_ADD_HOOK(id, num) \
	CAT4(hook, num, _, id) = g_SHPtr->AddHook(g_PLID, SourceHook::ISourceHook::Hook_Normal, reinterpret_cast<void*>(pTest##id), \
		0, myhookman##id, PtrBufPtr(new TestClass##id::Delegate##num), num >= 3);

#define THGM_REMOVE_HOOK(id, num) \
	g_SHPtr->RemoveHookByID(CAT4(hook, num, _, id));

#define THGM_CALLS_void(id, call_params) \
	pTest##id->Func call_params; \
	SH_CALL(pTest##id, &TestClass##id::Func) call_params;

#define THGM_DO_TEST_void(id, call_params) \
	setuppi_##id(); \
	g_Genc##id = new SourceHook::Impl::GenContext(&protoinfo_##id, 0, 0, g_SHPtr); \
	g_Genc_ad##id.set(g_Genc##id); \
	SourceHook::HookManagerPubFunc myhookman##id = g_Genc##id->Generate(); \
	int hook1_##id, hook2_##id, hook3_##id, hook4_##id; \
	\
	TestClass##id::ms_DoRecall = false; \
	TestClass##id *pTest##id = new TestClass##id; \
	CAutoPtrDestruction<TestClass##id> apd##id(pTest##id); \
	\
			/* no hooks - no hooks */ \
	PtrBuf_Clear(); \
	THGM_CALLS_void(id, call_params); \
	CHECK_STATES((&g_States, \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part1"); \
	\
			/* hook1 - no hooks */ \
	PtrBuf_Clear(); \
	THGM_ADD_HOOK(id, 1); \
	THGM_CALLS_void(id, call_params); \
	CHECK_STATES((&g_States, \
		new State_Deleg1_##id(pTest##id, 0, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part2"); \
	THGM_REMOVE_HOOK(id, 1); \
	\
			/* no hooks - hook3 */ \
	PtrBuf_Clear(); \
	THGM_ADD_HOOK(id, 3); \
	THGM_CALLS_void(id, call_params); \
	CHECK_STATES((&g_States, \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		new State_Deleg3_##id(pTest##id, 0, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part3"); \
	THGM_REMOVE_HOOK(id, 3); \
	\
			/* hook1 - hook3 */ \
	PtrBuf_Clear(); \
	THGM_ADD_HOOK(id, 1); \
	THGM_ADD_HOOK(id, 3); \
	THGM_CALLS_void(id, call_params); \
	CHECK_STATES((&g_States, \
		new State_Deleg1_##id(pTest##id, 0, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		new State_Deleg3_##id(pTest##id, 1, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part4"); \
	THGM_REMOVE_HOOK(id, 1); \
	THGM_REMOVE_HOOK(id, 3); \
	\
			/* hook1, hook2 - hook3 */ \
	PtrBuf_Clear(); \
	THGM_ADD_HOOK(id, 1); \
	THGM_ADD_HOOK(id, 2); \
	THGM_ADD_HOOK(id, 3); \
	THGM_CALLS_void(id, call_params); \
	CHECK_STATES((&g_States, \
		new State_Deleg1_##id(pTest##id, 0, ParamState_m##id call_params), \
		new State_Deleg2_##id(pTest##id, 1, ParamState_m##id call_params), \
		new State_Deleg3_##id(pTest##id, 2, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part5"); \
	THGM_REMOVE_HOOK(id, 1); \
	THGM_REMOVE_HOOK(id, 2); \
	THGM_REMOVE_HOOK(id, 3); \
			/* hook1, hook2 - hook3, hook4 */ \
	PtrBuf_Clear(); \
	THGM_ADD_HOOK(id, 1); \
	THGM_ADD_HOOK(id, 2); \
	THGM_ADD_HOOK(id, 3); \
	THGM_ADD_HOOK(id, 4); \
	THGM_CALLS_void(id, call_params); \
	CHECK_STATES((&g_States, \
		new State_Deleg1_##id(pTest##id, 0, ParamState_m##id call_params), \
		new State_Deleg2_##id(pTest##id, 1, ParamState_m##id call_params), \
		new State_Deleg3_##id(pTest##id, 2, ParamState_m##id call_params), \
		new State_Deleg4_##id(pTest##id, 3, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part6"); \
		\
			/* hook1 - hook3, hook4, with recalls! */ \
		\
	TestClass##id::ms_DoRecall = true; \
	THGM_REMOVE_HOOK(id, 2); \
	THGM_CALLS_void(id, call_params); \
	CHECK_STATES((&g_States, \
		new State_Deleg1_##id(pTest##id, 0, ParamState_m##id call_params(0)), \
		new State_Func##id(pTest##id, ParamState_m##id call_params(1)), \
		new State_Deleg3_##id(pTest##id, 2, ParamState_m##id call_params(1)), \
		new State_Deleg4_##id(pTest##id, 3, ParamState_m##id call_params(2)), \
		/* sh_call one */ \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part7"); \
	THGM_REMOVE_HOOK(id, 1); \
	THGM_REMOVE_HOOK(id, 3); \
	THGM_REMOVE_HOOK(id, 4);


#define THGM_CALLS(id, call_params, exp_ret_norm, exp_ret_shcall, err) \
	CHECK_COND(pTest##id->Func call_params == exp_ret_norm, err " /retcallnorm"); \
	CHECK_COND(SH_CALL(pTest##id, &TestClass##id::Func) call_params == exp_ret_shcall, err " /retcallshcall");

#define THGM_DO_TEST(id, call_params) \
	setuppi_##id(); \
	setupri_##id(); \
	g_Genc##id = new SourceHook::Impl::GenContext(&protoinfo_##id, 0, 0, g_SHPtr); \
	g_Genc_ad##id.set(g_Genc##id); \
	SourceHook::HookManagerPubFunc myhookman##id = g_Genc##id->Generate(); \
	int hook1_##id, hook2_##id, hook3_##id, hook4_##id; \
	\
	TestClass##id::ms_DoRecall = false; \
	TestClass##id *pTest##id = new TestClass##id; \
	CAutoPtrDestruction<TestClass##id> apd##id(pTest##id); \
	\
			/* no hooks - no hooks */ \
	PtrBuf_Clear(); \
	THGM_CALLS(id, call_params, 0, 0, "Part1"); \
	CHECK_STATES((&g_States, \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part1"); \
	\
			/* hook1 - no hooks */ \
	PtrBuf_Clear(); \
	THGM_ADD_HOOK(id, 1); \
	THGM_CALLS(id, call_params, 0, 0, "Part2"); \
	CHECK_STATES((&g_States, \
		new State_Deleg1_##id(pTest##id, 0, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part2"); \
	THGM_REMOVE_HOOK(id, 1); \
	\
			/* no hooks - hook3 */ \
	PtrBuf_Clear(); \
	THGM_ADD_HOOK(id, 3); \
	THGM_CALLS(id, call_params, 0, 0, "Part3"); \
	CHECK_STATES((&g_States, \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		new State_Deleg3_##id(pTest##id, 0, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part3"); \
	THGM_REMOVE_HOOK(id, 3); \
	\
			/* hook1 - hook3 */ \
	PtrBuf_Clear(); \
	THGM_ADD_HOOK(id, 1); \
	THGM_ADD_HOOK(id, 3); \
	THGM_CALLS(id, call_params, 0, 0, "Part4"); \
	CHECK_STATES((&g_States, \
		new State_Deleg1_##id(pTest##id, 0, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		new State_Deleg3_##id(pTest##id, 1, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part4"); \
	THGM_REMOVE_HOOK(id, 1); \
	THGM_REMOVE_HOOK(id, 3); \
	\
			/* hook1, hook2 - hook3 */ \
	PtrBuf_Clear(); \
	THGM_ADD_HOOK(id, 1); \
	THGM_ADD_HOOK(id, 2); \
	THGM_ADD_HOOK(id, 3); \
	THGM_CALLS(id, call_params, 2, 0, "Part5"); \
	CHECK_STATES((&g_States, \
		new State_Deleg1_##id(pTest##id, 0, ParamState_m##id call_params), \
		new State_Deleg2_##id(pTest##id, 1, ParamState_m##id call_params), \
		new State_Deleg3_##id(pTest##id, 2, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part5"); \
	THGM_REMOVE_HOOK(id, 1); \
	THGM_REMOVE_HOOK(id, 2); \
	THGM_REMOVE_HOOK(id, 3); \
			/* hook1, hook2 - hook3, hook4 */ \
	PtrBuf_Clear(); \
	THGM_ADD_HOOK(id, 1); \
	THGM_ADD_HOOK(id, 2); \
	THGM_ADD_HOOK(id, 3); \
	THGM_ADD_HOOK(id, 4); \
	THGM_CALLS(id, call_params, 4, 0, "Part6"); \
	CHECK_STATES((&g_States, \
		new State_Deleg1_##id(pTest##id, 0, ParamState_m##id call_params), \
		new State_Deleg2_##id(pTest##id, 1, ParamState_m##id call_params), \
		new State_Deleg3_##id(pTest##id, 2, ParamState_m##id call_params), \
		new State_Deleg4_##id(pTest##id, 3, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part6"); \
		\
			/* hook1 - hook3, hook4, with recalls! */ \
		\
	TestClass##id::ms_DoRecall = true; \
	THGM_REMOVE_HOOK(id, 2); \
	THGM_CALLS(id, call_params, 4, 0, "Part7"); \
	CHECK_STATES((&g_States, \
		new State_Deleg1_##id(pTest##id, 0, ParamState_m##id call_params(0)), \
		new State_Func##id(pTest##id, ParamState_m##id call_params(1)), \
		new State_Deleg3_##id(pTest##id, 2, ParamState_m##id call_params(1)), \
		new State_Deleg4_##id(pTest##id, 3, ParamState_m##id call_params(2)), \
		/* sh_call one */ \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part7"); \
	THGM_REMOVE_HOOK(id, 1); \
	THGM_REMOVE_HOOK(id, 3); \
	THGM_REMOVE_HOOK(id, 4);
