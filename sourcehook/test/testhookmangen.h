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

// Stores parameter status


template<int dummy>
struct ParamState0
{
	
	
	bool operator==(const ParamState0<dummy> &other)
	{
		return true
			
			;
	}
	ParamState0()  
	{
	}
	
	ParamState0<dummy> & operator() (int incrsteps)
	{
		
		
		
		return *this;
	}
};


std::ostream& operator <<(std::ostream &os,const ParamState0<0> &obj)
{
	 ;
	return os;
}
	


template<int dummy, class p1>
struct ParamState1
{
	typename MyRefCarrier<p1>::type m_1; 
	
	bool operator==(const ParamState1<dummy, p1> &other)
	{
		return true
			 && m_1 == other.m_1
			;
	}
	ParamState1(p1 a1) : m_1(a1)
	{
	}
	
	ParamState1<dummy, p1> & operator() (int incrsteps)
	{
		int i;
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p1 >::type>::Incr(m_1);
		
		
		return *this;
	}
};

template<class p1>
std::ostream& operator <<(std::ostream &os,const ParamState1<0, p1> &obj)
{
	os << obj.m_1;
	return os;
}
	


template<int dummy, class p1, class p2>
struct ParamState2
{
	typename MyRefCarrier<p1>::type m_1; typename MyRefCarrier<p2>::type m_2; 
	
	bool operator==(const ParamState2<dummy, p1, p2> &other)
	{
		return true
			 && m_2 == other.m_2 && m_2 == other.m_2
			;
	}
	ParamState2(p1 a1, p2 a2) : m_1(a1), m_2(a2)
	{
	}
	
	ParamState2<dummy, p1, p2> & operator() (int incrsteps)
	{
		int i;
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p1 >::type>::Incr(m_1);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p2 >::type>::Incr(m_2);
		
		
		return *this;
	}
};

template<class p1, class p2>
std::ostream& operator <<(std::ostream &os,const ParamState2<0, p1, p2> &obj)
{
	os << obj.m_1<< obj.m_2;
	return os;
}
	


template<int dummy, class p1, class p2, class p3>
struct ParamState3
{
	typename MyRefCarrier<p1>::type m_1; typename MyRefCarrier<p2>::type m_2; typename MyRefCarrier<p3>::type m_3; 
	
	bool operator==(const ParamState3<dummy, p1, p2, p3> &other)
	{
		return true
			 && m_3 == other.m_3 && m_3 == other.m_3 && m_3 == other.m_3
			;
	}
	ParamState3(p1 a1, p2 a2, p3 a3) : m_1(a1), m_2(a2), m_3(a3)
	{
	}
	
	ParamState3<dummy, p1, p2, p3> & operator() (int incrsteps)
	{
		int i;
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p1 >::type>::Incr(m_1);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p2 >::type>::Incr(m_2);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p3 >::type>::Incr(m_3);
		
		
		return *this;
	}
};

template<class p1, class p2, class p3>
std::ostream& operator <<(std::ostream &os,const ParamState3<0, p1, p2, p3> &obj)
{
	os << obj.m_1<< obj.m_2<< obj.m_3;
	return os;
}
	


template<int dummy, class p1, class p2, class p3, class p4>
struct ParamState4
{
	typename MyRefCarrier<p1>::type m_1; typename MyRefCarrier<p2>::type m_2; typename MyRefCarrier<p3>::type m_3; typename MyRefCarrier<p4>::type m_4; 
	
	bool operator==(const ParamState4<dummy, p1, p2, p3, p4> &other)
	{
		return true
			 && m_4 == other.m_4 && m_4 == other.m_4 && m_4 == other.m_4 && m_4 == other.m_4
			;
	}
	ParamState4(p1 a1, p2 a2, p3 a3, p4 a4) : m_1(a1), m_2(a2), m_3(a3), m_4(a4)
	{
	}
	
	ParamState4<dummy, p1, p2, p3, p4> & operator() (int incrsteps)
	{
		int i;
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p1 >::type>::Incr(m_1);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p2 >::type>::Incr(m_2);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p3 >::type>::Incr(m_3);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p4 >::type>::Incr(m_4);
		
		
		return *this;
	}
};

template<class p1, class p2, class p3, class p4>
std::ostream& operator <<(std::ostream &os,const ParamState4<0, p1, p2, p3, p4> &obj)
{
	os << obj.m_1<< obj.m_2<< obj.m_3<< obj.m_4;
	return os;
}
	


template<int dummy, class p1, class p2, class p3, class p4, class p5>
struct ParamState5
{
	typename MyRefCarrier<p1>::type m_1; typename MyRefCarrier<p2>::type m_2; typename MyRefCarrier<p3>::type m_3; typename MyRefCarrier<p4>::type m_4; typename MyRefCarrier<p5>::type m_5; 
	
	bool operator==(const ParamState5<dummy, p1, p2, p3, p4, p5> &other)
	{
		return true
			 && m_5 == other.m_5 && m_5 == other.m_5 && m_5 == other.m_5 && m_5 == other.m_5 && m_5 == other.m_5
			;
	}
	ParamState5(p1 a1, p2 a2, p3 a3, p4 a4, p5 a5) : m_1(a1), m_2(a2), m_3(a3), m_4(a4), m_5(a5)
	{
	}
	
	ParamState5<dummy, p1, p2, p3, p4, p5> & operator() (int incrsteps)
	{
		int i;
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p1 >::type>::Incr(m_1);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p2 >::type>::Incr(m_2);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p3 >::type>::Incr(m_3);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p4 >::type>::Incr(m_4);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p5 >::type>::Incr(m_5);
		
		
		return *this;
	}
};

template<class p1, class p2, class p3, class p4, class p5>
std::ostream& operator <<(std::ostream &os,const ParamState5<0, p1, p2, p3, p4, p5> &obj)
{
	os << obj.m_1<< obj.m_2<< obj.m_3<< obj.m_4<< obj.m_5;
	return os;
}
	


template<int dummy, class p1, class p2, class p3, class p4, class p5, class p6>
struct ParamState6
{
	typename MyRefCarrier<p1>::type m_1; typename MyRefCarrier<p2>::type m_2; typename MyRefCarrier<p3>::type m_3; typename MyRefCarrier<p4>::type m_4; typename MyRefCarrier<p5>::type m_5; typename MyRefCarrier<p6>::type m_6; 
	
	bool operator==(const ParamState6<dummy, p1, p2, p3, p4, p5, p6> &other)
	{
		return true
			 && m_6 == other.m_6 && m_6 == other.m_6 && m_6 == other.m_6 && m_6 == other.m_6 && m_6 == other.m_6 && m_6 == other.m_6
			;
	}
	ParamState6(p1 a1, p2 a2, p3 a3, p4 a4, p5 a5, p6 a6) : m_1(a1), m_2(a2), m_3(a3), m_4(a4), m_5(a5), m_6(a6)
	{
	}
	
	ParamState6<dummy, p1, p2, p3, p4, p5, p6> & operator() (int incrsteps)
	{
		int i;
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p1 >::type>::Incr(m_1);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p2 >::type>::Incr(m_2);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p3 >::type>::Incr(m_3);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p4 >::type>::Incr(m_4);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p5 >::type>::Incr(m_5);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<StripRef< p6 >::type>::Incr(m_6);
		
		
		return *this;
	}
};

template<class p1, class p2, class p3, class p4, class p5, class p6>
std::ostream& operator <<(std::ostream &os,const ParamState6<0, p1, p2, p3, p4, p5, p6> &obj)
{
	os << obj.m_1<< obj.m_2<< obj.m_3<< obj.m_4<< obj.m_5<< obj.m_6;
	return os;
}
	


#define CAT2(a, b) a##b
#define CAT3(a, b, c) a##b##c
#define CAT4(a, b, c, d) a##b##c##d

// hook1: pre ignore
// hook2: pre supercede
// hook3: post ignore
// hook4: post supercede




#define THGM_MAKE_TEST0_void(id) \
	struct TestClass##id; \
	typedef ParamState0<0 > ParamState_m##id; \
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
		virtual void Func() \
		{ \
			g_Inside_LeafFunc = true; \
			ADD_STATE(State_Func##id(this, ParamState_m##id())); \
			g_Inside_LeafFunc = false; \
		} \
		\
		struct Delegate1 : public MyDelegate \
		{ \
			virtual void Call() \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg1_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id())); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					 \
					RETURN_META_NEWPARAMS(MRES_IGNORED, &TestClass##id::Func, ()); \
				} \
				else \
					RETURN_META(MRES_IGNORED); \
			} \
		}; \
		struct Delegate2 : public MyDelegate \
		{ \
			virtual void Call() \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg2_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id())); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					 \
					RETURN_META_NEWPARAMS(MRES_SUPERCEDE, &TestClass##id::Func, ()); \
				} \
				else \
					RETURN_META(MRES_SUPERCEDE); \
			} \
		}; \
		struct Delegate3 : public MyDelegate \
		{ \
			virtual void Call() \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg3_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id())); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					 \
					RETURN_META_NEWPARAMS(MRES_IGNORED, &TestClass##id::Func, ()); \
				} \
				else \
					RETURN_META(MRES_IGNORED); \
			} \
		}; \
		struct Delegate4 : public MyDelegate \
		{ \
			virtual void Call() \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg4_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id())); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					 \
					RETURN_META_NEWPARAMS(MRES_SUPERCEDE, &TestClass##id::Func, ()); \
				} \
				else \
					RETURN_META(MRES_SUPERCEDE); \
			}; \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	\
	SourceHook::PassInfo paraminfos_##id[0+1]; \
	SourceHook::PassInfo::V2Info paraminfos2_##id[0+1]; \
	SourceHook::ProtoInfo protoinfo_##id = { 0, {0, 0, 0}, paraminfos_##id, \
		SourceHook::ProtoInfo::CallConv_ThisCall, __SH_EPI, paraminfos2_##id }; \
	\
	SourceHook::Impl::GenContext *g_Genc##id = NULL; \
	CAutoPtrDestruction<SourceHook::Impl::GenContext> g_Genc_ad##id(NULL); 


#define THGM_MAKE_TEST0(id, ret_type) \
	struct TestClass##id; \
	typedef ParamState0<0 > ParamState_m##id; \
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
		virtual ret_type Func() \
		{ \
			g_Inside_LeafFunc = true; \
			ADD_STATE(State_Func##id(this, ParamState_m##id())); \
			g_Inside_LeafFunc = false; \
			\
			return 0; \
		} \
		\
		struct Delegate1 : public MyDelegate \
		{ \
			virtual ret_type Call() \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg1_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id())); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					 \
					RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, 1, &TestClass##id::Func, ()); \
				} \
				else \
					RETURN_META_VALUE(MRES_IGNORED, 1); \
			} \
		}; \
		struct Delegate2 : public MyDelegate \
		{ \
			virtual ret_type Call() \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg2_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id())); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					 \
					RETURN_META_VALUE_NEWPARAMS(MRES_SUPERCEDE, 2, &TestClass##id::Func, ()); \
				} \
				else \
					RETURN_META_VALUE(MRES_SUPERCEDE, 2); \
			} \
		}; \
		struct Delegate3 : public MyDelegate \
		{ \
			virtual ret_type Call() \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg3_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id())); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					 \
					RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, 3, &TestClass##id::Func, ()); \
				} \
				else \
					RETURN_META_VALUE(MRES_IGNORED, 3); \
			} \
		}; \
		struct Delegate4 : public MyDelegate \
		{ \
			virtual ret_type Call() \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg4_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id())); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					 \
					RETURN_META_VALUE_NEWPARAMS(MRES_SUPERCEDE, 4, &TestClass##id::Func, ()); \
				} \
				else \
					RETURN_META_VALUE(MRES_SUPERCEDE, 4); \
			}; \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	\
	SourceHook::PassInfo paraminfos_##id[0+1]; \
	SourceHook::PassInfo::V2Info paraminfos2_##id[0+1]; \
	SourceHook::ProtoInfo protoinfo_##id = { 0, {0, 0, 0}, paraminfos_##id, \
		SourceHook::ProtoInfo::CallConv_ThisCall, __SH_EPI, paraminfos2_##id }; \
	\
	SourceHook::Impl::GenContext *g_Genc##id = NULL; \
	CAutoPtrDestruction<SourceHook::Impl::GenContext> g_Genc_ad##id(NULL); 
	
#define THGM_SETUP_PI0(id) \
	void setuppi_##id() \
	{ \
		paraminfos_##id[0].size = 1; paraminfos_##id[0].type = 0; paraminfos_##id[0].flags = 0; \
		\
		 \
		\
		 \
		 \
		 \
	}
	


#define THGM_MAKE_TEST1_void(id, param1) \
	struct TestClass##id; \
	typedef ParamState1<0, param1 > ParamState_m##id; \
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
		virtual void Func(param1 p1) \
		{ \
			g_Inside_LeafFunc = true; \
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1))); \
			g_Inside_LeafFunc = false; \
		} \
		\
		struct Delegate1 : public MyDelegate \
		{ \
			virtual void Call(param1 p1) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg1_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1); \
					RETURN_META_NEWPARAMS(MRES_IGNORED, &TestClass##id::Func, (p1)); \
				} \
				else \
					RETURN_META(MRES_IGNORED); \
			} \
		}; \
		struct Delegate2 : public MyDelegate \
		{ \
			virtual void Call(param1 p1) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg2_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1); \
					RETURN_META_NEWPARAMS(MRES_SUPERCEDE, &TestClass##id::Func, (p1)); \
				} \
				else \
					RETURN_META(MRES_SUPERCEDE); \
			} \
		}; \
		struct Delegate3 : public MyDelegate \
		{ \
			virtual void Call(param1 p1) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg3_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1); \
					RETURN_META_NEWPARAMS(MRES_IGNORED, &TestClass##id::Func, (p1)); \
				} \
				else \
					RETURN_META(MRES_IGNORED); \
			} \
		}; \
		struct Delegate4 : public MyDelegate \
		{ \
			virtual void Call(param1 p1) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg4_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1); \
					RETURN_META_NEWPARAMS(MRES_SUPERCEDE, &TestClass##id::Func, (p1)); \
				} \
				else \
					RETURN_META(MRES_SUPERCEDE); \
			}; \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	\
	SourceHook::PassInfo paraminfos_##id[1+1]; \
	SourceHook::PassInfo::V2Info paraminfos2_##id[1+1]; \
	SourceHook::ProtoInfo protoinfo_##id = { 1, {0, 0, 0}, paraminfos_##id, \
		SourceHook::ProtoInfo::CallConv_ThisCall, __SH_EPI, paraminfos2_##id }; \
	\
	SourceHook::Impl::GenContext *g_Genc##id = NULL; \
	CAutoPtrDestruction<SourceHook::Impl::GenContext> g_Genc_ad##id(NULL); 


#define THGM_MAKE_TEST1(id, ret_type, param1) \
	struct TestClass##id; \
	typedef ParamState1<0, param1 > ParamState_m##id; \
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
		virtual ret_type Func(param1 p1) \
		{ \
			g_Inside_LeafFunc = true; \
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1))); \
			g_Inside_LeafFunc = false; \
			\
			return 0; \
		} \
		\
		struct Delegate1 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg1_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1); \
					RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, 1, &TestClass##id::Func, (p1)); \
				} \
				else \
					RETURN_META_VALUE(MRES_IGNORED, 1); \
			} \
		}; \
		struct Delegate2 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg2_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1); \
					RETURN_META_VALUE_NEWPARAMS(MRES_SUPERCEDE, 2, &TestClass##id::Func, (p1)); \
				} \
				else \
					RETURN_META_VALUE(MRES_SUPERCEDE, 2); \
			} \
		}; \
		struct Delegate3 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg3_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1); \
					RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, 3, &TestClass##id::Func, (p1)); \
				} \
				else \
					RETURN_META_VALUE(MRES_IGNORED, 3); \
			} \
		}; \
		struct Delegate4 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg4_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1); \
					RETURN_META_VALUE_NEWPARAMS(MRES_SUPERCEDE, 4, &TestClass##id::Func, (p1)); \
				} \
				else \
					RETURN_META_VALUE(MRES_SUPERCEDE, 4); \
			}; \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	\
	SourceHook::PassInfo paraminfos_##id[1+1]; \
	SourceHook::PassInfo::V2Info paraminfos2_##id[1+1]; \
	SourceHook::ProtoInfo protoinfo_##id = { 1, {0, 0, 0}, paraminfos_##id, \
		SourceHook::ProtoInfo::CallConv_ThisCall, __SH_EPI, paraminfos2_##id }; \
	\
	SourceHook::Impl::GenContext *g_Genc##id = NULL; \
	CAutoPtrDestruction<SourceHook::Impl::GenContext> g_Genc_ad##id(NULL); 
	
#define THGM_SETUP_PI1(id, p1_type, p1_passtype, p1_flags) \
	void setuppi_##id() \
	{ \
		paraminfos_##id[0].size = 1; paraminfos_##id[0].type = 0; paraminfos_##id[0].flags = 0; \
		\
		 paraminfos_##id[1].size = sizeof(p1_type); paraminfos_##id[1].type = p1_passtype; paraminfos_##id[1].flags = p1_flags;  \
		\
		 paraminfos2_##id[1].pNormalCtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::NormalConstructor) : NULL;  \
		 paraminfos2_##id[1].pCopyCtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::CopyConstructor) : NULL;  \
		 paraminfos2_##id[1].pDtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::Destructor) : NULL;  \
	}
	


#define THGM_MAKE_TEST2_void(id, param1, param2) \
	struct TestClass##id; \
	typedef ParamState2<0, param1, param2 > ParamState_m##id; \
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
		virtual void Func(param1 p1, param2 p2) \
		{ \
			g_Inside_LeafFunc = true; \
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2))); \
			g_Inside_LeafFunc = false; \
		} \
		\
		struct Delegate1 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg1_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2); \
					RETURN_META_NEWPARAMS(MRES_IGNORED, &TestClass##id::Func, (p1, p2)); \
				} \
				else \
					RETURN_META(MRES_IGNORED); \
			} \
		}; \
		struct Delegate2 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg2_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2); \
					RETURN_META_NEWPARAMS(MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2)); \
				} \
				else \
					RETURN_META(MRES_SUPERCEDE); \
			} \
		}; \
		struct Delegate3 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg3_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2); \
					RETURN_META_NEWPARAMS(MRES_IGNORED, &TestClass##id::Func, (p1, p2)); \
				} \
				else \
					RETURN_META(MRES_IGNORED); \
			} \
		}; \
		struct Delegate4 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg4_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2); \
					RETURN_META_NEWPARAMS(MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2)); \
				} \
				else \
					RETURN_META(MRES_SUPERCEDE); \
			}; \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	\
	SourceHook::PassInfo paraminfos_##id[2+1]; \
	SourceHook::PassInfo::V2Info paraminfos2_##id[2+1]; \
	SourceHook::ProtoInfo protoinfo_##id = { 2, {0, 0, 0}, paraminfos_##id, \
		SourceHook::ProtoInfo::CallConv_ThisCall, __SH_EPI, paraminfos2_##id }; \
	\
	SourceHook::Impl::GenContext *g_Genc##id = NULL; \
	CAutoPtrDestruction<SourceHook::Impl::GenContext> g_Genc_ad##id(NULL); 


#define THGM_MAKE_TEST2(id, ret_type, param1, param2) \
	struct TestClass##id; \
	typedef ParamState2<0, param1, param2 > ParamState_m##id; \
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
		virtual ret_type Func(param1 p1, param2 p2) \
		{ \
			g_Inside_LeafFunc = true; \
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2))); \
			g_Inside_LeafFunc = false; \
			\
			return 0; \
		} \
		\
		struct Delegate1 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg1_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2); \
					RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, 1, &TestClass##id::Func, (p1, p2)); \
				} \
				else \
					RETURN_META_VALUE(MRES_IGNORED, 1); \
			} \
		}; \
		struct Delegate2 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg2_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2); \
					RETURN_META_VALUE_NEWPARAMS(MRES_SUPERCEDE, 2, &TestClass##id::Func, (p1, p2)); \
				} \
				else \
					RETURN_META_VALUE(MRES_SUPERCEDE, 2); \
			} \
		}; \
		struct Delegate3 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg3_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2); \
					RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, 3, &TestClass##id::Func, (p1, p2)); \
				} \
				else \
					RETURN_META_VALUE(MRES_IGNORED, 3); \
			} \
		}; \
		struct Delegate4 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg4_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2); \
					RETURN_META_VALUE_NEWPARAMS(MRES_SUPERCEDE, 4, &TestClass##id::Func, (p1, p2)); \
				} \
				else \
					RETURN_META_VALUE(MRES_SUPERCEDE, 4); \
			}; \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	\
	SourceHook::PassInfo paraminfos_##id[2+1]; \
	SourceHook::PassInfo::V2Info paraminfos2_##id[2+1]; \
	SourceHook::ProtoInfo protoinfo_##id = { 2, {0, 0, 0}, paraminfos_##id, \
		SourceHook::ProtoInfo::CallConv_ThisCall, __SH_EPI, paraminfos2_##id }; \
	\
	SourceHook::Impl::GenContext *g_Genc##id = NULL; \
	CAutoPtrDestruction<SourceHook::Impl::GenContext> g_Genc_ad##id(NULL); 
	
#define THGM_SETUP_PI2(id, p1_type, p1_passtype, p1_flags, p2_type, p2_passtype, p2_flags) \
	void setuppi_##id() \
	{ \
		paraminfos_##id[0].size = 1; paraminfos_##id[0].type = 0; paraminfos_##id[0].flags = 0; \
		\
		 paraminfos_##id[1].size = sizeof(p1_type); paraminfos_##id[1].type = p1_passtype; paraminfos_##id[1].flags = p1_flags;  paraminfos_##id[2].size = sizeof(p2_type); paraminfos_##id[2].type = p2_passtype; paraminfos_##id[2].flags = p2_flags;  \
		\
		 paraminfos2_##id[1].pNormalCtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::NormalConstructor) : NULL;  paraminfos2_##id[2].pNormalCtor = (paraminfos_##id[2].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::NormalConstructor) : NULL;  \
		 paraminfos2_##id[1].pCopyCtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::CopyConstructor) : NULL;  paraminfos2_##id[2].pCopyCtor = (paraminfos_##id[2].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::CopyConstructor) : NULL;  \
		 paraminfos2_##id[1].pDtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::Destructor) : NULL;  paraminfos2_##id[2].pDtor = (paraminfos_##id[2].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::Destructor) : NULL;  \
	}
	


#define THGM_MAKE_TEST3_void(id, param1, param2, param3) \
	struct TestClass##id; \
	typedef ParamState3<0, param1, param2, param3 > ParamState_m##id; \
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
		virtual void Func(param1 p1, param2 p2, param3 p3) \
		{ \
			g_Inside_LeafFunc = true; \
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, p3))); \
			g_Inside_LeafFunc = false; \
		} \
		\
		struct Delegate1 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2, param3 p3) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg1_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3); \
					RETURN_META_NEWPARAMS(MRES_IGNORED, &TestClass##id::Func, (p1, p2, p3)); \
				} \
				else \
					RETURN_META(MRES_IGNORED); \
			} \
		}; \
		struct Delegate2 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2, param3 p3) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg2_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3); \
					RETURN_META_NEWPARAMS(MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2, p3)); \
				} \
				else \
					RETURN_META(MRES_SUPERCEDE); \
			} \
		}; \
		struct Delegate3 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2, param3 p3) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg3_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3); \
					RETURN_META_NEWPARAMS(MRES_IGNORED, &TestClass##id::Func, (p1, p2, p3)); \
				} \
				else \
					RETURN_META(MRES_IGNORED); \
			} \
		}; \
		struct Delegate4 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2, param3 p3) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg4_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3); \
					RETURN_META_NEWPARAMS(MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2, p3)); \
				} \
				else \
					RETURN_META(MRES_SUPERCEDE); \
			}; \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	\
	SourceHook::PassInfo paraminfos_##id[3+1]; \
	SourceHook::PassInfo::V2Info paraminfos2_##id[3+1]; \
	SourceHook::ProtoInfo protoinfo_##id = { 3, {0, 0, 0}, paraminfos_##id, \
		SourceHook::ProtoInfo::CallConv_ThisCall, __SH_EPI, paraminfos2_##id }; \
	\
	SourceHook::Impl::GenContext *g_Genc##id = NULL; \
	CAutoPtrDestruction<SourceHook::Impl::GenContext> g_Genc_ad##id(NULL); 


#define THGM_MAKE_TEST3(id, ret_type, param1, param2, param3) \
	struct TestClass##id; \
	typedef ParamState3<0, param1, param2, param3 > ParamState_m##id; \
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
		virtual ret_type Func(param1 p1, param2 p2, param3 p3) \
		{ \
			g_Inside_LeafFunc = true; \
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, p3))); \
			g_Inside_LeafFunc = false; \
			\
			return 0; \
		} \
		\
		struct Delegate1 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2, param3 p3) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg1_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3); \
					RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, 1, &TestClass##id::Func, (p1, p2, p3)); \
				} \
				else \
					RETURN_META_VALUE(MRES_IGNORED, 1); \
			} \
		}; \
		struct Delegate2 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2, param3 p3) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg2_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3); \
					RETURN_META_VALUE_NEWPARAMS(MRES_SUPERCEDE, 2, &TestClass##id::Func, (p1, p2, p3)); \
				} \
				else \
					RETURN_META_VALUE(MRES_SUPERCEDE, 2); \
			} \
		}; \
		struct Delegate3 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2, param3 p3) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg3_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3); \
					RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, 3, &TestClass##id::Func, (p1, p2, p3)); \
				} \
				else \
					RETURN_META_VALUE(MRES_IGNORED, 3); \
			} \
		}; \
		struct Delegate4 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2, param3 p3) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg4_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3); \
					RETURN_META_VALUE_NEWPARAMS(MRES_SUPERCEDE, 4, &TestClass##id::Func, (p1, p2, p3)); \
				} \
				else \
					RETURN_META_VALUE(MRES_SUPERCEDE, 4); \
			}; \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	\
	SourceHook::PassInfo paraminfos_##id[3+1]; \
	SourceHook::PassInfo::V2Info paraminfos2_##id[3+1]; \
	SourceHook::ProtoInfo protoinfo_##id = { 3, {0, 0, 0}, paraminfos_##id, \
		SourceHook::ProtoInfo::CallConv_ThisCall, __SH_EPI, paraminfos2_##id }; \
	\
	SourceHook::Impl::GenContext *g_Genc##id = NULL; \
	CAutoPtrDestruction<SourceHook::Impl::GenContext> g_Genc_ad##id(NULL); 
	
#define THGM_SETUP_PI3(id, p1_type, p1_passtype, p1_flags, p2_type, p2_passtype, p2_flags, p3_type, p3_passtype, p3_flags) \
	void setuppi_##id() \
	{ \
		paraminfos_##id[0].size = 1; paraminfos_##id[0].type = 0; paraminfos_##id[0].flags = 0; \
		\
		 paraminfos_##id[1].size = sizeof(p1_type); paraminfos_##id[1].type = p1_passtype; paraminfos_##id[1].flags = p1_flags;  paraminfos_##id[2].size = sizeof(p2_type); paraminfos_##id[2].type = p2_passtype; paraminfos_##id[2].flags = p2_flags;  paraminfos_##id[3].size = sizeof(p3_type); paraminfos_##id[3].type = p3_passtype; paraminfos_##id[3].flags = p3_flags;  \
		\
		 paraminfos2_##id[1].pNormalCtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::NormalConstructor) : NULL;  paraminfos2_##id[2].pNormalCtor = (paraminfos_##id[2].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::NormalConstructor) : NULL;  paraminfos2_##id[3].pNormalCtor = (paraminfos_##id[3].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::NormalConstructor) : NULL;  \
		 paraminfos2_##id[1].pCopyCtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::CopyConstructor) : NULL;  paraminfos2_##id[2].pCopyCtor = (paraminfos_##id[2].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::CopyConstructor) : NULL;  paraminfos2_##id[3].pCopyCtor = (paraminfos_##id[3].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::CopyConstructor) : NULL;  \
		 paraminfos2_##id[1].pDtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::Destructor) : NULL;  paraminfos2_##id[2].pDtor = (paraminfos_##id[2].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::Destructor) : NULL;  paraminfos2_##id[3].pDtor = (paraminfos_##id[3].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::Destructor) : NULL;  \
	}
	


#define THGM_MAKE_TEST4_void(id, param1, param2, param3, param4) \
	struct TestClass##id; \
	typedef ParamState4<0, param1, param2, param3, param4 > ParamState_m##id; \
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
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4) \
		{ \
			g_Inside_LeafFunc = true; \
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, p3, p4))); \
			g_Inside_LeafFunc = false; \
		} \
		\
		struct Delegate1 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg1_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4); \
					RETURN_META_NEWPARAMS(MRES_IGNORED, &TestClass##id::Func, (p1, p2, p3, p4)); \
				} \
				else \
					RETURN_META(MRES_IGNORED); \
			} \
		}; \
		struct Delegate2 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg2_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4); \
					RETURN_META_NEWPARAMS(MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2, p3, p4)); \
				} \
				else \
					RETURN_META(MRES_SUPERCEDE); \
			} \
		}; \
		struct Delegate3 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg3_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4); \
					RETURN_META_NEWPARAMS(MRES_IGNORED, &TestClass##id::Func, (p1, p2, p3, p4)); \
				} \
				else \
					RETURN_META(MRES_IGNORED); \
			} \
		}; \
		struct Delegate4 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg4_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4); \
					RETURN_META_NEWPARAMS(MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2, p3, p4)); \
				} \
				else \
					RETURN_META(MRES_SUPERCEDE); \
			}; \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	\
	SourceHook::PassInfo paraminfos_##id[4+1]; \
	SourceHook::PassInfo::V2Info paraminfos2_##id[4+1]; \
	SourceHook::ProtoInfo protoinfo_##id = { 4, {0, 0, 0}, paraminfos_##id, \
		SourceHook::ProtoInfo::CallConv_ThisCall, __SH_EPI, paraminfos2_##id }; \
	\
	SourceHook::Impl::GenContext *g_Genc##id = NULL; \
	CAutoPtrDestruction<SourceHook::Impl::GenContext> g_Genc_ad##id(NULL); 


#define THGM_MAKE_TEST4(id, ret_type, param1, param2, param3, param4) \
	struct TestClass##id; \
	typedef ParamState4<0, param1, param2, param3, param4 > ParamState_m##id; \
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
		virtual ret_type Func(param1 p1, param2 p2, param3 p3, param4 p4) \
		{ \
			g_Inside_LeafFunc = true; \
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, p3, p4))); \
			g_Inside_LeafFunc = false; \
			\
			return 0; \
		} \
		\
		struct Delegate1 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg1_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4); \
					RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, 1, &TestClass##id::Func, (p1, p2, p3, p4)); \
				} \
				else \
					RETURN_META_VALUE(MRES_IGNORED, 1); \
			} \
		}; \
		struct Delegate2 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg2_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4); \
					RETURN_META_VALUE_NEWPARAMS(MRES_SUPERCEDE, 2, &TestClass##id::Func, (p1, p2, p3, p4)); \
				} \
				else \
					RETURN_META_VALUE(MRES_SUPERCEDE, 2); \
			} \
		}; \
		struct Delegate3 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg3_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4); \
					RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, 3, &TestClass##id::Func, (p1, p2, p3, p4)); \
				} \
				else \
					RETURN_META_VALUE(MRES_IGNORED, 3); \
			} \
		}; \
		struct Delegate4 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg4_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4); \
					RETURN_META_VALUE_NEWPARAMS(MRES_SUPERCEDE, 4, &TestClass##id::Func, (p1, p2, p3, p4)); \
				} \
				else \
					RETURN_META_VALUE(MRES_SUPERCEDE, 4); \
			}; \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	\
	SourceHook::PassInfo paraminfos_##id[4+1]; \
	SourceHook::PassInfo::V2Info paraminfos2_##id[4+1]; \
	SourceHook::ProtoInfo protoinfo_##id = { 4, {0, 0, 0}, paraminfos_##id, \
		SourceHook::ProtoInfo::CallConv_ThisCall, __SH_EPI, paraminfos2_##id }; \
	\
	SourceHook::Impl::GenContext *g_Genc##id = NULL; \
	CAutoPtrDestruction<SourceHook::Impl::GenContext> g_Genc_ad##id(NULL); 
	
#define THGM_SETUP_PI4(id, p1_type, p1_passtype, p1_flags, p2_type, p2_passtype, p2_flags, p3_type, p3_passtype, p3_flags, p4_type, p4_passtype, p4_flags) \
	void setuppi_##id() \
	{ \
		paraminfos_##id[0].size = 1; paraminfos_##id[0].type = 0; paraminfos_##id[0].flags = 0; \
		\
		 paraminfos_##id[1].size = sizeof(p1_type); paraminfos_##id[1].type = p1_passtype; paraminfos_##id[1].flags = p1_flags;  paraminfos_##id[2].size = sizeof(p2_type); paraminfos_##id[2].type = p2_passtype; paraminfos_##id[2].flags = p2_flags;  paraminfos_##id[3].size = sizeof(p3_type); paraminfos_##id[3].type = p3_passtype; paraminfos_##id[3].flags = p3_flags;  paraminfos_##id[4].size = sizeof(p4_type); paraminfos_##id[4].type = p4_passtype; paraminfos_##id[4].flags = p4_flags;  \
		\
		 paraminfos2_##id[1].pNormalCtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::NormalConstructor) : NULL;  paraminfos2_##id[2].pNormalCtor = (paraminfos_##id[2].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::NormalConstructor) : NULL;  paraminfos2_##id[3].pNormalCtor = (paraminfos_##id[3].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::NormalConstructor) : NULL;  paraminfos2_##id[4].pNormalCtor = (paraminfos_##id[4].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::NormalConstructor) : NULL;  \
		 paraminfos2_##id[1].pCopyCtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::CopyConstructor) : NULL;  paraminfos2_##id[2].pCopyCtor = (paraminfos_##id[2].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::CopyConstructor) : NULL;  paraminfos2_##id[3].pCopyCtor = (paraminfos_##id[3].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::CopyConstructor) : NULL;  paraminfos2_##id[4].pCopyCtor = (paraminfos_##id[4].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::CopyConstructor) : NULL;  \
		 paraminfos2_##id[1].pDtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::Destructor) : NULL;  paraminfos2_##id[2].pDtor = (paraminfos_##id[2].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::Destructor) : NULL;  paraminfos2_##id[3].pDtor = (paraminfos_##id[3].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::Destructor) : NULL;  paraminfos2_##id[4].pDtor = (paraminfos_##id[4].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::Destructor) : NULL;  \
	}
	


#define THGM_MAKE_TEST5_void(id, param1, param2, param3, param4, param5) \
	struct TestClass##id; \
	typedef ParamState5<0, param1, param2, param3, param4, param5 > ParamState_m##id; \
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
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
		{ \
			g_Inside_LeafFunc = true; \
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, p3, p4, p5))); \
			g_Inside_LeafFunc = false; \
		} \
		\
		struct Delegate1 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg1_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5); \
					RETURN_META_NEWPARAMS(MRES_IGNORED, &TestClass##id::Func, (p1, p2, p3, p4, p5)); \
				} \
				else \
					RETURN_META(MRES_IGNORED); \
			} \
		}; \
		struct Delegate2 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg2_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5); \
					RETURN_META_NEWPARAMS(MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2, p3, p4, p5)); \
				} \
				else \
					RETURN_META(MRES_SUPERCEDE); \
			} \
		}; \
		struct Delegate3 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg3_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5); \
					RETURN_META_NEWPARAMS(MRES_IGNORED, &TestClass##id::Func, (p1, p2, p3, p4, p5)); \
				} \
				else \
					RETURN_META(MRES_IGNORED); \
			} \
		}; \
		struct Delegate4 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg4_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5); \
					RETURN_META_NEWPARAMS(MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2, p3, p4, p5)); \
				} \
				else \
					RETURN_META(MRES_SUPERCEDE); \
			}; \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	\
	SourceHook::PassInfo paraminfos_##id[5+1]; \
	SourceHook::PassInfo::V2Info paraminfos2_##id[5+1]; \
	SourceHook::ProtoInfo protoinfo_##id = { 5, {0, 0, 0}, paraminfos_##id, \
		SourceHook::ProtoInfo::CallConv_ThisCall, __SH_EPI, paraminfos2_##id }; \
	\
	SourceHook::Impl::GenContext *g_Genc##id = NULL; \
	CAutoPtrDestruction<SourceHook::Impl::GenContext> g_Genc_ad##id(NULL); 


#define THGM_MAKE_TEST5(id, ret_type, param1, param2, param3, param4, param5) \
	struct TestClass##id; \
	typedef ParamState5<0, param1, param2, param3, param4, param5 > ParamState_m##id; \
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
		virtual ret_type Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
		{ \
			g_Inside_LeafFunc = true; \
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, p3, p4, p5))); \
			g_Inside_LeafFunc = false; \
			\
			return 0; \
		} \
		\
		struct Delegate1 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg1_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5); \
					RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, 1, &TestClass##id::Func, (p1, p2, p3, p4, p5)); \
				} \
				else \
					RETURN_META_VALUE(MRES_IGNORED, 1); \
			} \
		}; \
		struct Delegate2 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg2_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5); \
					RETURN_META_VALUE_NEWPARAMS(MRES_SUPERCEDE, 2, &TestClass##id::Func, (p1, p2, p3, p4, p5)); \
				} \
				else \
					RETURN_META_VALUE(MRES_SUPERCEDE, 2); \
			} \
		}; \
		struct Delegate3 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg3_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5); \
					RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, 3, &TestClass##id::Func, (p1, p2, p3, p4, p5)); \
				} \
				else \
					RETURN_META_VALUE(MRES_IGNORED, 3); \
			} \
		}; \
		struct Delegate4 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg4_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5); \
					RETURN_META_VALUE_NEWPARAMS(MRES_SUPERCEDE, 4, &TestClass##id::Func, (p1, p2, p3, p4, p5)); \
				} \
				else \
					RETURN_META_VALUE(MRES_SUPERCEDE, 4); \
			}; \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	\
	SourceHook::PassInfo paraminfos_##id[5+1]; \
	SourceHook::PassInfo::V2Info paraminfos2_##id[5+1]; \
	SourceHook::ProtoInfo protoinfo_##id = { 5, {0, 0, 0}, paraminfos_##id, \
		SourceHook::ProtoInfo::CallConv_ThisCall, __SH_EPI, paraminfos2_##id }; \
	\
	SourceHook::Impl::GenContext *g_Genc##id = NULL; \
	CAutoPtrDestruction<SourceHook::Impl::GenContext> g_Genc_ad##id(NULL); 
	
#define THGM_SETUP_PI5(id, p1_type, p1_passtype, p1_flags, p2_type, p2_passtype, p2_flags, p3_type, p3_passtype, p3_flags, p4_type, p4_passtype, p4_flags, p5_type, p5_passtype, p5_flags) \
	void setuppi_##id() \
	{ \
		paraminfos_##id[0].size = 1; paraminfos_##id[0].type = 0; paraminfos_##id[0].flags = 0; \
		\
		 paraminfos_##id[1].size = sizeof(p1_type); paraminfos_##id[1].type = p1_passtype; paraminfos_##id[1].flags = p1_flags;  paraminfos_##id[2].size = sizeof(p2_type); paraminfos_##id[2].type = p2_passtype; paraminfos_##id[2].flags = p2_flags;  paraminfos_##id[3].size = sizeof(p3_type); paraminfos_##id[3].type = p3_passtype; paraminfos_##id[3].flags = p3_flags;  paraminfos_##id[4].size = sizeof(p4_type); paraminfos_##id[4].type = p4_passtype; paraminfos_##id[4].flags = p4_flags;  paraminfos_##id[5].size = sizeof(p5_type); paraminfos_##id[5].type = p5_passtype; paraminfos_##id[5].flags = p5_flags;  \
		\
		 paraminfos2_##id[1].pNormalCtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::NormalConstructor) : NULL;  paraminfos2_##id[2].pNormalCtor = (paraminfos_##id[2].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::NormalConstructor) : NULL;  paraminfos2_##id[3].pNormalCtor = (paraminfos_##id[3].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::NormalConstructor) : NULL;  paraminfos2_##id[4].pNormalCtor = (paraminfos_##id[4].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::NormalConstructor) : NULL;  paraminfos2_##id[5].pNormalCtor = (paraminfos_##id[5].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p5_type >::type>::NormalConstructor) : NULL;  \
		 paraminfos2_##id[1].pCopyCtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::CopyConstructor) : NULL;  paraminfos2_##id[2].pCopyCtor = (paraminfos_##id[2].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::CopyConstructor) : NULL;  paraminfos2_##id[3].pCopyCtor = (paraminfos_##id[3].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::CopyConstructor) : NULL;  paraminfos2_##id[4].pCopyCtor = (paraminfos_##id[4].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::CopyConstructor) : NULL;  paraminfos2_##id[5].pCopyCtor = (paraminfos_##id[5].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p5_type >::type>::CopyConstructor) : NULL;  \
		 paraminfos2_##id[1].pDtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::Destructor) : NULL;  paraminfos2_##id[2].pDtor = (paraminfos_##id[2].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::Destructor) : NULL;  paraminfos2_##id[3].pDtor = (paraminfos_##id[3].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::Destructor) : NULL;  paraminfos2_##id[4].pDtor = (paraminfos_##id[4].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::Destructor) : NULL;  paraminfos2_##id[5].pDtor = (paraminfos_##id[5].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p5_type >::type>::Destructor) : NULL;  \
	}
	


#define THGM_MAKE_TEST6_void(id, param1, param2, param3, param4, param5, param6) \
	struct TestClass##id; \
	typedef ParamState6<0, param1, param2, param3, param4, param5, param6 > ParamState_m##id; \
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
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
		{ \
			g_Inside_LeafFunc = true; \
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, p3, p4, p5, p6))); \
			g_Inside_LeafFunc = false; \
		} \
		\
		struct Delegate1 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg1_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5, p6))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5);Increment<StripRef< param6 >::type>::Incr(p6); \
					RETURN_META_NEWPARAMS(MRES_IGNORED, &TestClass##id::Func, (p1, p2, p3, p4, p5, p6)); \
				} \
				else \
					RETURN_META(MRES_IGNORED); \
			} \
		}; \
		struct Delegate2 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg2_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5, p6))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5);Increment<StripRef< param6 >::type>::Incr(p6); \
					RETURN_META_NEWPARAMS(MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2, p3, p4, p5, p6)); \
				} \
				else \
					RETURN_META(MRES_SUPERCEDE); \
			} \
		}; \
		struct Delegate3 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg3_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5, p6))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5);Increment<StripRef< param6 >::type>::Incr(p6); \
					RETURN_META_NEWPARAMS(MRES_IGNORED, &TestClass##id::Func, (p1, p2, p3, p4, p5, p6)); \
				} \
				else \
					RETURN_META(MRES_IGNORED); \
			} \
		}; \
		struct Delegate4 : public MyDelegate \
		{ \
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg4_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5, p6))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5);Increment<StripRef< param6 >::type>::Incr(p6); \
					RETURN_META_NEWPARAMS(MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2, p3, p4, p5, p6)); \
				} \
				else \
					RETURN_META(MRES_SUPERCEDE); \
			}; \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	\
	SourceHook::PassInfo paraminfos_##id[6+1]; \
	SourceHook::PassInfo::V2Info paraminfos2_##id[6+1]; \
	SourceHook::ProtoInfo protoinfo_##id = { 6, {0, 0, 0}, paraminfos_##id, \
		SourceHook::ProtoInfo::CallConv_ThisCall, __SH_EPI, paraminfos2_##id }; \
	\
	SourceHook::Impl::GenContext *g_Genc##id = NULL; \
	CAutoPtrDestruction<SourceHook::Impl::GenContext> g_Genc_ad##id(NULL); 


#define THGM_MAKE_TEST6(id, ret_type, param1, param2, param3, param4, param5, param6) \
	struct TestClass##id; \
	typedef ParamState6<0, param1, param2, param3, param4, param5, param6 > ParamState_m##id; \
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
		virtual ret_type Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
		{ \
			g_Inside_LeafFunc = true; \
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, p3, p4, p5, p6))); \
			g_Inside_LeafFunc = false; \
			\
			return 0; \
		} \
		\
		struct Delegate1 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg1_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5, p6))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5);Increment<StripRef< param6 >::type>::Incr(p6); \
					RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, 1, &TestClass##id::Func, (p1, p2, p3, p4, p5, p6)); \
				} \
				else \
					RETURN_META_VALUE(MRES_IGNORED, 1); \
			} \
		}; \
		struct Delegate2 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg2_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5, p6))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5);Increment<StripRef< param6 >::type>::Incr(p6); \
					RETURN_META_VALUE_NEWPARAMS(MRES_SUPERCEDE, 2, &TestClass##id::Func, (p1, p2, p3, p4, p5, p6)); \
				} \
				else \
					RETURN_META_VALUE(MRES_SUPERCEDE, 2); \
			} \
		}; \
		struct Delegate3 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg3_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5, p6))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5);Increment<StripRef< param6 >::type>::Incr(p6); \
					RETURN_META_VALUE_NEWPARAMS(MRES_IGNORED, 3, &TestClass##id::Func, (p1, p2, p3, p4, p5, p6)); \
				} \
				else \
					RETURN_META_VALUE(MRES_IGNORED, 3); \
			} \
		}; \
		struct Delegate4 : public MyDelegate \
		{ \
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg4_##id(META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5, p6))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5);Increment<StripRef< param6 >::type>::Incr(p6); \
					RETURN_META_VALUE_NEWPARAMS(MRES_SUPERCEDE, 4, &TestClass##id::Func, (p1, p2, p3, p4, p5, p6)); \
				} \
				else \
					RETURN_META_VALUE(MRES_SUPERCEDE, 4); \
			}; \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	\
	SourceHook::PassInfo paraminfos_##id[6+1]; \
	SourceHook::PassInfo::V2Info paraminfos2_##id[6+1]; \
	SourceHook::ProtoInfo protoinfo_##id = { 6, {0, 0, 0}, paraminfos_##id, \
		SourceHook::ProtoInfo::CallConv_ThisCall, __SH_EPI, paraminfos2_##id }; \
	\
	SourceHook::Impl::GenContext *g_Genc##id = NULL; \
	CAutoPtrDestruction<SourceHook::Impl::GenContext> g_Genc_ad##id(NULL); 
	
#define THGM_SETUP_PI6(id, p1_type, p1_passtype, p1_flags, p2_type, p2_passtype, p2_flags, p3_type, p3_passtype, p3_flags, p4_type, p4_passtype, p4_flags, p5_type, p5_passtype, p5_flags, p6_type, p6_passtype, p6_flags) \
	void setuppi_##id() \
	{ \
		paraminfos_##id[0].size = 1; paraminfos_##id[0].type = 0; paraminfos_##id[0].flags = 0; \
		\
		 paraminfos_##id[1].size = sizeof(p1_type); paraminfos_##id[1].type = p1_passtype; paraminfos_##id[1].flags = p1_flags;  paraminfos_##id[2].size = sizeof(p2_type); paraminfos_##id[2].type = p2_passtype; paraminfos_##id[2].flags = p2_flags;  paraminfos_##id[3].size = sizeof(p3_type); paraminfos_##id[3].type = p3_passtype; paraminfos_##id[3].flags = p3_flags;  paraminfos_##id[4].size = sizeof(p4_type); paraminfos_##id[4].type = p4_passtype; paraminfos_##id[4].flags = p4_flags;  paraminfos_##id[5].size = sizeof(p5_type); paraminfos_##id[5].type = p5_passtype; paraminfos_##id[5].flags = p5_flags;  paraminfos_##id[6].size = sizeof(p6_type); paraminfos_##id[6].type = p6_passtype; paraminfos_##id[6].flags = p6_flags;  \
		\
		 paraminfos2_##id[1].pNormalCtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::NormalConstructor) : NULL;  paraminfos2_##id[2].pNormalCtor = (paraminfos_##id[2].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::NormalConstructor) : NULL;  paraminfos2_##id[3].pNormalCtor = (paraminfos_##id[3].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::NormalConstructor) : NULL;  paraminfos2_##id[4].pNormalCtor = (paraminfos_##id[4].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::NormalConstructor) : NULL;  paraminfos2_##id[5].pNormalCtor = (paraminfos_##id[5].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p5_type >::type>::NormalConstructor) : NULL;  paraminfos2_##id[6].pNormalCtor = (paraminfos_##id[6].flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p6_type >::type>::NormalConstructor) : NULL;  \
		 paraminfos2_##id[1].pCopyCtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::CopyConstructor) : NULL;  paraminfos2_##id[2].pCopyCtor = (paraminfos_##id[2].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::CopyConstructor) : NULL;  paraminfos2_##id[3].pCopyCtor = (paraminfos_##id[3].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::CopyConstructor) : NULL;  paraminfos2_##id[4].pCopyCtor = (paraminfos_##id[4].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::CopyConstructor) : NULL;  paraminfos2_##id[5].pCopyCtor = (paraminfos_##id[5].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p5_type >::type>::CopyConstructor) : NULL;  paraminfos2_##id[6].pCopyCtor = (paraminfos_##id[6].flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p6_type >::type>::CopyConstructor) : NULL;  \
		 paraminfos2_##id[1].pDtor = (paraminfos_##id[1].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::Destructor) : NULL;  paraminfos2_##id[2].pDtor = (paraminfos_##id[2].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::Destructor) : NULL;  paraminfos2_##id[3].pDtor = (paraminfos_##id[3].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::Destructor) : NULL;  paraminfos2_##id[4].pDtor = (paraminfos_##id[4].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::Destructor) : NULL;  paraminfos2_##id[5].pDtor = (paraminfos_##id[5].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p5_type >::type>::Destructor) : NULL;  paraminfos2_##id[6].pDtor = (paraminfos_##id[6].flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p6_type >::type>::Destructor) : NULL;  \
	}
	


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
