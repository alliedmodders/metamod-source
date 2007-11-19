struct CAutoReleaseHookMan
{
	SourceHook::HookManagerPubFunc m_Ptr;
	CAutoReleaseHookMan(SourceHook::HookManagerPubFunc ptr) : m_Ptr(ptr)
	{
	}
	~CAutoReleaseHookMan()
	{
		g_HMAGPtr->ReleaseHookMan(m_Ptr);
	}
};

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

	const T& AssignOp(const T &other)
	{
		return (*reinterpret_cast<T*>(this) = other);
	}
};


template <class T>
void *FindFuncAddr(T mfp)
{
	union
	{
		T a;
		void *b;
	} u;
	u.a = mfp;
	return u.b;
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

#if SH_COMP==SH_COMP_GCC
#define NO_OPTIMIZE __attribute__((noinline))
#else
#define NO_OPTIMIZE
#endif

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
		POD<SIZE> x;
		memset(reinterpret_cast<void*>(x.x), a, SIZE);
		return x;
	}
};

// Stores parameter status
template <class T>
bool EqualToMyFmtString(T sth)
{
	return false;
}

bool EqualToMyFmtString(std::string &sth)
{
	if (sth == "Hello %s%d%s")
		sth = "Hello BA1L!";
	return true;
}




template<int dummy>
struct ParamState0
{
	
	
	bool operator==(const ParamState0<dummy> &other)
	{
		return true
			
			;
	}
	ParamState0(...)  
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
	ParamState1(p1 a1, ...) : m_1(a1)
	{
		
		EqualToMyFmtString(m_1);
		
	}
	
	ParamState1<dummy, p1> & operator() (int incrsteps)
	{
		int i;
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p1 >::type >::Incr(m_1);
		
		
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
	ParamState2(p1 a1, p2 a2, ...) : m_1(a1), m_2(a2)
	{
		
		EqualToMyFmtString(m_1);
		
		EqualToMyFmtString(m_2);
		
	}
	
	ParamState2<dummy, p1, p2> & operator() (int incrsteps)
	{
		int i;
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p1 >::type >::Incr(m_1);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p2 >::type >::Incr(m_2);
		
		
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
	ParamState3(p1 a1, p2 a2, p3 a3, ...) : m_1(a1), m_2(a2), m_3(a3)
	{
		
		EqualToMyFmtString(m_1);
		
		EqualToMyFmtString(m_2);
		
		EqualToMyFmtString(m_3);
		
	}
	
	ParamState3<dummy, p1, p2, p3> & operator() (int incrsteps)
	{
		int i;
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p1 >::type >::Incr(m_1);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p2 >::type >::Incr(m_2);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p3 >::type >::Incr(m_3);
		
		
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
	ParamState4(p1 a1, p2 a2, p3 a3, p4 a4, ...) : m_1(a1), m_2(a2), m_3(a3), m_4(a4)
	{
		
		EqualToMyFmtString(m_1);
		
		EqualToMyFmtString(m_2);
		
		EqualToMyFmtString(m_3);
		
		EqualToMyFmtString(m_4);
		
	}
	
	ParamState4<dummy, p1, p2, p3, p4> & operator() (int incrsteps)
	{
		int i;
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p1 >::type >::Incr(m_1);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p2 >::type >::Incr(m_2);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p3 >::type >::Incr(m_3);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p4 >::type >::Incr(m_4);
		
		
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
	ParamState5(p1 a1, p2 a2, p3 a3, p4 a4, p5 a5, ...) : m_1(a1), m_2(a2), m_3(a3), m_4(a4), m_5(a5)
	{
		
		EqualToMyFmtString(m_1);
		
		EqualToMyFmtString(m_2);
		
		EqualToMyFmtString(m_3);
		
		EqualToMyFmtString(m_4);
		
		EqualToMyFmtString(m_5);
		
	}
	
	ParamState5<dummy, p1, p2, p3, p4, p5> & operator() (int incrsteps)
	{
		int i;
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p1 >::type >::Incr(m_1);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p2 >::type >::Incr(m_2);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p3 >::type >::Incr(m_3);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p4 >::type >::Incr(m_4);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p5 >::type >::Incr(m_5);
		
		
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
	ParamState6(p1 a1, p2 a2, p3 a3, p4 a4, p5 a5, p6 a6, ...) : m_1(a1), m_2(a2), m_3(a3), m_4(a4), m_5(a5), m_6(a6)
	{
		
		EqualToMyFmtString(m_1);
		
		EqualToMyFmtString(m_2);
		
		EqualToMyFmtString(m_3);
		
		EqualToMyFmtString(m_4);
		
		EqualToMyFmtString(m_5);
		
		EqualToMyFmtString(m_6);
		
	}
	
	ParamState6<dummy, p1, p2, p3, p4, p5, p6> & operator() (int incrsteps)
	{
		int i;
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p1 >::type >::Incr(m_1);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p2 >::type >::Incr(m_2);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p3 >::type >::Incr(m_3);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p4 >::type >::Incr(m_4);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p5 >::type >::Incr(m_5);
		
		for (i = 0; i < incrsteps; ++i)
			Increment<typename StripRef< p6 >::type >::Incr(m_6);
		
		
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
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
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
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual void Call() \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id())); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					 \
					RETURN_META_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, &TestClass##id::Func, ()); \
				} \
				else \
					RETURN_META((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall);


#define THGM_MAKE_TEST0(id, ret_type) \
	struct TestClass##id; \
	typedef ret_type RetType##id; \
	typedef ParamState0<0 > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
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
			return MakeRet< ret_type >::Do(0); \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual ret_type Call() \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id())); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					 \
					RETURN_META_VALUE_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber), &TestClass##id::Func, ()); \
				} \
				else \
					RETURN_META_VALUE((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber)); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall);
	
#define THGM_MAKE_TEST0_vafmt_void(id) \
	struct TestClass##id; \
	typedef ParamState1<0, std::string > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	\
	struct TestClass##id \
	{ \
		static bool ms_DoRecall; \
		\
		virtual void Func(const char *fmt, ...) \
		{ \
			g_Inside_LeafFunc = true; \
			\
			char buf[9999]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, 9998, fmt, ap); \
			buf[9998] = 0; \
			va_end(ap); \
			\
			ADD_STATE(State_Func##id(this, ParamState_m##id(std::string(buf)))); \
			g_Inside_LeafFunc = false; \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual void Call(const char *buf) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(buf))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					 \
					RETURN_META_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, &TestClass##id::Func, ("%s!", buf)); \
				} \
				else \
					RETURN_META((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall | SourceHook::ProtoInfo::CallConv_HasVafmt);

#define THGM_MAKE_TEST0_vafmt(id, ret_type) \
	struct TestClass##id; \
	typedef ret_type RetType##id; \
	typedef ParamState1<0, std::string > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	\
	struct TestClass##id \
	{ \
		static bool ms_DoRecall; \
		\
		virtual ret_type Func(const char *fmt, ...) \
		{ \
			g_Inside_LeafFunc = true; \
			\
			char buf[9999]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, 9998, fmt, ap); \
			buf[9998] = 0; \
			va_end(ap); \
			\
			ADD_STATE(State_Func##id(this, ParamState_m##id(std::string(buf)))); \
			\
			return MakeRet< ret_type >::Do(0); \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual ret_type Call(const char *buf) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(buf))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					 \
					RETURN_META_VALUE_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber), &TestClass##id::Func, ("%s!", buf)); \
				} \
				else \
					RETURN_META_VALUE((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber)); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall | SourceHook::ProtoInfo::CallConv_HasVafmt);

#define THGM_SETUP_PI0(id) \
	void setuppi_##id() \
	{ \
		 \
	}
	


#define THGM_MAKE_TEST1_void(id, param1) \
	struct TestClass##id; \
	typedef ParamState1<0, param1 > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
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
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual void Call(param1 p1) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1); \
					RETURN_META_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, &TestClass##id::Func, (p1)); \
				} \
				else \
					RETURN_META((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall);


#define THGM_MAKE_TEST1(id, ret_type, param1) \
	struct TestClass##id; \
	typedef ret_type RetType##id; \
	typedef ParamState1<0, param1 > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
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
			return MakeRet< ret_type >::Do(0); \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual ret_type Call(param1 p1) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1); \
					RETURN_META_VALUE_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber), &TestClass##id::Func, (p1)); \
				} \
				else \
					RETURN_META_VALUE((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber)); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall);
	
#define THGM_MAKE_TEST1_vafmt_void(id, param1) \
	struct TestClass##id; \
	typedef ParamState2<0, param1, std::string > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	\
	struct TestClass##id \
	{ \
		static bool ms_DoRecall; \
		\
		virtual void Func(param1 p1, const char *fmt, ...) \
		{ \
			g_Inside_LeafFunc = true; \
			\
			char buf[9999]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, 9998, fmt, ap); \
			buf[9998] = 0; \
			va_end(ap); \
			\
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, std::string(buf)))); \
			g_Inside_LeafFunc = false; \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual void Call(param1 p1, const char *buf) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, buf))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1); \
					RETURN_META_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, &TestClass##id::Func, (p1, "%s!", buf)); \
				} \
				else \
					RETURN_META((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall | SourceHook::ProtoInfo::CallConv_HasVafmt);

#define THGM_MAKE_TEST1_vafmt(id, ret_type, param1) \
	struct TestClass##id; \
	typedef ret_type RetType##id; \
	typedef ParamState2<0, param1, std::string > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	\
	struct TestClass##id \
	{ \
		static bool ms_DoRecall; \
		\
		virtual ret_type Func(param1 p1, const char *fmt, ...) \
		{ \
			g_Inside_LeafFunc = true; \
			\
			char buf[9999]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, 9998, fmt, ap); \
			buf[9998] = 0; \
			va_end(ap); \
			\
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, std::string(buf)))); \
			\
			return MakeRet< ret_type >::Do(0); \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual ret_type Call(param1 p1, const char *buf) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, buf))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1); \
					RETURN_META_VALUE_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber), &TestClass##id::Func, (p1, "%s!", buf)); \
				} \
				else \
					RETURN_META_VALUE((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber)); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall | SourceHook::ProtoInfo::CallConv_HasVafmt);

#define THGM_SETUP_PI1(id, p1_type, p1_passtype, p1_flags) \
	void setuppi_##id() \
	{ \
		 \
		protoinfo_##id.AddParam(sizeof(p1_type), p1_passtype, p1_flags, \
			(p1_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::NormalConstructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::CopyConstructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::Destructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::AssignOp) : NULL \
			); \
		 \
	}
	


#define THGM_MAKE_TEST2_void(id, param1, param2) \
	struct TestClass##id; \
	typedef ParamState2<0, param1, param2 > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
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
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual void Call(param1 p1, param2 p2) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2); \
					RETURN_META_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2)); \
				} \
				else \
					RETURN_META((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall);


#define THGM_MAKE_TEST2(id, ret_type, param1, param2) \
	struct TestClass##id; \
	typedef ret_type RetType##id; \
	typedef ParamState2<0, param1, param2 > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
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
			return MakeRet< ret_type >::Do(0); \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual ret_type Call(param1 p1, param2 p2) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2); \
					RETURN_META_VALUE_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber), &TestClass##id::Func, (p1, p2)); \
				} \
				else \
					RETURN_META_VALUE((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber)); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall);
	
#define THGM_MAKE_TEST2_vafmt_void(id, param1, param2) \
	struct TestClass##id; \
	typedef ParamState3<0, param1, param2, std::string > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	\
	struct TestClass##id \
	{ \
		static bool ms_DoRecall; \
		\
		virtual void Func(param1 p1, param2 p2, const char *fmt, ...) \
		{ \
			g_Inside_LeafFunc = true; \
			\
			char buf[9999]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, 9998, fmt, ap); \
			buf[9998] = 0; \
			va_end(ap); \
			\
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, std::string(buf)))); \
			g_Inside_LeafFunc = false; \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual void Call(param1 p1, param2 p2, const char *buf) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, buf))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2); \
					RETURN_META_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2, "%s!", buf)); \
				} \
				else \
					RETURN_META((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall | SourceHook::ProtoInfo::CallConv_HasVafmt);

#define THGM_MAKE_TEST2_vafmt(id, ret_type, param1, param2) \
	struct TestClass##id; \
	typedef ret_type RetType##id; \
	typedef ParamState3<0, param1, param2, std::string > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	\
	struct TestClass##id \
	{ \
		static bool ms_DoRecall; \
		\
		virtual ret_type Func(param1 p1, param2 p2, const char *fmt, ...) \
		{ \
			g_Inside_LeafFunc = true; \
			\
			char buf[9999]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, 9998, fmt, ap); \
			buf[9998] = 0; \
			va_end(ap); \
			\
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, std::string(buf)))); \
			\
			return MakeRet< ret_type >::Do(0); \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual ret_type Call(param1 p1, param2 p2, const char *buf) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, buf))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2); \
					RETURN_META_VALUE_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber), &TestClass##id::Func, (p1, p2, "%s!", buf)); \
				} \
				else \
					RETURN_META_VALUE((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber)); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall | SourceHook::ProtoInfo::CallConv_HasVafmt);

#define THGM_SETUP_PI2(id, p1_type, p1_passtype, p1_flags, p2_type, p2_passtype, p2_flags) \
	void setuppi_##id() \
	{ \
		 \
		protoinfo_##id.AddParam(sizeof(p1_type), p1_passtype, p1_flags, \
			(p1_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::NormalConstructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::CopyConstructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::Destructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::AssignOp) : NULL \
			); \
		 \
		protoinfo_##id.AddParam(sizeof(p2_type), p2_passtype, p2_flags, \
			(p2_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::NormalConstructor) : NULL, \
			(p2_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::CopyConstructor) : NULL, \
			(p2_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::Destructor) : NULL, \
			(p2_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::AssignOp) : NULL \
			); \
		 \
	}
	


#define THGM_MAKE_TEST3_void(id, param1, param2, param3) \
	struct TestClass##id; \
	typedef ParamState3<0, param1, param2, param3 > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
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
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual void Call(param1 p1, param2 p2, param3 p3) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3); \
					RETURN_META_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2, p3)); \
				} \
				else \
					RETURN_META((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall);


#define THGM_MAKE_TEST3(id, ret_type, param1, param2, param3) \
	struct TestClass##id; \
	typedef ret_type RetType##id; \
	typedef ParamState3<0, param1, param2, param3 > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
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
			return MakeRet< ret_type >::Do(0); \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual ret_type Call(param1 p1, param2 p2, param3 p3) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3); \
					RETURN_META_VALUE_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber), &TestClass##id::Func, (p1, p2, p3)); \
				} \
				else \
					RETURN_META_VALUE((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber)); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall);
	
#define THGM_MAKE_TEST3_vafmt_void(id, param1, param2, param3) \
	struct TestClass##id; \
	typedef ParamState4<0, param1, param2, param3, std::string > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	\
	struct TestClass##id \
	{ \
		static bool ms_DoRecall; \
		\
		virtual void Func(param1 p1, param2 p2, param3 p3, const char *fmt, ...) \
		{ \
			g_Inside_LeafFunc = true; \
			\
			char buf[9999]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, 9998, fmt, ap); \
			buf[9998] = 0; \
			va_end(ap); \
			\
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, p3, std::string(buf)))); \
			g_Inside_LeafFunc = false; \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual void Call(param1 p1, param2 p2, param3 p3, const char *buf) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, buf))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3); \
					RETURN_META_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2, p3, "%s!", buf)); \
				} \
				else \
					RETURN_META((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall | SourceHook::ProtoInfo::CallConv_HasVafmt);

#define THGM_MAKE_TEST3_vafmt(id, ret_type, param1, param2, param3) \
	struct TestClass##id; \
	typedef ret_type RetType##id; \
	typedef ParamState4<0, param1, param2, param3, std::string > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	\
	struct TestClass##id \
	{ \
		static bool ms_DoRecall; \
		\
		virtual ret_type Func(param1 p1, param2 p2, param3 p3, const char *fmt, ...) \
		{ \
			g_Inside_LeafFunc = true; \
			\
			char buf[9999]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, 9998, fmt, ap); \
			buf[9998] = 0; \
			va_end(ap); \
			\
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, p3, std::string(buf)))); \
			\
			return MakeRet< ret_type >::Do(0); \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, const char *buf) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, buf))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3); \
					RETURN_META_VALUE_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber), &TestClass##id::Func, (p1, p2, p3, "%s!", buf)); \
				} \
				else \
					RETURN_META_VALUE((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber)); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall | SourceHook::ProtoInfo::CallConv_HasVafmt);

#define THGM_SETUP_PI3(id, p1_type, p1_passtype, p1_flags, p2_type, p2_passtype, p2_flags, p3_type, p3_passtype, p3_flags) \
	void setuppi_##id() \
	{ \
		 \
		protoinfo_##id.AddParam(sizeof(p1_type), p1_passtype, p1_flags, \
			(p1_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::NormalConstructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::CopyConstructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::Destructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::AssignOp) : NULL \
			); \
		 \
		protoinfo_##id.AddParam(sizeof(p2_type), p2_passtype, p2_flags, \
			(p2_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::NormalConstructor) : NULL, \
			(p2_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::CopyConstructor) : NULL, \
			(p2_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::Destructor) : NULL, \
			(p2_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::AssignOp) : NULL \
			); \
		 \
		protoinfo_##id.AddParam(sizeof(p3_type), p3_passtype, p3_flags, \
			(p3_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::NormalConstructor) : NULL, \
			(p3_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::CopyConstructor) : NULL, \
			(p3_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::Destructor) : NULL, \
			(p3_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::AssignOp) : NULL \
			); \
		 \
	}
	


#define THGM_MAKE_TEST4_void(id, param1, param2, param3, param4) \
	struct TestClass##id; \
	typedef ParamState4<0, param1, param2, param3, param4 > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
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
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4); \
					RETURN_META_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2, p3, p4)); \
				} \
				else \
					RETURN_META((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall);


#define THGM_MAKE_TEST4(id, ret_type, param1, param2, param3, param4) \
	struct TestClass##id; \
	typedef ret_type RetType##id; \
	typedef ParamState4<0, param1, param2, param3, param4 > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
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
			return MakeRet< ret_type >::Do(0); \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4); \
					RETURN_META_VALUE_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber), &TestClass##id::Func, (p1, p2, p3, p4)); \
				} \
				else \
					RETURN_META_VALUE((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber)); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall);
	
#define THGM_MAKE_TEST4_vafmt_void(id, param1, param2, param3, param4) \
	struct TestClass##id; \
	typedef ParamState5<0, param1, param2, param3, param4, std::string > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	\
	struct TestClass##id \
	{ \
		static bool ms_DoRecall; \
		\
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, const char *fmt, ...) \
		{ \
			g_Inside_LeafFunc = true; \
			\
			char buf[9999]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, 9998, fmt, ap); \
			buf[9998] = 0; \
			va_end(ap); \
			\
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, p3, p4, std::string(buf)))); \
			g_Inside_LeafFunc = false; \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4, const char *buf) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, buf))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4); \
					RETURN_META_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2, p3, p4, "%s!", buf)); \
				} \
				else \
					RETURN_META((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall | SourceHook::ProtoInfo::CallConv_HasVafmt);

#define THGM_MAKE_TEST4_vafmt(id, ret_type, param1, param2, param3, param4) \
	struct TestClass##id; \
	typedef ret_type RetType##id; \
	typedef ParamState5<0, param1, param2, param3, param4, std::string > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	\
	struct TestClass##id \
	{ \
		static bool ms_DoRecall; \
		\
		virtual ret_type Func(param1 p1, param2 p2, param3 p3, param4 p4, const char *fmt, ...) \
		{ \
			g_Inside_LeafFunc = true; \
			\
			char buf[9999]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, 9998, fmt, ap); \
			buf[9998] = 0; \
			va_end(ap); \
			\
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, p3, p4, std::string(buf)))); \
			\
			return MakeRet< ret_type >::Do(0); \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4, const char *buf) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, buf))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4); \
					RETURN_META_VALUE_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber), &TestClass##id::Func, (p1, p2, p3, p4, "%s!", buf)); \
				} \
				else \
					RETURN_META_VALUE((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber)); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall | SourceHook::ProtoInfo::CallConv_HasVafmt);

#define THGM_SETUP_PI4(id, p1_type, p1_passtype, p1_flags, p2_type, p2_passtype, p2_flags, p3_type, p3_passtype, p3_flags, p4_type, p4_passtype, p4_flags) \
	void setuppi_##id() \
	{ \
		 \
		protoinfo_##id.AddParam(sizeof(p1_type), p1_passtype, p1_flags, \
			(p1_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::NormalConstructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::CopyConstructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::Destructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::AssignOp) : NULL \
			); \
		 \
		protoinfo_##id.AddParam(sizeof(p2_type), p2_passtype, p2_flags, \
			(p2_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::NormalConstructor) : NULL, \
			(p2_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::CopyConstructor) : NULL, \
			(p2_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::Destructor) : NULL, \
			(p2_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::AssignOp) : NULL \
			); \
		 \
		protoinfo_##id.AddParam(sizeof(p3_type), p3_passtype, p3_flags, \
			(p3_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::NormalConstructor) : NULL, \
			(p3_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::CopyConstructor) : NULL, \
			(p3_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::Destructor) : NULL, \
			(p3_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::AssignOp) : NULL \
			); \
		 \
		protoinfo_##id.AddParam(sizeof(p4_type), p4_passtype, p4_flags, \
			(p4_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::NormalConstructor) : NULL, \
			(p4_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::CopyConstructor) : NULL, \
			(p4_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::Destructor) : NULL, \
			(p4_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::AssignOp) : NULL \
			); \
		 \
	}
	


#define THGM_MAKE_TEST5_void(id, param1, param2, param3, param4, param5) \
	struct TestClass##id; \
	typedef ParamState5<0, param1, param2, param3, param4, param5 > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
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
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5); \
					RETURN_META_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2, p3, p4, p5)); \
				} \
				else \
					RETURN_META((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall);


#define THGM_MAKE_TEST5(id, ret_type, param1, param2, param3, param4, param5) \
	struct TestClass##id; \
	typedef ret_type RetType##id; \
	typedef ParamState5<0, param1, param2, param3, param4, param5 > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
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
			return MakeRet< ret_type >::Do(0); \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5); \
					RETURN_META_VALUE_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber), &TestClass##id::Func, (p1, p2, p3, p4, p5)); \
				} \
				else \
					RETURN_META_VALUE((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber)); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall);
	
#define THGM_MAKE_TEST5_vafmt_void(id, param1, param2, param3, param4, param5) \
	struct TestClass##id; \
	typedef ParamState6<0, param1, param2, param3, param4, param5, std::string > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	\
	struct TestClass##id \
	{ \
		static bool ms_DoRecall; \
		\
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, const char *fmt, ...) \
		{ \
			g_Inside_LeafFunc = true; \
			\
			char buf[9999]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, 9998, fmt, ap); \
			buf[9998] = 0; \
			va_end(ap); \
			\
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, p3, p4, p5, std::string(buf)))); \
			g_Inside_LeafFunc = false; \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, const char *buf) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5, buf))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5); \
					RETURN_META_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2, p3, p4, p5, "%s!", buf)); \
				} \
				else \
					RETURN_META((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall | SourceHook::ProtoInfo::CallConv_HasVafmt);

#define THGM_MAKE_TEST5_vafmt(id, ret_type, param1, param2, param3, param4, param5) \
	struct TestClass##id; \
	typedef ret_type RetType##id; \
	typedef ParamState6<0, param1, param2, param3, param4, param5, std::string > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	\
	struct TestClass##id \
	{ \
		static bool ms_DoRecall; \
		\
		virtual ret_type Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, const char *fmt, ...) \
		{ \
			g_Inside_LeafFunc = true; \
			\
			char buf[9999]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, 9998, fmt, ap); \
			buf[9998] = 0; \
			va_end(ap); \
			\
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, p3, p4, p5, std::string(buf)))); \
			\
			return MakeRet< ret_type >::Do(0); \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, const char *buf) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5, buf))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5); \
					RETURN_META_VALUE_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber), &TestClass##id::Func, (p1, p2, p3, p4, p5, "%s!", buf)); \
				} \
				else \
					RETURN_META_VALUE((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber)); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall | SourceHook::ProtoInfo::CallConv_HasVafmt);

#define THGM_SETUP_PI5(id, p1_type, p1_passtype, p1_flags, p2_type, p2_passtype, p2_flags, p3_type, p3_passtype, p3_flags, p4_type, p4_passtype, p4_flags, p5_type, p5_passtype, p5_flags) \
	void setuppi_##id() \
	{ \
		 \
		protoinfo_##id.AddParam(sizeof(p1_type), p1_passtype, p1_flags, \
			(p1_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::NormalConstructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::CopyConstructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::Destructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::AssignOp) : NULL \
			); \
		 \
		protoinfo_##id.AddParam(sizeof(p2_type), p2_passtype, p2_flags, \
			(p2_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::NormalConstructor) : NULL, \
			(p2_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::CopyConstructor) : NULL, \
			(p2_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::Destructor) : NULL, \
			(p2_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::AssignOp) : NULL \
			); \
		 \
		protoinfo_##id.AddParam(sizeof(p3_type), p3_passtype, p3_flags, \
			(p3_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::NormalConstructor) : NULL, \
			(p3_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::CopyConstructor) : NULL, \
			(p3_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::Destructor) : NULL, \
			(p3_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::AssignOp) : NULL \
			); \
		 \
		protoinfo_##id.AddParam(sizeof(p4_type), p4_passtype, p4_flags, \
			(p4_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::NormalConstructor) : NULL, \
			(p4_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::CopyConstructor) : NULL, \
			(p4_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::Destructor) : NULL, \
			(p4_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::AssignOp) : NULL \
			); \
		 \
		protoinfo_##id.AddParam(sizeof(p5_type), p5_passtype, p5_flags, \
			(p5_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p5_type >::type>::NormalConstructor) : NULL, \
			(p5_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p5_type >::type>::CopyConstructor) : NULL, \
			(p5_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p5_type >::type>::Destructor) : NULL, \
			(p5_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p5_type >::type>::AssignOp) : NULL \
			); \
		 \
	}
	


#define THGM_MAKE_TEST6_void(id, param1, param2, param3, param4, param5, param6) \
	struct TestClass##id; \
	typedef ParamState6<0, param1, param2, param3, param4, param5, param6 > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
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
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5, p6))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5);Increment<StripRef< param6 >::type>::Incr(p6); \
					RETURN_META_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2, p3, p4, p5, p6)); \
				} \
				else \
					RETURN_META((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall);


#define THGM_MAKE_TEST6(id, ret_type, param1, param2, param3, param4, param5, param6) \
	struct TestClass##id; \
	typedef ret_type RetType##id; \
	typedef ParamState6<0, param1, param2, param3, param4, param5, param6 > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
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
			return MakeRet< ret_type >::Do(0); \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5, p6))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5);Increment<StripRef< param6 >::type>::Incr(p6); \
					RETURN_META_VALUE_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber), &TestClass##id::Func, (p1, p2, p3, p4, p5, p6)); \
				} \
				else \
					RETURN_META_VALUE((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber)); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall);
	
#define THGM_MAKE_TEST6_vafmt_void(id, param1, param2, param3, param4, param5, param6) \
	struct TestClass##id; \
	typedef ParamState7<0, param1, param2, param3, param4, param5, param6, std::string > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	\
	struct TestClass##id \
	{ \
		static bool ms_DoRecall; \
		\
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, const char *fmt, ...) \
		{ \
			g_Inside_LeafFunc = true; \
			\
			char buf[9999]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, 9998, fmt, ap); \
			buf[9998] = 0; \
			va_end(ap); \
			\
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, p3, p4, p5, p6, std::string(buf)))); \
			g_Inside_LeafFunc = false; \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual void Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, const char *buf) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5, p6, buf))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5);Increment<StripRef< param6 >::type>::Incr(p6); \
					RETURN_META_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, &TestClass##id::Func, (p1, p2, p3, p4, p5, p6, "%s!", buf)); \
				} \
				else \
					RETURN_META((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall | SourceHook::ProtoInfo::CallConv_HasVafmt);

#define THGM_MAKE_TEST6_vafmt(id, ret_type, param1, param2, param3, param4, param5, param6) \
	struct TestClass##id; \
	typedef ret_type RetType##id; \
	typedef ParamState7<0, param1, param2, param3, param4, param5, param6, std::string > ParamState_m##id; \
	MAKE_STATE_2(State_Func##id, TestClass##id* /*thisptr*/, ParamState_m##id ); \
	MAKE_STATE_4(State_Deleg_##id, int /*delegnumber*/, TestClass##id* /*ifptr*/, int /*deleg thisptr*/, ParamState_m##id ); \
	\
	struct TestClass##id \
	{ \
		static bool ms_DoRecall; \
		\
		virtual ret_type Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, const char *fmt, ...) \
		{ \
			g_Inside_LeafFunc = true; \
			\
			char buf[9999]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, 9998, fmt, ap); \
			buf[9998] = 0; \
			va_end(ap); \
			\
			ADD_STATE(State_Func##id(this, ParamState_m##id(p1, p2, p3, p4, p5, p6, std::string(buf)))); \
			\
			return MakeRet< ret_type >::Do(0); \
		} \
		\
		struct Delegate : public MyDelegate \
		{ \
			int m_DelegNumber; \
			Delegate(int num) : m_DelegNumber(num) { } \
			\
			virtual ret_type Call(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, const char *buf) \
			{ \
				g_Inside_LeafFunc = true; \
				ADD_STATE(State_Deleg_##id(m_DelegNumber, META_IFACEPTR(TestClass##id), PtrBuf(this), ParamState_m##id(p1, p2, p3, p4, p5, p6, buf))); \
				g_Inside_LeafFunc = false; \
				if (ms_DoRecall) \
				{ \
					Increment<StripRef< param1 >::type>::Incr(p1);Increment<StripRef< param2 >::type>::Incr(p2);Increment<StripRef< param3 >::type>::Incr(p3);Increment<StripRef< param4 >::type>::Incr(p4);Increment<StripRef< param5 >::type>::Incr(p5);Increment<StripRef< param6 >::type>::Incr(p6); \
					RETURN_META_VALUE_NEWPARAMS((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber), &TestClass##id::Func, (p1, p2, p3, p4, p5, p6, "%s!", buf)); \
				} \
				else \
					RETURN_META_VALUE((m_DelegNumber & 1) ? MRES_IGNORED : MRES_SUPERCEDE, MakeRet< ret_type >::Do(m_DelegNumber)); \
			} \
		}; \
	}; \
	\
	bool TestClass##id::ms_DoRecall = false; \
	SourceHook::CProtoInfoBuilder protoinfo_##id(SourceHook::ProtoInfo::CallConv_ThisCall | SourceHook::ProtoInfo::CallConv_HasVafmt);

#define THGM_SETUP_PI6(id, p1_type, p1_passtype, p1_flags, p2_type, p2_passtype, p2_flags, p3_type, p3_passtype, p3_flags, p4_type, p4_passtype, p4_flags, p5_type, p5_passtype, p5_flags, p6_type, p6_passtype, p6_flags) \
	void setuppi_##id() \
	{ \
		 \
		protoinfo_##id.AddParam(sizeof(p1_type), p1_passtype, p1_flags, \
			(p1_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::NormalConstructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::CopyConstructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::Destructor) : NULL, \
			(p1_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p1_type >::type>::AssignOp) : NULL \
			); \
		 \
		protoinfo_##id.AddParam(sizeof(p2_type), p2_passtype, p2_flags, \
			(p2_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::NormalConstructor) : NULL, \
			(p2_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::CopyConstructor) : NULL, \
			(p2_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::Destructor) : NULL, \
			(p2_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p2_type >::type>::AssignOp) : NULL \
			); \
		 \
		protoinfo_##id.AddParam(sizeof(p3_type), p3_passtype, p3_flags, \
			(p3_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::NormalConstructor) : NULL, \
			(p3_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::CopyConstructor) : NULL, \
			(p3_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::Destructor) : NULL, \
			(p3_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p3_type >::type>::AssignOp) : NULL \
			); \
		 \
		protoinfo_##id.AddParam(sizeof(p4_type), p4_passtype, p4_flags, \
			(p4_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::NormalConstructor) : NULL, \
			(p4_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::CopyConstructor) : NULL, \
			(p4_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::Destructor) : NULL, \
			(p4_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p4_type >::type>::AssignOp) : NULL \
			); \
		 \
		protoinfo_##id.AddParam(sizeof(p5_type), p5_passtype, p5_flags, \
			(p5_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p5_type >::type>::NormalConstructor) : NULL, \
			(p5_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p5_type >::type>::CopyConstructor) : NULL, \
			(p5_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p5_type >::type>::Destructor) : NULL, \
			(p5_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p5_type >::type>::AssignOp) : NULL \
			); \
		 \
		protoinfo_##id.AddParam(sizeof(p6_type), p6_passtype, p6_flags, \
			(p6_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p6_type >::type>::NormalConstructor) : NULL, \
			(p6_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p6_type >::type>::CopyConstructor) : NULL, \
			(p6_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< p6_type >::type>::Destructor) : NULL, \
			(p6_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< p6_type >::type>::AssignOp) : NULL \
			); \
		 \
	}
	


#define THGM_SETUP_RI(id, ret_type, ret_passtype, ret_flags) \
	void setupri_##id() \
	{ \
		protoinfo_##id.SetReturnType(sizeof(ret_type), ret_passtype, ret_flags, \
			(ret_flags & SourceHook::PassInfo::PassFlag_OCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< ret_type >::type>::NormalConstructor) : NULL, \
			(ret_flags & SourceHook::PassInfo::PassFlag_CCtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< ret_type >::type>::CopyConstructor) : NULL, \
			(ret_flags & SourceHook::PassInfo::PassFlag_ODtor) ? FindFuncAddr(&Ctor_Thunk<StripRef< ret_type >::type>::Destructor) : NULL, \
			(ret_flags & SourceHook::PassInfo::PassFlag_AssignOp) ? FindFuncAddr(&Ctor_Thunk<StripRef< ret_type >::type>::AssignOp) : NULL \
			); \
	}
	
#define THGM_ADD_HOOK(id, num) \
	CAT4(hook, num, _, id) = g_SHPtr->AddHook(g_PLID, SourceHook::ISourceHook::Hook_Normal, reinterpret_cast<void*>(pTest##id), \
		0, myhookman##id, PtrBufPtr(new TestClass##id::Delegate(num)), num >= 3);

#define THGM_REMOVE_HOOK(id, num) \
	g_SHPtr->RemoveHookByID(CAT4(hook, num, _, id));

#define THGM_CALLS_void(id, call_params) \
	pTest##id->Func call_params; \
	SH_CALL(pTest##id, &TestClass##id::Func) call_params;

#define THGM_DO_TEST_void(id, call_params) \
	setuppi_##id(); \
	SourceHook::HookManagerPubFunc myhookman##id = g_HMAGPtr->MakeHookMan(protoinfo_##id, 0, 0); \
	CAutoReleaseHookMan arhm_##id(myhookman##id); \
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
			/* hook1 - hook3 */ \
	PtrBuf_Clear(); \
	THGM_ADD_HOOK(id, 1); \
	THGM_ADD_HOOK(id, 3); \
	THGM_CALLS_void(id, call_params); \
	CHECK_STATES((&g_States, \
		new State_Deleg_##id(1, pTest##id, 0, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		new State_Deleg_##id(3, pTest##id, 1, ParamState_m##id call_params), \
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
		new State_Deleg_##id(1, pTest##id, 0, ParamState_m##id call_params), \
		new State_Deleg_##id(2, pTest##id, 1, ParamState_m##id call_params), \
		new State_Deleg_##id(3, pTest##id, 2, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part5"); \
			/* hook1, hook2 - hook3, hook4 */ \
	THGM_ADD_HOOK(id, 4); \
	THGM_CALLS_void(id, call_params); \
	CHECK_STATES((&g_States, \
		new State_Deleg_##id(1, pTest##id, 0, ParamState_m##id call_params), \
		new State_Deleg_##id(2, pTest##id, 1, ParamState_m##id call_params), \
		new State_Deleg_##id(3, pTest##id, 2, ParamState_m##id call_params), \
		new State_Deleg_##id(4, pTest##id, 3, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part6"); \
		\
			/* hook1 - hook3, hook4, with recalls! */ \
		\
	TestClass##id::ms_DoRecall = true; \
	THGM_REMOVE_HOOK(id, 2); \
	THGM_CALLS_void(id, call_params); \
	CHECK_STATES((&g_States, \
		new State_Deleg_##id(1, pTest##id, 0, ParamState_m##id call_params(0)), \
		new State_Func##id(pTest##id, ParamState_m##id call_params(1)), \
		new State_Deleg_##id(3, pTest##id, 2, ParamState_m##id call_params(1)), \
		new State_Deleg_##id(4, pTest##id, 3, ParamState_m##id call_params(2)), \
		/* sh_call one */ \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part7"); \
	THGM_REMOVE_HOOK(id, 1); \
	THGM_REMOVE_HOOK(id, 3); \
	THGM_REMOVE_HOOK(id, 4);

template<class T>
T ComparableRef(T x)
{
	return x;
}

template <class T>
T* ComparableRef(T& x)
{
	return &x;
}

#define THGM_CALLS(id, call_params, exp_ret_norm, exp_ret_shcall, err) \
	CHECK_COND(ComparableRef<RetType##id>(pTest##id->Func call_params) == ComparableRef<RetType##id>(MakeRet< RetType##id >::Do(exp_ret_norm)), err " /retcallnorm"); \
	CHECK_COND(ComparableRef<RetType##id>(SH_CALL(pTest##id, &TestClass##id::Func) call_params) == ComparableRef<RetType##id>(MakeRet< RetType##id >::Do(exp_ret_shcall)), err " /retcallshcall");

#define THGM_DO_TEST(id, call_params) \
	setuppi_##id(); \
	setupri_##id(); \
	SourceHook::HookManagerPubFunc myhookman##id = g_HMAGPtr->MakeHookMan(protoinfo_##id, 0, 0); \
	CAutoReleaseHookMan arhm_##id(myhookman##id); \
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
			/* hook1 - hook3 */ \
	PtrBuf_Clear(); \
	THGM_ADD_HOOK(id, 1); \
	THGM_ADD_HOOK(id, 3); \
	THGM_CALLS(id, call_params, 0, 0, "Part4"); \
	CHECK_STATES((&g_States, \
		new State_Deleg_##id(1, pTest##id, 0, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		new State_Deleg_##id(3, pTest##id, 1, ParamState_m##id call_params), \
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
		new State_Deleg_##id(1, pTest##id, 0, ParamState_m##id call_params), \
		new State_Deleg_##id(2, pTest##id, 1, ParamState_m##id call_params), \
		new State_Deleg_##id(3, pTest##id, 2, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part5"); \
			/* hook1, hook2 - hook3, hook4 */ \
	THGM_ADD_HOOK(id, 4); \
	THGM_CALLS(id, call_params, 4, 0, "Part6"); \
	CHECK_STATES((&g_States, \
		new State_Deleg_##id(1, pTest##id, 0, ParamState_m##id call_params), \
		new State_Deleg_##id(2, pTest##id, 1, ParamState_m##id call_params), \
		new State_Deleg_##id(3, pTest##id, 2, ParamState_m##id call_params), \
		new State_Deleg_##id(4, pTest##id, 3, ParamState_m##id call_params), \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part6"); \
		\
			/* hook1 - hook3, hook4, with recalls! */ \
		\
	TestClass##id::ms_DoRecall = true; \
	THGM_REMOVE_HOOK(id, 2); \
	THGM_CALLS(id, call_params, 4, 0, "Part7"); \
	CHECK_STATES((&g_States, \
		new State_Deleg_##id(1, pTest##id, 0, ParamState_m##id call_params(0)), \
		new State_Func##id(pTest##id, ParamState_m##id call_params(1)), \
		new State_Deleg_##id(3, pTest##id, 2, ParamState_m##id call_params(1)), \
		new State_Deleg_##id(4, pTest##id, 3, ParamState_m##id call_params(2)), \
		/* sh_call one */ \
		new State_Func##id(pTest##id, ParamState_m##id call_params), \
		NULL), "Test" #id " Part7"); \
	THGM_REMOVE_HOOK(id, 1); \
	THGM_REMOVE_HOOK(id, 3); \
	THGM_REMOVE_HOOK(id, 4);
