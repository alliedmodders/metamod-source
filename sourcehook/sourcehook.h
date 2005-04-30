/* ======== SourceHook ========
* By PM
* No warranties of any kind
* ============================
*/

/**
*	@file sourcehook.h
*	@brief Contains the public SourceHook API
*/

#ifndef __SOURCEHOOK_H__
#define __SOURCEHOOK_H__

#ifndef SH_GLOB_SHPTR
#define SH_GLOB_SHPTR g_SHPtr
#endif

#ifndef SH_GLOB_PLUGPTR
#define SH_GLOB_PLUGPTR g_PLID
#endif

#define SH_ASSERT(x, info) if (!(x)) __asm { int 3 }

// System
#define SH_SYS_WIN32 1
#define SH_SYS_LINUX 2

#ifdef _WIN32
# define SH_SYS SH_SYS_WIN32
#elif defined __linux__
# define SH_SYS SH_SYS_LINUX
#else
# error Unsupported system
#endif

// Compiler
#define SH_COMP_GCC 1
#define SH_COMP_MSVC 2

#ifdef _MSC_VER
# define SH_COMP SH_COMP_MSVC
#elif defined __GNUC__
# define SH_COMP SH_COMP_GCC
#else
# error Unsupported compiler
#endif

#if SH_COMP==SH_COMP_MSVC
# define vsnprintf _vsnprintf
#endif

#include "FastDelegate.h"
#include "sh_memfuncinfo.h"
#include "sh_memory.h"
#include <list>
#include <vector>
#include <map>
#include <algorithm>

// Good old metamod!

// Flags returned by a plugin's api function.
// NOTE: order is crucial, as greater/less comparisons are made.
enum META_RES
{
	MRES_IGNORED=0,		// plugin didn't take any action
	MRES_HANDLED,		// plugin did something, but real function should still be called
	MRES_OVERRIDE,		// call real function, but use my return value
	MRES_SUPERCEDE,		// skip real function; use my return value
};


namespace SourceHook
{
	const int STRBUF_LEN=8192;		// In bytes, for "vafmt" functions

	/**
	*	@brief An empty class. No inheritance used. Used for original-function-call hacks
	*/
	class EmptyClass
	{
	};

	/**
	*	@brief A plugin typedef
	*
	*	SourceHook doesn't really care what this is. As long as the ==, != and = operators work on it
	*	and every plugin has a unique identifier, everything is ok.
	*/
	typedef int Plugin;

	enum HookManagerAction
	{
		HA_GetInfo = 0,			// -> Only store info
		HA_Register,			// -> Save the pointer for future reference
		HA_Unregister			// -> Clear the saved pointer
	};

	struct HookManagerInfo;

	/**
	*	@brief Pointer to hook manager type
	*
	*	A "hook manager" is a the only thing that knows the actual protoype of the function at compile time.
	*	
	*	@param hi A pointer to a HookManagerInfo structure. The hook manager should fill it and store it for
	*				future reference (mainly if something should get hooked to its hookfunc)
	*/
	typedef int (*HookManagerPubFunc)(HookManagerAction ha, HookManagerInfo *hi);

	class ISHDelegate
	{
	public:
		virtual void DeleteThis() = 0;				// Ugly, I know
		virtual bool IsEqual(ISHDelegate *other) = 0;
	};

	template <class T> class CSHDelegate : public ISHDelegate
	{
		T m_Deleg;
	public:
		CSHDelegate(T deleg) : m_Deleg(deleg)
		{
		}

		void DeleteThis()
		{
			delete this;
		}

		bool IsEqual(ISHDelegate *other)
		{
			return static_cast<CSHDelegate<T>* >(other)->GetDeleg() == GetDeleg();				
		}

		T &GetDeleg()
		{
			return m_Deleg;
		}
	};

	/**
	*	@brief This structure contains information about a hook manager (hook manager)
	*/
	struct HookManagerInfo
	{
		struct VfnPtr
		{
			struct Iface
			{
				struct Hook
				{
					ISHDelegate *handler;			//!< Pointer to the handler
					bool paused;					//!< If true, the hook should not be executed
					Plugin plug;					//!< The owner plugin
				};
				void *ptr;							//!< Pointer to the interface instance
				std::list<Hook> hooks_pre;			//!< A list of pre-hooks
				std::list<Hook> hooks_post;			//!< A list of post-hooks
				bool operator ==(void *other) const
				{
					return ptr == other;
				}
			};

			void *vfnptr;							//!< Pointer to the function
			void *orig_entry;					//!< The original vtable entry

			typedef std::list<Iface> IfaceList;
			typedef IfaceList::iterator IfaceListIter;
			IfaceList ifaces;

			bool operator ==(void *other)
			{
				return vfnptr == other;
			}
		};
		Plugin plug;							//!< The owner plugin
		const char *proto;						//!< The prototype of the function the hook manager is responsible for
		int vtbl_idx;							//!< The vtable index
		int vtbl_offs;							//!< The vtable offset
		HookManagerPubFunc func;				//!< The interface to the hook manager

		void *hookfunc_vfnptr;					//!< Pointer to the hookfunc impl
		
		typedef std::list<VfnPtr> VfnPtrList;
		typedef VfnPtrList::iterator VfnPtrListIter;
		VfnPtrList vfnptrs;				//!< List of hooked interfaces
	};

	typedef std::vector<void*> OrigFuncs;
	typedef std::map<int, OrigFuncs> OrigVTables;

	template<class B> struct CallClass
	{
		B *ptr;
		OrigVTables vt;
	};

	typedef CallClass<void> GenericCallClass;

	/**
	*	@brief The main SourceHook interface
	*/
	class ISourceHook
	{
	public:
		/**
		*	@brief Add a hook.
		*
		*	@return True if the function succeeded, false otherwise
		*
		*	@param plug The unique identifier of the plugin that calls this function
		*	@param iface The interface pointer
		*	@param ifacesize The size of the class iface points to
		*	@param myHookMan A hook manager function that should be capable of handling the function
		*	@param handler A pointer to a FastDelegate containing the hook handler
		*	@param post Set to true if you want a post handler
		*/
		virtual bool AddHook(Plugin plug, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan, ISHDelegate *handler, bool post) = 0;

		/**
		*	@brief Removes a hook.
		*
		*	@return True if the function succeeded, false otherwise
		*
		*	@param plug The unique identifier of the plugin that calls this function
		*	@param iface The interface pointer
		*	@param myHookMan A hook manager function that should be capable of handling the function
		*	@param handler A pointer to a FastDelegate containing the hook handler
		*	@param post Set to true if you want a post handler
		*/
		virtual bool RemoveHook(Plugin plug, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan, ISHDelegate *handler, bool post) = 0;

		/**
		*	@brief Checks whether a plugin has (a) hook manager(s) that is/are currently used by other plugins
		*
		*	@param plug The unique identifier of the plugin in question
		*/
		virtual bool IsPluginInUse(Plugin plug) = 0;

		/**
		*	@brief Return a pointer to a callclass. Generate a new one if required.
		*
		*	@param iface The interface pointer
		*	@param size Size of the class
		*/
		virtual GenericCallClass *GetCallClass(void *iface) = 0;

		/**
		*	@brief Release a callclass
		*
		*	@param ptr Pointer to the callclass
		*/
		virtual void ReleaseCallClass(GenericCallClass *ptr) = 0;

		virtual void SetRes(META_RES res) = 0;				//!< Sets the meta result
		virtual META_RES GetPrevRes() = 0;					//!< Gets the meta result of the previously called handler
		virtual META_RES GetStatus() = 0;					//!< Gets the highest meta result
		virtual const void *GetOrigRet() = 0;				//!< Gets the original result. If not in post function, undefined
		virtual const void *GetOverrideRet() = 0;			//!< Gets the override result. If none is specified, NULL
		virtual void *GetIfacePtr() = 0;					//!< Gets the interface pointer
		//////////////////////////////////////////////////////////////////////////
		// For hook managers
		virtual META_RES &GetCurResRef() = 0;				//!< Gets the pointer to the current meta result
		virtual META_RES &GetPrevResRef() = 0;				//!< Gets the pointer to the previous meta result
		virtual META_RES &GetStatusRef() = 0;				//!< Gets the pointer to the status variable
		virtual void SetOrigRet(const void *ptr) = 0;		//!< Sets the original return pointer
		virtual void SetOverrideRet(const void *ptr) = 0;	//!< Sets the override result pointer
		virtual void SetIfacePtr(void *ptr) = 0;			//!< Sets the interface this pointer
	};
}

//////////////////////////////////////////////////////////////////////////
// Macro interface
#define SET_META_RESULT(result)		SH_GLOB_SHPTR->SetRes(result)
#define RETURN_META(result)			do { SH_GLOB_SHPTR->SetRes(result); return; } while(0)
#define RETURN_META_VALUE(result, value)	do { SH_GLOB_SHPTR->SetRes(result); return (value); } while(0)

#define META_RESULT_STATUS					SH_GLOB_SHPTR->GetStatus()
#define META_RESULT_PREVIOUS				SH_GLOB_SHPTR->GetPrevRes()
#define META_RESULT_ORIG_RET(type)			*(const type *)SH_GLOB_SHPTR->GetOrigRet()
#define META_RESULT_OVERRIDE_RET(type)		*(const type *)SH_GLOB_SHPTR->GetOverrideRet()
#define META_IFACEPTR						SH_GLOB_SHPTR->GetIfacePtr()


/**
*	@brief Get/generate callclass for an interface pointer
*
*	@param ifaceptr The interface pointer
*/
template<class ifacetype>
inline SourceHook::CallClass<ifacetype> *SH_GET_CALLCLASS(ifacetype *ptr)
{
	return reinterpret_cast<SourceHook::CallClass<ifacetype>*>(
		SH_GLOB_SHPTR->GetCallClass(reinterpret_cast<void*>(ptr)));
}

#define SH_RELEASE_CALLCLASS(ptr) SH_GLOB_SHPTR->ReleaseCallClass(reinterpret_cast<SourceHook::GenericCallClass*>(ptr))

#define SH_ADD_HOOK(ifacetype, ifacefunc, ifaceptr, handler, post) \
	__SourceHook_FHAdd##ifacetype##ifacefunc((void*)ifaceptr, post, handler)
#define SH_ADD_HOOK_STATICFUNC(ifacetype, ifacefunc, ifaceptr, handler, post) \
	SH_ADD_HOOK(ifacetype, ifacefunc, ifaceptr, fastdelegate::MakeDelegate(handler), post)
#define SH_ADD_HOOK_MEMFUNC(ifacetype, ifacefunc, ifaceptr, handler_inst, handler_func, post) \
	SH_ADD_HOOK(ifacetype, ifacefunc, ifaceptr, fastdelegate::MakeDelegate(handler_inst, handler_func), post)

#define SH_REMOVE_HOOK(ifacetype, ifacefunc, ifaceptr, handler, post) \
	__SourceHook_FHRemove##ifacetype##ifacefunc((void*)ifaceptr, post, handler)
#define SH_REMOVE_HOOK_STATICFUNC(ifacetype, ifacefunc, ifaceptr, handler, post) \
	SH_REMOVE_HOOK(ifacetype, ifacefunc, ifaceptr, fastdelegate::MakeDelegate(handler), post)
#define SH_REMOVE_HOOK_MEMFUNC(ifacetype, ifacefunc, ifaceptr, handler_inst, handler_func, post) \
	SH_REMOVE_HOOK(ifacetype, ifacefunc, ifaceptr, fastdelegate::MakeDelegate(handler_inst, handler_func), post)

#define SH_NOATTRIB



#if SH_COMP == SH_COMP_MSVC
# define SH_SETUP_MFP(mfp) \
	reinterpret_cast<void**>(&mfp)[0] = vfnptr.orig_entry;
#elif SH_COMP == SH_COMP_GCC
# define SH_SETUP_MFP(mfp) \
	reinterpret_cast<void**>(&mfp)[0] = vfnptr.orig_entry; \
	reinterpret_cast<void**>(&mfp)[1] = 0;
#else
# error Not supported yet.
#endif

//////////////////////////////////////////////////////////////////////////
#define SH_FHCls(ift, iff, ov) __SourceHook_FHCls_##ift##iff##ov

#define SHINT_MAKE_HOOKMANPUBFUNC(ifacetype, ifacefunc, overload, funcptr) \
	static int HookManPubFunc(::SourceHook::HookManagerAction action, ::SourceHook::HookManagerInfo *param) \
	{ \
		using namespace ::SourceHook; \
		if (action == ::SourceHook::HA_GetInfo) \
		{ \
			param->proto = ms_Proto; \
			MemFuncInfo mfi; \
			GetFuncInfo(funcptr, mfi); \
			param->vtbl_idx = mfi.vtblindex; \
			param->vtbl_offs = mfi.vtbloffs; \
			\
			GetFuncInfo(&SH_FHCls(ifacetype,ifacefunc,overload)::Func, mfi); \
			param->hookfunc_vfnptr = \
			reinterpret_cast<void**>(reinterpret_cast<char*>(&ms_Inst) + mfi.vtbloffs)[mfi.vtblindex]; \
			return 0; \
		} \
		else if (action == ::SourceHook::HA_Register) \
		{ \
			ms_HI = param; \
			return 0; \
		} \
		else if (action == ::SourceHook::HA_Unregister) \
		{ \
			ms_HI = NULL; \
			return 0; \
		} \
		else \
			return 1; \
	}

// It has to be possible to use the macros in namespaces
// -> So we need to access and extend the global SourceHook namespace
// We use a namespace alias for this
#define SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, funcptr) \
	struct SH_FHCls(ifacetype,ifacefunc,overload) \
	{ \
		static SH_FHCls(ifacetype,ifacefunc,overload) ms_Inst; \
		static ::SourceHook::HookManagerInfo *ms_HI; \
		static const char *ms_Proto; \
		SHINT_MAKE_HOOKMANPUBFUNC(ifacetype, ifacefunc, overload, funcptr)

#define SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, proto, funcptr) \
	}; \
	const char *SH_FHCls(ifacetype,ifacefunc,overload)::ms_Proto = proto; \
	SH_FHCls(ifacetype,ifacefunc,overload) SH_FHCls(ifacetype,ifacefunc,overload)::ms_Inst; \
	::SourceHook::HookManagerInfo *SH_FHCls(ifacetype,ifacefunc,overload)::ms_HI; \
	bool __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, bool post, \
		SH_FHCls(ifacetype,ifacefunc,overload)::FD handler) \
	{ \
		using namespace ::SourceHook; \
		MemFuncInfo mfi; \
		GetFuncInfo(funcptr, mfi); \
		if (mfi.thisptroffs < 0) \
			return false; /* No virtual inheritance supported */ \
		\
		return SH_GLOB_SHPTR->AddHook(SH_GLOB_PLUGPTR, iface, mfi.thisptroffs, \
			SH_FHCls(ifacetype,ifacefunc,overload)::HookManPubFunc, \
			new CSHDelegate<SH_FHCls(ifacetype,ifacefunc,overload)::FD>(handler), post); \
	} \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		SH_FHCls(ifacetype,ifacefunc,overload)::FD handler) \
	{ \
		using namespace ::SourceHook; \
		MemFuncInfo mfi; \
		GetFuncInfo(funcptr, mfi); \
		if (mfi.thisptroffs < 0) \
			return false; /* No virtual inheritance supported */ \
		\
		CSHDelegate<SH_FHCls(ifacetype,ifacefunc,overload)::FD> tmp(handler); \
		return SH_GLOB_SHPTR->RemoveHook(SH_GLOB_PLUGPTR, iface, mfi.thisptroffs, \
			SH_FHCls(ifacetype,ifacefunc,overload)::HookManPubFunc, &tmp, post); \
	} \

#define SH_SETUPCALLS(rettype, paramtypes, params) \
	/* 1) Find the vfnptr */ \
	using namespace ::SourceHook; \
	void *ourvfnptr = reinterpret_cast<void*>( \
		*reinterpret_cast<void***>(reinterpret_cast<char*>(this) + ms_HI->vtbl_offs) + ms_HI->vtbl_idx); \
	\
	HookManagerInfo::VfnPtrListIter vfptriter = std::find(ms_HI->vfnptrs.begin(), \
		ms_HI->vfnptrs.end(), ourvfnptr); \
	if (vfptriter == ms_HI->vfnptrs.end()) \
	{ \
		/* Bleh? Should be impossible! */ \
		SH_ASSERT(0, "Called with vfnptr 0x%p which couldn't be found in the list"); \
	} \
	HookManagerInfo::VfnPtr &vfnptr = *vfptriter; \
	/* 2) Find the iface */ \
	HookManagerInfo::VfnPtr::IfaceListIter ifiter = std::find(vfnptr.ifaces.begin(), vfnptr.ifaces.end(), this); \
	if (ifiter == vfnptr.ifaces.end()) \
	{ \
		/* The iface info was not found. Redirect the call to the original function. */ \
		rettype (EmptyClass::*mfp)paramtypes; \
		SH_SETUP_MFP(mfp); \
		return (reinterpret_cast<EmptyClass*>(this)->*mfp)params; \
	} \
	HookManagerInfo::VfnPtr::Iface &ci = *ifiter; \
	/* 2) Declare some vars and set it up */ \
	std::list<HookManagerInfo::VfnPtr::Iface::Hook> &prelist = ci.hooks_pre; \
	std::list<HookManagerInfo::VfnPtr::Iface::Hook> &postlist = ci.hooks_post; \
	rettype orig_ret; \
	rettype override_ret; \
	rettype plugin_ret; \
	META_RES &cur_res = SH_GLOB_SHPTR->GetCurResRef(); \
	META_RES &prev_res = SH_GLOB_SHPTR->GetPrevResRef(); \
	META_RES &status = SH_GLOB_SHPTR->GetStatusRef(); \
	status = MRES_IGNORED; \
	SH_GLOB_SHPTR->SetIfacePtr(this); \
	SH_GLOB_SHPTR->SetOrigRet(reinterpret_cast<void*>(&orig_ret)); \
	SH_GLOB_SHPTR->SetOverrideRet(NULL);

#define SH_CALL_HOOKS(post, params) \
	prev_res = MRES_IGNORED; \
	for (std::list<HookManagerInfo::VfnPtr::Iface::Hook>::iterator hiter = post##list.begin(); hiter != post##list.end(); ++hiter) \
	{ \
		if (hiter->paused) continue; \
		cur_res = MRES_IGNORED; \
		plugin_ret = reinterpret_cast<CSHDelegate<FD>*>(hiter->handler)->GetDeleg() params; \
		prev_res = cur_res; \
		if (cur_res > status) \
			status = cur_res; \
		if (cur_res >= MRES_OVERRIDE) \
		{ \
			override_ret = plugin_ret; \
			SH_GLOB_SHPTR->SetOverrideRet(&override_ret); \
		} \
	}

#define SH_CALL_ORIG(ifacetype, ifacefunc, rettype, paramtypes, params) \
	if (status != MRES_SUPERCEDE) \
	{ \
		rettype (EmptyClass::*mfp)paramtypes; \
		SH_SETUP_MFP(mfp); \
		orig_ret = (reinterpret_cast<EmptyClass*>(ci.ptr)->*mfp)params; \
	} \
	else \
		orig_ret = override_ret;

#define SH_RETURN() \
	return status >= MRES_OVERRIDE ? override_ret : orig_ret;

#define SH_HANDLEFUNC(ifacetype, ifacefunc, paramtypes, params, rettype) \
	SH_SETUPCALLS(rettype, paramtypes, params) \
	SH_CALL_HOOKS(pre, params) \
	SH_CALL_ORIG(ifacetype, ifacefunc, rettype, paramtypes, params) \
	SH_CALL_HOOKS(post, params) \
	SH_RETURN()

//////////////////////////////////////////////////////////////////////////
#define SH_SETUPCALLS_void(paramtypes, params) \
	using namespace ::SourceHook; \
	/* 1) Find the vfnptr */ \
	void *ourvfnptr = reinterpret_cast<void*>( \
		*reinterpret_cast<void***>(reinterpret_cast<char*>(this) + ms_HI->vtbl_offs) + ms_HI->vtbl_idx); \
	\
	HookManagerInfo::VfnPtrListIter vfptriter = std::find(ms_HI->vfnptrs.begin(), \
		ms_HI->vfnptrs.end(), ourvfnptr); \
	if (vfptriter == ms_HI->vfnptrs.end()) \
	{ \
		/* Bleh? Should be impossible! */ \
		SH_ASSERT(0, "Called with vfnptr 0x%p which couldn't be found in the list"); \
	} \
	HookManagerInfo::VfnPtr &vfnptr = *vfptriter; \
	/* 2) Find the iface */ \
	HookManagerInfo::VfnPtr::IfaceListIter ifiter = std::find(vfnptr.ifaces.begin(), vfnptr.ifaces.end(), this); \
	if (ifiter == vfnptr.ifaces.end()) \
	{ \
		/* The iface info was not found. Redirect the call to the original function. */ \
		void (EmptyClass::*mfp)paramtypes; \
		SH_SETUP_MFP(mfp); \
		(reinterpret_cast<EmptyClass*>(this)->*mfp)params; \
		return; \
	} \
	HookManagerInfo::VfnPtr::Iface &ci = *ifiter; \
	/* 2) Declare some vars and set it up */ \
	std::list<HookManagerInfo::VfnPtr::Iface::Hook> &prelist = ci.hooks_pre; \
	std::list<HookManagerInfo::VfnPtr::Iface::Hook> &postlist = ci.hooks_post; \
	META_RES &cur_res = SH_GLOB_SHPTR->GetCurResRef(); \
	META_RES &prev_res = SH_GLOB_SHPTR->GetPrevResRef(); \
	META_RES &status = SH_GLOB_SHPTR->GetStatusRef(); \
	status = MRES_IGNORED; \
	SH_GLOB_SHPTR->SetIfacePtr(this); \
	SH_GLOB_SHPTR->SetOverrideRet(NULL);

#define SH_CALL_HOOKS_void(post, params) \
	prev_res = MRES_IGNORED; \
	for (std::list<HookManagerInfo::VfnPtr::Iface::Hook>::iterator hiter = post##list.begin(); hiter != post##list.end(); ++hiter) \
	{ \
		if (hiter->paused) continue; \
		cur_res = MRES_IGNORED; \
		reinterpret_cast<CSHDelegate<FD>*>(hiter->handler)->GetDeleg() params; \
		prev_res = cur_res; \
		if (cur_res > status) \
			status = cur_res; \
	}

#define SH_CALL_ORIG_void(ifacetype, ifacefunc, paramtypes, params) \
	if (status != MRES_SUPERCEDE) \
	{ \
		void (EmptyClass::*mfp)paramtypes; \
		SH_SETUP_MFP(mfp); \
		(reinterpret_cast<EmptyClass*>(ci.ptr)->*mfp)params; \
	}

#define SH_RETURN_void()

#define SH_HANDLEFUNC_void(ifacetype, ifacefunc, paramtypes, params) \
	SH_SETUPCALLS_void(paramtypes, params) \
	SH_CALL_HOOKS_void(pre, params) \
	SH_CALL_ORIG_void(ifacetype, ifacefunc, paramtypes, params) \
	SH_CALL_HOOKS_void(post, params) \
	SH_RETURN_void()


// Special vafmt handlers
#define SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, paramtypes, params_orig, params_plug, rettype) \
	SH_SETUPCALLS(rettype, paramtypes, params_orig) \
	SH_CALL_HOOKS(pre, params_plug) \
	SH_CALL_ORIG(ifacetype, ifacefunc, rettype, paramtypes, params_orig) \
	SH_CALL_HOOKS(post, params_plug) \
	SH_RETURN()

#define SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, paramtypes, params_orig, params_plug) \
	SH_SETUPCALLS_void(paramtypes, params_orig) \
	SH_CALL_HOOKS_void(pre, params_plug) \
	SH_CALL_ORIG_void(ifacetype, ifacefunc, paramtypes, params_orig) \
	SH_CALL_HOOKS_void(post, params_plug) \
	SH_RETURN_void()

//////////////////////////////////////////////////////////////////////////

// ********* Support for 0 arguments *********
#define SH_DECL_HOOK0(ifacetype, ifacefunc, attr, overload, rettype) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)() attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate0<rettype> FD; \
		virtual rettype Func() \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (), (), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype , \
	(static_cast<rettype (ifacetype::*)() attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK0_void(ifacetype, ifacefunc, attr, overload) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)() attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate0<> FD; \
		virtual void Func() \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (), ()); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr , \
	(static_cast<void (ifacetype::*)() attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK0_vafmt(ifacetype, ifacefunc, attr, overload, rettype) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate1<const char *, rettype> FD; \
		virtual rettype Func(const char *fmt, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (...), ("%s", buf), (buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype  "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK0_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate1<const char *> FD; \
		virtual void Func(const char *, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (...), ("%s", buf), (buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr  "|const char*|...", \
	(static_cast<void (ifacetype::*)(const char *, ...) attr>(&ifacetype::ifacefunc)))

// ********* Support for 1 arguments *********
#define SH_DECL_HOOK1(ifacetype, ifacefunc, attr, overload, rettype, param1) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate1<param1, rettype> FD; \
		virtual rettype Func(param1 p1) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (param1), (p1), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1, \
	(static_cast<rettype (ifacetype::*)(param1) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK1_void(ifacetype, ifacefunc, attr, overload, param1) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate1<param1> FD; \
		virtual void Func(param1 p1) \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (param1), (p1)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1, \
	(static_cast<void (ifacetype::*)(param1) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK1_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate2<param1, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, const char *fmt, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (param1, ...), (p1, "%s", buf), (p1, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(param1, const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK1_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate2<param1, const char *> FD; \
		virtual void Func(param1 p1, const char *, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (param1, ...), (p1, "%s", buf), (p1, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|const char*|...", \
	(static_cast<void (ifacetype::*)(param1, const char *, ...) attr>(&ifacetype::ifacefunc)))

// ********* Support for 2 arguments *********
#define SH_DECL_HOOK2(ifacetype, ifacefunc, attr, overload, rettype, param1, param2) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate2<param1, param2, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (param1, param2), (p1, p2), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2, \
	(static_cast<rettype (ifacetype::*)(param1, param2) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK2_void(ifacetype, ifacefunc, attr, overload, param1, param2) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate2<param1, param2> FD; \
		virtual void Func(param1 p1, param2 p2) \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (param1, param2), (p1, p2)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2, \
	(static_cast<void (ifacetype::*)(param1, param2) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK2_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate3<param1, param2, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, const char *fmt, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (param1, param2, ...), (p1, p2, "%s", buf), (p1, p2, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(param1, param2, const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK2_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate3<param1, param2, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, const char *, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (param1, param2, ...), (p1, p2, "%s", buf), (p1, p2, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|const char*|...", \
	(static_cast<void (ifacetype::*)(param1, param2, const char *, ...) attr>(&ifacetype::ifacefunc)))

// ********* Support for 3 arguments *********
#define SH_DECL_HOOK3(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate3<param1, param2, param3, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (param1, param2, param3), (p1, p2, p3), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3, \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK3_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate3<param1, param2, param3> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3) \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (param1, param2, param3), (p1, p2, p3)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3, \
	(static_cast<void (ifacetype::*)(param1, param2, param3) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK3_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate4<param1, param2, param3, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, const char *fmt, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (param1, param2, param3, ...), (p1, p2, p3, "%s", buf), (p1, p2, p3, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK3_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate4<param1, param2, param3, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, const char *, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (param1, param2, param3, ...), (p1, p2, p3, "%s", buf), (p1, p2, p3, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|const char*|...", \
	(static_cast<void (ifacetype::*)(param1, param2, param3, const char *, ...) attr>(&ifacetype::ifacefunc)))

// ********* Support for 4 arguments *********
#define SH_DECL_HOOK4(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate4<param1, param2, param3, param4, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (param1, param2, param3, param4), (p1, p2, p3, p4), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4, \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK4_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate4<param1, param2, param3, param4> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4) \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (param1, param2, param3, param4), (p1, p2, p3, p4)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4, \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK4_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate5<param1, param2, param3, param4, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, const char *fmt, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, ...), (p1, p2, p3, p4, "%s", buf), (p1, p2, p3, p4, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK4_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate5<param1, param2, param3, param4, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, const char *, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, ...), (p1, p2, p3, p4, "%s", buf), (p1, p2, p3, p4, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|const char*|...", \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, const char *, ...) attr>(&ifacetype::ifacefunc)))

// ********* Support for 5 arguments *********
#define SH_DECL_HOOK5(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate5<param1, param2, param3, param4, param5, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (param1, param2, param3, param4, param5), (p1, p2, p3, p4, p5), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5, \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK5_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate5<param1, param2, param3, param4, param5> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (param1, param2, param3, param4, param5), (p1, p2, p3, p4, p5)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5, \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK5_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, const char *fmt, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, ...), (p1, p2, p3, p4, p5, "%s", buf), (p1, p2, p3, p4, p5, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK5_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, const char *, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, ...), (p1, p2, p3, p4, p5, "%s", buf), (p1, p2, p3, p4, p5, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|const char*|...", \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, const char *, ...) attr>(&ifacetype::ifacefunc)))

// ********* Support for 6 arguments *********
#define SH_DECL_HOOK6(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, param6, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6), (p1, p2, p3, p4, p5, p6), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6, \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK6_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, param6> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6), (p1, p2, p3, p4, p5, p6)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6, \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK6_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, const char *fmt, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, ...), (p1, p2, p3, p4, p5, p6, "%s", buf), (p1, p2, p3, p4, p5, p6, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK6_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, const char *, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, ...), (p1, p2, p3, p4, p5, p6, "%s", buf), (p1, p2, p3, p4, p5, p6, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|const char*|...", \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, const char *, ...) attr>(&ifacetype::ifacefunc)))

// ********* Support for 7 arguments *********
#define SH_DECL_HOOK7(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, param7, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7), (p1, p2, p3, p4, p5, p6, p7), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7, \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK7_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, param7> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7) \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7), (p1, p2, p3, p4, p5, p6, p7)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7, \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK7_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, const char *fmt, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, ...), (p1, p2, p3, p4, p5, p6, p7, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK7_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, const char *, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, ...), (p1, p2, p3, p4, p5, p6, p7, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|const char*|...", \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, const char *, ...) attr>(&ifacetype::ifacefunc)))

// ********* Support for 8 arguments *********
#define SH_DECL_HOOK8(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, param8, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8), (p1, p2, p3, p4, p5, p6, p7, p8), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8, \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK8_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, param8> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8) \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8), (p1, p2, p3, p4, p5, p6, p7, p8)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8, \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK8_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, const char *fmt, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, ...), (p1, p2, p3, p4, p5, p6, p7, p8, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK8_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, const char *, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, ...), (p1, p2, p3, p4, p5, p6, p7, p8, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|const char*|...", \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...) attr>(&ifacetype::ifacefunc)))

// ********* Support for 9 arguments *********
#define SH_DECL_HOOK9(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, param9, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9), (p1, p2, p3, p4, p5, p6, p7, p8, p9), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9, \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK9_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, param9> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9) \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9), (p1, p2, p3, p4, p5, p6, p7, p8, p9)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9, \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK9_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, const char *fmt, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK9_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, const char *, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|const char*|...", \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...) attr>(&ifacetype::ifacefunc)))

// ********* Support for 10 arguments *********
#define SH_DECL_HOOK10(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10, \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK10_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10) \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10, \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK10_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, const char *fmt, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK10_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, const char *, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|const char*|...", \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...) attr>(&ifacetype::ifacefunc)))

// ********* Support for 11 arguments *********
#define SH_DECL_HOOK11(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11, \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK11_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11) \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11, \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK11_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, const char *fmt, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK11_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, const char *, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|const char*|...", \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...) attr>(&ifacetype::ifacefunc)))

// ********* Support for 12 arguments *********
#define SH_DECL_HOOK12(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12, \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK12_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12) \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12, \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK12_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, const char *fmt, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK12_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, const char *, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|const char*|...", \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...) attr>(&ifacetype::ifacefunc)))

// ********* Support for 13 arguments *********
#define SH_DECL_HOOK13(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13, \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK13_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13) \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13, \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK13_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, const char *fmt, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK13_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, const char *, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|const char*|...", \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...) attr>(&ifacetype::ifacefunc)))

// ********* Support for 14 arguments *********
#define SH_DECL_HOOK14(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14, \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK14_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14) \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14, \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK14_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, const char *fmt, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK14_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, const char *, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|const char*|...", \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...) attr>(&ifacetype::ifacefunc)))

// ********* Support for 15 arguments *********
#define SH_DECL_HOOK15(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|" #param15, \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK15_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15) \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|" #param15, \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK15_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, const char *fmt, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|" #param15 "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK15_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, const char *, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|" #param15 "|const char*|...", \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...) attr>(&ifacetype::ifacefunc)))

// ********* Support for 16 arguments *********
#define SH_DECL_HOOK16(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|" #param15 "|" #param16, \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK16_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16) \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|" #param15 "|" #param16, \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK16_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, const char *fmt, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|" #param15 "|" #param16 "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK16_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, const char *, ...) \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|" #param15 "|" #param16 "|const char*|...", \
	(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...) attr>(&ifacetype::ifacefunc)))



//////////////////////////////////////////////////////////////////////////
// SH_CALL


#define SH_MAKE_EXECUTABLECLASS_OB(call, prms) \
{ \
	using namespace ::SourceHook; \
	MemFuncInfo mfi; \
	MFI_Impl<sizeof(m_MFP)>::GetFuncInfo(m_MFP, mfi); \
	OrigVTables::const_iterator iter = m_CC->vt.find(mfi.vtbloffs); \
	if (iter == m_CC->vt.end() || mfi.vtblindex >= (int)iter->second.size() || iter->second[mfi.vtblindex] == NULL) \
		return (m_CC->ptr->*m_MFP)call; \
	\
	/* It's hooked. Call the original function. */ \
	union \
	{ \
		RetType(EmptyClass::*mfpnew)prms; \
		void *addr; \
	} u; \
	u.addr = iter->second[mfi.vtblindex]; \
	\
	return (reinterpret_cast<EmptyClass*>(m_CC->ptr)->*u.mfpnew)call; \
}


namespace SourceHook
{
	template<class CCType, class RetType, class MFPType> class ExecutableClass
	{
		CCType *m_CC;
		MFPType m_MFP;
	public:
		ExecutableClass(CCType *cc, MFPType mfp) : m_CC(cc), m_MFP(mfp)
		{
		}

		// Support for 0 arguments
		RetType operator()() const 
			SH_MAKE_EXECUTABLECLASS_OB((), ())

		// Support for 1 arguments
		template<class Param1> RetType operator()(Param1 p1) const 
			SH_MAKE_EXECUTABLECLASS_OB((p1), (Param1))

		// Support for 2 arguments
		template<class Param1, class Param2> RetType operator()(Param1 p1, Param2 p2) const 
			SH_MAKE_EXECUTABLECLASS_OB((p1p2), (Param1Param2))

		// Support for 3 arguments
		template<class Param1, class Param2, class Param3> RetType operator()(Param1 p1, Param2 p2, Param3 p3) const 
			SH_MAKE_EXECUTABLECLASS_OB((p1p2p3), (Param1Param2Param3))

		// Support for 4 arguments
		template<class Param1, class Param2, class Param3, class Param4> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4) const 
			SH_MAKE_EXECUTABLECLASS_OB((p1p2p3p4), (Param1Param2Param3Param4))

		// Support for 5 arguments
		template<class Param1, class Param2, class Param3, class Param4, class Param5> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const 
			SH_MAKE_EXECUTABLECLASS_OB((p1p2p3p4p5), (Param1Param2Param3Param4Param5))

		// Support for 6 arguments
		template<class Param1, class Param2, class Param3, class Param4, class Param5, class Param6> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const 
			SH_MAKE_EXECUTABLECLASS_OB((p1p2p3p4p5p6), (Param1Param2Param3Param4Param5Param6))

		// Support for 7 arguments
		template<class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const 
			SH_MAKE_EXECUTABLECLASS_OB((p1p2p3p4p5p6p7), (Param1Param2Param3Param4Param5Param6Param7))

		// Support for 8 arguments
		template<class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const 
			SH_MAKE_EXECUTABLECLASS_OB((p1p2p3p4p5p6p7p8), (Param1Param2Param3Param4Param5Param6Param7Param8))

		// Support for 9 arguments
		template<class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const 
			SH_MAKE_EXECUTABLECLASS_OB((p1p2p3p4p5p6p7p8p9), (Param1Param2Param3Param4Param5Param6Param7Param8Param9))

		// Support for 10 arguments
		template<class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const 
			SH_MAKE_EXECUTABLECLASS_OB((p1p2p3p4p5p6p7p8p9p10), (Param1Param2Param3Param4Param5Param6Param7Param8Param9Param10))

		// Support for 11 arguments
		template<class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const 
			SH_MAKE_EXECUTABLECLASS_OB((p1p2p3p4p5p6p7p8p9p10p11), (Param1Param2Param3Param4Param5Param6Param7Param8Param9Param10Param11))

		// Support for 12 arguments
		template<class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const 
			SH_MAKE_EXECUTABLECLASS_OB((p1p2p3p4p5p6p7p8p9p10p11p12), (Param1Param2Param3Param4Param5Param6Param7Param8Param9Param10Param11Param12))

		// Support for 13 arguments
		template<class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const 
			SH_MAKE_EXECUTABLECLASS_OB((p1p2p3p4p5p6p7p8p9p10p11p12p13), (Param1Param2Param3Param4Param5Param6Param7Param8Param9Param10Param11Param12Param13))

		// Support for 14 arguments
		template<class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const 
			SH_MAKE_EXECUTABLECLASS_OB((p1p2p3p4p5p6p7p8p9p10p11p12p13p14), (Param1Param2Param3Param4Param5Param6Param7Param8Param9Param10Param11Param12Param13Param14))

		// Support for 15 arguments
		template<class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const 
			SH_MAKE_EXECUTABLECLASS_OB((p1p2p3p4p5p6p7p8p9p10p11p12p13p14p15), (Param1Param2Param3Param4Param5Param6Param7Param8Param9Param10Param11Param12Param13Param14Param15))

		// Support for 16 arguments
		template<class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const 
			SH_MAKE_EXECUTABLECLASS_OB((p1p2p3p4p5p6p7p8p9p10p11p12p13p14p15p16), (Param1Param2Param3Param4Param5Param6Param7Param8Param9Param10Param11Param12Param13Param14Param15Param16))

	};
}

// Support for 0 arguments
template <class X, class Y, class RetType>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)()>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)())
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)()>(ptr, mfp);
}

template <class X, class Y, class RetType>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)() const>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)()const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)() const>(ptr, mfp);
}

// Support for 1 arguments
template <class X, class Y, class RetType, class Param1>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1)>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1)>(ptr, mfp);
}

template <class X, class Y, class RetType, class Param1>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1) const>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1) const>(ptr, mfp);
}

// Support for 2 arguments
template <class X, class Y, class RetType, class Param1, class Param2>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2)>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2)>(ptr, mfp);
}

template <class X, class Y, class RetType, class Param1, class Param2>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2) const>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2) const>(ptr, mfp);
}

// Support for 3 arguments
template <class X, class Y, class RetType, class Param1, class Param2, class Param3>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3)>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3)>(ptr, mfp);
}

template <class X, class Y, class RetType, class Param1, class Param2, class Param3>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3) const>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3) const>(ptr, mfp);
}

// Support for 4 arguments
template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4)>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4)>(ptr, mfp);
}

template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4) const>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4) const>(ptr, mfp);
}

// Support for 5 arguments
template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5)>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5)>(ptr, mfp);
}

template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5) const>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5) const>(ptr, mfp);
}

// Support for 6 arguments
template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6)>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6)>(ptr, mfp);
}

template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6) const>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6) const>(ptr, mfp);
}

// Support for 7 arguments
template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7)>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7)>(ptr, mfp);
}

template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7) const>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7) const>(ptr, mfp);
}

// Support for 8 arguments
template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8)>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8)>(ptr, mfp);
}

template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8) const>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8) const>(ptr, mfp);
}

// Support for 9 arguments
template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9)>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9)>(ptr, mfp);
}

template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9) const>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9) const>(ptr, mfp);
}

// Support for 10 arguments
template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10)>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10)>(ptr, mfp);
}

template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10) const>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10) const>(ptr, mfp);
}

// Support for 11 arguments
template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11)>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11)>(ptr, mfp);
}

template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11) const>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11) const>(ptr, mfp);
}

// Support for 12 arguments
template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12)>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12)>(ptr, mfp);
}

template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12) const>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12) const>(ptr, mfp);
}

// Support for 13 arguments
template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13)>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13)>(ptr, mfp);
}

template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13) const>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13) const>(ptr, mfp);
}

// Support for 14 arguments
template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14)>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14)>(ptr, mfp);
}

template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14) const>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14) const>(ptr, mfp);
}

// Support for 15 arguments
template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15)>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15)>(ptr, mfp);
}

template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15) const>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15) const>(ptr, mfp);
}

// Support for 16 arguments
template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16)>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16)>(ptr, mfp);
}

template <class X, class Y, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16) const>
SH_CALL(SourceHook::CallClass<Y> *ptr, RetType(X::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, RetType (X::*)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16) const>(ptr, mfp);
}


#undef SH_MAKE_EXECUTABLECLASS_BODY

#endif
	// The pope is dead. -> :(
