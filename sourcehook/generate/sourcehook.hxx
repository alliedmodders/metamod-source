/* ======== SourceHook ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

/**
*	@file sourcehook.h
*	@brief Contains the public SourceHook API
*/

#ifndef __SOURCEHOOK_H__
#define __SOURCEHOOK_H__

#define SH_IFACE_VERSION 1
#define SH_IMPL_VERSION 1

#ifndef SH_GLOB_SHPTR
#define SH_GLOB_SHPTR g_SHPtr
#endif

#ifndef SH_GLOB_PLUGPTR
#define SH_GLOB_PLUGPTR g_PLID
#endif

#ifdef SH_DEBUG
# define SH_ASSERT__(x, info, file, line, func) \
	((printf("SOURCEHOOK DEBUG ASSERTION FAILED:\n  %s:%u(%s): %s\n", file, line, func, info), true) ? (abort(), 0) : 0)
# define SH_ASSERT(x, info) if (!(x)) SH_ASSERT__(x, info, __FILE__, __LINE__, __FUNCTION__)
#else
# define SH_ASSERT(x, info)
#endif

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

#define SH_PTRSIZE sizeof(void*)
										
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
	MRES_SUPERCEDE		// skip real function; use my return value
};


namespace SourceHook
{
	const int STRBUF_LEN=4096;		// In bytes, for "vafmt" functions
	
	/**
	*	@brief An empty class. No inheritance used. Used for original-function-call hacks
	*/
	class EmptyClass
	{
	};

	/**
	*	@brief Implicit cast.
	*/
	template <class In, class Out>
		inline Out implicit_cast(In input)
		{
			return input;
		}

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

		CSHDelegate(const CSHDelegate &other) : m_Deleg(other.m_Deleg)
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
					int thisptr_offs;				//!< This pointer offset
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
			void *orig_entry;						//!< The original vtable entry

			typedef std::list<Iface> IfaceList;
			typedef IfaceList::iterator IfaceListIter;
			IfaceList ifaces;						//!< List of interface pointers

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
		B *ptr;					//!< Pointer to the actual object
		size_t objsize;			//!< Size of the instance
		OrigVTables vt;			//!< Info about vtables & functions
	};

	typedef CallClass<void> GenericCallClass;

	/**
	*	@brief The main SourceHook interface
	*/
	class ISourceHook
	{
	public:
		/**
		*	@brief Return interface version
		*/
		virtual int GetIfaceVersion() = 0;

		/**
		*	@brief Return implementation version
		*/
		virtual int GetImplVersion() = 0;

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
		*	@param size Size of the class instance
		*/
		virtual GenericCallClass *GetCallClass(void *iface, size_t size) = 0;

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
		virtual META_RES &GetCurResRef() = 0;				//!< Gets the reference to the current meta result
		virtual META_RES &GetPrevResRef() = 0;				//!< Gets the reference to the previous meta result
		virtual META_RES &GetStatusRef() = 0;				//!< Gets the reference to the status variable
		virtual void* &GetIfacePtrRef() = 0;				//!< Gets the reference to the interface this pointer
		virtual void SetOrigRet(const void *ptr) = 0;		//!< Sets the original return pointer
		virtual void SetOverrideRet(const void *ptr) = 0;	//!< Sets the override result pointer
	};
}

/************************************************************************/
/* High level interface                                                 */
/************************************************************************/
#define SET_META_RESULT(result)				SH_GLOB_SHPTR->SetRes(result)
#define RETURN_META(result)					do { SET_META_RESULT(result); return; } while(0)
#define RETURN_META_VALUE(result, value)	do { SET_META_RESULT(result); return (value); } while(0)

#define META_RESULT_STATUS					SH_GLOB_SHPTR->GetStatus()
#define META_RESULT_PREVIOUS				SH_GLOB_SHPTR->GetPrevRes()
#define META_RESULT_ORIG_RET(type)			*reinterpret_cast<const type*>(SH_GLOB_SHPTR->GetOrigRet())
#define META_RESULT_OVERRIDE_RET(type)		*reinterpret_cast<const type*>(SH_GLOB_SHPTR->GetOverrideRet())
#define META_IFACEPTR						SH_GLOB_SHPTR->GetIfacePtr()


/**
*	@brief Get/generate callclass for an interface pointer
*
*	@param ifaceptr The interface pointer
*/
template<class ifacetype>
inline SourceHook::CallClass<ifacetype> *SH_GET_CALLCLASS_R(SourceHook::ISourceHook *shptr, ifacetype *ptr)
{
	return reinterpret_cast<SourceHook::CallClass<ifacetype>*>(
		shptr->GetCallClass(reinterpret_cast<void*>(ptr), sizeof(ifacetype)));
}

template<class ifacetype>
inline void SH_RELEASE_CALLCLASS_R(SourceHook::ISourceHook *shptr, SourceHook::CallClass<ifacetype> *ptr)
{
	shptr->ReleaseCallClass(reinterpret_cast<SourceHook::GenericCallClass*>(ptr));
}

#define SH_GET_CALLCLASS(ptr) SH_GET_CALLCLASS_R(SH_GLOB_SHPTR, ptr)
#define SH_RELEASE_CALLCLASS(ptr) SH_RELEASE_CALLCLASS_R(SH_GLOB_SHPTR, ptr)

#define SH_ADD_HOOK(ifacetype, ifacefunc, ifaceptr, handler, post) \
	__SourceHook_FHAdd##ifacetype##ifacefunc((void*)SourceHook::implicit_cast<ifacetype*>(ifaceptr), \
	post, handler)
#define SH_ADD_HOOK_STATICFUNC(ifacetype, ifacefunc, ifaceptr, handler, post) \
	SH_ADD_HOOK(ifacetype, ifacefunc, ifaceptr, fastdelegate::MakeDelegate(handler), post)
#define SH_ADD_HOOK_MEMFUNC(ifacetype, ifacefunc, ifaceptr, handler_inst, handler_func, post) \
	SH_ADD_HOOK(ifacetype, ifacefunc, ifaceptr, fastdelegate::MakeDelegate(handler_inst, handler_func), post)

#define SH_REMOVE_HOOK(ifacetype, ifacefunc, ifaceptr, handler, post) \
	__SourceHook_FHRemove##ifacetype##ifacefunc((void*)SourceHook::implicit_cast<ifacetype*>(ifaceptr), \
	post, handler)
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
		/* Verify interface version */ \
		if (SH_GLOB_SHPTR->GetIfaceVersion() != SH_IFACE_VERSION) \
			return 1; \
		\
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
	void* &ifptr = SH_GLOB_SHPTR->GetIfacePtrRef(); \
	status = MRES_IGNORED; \
	SH_GLOB_SHPTR->SetOrigRet(reinterpret_cast<void*>(&orig_ret)); \
	SH_GLOB_SHPTR->SetOverrideRet(NULL);

#define SH_CALL_HOOKS(post, params) \
	prev_res = MRES_IGNORED; \
	for (std::list<HookManagerInfo::VfnPtr::Iface::Hook>::iterator hiter = post##list.begin(); hiter != post##list.end(); ++hiter) \
	{ \
		if (hiter->paused) continue; \
		cur_res = MRES_IGNORED; \
		ifptr = reinterpret_cast<void*>(reinterpret_cast<char*>(this) - hiter->thisptr_offs); \
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
	void* &ifptr = SH_GLOB_SHPTR->GetIfacePtrRef(); \
	status = MRES_IGNORED; \
	SH_GLOB_SHPTR->SetOverrideRet(NULL);

#define SH_CALL_HOOKS_void(post, params) \
	prev_res = MRES_IGNORED; \
	for (std::list<HookManagerInfo::VfnPtr::Iface::Hook>::iterator hiter = post##list.begin(); hiter != post##list.end(); ++hiter) \
	{ \
		if (hiter->paused) continue; \
		cur_res = MRES_IGNORED; \
		ifptr = reinterpret_cast<void*>(reinterpret_cast<char*>(this) - hiter->thisptr_offs); \
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

@VARARGS@
// ********* Support for @$@ arguments *********
#define SH_DECL_HOOK@$@(ifacetype, ifacefunc, attr, overload, rettype@, param%%@) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(@param%%|, @) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate@$@<@param%%|, @@, @rettype> FD; \
		virtual rettype Func(@param%% p%%|, @) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (@param%%|, @), (@p%%|, @), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype @"|" #param%%| @, \
	(static_cast<rettype (ifacetype::*)(@param%%|, @) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK@$@_void(ifacetype, ifacefunc, attr, overload@, param%%@) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(@param%%|, @) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate@$@<@param%%|, @> FD; \
		virtual void Func(@param%% p%%|, @) \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (@param%%|, @), (@p%%|, @)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr @"|" #param%%| @, \
	(static_cast<void (ifacetype::*)(@param%%|, @) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK@$@_vafmt(ifacetype, ifacefunc, attr, overload, rettype@, param%%@) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(@param%%|, @@, @const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate@$+1@<@param%%|, @@, @const char *, rettype> FD; \
		virtual rettype Func(@param%% p%%|, @@, @const char *fmt, ...) \
		{ \
			char buf[::SourceHook::STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (@param%%|, @@, @...), (@p%%|, @@, @"%s", buf), (@p%%|, @@, @buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype @"|" #param%%| @ "|const char*|...", \
	(static_cast<rettype (ifacetype::*)(@param%%|, @@, @const char *, ...) attr>(&ifacetype::ifacefunc)))

#define SH_DECL_HOOK@$@_void_vafmt(ifacetype, ifacefunc, attr, overload@, param%%@) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(@param%%|, @@, @const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate@$+1@<@param%%|, @@, @const char *> FD; \
		virtual void Func(@param%% p%%|, @@, @const char *fmt, ...) \
		{ \
			char buf[::SourceHook::STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (@param%%|, @@, @...), (@p%%|, @@, @"%s", buf), (@p%%|, @@, @buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr @"|" #param%%| @ "|const char*|...", \
	(static_cast<void (ifacetype::*)(@param%%|, @@, @const char *, ...) attr>(&ifacetype::ifacefunc)))

@ENDARGS@


//////////////////////////////////////////////////////////////////////////
// SH_CALL

#if SH_COMP == SH_COMP_MSVC

# define SH_MAKE_EXECUTABLECLASS_OB(call, prms) \
{ \
	using namespace ::SourceHook; \
	MemFuncInfo mfi; \
	GetFuncInfo(m_CC->ptr, m_MFP, mfi); \
	OrigVTables::const_iterator iter = m_CC->vt.find(mfi.thisptroffs + mfi.vtbloffs); \
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
	void *adjustedthisptr = reinterpret_cast<void*>(reinterpret_cast<char*>(m_CC->ptr) + mfi.thisptroffs); \
	return (reinterpret_cast<EmptyClass*>(adjustedthisptr)->*u.mfpnew)call; \
}

#elif SH_COMP == SH_COMP_GCC

# define SH_MAKE_EXECUTABLECLASS_OB(call, prms) \
{ \
	using namespace ::SourceHook; \
	MemFuncInfo mfi; \
	GetFuncInfo(m_CC->ptr, m_MFP, mfi); \
	OrigVTables::const_iterator iter = m_CC->vt.find(mfi.thisptroffs + mfi.vtbloffs); \
	if (iter == m_CC->vt.end() || mfi.vtblindex >= (int)iter->second.size() || iter->second[mfi.vtblindex] == NULL) \
		return (m_CC->ptr->*m_MFP)call; \
	\
	/* It's hooked. Call the original function. */ \
	union \
	{ \
		RetType(EmptyClass::*mfpnew)prms; \
		struct \
		{ \
			void *addr; \
			intptr_t adjustor; \
		} s; \
	} u; \
	u.s.addr = iter->second[mfi.vtblindex]; \
	u.s.adjustor = mfi.thisptroffs; \
	\
	return (reinterpret_cast<EmptyClass*>(m_CC->ptr)->*u.mfpnew)call; \
}

#endif

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

@VARARGS@
		// Support for @$@ arguments
		@template<@@class Param%%|, @@> @RetType operator()(@Param%% p%%|, @) const 
			SH_MAKE_EXECUTABLECLASS_OB((@p%%|, @), (@Param%%|, @))

@ENDARGS@
	};
}

// SH_CALL needs to deduce the return type -> it uses templates and function overloading
// That's why SH_CALL takes two parameters: "mfp2" of type RetType(X::*mfp)(params), and "mfp" of type MFP
// The only purpose of the mfp2 parameter is to extract the return type

@VARARGS@
// Support for @$@ arguments
template <class X, class Y, class MFP, class RetType@, @@class Param%%|, @>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, MFP>
SH_CALL2(SourceHook::CallClass<Y> *ptr, MFP mfp, RetType(X::*mfp2)(@Param%%|, @))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, MFP>(ptr, mfp);
}

template <class X, class Y, class MFP, class RetType@, @@class Param%%|, @>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, MFP>
SH_CALL2(SourceHook::CallClass<Y> *ptr, MFP mfp, RetType(X::*mfp2)(@Param%%|, @)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, MFP>(ptr, mfp);
}

@ENDARGS@

#if SH_COMP != SH_COMP_MSVC

// **** MSVC doesn't like these, GCC needs them ****

@VARARGS@
// Support for @$@ arguments
template <class X, class Y, class MFP, class RetType@, @@class Param%%|, @>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, MFP>
SH_CALL2(SourceHook::CallClass<Y> *ptr, MFP mfp, RetType(X::*mfp2)(@Param%%|, @@, @...))
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, MFP>(ptr, mfp);
}

template <class X, class Y, class MFP, class RetType@, @@class Param%%|, @>
SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, MFP>
SH_CALL2(SourceHook::CallClass<Y> *ptr, MFP mfp, RetType(X::*mfp2)(@Param%%|, @@, @...)const)
{
	return SourceHook::ExecutableClass<SourceHook::CallClass<Y>, RetType, MFP>(ptr, mfp);
}

@ENDARGS@

#endif

#define SH_CALL(ptr, mfp) SH_CALL2((ptr), (mfp), (mfp))

#undef SH_MAKE_EXECUTABLECLASS_BODY

#endif
	// The pope is dead. -> :(
