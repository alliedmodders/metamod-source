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
 * @file sourcehook.h
 * @brief Contains the public SourceHook API
*/

#ifndef __SOURCEHOOK_H__
#define __SOURCEHOOK_H__

#ifndef SH_GLOB_SHPTR
#define SH_GLOB_SHPTR g_SHPtr
#endif

#ifndef SH_GLOB_PLUGPTR
#define SH_GLOB_PLUGPTR g_PLID
#endif

#define SH_ASSERT(x) if (!(x)) __asm { int 3 }

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
		struct Iface
		{
			struct Hook
			{
				ISHDelegate *handler;			//!< Pointer to the handler
				bool paused;					//!< If true, the hook should not be executed
				Plugin plug;					//!< The owner plugin
			};
			void *callclass;					//!< Stores a call class for this interface
			void *ptr;							//!< Pointer to the interface instance
			void *orig_entry;					//!< The original vtable entry
			std::list<Hook> hooks_pre;			//!< A list of pre-hooks
			std::list<Hook> hooks_post;			//!< A list of post-hooks
			bool operator ==(void *other) const
			{
				return ptr == other;
			}
		};
		Plugin plug;							//!< The owner plugin
		const char *proto;						//!< The prototype of the function the hook manager is responsible for
		int vtbl_idx;							//!< The vtable index
		int vtbl_offs;							//!< The vtable offset
		int thisptr_offs;						//!< The this-pointer-adjuster
		HookManagerPubFunc func;				//!< The interface to the hook manager

		int hookfunc_vtbl_idx;					//!< the vtable index of the hookfunc
		int hookfunc_vtbl_offs;					//!< the vtable offset of the hookfunc
		void *hookfunc_inst;					//!< Instance of the class the hookfunc is in
		
		std::list<Iface> ifaces;				//!< List of hooked interfaces
	};

	/**
	*	@brief Structure describing a callclass
	*/
	struct CallClass
	{
		struct VTable
		{
			void *ptr;
			int actsize;
			std::list<int> patches;	//!< Already patched entries
			bool operator ==(void *other) const
			{
				return ptr == other;
			}
		};
		void *iface;				//!< The iface pointer this callclass belongs to
		size_t size;				//!< The size of the callclass
		void *ptr;					//!< The actual "object"
		typedef std::list<VTable> VTableList;
		VTableList vtables;			//!< Already known vtables
		int refcounter;				//!< How many times it was requested

		bool operator ==(void *other) const
		{
			return ptr == other;
		}
	};

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
		virtual bool AddHook(Plugin plug, void *iface, int ifacesize, HookManagerPubFunc myHookMan, ISHDelegate *handler, bool post) = 0;

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
		virtual bool RemoveHook(Plugin plug, void *iface, HookManagerPubFunc myHookMan, ISHDelegate *handler, bool post) = 0;

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
		virtual void *GetCallClass(void *iface, size_t size) = 0;

		/**
		*	@brief Release a callclass
		*
		*	@param ptr Pointer to the callclass
		*/
		virtual void ReleaseCallClass(void *ptr) = 0;

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
*	@param ifacetype The type of the interface
*	@param ifaceptr The interface pointer
*/
#define SH_GET_CALLCLASS(ifacetype, ifaceptr) reinterpret_cast<ifacetype*>(SH_GLOB_SHPTR->GetCallClass(ifaceptr, sizeof(ifacetype)))
#define SH_RELEASE_CALLCLASS(ptr) SH_GLOB_SHPTR->ReleaseCallClass(reinterpret_cast<void*>(ptr))

#define SH_ADD_HOOK(ifacetype, ifacefunc, ifaceptr, handler, post) \
	SourceHook::SH_FHAdd##ifacetype##ifacefunc((void*)ifaceptr, post, handler)
#define SH_ADD_HOOK_STATICFUNC(ifacetype, ifacefunc, ifaceptr, handler, post) \
	SH_ADD_HOOK(ifacetype, ifacefunc, ifaceptr, handler, post)
#define SH_ADD_HOOK_MEMFUNC(ifacetype, ifacefunc, ifaceptr, handler_inst, handler_func, post) \
	SH_ADD_HOOK(ifacetype, ifacefunc, ifaceptr, fastdelegate::MakeDelegate(handler_inst, handler_func), post)

#define SH_REMOVE_HOOK(ifacetype, ifacefunc, ifaceptr, handler, post) \
	SourceHook::SH_FHRemove##ifacetype##ifacefunc((void*)ifaceptr, post, handler)
#define SH_REMOVE_HOOK_STATICFUNC(ifacetype, ifacefunc, ifaceptr, handler, post) \
	SH_REMOVE_HOOK(ifacetype, ifacefunc, ifaceptr, handler, post)
#define SH_REMOVE_HOOK_MEMFUNC(ifacetype, ifacefunc, ifaceptr, handler_inst, handler_func, post) \
	SH_REMOVE_HOOK(ifacetype, ifacefunc, ifaceptr, fastdelegate::MakeDelegate(handler_inst, handler_func), post)

#define SH_NOATTRIB


//////////////////////////////////////////////////////////////////////////
#define SH_FHCls(ift, iff, ov) FHCls_##ift##iff##ov

#define SHINT_MAKE_HOOKMANPUBFUNC(ifacetype, ifacefunc, overload, funcptr) \
	static int HookManPubFunc(HookManagerAction action, HookManagerInfo *param) \
	{ \
		if (action == HA_GetInfo) \
		{ \
			param->proto = ms_Proto; \
			MemFuncInfo mfi; \
			GetFuncInfo(funcptr, mfi); \
			param->vtbl_idx = mfi.vtblindex; \
			param->vtbl_offs = mfi.vtbloffs; \
			param->thisptr_offs = mfi.thisptroffs; \
			if (param->thisptr_offs < 0) \
				return 2; /*No virtual inheritance supported*/ \
			GetFuncInfo(&SH_FHCls(ifacetype,ifacefunc,overload)::Func, mfi); \
			param->hookfunc_vtbl_idx = mfi.vtblindex; \
			param->hookfunc_vtbl_offs = mfi.vtbloffs; \
			param->hookfunc_inst = (void*)&ms_Inst; \
			return 0; \
		} \
		else if (action == HA_Register) \
		{ \
			ms_HI = param; \
			return 0; \
		} \
		else if (action == HA_Unregister) \
		{ \
			ms_HI = NULL; \
			return 0; \
		} \
		else \
			return 1; \
	}

#define SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, funcptr) \
	namespace SourceHook \
	{ \
		struct SH_FHCls(ifacetype,ifacefunc,overload) \
		{ \
			static SH_FHCls(ifacetype,ifacefunc,overload) ms_Inst; \
			static HookManagerInfo *ms_HI; \
			static const char *ms_Proto; \
			SHINT_MAKE_HOOKMANPUBFUNC(ifacetype, ifacefunc, overload, funcptr)

#define SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, proto) \
		}; \
		const char *SH_FHCls(ifacetype,ifacefunc,overload)::ms_Proto = proto; \
		SH_FHCls(ifacetype,ifacefunc,overload) SH_FHCls(ifacetype,ifacefunc,overload)::ms_Inst; \
		HookManagerInfo *SH_FHCls(ifacetype,ifacefunc,overload)::ms_HI; \
		bool SH_FHAdd##ifacetype##ifacefunc(void *iface, bool post, \
			SH_FHCls(ifacetype,ifacefunc,overload)::FD handler) \
		{ \
			return SH_GLOB_SHPTR->AddHook(SH_GLOB_PLUGPTR, iface, sizeof(ifacetype), \
				SH_FHCls(ifacetype,ifacefunc,overload)::HookManPubFunc, \
				new CSHDelegate<SH_FHCls(ifacetype,ifacefunc,overload)::FD>(handler), post); \
		} \
		bool SH_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
			SH_FHCls(ifacetype,ifacefunc,overload)::FD handler) \
		{ \
			CSHDelegate<SH_FHCls(ifacetype,ifacefunc,overload)::FD> tmp(handler); \
			return SH_GLOB_SHPTR->RemoveHook(SH_GLOB_PLUGPTR, iface, \
				SH_FHCls(ifacetype,ifacefunc,overload)::HookManPubFunc, &tmp, post); \
		} \
	}

#define SH_SETUPCALLS(rettype) \
	/* 1) Find the iface ptr */ \
	/* 1.1) Adjust to original this pointer */ \
	void *origthis = this - ms_HI->thisptr_offs; \
	std::list<HookManagerInfo::Iface>::iterator ifaceiter = std::find(ms_HI->ifaces.begin(), \
		ms_HI->ifaces.end(), origthis); \
	SH_ASSERT(ifaceiter != ms_HI->ifaces.end()); \
	HookManagerInfo::Iface &ci = *ifaceiter; \
	/* 2) Declare some vars and set it up */ \
	std::list<HookManagerInfo::Iface::Hook> &prelist = ci.hooks_pre; \
	std::list<HookManagerInfo::Iface::Hook> &postlist = ci.hooks_post; \
	rettype orig_ret; \
	rettype override_ret; \
	rettype plugin_ret; \
	META_RES &cur_res = SH_GLOB_SHPTR->GetCurResRef(); \
	META_RES &prev_res = SH_GLOB_SHPTR->GetPrevResRef(); \
	META_RES &status = SH_GLOB_SHPTR->GetStatusRef(); \
	status = MRES_IGNORED; \
	SH_GLOB_SHPTR->SetIfacePtr(ci.ptr); \
	SH_GLOB_SHPTR->SetOrigRet(reinterpret_cast<void*>(&orig_ret)); \
	SH_GLOB_SHPTR->SetOverrideRet(NULL);

#define SH_CALL_HOOKS(post, params) \
	prev_res = MRES_IGNORED; \
	for (std::list<HookManagerInfo::Iface::Hook>::iterator hiter = post##list.begin(); hiter != post##list.end(); ++hiter) \
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

#define SH_CALL_ORIG(ifacetype, ifacefunc, params) \
	if (status != MRES_SUPERCEDE) \
		orig_ret = reinterpret_cast<ifacetype*>(ci.callclass)->ifacefunc params; \
	else \
		orig_ret = override_ret;

#define SH_RETURN() \
	return status >= MRES_OVERRIDE ? override_ret : orig_ret;

#define SH_HANDLEFUNC(ifacetype, ifacefunc, params, rettype) \
	SH_SETUPCALLS(rettype) \
	SH_CALL_HOOKS(pre, params) \
	SH_CALL_ORIG(ifacetype, ifacefunc, params) \
	SH_CALL_HOOKS(post, params) \
	SH_RETURN()

//////////////////////////////////////////////////////////////////////////
#define SH_SETUPCALLS_void() \
	/* 1) Find the iface ptr */ \
	/* 1.1) Adjust to original this pointer */ \
	void *origthis = this - ms_HI->thisptr_offs; \
	std::list<HookManagerInfo::Iface>::iterator ifaceiter = std::find(ms_HI->ifaces.begin(), \
		ms_HI->ifaces.end(), origthis); \
	SH_ASSERT(ifaceiter != ms_HI->ifaces.end()); \
	HookManagerInfo::Iface &ci = *ifaceiter; \
	/* 2) Declare some vars and set it up */ \
	std::list<HookManagerInfo::Iface::Hook> &prelist = ci.hooks_pre; \
	std::list<HookManagerInfo::Iface::Hook> &postlist = ci.hooks_post; \
	META_RES &cur_res = SH_GLOB_SHPTR->GetCurResRef(); \
	META_RES &prev_res = SH_GLOB_SHPTR->GetPrevResRef(); \
	META_RES &status = SH_GLOB_SHPTR->GetStatusRef(); \
	status = MRES_IGNORED; \
	SH_GLOB_SHPTR->SetIfacePtr(ci.ptr); \
	SH_GLOB_SHPTR->SetOverrideRet(NULL);

#define SH_CALL_HOOKS_void(post, params) \
	prev_res = MRES_IGNORED; \
	for (std::list<HookManagerInfo::Iface::Hook>::iterator hiter = post##list.begin(); hiter != post##list.end(); ++hiter) \
	{ \
		if (hiter->paused) continue; \
		cur_res = MRES_IGNORED; \
		reinterpret_cast<CSHDelegate<FD>*>(hiter->handler)->GetDeleg() params; \
		prev_res = cur_res; \
		if (cur_res > status) \
			status = cur_res; \
	}

#define SH_CALL_ORIG_void(ifacetype, ifacefunc, params) \
	if (status != MRES_SUPERCEDE) \
		reinterpret_cast<ifacetype*>(ci.callclass)->ifacefunc params;

#define SH_RETURN_void()

#define SH_HANDLEFUNC_void(ifacetype, ifacefunc, params) \
	SH_SETUPCALLS_void() \
	SH_CALL_HOOKS_void(pre, params) \
	SH_CALL_ORIG_void(ifacetype, ifacefunc, params) \
	SH_CALL_HOOKS_void(post, params) \
	SH_RETURN_void()


// Special vafmt handlers
#define SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, params_orig, params_plug, rettype) \
	SH_SETUPCALLS(rettype) \
	SH_CALL_HOOKS(pre, params_plug) \
	SH_CALL_ORIG(ifacetype, ifacefunc, params_orig) \
	SH_CALL_HOOKS(post, params_plug) \
	SH_RETURN()

#define SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, params_orig, params_plug) \
	SH_SETUPCALLS_void() \
	SH_CALL_HOOKS_void(pre, params_plug) \
	SH_CALL_ORIG_void(ifacetype, ifacefunc, params_orig) \
	SH_CALL_HOOKS_void(post, params_plug) \
	SH_RETURN_void()

//////////////////////////////////////////////////////////////////////////

// *********
#define SH_DECL_HOOK0(ifacetype, ifacefunc, attr, overload, rettype) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)()> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate0<rettype> FD; \
		virtual rettype Func() attr \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype )

#define SH_DECL_HOOK0_void(ifacetype, ifacefunc, attr, overload) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)()> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate0<> FD; \
		virtual void Func() attr \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, ()); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr )

#define SH_DECL_HOOK0_vafmt(ifacetype, ifacefunc, attr, overload, rettype) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate1<const char *, rettype> FD; \
		virtual rettype Func(const char *fmt, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, ("%s", buf), (buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype  "|const char*|...")

#define SH_DECL_HOOK0_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate1<const char *> FD; \
		virtual void Func(const char *, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, ("%s", buf), (buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr  "|const char*|...")

// *********
#define SH_DECL_HOOK1(ifacetype, ifacefunc, attr, overload, rettype, param1) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate1<param1, rettype> FD; \
		virtual rettype Func(param1 p1) attr \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (p1), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1)

#define SH_DECL_HOOK1_void(ifacetype, ifacefunc, attr, overload, param1) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate1<param1> FD; \
		virtual void Func(param1 p1) attr \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (p1)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1)

#define SH_DECL_HOOK1_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate2<param1, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, const char *fmt, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (p1, "%s", buf), (p1, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|const char*|...")

#define SH_DECL_HOOK1_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate2<param1, const char *> FD; \
		virtual void Func(param1 p1, const char *, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (p1, "%s", buf), (p1, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|const char*|...")

// *********
#define SH_DECL_HOOK2(ifacetype, ifacefunc, attr, overload, rettype, param1, param2) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate2<param1, param2, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2) attr \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (p1, p2), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2)

#define SH_DECL_HOOK2_void(ifacetype, ifacefunc, attr, overload, param1, param2) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate2<param1, param2> FD; \
		virtual void Func(param1 p1, param2 p2) attr \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (p1, p2)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2)

#define SH_DECL_HOOK2_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate3<param1, param2, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, const char *fmt, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (p1, p2, "%s", buf), (p1, p2, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|const char*|...")

#define SH_DECL_HOOK2_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate3<param1, param2, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, const char *, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (p1, p2, "%s", buf), (p1, p2, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|const char*|...")

// *********
#define SH_DECL_HOOK3(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate3<param1, param2, param3, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3) attr \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (p1, p2, p3), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3)

#define SH_DECL_HOOK3_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate3<param1, param2, param3> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3) attr \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (p1, p2, p3)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3)

#define SH_DECL_HOOK3_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate4<param1, param2, param3, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, const char *fmt, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (p1, p2, p3, "%s", buf), (p1, p2, p3, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|const char*|...")

#define SH_DECL_HOOK3_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate4<param1, param2, param3, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, const char *, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (p1, p2, p3, "%s", buf), (p1, p2, p3, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|const char*|...")

// *********
#define SH_DECL_HOOK4(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate4<param1, param2, param3, param4, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4) attr \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (p1, p2, p3, p4), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4)

#define SH_DECL_HOOK4_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate4<param1, param2, param3, param4> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4) attr \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (p1, p2, p3, p4)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4)

#define SH_DECL_HOOK4_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate5<param1, param2, param3, param4, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, const char *fmt, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, "%s", buf), (p1, p2, p3, p4, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|const char*|...")

#define SH_DECL_HOOK4_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate5<param1, param2, param3, param4, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, const char *, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, "%s", buf), (p1, p2, p3, p4, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|const char*|...")

// *********
#define SH_DECL_HOOK5(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate5<param1, param2, param3, param4, param5, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) attr \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (p1, p2, p3, p4, p5), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5)

#define SH_DECL_HOOK5_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate5<param1, param2, param3, param4, param5> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) attr \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (p1, p2, p3, p4, p5)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5)

#define SH_DECL_HOOK5_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, const char *fmt, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, "%s", buf), (p1, p2, p3, p4, p5, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|const char*|...")

#define SH_DECL_HOOK5_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, const char *, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, "%s", buf), (p1, p2, p3, p4, p5, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|const char*|...")

// *********
#define SH_DECL_HOOK6(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, param6, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) attr \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6)

#define SH_DECL_HOOK6_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, param6> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) attr \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6)

#define SH_DECL_HOOK6_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, const char *fmt, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, "%s", buf), (p1, p2, p3, p4, p5, p6, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|const char*|...")

#define SH_DECL_HOOK6_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, const char *, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, "%s", buf), (p1, p2, p3, p4, p5, p6, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|const char*|...")

// *********
#define SH_DECL_HOOK7(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, param7, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7) attr \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7)

#define SH_DECL_HOOK7_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, param7> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7) attr \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7)

#define SH_DECL_HOOK7_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, const char *fmt, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|const char*|...")

#define SH_DECL_HOOK7_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, const char *, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|const char*|...")

// *********
#define SH_DECL_HOOK8(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, param8, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8) attr \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8)

#define SH_DECL_HOOK8_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, param8> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8) attr \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8)

#define SH_DECL_HOOK8_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, const char *fmt, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|const char*|...")

#define SH_DECL_HOOK8_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, const char *, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|const char*|...")

// *********
#define SH_DECL_HOOK9(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, param9, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9) attr \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9)

#define SH_DECL_HOOK9_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, param9> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9) attr \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9)

#define SH_DECL_HOOK9_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, const char *fmt, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|const char*|...")

#define SH_DECL_HOOK9_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, const char *, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|const char*|...")

// *********
#define SH_DECL_HOOK10(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10) attr \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10)

#define SH_DECL_HOOK10_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10) attr \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10)

#define SH_DECL_HOOK10_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, const char *fmt, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|const char*|...")

#define SH_DECL_HOOK10_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, const char *, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|const char*|...")

// *********
#define SH_DECL_HOOK11(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11) attr \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11)

#define SH_DECL_HOOK11_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11) attr \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11)

#define SH_DECL_HOOK11_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, const char *fmt, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|const char*|...")

#define SH_DECL_HOOK11_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, const char *, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|const char*|...")

// *********
#define SH_DECL_HOOK12(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12) attr \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12)

#define SH_DECL_HOOK12_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12) attr \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12)

#define SH_DECL_HOOK12_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, const char *fmt, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|const char*|...")

#define SH_DECL_HOOK12_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, const char *, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|const char*|...")

// *********
#define SH_DECL_HOOK13(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13) attr \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13)

#define SH_DECL_HOOK13_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13) attr \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13)

#define SH_DECL_HOOK13_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, const char *fmt, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|const char*|...")

#define SH_DECL_HOOK13_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, const char *, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|const char*|...")

// *********
#define SH_DECL_HOOK14(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14) attr \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14)

#define SH_DECL_HOOK14_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14) attr \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14)

#define SH_DECL_HOOK14_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, const char *fmt, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|const char*|...")

#define SH_DECL_HOOK14_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, const char *, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|const char*|...")

// *********
#define SH_DECL_HOOK15(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15) attr \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|" #param15)

#define SH_DECL_HOOK15_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15) attr \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|" #param15)

#define SH_DECL_HOOK15_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, const char *fmt, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|" #param15 "|const char*|...")

#define SH_DECL_HOOK15_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, const char *, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|" #param15 "|const char*|...")

// *********
#define SH_DECL_HOOK16(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16) attr \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|" #param15 "|" #param16)

#define SH_DECL_HOOK16_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16) attr \
		{ SH_HANDLEFUNC_void(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|" #param15 "|" #param16)

#define SH_DECL_HOOK16_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, rettype> FD; \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, const char *fmt, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|" #param15 "|" #param16 "|const char*|...")

#define SH_DECL_HOOK16_void_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...)> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *> FD; \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, const char *, ...) attr \
		{ \
			char buf[STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt(ifacetype, ifacefunc, (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #param1 "|" #param2 "|" #param3 "|" #param4 "|" #param5 "|" #param6 "|" #param7 "|" #param8 "|" #param9 "|" #param10 "|" #param11 "|" #param12 "|" #param13 "|" #param14 "|" #param15 "|" #param16 "|const char*|...")


/*
#define SH_DECL_HOOK1(ifacetype, ifacefunc, attr, overload, rettype, param1) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload) \
		typedef fastdelegate::FastDelegate1<param1, rettype> FD; \
		virtual rettype Func(param1 p1) \
		{ SH_HANDLEFUNC(ifacetype, ifacefunc, (p1), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, #attr "|" #rettype "|" #param1)
		

*/

#endif
	// The pope is dead. -> :(
