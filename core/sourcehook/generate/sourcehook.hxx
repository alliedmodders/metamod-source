/* ======== SourceHook ========
* Copyright (C) 2004-2010 Metamod:Source Development Team
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

// Interface revisions:
//  1 - Initial revision
//  2 - Changed to virtual functions for iterators and all queries
//  3 - Added "hook loop status variable"
//  4 - Reentrant
//  5 - New "V2" interface
#define SH_IFACE_VERSION 5

// Impl versions:
// ???
// 4 - addition of hook ids and vp hooks (with them, AddHookNew and RemoveHookNew)
//     This is not a SH_IFACE_VERSION change so that old plugins continue working!
// 5 - implementation of the new "V2" interface
#define SH_IMPL_VERSION 5

// Hookman version:
// 1 - standard
#define SH_HOOKMAN_VERSION 1

// Hookmanautogen versions
//  1 - initial
#define SH_HOOKMANAUTOGEN_IFACE_VERSION 1
#define SH_HOOKMANAUTOGEN_IMPL_VERSION 1

// The value of SH_GLOB_SHPTR has to be a pointer to SourceHook::ISourceHook
// It's used in various macros
#ifndef SH_GLOB_SHPTR
#define SH_GLOB_SHPTR g_SHPtr
#endif

// Used to identify the current plugin
#ifndef SH_GLOB_PLUGPTR
#define SH_GLOB_PLUGPTR g_PLID
#endif

#ifdef SH_DEBUG
# include <stdio.h>
# include <stdlib.h>

# define SH_ASSERT(x, info) \
	do { \
		if (!(x)) \
		{ \
			printf("SOURCEHOOK DEBUG ASSERTION FAILED: %s:%u(%s): %s: ", __FILE__, __LINE__, __FUNCTION__, #x); \
			printf info; \
			putchar('\n'); \
			abort(); \
		} \
	} while(0)

#else
# define SH_ASSERT(x, info)
#endif

// System
#define SH_SYS_WIN32 1
#define SH_SYS_LINUX 2
#define SH_SYS_APPLE 3

// Platform
#define SH_XP_POSIX  10
#define SH_XP_WINAPI 20

#ifdef _WIN32
# define SH_SYS SH_SYS_WIN32
# define SH_XP  SH_XP_WINAPI
#elif defined __linux__
# define SH_SYS SH_SYS_LINUX
# define SH_XP  SH_XP_POSIX
#elif defined __APPLE__
# define SH_SYS SH_SYS_APPLE
# define SH_XP  SH_XP_POSIX
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

#if SH_COMP==SH_COMP_MSVC && _MSC_VER < 1500
# define vsnprintf _vsnprintf
#endif

#if SH_SYS != SH_SYS_WIN32
# include <unistd.h>
#endif

#define SH_PTRSIZE sizeof(void*)

#include "sh_memfuncinfo.h"
#include "FastDelegate.h"

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
	/**
	*	@brief	Specifies the size (in bytes) for the internal buffer of vafmt(printf-like) function handlers
	*/
	const int STRBUF_LEN=4096;

	/**
	*	@brief	An empty class. No inheritance used. Used for original-function-call hacks
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
	*	SourceHook doesn't really care what this is. As long as the ==, != and = operators work on it and every
	*	plugin has a unique identifier, everything is ok.
	*	It should also be possible to set it to 0.
	*/
	typedef int Plugin;


	template <class T> class CallClass : public T
	{
	};
	typedef void ManualCallClass;

	/**
	*	@brief Describes the way a parameter/retval is passed.
	*/
	struct PassInfo
	{
		enum PassType
		{
			PassType_Unknown=0,		/**< Unknown -- no extra info available */
			PassType_Basic,			/**< Plain old register data (pointers, integers) */
			PassType_Float,			/**< Floating point data */
			PassType_Object,		/**< Object or structure */
		};

		enum PassFlags
		{
			PassFlag_ByVal		= (1<<0),		/**< Passing by value */
			PassFlag_ByRef		= (1<<1),		/**< Passing by reference */
			PassFlag_ODtor		= (1<<2),		/**< Object has a destructor */
			PassFlag_OCtor		= (1<<3),		/**< Object has a normal non-trivial constructor */
			PassFlag_AssignOp	= (1<<4),		/**< Object has a non-trivial assignment operator */
			PassFlag_CCtor		= (1<<5),		/**< Object has a copy constructor (which takes const Object& as only parameter) */

			// The following two flags are only relevant for byval return types.
			// SH tries to auto-detect these
			// If you want to override SH's auto-detection, pass them in yourself
			PassFlag_RetMem		= (1<<6),		/**< Object is returned in memory (through hidden first param */
			PassFlag_RetReg		= (1<<7)		/**< Object is returned in EAX(:EDX) */
		};

		size_t size;			//!< Size of the data being passed

		// Extra info:
		//  (might be used in future versions for automatic hookfunc generation)
		int type;				//!< PassType value
		unsigned int flags;		//!< Pass/return flags

		struct V2Info
		{
			void *pNormalCtor;
			void *pCopyCtor;
			void *pDtor;
			void *pAssignOperator;
		};
	};

	struct ProtoInfo
	{
		enum CallConvention
		{
			CallConv_Unknown,		/**< Unknown  -- no extra info available (0)*/
			CallConv_ThisCall,		/**< This call (object pointer required) (1)*/
			CallConv_Cdecl,			/**< C call								 (2)*/
			CallConv_StdCall,		/**< Windows "stdcall"					 (3)*/

			CallConv_HasVarArgs = (1<<16),	/**< Has variable arguments */
			CallConv_HasVafmt = CallConv_HasVarArgs | (1<<17)	/**< last params: const char*, ... */
		};

		int numOfParams;			//!< number of parameters
		PassInfo retPassInfo;		//!< PassInfo for the return value. size=0 -> no retval
		const PassInfo *paramsPassInfo;	//!< PassInfos for the parameters

		// paramsPassInfo[0] is basically a dummy parameter.
		// However, paramsPassInfo[0].size stores the version of the ProtoInfo structure.

		// Extra info:
		int convention;

		// Version2:
		PassInfo::V2Info retPassInfo2;
		const PassInfo::V2Info *paramsPassInfo2;
	};

	struct IHookManagerInfo;

	/**
	*	@brief Pointer to hook manager interface function
	*
	*	The hook manager should store hi for later use if store==true. It should then call hi->SetInfo(...) if hi
	*   is non-null. The hook manager can return 0 for success or a non-zero value if it doesn't want to be used.
	*
	*	@param hi A pointer to IHookManagerInfo
	*/
	typedef int (*HookManagerPubFunc)(bool store, IHookManagerInfo *hi);

	class ISHDelegate
	{
	public:
		virtual bool IsEqual(ISHDelegate *pOtherDeleg) = 0;		// pOtherDeleg is from the same plugin and hookman
		virtual void DeleteThis() = 0;
	};

	struct IHookManagerInfo
	{
		virtual void SetInfo(int hookman_version, int vtbloffs, int vtblidx,
			ProtoInfo *proto, void *hookfunc_vfnptr) = 0;
	};

	// I'm adding support for functions which return references.

	// How it works:
	//  SH_SETUPCALLS doesn't use plain rettype to store the temporary return values (plugin ret, orig ret,
	//  override ret) anymore; instead, it uses SourceHook::ReferenceCarrier<rettype>::type
	//  this is typedefed to the original rettype normally, but if the rettype is a reference, it's a special class
	//  which stores the reference as a pointer, and implements constructors, operator= and a conversion operator.
	//  special cases were needed for getoverrideret / getorigret; these are implemented through the
	//  SourceHook::MacroRefHelpers structs.
	//  Furthermore, SetOverrideRet had to be changed a bit; see SourceHook::OverrideFunctor somewhere down in this file.
	template <class T> struct ReferenceCarrier
	{
		typedef T type;
	};

	template <class T> struct ReferenceCarrier<T&>
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
		};
	};

	template <class T> struct ReferenceUtil
	{
		typedef T plain_type;
		typedef T* pointer_type;
		typedef T& reference_type;
	};

	template <class T> struct ReferenceUtil<T&>
	{
		typedef T plain_type;
		typedef T* pointer_type;
		typedef T& reference_type;
	};

	struct IHookContext
	{
		virtual ISHDelegate *GetNext() = 0;
		virtual void *GetOverrideRetPtr() = 0;
		virtual const void *GetOrigRetPtr() = 0;
		virtual bool ShouldCallOrig() = 0;
	};

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
		*	@brief Modes for the new AddHook
		*/
		enum AddHookMode
		{
			Hook_Normal,
			Hook_VP,
			Hook_DVP
		}; 

		/**
		*	@brief Add a (VP) hook.
		*
		*	@return non-zero hook id on success, 0 otherwise
		*
		*	@param plug The unique identifier of the plugin that calls this function
		*	@param mode	Can be either Hook_Normal or Hook_VP (vtable-wide hook)
		*	@param iface The interface pointer
		*	@param ifacesize The size of the class iface points to
		*	@param myHookMan A hook manager function that should be capable of handling the function
		*	@param handler A pointer to the hook handler something
		*	@param post Set to true if you want a post handler
		*/
		virtual int AddHook(Plugin plug, AddHookMode mode, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan,
			ISHDelegate *handler, bool post) = 0;

		// Source backwarts compat (only for normal hooks)
		virtual bool RemoveHook(Plugin plug, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan,
			ISHDelegate *handler, bool post) = 0;

		/**
		*	@brief Remove a hook by ID.
		*
		*	@return true on success, false otherwise
		*
		*	@param hookid The hook id (returned by AddHook)
		*/
		virtual bool RemoveHookByID(int hookid) = 0;

		/**
		*	@brief Pause a hook by ID.
		*
		*	@return true on success, false otherwise
		*
		*	@param hookid The hook id (returned by AddHook)
		*/
		virtual bool PauseHookByID(int hookid) = 0;

		/**
		*	@brief Unpause a hook by ID.
		*
		*	@return true on success, false otherwise
		*
		*	@param hookid The hook id (returned by AddHook)
		*/
		virtual bool UnpauseHookByID(int hookid) = 0;

		/**
		*	@brief Checks whether a plugin has (a) hook manager(s) that is/are currently used by other plugins
		*
		*	@param plug The unique identifier of the plugin in question
		*/

		virtual void SetRes(META_RES res) = 0;				//!< Sets the meta result
		virtual META_RES GetPrevRes() = 0;					//!< Gets the meta result of the
															//!<  previously calledhandler
		virtual META_RES GetStatus() = 0;					//!< Gets the highest meta result
		virtual const void *GetOrigRet() = 0;				//!< Gets the original result.
															//!<  If not in post function, undefined
		virtual const void *GetOverrideRet() = 0;			//!< Gets the override result.
															//!<  If none is specified, NULL
		virtual void *GetIfacePtr() = 0;					//!< Gets the interface pointer

		virtual void *GetOverrideRetPtr() = 0;				//!< Used for setting the override return value

		/**
		*	@brief Remove a hook manager. Auto-removes all hooks attached to it from plugin plug.
		*
		*	@param plug The owner of the hook manager
		*   @param pubFunc The hook manager's info function
		*/
		virtual void RemoveHookManager(Plugin plug, HookManagerPubFunc pubFunc) = 0;

		// For SH_CALL:

		/**
		*	@brief Makes sure that hooks are going to be ignored on the next call of vfnptr
		*
		*	@param plug The unique identifier of the plugin that calls this function
		*	@param vfnptr The virtual function pointer of the function in question
		*/
		virtual void SetIgnoreHooks(void *vfnptr) = 0;

		/**
		*	@brief Reverses SetIgnoreHooks' effect
		*
		*	@param plug The unique identifier of the plugin that calls this function
		*	@param vfnptr The virtual function pointer of the function in question
		*/
		virtual void ResetIgnoreHooks(void *vfnptr) = 0;

		/**
		*	@brief Finds the original entry of a virtual function pointer
		*
		*	@param vfnptr The virtual function pointer
		*	@return The original entry if the virtual function pointer has been patched; NULL otherwise.
		*/
		virtual void *GetOrigVfnPtrEntry(void *vfnptr) = 0;

		//////////////////////////////////////////////////////////////////////////

		// For hook managers	

		// Returns the current iface ptr
		virtual void DoRecall() = 0;							//!< Initiates a recall sequence

		/*
			HOW RECALLS WORK:
			
			The problem:
				Users want the ability to change parameters of the called function
				from inside their handler.
			The solution:
				1) Mark as "recall"
				2) Recall the function
				3) => SH's hook func gets called:
				  4) The first iterator points at the first hook the last hookfunc didn't execute yet
				  5) does all iteration and returns normally
		        6) The user's handler returns immediately
				7) The hook func returns immediately as well

				Also note that the recalled hookfunc starts with the status the recalling hookfunc
				ended with. The last handler (doing the recall) is also able to specify its own
				META_RES.
		*/

		/**
		*	@brief Set up the hook loop.
		*
		*	@param statusPtr		pointer to status variable
		*	@param prevResPtr		pointer to previous result variable
		*	@param curResPtr		pointer to current result variable
		*	@param ifacePtrPtr		pointer to interface this pointer variable
		*	@param origRetPr		pointer to original return value variable. NULL for void funcs
		*	@param overrideRetPtr	pointer to override return value variable. NULL for void funcs
		*	
		*	@return Override Return Pointer the hookfunc should use (may differ from overrideRetPtr
		*		when the hook func is being called as part of a recall
		*/
		virtual IHookContext *SetupHookLoop(IHookManagerInfo *hi, void *vfnptr, void *thisptr, void **origCallAddr,
			META_RES *statusPtr, META_RES *prevResPtr, META_RES *curResPtr,
			const void *origRetPtr, void *overrideRetPtr) = 0;

		virtual void EndContext(IHookContext *pCtx) = 0;
	};


	class IHookManagerAutoGen
	{
	public:
		virtual int GetIfaceVersion() = 0;
		virtual int GetImplVersion() = 0;
		
		virtual HookManagerPubFunc MakeHookMan(const ProtoInfo *proto, int vtbl_offs, int vtbl_idx) = 0;
		virtual void ReleaseHookMan(HookManagerPubFunc pubFunc) = 0;
	};
	
	// For META_RESULT_ORIG_RET and META_RESULT_OVERRIDE_RET:
	//  These have to be able to return references. If T is a reference, the pointers returned
	//  from the SH_GLOB_SHPTR are pointers to instances of ReferenceCarrier<T>::type.
	template <class T> struct MacroRefHelpers
	{
		inline static const T* GetOrigRet(ISourceHook *shptr)
		{
			return reinterpret_cast<const T*>(shptr->GetOrigRet());
		}
		inline static const T* GetOverrideRet(ISourceHook *shptr)
		{
			return reinterpret_cast<const T*>(shptr->GetOverrideRet());
		}
	};

	template <class T> struct MacroRefHelpers<T&>
	{
		inline static T* GetOrigRet(ISourceHook *shptr)
		{
			T &ref = *reinterpret_cast<const typename ReferenceCarrier<T&>::type *>(shptr->GetOrigRet());
			return &ref;
		}
		inline static T* GetOverrideRet(ISourceHook *shptr)
		{
			T &ref = *reinterpret_cast<const typename ReferenceCarrier<T&>::type *>(shptr->GetOverrideRet());
			return &ref;
		}
	};

	template <class X, class MFP>
		void *GetOrigVfnPtrEntry(X *pInstance, MFP mfp, ISourceHook *pSH)
	{
		SourceHook::MemFuncInfo info = {true, -1, 0, 0};
		SourceHook::GetFuncInfo(pInstance, mfp, info);

		void *vfnptr = reinterpret_cast<void*>(
			*reinterpret_cast<void***>(reinterpret_cast<char*>(pInstance) + info.thisptroffs + info.vtbloffs) + info.vtblindex);

		void *origentry = pSH->GetOrigVfnPtrEntry(vfnptr);

		return origentry ? origentry : *reinterpret_cast<void**>(vfnptr);
	}

	template <class T> struct GetPassInfo
	{
		static const int type = 0;
		static const unsigned int flags = PassInfo::PassFlag_ByVal;
	};

	template <class T> struct GetPassInfo<T&>
	{
		static const int type = 0;
		static const unsigned int flags = PassInfo::PassFlag_ByRef;
	};
}

/************************************************************************/
/* High level interface                                                 */
/************************************************************************/
#define META_RESULT_STATUS					SH_GLOB_SHPTR->GetStatus()
#define META_RESULT_PREVIOUS				SH_GLOB_SHPTR->GetPrevRes()
#define META_RESULT_ORIG_RET(type)			*SourceHook::MacroRefHelpers<type>::GetOrigRet(SH_GLOB_SHPTR)
#define META_RESULT_OVERRIDE_RET(type)		*SourceHook::MacroRefHelpers<type>::GetOverrideRet(SH_GLOB_SHPTR)
#define META_IFACEPTR(type)					reinterpret_cast<type*>(SH_GLOB_SHPTR->GetIfacePtr())

#define SET_META_RESULT(result)				SH_GLOB_SHPTR->SetRes(result)
#define RETURN_META(result)					do { SET_META_RESULT(result); return; } while(0)
#define RETURN_META_VALUE(result, value)	do { SET_META_RESULT(result); return (value); } while(0)


template<class T>
SourceHook::CallClass<T> *SH_GET_CALLCLASS(T *p)
{
	return static_cast< SourceHook::CallClass<T> *>(p);
}
#define SH_RELEASE_CALLCLASS(p)

#define SH_GET_MCALLCLASS(p, size) reinterpret_cast<void*>(p)

// only call these from the hook handlers directly!

#define MAKE_NOREF_VALUE(rettype) \
	*reinterpret_cast< ::SourceHook::ReferenceUtil<rettype>::pointer_type >(0)

// If a hook on a function which returns a reference does not want to specify a return value,
// it can use this macro.
//   ONLY USE THIS WITH MRES_IGNORED AND MRES_HANDLED !!!
#define RETURN_META_NOREF(result, rettype) \
	RETURN_META_VALUE(result, MAKE_NOREF_VALUE(rettype))

// Why take a memfuncptr instead of iface and func when we have to deduce the iface anyway now?
// Well, without it, there'd be no way to specify which overloaded version we want in _VALUE

// SourceHook::SetOverrideRet is defined later.
#define RETURN_META_NEWPARAMS(result, memfuncptr, newparams) \
	do { \
		SET_META_RESULT(result); \
		SH_GLOB_SHPTR->DoRecall(); \
		(SourceHook::RecallGetIface(SH_GLOB_SHPTR, memfuncptr)->*(memfuncptr)) newparams; \
		RETURN_META(MRES_SUPERCEDE); \
	} while (0)

#define RETURN_META_VALUE_NEWPARAMS(result, value, memfuncptr, newparams) \
	do { \
		SET_META_RESULT(result); \
		SH_GLOB_SHPTR->DoRecall(); \
		if ((result) >= MRES_OVERRIDE) \
		{ \
			/* meh, set the override result here because we don't get a chance to return */ \
			/* before continuing the hook loop through the recall */ \
			SourceHook::SetOverrideResult(memfuncptr)(SH_GLOB_SHPTR, value); \
		} \
		RETURN_META_VALUE(MRES_SUPERCEDE, \
			(SourceHook::RecallGetIface(SH_GLOB_SHPTR, memfuncptr)->*(memfuncptr)) newparams); \
	} while (0)

// :TODO: thisptroffs in MNEWPARAMS ??

#if SH_COMP == SH_COMP_MSVC

#define SOUREHOOK__MNEWPARAMS_PREPAREMFP(hookname) \
	union \
	{ \
		SH_MFHCls(hookname)::ECMFP mfp; \
		void *addr; \
	} u; \
	u.addr = (*reinterpret_cast<void***>(reinterpret_cast<char*>(thisptr) + SH_MFHCls(hookname)::ms_MFI.vtbloffs))[ \
		SH_MFHCls(hookname)::ms_MFI.vtblindex];

#elif SH_COMP == SH_COMP_GCC

#define SOUREHOOK__MNEWPARAMS_PREPAREMFP(hookname) \
	union \
	{ \
		SH_MFHCls(hookname)::ECMFP mfp; \
		struct \
		{ \
			void *addr; \
			intptr_t adjustor; \
		} s; \
	} u; \
	u.s.addr = (*reinterpret_cast<void***>(reinterpret_cast<char*>(thisptr) + SH_MFHCls(hookname)::ms_MFI.vtbloffs))[ \
		SH_MFHCls(hookname)::ms_MFI.vtblindex]; \
	u.s.adjustor = 0;

#endif

#define RETURN_META_MNEWPARAMS(result, hookname, newparams) \
	do { \
		SET_META_RESULT(result); \
		SH_GLOB_SHPTR->DoRecall(); \
		SourceHook::EmptyClass *thisptr = reinterpret_cast<SourceHook::EmptyClass*>(SH_GLOB_SHPTR->GetIfacePtr()); \
		(thisptr->*(__SoureceHook_FHM_GetRecallMFP##hookname(thisptr))) newparams; \
		RETURN_META(MRES_SUPERCEDE); \
	} while (0)

#define RETURN_META_VALUE_MNEWPARAMS(result, value, hookname, newparams) \
	do { \
		SET_META_RESULT(result); \
		SH_GLOB_SHPTR->DoRecall(); \
		if ((result) >= MRES_OVERRIDE) \
		{ \
			/* see RETURN_META_VALUE_NEWPARAMS */ \
			__SoureceHook_FHM_SetOverrideResult##hookname(SH_GLOB_SHPTR, value); \
		} \
		SourceHook::EmptyClass *thisptr = reinterpret_cast<SourceHook::EmptyClass*>(SH_GLOB_SHPTR->GetIfacePtr()); \
		RETURN_META_VALUE(MRES_SUPERCEDE, (thisptr->*(__SoureceHook_FHM_GetRecallMFP##hookname(thisptr))) newparams); \
	} while (0)


#define SH_MANUALHOOK_RECONFIGURE(hookname, pvtblindex, pvtbloffs, pthisptroffs) \
	__SourceHook_FHM_Reconfigure##hookname(pvtblindex, pvtbloffs, pthisptroffs)

#define SH_GET_ORIG_VFNPTR_ENTRY(inst, mfp) (SourceHook::GetOrigVfnPtrEntry(inst, mfp, SH_GLOB_SHPTR))

// New ADD / REMOVE macros.
#define SH_STATIC(func) fastdelegate::MakeDelegate(func)
#define SH_MEMBER(inst, func) fastdelegate::MakeDelegate(inst, func)

#define SH_ADD_HOOK(ifacetype, ifacefunc, ifaceptr, handler, post) \
	__SourceHook_FHAdd##ifacetype##ifacefunc((void*)SourceHook::implicit_cast<ifacetype*>(ifaceptr), \
	SourceHook::ISourceHook::Hook_Normal, post, handler)

#define SH_REMOVE_HOOK(ifacetype, ifacefunc, ifaceptr, handler, post) \
	__SourceHook_FHRemove##ifacetype##ifacefunc((void*)SourceHook::implicit_cast<ifacetype*>(ifaceptr), \
	post, handler) 

#define SH_ADD_MANUALHOOK(hookname, ifaceptr, handler, post) \
	__SourceHook_FHMAdd##hookname(reinterpret_cast<void*>(ifaceptr), \
	SourceHook::ISourceHook::Hook_Normal, post, handler)

#define SH_REMOVE_MANUALHOOK(hookname, ifaceptr, handler, post) \
	__SourceHook_FHMRemove##hookname(reinterpret_cast<void*>(ifaceptr), post, handler) 

#define SH_ADD_VPHOOK(ifacetype, ifacefunc, ifaceptr, handler, post) \
	__SourceHook_FHAdd##ifacetype##ifacefunc((void*)SourceHook::implicit_cast<ifacetype*>(ifaceptr), \
	SourceHook::ISourceHook::Hook_VP, post, handler)

#define SH_ADD_DVPHOOK(ifacetype, ifacefunc, ifaceptr, handler, post) \
	__SourceHook_FHAdd##ifacetype##ifacefunc((void*)SourceHook::implicit_cast<ifacetype*>(ifaceptr), \
	SourceHook::ISourceHook::Hook_DVP, post, handler)

#define SH_ADD_MANUALVPHOOK(hookname, ifaceptr, handler, post) \
	__SourceHook_FHMAdd##hookname(reinterpret_cast<void*>(ifaceptr), SourceHook::ISourceHook::Hook_VP, post, handler) 

#define SH_ADD_MANUALDVPHOOK(hookname, ifaceptr, handler, post) \
	__SourceHook_FHMAdd##hookname(reinterpret_cast<void*>(ifaceptr), SourceHook::ISourceHook::Hook_DVP, post, handler) 

#define SH_REMOVE_HOOK_ID(hookid) \
	(SH_GLOB_SHPTR->RemoveHookByID(hookid))

// Old macros
// !! These are now deprecated. Instead, use one of these:
//  SH_ADD_HOOK(ifacetype, ifacefunc, ifaceptr, SH_STATIC(handler), post)
//  SH_ADD_HOOK(ifacetype, ifacefunc, ifaceptr, SH_MEMBER(inst, func), post)

#define SH_ADD_HOOK_STATICFUNC(ifacetype, ifacefunc, ifaceptr, handler, post) \
	SH_ADD_HOOK(ifacetype, ifacefunc, ifaceptr, SH_STATIC(handler), post)
#define SH_ADD_HOOK_MEMFUNC(ifacetype, ifacefunc, ifaceptr, handler_inst, handler_func, post) \
	SH_ADD_HOOK(ifacetype, ifacefunc, ifaceptr, SH_MEMBER(handler_inst, handler_func), post)

//  SH_REMOVE_HOOK(ifacetype, ifacefunc, ifaceptr, SH_STATIC(handler), post)
//  SH_REMOVE_HOOK(ifacetype, ifacefunc, ifaceptr, SH_MEMBER(inst, func), post)

#define SH_REMOVE_HOOK_STATICFUNC(ifacetype, ifacefunc, ifaceptr, handler, post) \
	SH_REMOVE_HOOK(ifacetype, ifacefunc, ifaceptr, SH_STATIC(handler), post)
#define SH_REMOVE_HOOK_MEMFUNC(ifacetype, ifacefunc, ifaceptr, handler_inst, handler_func, post) \
	SH_REMOVE_HOOK(ifacetype, ifacefunc, ifaceptr, SH_MEMBER(handler_inst, handler_func), post)

//  SH_ADD_MANUALHOOK(hookname, ifaceptr, SH_STATIC(handler), post)
//  SH_ADD_MANUALHOOK(hookname, ifaceptr, SH_MEMBER(inst, func), post)

#define SH_ADD_MANUALHOOK_STATICFUNC(hookname, ifaceptr, handler, post) \
	SH_ADD_MANUALHOOK(hookname, ifaceptr, SH_STATIC(handler), post)
#define SH_ADD_MANUALHOOK_MEMFUNC(hookname, ifaceptr, handler_inst, handler_func, post) \
	SH_ADD_MANUALHOOK(hookname, ifaceptr, SH_MEMBER(handler_inst, handler_func), post)

//  SH_REMOVE_MANUALHOOK(hookname, ifaceptr, SH_STATIC(handler), post)
//  SH_REMOVE_MANUALHOOK(hookname, ifaceptr, SH_MEMBER(inst, func), post)

#define SH_REMOVE_MANUALHOOK_STATICFUNC(hookname, ifaceptr, handler, post) \
	SH_REMOVE_MANUALHOOK(hookname, ifaceptr, SH_STATIC(handler), post)
#define SH_REMOVE_MANUALHOOK_MEMFUNC(hookname, ifaceptr, handler_inst, handler_func, post) \
	SH_REMOVE_MANUALHOOK(hookname, ifaceptr, SH_MEMBER(handler_inst, handler_func), post)  

#define SH_NOATTRIB




#if SH_COMP == SH_COMP_MSVC
# define SH_SETUP_MFP(mfp) \
	reinterpret_cast<void**>(&mfp)[0] = vfnptr_origentry;
#elif SH_COMP == SH_COMP_GCC
# define SH_SETUP_MFP(mfp) \
	reinterpret_cast<void**>(&mfp)[0] = vfnptr_origentry; \
	reinterpret_cast<void**>(&mfp)[1] = 0;
#else
# error Not supported yet.
#endif

//////////////////////////////////////////////////////////////////////////
#define SH_FHCls(ift, iff, ov) __SourceHook_FHCls_##ift##iff##ov
#define SH_MFHCls(hookname) __SourceHook_MFHCls_##hookname

#define SHINT_MAKE_HOOKMANPUBFUNC(ifacetype, ifacefunc, overload, funcptr) \
	static int HookManPubFunc(bool store, ::SourceHook::IHookManagerInfo *hi) \
	{ \
		using namespace ::SourceHook; \
		GetFuncInfo(funcptr, ms_MFI); \
		/* Verify interface version */ \
		if (SH_GLOB_SHPTR->GetIfaceVersion() != SH_IFACE_VERSION) \
			return 1; \
		if (SH_GLOB_SHPTR->GetImplVersion() < SH_IMPL_VERSION) \
			return 1; \
		if (store) \
			ms_HI = hi; \
		if (hi) \
		{ \
			MemFuncInfo mfi = {true, -1, 0, 0}; \
			GetFuncInfo(&SH_FHCls(ifacetype,ifacefunc,overload)::Func, mfi); \
			hi->SetInfo(SH_HOOKMAN_VERSION, ms_MFI.vtbloffs, ms_MFI.vtblindex, &ms_Proto, \
				reinterpret_cast<void**>(reinterpret_cast<char*>(&ms_Inst) + mfi.vtbloffs)[mfi.vtblindex] ); \
		} \
		return 0; \
	}

// It has to be possible to use the macros in namespaces
// -> So we need to access and extend the global SourceHook namespace
// We use a namespace alias for this
#define SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, funcptr) \
	struct SH_FHCls(ifacetype,ifacefunc,overload) \
	{ \
		static SH_FHCls(ifacetype,ifacefunc,overload) ms_Inst; \
		static ::SourceHook::MemFuncInfo ms_MFI; \
		static ::SourceHook::IHookManagerInfo *ms_HI; \
		static ::SourceHook::ProtoInfo ms_Proto; \
		SHINT_MAKE_HOOKMANPUBFUNC(ifacetype, ifacefunc, overload, funcptr)

#define SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, funcptr) \
	}; \
	SH_FHCls(ifacetype,ifacefunc,overload) SH_FHCls(ifacetype,ifacefunc,overload)::ms_Inst; \
	::SourceHook::MemFuncInfo SH_FHCls(ifacetype,ifacefunc,overload)::ms_MFI; \
	::SourceHook::IHookManagerInfo *SH_FHCls(ifacetype,ifacefunc,overload)::ms_HI; \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		SH_FHCls(ifacetype,ifacefunc,overload)::FD handler) \
	{ \
		using namespace ::SourceHook; \
		MemFuncInfo mfi = {true, -1, 0, 0}; \
		GetFuncInfo(funcptr, mfi); \
		if (mfi.thisptroffs < 0 || !mfi.isVirtual) \
			return false; /* No non-virtual functions / virtual inheritance supported */ \
		\
		return SH_GLOB_SHPTR->AddHook(SH_GLOB_PLUGPTR, mode, \
			iface, mfi.thisptroffs, SH_FHCls(ifacetype,ifacefunc,overload)::HookManPubFunc, \
			new SH_FHCls(ifacetype,ifacefunc,overload)::CMyDelegateImpl(handler), post); \
	} \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		SH_FHCls(ifacetype,ifacefunc,overload)::FD handler) \
	{ \
		using namespace ::SourceHook; \
		MemFuncInfo mfi = {true, -1, 0, 0}; \
		GetFuncInfo(funcptr, mfi); \
		SH_FHCls(ifacetype,ifacefunc,overload)::CMyDelegateImpl tmp(handler); \
		return SH_GLOB_SHPTR->RemoveHook(SH_GLOB_PLUGPTR, iface, mfi.thisptroffs, \
			SH_FHCls(ifacetype,ifacefunc,overload)::HookManPubFunc, &tmp, post); \
	} \

#define SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, pvtbloffs, pvtblidx, pthisptroffs) \
	struct SH_MFHCls(hookname) \
	{ \
		static SH_MFHCls(hookname) ms_Inst; \
		static ::SourceHook::MemFuncInfo ms_MFI; \
		static ::SourceHook::IHookManagerInfo *ms_HI; \
		static ::SourceHook::ProtoInfo ms_Proto; \
		\
		SH_MFHCls(hookname)() \
		{ \
			ms_MFI.isVirtual = true; \
			ms_MFI.thisptroffs = pthisptroffs; \
			ms_MFI.vtblindex = pvtblidx; \
			ms_MFI.vtbloffs = pvtbloffs; \
		} \
		static int HookManPubFunc(bool store, ::SourceHook::IHookManagerInfo *hi) \
		{ \
			using namespace ::SourceHook; \
			/* Verify interface version */ \
			if (SH_GLOB_SHPTR->GetIfaceVersion() != SH_IFACE_VERSION) \
				return 1; \
			if (SH_GLOB_SHPTR->GetImplVersion() < SH_IMPL_VERSION) \
				return 1; \
			if (store) \
				ms_HI = hi; \
			if (hi) \
			{ \
				MemFuncInfo mfi = {true, -1, 0, 0}; \
				GetFuncInfo(&SH_MFHCls(hookname)::Func, mfi); \
				hi->SetInfo(SH_HOOKMAN_VERSION, ms_MFI.vtbloffs, ms_MFI.vtblindex, &ms_Proto, \
					reinterpret_cast<void**>(reinterpret_cast<char*>(&ms_Inst) + mfi.vtbloffs)[mfi.vtblindex] ); \
			} \
			return 0; \
		}

#define SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, pvtbloffs, pvtblidx, pthisptroffs) \
	}; \
	SH_MFHCls(hookname) SH_MFHCls(hookname)::ms_Inst; \
	::SourceHook::MemFuncInfo SH_MFHCls(hookname)::ms_MFI; \
	::SourceHook::IHookManagerInfo *SH_MFHCls(hookname)::ms_HI; \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		SH_MFHCls(hookname)::FD handler) \
	{ \
		return SH_GLOB_SHPTR->AddHook(SH_GLOB_PLUGPTR, mode, \
			iface, pthisptroffs, SH_MFHCls(hookname)::HookManPubFunc, \
			new SH_MFHCls(hookname)::CMyDelegateImpl(handler), post); \
	} \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		SH_MFHCls(hookname)::FD handler) \
	{ \
		SH_MFHCls(hookname)::CMyDelegateImpl tmp(handler); \
		return SH_GLOB_SHPTR->RemoveHook(SH_GLOB_PLUGPTR, iface, pthisptroffs, \
			SH_MFHCls(hookname)::HookManPubFunc, &tmp, post); \
	} \
	SH_MFHCls(hookname)::ECMFP __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) \
	{ \
		SOUREHOOK__MNEWPARAMS_PREPAREMFP(hookname); \
		return u.mfp; \
	} \
	SH_MFHCls(hookname)::CallEC __SoureceHook_FHM_SHCall##hookname(void *ptr) \
	{ \
		SH_MFHCls(hookname)::ECMFP mfp; \
		void *vfnptr = reinterpret_cast<void*>( \
			*reinterpret_cast<void***>( (reinterpret_cast<char*>(ptr) + SH_MFHCls(hookname)::ms_MFI.thisptroffs + SH_MFHCls(hookname)::ms_MFI.vtbloffs) ) + SH_MFHCls(hookname)::ms_MFI.vtblindex); \
		/* patch mfp */ \
		*reinterpret_cast<void**>(&mfp) = *reinterpret_cast<void**>(vfnptr); \
		if (sizeof(mfp) == 2*sizeof(void*)) /* gcc */ \
			*(reinterpret_cast<void**>(&mfp) + 1) = 0; \
		return SH_MFHCls(hookname)::CallEC(reinterpret_cast< ::SourceHook::EmptyClass* >(ptr), mfp, vfnptr, SH_GLOB_SHPTR); \
	} \
	void __SourceHook_FHM_Reconfigure##hookname(int p_vtblindex, int p_vtbloffs, int p_thisptroffs) \
	{ \
		SH_GLOB_SHPTR->RemoveHookManager(SH_GLOB_PLUGPTR, SH_MFHCls(hookname)::HookManPubFunc); \
		SH_MFHCls(hookname)::ms_MFI.thisptroffs = p_thisptroffs; \
		SH_MFHCls(hookname)::ms_MFI.vtblindex = p_vtblindex; \
		SH_MFHCls(hookname)::ms_MFI.vtbloffs = p_vtbloffs; \
	}

#define SH_SETUPCALLS(rettype, paramtypes, params) \
	/* 1) Set up */ \
	using namespace ::SourceHook; \
	void *ourvfnptr = reinterpret_cast<void*>( \
		*reinterpret_cast<void***>(reinterpret_cast<char*>(this) + ms_MFI.vtbloffs) + ms_MFI.vtblindex); \
	void *vfnptr_origentry; \
	\
	META_RES status = MRES_IGNORED; \
	META_RES prev_res; \
	META_RES cur_res; \
	\
	typedef ReferenceCarrier< rettype >::type my_rettype; \
	my_rettype orig_ret; \
	my_rettype override_ret; \
	my_rettype plugin_ret; \
	IMyDelegate *iter; \
	IHookContext *pContext = SH_GLOB_SHPTR->SetupHookLoop(ms_HI, ourvfnptr, reinterpret_cast<void*>(this), \
		&vfnptr_origentry, &status, &prev_res, &cur_res, &orig_ret, &override_ret);

#define SH_CALL_HOOKS(post, params) \
	prev_res = MRES_IGNORED; \
	while ( (iter = static_cast<IMyDelegate*>(pContext->GetNext())) ) \
	{ \
		cur_res = MRES_IGNORED; \
		plugin_ret = iter->Call params; \
		prev_res = cur_res; \
		if (cur_res > status) \
			status = cur_res; \
		if (cur_res >= MRES_OVERRIDE) \
			*reinterpret_cast<my_rettype*>(pContext->GetOverrideRetPtr()) = plugin_ret; \
	}

#define SH_CALL_ORIG(rettype, paramtypes, params) \
	if (status != MRES_SUPERCEDE && pContext->ShouldCallOrig()) \
	{ \
		rettype (EmptyClass::*mfp)paramtypes; \
		SH_SETUP_MFP(mfp); \
		orig_ret = (reinterpret_cast<EmptyClass*>(this)->*mfp)params; \
	} \
	else \
		orig_ret = override_ret; /* :TODO: ??? : use pContext->GetOverrideRetPtr() or not? */

#define SH_RETURN() \
	const my_rettype *retptr = reinterpret_cast<const my_rettype*>( \
		(status >= MRES_OVERRIDE) ? pContext->GetOverrideRetPtr() : pContext->GetOrigRetPtr()); \
	SH_GLOB_SHPTR->EndContext(pContext); \
	return *retptr;

#define SH_HANDLEFUNC(paramtypes, params, rettype) \
	SH_SETUPCALLS(rettype, paramtypes, params) \
	SH_CALL_HOOKS(pre, params) \
	SH_CALL_ORIG(rettype, paramtypes, params) \
	SH_CALL_HOOKS(post, params) \
	SH_RETURN()

//////////////////////////////////////////////////////////////////////////
#define SH_SETUPCALLS_void(paramtypes, params) \
	/* 1) Set up */ \
	using namespace ::SourceHook; \
	void *ourvfnptr = reinterpret_cast<void*>( \
		*reinterpret_cast<void***>(reinterpret_cast<char*>(this) + ms_MFI.vtbloffs) + ms_MFI.vtblindex); \
	void *vfnptr_origentry; \
	\
	META_RES status = MRES_IGNORED; \
	META_RES prev_res; \
	META_RES cur_res; \
	\
	IMyDelegate *iter; \
	IHookContext *pContext = SH_GLOB_SHPTR->SetupHookLoop(ms_HI, ourvfnptr, reinterpret_cast<void*>(this), \
		&vfnptr_origentry, &status, &prev_res, &cur_res, NULL, NULL);

#define SH_CALL_HOOKS_void(post, params) \
	prev_res = MRES_IGNORED; \
	while ( (iter = static_cast<IMyDelegate*>(pContext->GetNext())) ) \
	{ \
		cur_res = MRES_IGNORED; \
		iter->Call params; \
		prev_res = cur_res; \
		if (cur_res > status) \
			status = cur_res; \
	}

#define SH_CALL_ORIG_void(paramtypes, params) \
	if (status != MRES_SUPERCEDE && pContext->ShouldCallOrig()) \
	{ \
		void (EmptyClass::*mfp)paramtypes; \
		SH_SETUP_MFP(mfp); \
		(reinterpret_cast<EmptyClass*>(this)->*mfp)params; \
	}

#define SH_RETURN_void() \
	SH_GLOB_SHPTR->EndContext(pContext);

#define SH_HANDLEFUNC_void(paramtypes, params) \
	SH_SETUPCALLS_void(paramtypes, params) \
	SH_CALL_HOOKS_void(pre, params) \
	SH_CALL_ORIG_void(paramtypes, params) \
	SH_CALL_HOOKS_void(post, params) \
	SH_RETURN_void()


// Special vafmt handlers
// :TODO: what
#define SH_HANDLEFUNC_vafmt(paramtypes, params_orig, params_plug, rettype) \
	SH_SETUPCALLS(rettype, paramtypes, params_orig) \
	SH_CALL_HOOKS(pre, params_plug) \
	SH_CALL_ORIG(rettype, paramtypes, params_orig) \
	SH_CALL_HOOKS(post, params_plug) \
	SH_RETURN()

#define SH_HANDLEFUNC_void_vafmt(paramtypes, params_orig, params_plug) \
	SH_SETUPCALLS_void(paramtypes, params_orig) \
	SH_CALL_HOOKS_void(pre, params_plug) \
	SH_CALL_ORIG_void(paramtypes, params_orig) \
	SH_CALL_HOOKS_void(post, params_plug) \
	SH_RETURN_void()

//////////////////////////////////////////////////////////////////////////

#define MAKE_DELEG(ret_type, params_decl, params_pass) \
	struct IMyDelegate : ::SourceHook::ISHDelegate { virtual ret_type Call params_decl = 0; }; \
	struct CMyDelegateImpl : IMyDelegate \
	{ \
		FD m_Deleg; \
		CMyDelegateImpl(FD deleg) : m_Deleg(deleg) {} \
		ret_type Call params_decl { return m_Deleg params_pass; } \
		void DeleteThis() { delete this; } \
		bool IsEqual(ISHDelegate *pOtherDeleg) { return m_Deleg == static_cast<CMyDelegateImpl*>(pOtherDeleg)->m_Deleg; } \
	};

#define MAKE_DELEG_void(params_decl, params_pass) \
	struct IMyDelegate : ::SourceHook::ISHDelegate { virtual void  Call params_decl = 0; }; \
	struct CMyDelegateImpl : IMyDelegate \
	{ \
		FD m_Deleg; \
		CMyDelegateImpl(FD deleg) : m_Deleg(deleg) {} \
		void Call params_decl { m_Deleg params_pass; } \
		void DeleteThis() { delete this; } \
		bool IsEqual(ISHDelegate *pOtherDeleg) { return m_Deleg == static_cast<CMyDelegateImpl*>(pOtherDeleg)->m_Deleg; } \
	};

// GetPassInfo -> easier access. __SH_GPI generates a SourceHook::PassInfo instance.
#define __SH_GPI(tt) { sizeof(tt), ::SourceHook::GetPassInfo< tt >::type, ::SourceHook::GetPassInfo< tt >::flags }
// Extra pass info V2
#define __SH_EPI { 0, 0, 0, 0 }

#define __SH_VAFMT_SEQ(fmt, buf) \
	char buf[::SourceHook::STRBUF_LEN]; \
	va_list ap; \
	va_start(ap, fmt); \
	vsnprintf(buf, sizeof(buf), fmt, ap); \
	buf[sizeof(buf) - 1] = 0; \
	va_end(ap);

@[$1,0,$a:
// ********* Support for $1 arguments *********
#define SH_DECL_HOOK$1(ifacetype, ifacefunc, attr, overload, rettype@[$2,1,$1:, param$2@]) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(@[$2,1,$1|, :param$2@]) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate$1<@[$2,1,$1|, :param$2@]@[$1!=0:, @]rettype> FD; \
		MAKE_DELEG(rettype, (@[$2,1,$1|, :param$2 p$2@]), (@[$2,1,$1|, :p$2@])); \
		virtual rettype Func(@[$2,1,$1|, :param$2 p$2@]) \
		{ SH_HANDLEFUNC((@[$2,1,$1|, :param$2@]), (@[$2,1,$1|, :p$2@]), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(@[$2,1,$1|, :param$2@]) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}@[$2,1,$1:, __SH_GPI(param$2)@] }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI@[$2,1,$1:, __SH_EPI@] }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { $1, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN$1(ifacetype, ifacefunc, attr, overload, rettype@[$2,1,$1:, param$2@]) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate$1<@[$2,1,$1|, :param$2@]@[$1!=0:, @]rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate$1<@[$2,1,$1|, :param$2@]@[$1!=0:, @]rettype> handler);

#define SH_DECL_HOOK$1_void(ifacetype, ifacefunc, attr, overload@[$2,1,$1:, param$2@]) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(@[$2,1,$1|, :param$2@]) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate$1<@[$2,1,$1|, :param$2@]> FD; \
		MAKE_DELEG_void((@[$2,1,$1|, :param$2 p$2@]), (@[$2,1,$1|, :p$2@])); \
		virtual void Func(@[$2,1,$1|, :param$2 p$2@]) \
		{ SH_HANDLEFUNC_void((@[$2,1,$1|, :param$2@]), (@[$2,1,$1|, :p$2@])); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(@[$2,1,$1|, :param$2@]) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}@[$2,1,$1:,__SH_GPI(param$2)@] }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI@[$2,1,$1:, __SH_EPI@] }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { $1, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN$1_void(ifacetype, ifacefunc, attr, overload@[$2,1,$1:, param$2@]) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate$1<@[$2,1,$1|, :param$2@]> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate$1<@[$2,1,$1|, :param$2@]> handler);

#define SH_DECL_HOOK$1_vafmt(ifacetype, ifacefunc, attr, overload, rettype@[$2,1,$1:, param$2@]) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(@[$2,1,$1|, :param$2@]@[$1!=0:, @]const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate@($1+1)<@[$2,1,$1|, :param$2@]@[$1!=0:, @]const char *, rettype> FD; \
		MAKE_DELEG(rettype, (@[$2,1,$1|, :param$2 p$2@]@[$1!=0:, @]const char *px), (@[$2,1,$1|, :p$2@]@[$1!=0:, @]px)); \
		virtual rettype Func(@[$2,1,$1|, :param$2 p$2@] @[$1!=0:, @]const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((@[$2,1,$1:param$2, @]const char *, ...), (@[$2,1,$1|, :p$2@]@[$1!=0:, @]"%s", buf), (@[$2,1,$1|, :p$2@]@[$1!=0:, @]buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(@[$2,1,$1|, :param$2@]@[$1!=0:, @]const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}@[$2,1,$1:, __SH_GPI(param$2)@] }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI@[$2,1,$1:, __SH_EPI@] }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { $1, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN$1_vafmt(ifacetype, ifacefunc, attr, overload, rettype@[$2,1,$1:, param$2@]) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate@($1+1)<@[$2,1,$1|, :param$2@]@[$1!=0:, @]const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate@($1+1)<@[$2,1,$1|, :param$2@]@[$1!=0:, @]const char *, rettype> handler);

#define SH_DECL_HOOK$1_void_vafmt(ifacetype, ifacefunc, attr, overload@[$2,1,$1:, param$2@]) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(@[$2,1,$1|, :param$2@]@[$1!=0:, @]const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate@($1+1)<@[$2,1,$1|, :param$2@]@[$1!=0:, @]const char *> FD; \
		MAKE_DELEG_void((@[$2,1,$1|, :param$2 p$2@]@[$1!=0:, @]const char *px), (@[$2,1,$1|, :p$2@]@[$1!=0:, @]px)); \
		virtual void Func(@[$2,1,$1|, :param$2 p$2@]@[$1!=0:, @]const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((@[$2,1,$1:param$2, @]const char *, ...), (@[$2,1,$1|, :p$2@]@[$1!=0:, @]"%s", buf), (@[$2,1,$1|, :p$2@]@[$1!=0:, @]buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(@[$2,1,$1|, :param$2@]@[$1!=0:, @]const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}@[$2,1,$1:, __SH_GPI(param$2)@] }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI@[$2,1,$1:, __SH_EPI@] }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { $1, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN$1_void_vafmt(ifacetype, ifacefunc, attr, overload@[$2,1,$1:, param$2@]) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate@($1+1)<@[$2,1,$1|, :param$2@]@[$1!=0:, @]const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate@($1+1)<@[$2,1,$1|, :param$2@]@[$1!=0:, @]const char *> handler);

#define SH_DECL_MANUALHOOK$1(hookname, vtblidx, vtbloffs, thisptroffs, rettype@[$2,1,$1:, param$2@]) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate$1<@[$2,1,$1|, :param$2@]@[$1!=0:, @]rettype> FD; \
		MAKE_DELEG(rettype, (@[$2,1,$1|, :param$2 p$2@]), (@[$2,1,$1|, :p$2@])); \
		virtual rettype Func(@[$2,1,$1|, :param$2 p$2@]) \
		{ SH_HANDLEFUNC((@[$2,1,$1|, :param$2@]), (@[$2,1,$1|, :p$2@]), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(@[$2,1,$1|, :param$2@]); \
		typedef SourceHook::ExecutableClass$1< ::SourceHook::EmptyClass, ECMFP, rettype@[$2,1,$1:, param$2@] > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}@[$2,1,$1:, __SH_GPI(param$2)@] }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI@[$2,1,$1:, __SH_EPI@] }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { $1, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN$1(hookname, rettype@[$2,1,$1:, param$2@]) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate$1<@[$2,1,$1:param$2, @]rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate$1<@[$2,1,$1:param$2, @]rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(@[$2,1,$1|, :param$2@]); \
	SourceHook::ExecutableClass$1<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(@[$2,1,$1|, :param$2@]), rettype@[$2,1,$1:, param$2@]> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK$1_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype@[$2,1,$1:, param$2@]) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate@($1+1)<@[$2,1,$1:param$2, @]const char *, rettype> FD; \
		MAKE_DELEG(rettype, (@[$2,1,$1:param$2 p$2, @]const char *px), (@[$2,1,$1:p$2, @]px)); \
		virtual rettype Func(@[$2,1,$1:param$2 p$2, @]const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((@[$2,1,$1:param$2, @]const char *, ...), (@[$2,1,$1|, :p$2@]@[$1!=0:, @]"%s", buf), (@[$2,1,$1|, :p$2@]@[$1!=0:, @]buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(@[$2,1,$1:param$2, @]const char *, ...); \
		typedef SourceHook::ExecutableClass@($1+1)< ::SourceHook::EmptyClass, ECMFP, rettype@[$2,1,$1:, param$2@], const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}@[$2,1,$1:, __SH_GPI(param$2)@] }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI@[$2,1,$1:, __SH_EPI@] }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { $1, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN$1_vafmt(hookname, rettype@[$2,1,$1:, param$2@]) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate@($1+1)<@[$2,1,$1:param$2, @]const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate@($1+1)<@[$2,1,$1:param$2, @]const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(@[$2,1,$1:param$2, @]const char *, ...); \
	SourceHook::ExecutableClass@($1+1)<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(@[$2,1,$1:param$2, @]const char *, ...), rettype@[$2,1,$1:, param$2@], const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK$1_void(hookname, vtblidx, vtbloffs, thisptroffs@[$2,1,$1:, param$2@]) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate$1<@[$2,1,$1|, :param$2@]> FD; \
		MAKE_DELEG_void((@[$2,1,$1|, :param$2 p$2@]), (@[$2,1,$1|, :p$2@])); \
		virtual void Func(@[$2,1,$1|, :param$2 p$2@]) \
		{ SH_HANDLEFUNC_void((@[$2,1,$1|, :param$2@]), (@[$2,1,$1|, :p$2@])); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(@[$2,1,$1|, :param$2@]); \
		typedef SourceHook::ExecutableClass$1<SourceHook::EmptyClass, ECMFP, void@[$2,1,$1:, param$2@]> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}@[$2,1,$1:, __SH_GPI(param$2)@] }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI@[$2,1,$1:, __SH_EPI@] }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { $1, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN$1_void(hookname@[$2,1,$1:, param$2@]) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate$1<@[$2,1,$1|, :param$2@]> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate$1<@[$2,1,$1|, :param$2@]> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(@[$2,1,$1|, :param$2@]); \
	SourceHook::ExecutableClass$1<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(@[$2,1,$1|, :param$2@]), void@[$2,1,$1:, param$2@]> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK$1_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs@[$2,1,$1:, param$2@]) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate@($1+1)<@[$2,1,$1:param$2, @]const char *> FD; \
		MAKE_DELEG_void((@[$2,1,$1:param$2 p$2, @]const char *px), (@[$2,1,$1:p$2, @]px)); \
		virtual void Func(@[$2,1,$1:param$2 p$2, @]const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((@[$2,1,$1:param$2, @]const char *, ...), (@[$2,1,$1|, :p$2@]@[$1!=0:, @]"%s", buf), (@[$2,1,$1|, :p$2@]@[$1!=0:, @]buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(@[$2,1,$1:param$2, @]const char *, ...); \
		typedef SourceHook::ExecutableClass@($1+1)< ::SourceHook::EmptyClass, ECMFP, void@[$2,1,$1:, param$2@], const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}@[$2,1,$1:, __SH_GPI(param$2)@] }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI@[$2,1,$1:, __SH_EPI@] }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { $1, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN$1_void_vafmt(hookname@[$2,1,$1:, param$2@]) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate@($1+1)<@[$2,1,$1:param$2, @]const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate@($1+1)<@[$2,1,$1:param$2, @]const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(@[$2,1,$1:param$2, @]const char *, ...); \
	SourceHook::ExecutableClass@($1+1)<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(@[$2,1,$1:param$2, @]const char *, ...), void@[$2,1,$1:, param$2@], const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);
@]


//////////////////////////////////////////////////////////////////////////
// SH_CALL

#define SH_MAKE_EXECUTABLECLASS_OB(call, prms) \
{ \
	using namespace ::SourceHook; \
	\
	m_pSH->SetIgnoreHooks(m_VfnPtr); \
	RetType tmpret = (m_ThisPtr->*m_MFP)call; \
	m_pSH->ResetIgnoreHooks(m_VfnPtr); \
	return tmpret; \
}

#define SH_MAKE_EXECUTABLECLASS_OB_void(call, prms) \
{ \
	using namespace ::SourceHook; \
	\
	m_pSH->SetIgnoreHooks(m_VfnPtr); \
	(m_ThisPtr->*m_MFP)call; \
	m_pSH->ResetIgnoreHooks(m_VfnPtr); \
}

namespace SourceHook
{
@[$1,0,$a:
	// Support for $1 arguments
	template<class ObjType, class MFPType, class RetType@[$2,1,$1:, class Param$2@]> class ExecutableClass$1
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass$1(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(@[$2,1,$1|, :Param$2 p$2@]) const
			SH_MAKE_EXECUTABLECLASS_OB((@[$2,1,$1|, :p$2@]), (@[$2,1,$1|, :Param$2@]))
	         
		@[$2,$1+1,$a:
		template <@[$3,$1+1,$2|, :class Param$3@]> RetType operator()(@[$3,1,$2|, :Param$3 p$3@]) const
			SH_MAKE_EXECUTABLECLASS_OB((@[$3,1,$2|, :p$3@]), (@[$3,1,$2|, :Param$3@]))
		@]
	};

	template<class ObjType, class MFPType@[$2,1,$1:, class Param$2@]> class ExecutableClass$1<ObjType, MFPType, void@[$2,1,$1:, Param$2@]>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass$1(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(@[$2,1,$1|, :Param$2 p$2@]) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((@[$2,1,$1|, :p$2@]), (@[$2,1,$1|, :Param$2@]))
	         
	   @[$2,$1+1,$a:
	   template <@[$3,$1+1,$2|, :class Param$3@]> void operator()(@[$3,1,$2|, :Param$3 p$3@]) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((@[$3,1,$2|, :p$3@]), (@[$3,1,$2|, :Param$3@]))
	   @]
	};
@]

}

#define SH__CALL_GET_VFNPTR_NORMAL \
	using namespace ::SourceHook; \
	MemFuncInfo mfi = {true, -1, 0, 0}; \
	GetFuncInfo(ptr, mfp, mfi); \
	void *vfnptr = reinterpret_cast<void*>( \
		*reinterpret_cast<void***>(reinterpret_cast<char*>(ptr) + mfi.thisptroffs + mfi.vtbloffs) + mfi.vtblindex);

#define SH__CALL_GET_VFNPTR_MANUAL \
	using namespace ::SourceHook; \
	void *vfnptr = reinterpret_cast<void*>( \
		*reinterpret_cast<void***>( (reinterpret_cast<char*>(ptr) + thisptroffs + vtbloffs) ) + vtblidx); \
	/* patch mfp */ \
	*reinterpret_cast<void**>(&mfp) = *reinterpret_cast<void**>(vfnptr);

// SH_CALL needs to deduce the return type -> it uses templates and function overloading
// That's why SH_CALL takes two parameters: "mfp2" of type RetType(X::*mfp)(params), and "mfp" of type MFP
// The only purpose of the mfp2 parameter is to extract the return type

@[$1,0,$a:
// Support for $1 arguments
template <class X, class Y, class MFP, class RetType@[$2,1,$1:, class Param$2@]>
SourceHook::ExecutableClass$1<Y, MFP, RetType@[$2,1,$1:, Param$2@]>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(@[$2,1,$1|, :Param$2@]), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass$1<Y, MFP, RetType@[$2,1,$1:, Param$2@]>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType@[$2,1,$1:, class Param$2@]>
SourceHook::ExecutableClass$1<Y, MFP, RetType@[$2,1,$1:, Param$2@]>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(@[$2,1,$1|, :Param$2@])const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass$1<Y, MFP, RetType@[$2,1,$1:, Param$2@]>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType@[$2,1,$1:, class Param$2@]>
SourceHook::ExecutableClass$1<SourceHook::EmptyClass, MFP, RetType@[$2,1,$1:, Param$2@]>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(@[$2,1,$1|, :Param$2@]), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass$1<EmptyClass, MFP, RetType@[$2,1,$1:, Param$2@]>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}
@]

#if SH_COMP != SH_COMP_MSVC || _MSC_VER > 1300
// GCC & MSVC 7.1 need this, MSVC 7.0 doesn't like it

@[$1,0,$a:
// Support for $1 arguments
template <class X, class Y, class MFP, class RetType@[$2,1,$1:, class Param$2@]>
SourceHook::ExecutableClass$1<Y, MFP, RetType@[$2,1,$1:, Param$2@]>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(@[$2,1,$1|, :Param$2@]@[$1!=0:, @]...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass$1<Y, MFP, RetType@[$2,1,$1:, Param$2@]>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType@[$2,1,$1:, class Param$2@]>
SourceHook::ExecutableClass$1<Y, MFP, RetType@[$2,1,$1:, Param$2@]>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(@[$2,1,$1|, :Param$2@]@[$1!=0:, @]...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass$1<Y, MFP, RetType@[$2,1,$1:, Param$2@]>(ptr, mfp, vfnptr, shptr);
}

@]

#endif

#define SH_CALL(ptr, mfp) SH_CALL2((ptr), (mfp), (mfp), SH_GLOB_SHPTR)
#define SH_MCALL2(ptr, mfp, vtblidx, vtbloffs, thisptroffs) SH_MCALL3((ptr), (mfp), (mfp), (vtblidx), (vtbloffs), (thisptroffs), SH_GLOB_SHPTR)
#define SH_MCALL(ptr, mhookname) __SoureceHook_FHM_SHCall##mhookname(ptr)

#undef SH_MAKE_EXECUTABLECLASS_OB
#undef SH_MAKE_EXECUTABLECLASS_OB_void

//////////////////////////////////////////////////////////////////////////
// SetOverrideRet and RecallGetIface for recalls
// These take a ISourceHook pointer instead of using SH_GLOB_SHPTR directly
// The reason is that the user may want to redefine SH_GLOB_SHPTR - then the macros
// (META_RETURN_VALUE_NEWPARAMS) should obey the new pointer.

namespace SourceHook
{
	// SetOverrideResult used to be implemented like this:
	//  SetOverrideResult(shptr, memfuncptr, return);
	//  normally the compiler can deduce the return type from memfuncptr, but (at least msvc8) failed when it was a reference
	//  (it thought it was ambigous - the ref and non-ref type)
	//  so now SetOverrideResult(memfuncptr) deduces the ret type, and returns a functor which does the work
	//  new syntax: SetOverrideResult(memfuncptr)(shptr, return)
	// This also allows us to create a special version for references which respects ReferenceCarrier.

	template <class T> struct OverrideFunctor
	{
		void operator()(ISourceHook *shptr, T res)
		{
			*reinterpret_cast<T*>(shptr->GetOverrideRetPtr()) = res;
		}
	};
	template <class T> struct OverrideFunctor<T&>
	{
		void operator()(ISourceHook *shptr, T &res)
		{
			// overrideretptr points to ReferenceCarrier<T&>
			*reinterpret_cast<typename ReferenceCarrier<T&>::type *>(shptr->GetOverrideRetPtr()) = res;
		}
	};

	// For manual hooks:
	// The rettype is passed in manually
	template <class RetType>
	OverrideFunctor<RetType> SetOverrideResult()
	{
		return OverrideFunctor<RetType>();
	}
@[$1,0,$a:
	template <class Iface, class RetType@[$2,1,$1:, class Param$2@]>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(@[$2,1,$1|, :Param$2@]))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType@[$2,1,$1:, class Param$2@]>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(@[$2,1,$1:Param$2, @]...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType@[$2,1,$1:, class Param$2@]>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(@[$2,1,$1|, :Param$2@]))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType@[$2,1,$1:, class Param$2@]>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(@[$2,1,$1:Param$2, @]...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}
@]
}

#endif
	// The pope is dead. -> :(
