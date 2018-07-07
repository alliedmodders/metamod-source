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


// ********* Support for 0 arguments *********
#define SH_DECL_HOOK0(ifacetype, ifacefunc, attr, overload, rettype) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)() attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate0<rettype> FD; \
		MAKE_DELEG(rettype, (), ()); \
		virtual rettype Func() \
		{ SH_HANDLEFUNC((), (), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)() attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0} }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 0, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN0(ifacetype, ifacefunc, attr, overload, rettype) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate0<rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate0<rettype> handler);

#define SH_DECL_HOOK0_void(ifacetype, ifacefunc, attr, overload) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)() attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate0<> FD; \
		MAKE_DELEG_void((), ()); \
		virtual void Func() \
		{ SH_HANDLEFUNC_void((), ()); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)() attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0} }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 0, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN0_void(ifacetype, ifacefunc, attr, overload) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate0<> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate0<> handler);

#define SH_DECL_HOOK0_vafmt(ifacetype, ifacefunc, attr, overload, rettype) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate1<const char *, rettype> FD; \
		MAKE_DELEG(rettype, (const char *px), (px)); \
		virtual rettype Func( const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((const char *, ...), ("%s", buf), (buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0} }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 0, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN0_vafmt(ifacetype, ifacefunc, attr, overload, rettype) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate1<const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate1<const char *, rettype> handler);

#define SH_DECL_HOOK0_void_vafmt(ifacetype, ifacefunc, attr, overload) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate1<const char *> FD; \
		MAKE_DELEG_void((const char *px), (px)); \
		virtual void Func(const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((const char *, ...), ("%s", buf), (buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0} }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 0, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN0_void_vafmt(ifacetype, ifacefunc, attr, overload) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate1<const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate1<const char *> handler);

#define SH_DECL_MANUALHOOK0(hookname, vtblidx, vtbloffs, thisptroffs, rettype) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate0<rettype> FD; \
		MAKE_DELEG(rettype, (), ()); \
		virtual rettype Func() \
		{ SH_HANDLEFUNC((), (), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(); \
		typedef SourceHook::ExecutableClass0< ::SourceHook::EmptyClass, ECMFP, rettype > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0} }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 0, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN0(hookname, rettype) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate0<rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate0<rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(); \
	SourceHook::ExecutableClass0<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(), rettype> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK0_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate1<const char *, rettype> FD; \
		MAKE_DELEG(rettype, (const char *px), (px)); \
		virtual rettype Func(const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((const char *, ...), ("%s", buf), (buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(const char *, ...); \
		typedef SourceHook::ExecutableClass1< ::SourceHook::EmptyClass, ECMFP, rettype, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0} }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 0, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN0_vafmt(hookname, rettype) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate1<const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate1<const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(const char *, ...); \
	SourceHook::ExecutableClass1<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(const char *, ...), rettype, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK0_void(hookname, vtblidx, vtbloffs, thisptroffs) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate0<> FD; \
		MAKE_DELEG_void((), ()); \
		virtual void Func() \
		{ SH_HANDLEFUNC_void((), ()); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(); \
		typedef SourceHook::ExecutableClass0<SourceHook::EmptyClass, ECMFP, void> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0} }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 0, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN0_void(hookname) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate0<> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate0<> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(); \
	SourceHook::ExecutableClass0<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(), void> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK0_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate1<const char *> FD; \
		MAKE_DELEG_void((const char *px), (px)); \
		virtual void Func(const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((const char *, ...), ("%s", buf), (buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(const char *, ...); \
		typedef SourceHook::ExecutableClass1< ::SourceHook::EmptyClass, ECMFP, void, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0} }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 0, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN0_void_vafmt(hookname) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate1<const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate1<const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(const char *, ...); \
	SourceHook::ExecutableClass1<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(const char *, ...), void, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 1 arguments *********
#define SH_DECL_HOOK1(ifacetype, ifacefunc, attr, overload, rettype, param1) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate1<param1, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1), (p1)); \
		virtual rettype Func(param1 p1) \
		{ SH_HANDLEFUNC((param1), (p1), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 1, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN1(ifacetype, ifacefunc, attr, overload, rettype, param1) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate1<param1, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate1<param1, rettype> handler);

#define SH_DECL_HOOK1_void(ifacetype, ifacefunc, attr, overload, param1) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate1<param1> FD; \
		MAKE_DELEG_void((param1 p1), (p1)); \
		virtual void Func(param1 p1) \
		{ SH_HANDLEFUNC_void((param1), (p1)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 1, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN1_void(ifacetype, ifacefunc, attr, overload, param1) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate1<param1> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate1<param1> handler);

#define SH_DECL_HOOK1_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate2<param1, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, const char *px), (p1, px)); \
		virtual rettype Func(param1 p1 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, const char *, ...), (p1, "%s", buf), (p1, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 1, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN1_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate2<param1, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate2<param1, const char *, rettype> handler);

#define SH_DECL_HOOK1_void_vafmt(ifacetype, ifacefunc, attr, overload, param1) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate2<param1, const char *> FD; \
		MAKE_DELEG_void((param1 p1, const char *px), (p1, px)); \
		virtual void Func(param1 p1, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, const char *, ...), (p1, "%s", buf), (p1, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 1, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN1_void_vafmt(ifacetype, ifacefunc, attr, overload, param1) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate2<param1, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate2<param1, const char *> handler);

#define SH_DECL_MANUALHOOK1(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate1<param1, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1), (p1)); \
		virtual rettype Func(param1 p1) \
		{ SH_HANDLEFUNC((param1), (p1), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1); \
		typedef SourceHook::ExecutableClass1< ::SourceHook::EmptyClass, ECMFP, rettype, param1 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 1, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN1(hookname, rettype, param1) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate1<param1, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate1<param1, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1); \
	SourceHook::ExecutableClass1<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1), rettype, param1> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK1_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate2<param1, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, const char *px), (p1, px)); \
		virtual rettype Func(param1 p1, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, const char *, ...), (p1, "%s", buf), (p1, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, const char *, ...); \
		typedef SourceHook::ExecutableClass2< ::SourceHook::EmptyClass, ECMFP, rettype, param1, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 1, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN1_vafmt(hookname, rettype, param1) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate2<param1, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate2<param1, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, const char *, ...); \
	SourceHook::ExecutableClass2<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, const char *, ...), rettype, param1, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK1_void(hookname, vtblidx, vtbloffs, thisptroffs, param1) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate1<param1> FD; \
		MAKE_DELEG_void((param1 p1), (p1)); \
		virtual void Func(param1 p1) \
		{ SH_HANDLEFUNC_void((param1), (p1)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1); \
		typedef SourceHook::ExecutableClass1<SourceHook::EmptyClass, ECMFP, void, param1> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 1, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN1_void(hookname, param1) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate1<param1> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate1<param1> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1); \
	SourceHook::ExecutableClass1<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1), void, param1> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK1_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate2<param1, const char *> FD; \
		MAKE_DELEG_void((param1 p1, const char *px), (p1, px)); \
		virtual void Func(param1 p1, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, const char *, ...), (p1, "%s", buf), (p1, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, const char *, ...); \
		typedef SourceHook::ExecutableClass2< ::SourceHook::EmptyClass, ECMFP, void, param1, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 1, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN1_void_vafmt(hookname, param1) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate2<param1, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate2<param1, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, const char *, ...); \
	SourceHook::ExecutableClass2<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, const char *, ...), void, param1, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 2 arguments *********
#define SH_DECL_HOOK2(ifacetype, ifacefunc, attr, overload, rettype, param1, param2) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate2<param1, param2, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2), (p1, p2)); \
		virtual rettype Func(param1 p1, param2 p2) \
		{ SH_HANDLEFUNC((param1, param2), (p1, p2), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 2, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN2(ifacetype, ifacefunc, attr, overload, rettype, param1, param2) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate2<param1, param2, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate2<param1, param2, rettype> handler);

#define SH_DECL_HOOK2_void(ifacetype, ifacefunc, attr, overload, param1, param2) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate2<param1, param2> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2), (p1, p2)); \
		virtual void Func(param1 p1, param2 p2) \
		{ SH_HANDLEFUNC_void((param1, param2), (p1, p2)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 2, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN2_void(ifacetype, ifacefunc, attr, overload, param1, param2) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate2<param1, param2> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate2<param1, param2> handler);

#define SH_DECL_HOOK2_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate3<param1, param2, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, const char *px), (p1, p2, px)); \
		virtual rettype Func(param1 p1, param2 p2 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, const char *, ...), (p1, p2, "%s", buf), (p1, p2, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 2, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN2_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate3<param1, param2, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate3<param1, param2, const char *, rettype> handler);

#define SH_DECL_HOOK2_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate3<param1, param2, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, const char *px), (p1, p2, px)); \
		virtual void Func(param1 p1, param2 p2, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, const char *, ...), (p1, p2, "%s", buf), (p1, p2, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 2, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN2_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate3<param1, param2, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate3<param1, param2, const char *> handler);

#define SH_DECL_MANUALHOOK2(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate2<param1, param2, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2), (p1, p2)); \
		virtual rettype Func(param1 p1, param2 p2) \
		{ SH_HANDLEFUNC((param1, param2), (p1, p2), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2); \
		typedef SourceHook::ExecutableClass2< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 2, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN2(hookname, rettype, param1, param2) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate2<param1, param2, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate2<param1, param2, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2); \
	SourceHook::ExecutableClass2<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2), rettype, param1, param2> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK2_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate3<param1, param2, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, const char *px), (p1, p2, px)); \
		virtual rettype Func(param1 p1, param2 p2, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, const char *, ...), (p1, p2, "%s", buf), (p1, p2, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, const char *, ...); \
		typedef SourceHook::ExecutableClass3< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 2, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN2_vafmt(hookname, rettype, param1, param2) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate3<param1, param2, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate3<param1, param2, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, const char *, ...); \
	SourceHook::ExecutableClass3<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, const char *, ...), rettype, param1, param2, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK2_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate2<param1, param2> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2), (p1, p2)); \
		virtual void Func(param1 p1, param2 p2) \
		{ SH_HANDLEFUNC_void((param1, param2), (p1, p2)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2); \
		typedef SourceHook::ExecutableClass2<SourceHook::EmptyClass, ECMFP, void, param1, param2> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 2, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN2_void(hookname, param1, param2) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate2<param1, param2> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate2<param1, param2> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2); \
	SourceHook::ExecutableClass2<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2), void, param1, param2> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK2_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate3<param1, param2, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, const char *px), (p1, p2, px)); \
		virtual void Func(param1 p1, param2 p2, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, const char *, ...), (p1, p2, "%s", buf), (p1, p2, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, const char *, ...); \
		typedef SourceHook::ExecutableClass3< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 2, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN2_void_vafmt(hookname, param1, param2) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate3<param1, param2, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate3<param1, param2, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, const char *, ...); \
	SourceHook::ExecutableClass3<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, const char *, ...), void, param1, param2, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 3 arguments *********
#define SH_DECL_HOOK3(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate3<param1, param2, param3, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3), (p1, p2, p3)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3) \
		{ SH_HANDLEFUNC((param1, param2, param3), (p1, p2, p3), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 3, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN3(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate3<param1, param2, param3, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate3<param1, param2, param3, rettype> handler);

#define SH_DECL_HOOK3_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate3<param1, param2, param3> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3), (p1, p2, p3)); \
		virtual void Func(param1 p1, param2 p2, param3 p3) \
		{ SH_HANDLEFUNC_void((param1, param2, param3), (p1, p2, p3)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 3, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN3_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate3<param1, param2, param3> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate3<param1, param2, param3> handler);

#define SH_DECL_HOOK3_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate4<param1, param2, param3, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, const char *px), (p1, p2, p3, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, const char *, ...), (p1, p2, p3, "%s", buf), (p1, p2, p3, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 3, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN3_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate4<param1, param2, param3, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate4<param1, param2, param3, const char *, rettype> handler);

#define SH_DECL_HOOK3_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate4<param1, param2, param3, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, const char *px), (p1, p2, p3, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, const char *, ...), (p1, p2, p3, "%s", buf), (p1, p2, p3, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 3, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN3_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate4<param1, param2, param3, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate4<param1, param2, param3, const char *> handler);

#define SH_DECL_MANUALHOOK3(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate3<param1, param2, param3, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3), (p1, p2, p3)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3) \
		{ SH_HANDLEFUNC((param1, param2, param3), (p1, p2, p3), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3); \
		typedef SourceHook::ExecutableClass3< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 3, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN3(hookname, rettype, param1, param2, param3) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate3<param1, param2, param3, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate3<param1, param2, param3, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3); \
	SourceHook::ExecutableClass3<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3), rettype, param1, param2, param3> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK3_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate4<param1, param2, param3, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, const char *px), (p1, p2, p3, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, const char *, ...), (p1, p2, p3, "%s", buf), (p1, p2, p3, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, const char *, ...); \
		typedef SourceHook::ExecutableClass4< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 3, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN3_vafmt(hookname, rettype, param1, param2, param3) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate4<param1, param2, param3, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate4<param1, param2, param3, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, const char *, ...); \
	SourceHook::ExecutableClass4<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, const char *, ...), rettype, param1, param2, param3, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK3_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate3<param1, param2, param3> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3), (p1, p2, p3)); \
		virtual void Func(param1 p1, param2 p2, param3 p3) \
		{ SH_HANDLEFUNC_void((param1, param2, param3), (p1, p2, p3)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3); \
		typedef SourceHook::ExecutableClass3<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 3, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN3_void(hookname, param1, param2, param3) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate3<param1, param2, param3> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate3<param1, param2, param3> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3); \
	SourceHook::ExecutableClass3<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3), void, param1, param2, param3> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK3_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate4<param1, param2, param3, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, const char *px), (p1, p2, p3, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, const char *, ...), (p1, p2, p3, "%s", buf), (p1, p2, p3, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, const char *, ...); \
		typedef SourceHook::ExecutableClass4< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 3, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN3_void_vafmt(hookname, param1, param2, param3) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate4<param1, param2, param3, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate4<param1, param2, param3, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, const char *, ...); \
	SourceHook::ExecutableClass4<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, const char *, ...), void, param1, param2, param3, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 4 arguments *********
#define SH_DECL_HOOK4(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate4<param1, param2, param3, param4, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4), (p1, p2, p3, p4)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4), (p1, p2, p3, p4), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 4, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN4(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate4<param1, param2, param3, param4, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate4<param1, param2, param3, param4, rettype> handler);

#define SH_DECL_HOOK4_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate4<param1, param2, param3, param4> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4), (p1, p2, p3, p4)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4), (p1, p2, p3, p4)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3),__SH_GPI(param4) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 4, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN4_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate4<param1, param2, param3, param4> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate4<param1, param2, param3, param4> handler);

#define SH_DECL_HOOK4_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate5<param1, param2, param3, param4, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, const char *px), (p1, p2, p3, p4, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, const char *, ...), (p1, p2, p3, p4, "%s", buf), (p1, p2, p3, p4, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 4, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN4_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate5<param1, param2, param3, param4, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate5<param1, param2, param3, param4, const char *, rettype> handler);

#define SH_DECL_HOOK4_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate5<param1, param2, param3, param4, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, const char *px), (p1, p2, p3, p4, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, const char *, ...), (p1, p2, p3, p4, "%s", buf), (p1, p2, p3, p4, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 4, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN4_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate5<param1, param2, param3, param4, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate5<param1, param2, param3, param4, const char *> handler);

#define SH_DECL_MANUALHOOK4(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate4<param1, param2, param3, param4, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4), (p1, p2, p3, p4)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4), (p1, p2, p3, p4), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4); \
		typedef SourceHook::ExecutableClass4< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 4, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN4(hookname, rettype, param1, param2, param3, param4) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate4<param1, param2, param3, param4, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate4<param1, param2, param3, param4, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4); \
	SourceHook::ExecutableClass4<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4), rettype, param1, param2, param3, param4> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK4_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate5<param1, param2, param3, param4, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, const char *px), (p1, p2, p3, p4, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, const char *, ...), (p1, p2, p3, p4, "%s", buf), (p1, p2, p3, p4, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, const char *, ...); \
		typedef SourceHook::ExecutableClass5< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 4, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN4_vafmt(hookname, rettype, param1, param2, param3, param4) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate5<param1, param2, param3, param4, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate5<param1, param2, param3, param4, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, const char *, ...); \
	SourceHook::ExecutableClass5<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, const char *, ...), rettype, param1, param2, param3, param4, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK4_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate4<param1, param2, param3, param4> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4), (p1, p2, p3, p4)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4), (p1, p2, p3, p4)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4); \
		typedef SourceHook::ExecutableClass4<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 4, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN4_void(hookname, param1, param2, param3, param4) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate4<param1, param2, param3, param4> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate4<param1, param2, param3, param4> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4); \
	SourceHook::ExecutableClass4<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4), void, param1, param2, param3, param4> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK4_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate5<param1, param2, param3, param4, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, const char *px), (p1, p2, p3, p4, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, const char *, ...), (p1, p2, p3, p4, "%s", buf), (p1, p2, p3, p4, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, const char *, ...); \
		typedef SourceHook::ExecutableClass5< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 4, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN4_void_vafmt(hookname, param1, param2, param3, param4) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate5<param1, param2, param3, param4, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate5<param1, param2, param3, param4, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, const char *, ...); \
	SourceHook::ExecutableClass5<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, const char *, ...), void, param1, param2, param3, param4, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 5 arguments *********
#define SH_DECL_HOOK5(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate5<param1, param2, param3, param4, param5, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5), (p1, p2, p3, p4, p5)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5), (p1, p2, p3, p4, p5), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 5, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN5(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate5<param1, param2, param3, param4, param5, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate5<param1, param2, param3, param4, param5, rettype> handler);

#define SH_DECL_HOOK5_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate5<param1, param2, param3, param4, param5> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5), (p1, p2, p3, p4, p5)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5), (p1, p2, p3, p4, p5)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3),__SH_GPI(param4),__SH_GPI(param5) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 5, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN5_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate5<param1, param2, param3, param4, param5> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate5<param1, param2, param3, param4, param5> handler);

#define SH_DECL_HOOK5_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, const char *px), (p1, p2, p3, p4, p5, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, const char *, ...), (p1, p2, p3, p4, p5, "%s", buf), (p1, p2, p3, p4, p5, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 5, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN5_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, const char *, rettype> handler);

#define SH_DECL_HOOK5_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, const char *px), (p1, p2, p3, p4, p5, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, const char *, ...), (p1, p2, p3, p4, p5, "%s", buf), (p1, p2, p3, p4, p5, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 5, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN5_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, const char *> handler);

#define SH_DECL_MANUALHOOK5(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate5<param1, param2, param3, param4, param5, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5), (p1, p2, p3, p4, p5)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5), (p1, p2, p3, p4, p5), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5); \
		typedef SourceHook::ExecutableClass5< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 5, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN5(hookname, rettype, param1, param2, param3, param4, param5) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate5<param1, param2, param3, param4, param5, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate5<param1, param2, param3, param4, param5, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5); \
	SourceHook::ExecutableClass5<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5), rettype, param1, param2, param3, param4, param5> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK5_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, const char *px), (p1, p2, p3, p4, p5, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, const char *, ...), (p1, p2, p3, p4, p5, "%s", buf), (p1, p2, p3, p4, p5, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, const char *, ...); \
		typedef SourceHook::ExecutableClass6< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 5, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN5_vafmt(hookname, rettype, param1, param2, param3, param4, param5) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, const char *, ...); \
	SourceHook::ExecutableClass6<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, const char *, ...), rettype, param1, param2, param3, param4, param5, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK5_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate5<param1, param2, param3, param4, param5> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5), (p1, p2, p3, p4, p5)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5), (p1, p2, p3, p4, p5)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5); \
		typedef SourceHook::ExecutableClass5<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 5, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN5_void(hookname, param1, param2, param3, param4, param5) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate5<param1, param2, param3, param4, param5> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate5<param1, param2, param3, param4, param5> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5); \
	SourceHook::ExecutableClass5<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5), void, param1, param2, param3, param4, param5> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK5_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, const char *px), (p1, p2, p3, p4, p5, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, const char *, ...), (p1, p2, p3, p4, p5, "%s", buf), (p1, p2, p3, p4, p5, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, const char *, ...); \
		typedef SourceHook::ExecutableClass6< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 5, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN5_void_vafmt(hookname, param1, param2, param3, param4, param5) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, const char *, ...); \
	SourceHook::ExecutableClass6<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, const char *, ...), void, param1, param2, param3, param4, param5, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 6 arguments *********
#define SH_DECL_HOOK6(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, param6, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6), (p1, p2, p3, p4, p5, p6)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6), (p1, p2, p3, p4, p5, p6), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 6, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN6(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, param6, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, param6, rettype> handler);

#define SH_DECL_HOOK6_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, param6> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6), (p1, p2, p3, p4, p5, p6)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6), (p1, p2, p3, p4, p5, p6)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3),__SH_GPI(param4),__SH_GPI(param5),__SH_GPI(param6) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 6, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN6_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, param6> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, param6> handler);

#define SH_DECL_HOOK6_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, const char *px), (p1, p2, p3, p4, p5, p6, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, const char *, ...), (p1, p2, p3, p4, p5, p6, "%s", buf), (p1, p2, p3, p4, p5, p6, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 6, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN6_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, const char *, rettype> handler);

#define SH_DECL_HOOK6_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, const char *px), (p1, p2, p3, p4, p5, p6, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, const char *, ...), (p1, p2, p3, p4, p5, p6, "%s", buf), (p1, p2, p3, p4, p5, p6, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 6, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN6_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, const char *> handler);

#define SH_DECL_MANUALHOOK6(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, param6, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6), (p1, p2, p3, p4, p5, p6)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6), (p1, p2, p3, p4, p5, p6), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6); \
		typedef SourceHook::ExecutableClass6< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 6, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN6(hookname, rettype, param1, param2, param3, param4, param5, param6) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, param6, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, param6, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6); \
	SourceHook::ExecutableClass6<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6), rettype, param1, param2, param3, param4, param5, param6> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK6_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, const char *px), (p1, p2, p3, p4, p5, p6, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, const char *, ...), (p1, p2, p3, p4, p5, p6, "%s", buf), (p1, p2, p3, p4, p5, p6, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, const char *, ...); \
		typedef SourceHook::ExecutableClass7< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 6, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN6_vafmt(hookname, rettype, param1, param2, param3, param4, param5, param6) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, const char *, ...); \
	SourceHook::ExecutableClass7<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, const char *, ...), rettype, param1, param2, param3, param4, param5, param6, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK6_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, param6> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6), (p1, p2, p3, p4, p5, p6)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6), (p1, p2, p3, p4, p5, p6)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6); \
		typedef SourceHook::ExecutableClass6<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 6, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN6_void(hookname, param1, param2, param3, param4, param5, param6) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, param6> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate6<param1, param2, param3, param4, param5, param6> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6); \
	SourceHook::ExecutableClass6<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6), void, param1, param2, param3, param4, param5, param6> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK6_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, const char *px), (p1, p2, p3, p4, p5, p6, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, const char *, ...), (p1, p2, p3, p4, p5, p6, "%s", buf), (p1, p2, p3, p4, p5, p6, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, const char *, ...); \
		typedef SourceHook::ExecutableClass7< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 6, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN6_void_vafmt(hookname, param1, param2, param3, param4, param5, param6) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, const char *, ...); \
	SourceHook::ExecutableClass7<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, const char *, ...), void, param1, param2, param3, param4, param5, param6, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 7 arguments *********
#define SH_DECL_HOOK7(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, param7, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7), (p1, p2, p3, p4, p5, p6, p7)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7), (p1, p2, p3, p4, p5, p6, p7), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 7, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN7(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, param7, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, param7, rettype> handler);

#define SH_DECL_HOOK7_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, param7> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7), (p1, p2, p3, p4, p5, p6, p7)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7), (p1, p2, p3, p4, p5, p6, p7)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3),__SH_GPI(param4),__SH_GPI(param5),__SH_GPI(param6),__SH_GPI(param7) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 7, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN7_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, param7> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, param7> handler);

#define SH_DECL_HOOK7_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, const char *px), (p1, p2, p3, p4, p5, p6, p7, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 7, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN7_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, const char *, rettype> handler);

#define SH_DECL_HOOK7_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, const char *px), (p1, p2, p3, p4, p5, p6, p7, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 7, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN7_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, const char *> handler);

#define SH_DECL_MANUALHOOK7(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, param7, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7), (p1, p2, p3, p4, p5, p6, p7)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7), (p1, p2, p3, p4, p5, p6, p7), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7); \
		typedef SourceHook::ExecutableClass7< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 7, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN7(hookname, rettype, param1, param2, param3, param4, param5, param6, param7) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, param7, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, param7, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7); \
	SourceHook::ExecutableClass7<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7), rettype, param1, param2, param3, param4, param5, param6, param7> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK7_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, const char *px), (p1, p2, p3, p4, p5, p6, p7, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, const char *, ...); \
		typedef SourceHook::ExecutableClass8< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 7, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN7_vafmt(hookname, rettype, param1, param2, param3, param4, param5, param6, param7) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, const char *, ...); \
	SourceHook::ExecutableClass8<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, const char *, ...), rettype, param1, param2, param3, param4, param5, param6, param7, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK7_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, param7> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7), (p1, p2, p3, p4, p5, p6, p7)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7), (p1, p2, p3, p4, p5, p6, p7)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7); \
		typedef SourceHook::ExecutableClass7<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 7, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN7_void(hookname, param1, param2, param3, param4, param5, param6, param7) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, param7> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate7<param1, param2, param3, param4, param5, param6, param7> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7); \
	SourceHook::ExecutableClass7<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7), void, param1, param2, param3, param4, param5, param6, param7> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK7_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, const char *px), (p1, p2, p3, p4, p5, p6, p7, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, const char *, ...); \
		typedef SourceHook::ExecutableClass8< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 7, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN7_void_vafmt(hookname, param1, param2, param3, param4, param5, param6, param7) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, const char *, ...); \
	SourceHook::ExecutableClass8<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, const char *, ...), void, param1, param2, param3, param4, param5, param6, param7, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 8 arguments *********
#define SH_DECL_HOOK8(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, param8, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8), (p1, p2, p3, p4, p5, p6, p7, p8)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8), (p1, p2, p3, p4, p5, p6, p7, p8), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 8, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN8(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, param8, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, param8, rettype> handler);

#define SH_DECL_HOOK8_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, param8> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8), (p1, p2, p3, p4, p5, p6, p7, p8)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8), (p1, p2, p3, p4, p5, p6, p7, p8)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3),__SH_GPI(param4),__SH_GPI(param5),__SH_GPI(param6),__SH_GPI(param7),__SH_GPI(param8) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 8, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN8_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, param8> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, param8> handler);

#define SH_DECL_HOOK8_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 8, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN8_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, const char *, rettype> handler);

#define SH_DECL_HOOK8_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 8, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN8_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, const char *> handler);

#define SH_DECL_MANUALHOOK8(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, param8, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8), (p1, p2, p3, p4, p5, p6, p7, p8)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8), (p1, p2, p3, p4, p5, p6, p7, p8), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8); \
		typedef SourceHook::ExecutableClass8< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 8, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN8(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, param8, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, param8, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8); \
	SourceHook::ExecutableClass8<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8), rettype, param1, param2, param3, param4, param5, param6, param7, param8> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK8_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...); \
		typedef SourceHook::ExecutableClass9< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 8, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN8_vafmt(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...); \
	SourceHook::ExecutableClass9<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...), rettype, param1, param2, param3, param4, param5, param6, param7, param8, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK8_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, param8> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8), (p1, p2, p3, p4, p5, p6, p7, p8)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8), (p1, p2, p3, p4, p5, p6, p7, p8)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8); \
		typedef SourceHook::ExecutableClass8<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 8, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN8_void(hookname, param1, param2, param3, param4, param5, param6, param7, param8) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, param8> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate8<param1, param2, param3, param4, param5, param6, param7, param8> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8); \
	SourceHook::ExecutableClass8<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8), void, param1, param2, param3, param4, param5, param6, param7, param8> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK8_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...); \
		typedef SourceHook::ExecutableClass9< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 8, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN8_void_vafmt(hookname, param1, param2, param3, param4, param5, param6, param7, param8) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...); \
	SourceHook::ExecutableClass9<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, const char *, ...), void, param1, param2, param3, param4, param5, param6, param7, param8, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 9 arguments *********
#define SH_DECL_HOOK9(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, param9, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9), (p1, p2, p3, p4, p5, p6, p7, p8, p9)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9), (p1, p2, p3, p4, p5, p6, p7, p8, p9), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 9, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN9(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, param9, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, param9, rettype> handler);

#define SH_DECL_HOOK9_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, param9> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9), (p1, p2, p3, p4, p5, p6, p7, p8, p9)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9), (p1, p2, p3, p4, p5, p6, p7, p8, p9)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3),__SH_GPI(param4),__SH_GPI(param5),__SH_GPI(param6),__SH_GPI(param7),__SH_GPI(param8),__SH_GPI(param9) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 9, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN9_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, param9> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, param9> handler);

#define SH_DECL_HOOK9_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 9, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN9_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, rettype> handler);

#define SH_DECL_HOOK9_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 9, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN9_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *> handler);

#define SH_DECL_MANUALHOOK9(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, param9, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9), (p1, p2, p3, p4, p5, p6, p7, p8, p9)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9), (p1, p2, p3, p4, p5, p6, p7, p8, p9), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9); \
		typedef SourceHook::ExecutableClass9< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 9, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN9(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, param9, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, param9, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9); \
	SourceHook::ExecutableClass9<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK9_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...); \
		typedef SourceHook::ExecutableClass10< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 9, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN9_vafmt(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...); \
	SourceHook::ExecutableClass10<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK9_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, param9> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9), (p1, p2, p3, p4, p5, p6, p7, p8, p9)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9), (p1, p2, p3, p4, p5, p6, p7, p8, p9)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9); \
		typedef SourceHook::ExecutableClass9<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 9, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN9_void(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, param9> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate9<param1, param2, param3, param4, param5, param6, param7, param8, param9> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9); \
	SourceHook::ExecutableClass9<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9), void, param1, param2, param3, param4, param5, param6, param7, param8, param9> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK9_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...); \
		typedef SourceHook::ExecutableClass10< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 9, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN9_void_vafmt(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...); \
	SourceHook::ExecutableClass10<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, const char *, ...), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 10 arguments *********
#define SH_DECL_HOOK10(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 10, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN10(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, rettype> handler);

#define SH_DECL_HOOK10_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3),__SH_GPI(param4),__SH_GPI(param5),__SH_GPI(param6),__SH_GPI(param7),__SH_GPI(param8),__SH_GPI(param9),__SH_GPI(param10) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 10, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN10_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10> handler);

#define SH_DECL_HOOK10_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 10, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN10_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, rettype> handler);

#define SH_DECL_HOOK10_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 10, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN10_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *> handler);

#define SH_DECL_MANUALHOOK10(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10); \
		typedef SourceHook::ExecutableClass10< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 10, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN10(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10); \
	SourceHook::ExecutableClass10<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK10_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...); \
		typedef SourceHook::ExecutableClass11< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 10, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN10_vafmt(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...); \
	SourceHook::ExecutableClass11<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK10_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10); \
		typedef SourceHook::ExecutableClass10<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 10, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN10_void(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate10<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10); \
	SourceHook::ExecutableClass10<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK10_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...); \
		typedef SourceHook::ExecutableClass11< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 10, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN10_void_vafmt(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...); \
	SourceHook::ExecutableClass11<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char *, ...), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 11 arguments *********
#define SH_DECL_HOOK11(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 11, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN11(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, rettype> handler);

#define SH_DECL_HOOK11_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3),__SH_GPI(param4),__SH_GPI(param5),__SH_GPI(param6),__SH_GPI(param7),__SH_GPI(param8),__SH_GPI(param9),__SH_GPI(param10),__SH_GPI(param11) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 11, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN11_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11> handler);

#define SH_DECL_HOOK11_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 11, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN11_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, rettype> handler);

#define SH_DECL_HOOK11_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 11, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN11_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *> handler);

#define SH_DECL_MANUALHOOK11(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11); \
		typedef SourceHook::ExecutableClass11< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 11, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN11(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11); \
	SourceHook::ExecutableClass11<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK11_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...); \
		typedef SourceHook::ExecutableClass12< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 11, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN11_vafmt(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...); \
	SourceHook::ExecutableClass12<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK11_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11); \
		typedef SourceHook::ExecutableClass11<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 11, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN11_void(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate11<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11); \
	SourceHook::ExecutableClass11<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK11_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...); \
		typedef SourceHook::ExecutableClass12< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 11, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN11_void_vafmt(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...); \
	SourceHook::ExecutableClass12<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char *, ...), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 12 arguments *********
#define SH_DECL_HOOK12(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 12, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN12(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, rettype> handler);

#define SH_DECL_HOOK12_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3),__SH_GPI(param4),__SH_GPI(param5),__SH_GPI(param6),__SH_GPI(param7),__SH_GPI(param8),__SH_GPI(param9),__SH_GPI(param10),__SH_GPI(param11),__SH_GPI(param12) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 12, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN12_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12> handler);

#define SH_DECL_HOOK12_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 12, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN12_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, rettype> handler);

#define SH_DECL_HOOK12_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 12, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN12_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *> handler);

#define SH_DECL_MANUALHOOK12(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12); \
		typedef SourceHook::ExecutableClass12< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 12, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN12(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12); \
	SourceHook::ExecutableClass12<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK12_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...); \
		typedef SourceHook::ExecutableClass13< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 12, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN12_vafmt(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...); \
	SourceHook::ExecutableClass13<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK12_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12); \
		typedef SourceHook::ExecutableClass12<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 12, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN12_void(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate12<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12); \
	SourceHook::ExecutableClass12<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK12_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...); \
		typedef SourceHook::ExecutableClass13< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 12, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN12_void_vafmt(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...); \
	SourceHook::ExecutableClass13<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char *, ...), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 13 arguments *********
#define SH_DECL_HOOK13(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 13, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN13(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, rettype> handler);

#define SH_DECL_HOOK13_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3),__SH_GPI(param4),__SH_GPI(param5),__SH_GPI(param6),__SH_GPI(param7),__SH_GPI(param8),__SH_GPI(param9),__SH_GPI(param10),__SH_GPI(param11),__SH_GPI(param12),__SH_GPI(param13) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 13, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN13_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13> handler);

#define SH_DECL_HOOK13_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 13, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN13_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, rettype> handler);

#define SH_DECL_HOOK13_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 13, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN13_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *> handler);

#define SH_DECL_MANUALHOOK13(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13); \
		typedef SourceHook::ExecutableClass13< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 13, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN13(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13); \
	SourceHook::ExecutableClass13<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK13_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...); \
		typedef SourceHook::ExecutableClass14< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 13, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN13_vafmt(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...); \
	SourceHook::ExecutableClass14<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK13_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13); \
		typedef SourceHook::ExecutableClass13<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 13, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN13_void(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate13<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13); \
	SourceHook::ExecutableClass13<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK13_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...); \
		typedef SourceHook::ExecutableClass14< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 13, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN13_void_vafmt(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...); \
	SourceHook::ExecutableClass14<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char *, ...), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 14 arguments *********
#define SH_DECL_HOOK14(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 14, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN14(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, rettype> handler);

#define SH_DECL_HOOK14_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3),__SH_GPI(param4),__SH_GPI(param5),__SH_GPI(param6),__SH_GPI(param7),__SH_GPI(param8),__SH_GPI(param9),__SH_GPI(param10),__SH_GPI(param11),__SH_GPI(param12),__SH_GPI(param13),__SH_GPI(param14) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 14, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN14_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14> handler);

#define SH_DECL_HOOK14_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 14, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN14_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, rettype> handler);

#define SH_DECL_HOOK14_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 14, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN14_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *> handler);

#define SH_DECL_MANUALHOOK14(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14); \
		typedef SourceHook::ExecutableClass14< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 14, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN14(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14); \
	SourceHook::ExecutableClass14<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK14_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...); \
		typedef SourceHook::ExecutableClass15< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 14, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN14_vafmt(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...); \
	SourceHook::ExecutableClass15<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK14_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14); \
		typedef SourceHook::ExecutableClass14<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 14, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN14_void(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate14<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14); \
	SourceHook::ExecutableClass14<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK14_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...); \
		typedef SourceHook::ExecutableClass15< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 14, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN14_void_vafmt(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...); \
	SourceHook::ExecutableClass15<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char *, ...), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 15 arguments *********
#define SH_DECL_HOOK15(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 15, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN15(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, rettype> handler);

#define SH_DECL_HOOK15_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3),__SH_GPI(param4),__SH_GPI(param5),__SH_GPI(param6),__SH_GPI(param7),__SH_GPI(param8),__SH_GPI(param9),__SH_GPI(param10),__SH_GPI(param11),__SH_GPI(param12),__SH_GPI(param13),__SH_GPI(param14),__SH_GPI(param15) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 15, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN15_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15> handler);

#define SH_DECL_HOOK15_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 15, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN15_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, rettype> handler);

#define SH_DECL_HOOK15_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 15, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN15_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *> handler);

#define SH_DECL_MANUALHOOK15(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15); \
		typedef SourceHook::ExecutableClass15< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 15, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN15(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15); \
	SourceHook::ExecutableClass15<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK15_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...); \
		typedef SourceHook::ExecutableClass16< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 15, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN15_vafmt(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...); \
	SourceHook::ExecutableClass16<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK15_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15); \
		typedef SourceHook::ExecutableClass15<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 15, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN15_void(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate15<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15); \
	SourceHook::ExecutableClass15<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK15_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...); \
		typedef SourceHook::ExecutableClass16< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 15, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN15_void_vafmt(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...); \
	SourceHook::ExecutableClass16<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char *, ...), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 16 arguments *********
#define SH_DECL_HOOK16(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 16, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN16(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, rettype> handler);

#define SH_DECL_HOOK16_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3),__SH_GPI(param4),__SH_GPI(param5),__SH_GPI(param6),__SH_GPI(param7),__SH_GPI(param8),__SH_GPI(param9),__SH_GPI(param10),__SH_GPI(param11),__SH_GPI(param12),__SH_GPI(param13),__SH_GPI(param14),__SH_GPI(param15),__SH_GPI(param16) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 16, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN16_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16> handler);

#define SH_DECL_HOOK16_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 16, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN16_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, rettype> handler);

#define SH_DECL_HOOK16_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 16, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN16_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *> handler);

#define SH_DECL_MANUALHOOK16(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16); \
		typedef SourceHook::ExecutableClass16< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 16, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN16(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16); \
	SourceHook::ExecutableClass16<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK16_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...); \
		typedef SourceHook::ExecutableClass17< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 16, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN16_vafmt(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...); \
	SourceHook::ExecutableClass17<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK16_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16); \
		typedef SourceHook::ExecutableClass16<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 16, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN16_void(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate16<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16); \
	SourceHook::ExecutableClass16<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK16_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...); \
		typedef SourceHook::ExecutableClass17< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 16, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN16_void_vafmt(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...); \
	SourceHook::ExecutableClass17<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char *, ...), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 17 arguments *********
#define SH_DECL_HOOK17(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 17, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN17(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, rettype> handler);

#define SH_DECL_HOOK17_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3),__SH_GPI(param4),__SH_GPI(param5),__SH_GPI(param6),__SH_GPI(param7),__SH_GPI(param8),__SH_GPI(param9),__SH_GPI(param10),__SH_GPI(param11),__SH_GPI(param12),__SH_GPI(param13),__SH_GPI(param14),__SH_GPI(param15),__SH_GPI(param16),__SH_GPI(param17) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 17, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN17_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17> handler);

#define SH_DECL_HOOK17_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 17, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN17_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, rettype> handler);

#define SH_DECL_HOOK17_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 17, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN17_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *> handler);

#define SH_DECL_MANUALHOOK17(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17); \
		typedef SourceHook::ExecutableClass17< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 17, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN17(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17); \
	SourceHook::ExecutableClass17<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK17_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, ...); \
		typedef SourceHook::ExecutableClass18< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 17, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN17_vafmt(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, ...); \
	SourceHook::ExecutableClass18<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, ...), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK17_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17); \
		typedef SourceHook::ExecutableClass17<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 17, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN17_void(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate17<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17); \
	SourceHook::ExecutableClass17<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK17_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, ...); \
		typedef SourceHook::ExecutableClass18< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 17, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN17_void_vafmt(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, ...); \
	SourceHook::ExecutableClass18<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char *, ...), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 18 arguments *********
#define SH_DECL_HOOK18(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 18, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN18(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, rettype> handler);

#define SH_DECL_HOOK18_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3),__SH_GPI(param4),__SH_GPI(param5),__SH_GPI(param6),__SH_GPI(param7),__SH_GPI(param8),__SH_GPI(param9),__SH_GPI(param10),__SH_GPI(param11),__SH_GPI(param12),__SH_GPI(param13),__SH_GPI(param14),__SH_GPI(param15),__SH_GPI(param16),__SH_GPI(param17),__SH_GPI(param18) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 18, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN18_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18> handler);

#define SH_DECL_HOOK18_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 18, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN18_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, rettype> handler);

#define SH_DECL_HOOK18_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 18, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN18_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *> handler);

#define SH_DECL_MANUALHOOK18(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18); \
		typedef SourceHook::ExecutableClass18< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 18, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN18(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18); \
	SourceHook::ExecutableClass18<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK18_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, ...); \
		typedef SourceHook::ExecutableClass19< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 18, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN18_vafmt(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, ...); \
	SourceHook::ExecutableClass19<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, ...), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK18_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18); \
		typedef SourceHook::ExecutableClass18<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 18, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN18_void(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate18<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18); \
	SourceHook::ExecutableClass18<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK18_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, ...); \
		typedef SourceHook::ExecutableClass19< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 18, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN18_void_vafmt(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, ...); \
	SourceHook::ExecutableClass19<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char *, ...), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 19 arguments *********
#define SH_DECL_HOOK19(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18), __SH_GPI(param19) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 19, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN19(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, rettype> handler);

#define SH_DECL_HOOK19_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3),__SH_GPI(param4),__SH_GPI(param5),__SH_GPI(param6),__SH_GPI(param7),__SH_GPI(param8),__SH_GPI(param9),__SH_GPI(param10),__SH_GPI(param11),__SH_GPI(param12),__SH_GPI(param13),__SH_GPI(param14),__SH_GPI(param15),__SH_GPI(param16),__SH_GPI(param17),__SH_GPI(param18),__SH_GPI(param19) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 19, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN19_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19> handler);

#define SH_DECL_HOOK19_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18), __SH_GPI(param19) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 19, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN19_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, rettype> handler);

#define SH_DECL_HOOK19_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18), __SH_GPI(param19) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 19, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN19_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *> handler);

#define SH_DECL_MANUALHOOK19(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19); \
		typedef SourceHook::ExecutableClass19< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18), __SH_GPI(param19) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 19, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN19(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19); \
	SourceHook::ExecutableClass19<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK19_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, ...); \
		typedef SourceHook::ExecutableClass20< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18), __SH_GPI(param19) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 19, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN19_vafmt(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, ...); \
	SourceHook::ExecutableClass20<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, ...), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK19_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19); \
		typedef SourceHook::ExecutableClass19<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18), __SH_GPI(param19) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 19, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN19_void(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate19<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19); \
	SourceHook::ExecutableClass19<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK19_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, ...); \
		typedef SourceHook::ExecutableClass20< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18), __SH_GPI(param19) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 19, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN19_void_vafmt(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, ...); \
	SourceHook::ExecutableClass20<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char *, ...), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

// ********* Support for 20 arguments *********
#define SH_DECL_HOOK20(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, param20 p20), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, param20 p20) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18), __SH_GPI(param19), __SH_GPI(param20) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 20, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN20(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, rettype> handler);

#define SH_DECL_HOOK20_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, param20 p20), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, param20 p20) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0},__SH_GPI(param1),__SH_GPI(param2),__SH_GPI(param3),__SH_GPI(param4),__SH_GPI(param5),__SH_GPI(param6),__SH_GPI(param7),__SH_GPI(param8),__SH_GPI(param9),__SH_GPI(param10),__SH_GPI(param11),__SH_GPI(param12),__SH_GPI(param13),__SH_GPI(param14),__SH_GPI(param15),__SH_GPI(param16),__SH_GPI(param17),__SH_GPI(param18),__SH_GPI(param19),__SH_GPI(param20) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 20, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN20_void(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20> handler);

#define SH_DECL_HOOK20_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate21<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, param20 p20, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, param20 p20 , const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18), __SH_GPI(param19), __SH_GPI(param20) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 20, __SH_GPI(rettype), \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN20_vafmt(ifacetype, ifacefunc, attr, overload, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate21<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, rettype> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate21<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, rettype> handler);

#define SH_DECL_HOOK20_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate21<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, param20 p20, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, param20 p20, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfos_##ifacetype##ifacefunc##overload[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18), __SH_GPI(param19), __SH_GPI(param20) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto = { 20, { 0, 0, 0 }, \
		__SourceHook_ParamInfos_##ifacetype##ifacefunc##overload, 0, __SH_EPI, __SourceHook_ParamInfos2_##ifacetype##ifacefunc##overload };

#define SH_DECL_EXTERN20_void_vafmt(ifacetype, ifacefunc, attr, overload, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) \
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate21<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *> handler); \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		fastdelegate::FastDelegate21<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *> handler);

#define SH_DECL_MANUALHOOK20(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, param20 p20), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, param20 p20) \
		{ SH_HANDLEFUNC((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20); \
		typedef SourceHook::ExecutableClass20< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20 > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18), __SH_GPI(param19), __SH_GPI(param20) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 20, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN20(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20); \
	SourceHook::ExecutableClass20<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK20_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate21<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, rettype> FD; \
		MAKE_DELEG(rettype, (param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, param20 p20, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, px)); \
		virtual rettype Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, param20 p20, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, buf), rettype); \
		} \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, ...); \
		typedef SourceHook::ExecutableClass21< ::SourceHook::EmptyClass, ECMFP, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char * > CallEC; \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18), __SH_GPI(param19), __SH_GPI(param20) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 20, __SH_GPI(rettype), \
		__SourceHook_ParamInfosM_##hookname, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname }; \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype value) \
	{ \
		::SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>()(shptr, value); \
	}

#define SH_DECL_MANUALEXTERN20_vafmt(hookname, rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate21<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, rettype> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate21<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, rettype> handler); \
	rettype(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, ...); \
	SourceHook::ExecutableClass21<SourceHook::EmptyClass, rettype(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, ...), rettype, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SoureceHook_FHM_SetOverrideResult##hookname(::SourceHook::ISourceHook *shptr, rettype res); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK20_void(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, param20 p20), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, param20 p20) \
		{ SH_HANDLEFUNC_void((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20); \
		typedef SourceHook::ExecutableClass20<SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20> CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18), __SH_GPI(param19), __SH_GPI(param20) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 20, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN20_void(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate20<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20); \
	SourceHook::ExecutableClass20<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);

#define SH_DECL_MANUALHOOK20_void_vafmt(hookname, vtblidx, vtbloffs, thisptroffs, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate21<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *> FD; \
		MAKE_DELEG_void((param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, param20 p20, const char *px), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, px)); \
		virtual void Func(param1 p1, param2 p2, param3 p3, param4 p4, param5 p5, param6 p6, param7 p7, param8 p8, param9 p9, param10 p10, param11 p11, param12 p12, param13 p13, param14 p14, param15 p15, param16 p16, param17 p17, param18 p18, param19 p19, param20 p20, const char *fmt, ...) \
		{ \
			__SH_VAFMT_SEQ(fmt, buf); \
			SH_HANDLEFUNC_void_vafmt((param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, ...), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, "%s", buf), (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, buf)); \
		} \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, ...); \
		typedef SourceHook::ExecutableClass21< ::SourceHook::EmptyClass, ECMFP, void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char * > CallEC; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const ::SourceHook::PassInfo __SourceHook_ParamInfosM_##hookname[] = { {1, 0, 0}, __SH_GPI(param1), __SH_GPI(param2), __SH_GPI(param3), __SH_GPI(param4), __SH_GPI(param5), __SH_GPI(param6), __SH_GPI(param7), __SH_GPI(param8), __SH_GPI(param9), __SH_GPI(param10), __SH_GPI(param11), __SH_GPI(param12), __SH_GPI(param13), __SH_GPI(param14), __SH_GPI(param15), __SH_GPI(param16), __SH_GPI(param17), __SH_GPI(param18), __SH_GPI(param19), __SH_GPI(param20) }; \
	const ::SourceHook::PassInfo::V2Info __SourceHook_ParamInfos2M_##hookname[] = { __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI, __SH_EPI }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto = { 20, { 0, 0, 0 }, \
		__SourceHook_ParamInfosM_##hookname	, 0, __SH_EPI, __SourceHook_ParamInfos2M_##hookname };

#define SH_DECL_MANUALEXTERN20_void_vafmt(hookname, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20) \
	int __SourceHook_FHMAdd##hookname(void *iface, ::SourceHook::ISourceHook::AddHookMode mode, bool post, \
		fastdelegate::FastDelegate21<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *> handler); \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		fastdelegate::FastDelegate21<param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *> handler); \
	void(::SourceHook::EmptyClass::* __SoureceHook_FHM_GetRecallMFP##hookname(::SourceHook::EmptyClass *thisptr) )(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, ...); \
	SourceHook::ExecutableClass21<SourceHook::EmptyClass, void(::SourceHook::EmptyClass::*)(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char *, ...), void, param1, param2, param3, param4, param5, param6, param7, param8, param9, param10, param11, param12, param13, param14, param15, param16, param17, param18, param19, param20, const char*> __SoureceHook_FHM_SHCall##hookname(void *ptr); \
	void __SourceHook_FHM_Reconfigure##hookname(int pvtblindex, int pvtbloffs, int pthisptroffs);



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

	// Support for 0 arguments
	template<class ObjType, class MFPType, class RetType> class ExecutableClass0
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass0(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()() const
			SH_MAKE_EXECUTABLECLASS_OB((), ())
	         
		
		template <class Param1> RetType operator()(Param1 p1) const
			SH_MAKE_EXECUTABLECLASS_OB((p1), (Param1))
		
		template <class Param1, class Param2> RetType operator()(Param1 p1, Param2 p2) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2), (Param1, Param2))
		
		template <class Param1, class Param2, class Param3> RetType operator()(Param1 p1, Param2 p2, Param3 p3) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3), (Param1, Param2, Param3))
		
		template <class Param1, class Param2, class Param3, class Param4> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4), (Param1, Param2, Param3, Param4))
		
		template <class Param1, class Param2, class Param3, class Param4, class Param5> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5), (Param1, Param2, Param3, Param4, Param5))
		
		template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6), (Param1, Param2, Param3, Param4, Param5, Param6))
		
		template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7), (Param1, Param2, Param3, Param4, Param5, Param6, Param7))
		
		template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
		
		template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
		
		template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
		
		template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
		
		template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
		
		template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
		
		template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
		
		template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
		
		template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
		
		template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
		
		template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType> class ExecutableClass0<ObjType, MFPType, void>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass0(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()() const
	      SH_MAKE_EXECUTABLECLASS_OB_void((), ())
	         
	   
	   template <class Param1> void operator()(Param1 p1) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1), (Param1))
	   
	   template <class Param1, class Param2> void operator()(Param1 p1, Param2 p2) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2), (Param1, Param2))
	   
	   template <class Param1, class Param2, class Param3> void operator()(Param1 p1, Param2 p2, Param3 p3) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3), (Param1, Param2, Param3))
	   
	   template <class Param1, class Param2, class Param3, class Param4> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4), (Param1, Param2, Param3, Param4))
	   
	   template <class Param1, class Param2, class Param3, class Param4, class Param5> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5), (Param1, Param2, Param3, Param4, Param5))
	   
	   template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6), (Param1, Param2, Param3, Param4, Param5, Param6))
	   
	   template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7), (Param1, Param2, Param3, Param4, Param5, Param6, Param7))
	   
	   template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
	   
	   template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
	   
	   template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
	   
	   template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
	   
	   template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
	   
	   template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
	   
	   template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	   
	   template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	   
	   template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	   
	   template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	   
	   template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 1 arguments
	template<class ObjType, class MFPType, class RetType, class Param1> class ExecutableClass1
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass1(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1) const
			SH_MAKE_EXECUTABLECLASS_OB((p1), (Param1))
	         
		
		template <class Param2> RetType operator()(Param1 p1, Param2 p2) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2), (Param1, Param2))
		
		template <class Param2, class Param3> RetType operator()(Param1 p1, Param2 p2, Param3 p3) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3), (Param1, Param2, Param3))
		
		template <class Param2, class Param3, class Param4> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4), (Param1, Param2, Param3, Param4))
		
		template <class Param2, class Param3, class Param4, class Param5> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5), (Param1, Param2, Param3, Param4, Param5))
		
		template <class Param2, class Param3, class Param4, class Param5, class Param6> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6), (Param1, Param2, Param3, Param4, Param5, Param6))
		
		template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7), (Param1, Param2, Param3, Param4, Param5, Param6, Param7))
		
		template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
		
		template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
		
		template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
		
		template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
		
		template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
		
		template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
		
		template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
		
		template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
		
		template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
		
		template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
		
		template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1> class ExecutableClass1<ObjType, MFPType, void, Param1>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass1(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1), (Param1))
	         
	   
	   template <class Param2> void operator()(Param1 p1, Param2 p2) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2), (Param1, Param2))
	   
	   template <class Param2, class Param3> void operator()(Param1 p1, Param2 p2, Param3 p3) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3), (Param1, Param2, Param3))
	   
	   template <class Param2, class Param3, class Param4> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4), (Param1, Param2, Param3, Param4))
	   
	   template <class Param2, class Param3, class Param4, class Param5> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5), (Param1, Param2, Param3, Param4, Param5))
	   
	   template <class Param2, class Param3, class Param4, class Param5, class Param6> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6), (Param1, Param2, Param3, Param4, Param5, Param6))
	   
	   template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7), (Param1, Param2, Param3, Param4, Param5, Param6, Param7))
	   
	   template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
	   
	   template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
	   
	   template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
	   
	   template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
	   
	   template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
	   
	   template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
	   
	   template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	   
	   template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	   
	   template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	   
	   template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	   
	   template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 2 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2> class ExecutableClass2
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass2(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2), (Param1, Param2))
	         
		
		template <class Param3> RetType operator()(Param1 p1, Param2 p2, Param3 p3) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3), (Param1, Param2, Param3))
		
		template <class Param3, class Param4> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4), (Param1, Param2, Param3, Param4))
		
		template <class Param3, class Param4, class Param5> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5), (Param1, Param2, Param3, Param4, Param5))
		
		template <class Param3, class Param4, class Param5, class Param6> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6), (Param1, Param2, Param3, Param4, Param5, Param6))
		
		template <class Param3, class Param4, class Param5, class Param6, class Param7> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7), (Param1, Param2, Param3, Param4, Param5, Param6, Param7))
		
		template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
		
		template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
		
		template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
		
		template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
		
		template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
		
		template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
		
		template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
		
		template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
		
		template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
		
		template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
		
		template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2> class ExecutableClass2<ObjType, MFPType, void, Param1, Param2>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass2(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2), (Param1, Param2))
	         
	   
	   template <class Param3> void operator()(Param1 p1, Param2 p2, Param3 p3) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3), (Param1, Param2, Param3))
	   
	   template <class Param3, class Param4> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4), (Param1, Param2, Param3, Param4))
	   
	   template <class Param3, class Param4, class Param5> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5), (Param1, Param2, Param3, Param4, Param5))
	   
	   template <class Param3, class Param4, class Param5, class Param6> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6), (Param1, Param2, Param3, Param4, Param5, Param6))
	   
	   template <class Param3, class Param4, class Param5, class Param6, class Param7> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7), (Param1, Param2, Param3, Param4, Param5, Param6, Param7))
	   
	   template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
	   
	   template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
	   
	   template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
	   
	   template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
	   
	   template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
	   
	   template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
	   
	   template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	   
	   template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	   
	   template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	   
	   template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	   
	   template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 3 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3> class ExecutableClass3
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass3(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3), (Param1, Param2, Param3))
	         
		
		template <class Param4> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4), (Param1, Param2, Param3, Param4))
		
		template <class Param4, class Param5> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5), (Param1, Param2, Param3, Param4, Param5))
		
		template <class Param4, class Param5, class Param6> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6), (Param1, Param2, Param3, Param4, Param5, Param6))
		
		template <class Param4, class Param5, class Param6, class Param7> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7), (Param1, Param2, Param3, Param4, Param5, Param6, Param7))
		
		template <class Param4, class Param5, class Param6, class Param7, class Param8> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
		
		template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
		
		template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
		
		template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
		
		template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
		
		template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
		
		template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
		
		template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
		
		template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
		
		template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
		
		template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3> class ExecutableClass3<ObjType, MFPType, void, Param1, Param2, Param3>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass3(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3), (Param1, Param2, Param3))
	         
	   
	   template <class Param4> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4), (Param1, Param2, Param3, Param4))
	   
	   template <class Param4, class Param5> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5), (Param1, Param2, Param3, Param4, Param5))
	   
	   template <class Param4, class Param5, class Param6> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6), (Param1, Param2, Param3, Param4, Param5, Param6))
	   
	   template <class Param4, class Param5, class Param6, class Param7> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7), (Param1, Param2, Param3, Param4, Param5, Param6, Param7))
	   
	   template <class Param4, class Param5, class Param6, class Param7, class Param8> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
	   
	   template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
	   
	   template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
	   
	   template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
	   
	   template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
	   
	   template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
	   
	   template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	   
	   template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	   
	   template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	   
	   template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	   
	   template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 4 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3, class Param4> class ExecutableClass4
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass4(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4), (Param1, Param2, Param3, Param4))
	         
		
		template <class Param5> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5), (Param1, Param2, Param3, Param4, Param5))
		
		template <class Param5, class Param6> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6), (Param1, Param2, Param3, Param4, Param5, Param6))
		
		template <class Param5, class Param6, class Param7> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7), (Param1, Param2, Param3, Param4, Param5, Param6, Param7))
		
		template <class Param5, class Param6, class Param7, class Param8> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
		
		template <class Param5, class Param6, class Param7, class Param8, class Param9> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
		
		template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
		
		template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
		
		template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
		
		template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
		
		template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
		
		template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
		
		template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
		
		template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
		
		template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3, class Param4> class ExecutableClass4<ObjType, MFPType, void, Param1, Param2, Param3, Param4>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass4(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4), (Param1, Param2, Param3, Param4))
	         
	   
	   template <class Param5> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5), (Param1, Param2, Param3, Param4, Param5))
	   
	   template <class Param5, class Param6> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6), (Param1, Param2, Param3, Param4, Param5, Param6))
	   
	   template <class Param5, class Param6, class Param7> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7), (Param1, Param2, Param3, Param4, Param5, Param6, Param7))
	   
	   template <class Param5, class Param6, class Param7, class Param8> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
	   
	   template <class Param5, class Param6, class Param7, class Param8, class Param9> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
	   
	   template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
	   
	   template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
	   
	   template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
	   
	   template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
	   
	   template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	   
	   template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	   
	   template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	   
	   template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	   
	   template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 5 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5> class ExecutableClass5
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass5(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5), (Param1, Param2, Param3, Param4, Param5))
	         
		
		template <class Param6> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6), (Param1, Param2, Param3, Param4, Param5, Param6))
		
		template <class Param6, class Param7> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7), (Param1, Param2, Param3, Param4, Param5, Param6, Param7))
		
		template <class Param6, class Param7, class Param8> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
		
		template <class Param6, class Param7, class Param8, class Param9> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
		
		template <class Param6, class Param7, class Param8, class Param9, class Param10> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
		
		template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
		
		template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
		
		template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
		
		template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
		
		template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
		
		template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
		
		template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
		
		template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3, class Param4, class Param5> class ExecutableClass5<ObjType, MFPType, void, Param1, Param2, Param3, Param4, Param5>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass5(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5), (Param1, Param2, Param3, Param4, Param5))
	         
	   
	   template <class Param6> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6), (Param1, Param2, Param3, Param4, Param5, Param6))
	   
	   template <class Param6, class Param7> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7), (Param1, Param2, Param3, Param4, Param5, Param6, Param7))
	   
	   template <class Param6, class Param7, class Param8> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
	   
	   template <class Param6, class Param7, class Param8, class Param9> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
	   
	   template <class Param6, class Param7, class Param8, class Param9, class Param10> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
	   
	   template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
	   
	   template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
	   
	   template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
	   
	   template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	   
	   template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	   
	   template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	   
	   template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	   
	   template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 6 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6> class ExecutableClass6
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass6(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6), (Param1, Param2, Param3, Param4, Param5, Param6))
	         
		
		template <class Param7> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7), (Param1, Param2, Param3, Param4, Param5, Param6, Param7))
		
		template <class Param7, class Param8> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
		
		template <class Param7, class Param8, class Param9> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
		
		template <class Param7, class Param8, class Param9, class Param10> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
		
		template <class Param7, class Param8, class Param9, class Param10, class Param11> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
		
		template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
		
		template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
		
		template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
		
		template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
		
		template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
		
		template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
		
		template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6> class ExecutableClass6<ObjType, MFPType, void, Param1, Param2, Param3, Param4, Param5, Param6>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass6(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6), (Param1, Param2, Param3, Param4, Param5, Param6))
	         
	   
	   template <class Param7> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7), (Param1, Param2, Param3, Param4, Param5, Param6, Param7))
	   
	   template <class Param7, class Param8> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
	   
	   template <class Param7, class Param8, class Param9> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
	   
	   template <class Param7, class Param8, class Param9, class Param10> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
	   
	   template <class Param7, class Param8, class Param9, class Param10, class Param11> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
	   
	   template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
	   
	   template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
	   
	   template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	   
	   template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	   
	   template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	   
	   template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	   
	   template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 7 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7> class ExecutableClass7
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass7(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7), (Param1, Param2, Param3, Param4, Param5, Param6, Param7))
	         
		
		template <class Param8> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
		
		template <class Param8, class Param9> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
		
		template <class Param8, class Param9, class Param10> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
		
		template <class Param8, class Param9, class Param10, class Param11> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
		
		template <class Param8, class Param9, class Param10, class Param11, class Param12> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
		
		template <class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
		
		template <class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
		
		template <class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
		
		template <class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
		
		template <class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
		
		template <class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7> class ExecutableClass7<ObjType, MFPType, void, Param1, Param2, Param3, Param4, Param5, Param6, Param7>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass7(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7), (Param1, Param2, Param3, Param4, Param5, Param6, Param7))
	         
	   
	   template <class Param8> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
	   
	   template <class Param8, class Param9> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
	   
	   template <class Param8, class Param9, class Param10> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
	   
	   template <class Param8, class Param9, class Param10, class Param11> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
	   
	   template <class Param8, class Param9, class Param10, class Param11, class Param12> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
	   
	   template <class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
	   
	   template <class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	   
	   template <class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	   
	   template <class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	   
	   template <class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	   
	   template <class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 8 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8> class ExecutableClass8
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass8(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
	         
		
		template <class Param9> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
		
		template <class Param9, class Param10> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
		
		template <class Param9, class Param10, class Param11> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
		
		template <class Param9, class Param10, class Param11, class Param12> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
		
		template <class Param9, class Param10, class Param11, class Param12, class Param13> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
		
		template <class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
		
		template <class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
		
		template <class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
		
		template <class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
		
		template <class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8> class ExecutableClass8<ObjType, MFPType, void, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass8(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
	         
	   
	   template <class Param9> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
	   
	   template <class Param9, class Param10> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
	   
	   template <class Param9, class Param10, class Param11> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
	   
	   template <class Param9, class Param10, class Param11, class Param12> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
	   
	   template <class Param9, class Param10, class Param11, class Param12, class Param13> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
	   
	   template <class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	   
	   template <class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	   
	   template <class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	   
	   template <class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	   
	   template <class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 9 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9> class ExecutableClass9
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass9(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
	         
		
		template <class Param10> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
		
		template <class Param10, class Param11> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
		
		template <class Param10, class Param11, class Param12> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
		
		template <class Param10, class Param11, class Param12, class Param13> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
		
		template <class Param10, class Param11, class Param12, class Param13, class Param14> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
		
		template <class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
		
		template <class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
		
		template <class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
		
		template <class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9> class ExecutableClass9<ObjType, MFPType, void, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass9(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
	         
	   
	   template <class Param10> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
	   
	   template <class Param10, class Param11> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
	   
	   template <class Param10, class Param11, class Param12> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
	   
	   template <class Param10, class Param11, class Param12, class Param13> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
	   
	   template <class Param10, class Param11, class Param12, class Param13, class Param14> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	   
	   template <class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	   
	   template <class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	   
	   template <class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	   
	   template <class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 10 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10> class ExecutableClass10
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass10(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
	         
		
		template <class Param11> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
		
		template <class Param11, class Param12> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
		
		template <class Param11, class Param12, class Param13> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
		
		template <class Param11, class Param12, class Param13, class Param14> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
		
		template <class Param11, class Param12, class Param13, class Param14, class Param15> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
		
		template <class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
		
		template <class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
		
		template <class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10> class ExecutableClass10<ObjType, MFPType, void, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass10(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
	         
	   
	   template <class Param11> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
	   
	   template <class Param11, class Param12> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
	   
	   template <class Param11, class Param12, class Param13> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
	   
	   template <class Param11, class Param12, class Param13, class Param14> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	   
	   template <class Param11, class Param12, class Param13, class Param14, class Param15> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	   
	   template <class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	   
	   template <class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	   
	   template <class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 11 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11> class ExecutableClass11
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass11(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
	         
		
		template <class Param12> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
		
		template <class Param12, class Param13> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
		
		template <class Param12, class Param13, class Param14> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
		
		template <class Param12, class Param13, class Param14, class Param15> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
		
		template <class Param12, class Param13, class Param14, class Param15, class Param16> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
		
		template <class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
		
		template <class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11> class ExecutableClass11<ObjType, MFPType, void, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass11(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
	         
	   
	   template <class Param12> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
	   
	   template <class Param12, class Param13> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
	   
	   template <class Param12, class Param13, class Param14> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	   
	   template <class Param12, class Param13, class Param14, class Param15> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	   
	   template <class Param12, class Param13, class Param14, class Param15, class Param16> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	   
	   template <class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	   
	   template <class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 12 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12> class ExecutableClass12
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass12(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
	         
		
		template <class Param13> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
		
		template <class Param13, class Param14> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
		
		template <class Param13, class Param14, class Param15> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
		
		template <class Param13, class Param14, class Param15, class Param16> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
		
		template <class Param13, class Param14, class Param15, class Param16, class Param17> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
		
		template <class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12> class ExecutableClass12<ObjType, MFPType, void, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass12(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
	         
	   
	   template <class Param13> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
	   
	   template <class Param13, class Param14> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	   
	   template <class Param13, class Param14, class Param15> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	   
	   template <class Param13, class Param14, class Param15, class Param16> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	   
	   template <class Param13, class Param14, class Param15, class Param16, class Param17> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	   
	   template <class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 13 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> class ExecutableClass13
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass13(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
	         
		
		template <class Param14> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
		
		template <class Param14, class Param15> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
		
		template <class Param14, class Param15, class Param16> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
		
		template <class Param14, class Param15, class Param16, class Param17> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
		
		template <class Param14, class Param15, class Param16, class Param17, class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13> class ExecutableClass13<ObjType, MFPType, void, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass13(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
	         
	   
	   template <class Param14> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	   
	   template <class Param14, class Param15> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	   
	   template <class Param14, class Param15, class Param16> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	   
	   template <class Param14, class Param15, class Param16, class Param17> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	   
	   template <class Param14, class Param15, class Param16, class Param17, class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 14 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> class ExecutableClass14
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass14(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	         
		
		template <class Param15> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
		
		template <class Param15, class Param16> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
		
		template <class Param15, class Param16, class Param17> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
		
		template <class Param15, class Param16, class Param17, class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param15, class Param16, class Param17, class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14> class ExecutableClass14<ObjType, MFPType, void, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass14(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	         
	   
	   template <class Param15> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	   
	   template <class Param15, class Param16> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	   
	   template <class Param15, class Param16, class Param17> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	   
	   template <class Param15, class Param16, class Param17, class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param15, class Param16, class Param17, class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 15 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> class ExecutableClass15
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass15(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	         
		
		template <class Param16> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
		
		template <class Param16, class Param17> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
		
		template <class Param16, class Param17, class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param16, class Param17, class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param16, class Param17, class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15> class ExecutableClass15<ObjType, MFPType, void, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass15(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	         
	   
	   template <class Param16> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	   
	   template <class Param16, class Param17> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	   
	   template <class Param16, class Param17, class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param16, class Param17, class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param16, class Param17, class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 16 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> class ExecutableClass16
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass16(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	         
		
		template <class Param17> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
		
		template <class Param17, class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param17, class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param17, class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16> class ExecutableClass16<ObjType, MFPType, void, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass16(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	         
	   
	   template <class Param17> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	   
	   template <class Param17, class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param17, class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param17, class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 17 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> class ExecutableClass17
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass17(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	         
		
		template <class Param18> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
		
		template <class Param18, class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param18, class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17> class ExecutableClass17<ObjType, MFPType, void, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass17(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	         
	   
	   template <class Param18> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	   
	   template <class Param18, class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param18, class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 18 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> class ExecutableClass18
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass18(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	         
		
		template <class Param19> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
		
		template <class Param19, class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18> class ExecutableClass18<ObjType, MFPType, void, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass18(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	         
	   
	   template <class Param19> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	   
	   template <class Param19, class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 19 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> class ExecutableClass19
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass19(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	         
		
		template <class Param20> RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19> class ExecutableClass19<ObjType, MFPType, void, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass19(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	         
	   
	   template <class Param20> void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	   
	};

	// Support for 20 arguments
	template<class ObjType, class MFPType, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> class ExecutableClass20
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass20(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
		RetType operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
			SH_MAKE_EXECUTABLECLASS_OB((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	         
		
	};

	template<class ObjType, class MFPType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20> class ExecutableClass20<ObjType, MFPType, void, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20>
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFPType m_MFP;
		ISourceHook *m_pSH;
	public:
		ExecutableClass20(ObjType *tp, MFPType mfp, void *vp, ISourceHook *pSH) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH) { }
	
	   void operator()(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6, Param7 p7, Param8 p8, Param9 p9, Param10 p10, Param11 p11, Param12 p12, Param13 p13, Param14 p14, Param15 p15, Param16 p16, Param17 p17, Param18 p18, Param19 p19, Param20 p20) const
	      SH_MAKE_EXECUTABLECLASS_OB_void((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), (Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	         
	   
	};


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


// Support for 0 arguments
template <class X, class Y, class MFP, class RetType>
SourceHook::ExecutableClass0<Y, MFP, RetType>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass0<Y, MFP, RetType>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType>
SourceHook::ExecutableClass0<Y, MFP, RetType>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)()const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass0<Y, MFP, RetType>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType>
SourceHook::ExecutableClass0<SourceHook::EmptyClass, MFP, RetType>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass0<EmptyClass, MFP, RetType>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 1 arguments
template <class X, class Y, class MFP, class RetType, class Param1>
SourceHook::ExecutableClass1<Y, MFP, RetType, Param1>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass1<Y, MFP, RetType, Param1>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1>
SourceHook::ExecutableClass1<Y, MFP, RetType, Param1>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass1<Y, MFP, RetType, Param1>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1>
SourceHook::ExecutableClass1<SourceHook::EmptyClass, MFP, RetType, Param1>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass1<EmptyClass, MFP, RetType, Param1>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 2 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2>
SourceHook::ExecutableClass2<Y, MFP, RetType, Param1, Param2>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass2<Y, MFP, RetType, Param1, Param2>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2>
SourceHook::ExecutableClass2<Y, MFP, RetType, Param1, Param2>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass2<Y, MFP, RetType, Param1, Param2>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2>
SourceHook::ExecutableClass2<SourceHook::EmptyClass, MFP, RetType, Param1, Param2>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass2<EmptyClass, MFP, RetType, Param1, Param2>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 3 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3>
SourceHook::ExecutableClass3<Y, MFP, RetType, Param1, Param2, Param3>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass3<Y, MFP, RetType, Param1, Param2, Param3>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3>
SourceHook::ExecutableClass3<Y, MFP, RetType, Param1, Param2, Param3>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass3<Y, MFP, RetType, Param1, Param2, Param3>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3>
SourceHook::ExecutableClass3<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass3<EmptyClass, MFP, RetType, Param1, Param2, Param3>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 4 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4>
SourceHook::ExecutableClass4<Y, MFP, RetType, Param1, Param2, Param3, Param4>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass4<Y, MFP, RetType, Param1, Param2, Param3, Param4>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4>
SourceHook::ExecutableClass4<Y, MFP, RetType, Param1, Param2, Param3, Param4>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass4<Y, MFP, RetType, Param1, Param2, Param3, Param4>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4>
SourceHook::ExecutableClass4<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass4<EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 5 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5>
SourceHook::ExecutableClass5<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass5<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5>
SourceHook::ExecutableClass5<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass5<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5>
SourceHook::ExecutableClass5<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass5<EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 6 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6>
SourceHook::ExecutableClass6<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass6<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6>
SourceHook::ExecutableClass6<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass6<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6>
SourceHook::ExecutableClass6<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass6<EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 7 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7>
SourceHook::ExecutableClass7<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass7<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7>
SourceHook::ExecutableClass7<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass7<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7>
SourceHook::ExecutableClass7<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass7<EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 8 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8>
SourceHook::ExecutableClass8<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass8<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8>
SourceHook::ExecutableClass8<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass8<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8>
SourceHook::ExecutableClass8<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass8<EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 9 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9>
SourceHook::ExecutableClass9<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass9<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9>
SourceHook::ExecutableClass9<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass9<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9>
SourceHook::ExecutableClass9<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass9<EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 10 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10>
SourceHook::ExecutableClass10<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass10<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10>
SourceHook::ExecutableClass10<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass10<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10>
SourceHook::ExecutableClass10<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass10<EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 11 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11>
SourceHook::ExecutableClass11<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass11<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11>
SourceHook::ExecutableClass11<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass11<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11>
SourceHook::ExecutableClass11<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass11<EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 12 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12>
SourceHook::ExecutableClass12<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass12<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12>
SourceHook::ExecutableClass12<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass12<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12>
SourceHook::ExecutableClass12<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass12<EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 13 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13>
SourceHook::ExecutableClass13<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass13<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13>
SourceHook::ExecutableClass13<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass13<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13>
SourceHook::ExecutableClass13<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass13<EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 14 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14>
SourceHook::ExecutableClass14<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass14<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14>
SourceHook::ExecutableClass14<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass14<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14>
SourceHook::ExecutableClass14<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass14<EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 15 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15>
SourceHook::ExecutableClass15<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass15<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15>
SourceHook::ExecutableClass15<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass15<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15>
SourceHook::ExecutableClass15<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass15<EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 16 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16>
SourceHook::ExecutableClass16<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass16<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16>
SourceHook::ExecutableClass16<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass16<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16>
SourceHook::ExecutableClass16<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass16<EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 17 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17>
SourceHook::ExecutableClass17<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass17<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17>
SourceHook::ExecutableClass17<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass17<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17>
SourceHook::ExecutableClass17<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass17<EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 18 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18>
SourceHook::ExecutableClass18<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass18<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18>
SourceHook::ExecutableClass18<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass18<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18>
SourceHook::ExecutableClass18<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass18<EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 19 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19>
SourceHook::ExecutableClass19<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass19<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19>
SourceHook::ExecutableClass19<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass19<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19>
SourceHook::ExecutableClass19<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass19<EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}

// Support for 20 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20>
SourceHook::ExecutableClass20<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass20<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20>
SourceHook::ExecutableClass20<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass20<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20>
SourceHook::ExecutableClass20<SourceHook::EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass20<EmptyClass, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20>(
		reinterpret_cast<SourceHook::EmptyClass*>(ptr), mfp, vfnptr, shptr);
}


#if SH_COMP != SH_COMP_MSVC || _MSC_VER > 1300
// GCC & MSVC 7.1 need this, MSVC 7.0 doesn't like it


// Support for 0 arguments
template <class X, class Y, class MFP, class RetType>
SourceHook::ExecutableClass0<Y, MFP, RetType>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass0<Y, MFP, RetType>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType>
SourceHook::ExecutableClass0<Y, MFP, RetType>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass0<Y, MFP, RetType>(ptr, mfp, vfnptr, shptr);
}


// Support for 1 arguments
template <class X, class Y, class MFP, class RetType, class Param1>
SourceHook::ExecutableClass1<Y, MFP, RetType, Param1>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass1<Y, MFP, RetType, Param1>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1>
SourceHook::ExecutableClass1<Y, MFP, RetType, Param1>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass1<Y, MFP, RetType, Param1>(ptr, mfp, vfnptr, shptr);
}


// Support for 2 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2>
SourceHook::ExecutableClass2<Y, MFP, RetType, Param1, Param2>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass2<Y, MFP, RetType, Param1, Param2>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2>
SourceHook::ExecutableClass2<Y, MFP, RetType, Param1, Param2>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass2<Y, MFP, RetType, Param1, Param2>(ptr, mfp, vfnptr, shptr);
}


// Support for 3 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3>
SourceHook::ExecutableClass3<Y, MFP, RetType, Param1, Param2, Param3>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass3<Y, MFP, RetType, Param1, Param2, Param3>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3>
SourceHook::ExecutableClass3<Y, MFP, RetType, Param1, Param2, Param3>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass3<Y, MFP, RetType, Param1, Param2, Param3>(ptr, mfp, vfnptr, shptr);
}


// Support for 4 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4>
SourceHook::ExecutableClass4<Y, MFP, RetType, Param1, Param2, Param3, Param4>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass4<Y, MFP, RetType, Param1, Param2, Param3, Param4>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4>
SourceHook::ExecutableClass4<Y, MFP, RetType, Param1, Param2, Param3, Param4>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass4<Y, MFP, RetType, Param1, Param2, Param3, Param4>(ptr, mfp, vfnptr, shptr);
}


// Support for 5 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5>
SourceHook::ExecutableClass5<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass5<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5>
SourceHook::ExecutableClass5<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass5<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5>(ptr, mfp, vfnptr, shptr);
}


// Support for 6 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6>
SourceHook::ExecutableClass6<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass6<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6>
SourceHook::ExecutableClass6<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass6<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6>(ptr, mfp, vfnptr, shptr);
}


// Support for 7 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7>
SourceHook::ExecutableClass7<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass7<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7>
SourceHook::ExecutableClass7<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass7<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7>(ptr, mfp, vfnptr, shptr);
}


// Support for 8 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8>
SourceHook::ExecutableClass8<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass8<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8>
SourceHook::ExecutableClass8<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass8<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8>(ptr, mfp, vfnptr, shptr);
}


// Support for 9 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9>
SourceHook::ExecutableClass9<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass9<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9>
SourceHook::ExecutableClass9<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass9<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9>(ptr, mfp, vfnptr, shptr);
}


// Support for 10 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10>
SourceHook::ExecutableClass10<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass10<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10>
SourceHook::ExecutableClass10<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass10<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10>(ptr, mfp, vfnptr, shptr);
}


// Support for 11 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11>
SourceHook::ExecutableClass11<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass11<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11>
SourceHook::ExecutableClass11<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass11<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11>(ptr, mfp, vfnptr, shptr);
}


// Support for 12 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12>
SourceHook::ExecutableClass12<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass12<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12>
SourceHook::ExecutableClass12<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass12<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12>(ptr, mfp, vfnptr, shptr);
}


// Support for 13 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13>
SourceHook::ExecutableClass13<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass13<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13>
SourceHook::ExecutableClass13<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass13<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13>(ptr, mfp, vfnptr, shptr);
}


// Support for 14 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14>
SourceHook::ExecutableClass14<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass14<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14>
SourceHook::ExecutableClass14<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass14<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14>(ptr, mfp, vfnptr, shptr);
}


// Support for 15 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15>
SourceHook::ExecutableClass15<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass15<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15>
SourceHook::ExecutableClass15<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass15<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15>(ptr, mfp, vfnptr, shptr);
}


// Support for 16 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16>
SourceHook::ExecutableClass16<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass16<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16>
SourceHook::ExecutableClass16<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass16<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16>(ptr, mfp, vfnptr, shptr);
}


// Support for 17 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17>
SourceHook::ExecutableClass17<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass17<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17>
SourceHook::ExecutableClass17<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass17<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17>(ptr, mfp, vfnptr, shptr);
}


// Support for 18 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18>
SourceHook::ExecutableClass18<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass18<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18>
SourceHook::ExecutableClass18<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass18<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18>(ptr, mfp, vfnptr, shptr);
}


// Support for 19 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19>
SourceHook::ExecutableClass19<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass19<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19>
SourceHook::ExecutableClass19<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass19<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19>(ptr, mfp, vfnptr, shptr);
}


// Support for 20 arguments
template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20>
SourceHook::ExecutableClass20<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20, ...), SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass20<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20>(ptr, mfp, vfnptr, shptr);
}

template <class X, class Y, class MFP, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20>
SourceHook::ExecutableClass20<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20, ...)const, SourceHook::ISourceHook *shptr)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass20<Y, MFP, RetType, Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20>(ptr, mfp, vfnptr, shptr);
}



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

	template <class Iface, class RetType>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)())
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)())
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20, ...))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

	template <class Iface, class RetType, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6, class Param7, class Param8, class Param9, class Param10, class Param11, class Param12, class Param13, class Param14, class Param15, class Param16, class Param17, class Param18, class Param19, class Param20>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(Param1, Param2, Param3, Param4, Param5, Param6, Param7, Param8, Param9, Param10, Param11, Param12, Param13, Param14, Param15, Param16, Param17, Param18, Param19, Param20, ...))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}

}

#endif
	// The pope is dead. -> :(
