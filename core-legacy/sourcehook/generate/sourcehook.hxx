/* ======== SourceHook ========
* Copyright (C) 2004-2007 Metamod:Source Development Team
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
#define SH_IFACE_VERSION 4

// Impl versions:
// ???
// 4 - addition of hook ids and vp hooks (with them, AddHookNew and RemoveHookNew)
//     This is not a SH_IFACE_VERSION change so that old plugins continue working!
// 5 - addition of direct vp hooks (new hook mode; from now on AddHookNew checks for
//	   invalid hookmode -> impl version won't have to change because of things like this)
#define SH_IMPL_VERSION 5

// Hookman version:
//  1 - Support for recalls, performance optimisations
#define SH_HOOKMAN_VERSION 1

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

#if SH_COMP==SH_COMP_MSVC && _MSC_VER < 1500
# define vsnprintf _vsnprintf
#endif

#if SH_SYS != SH_SYS_WIN32
# include <unistd.h>
#endif

#define SH_PTRSIZE sizeof(void*)

#include "FastDelegate.h"
#include "sh_memfuncinfo.h"

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

	struct ProtoInfo
	{
		ProtoInfo(int rtsz, int nop, const int *p) : beginningNull(0), retTypeSize(rtsz), numOfParams(nop), params(p)
		{
		}
		int beginningNull;		//!< To distinguish from old protos (which never begin with 0)
		int retTypeSize;		//!< 0 if void
		int numOfParams;		//!< number of parameters
		const int *params;		//!< params[0]=0 (or -1 for vararg), params[1]=size of first param, ...
	};

	/**
	*	@brief Specifies the actions for hook managers
	*/
	enum HookManagerAction
	{
		HA_GetInfo = 0,			//!< Store info about the hook manager
		HA_Register,			//!< Save the IHookManagerInfo pointer for future reference
		HA_Unregister			//!< Clear the saved pointer
	};

	struct IHookManagerInfo;

	/**
	*	@brief Pointer to hook manager interface function
	*
	*	A "hook manager" is a the only thing that knows the actual protoype of the function at compile time.
	*
	*   @param ha What the hook manager should do
	*	@param hi A pointer to IHookManagerInfo
	*/
	typedef int (*HookManagerPubFunc)(HookManagerAction ha, IHookManagerInfo *hi);

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

	struct IHookList
	{
		struct IIter
		{
			virtual bool End() = 0;
			virtual void Next() = 0;
			virtual ISHDelegate *Handler() = 0;
			virtual int ThisPtrOffs() = 0;
		};
		virtual IIter *GetIter() = 0;
		virtual void ReleaseIter(IIter *pIter) = 0;
	};

	struct IIface
	{
		virtual void *GetPtr() = 0;
		virtual IHookList *GetPreHooks() = 0;
		virtual IHookList *GetPostHooks() = 0;
	};

	struct IVfnPtr
	{
		virtual void *GetVfnPtr() = 0;
		virtual void *GetOrigEntry() = 0;

		virtual IIface *FindIface(void *ptr) = 0;
	};

	struct IHookManagerInfo
	{
		virtual IVfnPtr *FindVfnPtr(void *vfnptr) = 0;

		virtual void SetInfo(int vtbloffs, int vtblidx, const char *proto) = 0;
		virtual void SetHookfuncVfnptr(void *hookfunc_vfnptr) = 0;

		// Added 23.12.2005 (yup! I'm coding RIGHT BEFORE CHRISTMAS!)
		// If the hookman doesn't set this, it defaults 0
		// SourceHook prefers hookmans with higher version numbers
		virtual void SetVersion(int version) = 0;
	};

	class AutoHookIter
	{
		IHookList *m_pList;
		IHookList::IIter *m_pIter;
	public:
		AutoHookIter(IHookList *pList)
			: m_pList(pList), m_pIter(pList->GetIter())
		{
		}

		~AutoHookIter()
		{
			if (m_pList)
				m_pList->ReleaseIter(m_pIter);
		}

		bool End()
		{
			return m_pIter->End();
		}

		void Next()
		{
			m_pIter->Next();
		}

		ISHDelegate *Handler()
		{
			return m_pIter->Handler();
		}

		int ThisPtrOffs()
		{
			return m_pIter->ThisPtrOffs();
		}

		void SetToZero()
		{
			m_pList = 0;
		}
	};

	template<class B> struct DeprecatedCallClass
	{
		virtual B *GetThisPtr() = 0;
		virtual void *GetOrigFunc(int vtbloffs, int vtblidx) = 0;
	};

	// 09.08.2008 (6 AM, I just woke up, the others are still sleeping so i finally can use this notebook !!)
	// - Today is an important day.
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
		virtual bool AddHook(Plugin plug, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan,
			ISHDelegate *handler, bool post) = 0;

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
		virtual bool RemoveHook(Plugin plug, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan,
			ISHDelegate *handler, bool post) = 0;

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
		virtual DeprecatedCallClass<void> *GetCallClass(void *iface, size_t size) = 0;

		/**
		*	@brief Release a callclass
		*
		*	@param ptr Pointer to the callclass
		*/
		virtual void ReleaseCallClass(DeprecatedCallClass<void> *ptr) = 0;

		virtual void SetRes(META_RES res) = 0;				//!< Sets the meta result
		virtual META_RES GetPrevRes() = 0;					//!< Gets the meta result of the
															//!<  previously calledhandler
		virtual META_RES GetStatus() = 0;					//!< Gets the highest meta result
		virtual const void *GetOrigRet() = 0;				//!< Gets the original result.
															//!<  If not in post function, undefined
		virtual const void *GetOverrideRet() = 0;			//!< Gets the override result.
															//!<  If none is specified, NULL
		virtual void *GetIfacePtr() = 0;					//!< Gets the interface pointer
		//////////////////////////////////////////////////////////////////////////
		// For hook managers
		virtual void HookLoopBegin(IIface *pIface) = 0;			//!< Should be called when a hook loop begins
		virtual void HookLoopEnd() = 0;							//!< Should be called when a hook loop exits
		virtual void SetCurResPtr(META_RES *mres) = 0;			//!< Sets pointer to the current meta result
		virtual void SetPrevResPtr(META_RES *mres) = 0;			//!< Sets pointer to previous meta result
		virtual void SetStatusPtr(META_RES *mres) = 0;			//!< Sets pointer to the status variable
		virtual void SetIfacePtrPtr(void **pp) = 0;				//!< Sets pointer to the interface this pointer
		virtual void SetOrigRetPtr(const void *ptr) = 0;		//!< Sets the original return pointer
		virtual void SetOverrideRetPtr(void *ptr) = 0;			//!< Sets the override result pointer
		virtual bool ShouldContinue() = 0;						//!< Returns false if the hook loop should exit

		/**
		*	@brief Remove a hook manager. Auto-removes all hooks attached to it from plugin plug.
		*
		*	@param plug The owner of the hook manager
		*   @param pubFunc The hook manager's info function
		*/
		virtual void RemoveHookManager(Plugin plug, HookManagerPubFunc pubFunc) = 0;

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

		virtual void *GetOverrideRetPtr() = 0;		//!< Returns the pointer set by SetOverrideRetPtr

		/**
		*	@brief Set up the hook loop. Equivalent to calling:
		*	SetStatusPtr, SetPrevResPtr, SetCurResPtr, SetIfacePtrPtr, SetOrigRetPtr, Get/SetOverrideRetPtr
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
		virtual void *SetupHookLoop(META_RES *statusPtr, META_RES *prevResPtr, META_RES *curResPtr,
			void **ifacePtrPtr, const void *origRetPtr, void *overrideRetPtr) = 0;

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
		*	@param handler A pointer to a FastDelegate containing the hook handler
		*	@param post Set to true if you want a post handler
		*/
		virtual int AddHookNew(Plugin plug, AddHookMode mode, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan,
			ISHDelegate *handler, bool post) = 0;

		/**
		*	@brief Remove a VP hook by ID.
		*
		*	@return true on success, false otherwise
		*
		*	@param plug The unique identifier of the plugin that calls this function
		*	@param hookid The hook id (returned by AddHookNew)
		*/
		virtual bool RemoveHookByID(Plugin plug, int hookid) = 0;

		/**
		*	@brief Makes sure that hooks are going to be ignored on the next call of vfnptr
		*
		*	@param plug The unique identifier of the plugin that calls this function
		*	@param vfnptr The virtual function pointer of the function in question
		*/
		virtual void SetIgnoreHooks(Plugin plug, void *vfnptr) = 0;

		/**
		*	@brief Reverses SetIgnoreHooks' effect
		*
		*	@param plug The unique identifier of the plugin that calls this function
		*	@param vfnptr The virtual function pointer of the function in question
		*/
		virtual void ResetIgnoreHooks(Plugin plug, void *vfnptr) = 0;

		/**
		*	@brief Finds the original entry of a virtual function pointer
		*
		*	@param vfnptr The virtual function pointer
		*	@return The original entry if the virtual function pointer has been patched; NULL otherwise.
		*/
		virtual void *GetOrigVfnPtrEntry(void *vfnptr) = 0;
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

	// For source-level compatibility
	template <class T> struct CallClass
	{
		T *ptr;

		CallClass(T *p) : ptr(p)
		{
		}

		operator T*()
		{
			return ptr;
		}
	};

	typedef CallClass<void> GenericCallClass;
	typedef CallClass<EmptyClass> ManualCallClass;

	template <class T>
	CallClass<T> *GetCallClass(T *p)
	{
		return new CallClass<T>(p);
	}

	template <class T>
	void ReleaseCallClass(CallClass<T> *p)
	{
		delete p;
	}

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

// If a hook on a function which returns a reference does not want to specify a return value,
// it can use this macro.
//   ONLY USE THIS WITH MRES_IGNORED AND MRES_HANDLED !!!
#define RETURN_META_NOREF(result, rettype)	do { SET_META_RESULT(result); return reinterpret_cast<rettype>(*SH_GLOB_SHPTR); } while(0)

// only call these from the hook handlers directly!
// :TODO: enforce it ?

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
	SourceHook::EmptyClass *thisptr = reinterpret_cast<SourceHook::EmptyClass*>(SH_GLOB_SHPTR->GetIfacePtr()); \
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
	SourceHook::EmptyClass *thisptr = reinterpret_cast<SourceHook::EmptyClass*>(SH_GLOB_SHPTR->GetIfacePtr()); \
	u.s.addr = (*reinterpret_cast<void***>(reinterpret_cast<char*>(thisptr) + SH_MFHCls(hookname)::ms_MFI.vtbloffs))[ \
		SH_MFHCls(hookname)::ms_MFI.vtblindex]; \
	u.s.adjustor = 0;

#endif

#define RETURN_META_MNEWPARAMS(result, hookname, newparams) \
	do { \
		SET_META_RESULT(result); \
		SH_GLOB_SHPTR->DoRecall(); \
		SOUREHOOK__MNEWPARAMS_PREPAREMFP(hookname); \
		(thisptr->*(u.mfp)) newparams; \
		RETURN_META(MRES_SUPERCEDE); \
	} while (0)

#define RETURN_META_VALUE_MNEWPARAMS(result, value, hookname, newparams) \
	do { \
		SET_META_RESULT(result); \
		SH_GLOB_SHPTR->DoRecall(); \
		if ((result) >= MRES_OVERRIDE) \
		{ \
			/* see RETURN_META_VALUE_NEWPARAMS */ \
			SourceHook::SetOverrideResult<SH_MFHCls(hookname)::RetType>(SH_GLOB_SHPTR, value); \
		} \
		SOUREHOOK__MNEWPARAMS_PREPAREMFP(hookname); \
		RETURN_META_VALUE(MRES_SUPERCEDE, (thisptr->*(u.mfp)) newparams); \
	} while (0)

#define SH_MANUALHOOK_RECONFIGURE(hookname, pvtblindex, pvtbloffs, pthisptroffs) \
	do { \
		SH_GLOB_SHPTR->RemoveHookManager(SH_GLOB_PLUGPTR, SH_MFHCls(hookname)::HookManPubFunc); \
		SH_MFHCls(hookname)::ms_MFI.thisptroffs = pthisptroffs; \
		SH_MFHCls(hookname)::ms_MFI.vtblindex = pvtblindex; \
		SH_MFHCls(hookname)::ms_MFI.vtbloffs = pvtbloffs; \
	} while (0)


#define SH_GET_ORIG_VFNPTR_ENTRY(inst, mfp) (SourceHook::GetOrigVfnPtrEntry(inst, mfp, SH_GLOB_SHPTR))

// For source-level compatibility

#define SH_GET_CALLCLASS(ptr) SourceHook::GetCallClass(ptr)
#define SH_GET_MCALLCLASS(ptr, size) SourceHook::GetCallClass(reinterpret_cast<SourceHook::EmptyClass*>(ptr))
#define SH_RELEASE_CALLCLASS(ptr) SourceHook::ReleaseCallClass(ptr)

// New ADD / REMOVE macros.
#define SH_STATIC(func) fastdelegate::MakeDelegate(func)
#define SH_MEMBER(inst, func) fastdelegate::MakeDelegate(inst, func)

#define SH_ADD_HOOK(ifacetype, ifacefunc, ifaceptr, handler, post) \
	__SourceHook_FHAdd##ifacetype##ifacefunc((void*)SourceHook::implicit_cast<ifacetype*>(ifaceptr), \
	post, handler)

#define SH_REMOVE_HOOK(ifacetype, ifacefunc, ifaceptr, handler, post) \
	__SourceHook_FHRemove##ifacetype##ifacefunc((void*)SourceHook::implicit_cast<ifacetype*>(ifaceptr), \
	post, handler) 

#define SH_ADD_MANUALHOOK(hookname, ifaceptr, handler, post) \
	__SourceHook_FHMAdd##hookname(reinterpret_cast<void*>(ifaceptr), post, handler)

#define SH_REMOVE_MANUALHOOK(hookname, ifaceptr, handler, post) \
	__SourceHook_FHMRemove##hookname(reinterpret_cast<void*>(ifaceptr), post, handler) 

#define SH_ADD_VPHOOK(ifacetype, ifacefunc, ifaceptr, handler, post) \
	__SourceHook_FHVPAdd##ifacetype##ifacefunc((void*)SourceHook::implicit_cast<ifacetype*>(ifaceptr), \
	post, handler, false)

#define SH_ADD_DVPHOOK(ifacetype, ifacefunc, vtableptr, handler, post) \
	__SourceHook_FHVPAdd##ifacetype##ifacefunc(reinterpret_cast<void*>(vtableptr), \
	post, handler, true)

#define SH_ADD_MANUALVPHOOK(hookname, ifaceptr, handler, post) \
	__SourceHook_FHMVPAdd##hookname(reinterpret_cast<void*>(ifaceptr), post, handler, false)

#define SH_ADD_MANUALDVPHOOK(hookname, vtableptr, handler, post) \
	__SourceHook_FHMVPAdd##hookname(reinterpret_cast<void*>(vtableptr), post, handler, true)

#define SH_REMOVE_HOOK_ID(hookid) \
	(SH_GLOB_SHPTR->RemoveHookByID(SH_GLOB_PLUGPTR, hookid))

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
	SH_FHCls(ifacetype,ifacefunc,overload)() \
	{ \
		GetFuncInfo(funcptr, ms_MFI); \
	} \
	\
	static int HookManPubFunc(::SourceHook::HookManagerAction action, ::SourceHook::IHookManagerInfo *param) \
	{ \
		using namespace ::SourceHook; \
		GetFuncInfo(funcptr, ms_MFI); \
		/* Verify interface version */ \
		if (SH_GLOB_SHPTR->GetIfaceVersion() != SH_IFACE_VERSION) \
			return 1; \
		if (SH_GLOB_SHPTR->GetImplVersion() < SH_IMPL_VERSION) \
			return 1; \
		\
		if (action == HA_GetInfo) \
		{ \
			param->SetVersion(SH_HOOKMAN_VERSION); \
			param->SetInfo(ms_MFI.vtbloffs, ms_MFI.vtblindex, \
				reinterpret_cast<const char*>(&ms_Proto)); \
			\
			MemFuncInfo mfi = {true, -1, 0, 0}; \
			GetFuncInfo(&SH_FHCls(ifacetype,ifacefunc,overload)::Func, mfi); \
			param->SetHookfuncVfnptr( \
				reinterpret_cast<void**>(reinterpret_cast<char*>(&ms_Inst) + mfi.vtbloffs)[mfi.vtblindex]); \
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
	int __SourceHook_FHAdd##ifacetype##ifacefunc(void *iface, bool post, \
		SH_FHCls(ifacetype,ifacefunc,overload)::FD handler) \
	{ \
		using namespace ::SourceHook; \
		MemFuncInfo mfi = {true, -1, 0, 0}; \
		GetFuncInfo(funcptr, mfi); \
		if (mfi.thisptroffs < 0 || !mfi.isVirtual) \
			return false; /* No non-virtual functions / virtual inheritance supported */ \
		\
		return SH_GLOB_SHPTR->AddHookNew(SH_GLOB_PLUGPTR, ::SourceHook::ISourceHook::Hook_Normal, iface, mfi.thisptroffs, \
			SH_FHCls(ifacetype,ifacefunc,overload)::HookManPubFunc, \
			new CSHDelegate<SH_FHCls(ifacetype,ifacefunc,overload)::FD>(handler), post); \
	} \
	int __SourceHook_FHVPAdd##ifacetype##ifacefunc(void *iface, bool post, \
		SH_FHCls(ifacetype,ifacefunc,overload)::FD handler, bool direct) \
	{ \
		using namespace ::SourceHook; \
		MemFuncInfo mfi = {true, -1, 0, 0}; \
		GetFuncInfo(funcptr, mfi); \
		if (mfi.thisptroffs < 0 || !mfi.isVirtual) \
			return false; /* No non-virtual functions / virtual inheritance supported */ \
		\
		return SH_GLOB_SHPTR->AddHookNew(SH_GLOB_PLUGPTR, \
			direct ? ::SourceHook::ISourceHook::Hook_DVP : ::SourceHook::ISourceHook::Hook_VP, \
			iface, mfi.thisptroffs, SH_FHCls(ifacetype,ifacefunc,overload)::HookManPubFunc, \
			new CSHDelegate<SH_FHCls(ifacetype,ifacefunc,overload)::FD>(handler), post); \
	} \
	bool __SourceHook_FHRemove##ifacetype##ifacefunc(void *iface, bool post, \
		SH_FHCls(ifacetype,ifacefunc,overload)::FD handler) \
	{ \
		using namespace ::SourceHook; \
		MemFuncInfo mfi = {true, -1, 0, 0}; \
		GetFuncInfo(funcptr, mfi); \
		if (mfi.thisptroffs < 0) \
			return false; /* No virtual inheritance supported */ \
		\
		CSHDelegate<SH_FHCls(ifacetype,ifacefunc,overload)::FD> tmp(handler); \
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
		static int HookManPubFunc(::SourceHook::HookManagerAction action, ::SourceHook::IHookManagerInfo *param) \
		{ \
			using namespace ::SourceHook; \
			/* we don't set ms_MFI here because manual hookmans can be reconfigured */ \
			/* :FIXME: possible problem: someone adding a hook from a constructor of a global entity */ \
			/* which is construced before SH_MFHCls(hookname)() gets called? */ \
			/* Verify interface version */ \
			if (SH_GLOB_SHPTR->GetIfaceVersion() != SH_IFACE_VERSION) \
				return 1; \
			if (SH_GLOB_SHPTR->GetImplVersion() < SH_IMPL_VERSION) \
				return 1; \
			\
			if (action == HA_GetInfo) \
			{ \
				param->SetVersion(SH_HOOKMAN_VERSION); \
				param->SetInfo(ms_MFI.vtbloffs, ms_MFI.vtblindex, \
					reinterpret_cast<const char*>(&ms_Proto)); \
				\
				MemFuncInfo mfi = {true, -1, 0, 0}; \
				GetFuncInfo(&SH_MFHCls(hookname)::Func, mfi); \
				param->SetHookfuncVfnptr( \
					reinterpret_cast<void**>(reinterpret_cast<char*>(&ms_Inst) + mfi.vtbloffs)[mfi.vtblindex]); \
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

#define SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, pvtbloffs, pvtblidx, pthisptroffs) \
	}; \
	SH_MFHCls(hookname) SH_MFHCls(hookname)::ms_Inst; \
	::SourceHook::MemFuncInfo SH_MFHCls(hookname)::ms_MFI; \
	::SourceHook::IHookManagerInfo *SH_MFHCls(hookname)::ms_HI; \
	int __SourceHook_FHMAdd##hookname(void *iface, bool post, \
		SH_MFHCls(hookname)::FD handler) \
	{ \
		return SH_GLOB_SHPTR->AddHookNew(SH_GLOB_PLUGPTR, ::SourceHook::ISourceHook::Hook_Normal, iface, pthisptroffs, \
			SH_MFHCls(hookname)::HookManPubFunc, \
			new ::SourceHook::CSHDelegate<SH_MFHCls(hookname)::FD>(handler), post); \
	} \
	int __SourceHook_FHMVPAdd##hookname(void *iface, bool post, \
		SH_MFHCls(hookname)::FD handler, bool direct) \
	{ \
		return SH_GLOB_SHPTR->AddHookNew(SH_GLOB_PLUGPTR, \
			direct ? ::SourceHook::ISourceHook::Hook_DVP : ::SourceHook::ISourceHook::Hook_VP, \
			iface, pthisptroffs, SH_MFHCls(hookname)::HookManPubFunc, \
			new ::SourceHook::CSHDelegate<SH_MFHCls(hookname)::FD>(handler), post); \
	} \
	bool __SourceHook_FHMRemove##hookname(void *iface, bool post, \
		SH_MFHCls(hookname)::FD handler) \
	{ \
		::SourceHook::CSHDelegate<SH_MFHCls(hookname)::FD> tmp(handler); \
		return SH_GLOB_SHPTR->RemoveHook(SH_GLOB_PLUGPTR, iface, pthisptroffs, \
			SH_MFHCls(hookname)::HookManPubFunc, &tmp, post); \
	} \


#define SH_SETUPCALLS(rettype, paramtypes, params) \
	/* 1) Find the vfnptr */ \
	using namespace ::SourceHook; \
	void *ourvfnptr = reinterpret_cast<void*>( \
		*reinterpret_cast<void***>(reinterpret_cast<char*>(this) + ms_MFI.vtbloffs) + ms_MFI.vtblindex); \
	IVfnPtr *vfnptr = ms_HI->FindVfnPtr(ourvfnptr); \
	SH_ASSERT(vfnptr, ("Called with vfnptr 0x%p which couldn't be found in the list", ourvfnptr)); \
	\
	void *vfnptr_origentry = vfnptr->GetOrigEntry(); \
	/* ... and the iface */ \
	IIface *ifinfo = vfnptr->FindIface(reinterpret_cast<void*>(this)); \
	if (!ifinfo) \
	{ \
		/* The iface info was not found. Redirect the call to the original function. */ \
		rettype (EmptyClass::*mfp)paramtypes; \
		SH_SETUP_MFP(mfp); \
		return (reinterpret_cast<EmptyClass*>(this)->*mfp)params; \
	} \
	/* 2) Declare some vars and set it up */ \
	SH_GLOB_SHPTR->HookLoopBegin(ifinfo); \
	IHookList *prelist = ifinfo->GetPreHooks(); \
	IHookList *postlist = ifinfo->GetPostHooks(); \
	META_RES status = MRES_IGNORED; \
	META_RES prev_res; \
	META_RES cur_res; \
	typedef ReferenceCarrier<rettype>::type my_rettype; \
	my_rettype orig_ret; \
	my_rettype override_ret; \
	my_rettype plugin_ret; \
	void* ifptr; \
	my_rettype *pOverrideRet = reinterpret_cast<my_rettype*>(SH_GLOB_SHPTR->SetupHookLoop( \
		&status, &prev_res, &cur_res, &ifptr, &orig_ret, &override_ret));

#define SH_CALL_HOOKS(post, params) \
	if (SH_GLOB_SHPTR->ShouldContinue()) \
	{ \
		prev_res = MRES_IGNORED; \
		for (AutoHookIter iter(post##list); !iter.End(); iter.Next()) \
		{ \
			cur_res = MRES_IGNORED; \
			ifptr = reinterpret_cast<void*>(reinterpret_cast<char*>(this) - iter.ThisPtrOffs()); \
			plugin_ret = reinterpret_cast<CSHDelegate<FD>*>(iter.Handler())->GetDeleg() params; \
			prev_res = cur_res; \
			if (cur_res > status) \
				status = cur_res; \
			if (cur_res >= MRES_OVERRIDE) \
				*pOverrideRet = plugin_ret; \
			if (!SH_GLOB_SHPTR->ShouldContinue()) \
			{ \
				iter.SetToZero(); \
				break; \
			} \
		} \
	}

#define SH_CALL_ORIG(rettype, paramtypes, params) \
	if (status != MRES_SUPERCEDE) \
	{ \
		rettype (EmptyClass::*mfp)paramtypes; \
		SH_SETUP_MFP(mfp); \
		orig_ret = (reinterpret_cast<EmptyClass*>(this)->*mfp)params; \
	} \
	else \
		orig_ret = override_ret; \

#define SH_RETURN() \
	SH_GLOB_SHPTR->HookLoopEnd(); \
	return status >= MRES_OVERRIDE ? *pOverrideRet : orig_ret;

#define SH_HANDLEFUNC(paramtypes, params, rettype) \
	SH_SETUPCALLS(rettype, paramtypes, params) \
	SH_CALL_HOOKS(pre, params) \
	SH_CALL_ORIG(rettype, paramtypes, params) \
	SH_CALL_HOOKS(post, params) \
	SH_RETURN()

//////////////////////////////////////////////////////////////////////////
#define SH_SETUPCALLS_void(paramtypes, params) \
	/* 1) Find the vfnptr */ \
	using namespace ::SourceHook; \
	void *ourvfnptr = reinterpret_cast<void*>( \
		*reinterpret_cast<void***>(reinterpret_cast<char*>(this) + ms_MFI.vtbloffs) + ms_MFI.vtblindex); \
	IVfnPtr *vfnptr = ms_HI->FindVfnPtr(ourvfnptr); \
	SH_ASSERT(vfnptr, ("Called with vfnptr 0x%p which couldn't be found in the list", ourvfnptr)); \
	\
	void *vfnptr_origentry = vfnptr->GetOrigEntry(); \
	/* ... and the iface */ \
	IIface *ifinfo = vfnptr->FindIface(reinterpret_cast<void*>(this)); \
	if (!ifinfo) \
	{ \
		/* The iface info was not found. Redirect the call to the original function. */ \
		void (EmptyClass::*mfp)paramtypes; \
		SH_SETUP_MFP(mfp); \
		(reinterpret_cast<EmptyClass*>(this)->*mfp)params; \
		return; \
	} \
	/* 2) Declare some vars and set it up */ \
	SH_GLOB_SHPTR->HookLoopBegin(ifinfo); \
	IHookList *prelist = ifinfo->GetPreHooks(); \
	IHookList *postlist = ifinfo->GetPostHooks(); \
	META_RES status = MRES_IGNORED; \
	META_RES prev_res; \
	META_RES cur_res; \
	void* ifptr; \
	SH_GLOB_SHPTR->SetupHookLoop(&status, &prev_res, &cur_res, &ifptr, NULL, NULL); \

#define SH_CALL_HOOKS_void(post, params) \
	if (SH_GLOB_SHPTR->ShouldContinue()) \
	{ \
		prev_res = MRES_IGNORED; \
		for (AutoHookIter iter(post##list); !iter.End(); iter.Next()) \
		{ \
			cur_res = MRES_IGNORED; \
			ifptr = reinterpret_cast<void*>(reinterpret_cast<char*>(this) - iter.ThisPtrOffs()); \
			reinterpret_cast<CSHDelegate<FD>*>(iter.Handler())->GetDeleg() params; \
			prev_res = cur_res; \
			if (cur_res > status) \
				status = cur_res; \
			if (!SH_GLOB_SHPTR->ShouldContinue()) \
			{ \
				iter.SetToZero(); \
				break; \
			} \
		} \
	}

#define SH_CALL_ORIG_void(paramtypes, params) \
	if (status != MRES_SUPERCEDE) \
	{ \
		void (EmptyClass::*mfp)paramtypes; \
		SH_SETUP_MFP(mfp); \
		(reinterpret_cast<EmptyClass*>(this)->*mfp)params; \
	}

#define SH_RETURN_void() \
	SH_GLOB_SHPTR->HookLoopEnd();

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

// :FIXME:
//  sizeof on references returns the size of the datatype, NOT the pointer size or something
//  -> one should probably flag references in __SourceHook_ParamSizes_* !
//		or simply assume that their size is sizeof(void*)=SH_PTRSIZE... could be doable through a simple template

@[$1,0,$a:
// ********* Support for $1 arguments *********
#define SH_DECL_HOOK$1(ifacetype, ifacefunc, attr, overload, rettype@[$2,1,$1:, param$2@]) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(@[$2,1,$1|, :param$2@]) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate$1<@[$2,1,$1|, :param$2@]@[$1!=0:, @]rettype> FD; \
		virtual rettype Func(@[$2,1,$1|, :param$2 p$2@]) \
		{ SH_HANDLEFUNC((@[$2,1,$1|, :param$2@]), (@[$2,1,$1|, :p$2@]), rettype); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(@[$2,1,$1|, :param$2@]) attr>(&ifacetype::ifacefunc))) \
	\
	const int __SourceHook_ParamSizes_##ifacetype##ifacefunc##overload[] = { 0@[$2,1,$1:, sizeof(param$2)@] }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto(sizeof(rettype), \
		$1, __SourceHook_ParamSizes_##ifacetype##ifacefunc##overload);

#define SH_DECL_HOOK$1_void(ifacetype, ifacefunc, attr, overload@[$2,1,$1:, param$2@]) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(@[$2,1,$1|, :param$2@]) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate$1<@[$2,1,$1|, :param$2@]> FD; \
		virtual void Func(@[$2,1,$1|, :param$2 p$2@]) \
		{ SH_HANDLEFUNC_void((@[$2,1,$1|, :param$2@]), (@[$2,1,$1|, :p$2@])); } \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(@[$2,1,$1|, :param$2@]) attr>(&ifacetype::ifacefunc))) \
	\
	\
	const int __SourceHook_ParamSizes_##ifacetype##ifacefunc##overload[] = { 0@[$2,1,$1:, sizeof(param$2)@] }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto(0, \
		$1, __SourceHook_ParamSizes_##ifacetype##ifacefunc##overload);

#define SH_DECL_HOOK$1_vafmt(ifacetype, ifacefunc, attr, overload, rettype@[$2,1,$1:, param$2@]) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<rettype (ifacetype::*)(@[$2,1,$1|, :param$2@]@[$1!=0:, @]const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate@($1+1)<@[$2,1,$1|, :param$2@]@[$1!=0:, @]const char *, rettype> FD; \
		virtual rettype Func(@[$2,1,$1|, :param$2 p$2@]@[$1!=0:, @]const char *fmt, ...) \
		{ \
			char buf[::SourceHook::STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_vafmt((@[$2,1,$1|, :param$2@]@[$1!=0:, @]...), (@[$2,1,$1|, :p$2@]@[$1!=0:, @]"%s", buf), (@[$2,1,$1|, :p$2@]@[$1!=0:, @]buf), rettype); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<rettype (ifacetype::*)(@[$2,1,$1|, :param$2@]@[$1!=0:, @]const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const int __SourceHook_ParamSizes_##ifacetype##ifacefunc##overload[] = { -1@[$2,1,$1:, sizeof(param$2)@] }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto(sizeof(rettype), \
		$1, __SourceHook_ParamSizes_##ifacetype##ifacefunc##overload);

#define SH_DECL_HOOK$1_void_vafmt(ifacetype, ifacefunc, attr, overload@[$2,1,$1:, param$2@]) \
	SHINT_MAKE_GENERICSTUFF_BEGIN(ifacetype, ifacefunc, overload, (static_cast<void (ifacetype::*)(@[$2,1,$1|, :param$2@]@[$1!=0:, @]const char *, ...) attr> \
		(&ifacetype::ifacefunc))) \
		typedef fastdelegate::FastDelegate@($1+1)<@[$2,1,$1|, :param$2@]@[$1!=0:, @]const char *> FD; \
		virtual void Func(@[$2,1,$1|, :param$2 p$2@]@[$1!=0:, @]const char *fmt, ...) \
		{ \
			char buf[::SourceHook::STRBUF_LEN]; \
			va_list ap; \
			va_start(ap, fmt); \
			vsnprintf(buf, sizeof(buf), fmt, ap); \
			va_end(ap); \
			SH_HANDLEFUNC_void_vafmt((@[$2,1,$1|, :param$2@]@[$1!=0:, @]...), (@[$2,1,$1|, :p$2@]@[$1!=0:, @]"%s", buf), (@[$2,1,$1|, :p$2@]@[$1!=0:, @]buf)); \
		} \
	SHINT_MAKE_GENERICSTUFF_END(ifacetype, ifacefunc, overload, \
		(static_cast<void (ifacetype::*)(@[$2,1,$1|, :param$2@]@[$1!=0:, @]const char *, ...) attr>(&ifacetype::ifacefunc))) \
	\
	const int __SourceHook_ParamSizes_##ifacetype##ifacefunc##overload[] = { -1@[$2,1,$1:, sizeof(param$2)@] }; \
	::SourceHook::ProtoInfo SH_FHCls(ifacetype, ifacefunc, overload)::ms_Proto(0, \
		$1, __SourceHook_ParamSizes_##ifacetype##ifacefunc##overload);

#define SH_DECL_MANUALHOOK$1(hookname, vtblidx, vtbloffs, thisptroffs, rettype@[$2,1,$1:, param$2@]) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate$1<@[$2,1,$1|, :param$2@]@[$1!=0:, @]rettype> FD; \
		virtual rettype Func(@[$2,1,$1|, :param$2 p$2@]) \
		{ SH_HANDLEFUNC((@[$2,1,$1|, :param$2@]), (@[$2,1,$1|, :p$2@]), rettype); } \
		typedef rettype(::SourceHook::EmptyClass::*ECMFP)(@[$2,1,$1|, :param$2@]); \
		typedef rettype RetType; \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const int __SourceHook_ParamSizesM_##hookname[] = { 0@[$2,1,$1:, sizeof(param$2)@] }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto(sizeof(rettype), \
		$1, __SourceHook_ParamSizesM_##hookname); \

#define SH_DECL_MANUALHOOK$1_void(hookname, vtblidx, vtbloffs, thisptroffs@[$2,1,$1:, param$2@]) \
	SHINT_MAKE_GENERICSTUFF_BEGIN_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
		typedef fastdelegate::FastDelegate$1<@[$2,1,$1|, :param$2@]> FD; \
		virtual void Func(@[$2,1,$1|, :param$2 p$2@]) \
		{ SH_HANDLEFUNC_void((@[$2,1,$1|, :param$2@]), (@[$2,1,$1|, :p$2@])); } \
		typedef void(::SourceHook::EmptyClass::*ECMFP)(@[$2,1,$1|, :param$2@]); \
	SHINT_MAKE_GENERICSTUFF_END_MANUAL(hookname, vtbloffs, vtblidx, thisptroffs) \
	\
	const int __SourceHook_ParamSizesM_##hookname[] = { 0@[$2,1,$1:, sizeof(param$2)@] }; \
	::SourceHook::ProtoInfo SH_MFHCls(hookname)::ms_Proto(0, \
		$1, __SourceHook_ParamSizesM_##hookname); \

@]


//////////////////////////////////////////////////////////////////////////
// SH_CALL

#define SH_MAKE_EXECUTABLECLASS_OB(call, prms) \
{ \
	using namespace ::SourceHook; \
	\
	m_pSH->SetIgnoreHooks(m_Plug, m_VfnPtr); \
	RetType tmpret = (m_ThisPtr->*m_MFP)call; \
	m_pSH->ResetIgnoreHooks(m_Plug, m_VfnPtr); \
	return tmpret; \
}

#define SH_MAKE_EXECUTABLECLASS_OB_void(call, prms) \
{ \
	using namespace ::SourceHook; \
	\
	m_pSH->SetIgnoreHooks(m_Plug, m_VfnPtr); \
	(m_ThisPtr->*m_MFP)call; \
	m_pSH->ResetIgnoreHooks(m_Plug, m_VfnPtr); \
}

namespace SourceHook
{
	// Call Class Wrapper!
	template <class T> struct CCW
	{
		typedef T type;

		// Get Real Pointer!
		static inline T *GRP(T *p)
		{
			return p;
		}
	};

	template <class T> struct CCW< CallClass<T> >
	{
		typedef T type;

		// Get Real Pointer!
		static inline T *GRP(CallClass<T> *p)
		{
			return p->ptr;
		}
	};

@[$1,0,$a:
	// Support for $1 arguments
	template<class ObjType, class MFP, class RetType@[$2,1,$1:, class Param$2@]> class ExecutableClass$1
	{
		ObjType *m_ThisPtr;
		void *m_VfnPtr;
		MFP m_MFP;
		ISourceHook *m_pSH;
		Plugin m_Plug;
	public:
		ExecutableClass$1(ObjType *tp, MFP mfp, void *vp, ISourceHook *pSH, Plugin plug) : m_ThisPtr(tp),
			m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH), m_Plug(plug) { }
	
		RetType operator()(@[$2,1,$1|, :Param$2 p$2@]) const
			SH_MAKE_EXECUTABLECLASS_OB((@[$2,1,$1|, :p$2@]), (@[$2,1,$1|, :Param$2@]))
	         
		@[$2,$1+1,$a:
		template <@[$3,$1+1,$2|, :class Param$3@]> RetType operator()(@[$3,1,$2|, :Param$3 p$3@]) const
			SH_MAKE_EXECUTABLECLASS_OB((@[$3,1,$2|, :p$3@]), (@[$3,1,$2|, :Param$3@]))
		@]
	};

	template<class ObjType, class MFP@[$2,1,$1:, class Param$2@]> class ExecutableClass$1<ObjType, MFP, void@[$2,1,$1:, Param$2@]>
	{
	   ObjType *m_ThisPtr;
	   void *m_VfnPtr;
	   MFP m_MFP;
	   ISourceHook *m_pSH;
	   Plugin m_Plug;
	public:
	   ExecutableClass$1(ObjType *tp, MFP mfp, void *vp, ISourceHook *pSH, Plugin plug) : m_ThisPtr(tp),
		   m_VfnPtr(vp), m_MFP(mfp), m_pSH(pSH), m_Plug(plug) { }
	
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
	GetFuncInfo(CCW<Y>::GRP(ptr), mfp, mfi); \
	void *vfnptr = reinterpret_cast<void*>( \
		*reinterpret_cast<void***>(reinterpret_cast<char*>(CCW<Y>::GRP(ptr)) + mfi.thisptroffs + mfi.vtbloffs) + mfi.vtblindex);

#define SH__CALL_GET_VFNPTR_MANUAL \
	using namespace ::SourceHook; \
	void *vfnptr = reinterpret_cast<void*>( \
		*reinterpret_cast<void***>( (reinterpret_cast<char*>(CCW<Y>::GRP(ptr)) + thisptroffs + vtbloffs) ) + vtblidx); \
	/* patch mfp */ \
	*reinterpret_cast<void**>(&mfp) = *reinterpret_cast<void**>(vfnptr);

// SH_CALL needs to deduce the return type -> it uses templates and function overloading
// That's why SH_CALL takes two parameters: "mfp2" of type RetType(X::*mfp)(params), and "mfp" of type MFP
// The only purpose of the mfp2 parameter is to extract the return type

@[$1,0,$a:
// Support for $1 arguments
template <class X, class Y, class MFP, class RetType@[$2,1,$1:, class Param$2@]>
SourceHook::ExecutableClass$1<typename SourceHook::CCW<Y>::type, MFP, RetType@[$2,1,$1:, Param$2@]>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(@[$2,1,$1|, :Param$2@]), SourceHook::ISourceHook *shptr, SourceHook::Plugin plug)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass$1<typename CCW<Y>::type, MFP, RetType@[$2,1,$1:, Param$2@]>(CCW<Y>::GRP(ptr),
		mfp, vfnptr, shptr, plug);
}

template <class X, class Y, class MFP, class RetType@[$2,1,$1:, class Param$2@]>
SourceHook::ExecutableClass$1<typename SourceHook::CCW<Y>::type, MFP, RetType@[$2,1,$1:, Param$2@]>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(@[$2,1,$1|, :Param$2@])const, SourceHook::ISourceHook *shptr, SourceHook::Plugin plug)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass$1<typename CCW<Y>::type, MFP, RetType@[$2,1,$1:, Param$2@]>(CCW<Y>::GRP(ptr),
		mfp, vfnptr, shptr, plug);
}

template <class X, class Y, class MFP, class RetType@[$2,1,$1:, class Param$2@]>
SourceHook::ExecutableClass$1<SourceHook::EmptyClass, MFP, RetType@[$2,1,$1:, Param$2@]>
SH_MCALL3(Y *ptr, MFP mfp, RetType(X::*mfp2)(@[$2,1,$1|, :Param$2@]), int vtblidx, int vtbloffs, int thisptroffs, SourceHook::ISourceHook *shptr, SourceHook::Plugin plug)
{
	SH__CALL_GET_VFNPTR_MANUAL
	return SourceHook::ExecutableClass$1<EmptyClass, MFP, RetType@[$2,1,$1:, Param$2@]>(
		reinterpret_cast<SourceHook::EmptyClass*>(CCW<Y>::GRP(ptr)), mfp, vfnptr, shptr, plug);
}
@]

#if SH_COMP != SH_COMP_MSVC || _MSC_VER > 1300
// GCC & MSVC 7.1 need this, MSVC 7.0 doesn't like it

@[$1,0,$a:
// Support for $1 arguments
template <class X, class Y, class MFP, class RetType@[$2,1,$1:, class Param$2@]>
SourceHook::ExecutableClass$1<typename SourceHook::CCW<Y>::type, MFP, RetType@[$2,1,$1:, Param$2@]>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(@[$2,1,$1|, :Param$2@]@[$1!=0:, @]...), SourceHook::ISourceHook *shptr, SourceHook::Plugin plug)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass$1<typename CCW<Y>::type, MFP, RetType@[$2,1,$1:, Param$2@]>(CCW<Y>::GRP(ptr),
		mfp, vfnptr, shptr, plug);
}

template <class X, class Y, class MFP, class RetType@[$2,1,$1:, class Param$2@]>
SourceHook::ExecutableClass$1<typename SourceHook::CCW<Y>::type, MFP, RetType@[$2,1,$1:, Param$2@]>
SH_CALL2(Y *ptr, MFP mfp, RetType(X::*mfp2)(@[$2,1,$1|, :Param$2@]@[$1!=0:, @]...)const, SourceHook::ISourceHook *shptr, SourceHook::Plugin plug)
{
	SH__CALL_GET_VFNPTR_NORMAL
	return SourceHook::ExecutableClass$1<typename CCW<Y>::type, MFP, RetType@[$2,1,$1:, Param$2@]>(CCW<Y>::GRP(ptr),
		mfp, vfnptr, shptr, plug);
}

@]

#endif

#define SH_CALL(ptr, mfp) SH_CALL2((ptr), (mfp), (mfp), SH_GLOB_SHPTR, SH_GLOB_PLUGPTR)
#define SH_MCALL2(ptr, mfp, vtblidx, vtbloffs, thisptroffs) SH_MCALL3((ptr), (mfp), (mfp), (vtblidx), (vtbloffs), (thisptroffs), SH_GLOB_SHPTR, SH_GLOB_PLUGPTR)
#define SH_MCALL(ptr, mhookname) SH_MCALL2((ptr), SH_MFHCls(mhookname)::ECMFP(), SH_MFHCls(mhookname)::ms_MFI.vtblindex, \
	SH_MFHCls(mhookname)::ms_MFI.vtbloffs, SH_MFHCls(mhookname)::ms_MFI.thisptroffs)

#undef SH_MAKE_EXECUTABLECLASS_OB

//////////////////////////////////////////////////////////////////////////
// SetOverrideRet and RecallGetIface for recalls
// These take a ISourceHook pointer instead of using SH_GLOB_SHPTR directly
// The reason is that the user may want to redefine SH_GLOB_SHPTR - then the macros
// (META_RETURN_VALUE_NEWPARAMS) should obey the new pointer.

namespace SourceHook
{
	template <class RetType>
	void SetOverrideResult(ISourceHook *shptr, const RetType res)
	{
		*reinterpret_cast<RetType*>(shptr->GetOverrideRetPtr()) = res;
	}

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
@[$1,0,$a:
	template <class Iface, class RetType@[$2,1,$1:, class Param$2@]>
	OverrideFunctor<RetType> SetOverrideResult(RetType (Iface::*mfp)(@[$2,1,$1|, :Param$2@]))
	{
		return OverrideFunctor<RetType>();
	}

	template <class Iface, class RetType@[$2,1,$1:, class Param$2@]>
	Iface *RecallGetIface(ISourceHook *shptr, RetType (Iface::*mfp)(@[$2,1,$1|, :Param$2@]))
	{
		return reinterpret_cast<Iface*>(shptr->GetIfacePtr());
	}
@]
}

#endif
	// The pope is dead. -> :(
