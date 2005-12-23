/* ======== SourceHook ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

#ifndef __SOURCEHOOK_IMPL_H__
#define __SOURCEHOOK_IMPL_H__

#include "sourcehook.h"
#include "sh_list.h"
#include "sh_vector.h"
#include "sh_tinyhash.h"
#include "sh_stack.h"

/*

IMPLEMENTATION INFO

---------------------------------------
Protos ("Prototypes")
	The purpose of protos is to provide the amount of type information about a function
	which is required to be able to execute a function call without corrupting the stack.
	Our protos do not fully do this, but they provide the size of the return value, the amount of
	parameters, and the size of each parameter, which is enough for most situations,

	There are two version of protos:
	OLD:
		C-Style strings.

		0_void:
		"attrib"
		1_void:
		"attrib|param1_type"
		2_void:
		"attrib|param1_type|param2_type
		0:
		"attrib|ret_type"
		1:
		"attrib|ret_type|param1_type"
		2:
		"attrib|ret_type|param2_type"

		Old protos are deprecated.

	NEW:
		New protos are in fact pointers to the ProtoInfo structure (see sourcehook.h)

	Old protos begin with a non-zero byte, new protos begin with a zero byte.

	Protos are usually stored in a CProto instance.

---------------------------------------
Hook managers and hook manager containers
	Each hookman container is tied to _one_ proto/vtable index/vtable offset info.
	Hookman containers then contain a list of hook managers provided by plugins, sorted by version.
	(higher versions come first)

	Duplicate hook managers are ignored (ie. hook managers where proto, vtable index, vtable offset,
	plugin, version are the same as in an already exisiting hook manager)

	A new hook manager is always added to the end of the version group in the corresponding
	hook container. 

	If the new hook manager was added to the beginning of the container (which only happens if
	it is the first one or if it has a higher version than the previously first hook manager),
	the now second hook manager is shut down and the new hook manager takes its job.

	A "hook manager container id" (HMCI) consits of three values: proto, vtbl index, vtbl offset.
---------------------------------------
Hooks 
	When adding a hook, first the proposed hook manager is added to the corresponding
	hook manager container as described above.

	Then the first hook manager in the the manhook container is chosen to handle the function.

	Removing a hook does not neccessarily unreigster the plugin's hook manager. In order to do this,
	use RemoveHookManager or UnloadPlugin/

	Hooks can be paused - they remain in memory but they are not called. In SH, the hook iterator
	classes handle pausing transparently.

	The hook loop is supposed to call ShouldContinue before each iteration. This makes hook handlers
	able to remove themselves.

---------------------------------------
Call classes
	Call classes are identified by a this pointer and an instance size

	We use the instance size because a derived class instance and a base class instance could
	have the same this pointers, and we want to avoid that the derived class
	(which could be bigger) gets the same callclass as the base class (mainly if the
	base class' callclass was requested first).

	Call classes are reference counted.

	The original function pointers are stored in a vector (in order to allow fast random access).
	These vectors are stored as the value type of a hash. The key type is int and represents the
	vtable offset.

	If the hash key doesn't exist or the vtblidx is out of range or the corresponding element in the
	vector is NULL, there is no hook on that function.

---------------------------------------
Recalls
	Recalls are used for the META_RETURN_(VALUE_)NEWPARAMS macros, ie. to change the parameters
	in the hook loop on the fly.
	
	First, the macro calls DoRecall(), then it calls the function the hook is attached to -> it 
	calls the hookfunc. SourceHook makes sure that the newly invoked hook loop starts where the last
	one left off and that status variables like status, previous result, override return are kept.
	When this recurisvely called hookfunc returns, the macro returns what it returned
	(using MRES_SUPERCEDE). CSourceHookImpl returns false from ShouldContinue so the original hook loop
	is abandonned.
*/

namespace SourceHook
{
	/**
	*	@brief The SourceHook implementation class
	*/
	class CSourceHookImpl : public ISourceHook
	{
	private:
		class CProto
		{
			const char *m_Proto;

			static bool Equal(const char *p1, const char *p2);
		public:
			CProto(const char *szProto) : m_Proto(szProto)
			{
			}

			CProto(const CProto &other) : m_Proto(other.m_Proto)
			{
			}

			void operator = (const char *szProto)
			{
				m_Proto = szProto;
			}
			void operator = (const CProto &other)
			{
				m_Proto = other.m_Proto;
			}

			bool operator == (const char *szProto) const
			{
				return Equal(szProto, m_Proto);
			}
			bool operator == (const CProto &other) const
			{
				return Equal(other.m_Proto, m_Proto);
			}
		};


		/**
		*	@brief A hook can be removed if you have this information
		*/
		struct RemoveHookInfo
		{
			RemoveHookInfo(Plugin pplug, void *piface, int tpo, HookManagerPubFunc phookman,
				ISHDelegate *phandler, bool ppost)
				: plug(pplug), iface(piface), thisptr_offs(tpo),
				hookman(phookman), handler(phandler), post(ppost)
			{
			}

			Plugin plug;
			void *iface;
			int thisptr_offs;
			HookManagerPubFunc hookman;
			ISHDelegate *handler;
			bool post;
		};

		struct RemoveHookManInfo
		{
			RemoveHookManInfo(Plugin pplug, HookManagerPubFunc phookman)
				: plug(pplug), hookman(phookman)
			{
			}

			Plugin plug;
			HookManagerPubFunc hookman;
		};

		struct HookInfo
		{
			ISHDelegate *handler;			//!< Pointer to the handler
			bool paused;					//!< If true, the hook should not be executed
			Plugin plug;					//!< The owner plugin
			int thisptr_offs;				//!< This pointer offset
		};

		class CHookList : public IHookList
		{
		public:
			List<HookInfo> m_List;

			friend class CIter;

			class CIter : public IHookList::IIter
			{
				friend class CHookList;

				CHookList *m_pList;

				void SkipPaused();
			public:

				List<HookInfo>::iterator m_Iter;

				CIter(CHookList *pList);

				virtual ~CIter();

				void GoToBegin();
				void Set(CIter *pOther);

				bool End();
				void Next();
				ISHDelegate *Handler();
				int ThisPtrOffs();

				void Clear();

				CIter *m_pNext;		// When stored in m_FreeIters and m_UsedIters
				CIter *m_pPrev;		// Only used when stored in m_UsedIters
			};

			CIter *m_FreeIters;
			CIter *m_UsedIters;		// The last returned and not-yet-released iter is always m_UsedIters

			// For recalls
			bool m_Recall;

			void SetRecallState();	// Sets the list into a state where the next returned
									// iterator (from GetIter) will be a copy of the last
									// returned iterator, incremented by one. This is used in Recalls.
									// The hook resets this state automatically on:
									// GetIter, ReleaseIter

			CHookList();
			CHookList(const CHookList &other);
			virtual ~CHookList();

			void operator=(const CHookList &other);

			IIter *GetIter();
			void ReleaseIter(IIter *pIter);
		};

		// I know, data hiding... But I'm a lazy bastard!

		class CIface : public IIface
		{
		public:
			void *m_Ptr;
			CHookList m_PreHooks;
			CHookList m_PostHooks;
		public:
			CIface(void *ptr);
			virtual ~CIface();

			void *GetPtr();
			IHookList *GetPreHooks();
			IHookList *GetPostHooks();

			bool operator==(void *ptr)
			{
				return m_Ptr == ptr;
			}
		};

		class CVfnPtr : public IVfnPtr
		{
		public:
			typedef List<CIface> IfaceList;
			typedef IfaceList::iterator IfaceListIter;

			void *m_Ptr;
			void *m_OrigEntry;

			IfaceList m_Ifaces;

		public:
			CVfnPtr(void *ptr);
			virtual ~CVfnPtr();

			void *GetVfnPtr();
			void *GetOrigEntry();

			virtual IIface *FindIface(void *ptr);

			bool operator==(void *ptr)
			{
				return m_Ptr == ptr;
			}
		};

		class CHookManagerInfo : public IHookManagerInfo
		{
		public:
			typedef List<CVfnPtr> VfnPtrList;
			typedef VfnPtrList::iterator VfnPtrListIter;

			Plugin m_Plug;
			HookManagerPubFunc m_Func;

			int m_VtblOffs;
			int m_VtblIdx;
			const char *m_Proto;
			void *m_HookfuncVfnptr;

			VfnPtrList m_VfnPtrs;

			int m_HookManVersion;
		public:
			CHookManagerInfo();
			virtual ~CHookManagerInfo();

			IVfnPtr *FindVfnPtr(void *vfnptr);

			void SetInfo(int vtbl_offs, int vtbl_idx, const char *proto);
			void SetHookfuncVfnptr(void *hookfunc_vfnptr);

			void SetVersion(int version);

			bool operator < (const CHookManagerInfo &other)
			{
				return m_HookManVersion < other.m_HookManVersion;
			}

			struct Descriptor
			{
				Descriptor(Plugin pplug, HookManagerPubFunc ppubFunc) : plug(pplug), pubFunc(ppubFunc)
				{
				}

				Plugin plug;
				HookManagerPubFunc pubFunc;
			};

			bool operator == (const Descriptor desc)
			{
				return m_Func == desc.pubFunc && m_Plug == desc.plug;
			}
		};

		typedef List<CHookManagerInfo> HookManInfoList;

		class CHookManagerContainer : public HookManInfoList
		{
		public:
			// HMCI (Hook Manager Container Identification)
			class HMCI
			{
				CProto m_Proto;
				int m_VtableOffset;
				int m_VtableIndex;
			public:
				HMCI(const char *proto, int vtbloffs, int vtblidx) :
				  m_Proto(proto), m_VtableOffset(vtbloffs), m_VtableIndex(vtblidx)
				  {
				  }
				  ~HMCI()
				  {
				  }

				  bool operator==(const HMCI &other) const
				  {
					  return
						  other.m_VtableIndex == m_VtableIndex &&
						  other.m_Proto == m_Proto &&
						  other.m_VtableOffset == m_VtableOffset;
				  }

				  const CProto &GetProto() const
				  {
					  return m_Proto;
				  }
				  int GetVtableOffset() const
				  {
					  return m_VtableOffset;
				  }
				  int GetVtableIndex() const
				  {
					  return m_VtableIndex;
				  }
			};
			HMCI m_HCMI;

		public:
			CHookManagerContainer(const HMCI &hmci) : m_HCMI(hmci)
			{
			}
			bool operator == (const HMCI &other) const
			{
				return m_HCMI == other;
			}
			void AddHookManager(Plugin plug, const CHookManagerInfo &hookman);
		};

		class CCallClassImpl : public GenericCallClass
		{
		public:

			typedef SourceHook::CVector<void*> OrigFuncs;
			typedef SourceHook::THash<int, OrigFuncs> OrigVTables;

			void *m_Ptr;			//!< Pointer to the actual object
			size_t m_ObjSize;		//!< Size of the instance
			OrigVTables m_VT;		//!< Info about vtables & functions

			int m_RefCounter;

			CCallClassImpl(void *ptr, size_t size);
			virtual ~CCallClassImpl();

			bool operator==(void *other)
			{
				return m_Ptr == other;
			}

			void *GetThisPtr();
			void *GetOrigFunc(int vtbloffs, int vtblidx);

			void ApplyCallClassPatch(int vtbl_offs, int vtbl_idx, void *orig_entry);
			void RemoveCallClassPatch(int vtbl_offs, int vtbl_idx);
		};

		/**
		*	@brief A list of CallClass structures
		*/
		typedef List<CCallClassImpl> Impl_CallClassList;

		Impl_CallClassList m_CallClasses;			//!< A list of already generated callclasses

		/**
		*	@brief A list of CHookManagerContainers
		*/
		typedef List<CHookManagerContainer> HookManContList;

		HookManContList m_HookMans;					//!< A list of hook managers

		struct HookLoopInfo
		{
			META_RES *pStatus;
			META_RES *pPrevRes;
			META_RES *pCurRes;

			bool shouldContinue;
			bool recall;							//!< True if we're in a recall, eh.

			IIface *pCurIface;
			const void *pOrigRet;
			void *pOverrideRet;
			void **pIfacePtrPtr;
		};
		typedef CStack<HookLoopInfo> HookLoopInfoStack;

		void ApplyCallClassPatches(CCallClassImpl &cc);
		void ApplyCallClassPatches(void *ifaceptr, int vtbl_offs, int vtbl_idx, void *orig_entry);
		void RemoveCallClassPatches(void *ifaceptr, int vtbl_offs, int vtbl_idx);

		void SetPluginPaused(Plugin plug, bool paused);

		HookLoopInfoStack m_HLIStack;
	public:
		CSourceHookImpl();
		virtual ~CSourceHookImpl();

		/**
		*	@brief Returns the interface version
		*/
		int GetIfaceVersion();

		/**
		*	@brief Returns the implemnetation version
		*/
		int GetImplVersion();

		/**
		*	@brief Make sure that a plugin is not used by any other plugins anymore, and unregister all its hook managers
		*/
		void UnloadPlugin(Plugin plug);

		/**
		*	@brief Shut down the whole system, unregister all hook managers
		*/
		void CompleteShutdown();

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
		bool AddHook(Plugin plug, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan, ISHDelegate *handler, bool post);

		/**
		*	@brief Removes a hook.
		*
		*	@return True if the function succeeded, false otherwise
		*
		*	@param plug The unique identifier of the plugin that calls this function
		*	@param iface The interface pointer
		*	@param thisptr_offs This pointer adjuster
		*	@param myHookMan A hook manager function that should be capable of handling the function
		*	@param handler A pointer to a FastDelegate containing the hook handler
		*	@param post Set to true if you want a post handler
		*/
		bool RemoveHook(Plugin plug, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan, ISHDelegate *handler, bool post);

		/**
		*	@brief Removes a hook.
		*
		*	@ return True if the function succeeded, false otherwise
		*
		*	@param info A RemoveHookInfo structure, describing the hook
		*/
		bool RemoveHook(RemoveHookInfo info);

		/**
		*	@brief Checks whether a plugin has (a) hook manager(s) that is/are currently used by other plugins
		*
		*	@param plug The unique identifier of the plugin in question
		*/
		bool IsPluginInUse(Plugin plug);

		/**
		*	@brief Pauses all hooks of a plugin
		*
		*	@param plug The unique identifier of the plugin
		*/
		void PausePlugin(Plugin plug);

		/**
		*	@brief Unpauses all hooks of a plugin
		*
		*	@param plug The unique identifier of the plugin
		*/
		void UnpausePlugin(Plugin plug);

		/**
		*	@brief Return a pointer to a callclass. Generate a new one if required.
		*
		*	@param iface The interface pointer
		*	@param size Size of the class instance
		*/
		GenericCallClass *GetCallClass(void *iface, size_t size);

		/**
		*	@brief Release a callclass
		*
		*	@param ptr Pointer to the callclass
		*/
		virtual void ReleaseCallClass(GenericCallClass *ptr);

		virtual void SetRes(META_RES res);				//!< Sets the meta result
		virtual META_RES GetPrevRes();					//!< Gets the meta result of the previously called handler
		virtual META_RES GetStatus();					//!< Gets the highest meta result
		virtual const void *GetOrigRet();				//!< Gets the original result. If not in post function, undefined
		virtual const void *GetOverrideRet();			//!< Gets the override result. If none is specified, NULL
		virtual void *GetIfacePtr();					//!< Gets the interface pointer

		//////////////////////////////////////////////////////////////////////////
		// For hook managers
		void HookLoopBegin(IIface *pIface);			//!< Should be called when a hook loop begins
		void HookLoopEnd();							//!< Should be called when a hook loop exits
		void SetCurResPtr(META_RES *mres);			//!< Sets pointer to the current meta result
		void SetPrevResPtr(META_RES *mres);			//!< Sets pointer to previous meta result
		void SetStatusPtr(META_RES *mres);			//!< Sets pointer to the status variable
		void SetIfacePtrPtr(void **pp);				//!< Sets pointer to the interface this pointer
		void SetOrigRetPtr(const void *ptr);		//!< Sets the original return pointer
		void SetOverrideRetPtr(void *ptr);			//!< Sets the override result pointer
		bool ShouldContinue();						//!< Returns false if the hook loop should exit

		/**
		*	@brief Remove a hook manager. Auto-removes all hooks attached to it from plugin plug.
		*
		*	@param plug The owner of the hook manager
		*   @param pubFunc The hook manager's info function
		*/
		virtual void RemoveHookManager(Plugin plug, HookManagerPubFunc pubFunc);
		virtual void RemoveHookManager(RemoveHookManInfo info);

		virtual void DoRecall();					//!< Initiates a recall sequence
		virtual void *GetOverrideRetPtr();			//!< Returns the pointer set by SetOverrideRetPtr

		virtual void *SetupHookLoop(META_RES *statusPtr, META_RES *prevResPtr, META_RES *curResPtr,
			void **ifacePtrPtr, const void *origRetPtr, void *overrideRetPtr);
	};
}

#endif

