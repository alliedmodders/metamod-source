/* ======== SourceHook ========
* Copyright (C) 2004-2007 Metamod:Source Development Team
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
#include "sh_listcat.h"

/*

IMPLEMENTATION INFO

---------------------------------------
Protos ("Prototypes")
	The purpose of protos is to provide the amount of type information about a function
	which is required to be able to execute a function call without corrupting the stack.
	Our protos do not fully do this, but they provide the size of the return value, the number of
	parameters, and the size of each parameter, which is enough for most situations.

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

	!! deprecated !!   - see below (new SH_CALL)

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

Post Recalls
	People wanted to be able to use META_RETURN_(VALUE_)NEWPARAMS from post hooks as well. Crazy people!
	Anyway, for this, we have to know where a hook handler is. Is it executing pre or post hooks at the moment?
	The only way we can know this is watching when it calls CHookList::GetIter(). So CHookList gets a new variable:
	m_RequestedFlag. It also gets two new functions: RQFlagReset() and RQFlagGet().
	HookLoopBegin() calls RQFlagReset on both hooklists of the iface; then DoRecall() checks whether the postlist's 
	RQ flag is set. if yes, the hook loop is in post mode.

	So, what a about a recall in post mode? The first ShouldContinue returns false and sets Status to supercede. 
	This way the pre hooks and the function call will be skipped. Then, then next ShouldContinue returns true, so we get
	into the post hooks. HA!

Return Values in Post Recalls
	The easy case is when we already have an override return value. In this case, the status register gets transferred,
	and so does the override return pointer. So, basically, everything is ok.

	However, what happens if we don't? ie. the status register is on MRES_IGNORED? In this case we'd have to transfer the
	orig ret value. But we can't: There's no way to tell the hookfunc: "Use this as orig ret pointer". It uses its own.
	So, we emulate it. GetOrigRet will return the orig ret pointer from the old hook loop. If still no one overrides it,
	we'd have to return it. BUT! HOW TO DO THIS? Check out SH_RETURN(). First calls HookLoopEnd(), then decides whether
	to return the override retval or the orig retval. But it doesn't ask for a new override return. So we give the function
	the last orig return value as its new override return value; but leave status where it is so everything works, and in
	HookLoopEnd we make sure that status is high enough so that the override return will be returned. crazy.

	All this stuff could be much less complicated if I didn't try to preserve binary compatibility :)

VP Hooks
	VP hooks are hooks which are called on a vfnptr, regardless of the this pointer with which it was called. They are
	implemented as a special CIface instance with m_Ptr = NULL. All Hook Lists have a new "ListCatIterator" which
	virtually concatenates the NULL-interface-hook-list with their normal hook list.


	I'm afraid that with the addition of Recalls and VP Hooks, SourceHook is now a pretty complex and hacked-together
	binary compatible beast which is pretty hard to maintain unless you've written it :)

New SH_CALL
	The addition of VP hooks messed up the Call Classes concept (see above) - call classes are bound to an
	instance pointer; they only work on one of the hooked instances. But VP hooks are called on all instances.
	
	That's why now, SH_CALL takes an instance pointer instead of a callclass pointer. It basically does this:
	1) call SH_GLOB_PTR->SetIgnoreHooks(vfnptr)
	2) call this->*mfp
	3) call SH_GLOB_PTR->ResetIgnoreHooks(vfnptr)

	SourceHook stroes the "ignored vfnptr" and makes CVfnPtr::FindIface return NULL if the CVfnPtr instance
	corresponds to the ignored vfnptr. This way the hook manager thinks that the instance isn't hooked, and calls
	the original function. Everything works fine. This works even for VP hooks.
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
			char *m_Proto;

			static bool Equal(const char *p1, const char *p2);
			char *DupProto(const char *src);
			void FreeProto(char *prot);
		public:
			CProto() : m_Proto(NULL)
			{
			}

			CProto(const char *szProto) : m_Proto(DupProto(szProto))
			{
			}

			CProto(const CProto &other) : m_Proto(DupProto(other.m_Proto))
			{
			}

			~CProto()
			{
				FreeProto(m_Proto);
				m_Proto = NULL;
			}

			void operator = (const char *szProto)
			{
				m_Proto = DupProto(szProto);
			}

			void operator = (const CProto &other)
			{
				m_Proto = DupProto(other.m_Proto);
			}

			bool operator == (const char *szProto) const
			{
				return Equal(szProto, m_Proto);
			}
			bool operator == (const CProto &other) const
			{
				return Equal(other.m_Proto, m_Proto);
			}

			const char *GetProto() const
			{
				return m_Proto;
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

		// Associates hook ids with info about the hooks
		// Also used to keep track of used hook ids
		class CHookIDManager
		{
		public:
			struct Entry
			{
				bool isfree;

				// hookman info
				CProto proto;
				int vtbl_offs;
				int vtbl_idx;

				// vfnptr
				void *vfnptr;

				// iface
				void* adjustediface;

				// hook
				Plugin plug;
				int thisptr_offs;
				ISHDelegate *handler;
				bool post;

				Entry(const CProto &pprt, int pvo, int pvi, void *pvp, void *pai, Plugin pplug, int pto,
					ISHDelegate *ph, bool ppost)
					: isfree(false), proto(pprt), vtbl_offs(pvo), vtbl_idx(pvi), vfnptr(pvp), 
					adjustediface(pai), plug(pplug), thisptr_offs(pto), handler(ph), post(ppost)
				{
				}
				Entry()
				{
				}
			};
		private:
			// Internally, hookid 1 is stored as m_Entries[0]

			CVector<Entry> m_Entries;
		public:
			CHookIDManager();
			int New(const CProto &proto, int vtbl_offs, int vtbl_idx, void *vfnptr, void *adjustediface,
				Plugin plug, int thisptr_offs, ISHDelegate *handler, bool post);
			bool Remove(int hookid);
			const Entry * QueryHook(int hookid);

			// Finds all hooks with the given info, and fills the hookids into output.
			void FindAllHooks(CVector<int> &output, const CProto &proto, int vtbl_offs, int vtbl_idx,
				void *adjustediface, Plugin plug, int thisptr_offs, ISHDelegate *handler, bool post);

			// Removes all hooks with a specified vfnptr
			bool RemoveAll(void *vfnptr);
		};

		struct HookInfo
		{
			ISHDelegate *handler;			//!< Pointer to the handler
			bool paused;					//!< If true, the hook should not be executed
			Plugin plug;					//!< The owner plugin
			int thisptr_offs;				//!< This pointer offset
			int hookid;						//!< Unique ID given by CHookIDManager

			bool operator==(int otherid)
			{
				return hookid == otherid;
			}
		};

		class CHookList : public IHookList
		{
		public:
			List<HookInfo> *m_VPList;			// left-hand list for ListCatIterator -> for VP hooks
			List<HookInfo> m_List;

			friend class CIter;

			class CIter : public IHookList::IIter
			{
				friend class CHookList;

				CHookList *m_pList;

				void SkipPaused();
			public:

				ListCatIterator<HookInfo> m_Iter;

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
			bool m_RQFlag;

			void SetRecallState();	// Sets the list into a state where the next returned
									// iterator (from GetIter) will be a copy of the last
									// returned iterator, incremented by one. This is used in Recalls.
									// The hook resets this state automatically on:
									// GetIter, ReleaseIter

			void RQFlagReset() { m_RQFlag = false; }
			bool RQFlagGet() { return m_RQFlag; }
			CHookList();
			CHookList(const CHookList &other);
			virtual ~CHookList();

			void operator=(const CHookList &other);

			IIter *GetIter();
			void ReleaseIter(IIter *pIter);

			void SetVPList(List<HookInfo> *newList);
			void ClearVPList();
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
			bool operator!=(void *ptr)
			{
				return m_Ptr != ptr;
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

			void **m_pOneIgnore;
		public:
			CVfnPtr(void *ptr, void **pOneIgnore);
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

		class CCallClassImpl : public DeprecatedCallClass<void>
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
			enum RecallType
			{
				Recall_No=0,
				Recall_Pre,
				Recall_Post1,
				Recall_Post2
			};

			META_RES *pStatus;
			META_RES *pPrevRes;
			META_RES *pCurRes;

			META_RES temporaryStatus;				//!< Stored during Post1 recall phase
			bool shouldContinue;
			RecallType recall;						//!< Specifies which kind of recall we're in.

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
		CHookIDManager m_HookIDMan;

		void *m_OneIgnore; //:TODO:
		bool m_IgnoreActive;
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
		DeprecatedCallClass<void> *GetCallClass(void *iface, size_t size);

		/**
		*	@brief Release a callclass
		*
		*	@param ptr Pointer to the callclass
		*/
		virtual void ReleaseCallClass(DeprecatedCallClass<void> *ptr);

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

		/**
		*	@brief Add a (VP) hook.
		*
		*	@return non-zero hook id on success, 0 otherwise
		*
		*	@param plug The unique identifier of the plugin that calls this function
		*	@param mode	Can be either Hook_Normal or Hook_VP (vtable-wide hook)
		*	@param iface The interface pointer
		*				 The representative interface pointer for VP hooks
		*				 The vtable pointer for direct VP hooks !!!
		*	@param ifacesize The size of the class iface points to
		*	@param myHookMan A hook manager function that should be capable of handling the function
		*	@param handler A pointer to a FastDelegate containing the hook handler
		*	@param post Set to true if you want a post handler
		*/
		virtual int AddHookNew(Plugin plug, AddHookMode mode, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan,
			ISHDelegate *handler, bool post);

		/**
		*	@brief Remove a VP hook by ID.
		*
		*	@return true on success, false otherwise
		*
		*	@param plug The unique identifier of the plugin that calls this function
		*	@param hookid The hook id (returned by AddHookNew)
		*/
		virtual bool RemoveHookByID(Plugin plug, int hookid);

		/**
		*	@brief Makes sure that hooks are going to be ignored on the next call of vfnptr
		*
		*	@param plug The unique identifier of the plugin that calls this function
		*	@param vfnptr The virtual function pointer of the function in question
		*/
		virtual void SetIgnoreHooks(Plugin plug, void *vfnptr);

		/**
		*	@brief Reverses SetIgnoreHooks' effect
		*
		*	@param plug The unique identifier of the plugin that calls this function
		*	@param vfnptr The virtual function pointer of the function in question
		*/
		virtual void ResetIgnoreHooks(Plugin plug, void *vfnptr);

		/**
		*	@brief Finds the original entry of a virtual function pointer
		*
		*	@param vfnptr The virtual function pointer
		*	@return The original entry if the virtual function pointer has been patched; NULL otherwise.
		*/
		virtual void *GetOrigVfnPtrEntry(void *vfnptr);
	};
}

#endif
