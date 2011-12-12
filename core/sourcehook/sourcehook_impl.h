/* ======== SourceHook ========
* vim: set ts=4 sw=4 tw=99 noet:
* Copyright (C) 2004-2010 Metamod:Source Development Team
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
#include "sh_memory.h"
#include "sh_list.h"
#include "sh_vector.h"
#include "sh_tinyhash.h"
#include "sh_stack.h"

/*

IMPLEMENTATION INFO

---------------------------------------
 :TODO: update ???

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

#include "sourcehook_impl_cproto.h"
#include "sourcehook_impl_chookmaninfo.h"
#include "sourcehook_impl_chook.h"
#include "sourcehook_impl_ciface.h"
#include "sourcehook_impl_cvfnptr.h"
#include "sourcehook_impl_chookidman.h"

namespace SourceHook
{
	/**
	*	@brief The SourceHook implementation
	*/

	namespace Impl
	{
		struct CHookContext : IHookContext
		{
			CHookContext() : m_CleanupTask(NULL)
			{
			}

			enum State
			{
				State_Born,
				State_Pre,
				State_PreVP,
				State_Post,
				State_PostVP,
				State_OrigCall,
				State_Dead,

				// Special
				State_Ignore,
				State_Recall_Pre,
				State_Recall_PreVP,
				State_Recall_Post,
				State_Recall_PostVP
			};

			int m_State;
			List<CHook>::iterator m_Iter;

			CVfnPtr *pVfnPtr;
			CIface *pIface;
			
			META_RES *pStatus;
			META_RES *pPrevRes;
			META_RES *pCurRes;

			void *pThisPtr;
			const void *pOrigRet;
			void *pOverrideRet;
			void *pIfacePtr;

			bool m_CallOrig;

			ICleanupTask *m_CleanupTask;

			void SkipPaused(List<CHook>::iterator &iter, List<CHook> &list)
			{
				while (iter != list.end() && iter->IsPaused())
					++iter;
			}
		public:
			void HookRemoved(List<CHook>::iterator oldhookiter, List<CHook>::iterator nexthookiter);
			void IfaceRemoved(CIface *iface);
			void VfnPtrRemoved(CVfnPtr *vfnptr);

			ISHDelegate *GetNext();
			void *GetOverrideRetPtr();
			const void *GetOrigRetPtr();
			bool ShouldCallOrig();
			void DoCleanupTaskAndDeleteIt();
		};

		class CVfnPtrList : public List<CVfnPtr>
		{
		public:
			CVfnPtr *GetVfnPtr(void *p);
		};

		typedef CStack<CHookContext> HookContextStack;

		class UnloadListener
		{
		public:
			virtual void ReadyToUnload(Plugin plug) = 0;
		};

		class PendingUnload
		{
			UnloadListener *listener_;
			Plugin plug_;
			bool deactivated_;

		public:
			PendingUnload(UnloadListener *listener, Plugin plug)
			  : listener_(listener), plug_(plug), deactivated_(false)
			{ }

			Plugin plugin() const
			{
				return plug_;
			}

			UnloadListener *listener() const
			{
				return listener_;
			}

			void deactivate()
			{
				deactivated_ = true;
			}
			bool deactivated() const
			{
				return deactivated_;
			}
		};

		class CSourceHookImpl : public ISourceHook
		{
		private:
			CHookManList m_HookManList;
			CVfnPtrList m_VfnPtrs;
			CHookIDManager m_HookIDMan;
			HookContextStack m_ContextStack;
			List<PendingUnload *> m_PendingUnloads;

			bool SetHookPaused(int hookid, bool paused);
			CHookManList::iterator RemoveHookManager(CHookManList::iterator iter);
			List<CVfnPtr>::iterator RevertAndRemoveVfnPtr(List<CVfnPtr>::iterator vfnptr_iter);
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

			int AddHook(Plugin plug, AddHookMode mode, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan,
				ISHDelegate *handler, bool post);

			bool RemoveHook(Plugin plug, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan,
				ISHDelegate *handler, bool post);

			bool RemoveHookByID(int hookid);

			bool PauseHookByID(int hookid);
			bool UnpauseHookByID(int hookid);

			void SetRes(META_RES res);				//!< Sets the meta result
			META_RES GetPrevRes();					//!< Gets the meta result of the
													//!<  previously calledhandler
			META_RES GetStatus();					//!< Gets the highest meta result
			const void *GetOrigRet();				//!< Gets the original result.
													//!<  If not in post function, undefined
			const void *GetOverrideRet();			//!< Gets the override result.
													//!<  If none is specified, NULL
			void *GetIfacePtr();					//!< Gets the interface pointer

			void *GetOverrideRetPtr();				//!< Used for setting the override return value

			/* 
			 * @brief Make sure that a plugin is not used by any
			 * other plugins anymore, and unregister all its hook
			 * managers. If any hooks owned by this plugin are
			 * still on the callstack, defers notifying the listener
			 * until the count has dropped to 0.
			 */
			void UnloadPlugin(Plugin plug, UnloadListener *listener);

			void ResolvePendingUnloads(bool force = false);

			void RemoveHookManager(Plugin plug, HookManagerPubFunc pubFunc);

			void SetIgnoreHooks(void *vfnptr);
			void ResetIgnoreHooks(void *vfnptr);

			void DoRecall();

			IHookContext *SetupHookLoop(IHookManagerInfo *hi, void *vfnptr, void *thisptr, void **origCallAddr, META_RES *statusPtr,
				META_RES *prevResPtr, META_RES *curResPtr, const void *origRetPtr, void *overrideRetPtr);

			void EndContext(IHookContext *pCtx);

			void *GetOrigVfnPtrEntry(void *vfnptr);

			/**
			*	@brief Shut down the whole system, unregister all hook managers
			*/
			void CompleteShutdown();

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
		};
	}
}

#endif
