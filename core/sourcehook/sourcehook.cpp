/* ======== SourceHook ========
* vim: set ts=4 sw=4 tw=99 noet:
* Copyright (C) 2004-2010 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* Contributors: Scott "Damaged Soul" Ehlert
* ============================
*/

/**
 * @file sourcehook.cpp
 * @brief Contains the implementation of the SourceHook API
*/

#if defined __GNUC__
#include <stdint.h>
#endif

#include "sourcehook_impl.h"

namespace SourceHook
{
	template<>
	int HashFunction<int>(const int & k)
	{
		return k;
	}
	template<>
	int Compare<int>(const int & k1, const int & k2)
	{
		if (k1 == k2)
			return 0;
		if (k1 > k2)
			return 1;
		if (k1 < k2)
			return -1;
		return 0;
	}

	namespace Impl
	{
		//////////////////////////////////////////////////////////////////////////
		// CVfnPtrList
		//////////////////////////////////////////////////////////////////////////

		CVfnPtr *CVfnPtrList::GetVfnPtr(void *vfnptr)
		{
			iterator iter = find(vfnptr);
			if (iter == end())
			{
				// No vfnptr info object found
				// --> create a new one
				CVfnPtr newVfnPtr(vfnptr);
				if (newVfnPtr.Init())
				{
					push_back(newVfnPtr);

					return &(back());
				}
				else
				{
					// Initialization failed.
					return NULL;
				}
			}
			else
			{
				return &(*iter);
			}
		}

		//////////////////////////////////////////////////////////////////////////
		// CSourceHookImpl
		//////////////////////////////////////////////////////////////////////////
		

		CSourceHookImpl::CSourceHookImpl()
		{
		}
		CSourceHookImpl::~CSourceHookImpl()
		{
			CompleteShutdown();
		}

		int CSourceHookImpl::GetIfaceVersion()
		{
			return SH_IFACE_VERSION;
		}

		int CSourceHookImpl::GetImplVersion()
		{
			return SH_IMPL_VERSION;
		}

		int CSourceHookImpl::AddHook(Plugin plug, AddHookMode mode, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan,
			ISHDelegate *handler, bool post)
		{
			if (mode != Hook_Normal && mode != Hook_VP && mode != Hook_DVP)
				return 0;

			// Get info about hook manager
			CHookManager hookManager(plug, myHookMan);
			if (!hookManager)
				return 0;

			void *adjustediface = NULL;
			void **cur_vtptr = NULL;
			void *cur_vfnptr = NULL;

			// find vfnptr
			switch (mode)
			{
			case Hook_Normal:
				adjustediface = reinterpret_cast<void*>(reinterpret_cast<char*>(iface) + thisptr_offs);

				cur_vtptr = *reinterpret_cast<void***>(
					reinterpret_cast<char*>(adjustediface) + hookManager.GetVtblOffs());
				cur_vfnptr = reinterpret_cast<void*>(cur_vtptr + hookManager.GetVtblIdx());
				break;

			case Hook_VP:

				adjustediface = reinterpret_cast<void*>(reinterpret_cast<char*>(iface) + thisptr_offs);

				cur_vtptr = *reinterpret_cast<void***>(
					reinterpret_cast<char*>(adjustediface) + hookManager.GetVtblOffs());
				cur_vfnptr = reinterpret_cast<void*>(cur_vtptr + hookManager.GetVtblIdx());

				adjustediface = NULL;
				break;

			case Hook_DVP:
				adjustediface = NULL;

				cur_vtptr = reinterpret_cast<void**>(iface);
				cur_vfnptr = cur_vtptr + hookManager.GetVtblIdx();

				break;
			}

			CVfnPtr *vfnPtr = m_VfnPtrs.GetVfnPtr(cur_vfnptr);
			if (!vfnPtr)
			{
				// Could not create the vfnptr info object.
				// This could be because a thunk generation on GCC
				// has failed. See sourcehook_impl_cvfnptr.cpp
				// for details.
				return false;
			}

			vfnPtr->AddHookMan(m_HookManList.GetHookMan(hookManager));
			CIface &ifaceinst = vfnPtr->GetIface(adjustediface);

			// Add the hook
			CHook hook(plug, thisptr_offs, handler, 

				m_HookIDMan.New(hookManager.GetProto(), hookManager.GetVtblOffs(), hookManager.GetVtblIdx(),
					cur_vfnptr, adjustediface, plug, thisptr_offs, handler, post)

				);

			if (post)
				ifaceinst.GetPostHookList().push_back(hook);
			else
				ifaceinst.GetPreHookList().push_back(hook);

			return hook.GetID();
		}

		bool CSourceHookImpl::RemoveHook(Plugin plug, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan,
			ISHDelegate *handler, bool post)
		{
			// Get info about hook manager and compute adjustediface
			CHookManager tmpHookMan(plug, myHookMan);

			void *adjustediface = reinterpret_cast<void*>(reinterpret_cast<char*>(iface)+thisptr_offs);

			// Loop through all hooks and remove those which match:
			//  hookman, vfnptr, iface, plug, adjusted iface, this ptr offs, handler, post
			CVector<int> removehooks;
			m_HookIDMan.FindAllHooks(removehooks, tmpHookMan.GetProto(), tmpHookMan.GetVtblOffs(), tmpHookMan.GetVtblIdx(),
				adjustediface, plug, thisptr_offs, handler, post);

			if (removehooks.empty())
				return false;

			bool status = false;

			for (CVector<int>::iterator iter = removehooks.begin(); iter != removehooks.end(); ++iter)
			{
				if (RemoveHookByID(*iter))
					status = true;
			}
			return status; 
		}

		bool CSourceHookImpl::RemoveHookByID(int hookid)
		{
			const CHookIDManager::Entry *hentry;

			hentry = m_HookIDMan.QueryHook(hookid);
			if (!hentry)
			{
				// hookid doesn't exist !
				return false;
			}

			// find vfnptr
			List<CVfnPtr>::iterator vfnptr_iter = m_VfnPtrs.find(hentry->vfnptr);
			if (vfnptr_iter == m_VfnPtrs.end())
				return false;

			// find iface
			List<CIface>::iterator iface_iter = vfnptr_iter->GetIfaceList().find(hentry->adjustediface);
			if (iface_iter == vfnptr_iter->GetIfaceList().end())
				return false;

			// find hook
			List<CHook> &hooks = hentry->post ? iface_iter->GetPostHookList() : iface_iter->GetPreHookList();
			List<CHook>::iterator hook_iter = hooks.find(hookid);
			if (hook_iter == hooks.end())
				return false;

			hook_iter->GetHandler()->DeleteThis();

			// Iterator movage!
			List<CHook>::iterator oldhookiter = hook_iter;
			hook_iter = hooks.erase(hook_iter);

			for (CStack<CHookContext>::iterator ctx_iter = m_ContextStack.begin();
				ctx_iter != m_ContextStack.end(); ++ctx_iter)
			{
				ctx_iter->HookRemoved(oldhookiter, hook_iter);
			}

			if (iface_iter->GetPreHookList().empty() && iface_iter->GetPostHookList().empty())
			{
				// -> Kill all contexts that use it!
				for (CStack<CHookContext>::iterator ctx_iter = m_ContextStack.begin();
					ctx_iter != m_ContextStack.end(); ++ctx_iter)
				{
					ctx_iter->IfaceRemoved(&(*iface_iter));
				}

				// There are no hooks on this iface anymore...
				iface_iter = vfnptr_iter->GetIfaceList().erase(iface_iter);

				if (vfnptr_iter->GetIfaceList().empty())
				{
					// No ifaces at all -> Deactivate the hook

					for (CStack<CHookContext>::iterator ctx_iter = m_ContextStack.begin();
						ctx_iter != m_ContextStack.end(); ++ctx_iter)
					{
						ctx_iter->VfnPtrRemoved(&(*vfnptr_iter));
					}

					RevertAndRemoveVfnPtr(vfnptr_iter);
				}
			}

			m_HookIDMan.Remove(hookid);
			return true;
		}

		List<CVfnPtr>::iterator CSourceHookImpl::RevertAndRemoveVfnPtr(List<CVfnPtr>::iterator vfnptr_iter)
		{
			ICleanupTask *cleanupTask = vfnptr_iter->GetCleanupTask();

			// Some vfnptrs require cleanup.
			// Concrete case: on GCC, when the original vtable entry is not even
			// we generate an even-aligned thunk to call the original function.
			// If the vfnptr is being removed from a pre hook on the vfnptr
			// we have to delay the cleanup of this thunk until the hook loop is done
			// (because the orig function call mechanism is going to use the thunk).

			if (cleanupTask != NULL)
			{
				// If this vfnptr is in use in one of the hook loops running at the moment
				// Schedule it for removal on the DEEPEST hook loop.

				size_t numOfContexts = m_ContextStack.size();
				// m_ContextStack.at(0) is the deepest hook context
				// m_ContextStack.at(size-1) = m_ContextStack.front is the uppermost

				bool cleanupImmedieately = true;

				CVfnPtr *vfnPtrObjAddr = &(*vfnptr_iter);
				for (size_t i = 0; i < numOfContexts; ++i)
				{
					CHookContext &context = m_ContextStack.at(i);
					if (context.pVfnPtr == vfnPtrObjAddr)
					{
						// Found a hook context using this vfnptr at the moment.
						context.m_CleanupTask = cleanupTask;
						cleanupImmedieately = false;			// Delay the cleanup
						break;
					}
				}

				if (cleanupImmedieately)
				{
					cleanupTask->CleanupAndDeleteThis();
				}
			}

			// Do the work
			vfnptr_iter->Revert();
			return m_VfnPtrs.erase(vfnptr_iter);
		}

		void CSourceHookImpl::SetRes(META_RES res)
		{
			*m_ContextStack.front().pCurRes = res;
		}

		META_RES CSourceHookImpl::GetPrevRes()
		{
			return *m_ContextStack.front().pPrevRes;
		}

		META_RES CSourceHookImpl::GetStatus()
		{
			return *m_ContextStack.front().pStatus;

		}

		const void *CSourceHookImpl::GetOrigRet()
		{
			return m_ContextStack.front().pOrigRet;
		}

		const void *CSourceHookImpl::GetOverrideRet()
		{
			return (*m_ContextStack.front().pStatus < MRES_OVERRIDE) ?
				NULL : m_ContextStack.front().pOverrideRet;
		}

		void *CSourceHookImpl::GetIfacePtr()
		{
			// If in recall: return last one
			if (m_ContextStack.front().m_State >= CHookContext::State_Recall_Pre &&
				m_ContextStack.front().m_State <= CHookContext::State_Recall_PostVP)
			{
				return m_ContextStack.second().pIfacePtr;
			}
			else
			{
				return m_ContextStack.front().pIfacePtr;
			}
		}

		void *CSourceHookImpl::GetOverrideRetPtr()
		{
			return m_ContextStack.front().pOverrideRet;
		}

		void CSourceHookImpl::UnloadPlugin(Plugin plug, UnloadListener *listener)
		{
			// 1) Remove all hooks by this plugin

			CVector<int> removehooks;
			m_HookIDMan.FindAllHooks(removehooks, plug);

			for (CVector<int>::iterator iter = removehooks.begin(); iter != removehooks.end(); ++iter)
				RemoveHookByID(*iter);

			// 2) Remove all hook managers
			for (CHookManList::iterator iter = m_HookManList.begin(); iter != m_HookManList.end(); )
			{
				if (iter->GetOwnerPlugin() == plug)
					iter = RemoveHookManager(iter);
				else
					++iter;
			}

			// Fix for bug 5034. It's pretty tricky to find whether hookmanagers owned by this
			// plugin are active. We could change how hook managers are tracked in SH, and lazily
			// free them as the context stack drops to 0, or we could change the pubfunc API to
			// know whether it's active or not. Rather than deal with this extra complexity, we
			// just conservatively wait until the context stack hits 0 before unloading.
			if (m_ContextStack.size() == 0)
			{
				listener->ReadyToUnload(plug);
			}
			else
			{
				m_PendingUnloads.push_back(new PendingUnload(listener, plug));
			}
		}

		CHookManList::iterator CSourceHookImpl::RemoveHookManager(CHookManList::iterator hookman_iter)
		{
			// 2) Remove it
			for (CVfnPtrList::iterator vfnptr_iter = m_VfnPtrs.begin();
				vfnptr_iter != m_VfnPtrs.end();)
			{
				if (!vfnptr_iter->HookManRemoved(&(*hookman_iter)))
				{
					// This vfnptr has no more hook managers
					// and asks to be removed.

					m_HookIDMan.RemoveAll(vfnptr_iter->GetPtr());

					vfnptr_iter = RevertAndRemoveVfnPtr(vfnptr_iter);
				}
				else
				{
					++vfnptr_iter;
				}
			}

			return m_HookManList.erase(hookman_iter);
		}

		void CSourceHookImpl::RemoveHookManager(Plugin plug, HookManagerPubFunc pubFunc)
		{
			// Find the hook manager
			CHookManList::iterator hookman_iter = m_HookManList.find(CHookManager::Descriptor(plug, pubFunc));

			if (hookman_iter != m_HookManList.end())
			{
				RemoveHookManager(hookman_iter);
			}
		}

		void CSourceHookImpl::SetIgnoreHooks(void *vfnptr)
		{
			CHookContext ctx;
			ctx.m_State = CHookContext::State_Ignore;
			m_ContextStack.push(ctx);
		}

		void CSourceHookImpl::ResetIgnoreHooks(void *vfnptr)
		{
			if (!m_ContextStack.empty() && m_ContextStack.front().m_State == CHookContext::State_Ignore)
			{
				// Actually use EndContext
				// instead of m_ContextStack.pop directly
				// because it runs the cleanup task if neccesary
				EndContext(&(m_ContextStack.front()));
			}
		}

		void *CSourceHookImpl::GetOrigVfnPtrEntry(void *vfnptr)
		{
			for (CVfnPtrList::iterator vfnptr_iter = m_VfnPtrs.begin(); vfnptr_iter != m_VfnPtrs.end(); ++vfnptr_iter)
			{
				if (vfnptr_iter->GetPtr() == vfnptr)
					return vfnptr_iter->GetOrigEntry();
			}
			return NULL;
		}

		void CSourceHookImpl::DoRecall()
		{
			CHookContext newCtx;
			CHookContext &curCtx = m_ContextStack.front();


			newCtx.m_State = curCtx.m_State + (CHookContext::State_Recall_Pre - CHookContext::State_Pre);
			if (newCtx.m_State == CHookContext::State_Recall_Post || newCtx.m_State == CHookContext::State_Recall_PostVP)
			{
				// Also save orig ret
				newCtx.pOrigRet = curCtx.pOrigRet;
			}

			// The hookfunc usually does this, but it won't have a chance to see it, 
			// so for recalls, we update status here if it's required
			if (*curCtx.pCurRes > *curCtx.pStatus) 
				*curCtx.pStatus = *curCtx.pCurRes;

			newCtx.pStatus = curCtx.pStatus;
			newCtx.pOverrideRet = curCtx.pOverrideRet;
			newCtx.pPrevRes = curCtx.pPrevRes;
			newCtx.m_Iter = curCtx.m_Iter;

			// Take this with us!
			newCtx.pCurRes = curCtx.pCurRes;

			m_ContextStack.push(newCtx);
			curCtx.m_State = CHookContext::State_Dead;
		}

		IHookContext *CSourceHookImpl::SetupHookLoop(IHookManagerInfo *hi, void *vfnptr, void *thisptr, void **origCallAddr, META_RES *statusPtr,
			META_RES *prevResPtr, META_RES *curResPtr, const void *origRetPtr, void *overrideRetPtr)
		{
			CHookContext *pCtx = NULL;
			CHookContext *oldctx = m_ContextStack.empty() ? NULL : &m_ContextStack.front();
			if (oldctx)
			{
				// SH_CALL
				if (oldctx->m_State == CHookContext::State_Ignore)
				{
					*statusPtr = MRES_IGNORED;
					oldctx->m_CallOrig = true;
					oldctx->m_State = CHookContext::State_Dead;

					List<CVfnPtr*> &vfnptr_list = static_cast<CHookManager*>(hi)->GetVfnPtrList();
					List<CVfnPtr*>::iterator vfnptr_iter;
					for (vfnptr_iter = vfnptr_list.begin();
						vfnptr_iter != vfnptr_list.end(); ++vfnptr_iter)
					{
						if (**vfnptr_iter == vfnptr)
							break;
					}
					if (vfnptr_iter == vfnptr_list.end())
					{
						SH_ASSERT(false, ("How can a hook exist on a vfnptr which we don't have in our db?!"));
					}
					else
					{
						*origCallAddr = (*vfnptr_iter)->GetOrigCallAddr();
						oldctx->pVfnPtr = *vfnptr_iter;
					}

					oldctx->pOrigRet = origRetPtr;
					return oldctx;
				}
				// Recall
				else if (oldctx->m_State >= CHookContext::State_Recall_Pre &&
					oldctx->m_State <= CHookContext::State_Recall_PostVP)
				{
					pCtx = oldctx;

					*statusPtr = *(oldctx->pStatus);
					*prevResPtr = *(oldctx->pPrevRes);

					pCtx->m_Iter = oldctx->m_Iter;

					// Only have possibility of calling the orig func in pre recall mode
					pCtx->m_CallOrig = (oldctx->m_State == CHookContext::State_Recall_Pre || 
						oldctx->m_State == CHookContext::State_Recall_PreVP);

					overrideRetPtr = pCtx->pOverrideRet;

					// When the status is low so there's no override return value and we're in a post recall,
					// give it the orig return value as override return value.
					if (pCtx->m_State == CHookContext::State_Recall_Post ||
						pCtx->m_State == CHookContext::State_Recall_PostVP)
					{
						origRetPtr = oldctx->pOrigRet;
						if (*statusPtr < MRES_OVERRIDE)
							overrideRetPtr = const_cast<void*>(pCtx->pOrigRet);
					}
				}
			}
			if (!pCtx)
			{
				pCtx = m_ContextStack.make_next();
				pCtx->m_State = CHookContext::State_Born;
				pCtx->m_CallOrig = true;
			}

			pCtx->pIface = NULL;

			List<CVfnPtr*> &vfnptr_list = static_cast<CHookManager*>(hi)->GetVfnPtrList();
			List<CVfnPtr*>::iterator vfnptr_iter; 
			for (vfnptr_iter = vfnptr_list.begin();
				vfnptr_iter != vfnptr_list.end(); ++vfnptr_iter)
			{
				if (**vfnptr_iter == vfnptr)
					break;
			}
			if (vfnptr_iter == vfnptr_list.end())
			{
				pCtx->m_State = CHookContext::State_Dead;
			}
			else
			{
				pCtx->pVfnPtr = *vfnptr_iter;
				*origCallAddr = pCtx->pVfnPtr->GetOrigCallAddr();
				pCtx->pIface = pCtx->pVfnPtr->FindIface(thisptr);
			}

			pCtx->pStatus = statusPtr;
			pCtx->pPrevRes = prevResPtr;
			pCtx->pCurRes = curResPtr;
			pCtx->pThisPtr = thisptr;
			pCtx->pOverrideRet = overrideRetPtr;
			pCtx->pOrigRet = origRetPtr;

			return pCtx;
		}

		void CSourceHookImpl::ResolvePendingUnloads(bool force)
		{
			List<PendingUnload *>::iterator iter = m_PendingUnloads.begin();
			while (iter != m_PendingUnloads.end())
			{
				PendingUnload *unload = *iter;

				if (!force && !unload->deactivated())
				{
					// Unless being forced, wait one drop of the context stack
					// before actually unloading. Otherwise, we'll still return
					// to unloaded memory.
					unload->deactivate();
					iter++;
				}
				else
				{
					unload->listener()->ReadyToUnload(unload->plugin());
					delete unload;
					iter = m_PendingUnloads.erase(iter);
				}
			}
		}

		void CSourceHookImpl::EndContext(IHookContext *pCtx)
		{
			// Do clean up task, if any is associated with this context
			m_ContextStack.front().DoCleanupTaskAndDeleteIt();
			// Then remove it
			m_ContextStack.pop();

			// If we've reached 0 contexts and there are pending unloads,
			// resolve them now.
			if (m_ContextStack.size() == 0 && m_PendingUnloads.size() != 0)
				ResolvePendingUnloads();
		}

		void CSourceHookImpl::CompleteShutdown()
		{
			CVector<int> removehooks;
			m_HookIDMan.FindAllHooks(removehooks);

			for (CVector<int>::iterator iter = removehooks.begin(); iter != removehooks.end(); ++iter)
				RemoveHookByID(*iter);
		}

		void CSourceHookImpl::PausePlugin(Plugin plug)
		{
			CVector<int> pausehooks;
			m_HookIDMan.FindAllHooks(pausehooks, plug);

			for (CVector<int>::iterator iter = pausehooks.begin(); iter != pausehooks.end(); ++iter)
				PauseHookByID(*iter);
		}

		void CSourceHookImpl::UnpausePlugin(Plugin plug)
		{
			CVector<int> unpausehooks;
			m_HookIDMan.FindAllHooks(unpausehooks, plug);

			for (CVector<int>::iterator iter = unpausehooks.begin(); iter != unpausehooks.end(); ++iter)
				UnpauseHookByID(*iter);
		}

		bool CSourceHookImpl::PauseHookByID(int hookid)
		{
			return SetHookPaused(hookid, true);
		}

		bool CSourceHookImpl::UnpauseHookByID(int hookid)
		{
			return SetHookPaused(hookid, false);
		}

		bool CSourceHookImpl::SetHookPaused(int hookid, bool paused)
		{
			const CHookIDManager::Entry *hentry;

			hentry = m_HookIDMan.QueryHook(hookid);
			if (!hentry)
			{
				// hookid doesn't exist !
				return false;
			}

			// find vfnptr
			List<CVfnPtr>::iterator vfnptr_iter = m_VfnPtrs.find(hentry->vfnptr);
			if (vfnptr_iter == m_VfnPtrs.end())
				return false;

			// find iface
			List<CIface>::iterator iface_iter = vfnptr_iter->GetIfaceList().find(hentry->adjustediface);
			if (iface_iter == vfnptr_iter->GetIfaceList().end())
				return false;

			// find hook
			List<CHook> &hooks = hentry->post ? iface_iter->GetPostHookList() : iface_iter->GetPreHookList();
			List<CHook>::iterator hook_iter = hooks.find(hookid);
			if (hook_iter == hooks.end())
				return false;

			hook_iter->SetPaused(paused);
			return true;
		}

		//////////////////////////////////////////////////////////////////////////
		// CHookContext
		//////////////////////////////////////////////////////////////////////////
		ISHDelegate *CHookContext::GetNext()
		{
			CIface *pVPIface;
			switch (m_State)
			{
			case State_Dead:
				return NULL;

			case State_Born:
				m_Iter = List<CHook>::iterator();
				m_State = State_Pre;

				// fall-through
			case State_Recall_Pre:
				m_State = State_Pre;
			case State_Pre:
				if (pIface)
				{
					if (!m_Iter)
						m_Iter = pIface->GetPreHookList().begin();
					else
						++m_Iter;
					SkipPaused(m_Iter, pIface->GetPreHookList());

					if (m_Iter != pIface->GetPreHookList().end())
					{
						pIfacePtr = reinterpret_cast<void*>(reinterpret_cast<char*>(pThisPtr) - m_Iter->GetThisPointerOffset());
						return m_Iter->GetHandler();
					}
				}

				// end of normal hooks -> VP
				
				m_State = State_PreVP;
				m_Iter = List<CHook>::iterator();

				// fall-through
			case State_Recall_PreVP:
				m_State = State_PreVP;
			case State_PreVP:
				pVPIface = pVfnPtr->FindIface(NULL);
				if (pVPIface)
				{
					if (!m_Iter)
						m_Iter = pVPIface->GetPreHookList().begin();
					else
						++m_Iter;
					SkipPaused(m_Iter, pVPIface->GetPreHookList());

					if (m_Iter != pVPIface->GetPreHookList())
					{
						pIfacePtr = reinterpret_cast<void*>(reinterpret_cast<char*>(pThisPtr) - m_Iter->GetThisPointerOffset());
						return m_Iter->GetHandler();
					}
				}

				// end VP hooks -> orig call

				m_State = State_OrigCall;
				return NULL;
				
			case State_OrigCall:
				m_Iter = List<CHook>::iterator();
				m_State = State_Post;

				// fall-through
			case State_Post:
				if (pIface)
				{
					if (!m_Iter)
						m_Iter = pIface->GetPostHookList().begin();
					else
						++m_Iter;
					SkipPaused(m_Iter, pIface->GetPostHookList());

					if (m_Iter != pIface->GetPostHookList().end())
					{
						pIfacePtr = reinterpret_cast<void*>(reinterpret_cast<char*>(pThisPtr) - m_Iter->GetThisPointerOffset());
						return m_Iter->GetHandler();
					}
				}
				// end of normal hooks -> VP

				m_State = State_PostVP;
				m_Iter = List<CHook>::iterator();

				// fall-through
			case State_PostVP:
				pVPIface = pVfnPtr->FindIface(NULL);
				if (pVPIface)
				{
					if (!m_Iter)
						m_Iter = pVPIface->GetPostHookList().begin();
					else
						++m_Iter;
					SkipPaused(m_Iter, pVPIface->GetPostHookList());

					if (m_Iter != pVPIface->GetPostHookList())
					{
						pIfacePtr = reinterpret_cast<void*>(reinterpret_cast<char*>(pThisPtr) - m_Iter->GetThisPointerOffset());
						return m_Iter->GetHandler();
					}
				}

				// end VP hooks -> done

				m_State = State_Dead;
				return NULL;

			case State_Recall_Post:
				// Skip pre hooks _and_ orig call
				m_State = State_Post;
				return NULL;

			case State_Recall_PostVP:
				// Skip pre hooks _and_ orig call
				m_State = State_PostVP;
				return NULL;
			}
			return NULL;
		}

		void *CHookContext::GetOverrideRetPtr()
		{
			return pOverrideRet;
		}

		const void *CHookContext::GetOrigRetPtr()
		{
			return pOrigRet;
		}

		bool CHookContext::ShouldCallOrig()
		{
			return m_CallOrig;
		}

		void CHookContext::HookRemoved(List<CHook>::iterator oldhookiter, List<CHook>::iterator nexthookiter)
		{
			if (m_Iter == oldhookiter)
			{
				m_Iter = nexthookiter;
				--m_Iter;
			}
		}

		void CHookContext::IfaceRemoved(CIface *iface)
		{
			if (pIface == iface)
				pIface = NULL;
		}

		void CHookContext::VfnPtrRemoved(CVfnPtr *vfnptr)
		{
			if (pVfnPtr == vfnptr)
			{
				// Don't set pVfnPtr = NULL here!
				// It may be used still.
				// RevertAndRemoveVfnPtr uses it to find the hook context
				// to which to attach the cleanup task of the vfnptr.

				//pVfnPtr = NULL;
				m_State = State_Dead;
			}
		}

		void CHookContext::DoCleanupTaskAndDeleteIt()
		{
			if (m_CleanupTask != NULL)
			{
				m_CleanupTask->CleanupAndDeleteThis();
			}
		}
	}
}
