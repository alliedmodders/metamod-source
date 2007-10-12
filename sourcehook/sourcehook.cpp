/* ======== SourceHook ========
* Copyright (C) 2004-2007 Metamod:Source Development Team
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
#include "sh_memory.h"

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
		// CProto
		//////////////////////////////////////////////////////////////////////////
		ProtoInfo *CProto::DupProto(const ProtoInfo *src)
		{
			if (src == NULL)
				return NULL;

			PassInfo *newParamsPassInfo = new PassInfo[src->numOfParams + 1];
			for (int i = 0; i <= src->numOfParams; ++i)
				newParamsPassInfo[i] = src->paramsPassInfo[i];

			ProtoInfo *newProto = new ProtoInfo;
			newProto->retPassInfo = src->retPassInfo;
			newProto->paramsPassInfo = newParamsPassInfo;
			newProto->numOfParams = src->numOfParams;
			newProto->convention = src->convention;

			return newProto;
		}

		void CProto::FreeProto(ProtoInfo *prot)
		{
			if (!prot)
				return;

			delete [] prot->paramsPassInfo;
			delete prot;
		}

		bool CProto::Equal(const ProtoInfo *p1, const ProtoInfo *p2)
		{
			if (!p1 || !p2)
				return false;

			if (p1->numOfParams != p2->numOfParams)
				return false;

			if (p1->convention != ProtoInfo::CallConv_Unknown && p2->convention != ProtoInfo::CallConv_Unknown &&
				p1->convention != p2->convention)
				return false;

			if (p1->retPassInfo.size != p2->retPassInfo.size)
				return false;

			// Skip params[0] : dummy
			for (int i = 1; i <= p1->numOfParams; ++i)
			{
				if (GetRealSize(p1->paramsPassInfo[i]) != GetRealSize(p2->paramsPassInfo[i]))
					return false;
				if (p1->paramsPassInfo[i].type != PassInfo::PassType_Unknown && p2->paramsPassInfo[i].type != PassInfo::PassType_Unknown)
				{
					if (p1->paramsPassInfo[i].type != p2->paramsPassInfo[i].type)
						return false;
					if (GetRealFlags(p1->paramsPassInfo[i]) != GetRealFlags(p2->paramsPassInfo[i]))
						return false;
				}
			}

			return true;
		}


		//////////////////////////////////////////////////////////////////////////
		// CHookManContainerList
		//////////////////////////////////////////////////////////////////////////
		CHookManagerContainer &CHookManContainerList::GetContainer(int vtbloffs, int vtblidx, const CProto &proto)
		{
			iterator iter = find(CHookManagerContainer::Descriptor(vtbloffs, vtblidx, proto));
			if (iter == end())
			{
				CHookManagerContainer newCont(vtbloffs, vtblidx, proto);
				push_back(newCont);
				return back();
			}
			else
			{
				return *iter;
			}
		}

		void CHookManContainerList::RemoveHookMans(Plugin plug)
		{
			for (iterator iter = begin(); iter != end(); ++iter)
				iter->RemoveHookMans(plug);
		}

		void CHookManContainerList::RemoveHookMans(Plugin plug, HookManagerPubFunc pubFunc)
		{
			for (iterator iter = begin(); iter != end(); ++iter)
				iter->RemoveHookMans(plug, pubFunc);
		}

		//////////////////////////////////////////////////////////////////////////
		// CHookManagerContainer
		//////////////////////////////////////////////////////////////////////////
		
		bool CHookManagerContainer::AddHookManager(const CHookManager &hookMan)
		{
			iterator iter;

			// Don't accept invalid hook managers
			if (!hookMan)
				return false;

			// Check whether such a hook manager already exists; if yes, ignore.
			iter = find(hookMan);
			if (iter != end())
			{
				// but make sure that it's activated (ms_HI stored)
				iter->Register();
				return true;
			}

			// It doesn't -> add it. Add it to the end of its version group.
			for (iter = begin(); iter != end(); ++iter)
			{
				if (iter->GetVersion() < hookMan.GetVersion())
					break;
			}

			bool isBeginning = iter == begin();

			insert(iter, hookMan);

			if (isBeginning)
			{
				iter = begin();

				if (size() > 1)
				{
					// If the next hookman isn't empty (there were hooks before, but this hookman is better)
					// transfer its vfnptrs and deactivate it

					iterator second = iter;
					++second;

					if (!second->GetVfnPtrList().empty())
					{
						// Move the vfnptrs from the old hook manager to the new one
						iter->GetVfnPtrList() = second->GetVfnPtrList();
						second->GetVfnPtrList().clear();

						// Unregister the old one, register the new one
						second->Unregister();

						// Go through all vfnptrs in this hookman and patch them to point to the new manager's handler!
						// :TODO: change this to something like for_all :X
						for (List<CVfnPtr>::iterator vfnptr_iter = iter->GetVfnPtrList().begin();
							vfnptr_iter != iter->GetVfnPtrList().end(); ++vfnptr_iter)
						{
							vfnptr_iter->Patch(iter->GetHookFunc());
						}
					}
				}

				// activate the new active hookman
				iter->Register();
			}
			
			return true;
		}

		void CHookManagerContainer::RemoveHookMans(Plugin plug, HookManagerPubFunc pubFunc)
		{
			bool erasethis = false;

			for (iterator iter = begin(); iter != end(); erasethis ? (iter = erase(iter)) : ++iter)
			{
				erasethis = false;
				if (iter->GetOwnerPlugin() == plug
					&& (pubFunc == NULL ? true : (iter->GetPubFunc() == pubFunc)))
				{
					if (iter == begin() && !iter->GetVfnPtrList().empty())
					{
						// The hook manager is active
						// -> Transfer its hooks to the next hook manager (which _has_ to exist)
						SH_ASSERT(size() > 1, ("No more hook managers!?"));

						iterator second = iter;
						++second;

						SH_ASSERT(second->GetOwnerPlugin() != plug, ("New hook manager from same plugin!"));

						second->GetVfnPtrList() = iter->GetVfnPtrList();
						iter->GetVfnPtrList().clear();

						second->Register();
						iter->Unregister();

						// Go through all vfnptrs in this hookman and patch them to point to the new manager's handler!
						// :TODO: change this to something like for_all :X
						for (List<CVfnPtr>::iterator vfnptr_iter = second->GetVfnPtrList().begin();
							vfnptr_iter != second->GetVfnPtrList().end(); ++vfnptr_iter)
						{
							vfnptr_iter->Patch(second->GetHookFunc());
						}
					}
					erasethis = true;
				}
			}
		}
		
		//////////////////////////////////////////////////////////////////////////
		// CHookManager
		//////////////////////////////////////////////////////////////////////////
		void CHookManager::SetInfo(int hookman_version, int vtbloffs, int vtblidx,
			ProtoInfo *proto, void *hookfunc_vfnptr)
		{
			m_Version = hookman_version;
			m_VtblOffs = vtbloffs;
			m_VtblIdx = vtblidx;
			m_Proto = proto;
			m_HookfuncVfnptr = hookfunc_vfnptr;
		}

		CVfnPtr &CHookManager::GetVfnPtr(void *vfnptr)
		{
			List<CVfnPtr>::iterator iter = m_VfnPtrList.find(vfnptr);
			if (iter == m_VfnPtrList.end())
			{
				CVfnPtr newVfnPtr(vfnptr);
				m_VfnPtrList.push_back(newVfnPtr);

				newVfnPtr.Patch(GetHookFunc());

				return m_VfnPtrList.back();
			}
			else
			{
				return *iter;
			}
		}

		//////////////////////////////////////////////////////////////////////////
		// CVfnPtr
		//////////////////////////////////////////////////////////////////////////
		
		bool CVfnPtr::Patch(void *newValue)
		{
			if (!SetMemAccess(m_Ptr, sizeof(void*), SH_MEM_READ | SH_MEM_WRITE))
			{
				return false;
			}

			*reinterpret_cast<void**>(m_Ptr) = newValue;

			return true;
		}

		CIface &CVfnPtr::GetIface(void *iface)
		{
			List<CIface>::iterator iter = m_IfaceList.find(iface);
			if (iter == m_IfaceList.end())
			{
				CIface newIface(iface);
				if (iface == NULL)
				{
					m_IfaceList.push_front(newIface);
					return m_IfaceList.front();
				}
				else
				{
					m_IfaceList.push_back(newIface);
					return m_IfaceList.back();
				}
			}
			else
			{
				return *iter;
			}
		}

		//////////////////////////////////////////////////////////////////////////
		// CHookIdManager
		//////////////////////////////////////////////////////////////////////////
		CHookIDManager::CHookIDManager()
		{
		}

		int CHookIDManager::New(const CProto &proto, int vtbl_offs, int vtbl_idx, void *vfnptr,
			void *adjustediface, Plugin plug, int thisptr_offs, ISHDelegate *handler, bool post)
		{
			Entry tmp(proto, vtbl_offs, vtbl_idx, vfnptr, adjustediface, plug, thisptr_offs, handler, post);

			size_t cursize = m_Entries.size();
			for (size_t i = 0; i < cursize; ++i)
			{
				if (m_Entries[i].isfree)
				{
					m_Entries[i] = tmp;
					return static_cast<int>(i) + 1;
				}
			}

			m_Entries.push_back(tmp);
			return static_cast<int>(m_Entries.size());		// return size() because hookid = id+1 anyway
		}

		bool CHookIDManager::Remove(int hookid)
		{
			int realid = hookid - 1;
			if (realid < 0 || realid >= static_cast<int>(m_Entries.size()) || m_Entries[realid].isfree)
				return false;

			m_Entries[realid].isfree = true;

			// :TODO: remove free ids from back sometimes ??

			return true;
		}

		const CHookIDManager::Entry * CHookIDManager::QueryHook(int hookid)
		{
			int realid = hookid - 1;
			if (realid < 0 || realid >= static_cast<int>(m_Entries.size()) || m_Entries[realid].isfree)
				return NULL;

			return &m_Entries[realid];
		}

		void CHookIDManager::FindAllHooks(CVector<int> &output, const CProto &proto, int vtbl_offs,
			int vtbl_idx, void *adjustediface, Plugin plug, int thisptr_offs, ISHDelegate *handler, bool post)
		{
			// oh my god, a lot of parameters...
			size_t cursize = m_Entries.size();
			for (size_t i = 0; i < cursize; ++i)
			{
				if (!m_Entries[i].isfree && m_Entries[i].proto == proto && m_Entries[i].vtbl_offs == vtbl_offs &&
					m_Entries[i].vtbl_idx == vtbl_idx && m_Entries[i].adjustediface == adjustediface && m_Entries[i].plug == plug &&
					m_Entries[i].thisptr_offs == thisptr_offs && m_Entries[i].handler->IsEqual(handler) && m_Entries[i].post == post)
				{
					output.push_back(static_cast<int>(i) + 1);
				}
			}
		}

		void CHookIDManager::FindAllHooks(CVector<int> &output)
		{
			size_t cursize = m_Entries.size();
			for (size_t i = 0; i < cursize; ++i)
			{
				if (!m_Entries[i].isfree)
					output.push_back(static_cast<int>(i) + 1);
			}
		}

		void CHookIDManager::FindAllHooks(CVector<int> &output, Plugin plug)
		{
			size_t cursize = m_Entries.size();
			for (size_t i = 0; i < cursize; ++i)
			{
				if (!m_Entries[i].isfree && m_Entries[i].plug == plug)
					output.push_back(static_cast<int>(i) + 1);
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

			// Make sure that the container exists
			CHookManagerContainer &container = m_HookManContainers.GetContainer(hookManager.GetVtblOffs(),
				hookManager.GetVtblIdx(), hookManager.GetProto());

			container.AddHookManager(hookManager);

			if (!container)
			{
				// For some weird reason, the container is empty!
				SH_ASSERT(0, ("No hookman found - but we've just added one!"));
				return 0;
			}

			CHookManager &hookMan = container.GetActiveHookManager();

			void *adjustediface = NULL;
			void **cur_vtptr = NULL;
			void *cur_vfnptr = NULL;

			// find vfnptr
			switch (mode)
			{
			case Hook_Normal:
				adjustediface = reinterpret_cast<void*>(reinterpret_cast<char*>(iface) + thisptr_offs);

				cur_vtptr = *reinterpret_cast<void***>(
					reinterpret_cast<char*>(adjustediface) + hookMan.GetVtblOffs());
				cur_vfnptr = reinterpret_cast<void*>(cur_vtptr + hookMan.GetVtblIdx());
				break;

			case Hook_VP:

				adjustediface = reinterpret_cast<void*>(reinterpret_cast<char*>(iface) + thisptr_offs);

				cur_vtptr = *reinterpret_cast<void***>(
					reinterpret_cast<char*>(adjustediface) + hookMan.GetVtblOffs());
				cur_vfnptr = reinterpret_cast<void*>(cur_vtptr + hookMan.GetVtblIdx());

				adjustediface = NULL;
				break;

			case Hook_DVP:
				adjustediface = NULL;

				cur_vtptr = reinterpret_cast<void**>(iface);
				cur_vfnptr = cur_vtptr + hookMan.GetVtblIdx();

				break;
			}

			CVfnPtr &vfnPtr = hookMan.GetVfnPtr(cur_vfnptr);
			CIface &ifaceinst = vfnPtr.GetIface(adjustediface);

			// Add the hook
			CHook hook(plug, thisptr_offs, handler, 

				m_HookIDMan.New(hookMan.GetProto(), hookMan.GetVtblOffs(), hookMan.GetVtblIdx(),
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

			// find hookman
			CHookManContainerList::iterator hmcl_iter = m_HookManContainers.find(
				CHookManagerContainer::Descriptor(hentry->vtbl_offs, hentry->vtbl_idx, hentry->proto));

			if (hmcl_iter == m_HookManContainers.end() || !(*hmcl_iter))
				return false;

			CHookManager &hookman = hmcl_iter->GetActiveHookManager();

			// find vfnptr
			List<CVfnPtr>::iterator vfnptr_iter = hookman.GetVfnPtrList().find(hentry->vfnptr);
			if (vfnptr_iter == hookman.GetVfnPtrList().end())
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

					// Only patch the vfnptr back if the module is still in memory
					// If it's not, do not remove stuff like we did before
					// First off we did it wrong (shutdown the whole hookman, uh..) and secondly applications may be
					// confused by RemoveHook returning false then (yeah, I know, I made this one up, no one checks for RemoveHook error)
					if (ModuleInMemory(reinterpret_cast<char*>(vfnptr_iter->GetPtr()), SH_PTRSIZE))
					{
						vfnptr_iter->Revert();
					}

					hookman.GetVfnPtrList().erase(vfnptr_iter);

					if (hookman.GetVfnPtrList().empty())
					{
						// Unregister the hook manager
						hookman.Unregister();
					}
				}
			}

			m_HookIDMan.Remove(hookid);
			return true;
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

		void CSourceHookImpl::UnloadPlugin(Plugin plug)
		{
			// 1) Remove all hooks by this plugin

			CVector<int> removehooks;
			m_HookIDMan.FindAllHooks(removehooks, plug);

			for (CVector<int>::iterator iter = removehooks.begin(); iter != removehooks.end(); ++iter)
				RemoveHookByID(*iter);

			// 2) Remove all hook managers
			m_HookManContainers.RemoveHookMans(plug);
		}

		void CSourceHookImpl::RemoveHookManager(Plugin plug, HookManagerPubFunc pubFunc)
		{
			// 1) Remove all its hooks
			CVector<int> removehooks;

			// Get info about hook manager
			CHookManager hookManager(plug, pubFunc);
			if (!hookManager)
				return;

			// Make sure that the container exists
			CHookManagerContainer &container = m_HookManContainers.GetContainer(hookManager.GetVtblOffs(),
				hookManager.GetVtblIdx(), hookManager.GetProto());

			CHookManagerContainer::iterator hookman_iter = container.find(CHookManager::Descriptor(plug, pubFunc));

			if (hookman_iter != container.end())
			{
				for (List<CVfnPtr>::iterator vfnptr_iter = hookman_iter->GetVfnPtrList().begin();
					vfnptr_iter != hookman_iter->GetVfnPtrList().end(); ++vfnptr_iter)
				{
					for (List<CIface>::iterator iface_iter = vfnptr_iter->GetIfaceList().begin();
						iface_iter != vfnptr_iter->GetIfaceList().end(); ++iface_iter)
					{
						List<CHook>::iterator hook_iter;
						for (hook_iter = iface_iter->GetPreHookList().begin();
							hook_iter != iface_iter->GetPreHookList().end(); ++hook_iter)
							removehooks.push_back(hook_iter->GetID());
	
						for (hook_iter = iface_iter->GetPostHookList().begin();
							hook_iter != iface_iter->GetPostHookList().end(); ++hook_iter)
							removehooks.push_back(hook_iter->GetID());
					}
				}
			}

			for (CVector<int>::iterator iter = removehooks.begin(); iter != removehooks.end(); ++iter)
				RemoveHookByID(*iter);

			// 2) Remove it
			container.RemoveHookMans(plug, pubFunc);
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
				m_ContextStack.pop();
		}

		void *CSourceHookImpl::GetOrigVfnPtrEntry(void *vfnptr)
		{
			for (CHookManContainerList::iterator hmcl_iter = m_HookManContainers.begin();
				hmcl_iter != m_HookManContainers.end(); ++hmcl_iter)
			{
				for (CHookManagerContainer::iterator hookmaniter = hmcl_iter->begin();
					hookmaniter != hmcl_iter->end(); ++hookmaniter)
				{
					for (List<CVfnPtr>::iterator vfnptr_iter = hookmaniter->GetVfnPtrList().begin();
						vfnptr_iter != hookmaniter->GetVfnPtrList().end(); ++vfnptr_iter)
					{
						if (vfnptr_iter->GetPtr() == vfnptr)
							return vfnptr_iter->GetOrigEntry();
					}
				}
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

		IHookContext *CSourceHookImpl::SetupHookLoop(IHookManagerInfo *hi, void *vfnptr, void *thisptr, void **origentry, META_RES *statusPtr,
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

					List<CVfnPtr> &vfnptrList = static_cast<CHookManager*>(hi)->GetVfnPtrList();
					List<CVfnPtr>::iterator vfnptr_iter = vfnptrList.find(vfnptr);
					if (vfnptr_iter == vfnptrList.end())
					{
						// :TODO: what??
					}
					else
					{
						*origentry = vfnptr_iter->GetOrigEntry();
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

			List<CVfnPtr> &vfnptrList = static_cast<CHookManager*>(hi)->GetVfnPtrList();
			List<CVfnPtr>::iterator vfnptr_iter = vfnptrList.find(vfnptr);
			if (vfnptr_iter == vfnptrList.end())
			{
				pCtx->m_State = CHookContext::State_Dead;
			}
			else
			{
				pCtx->pVfnPtr = &(*vfnptr_iter);
				*origentry = vfnptr_iter->GetOrigEntry();
				pCtx->pIface = vfnptr_iter->FindIface(thisptr);
			}

			pCtx->pStatus = statusPtr;
			pCtx->pPrevRes = prevResPtr;
			pCtx->pCurRes = curResPtr;
			pCtx->pThisPtr = thisptr;
			pCtx->pOverrideRet = overrideRetPtr;
			pCtx->pOrigRet = origRetPtr;

			return pCtx;
		}

		void CSourceHookImpl::EndContext(IHookContext *pCtx)
		{
			m_ContextStack.pop();
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

			// find hookman
			CHookManContainerList::iterator hmcl_iter = m_HookManContainers.find(
				CHookManagerContainer::Descriptor(hentry->vtbl_offs, hentry->vtbl_idx, hentry->proto));

			if (hmcl_iter == m_HookManContainers.end() || !(*hmcl_iter))
				return false;

			CHookManager &hookman = hmcl_iter->GetActiveHookManager();

			// find vfnptr
			List<CVfnPtr>::iterator vfnptr_iter = hookman.GetVfnPtrList().find(hentry->vfnptr);
			if (vfnptr_iter == hookman.GetVfnPtrList().end())
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
				pVfnPtr = NULL;
				m_State = State_Dead;
			}
		}
	}
}
