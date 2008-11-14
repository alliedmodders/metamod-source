/* ======== SourceHook ========
* Copyright (C) 2004-2008 Metamod:Source Development Team
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
		// CProto
		//////////////////////////////////////////////////////////////////////////
		void CProto::Fill(const ProtoInfo *pProto)
		{
			if (pProto == NULL)
				m_Version = -1;

			m_ParamsPassInfo.clear();

			if (pProto->paramsPassInfo[0].size == 0)
			{
				// Version 1
				m_Version = 0;
				m_Convention = pProto->convention;
				m_NumOfParams = pProto->numOfParams;

				m_RetPassInfo.size = pProto->retPassInfo.size;
				m_RetPassInfo.type = pProto->retPassInfo.type;
				m_RetPassInfo.flags = GetRealFlags(pProto->retPassInfo);

				m_RetPassInfo.pNormalCtor = NULL;
				m_RetPassInfo.pCopyCtor = NULL;
				m_RetPassInfo.pDtor = NULL;
				m_RetPassInfo.pAssignOperator = NULL;

				
				m_ParamsPassInfo.resize(pProto->numOfParams);

				for (int i = 1; i <= pProto->numOfParams; ++i)
				{
					m_ParamsPassInfo[i-1].size = pProto->paramsPassInfo[i].size;
					m_ParamsPassInfo[i-1].type = pProto->paramsPassInfo[i].type;
					m_ParamsPassInfo[i-1].flags = GetRealFlags(pProto->paramsPassInfo[i]);

					m_ParamsPassInfo[i-1].pNormalCtor = NULL;
					m_ParamsPassInfo[i-1].pCopyCtor = NULL;
					m_ParamsPassInfo[i-1].pDtor = NULL;
					m_ParamsPassInfo[i-1].pAssignOperator = NULL;
				}
			}
			else if (pProto->paramsPassInfo[0].size == 1)
			{
				// Version 2
				m_Version = 1;
				m_Convention = pProto->convention;
				m_NumOfParams = pProto->numOfParams;

				m_RetPassInfo.size = pProto->retPassInfo.size;
				m_RetPassInfo.type = pProto->retPassInfo.type;
				m_RetPassInfo.flags = pProto->retPassInfo.flags;

				m_RetPassInfo.pNormalCtor = pProto->retPassInfo2.pNormalCtor;
				m_RetPassInfo.pCopyCtor = pProto->retPassInfo2.pCopyCtor;
				m_RetPassInfo.pDtor = pProto->retPassInfo2.pDtor;
				m_RetPassInfo.pAssignOperator = pProto->retPassInfo2.pAssignOperator;

				m_ParamsPassInfo.resize(pProto->numOfParams);

				for (int i = 1; i <= pProto->numOfParams; ++i)
				{
					m_ParamsPassInfo[i-1].size = pProto->paramsPassInfo[i].size;
					m_ParamsPassInfo[i-1].type = pProto->paramsPassInfo[i].type;
					m_ParamsPassInfo[i-1].flags = pProto->paramsPassInfo[i].flags;

					m_ParamsPassInfo[i-1].pNormalCtor = pProto->paramsPassInfo2[i].pNormalCtor;
					m_ParamsPassInfo[i-1].pCopyCtor = pProto->paramsPassInfo2[i].pCopyCtor;
					m_ParamsPassInfo[i-1].pDtor = pProto->paramsPassInfo2[i].pDtor;
					m_ParamsPassInfo[i-1].pAssignOperator = pProto->paramsPassInfo2[i].pAssignOperator;
				}
			}
			else
			{
				// Unknown
				m_Version = -1;
			}
		}

		// Basic compat test
		// Other than this, we assume that the plugins know what they're doing
		bool CProto::operator == (const CProto &other) const
		{
			if (m_Version < 0 || other.GetVersion() < 0)
				return false;

			if (m_NumOfParams != other.GetNumOfParams())
				return false;

			if (m_Convention != ProtoInfo::CallConv_Unknown && other.GetConvention() != ProtoInfo::CallConv_Unknown &&
				m_Convention != other.GetConvention())
				return false;

			if (GetRealSize(GetRet()) != GetRealSize(other.GetRet()))
				return false;

			for (int i = 0; i < m_NumOfParams; ++i)
			{
				if (GetRealSize(GetParam(i)) != GetRealSize(other.GetParam(i)))
					return false;
				if (GetParam(i).type != PassInfo::PassType_Unknown && other.GetParam(i).type != PassInfo::PassType_Unknown)
				{
					if (GetParam(i).type != other.GetParam(i).type)
						return false;
					if (GetParam(i).flags != other.GetParam(i).flags)
						return false;
				}
			}

			return true;
		}

		bool CProto::ExactlyEqual(const CProto &other) const
		{
			if (m_Version != other.m_Version ||
				m_NumOfParams != other.m_NumOfParams ||
				m_Convention != other.m_Convention ||
				GetRet() != other.GetRet())
			{
				return false;
			}

			for (int i = 0; i < m_NumOfParams; ++i)
			{
				if(GetParam(i) != other.GetParam(i))
					return false;
			}

			return true;
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

		//////////////////////////////////////////////////////////////////////////
		// CVfnPtrList
		//////////////////////////////////////////////////////////////////////////

		CVfnPtr &CVfnPtrList::GetVfnPtr(void *vfnptr)
		{
			iterator iter = find(vfnptr);
			if (iter == end())
			{
				CVfnPtr newVfnPtr(vfnptr);
				push_back(newVfnPtr);

				return back();
			}
			else
			{
				return *iter;
			}
		}
		//////////////////////////////////////////////////////////////////////////
		// CVfnPtr
		//////////////////////////////////////////////////////////////////////////
		
		void CVfnPtr::AddHookMan(CHookManager *pHookMan)
		{
			List<CHookManager*>::iterator iter;

			// Don't accept invalid hook managers
			if (!*pHookMan)
				return;

			// Check whether this hook manager already exists; if yes, ignore.
			iter = m_HookMans.find(pHookMan);
			if (iter != m_HookMans.end())
				return;

			// It doesn't -> add it. Add it to the end of its version group.
			for (iter = m_HookMans.begin(); iter != m_HookMans.end(); ++iter)
			{
				if ((*iter)->GetVersion() < pHookMan->GetVersion())
					break;
			}

			bool isBeginning = iter == m_HookMans.begin();

			m_HookMans.insert(iter, pHookMan);

			if (isBeginning)
			{
				pHookMan->IncrRef(this);
				if (m_HookMans.size() > 1)
				{
					// If another hookman was used until now but this one is better
					// (which it is because it's the first -> it has a higher version)
					// -> switch!

					List<CHookManager*>::iterator second = m_HookMans.begin();
					++second;

					(*second)->DecrRef(this);
				}

				// Make sure that this vfnptr points at it
				Patch(pHookMan->GetHookFunc());
			}
		}

		bool CVfnPtr::HookManRemoved(CHookManager *pHookMan)
		{
			// Don't accept invalid hook managers
			if (!*pHookMan)
				return true;

			List<CHookManager*>::iterator iter = m_HookMans.find(pHookMan);
			if (iter == m_HookMans.end())
				return true;							// Didn't exist here anyway

			if (iter == m_HookMans.begin())
			{
				// It is the first one!
				pHookMan->DecrRef(this);
				m_HookMans.erase(iter);

				if (m_HookMans.empty())
					return false;				// No more hookmans -> let SH delete us

				// Activate second -> now first hookman
				m_HookMans.front()->IncrRef(this);
				Patch(m_HookMans.front()->GetHookFunc());
			}
			else
			{
				m_HookMans.erase(iter);
			}
			return true;
		}

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


		void CHookIDManager::RemoveAll(void *vfnptr)
		{
			size_t cursize = m_Entries.size();
			for (size_t i = 0; i < cursize; ++i)
			{
				if (!m_Entries[i].isfree && m_Entries[i].vfnptr == vfnptr)
					m_Entries[i].isfree = true;
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

			CVfnPtr &vfnPtr = m_VfnPtrs.GetVfnPtr(cur_vfnptr);
			vfnPtr.AddHookMan(m_HookManList.GetHookMan(hookManager));
			CIface &ifaceinst = vfnPtr.GetIface(adjustediface);

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

					vfnptr_iter->Revert();

					m_VfnPtrs.erase(vfnptr_iter);
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
			for (CHookManList::iterator iter = m_HookManList.begin(); iter != m_HookManList.end(); )
			{
				if (iter->GetOwnerPlugin() == plug)
					iter = RemoveHookManager(iter);
				else
					++iter;
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

					vfnptr_iter->Revert();

					m_HookIDMan.RemoveAll(vfnptr_iter->GetPtr());
					vfnptr_iter = m_VfnPtrs.erase(vfnptr_iter);
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
				m_ContextStack.pop();
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
						*origentry = (*vfnptr_iter)->GetOrigEntry();
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
				*origentry = pCtx->pVfnPtr->GetOrigEntry();
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
				pVfnPtr = NULL;
				m_State = State_Dead;
			}
		}
	}
}
