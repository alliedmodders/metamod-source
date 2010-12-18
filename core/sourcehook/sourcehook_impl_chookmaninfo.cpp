/* ======== SourceHook ========
* Copyright (C) 2004-2010 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* Contributors: Scott "Damaged Soul" Ehlert
* ============================
*/

#include "sourcehook_impl.h"

namespace SourceHook
{
	namespace Impl
	{
		CHookManager::CHookManager(Plugin ownerPlugin, HookManagerPubFunc pubFunc)
			: m_OwnerPlugin(ownerPlugin), m_PubFunc(pubFunc), m_Version(-1)
		{
			// Query pubfunc
			//  -> Should call SetInfo and set all the other variables!
			if (m_PubFunc(false, this) != 0)
			{
				// Error!
				m_Version = -1;
			}
		}

		void CHookManager::SetInfo(int hookman_version, int vtbloffs, int vtblidx,
			ProtoInfo *proto, void *hookfunc_vfnptr)
		{
			m_Version = hookman_version;
			m_VtblOffs = vtbloffs;
			m_VtblIdx = vtblidx;
			m_Proto = proto;
			m_HookfuncVfnptr = hookfunc_vfnptr;
		}

		void CHookManager::Register()
		{
			m_PubFunc(true, this);
		}

		void CHookManager::Unregister()
		{
			m_PubFunc(true, NULL);
		}

		void CHookManager::IncrRef(CVfnPtr *pVfnPtr)
		{
			m_VfnPtrs.push_back(pVfnPtr);
			if (m_VfnPtrs.size() == 1)
				Register();
		}

		void CHookManager::DecrRef(CVfnPtr *pVfnPtr)
		{
			m_VfnPtrs.remove(pVfnPtr);
			if (m_VfnPtrs.empty())
				Unregister();
		}

		CHookManager *CHookManList::GetHookMan(Plugin plug, HookManagerPubFunc pubFunc)
		{
			CHookManager hm(plug, pubFunc);
			return GetHookMan(hm);
		}

		CHookManager *CHookManList::GetHookMan(CHookManager &hm)
		{
			iterator iter = find(hm);
			if (iter == end())
			{
				push_back(hm);
				return &(back());
			}
			else
			{
				return &(*iter);
			}
		}
	}
}
