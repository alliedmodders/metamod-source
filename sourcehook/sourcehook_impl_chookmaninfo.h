/* ======== SourceHook ========
* Copyright (C) 2004-2007 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

#ifndef __SOURCEHOOK_IMPL_CHOOKMANINFO_H__
#define __SOURCEHOOK_IMPL_CHOOKMANINFO_H__

#include "sh_list.h"
#include "sourcehook_impl_cproto.h"
#include "sourcehook_hookmangen.h"

namespace SourceHook
{
	namespace Impl
	{
		class CVfnPtr;
		
		class CHookManager : public IHookManagerInfo
		{
			// *** Data ***
			Plugin m_OwnerPlugin;
			HookManagerPubFunc m_PubFunc;
			int m_VtblOffs;
			int m_VtblIdx;
			CProto m_Proto;
			int m_Version;

			void *m_HookfuncVfnptr;

			List<CVfnPtr*> m_VfnPtrs;
		public:
			// *** Descriptor ***
			struct Descriptor
			{
				Plugin m_OwnerPlugin;
				HookManagerPubFunc m_PubFunc;
				Descriptor(Plugin ownerPlugin, HookManagerPubFunc pubFunc)
					: m_OwnerPlugin(ownerPlugin), m_PubFunc(pubFunc)
				{
				}
			};

			// *** Interface ***
			inline CHookManager(Plugin ownerPlugin, HookManagerPubFunc pubFunc);

			inline bool operator==(const Descriptor &other) const;
			inline bool operator==(const CHookManager &other) const;

			inline operator bool()  const;
			inline Plugin GetOwnerPlugin()  const;
			inline int GetVtblOffs()  const;
			inline int GetVtblIdx()  const;
			inline const CProto &GetProto()  const;
			inline int GetVersion()  const;
			inline void *GetHookFunc() const;
			inline HookManagerPubFunc GetPubFunc() const;

			inline void Register();
			inline void Unregister();

			inline void IncrRef(CVfnPtr *pVfnPtr);
			inline void DecrRef(CVfnPtr *pVfnPtr);

			List<CVfnPtr*> &GetVfnPtrList()
			{
				return m_VfnPtrs;
			}

			// *** IHookManagerInfo interface ***
			void SetInfo(int hookman_version, int vtbloffs, int vtblidx,
				ProtoInfo *proto, void *hookfunc_vfnptr);
		};

		class CHookManList : public List<CHookManager>
		{
		public:
			inline CHookManager *GetHookMan(Plugin plug, HookManagerPubFunc pubFunc);
			inline CHookManager *GetHookMan(CHookManager &hm);
		};

		// *** Implementation ***
		inline CHookManager::CHookManager(Plugin ownerPlugin, HookManagerPubFunc pubFunc)
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

		inline CHookManager::operator bool() const
		{
			return m_Version != -1;
		}

		inline bool CHookManager::operator==(const Descriptor &other) const
		{
			return m_OwnerPlugin == other.m_OwnerPlugin
				&& m_PubFunc == other.m_PubFunc;
		}

		inline bool CHookManager::operator==(const CHookManager &other) const
		{
			return m_OwnerPlugin == other.m_OwnerPlugin
				&& m_PubFunc == other.m_PubFunc;
		}

		inline Plugin CHookManager::GetOwnerPlugin() const
		{
			return m_OwnerPlugin;
		}

		inline int CHookManager::GetVtblOffs() const
		{
			return m_VtblOffs;
		}

		inline int CHookManager::GetVtblIdx() const
		{
			return m_VtblIdx;
		}

		inline const CProto &CHookManager::GetProto() const
		{
			return m_Proto;
		}

		inline int CHookManager::GetVersion() const
		{
			return m_Version;
		}

		inline void *CHookManager::GetHookFunc() const
		{
			return *reinterpret_cast<void**>(m_HookfuncVfnptr);
		}

		inline HookManagerPubFunc CHookManager::GetPubFunc() const
		{
			return m_PubFunc;
		}

		inline void CHookManager::Register()
		{
			m_PubFunc(true, this);
		}

		inline void CHookManager::Unregister()
		{
			m_PubFunc(true, NULL);
		}

		inline void CHookManager::IncrRef(CVfnPtr *pVfnPtr)
		{
			m_VfnPtrs.push_back(pVfnPtr);
			if (m_VfnPtrs.size() == 1)
				Register();
		}

		inline void CHookManager::DecrRef(CVfnPtr *pVfnPtr)
		{
			m_VfnPtrs.remove(pVfnPtr);
			if (m_VfnPtrs.empty())
				Unregister();
		}

		inline CHookManager *CHookManList::GetHookMan(Plugin plug, HookManagerPubFunc pubFunc)
		{
			CHookManager hm(plug, pubFunc);
			return GetHookMan(hm);
		}

		inline CHookManager *CHookManList::GetHookMan(CHookManager &hm)
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

#endif

