/* ======== SourceHook ========
* Copyright (C) 2004-2007 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

#ifndef __SOURCEHOOK_IMPL_CHOOKMANICONT_H__
#define __SOURCEHOOK_IMPL_CHOOKMANICONT_H__

#include "sh_list.h"

namespace SourceHook
{
	namespace Impl
	{
		class CHookManagerContainer : public List<CHookManager>
		{
			// *** Data ***
			int m_VtblOffs;
			int m_VtblIdx;
			CProto m_Proto;
		public:
			// *** Descriptor ***
			struct Descriptor
			{
				int m_VtblOffs;
				int m_VtblIdx;
				const CProto &m_Proto;		// we can assume that a Descriptor won't live for a long time

				Descriptor(int vtblOffs, int vtblIdx, const CProto &proto)
					: m_VtblOffs(vtblOffs), m_VtblIdx(vtblIdx), m_Proto(proto)
				{
				}
			};

			// *** Interface ***
			inline CHookManagerContainer(int vtblOffs, int vtblIdx, const CProto &proto);
			inline CHookManagerContainer(const CHookManager &hookMan);

			inline bool operator==(const Descriptor &other) const;
			inline operator bool() const;

			inline int GetVtblOffs() const;
			inline int GetVtblIdx() const;
			inline const CProto &GetProto() const;

			bool AddHookManager(const CHookManager &hookMan);
			inline CHookManager &GetActiveHookManager();
			inline const CHookManager &GetActiveHookManager() const;

			void RemoveHookMans(Plugin plug, HookManagerPubFunc pubFunc = NULL);
		};

		// *** Implementation ***
		inline CHookManagerContainer::CHookManagerContainer(int vtblOffs, int vtblIdx, const CProto &proto)
			: m_VtblOffs(vtblOffs), m_VtblIdx(vtblIdx), m_Proto(proto)
		{
		}

		inline CHookManagerContainer::CHookManagerContainer(const CHookManager &hookMan)
			: m_VtblOffs(hookMan.GetVtblOffs()), m_VtblIdx(hookMan.GetVtblIdx()), m_Proto(hookMan.GetProto())
		{
			push_back(hookMan);
		}

		inline bool CHookManagerContainer::operator==(const Descriptor &other) const
		{
			return m_VtblOffs == other.m_VtblOffs
				&& m_VtblIdx == other.m_VtblIdx
				&& m_Proto == other.m_Proto;
		}

		inline CHookManagerContainer::operator bool() const
		{
			return !empty();
		}

		inline int CHookManagerContainer::GetVtblOffs() const
		{
			return m_VtblOffs;
		}

		inline int CHookManagerContainer::GetVtblIdx() const
		{
			return m_VtblIdx;
		}

		inline const CProto &CHookManagerContainer::GetProto() const
		{
			return m_Proto;
		}

		inline CHookManager &CHookManagerContainer::GetActiveHookManager()
		{
			begin()->Register();
			return *begin();
		}

		const CHookManager &CHookManagerContainer::GetActiveHookManager() const
		{
			begin()->Register();
			return *begin();
		}
	}
}

#endif