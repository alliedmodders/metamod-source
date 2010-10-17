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

	}
}
