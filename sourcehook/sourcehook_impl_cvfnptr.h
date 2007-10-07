/* ======== SourceHook ========
* Copyright (C) 2004-2007 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

#ifndef __SOURCEHOOK_IMPL_CVFNPTR_H__
#define __SOURCEHOOK_IMPL_CVFNPTR_H__

#include "sh_list.h"

namespace SourceHook
{
	namespace Impl
	{
		class CVfnPtr
		{
			// *** Data ***
			void *m_Ptr;
			void *m_OrigEntry;

			List<CIface> m_IfaceList;
		public:
			// *** Descriptor ***
			typedef void* Descriptor;

			// *** Interface ***
			inline CVfnPtr(void *ptr);
			inline bool operator==(const Descriptor &other);
			inline void *GetPtr() const;
			inline void *GetOrigEntry() const;
			inline List<CIface> &GetIfaceList();
			inline const List<CIface> &GetIfaceList() const;
			inline CIface *FindIface(void *iface);
			CIface &GetIface(void *iface);
			bool Patch(void *newValue);
			inline bool Revert();
		};

		// *** Implementation ***
		inline CVfnPtr::CVfnPtr(void *ptr)
			: m_Ptr(ptr), m_OrigEntry(*reinterpret_cast<void**>(m_Ptr))
		{
		}

		inline bool CVfnPtr::operator==(const Descriptor &other)
		{
			return m_Ptr == other;
		}

		inline void *CVfnPtr::GetPtr() const
		{
			return m_Ptr;
		}

		inline void *CVfnPtr::GetOrigEntry() const
		{
			return m_OrigEntry;
		}

		inline List<CIface> &CVfnPtr::GetIfaceList()
		{
			return m_IfaceList;
		}

		inline const List<CIface> &CVfnPtr::GetIfaceList() const
		{
			return m_IfaceList;
		}

		inline bool CVfnPtr::Revert()
		{
			return Patch(m_OrigEntry);
		}

		inline CIface *CVfnPtr::FindIface(void *iface)
		{
			List<CIface>::iterator iter = m_IfaceList.find(iface);
			if (iter == m_IfaceList.end())
				return NULL;
			else
				return &(*iter);
		}
	}
}

#endif