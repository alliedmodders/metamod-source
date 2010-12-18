/* ======== SourceHook ========
* Copyright (C) 2004-2010 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

#ifndef __SOURCEHOOK_IMPL_CIFACE_H__
#define __SOURCEHOOK_IMPL_CIFACE_H__

#include "sh_list.h"

namespace SourceHook
{
	namespace Impl
	{
		class CIface
		{
			// *** Data ***
			void *m_Ptr;

			List<CHook> m_PreHooks;
			List<CHook> m_PostHooks;
		public:

			// *** Descriptor ***
			typedef void* Descriptor;

			// *** Interface ***
			inline CIface(void *ptr);
			inline ~CIface();
			inline bool operator==(const Descriptor &other);
			inline void *GetPtr() const;
			inline List<CHook> &GetPreHookList();
			inline List<CHook> &GetPostHookList();
			inline const List<CHook> &GetPreHookList() const;
			inline const List<CHook> &GetPostHookList() const;
		};

		// *** Implementation ***
		inline CIface::CIface(void *ptr)
			: m_Ptr(ptr)
		{
		}

		inline CIface::~CIface()
		{
			// Before getting deleted, delete all remaining hook handlers
			for (List<CHook>::iterator iter = m_PreHooks.begin(); iter != m_PreHooks.end(); ++iter)
			{
				iter->GetHandler()->DeleteThis();
			}

			for (List<CHook>::iterator iter = m_PostHooks.begin(); iter != m_PostHooks.end(); ++iter)
			{
				iter->GetHandler()->DeleteThis();
			}
		}

		inline bool CIface::operator==(const Descriptor &other)
		{
			return m_Ptr == other;
		}

		inline void *CIface::GetPtr() const
		{
			return m_Ptr;
		}

		inline List<CHook> &CIface::GetPreHookList()
		{
			return m_PreHooks;
		}

		inline List<CHook> &CIface::GetPostHookList()
		{
			return m_PostHooks;
		}

		inline const List<CHook> &CIface::GetPreHookList() const
		{
			return m_PreHooks;
		}

		inline const List<CHook> &CIface::GetPostHookList() const
		{
			return m_PostHooks;
		}
	}
}

#endif

