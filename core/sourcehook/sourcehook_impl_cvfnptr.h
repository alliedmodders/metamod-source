/* ======== SourceHook ========
* Copyright (C) 2004-2010 Metamod:Source Development Team
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
#include "sh_memory.h"
#include "sh_pagealloc.h"
#include "sourcehook_impl_cleanuptask.h"

namespace SourceHook
{
	namespace Impl
	{
		class CVfnPtr
		{
			static CPageAlloc ms_AlignedPageAllocator;

			// *** Data ***
			void *m_Ptr;
			void *m_OrigEntry;
			void *m_OrigCallThunk;		// See Init() method

			List<CHookManager*> m_HookMans;
			List<CIface> m_IfaceList;
		public:
			// *** Descriptor ***
			typedef void* Descriptor;

			// *** Interface ***
			CVfnPtr(void *ptr);
			~CVfnPtr();
			bool Init();
			inline bool operator==(const Descriptor &other);
			inline void *GetPtr() const;
			inline void *GetOrigEntry() const;
			void *GetOrigCallAddr() const;
			inline List<CIface> &GetIfaceList();
			inline const List<CIface> &GetIfaceList() const;
			CIface *FindIface(void *iface);
			CIface &GetIface(void *iface);
			bool Patch(void *newValue);
			bool Revert();

			ICleanupTask *GetCleanupTask();

			void AddHookMan(CHookManager *pHookMan);
			// If this returns false, it means that there is no hook manager left
			// to use and that this vfnptr should be removed.
			bool HookManRemoved(CHookManager *pHookMan);
		};

		// *** Implementation ***

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
	}
}

#endif

