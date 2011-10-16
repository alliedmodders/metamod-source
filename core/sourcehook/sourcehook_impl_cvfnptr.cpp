/* ======== SourceHook ========
* Copyright (C) 2004-2010 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

#include "sourcehook_impl.h"

namespace SourceHook
{
	namespace Impl
	{
		CPageAlloc CVfnPtr::ms_AlignedPageAllocator(8);

		CVfnPtr::CVfnPtr(void *ptr)
			: m_Ptr(ptr), m_OrigEntry(*reinterpret_cast<void**>(m_Ptr)),
			m_OrigCallThunk(NULL)
		{
		}

		CVfnPtr::~CVfnPtr()
		{
			if (!m_HookMans.empty())
				m_HookMans.front()->DecrRef(this);
		}

		bool CVfnPtr::Init()
		{
			// Initalize the vfn ptr info object.

			// If we're running on GCC and the original vtable entry is odd
			// we have a problem.
			// The hook functions use non-virtual member function pointers to
			// call the original function (see sourcehook.hxx, SH_SETUP_MFP and SH_CALL_ORIG)
			// GCC has the same format for virtual and non-virtual MFPs:
			// virtual ones have an odd value as "funcptr" where the offset into the vtable is encoded
			// non-virtual ones have the direct function pointer as "funcptr", assuming that it is even

			// When m_OrigEntry is odd, GCC's runtime MFP calling code interpretes it as a virtual
			// function call though we want to call the function in a non-virtual way
			// (the original function call mechanism between the pre and post hook loop has to bypass
			// the virtual calling mechanism in order to call the original function).

#if SH_COMP==SH_COMP_GCC
			if ((((ptrdiff_t)m_OrigEntry) & 1) != 0)
			{
				// Odd orig entry.
				if (SH_PTRSIZE != 4)
				{
					// We only have code for IA32 atm!
					return false;
				}

				// Generate a new thunk
				m_OrigCallThunk = ms_AlignedPageAllocator.Alloc(5);
				ms_AlignedPageAllocator.SetRW(m_OrigCallThunk);

				unsigned char* thunkBase = reinterpret_cast<unsigned char*>(m_OrigCallThunk);
				*(thunkBase + 0) = 0xE9;		// offset jump, immediate operand
				ptrdiff_t *offsetAddr = reinterpret_cast<ptrdiff_t*>(thunkBase + 1);
				
				// destination = src + offset + 5
				// <=>  offset = destination - src - 5
				*offsetAddr =
					(reinterpret_cast<unsigned char*>(m_OrigEntry) - thunkBase) - 5;

				ms_AlignedPageAllocator.SetRE(m_OrigCallThunk);
			}
#endif
			return true;
		}

		class CVfnPtrOrigThunkCleanup : public ICleanupTask
		{
			CPageAlloc *m_Allocator;
			void *m_AddrToFree;

		public:
			CVfnPtrOrigThunkCleanup(CPageAlloc *allocator, void *addrToFree) :
				m_Allocator(allocator), m_AddrToFree(addrToFree)
			{
			}

			virtual void CleanupAndDeleteThis()
			{
				m_Allocator->Free(m_AddrToFree);
				delete this;
			}
		};

		ICleanupTask* CVfnPtr::GetCleanupTask()
		{
			if (m_OrigCallThunk != NULL)
			{
				return new CVfnPtrOrigThunkCleanup(&ms_AlignedPageAllocator, m_OrigCallThunk);
			}
			else
			{
				return NULL;
			}
		}

		void* CVfnPtr::GetOrigCallAddr() const
		{
			if (m_OrigCallThunk)
			{
				return m_OrigCallThunk;
			}
			else
			{
				return m_OrigEntry;
			}
		}

		bool CVfnPtr::Revert()
		{
			// Only patch the vfnptr back if the module is still in memory
			// If it's not, do not remove stuff like we did before
			// First off we did it wrong (shutdown the whole hookman, uh..) and secondly applications may be
			// confused by RemoveHook returning false then (yeah, I know, I made this one up, no one checks for RemoveHook error)
			if (ModuleInMemory(reinterpret_cast<char*>(m_Ptr), SH_PTRSIZE))
			{
				return Patch(m_OrigEntry);
			}
			else
			{
				return true;
			}
		}

		CIface *CVfnPtr::FindIface(void *iface)
		{
			List<CIface>::iterator iter = m_IfaceList.find(iface);
			if (iter == m_IfaceList.end())
				return NULL;
			else
				return &(*iter);
		}

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
			if (!MakePageWritable(m_Ptr))
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
	}
}
