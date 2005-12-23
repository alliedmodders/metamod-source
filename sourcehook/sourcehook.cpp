/* ======== SourceHook ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
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
	int SourceHook::HashFunction<int>(const int & k)
	{
		return k;
	}
	template<>
	int SourceHook::Compare<int>(const int & k1, const int & k2)
	{
		if (k1 == k2)
			return 0;
		if (k1 > k2)
			return 1;
		if (k1 < k2)
			return -1;
		return 0;
	}
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

	bool CSourceHookImpl::IsPluginInUse(Plugin plug)
	{
		// Iterate through all hook managers which are in this plugin
		// Iterate through their vfnptrs, ifaces, hooks
		// If a hook from an other plugin is found, return true
		// Return false otherwise
#define TMP_CHECK_LIST(name) \
	for (hook_iter = iface_iter->name.m_List.begin(); hook_iter != iface_iter->name.m_List.end(); ++hook_iter) \
		if (hook_iter->plug == plug) \
			return true;

		for (HookManInfoList::iterator hmil_iter = m_HookMans.begin(); hmil_iter != m_HookMans.end(); ++hmil_iter)
		{
			if (hmil_iter->m_Plug != plug)
				continue;
			for (CHookManagerInfo::VfnPtrListIter vfnptr_iter = hmil_iter->m_VfnPtrs.begin();
				vfnptr_iter != hmil_iter->m_VfnPtrs.end(); ++vfnptr_iter)
			{
				for (CVfnPtr::IfaceListIter iface_iter = vfnptr_iter->m_Ifaces.begin();
					iface_iter != vfnptr_iter->m_Ifaces.end(); ++iface_iter)
				{
					List<HookInfo>::iterator hook_iter;
					TMP_CHECK_LIST(m_PreHooks);
					TMP_CHECK_LIST(m_PostHooks);
				}
			}
		}
#undef TMP_CHECK_LIST
		return false;
	}

	void CSourceHookImpl::UnloadPlugin(Plugin plug)
	{
		// 1) Manually remove all hooks by this plugin
		List<RemoveHookInfo> hookstoremove;
		HookManInfoList::iterator hmil_iter;

#define TMP_CHECK_LIST(name, ispost) \
	for (hook_iter = iface_iter->name.m_List.begin(); hook_iter != iface_iter->name.m_List.end(); ++hook_iter) \
		if (hook_iter->plug == plug) \
			hookstoremove.push_back(RemoveHookInfo(hook_iter->plug, iface_iter->m_Ptr, \
			hook_iter->thisptr_offs, hmil_iter->m_Func, hook_iter->handler, ispost))

		for (hmil_iter = m_HookMans.begin(); hmil_iter != m_HookMans.end(); ++hmil_iter)
		{
			for (CHookManagerInfo::VfnPtrListIter vfnptr_iter = hmil_iter->m_VfnPtrs.begin();
				vfnptr_iter != hmil_iter->m_VfnPtrs.end(); ++vfnptr_iter)
			{
				for (CVfnPtr::IfaceListIter iface_iter = vfnptr_iter->m_Ifaces.begin();
					iface_iter != vfnptr_iter->m_Ifaces.end(); ++iface_iter)
				{
					List<HookInfo>::iterator hook_iter;
					TMP_CHECK_LIST(m_PreHooks, false);
					TMP_CHECK_LIST(m_PostHooks, true);
				}
			}
		}
#undef TMP_CHECK_LIST

		for (List<RemoveHookInfo>::iterator rmiter = hookstoremove.begin(); rmiter != hookstoremove.end(); ++rmiter)
			RemoveHook(*rmiter);

		// 2) Other plugins may use hook managers in this plugin.
		// Get a list of hook managers that are in this plugin and are used by other plugins
		// Delete all hook managers that are in this plugin

		HookManInfoList tmphookmans;
		bool erase = false;
		for (hmil_iter = m_HookMans.begin(); hmil_iter != m_HookMans.end();
			erase ? hmil_iter=m_HookMans.erase(hmil_iter) : ++hmil_iter)
		{
			if (hmil_iter->m_Plug == plug)
			{
				if (!hmil_iter->m_VfnPtrs.empty())
				{
					// All hooks by this plugin are already removed
					// So if there is a vfnptr, it has to be used by an other plugin
					tmphookmans.push_back(*hmil_iter);
				}
				erase = true;
			}
			else
				erase = false;
		}

		// For each hook manager that is used in an other plugin:
		for (hmil_iter = tmphookmans.begin(); hmil_iter != tmphookmans.end(); ++hmil_iter)
		{
			// Find a suitable hook manager in an other plugin
			HookManInfoList::iterator newHookMan = FindHookMan(m_HookMans.begin(), m_HookMans.end(),
				hmil_iter->m_Proto, hmil_iter->m_VtblOffs, hmil_iter->m_VtblIdx);

			// This should _never_ happen.
			// If there is a hook from an other plugin, the plugin must have provided a hook manager as well.
			SH_ASSERT(newHookMan != m_HookMans.end(),
				("Could not find a suitable hook manager in an other plugin!"));

			// AddHook should make sure that every plugin only has _one_ hook manager for _one_ proto/vi/vo
			SH_ASSERT(newHookMan->m_Plug != plug, ("New hook manager from same plugin!"));

			// The first hook manager should be always used - so the new hook manager has to be empty
			SH_ASSERT(newHookMan->m_VfnPtrs.empty(), ("New hook manager not empty!"));

			// Move the vfnptrs from the old hook manager to the new one
			newHookMan->m_VfnPtrs = hmil_iter->m_VfnPtrs;

			// Unregister the old one, register the new one
			hmil_iter->m_Func(HA_Unregister, NULL);
			newHookMan->m_Func(HA_Register, &(*newHookMan));

			// zOMG BAIL, here is part of what you wanted:

			// Go through all vfnptrs in this hookman and patch them to point to the new manager's handler!
			// or whatever
			for (CHookManagerInfo::VfnPtrListIter vfnptr_iter = newHookMan->m_VfnPtrs.begin();
				vfnptr_iter != newHookMan->m_VfnPtrs.end(); ++vfnptr_iter)
			{
				// And DEREFERENCE newHookMan->m_HookfuncVfnptr!
				// otherwise it will be executing the vtable... had to find out the hard way
				*reinterpret_cast<void**>(vfnptr_iter->m_Ptr) = *reinterpret_cast<void**>(newHookMan->m_HookfuncVfnptr);
			}

			// That should fix it, bail!
		}
	}

	void CSourceHookImpl::RemoveHookManager(Plugin plug, HookManagerPubFunc pubFunc)
	{
		// Moo!

		CHookManagerInfo tmp;
		if (pubFunc(HA_GetInfo, &tmp) != 0)
			return;

		HookManInfoList::iterator hmil_iter = FindHookMan(m_HookMans.begin(), m_HookMans.end(),
			tmp.m_Proto, tmp.m_VtblOffs, tmp.m_VtblIdx);

		if (hmil_iter == m_HookMans.end())
		{
			 // Moo ?
			return;
		}

		bool stillInUse = false;
		List<RemoveHookInfo> hookstoremove;

		// Iterate through all of the hook manager's hooks. Remove all hooks from the hookman's plugin.
#define TMP_CHECK_LIST(name, ispost) \
	for (hook_iter = iface_iter->name.m_List.begin(); hook_iter != iface_iter->name.m_List.end(); ++hook_iter) \
		if (hook_iter->plug == plug) \
			hookstoremove.push_back(RemoveHookInfo(hook_iter->plug, iface_iter->m_Ptr, \
			hook_iter->thisptr_offs, hmil_iter->m_Func, hook_iter->handler, ispost)); \
		else \
			stillInUse = true; \

		for (CHookManagerInfo::VfnPtrListIter vfnptr_iter = hmil_iter->m_VfnPtrs.begin();
			vfnptr_iter != hmil_iter->m_VfnPtrs.end(); ++vfnptr_iter)
		{
			for (CVfnPtr::IfaceListIter iface_iter = vfnptr_iter->m_Ifaces.begin();
				iface_iter != vfnptr_iter->m_Ifaces.end(); ++iface_iter)
			{
				List<HookInfo>::iterator hook_iter;
				TMP_CHECK_LIST(m_PreHooks, false);
				TMP_CHECK_LIST(m_PostHooks, true);
			}
		}
#undef TMP_CHECK_LIST

		for (List<RemoveHookInfo>::iterator rmiter = hookstoremove.begin(); rmiter != hookstoremove.end(); ++rmiter)
			RemoveHook(*rmiter);

		CHookManagerInfo info = *hmil_iter;

		// Unlink the hook manager from the list
		m_HookMans.erase(hmil_iter);

		// If there were any hooks from other plugins, find a new suitable hook manager.
		if (stillInUse)
		{
			// Find a suitable hook manager in an other plugin
			HookManInfoList::iterator newHookMan = FindHookMan(m_HookMans.begin(), m_HookMans.end(),
				info.m_Proto, info.m_VtblOffs, info.m_VtblIdx);

			// This should _never_ happen.
			// If there is a hook from an other plugin, the plugin must have provided a hook manager as well.
			SH_ASSERT(newHookMan != m_HookMans.end(),
				("Could not find a suitable hook manager in an other plugin!"));

			// AddHook should make sure that every plugin only has _one_ hook manager for _one_ proto/vi/vo
			SH_ASSERT(newHookMan->m_Plug != plug, ("New hook manager from same plugin!"));

			// The first hook manager should be always used - so the new hook manager has to be empty
			SH_ASSERT(newHookMan->m_VfnPtrs.empty(), ("New hook manager not empty!"));

			// Move the vfnptrs from the old hook manager to the new one
			newHookMan->m_VfnPtrs = info.m_VfnPtrs;

			// Unregister the old one, register the new one
			info.m_Func(HA_Unregister, NULL);
			newHookMan->m_Func(HA_Register, &(*newHookMan));

			// Go through all vfnptrs in this hookman and patch them to point to the new manager's handler!
			// or whatever
			for (CHookManagerInfo::VfnPtrListIter vfnptr_iter = newHookMan->m_VfnPtrs.begin();
				vfnptr_iter != newHookMan->m_VfnPtrs.end(); ++vfnptr_iter)
			{
				// And DEREFERENCE newHookMan->m_HookfuncVfnptr!
				// otherwise it will be executing the vtable... had to find out the hard way
				*reinterpret_cast<void**>(vfnptr_iter->m_Ptr) = *reinterpret_cast<void**>(newHookMan->m_HookfuncVfnptr);
			}
		}
	}

	void CSourceHookImpl::CompleteShutdown()
	{
		List<RemoveHookInfo> hookstoremove;
#define TMP_CHECK_LIST(name, ispost) \
	for (hook_iter = iface_iter->name.m_List.begin(); hook_iter != iface_iter->name.m_List.end(); ++hook_iter) \
		hookstoremove.push_back(RemoveHookInfo(hook_iter->plug, iface_iter->m_Ptr, \
			hook_iter->thisptr_offs, hmil_iter->m_Func, hook_iter->handler, ispost))
		for (HookManInfoList::iterator hmil_iter = m_HookMans.begin(); hmil_iter != m_HookMans.end(); ++hmil_iter)
		{
			for (CHookManagerInfo::VfnPtrListIter vfnptr_iter = hmil_iter->m_VfnPtrs.begin();
				vfnptr_iter != hmil_iter->m_VfnPtrs.end(); ++vfnptr_iter)
			{
				for (CVfnPtr::IfaceListIter iface_iter = vfnptr_iter->m_Ifaces.begin();
					iface_iter != vfnptr_iter->m_Ifaces.end(); ++iface_iter)
				{
					List<HookInfo>::iterator hook_iter;
					TMP_CHECK_LIST(m_PreHooks, false);
					TMP_CHECK_LIST(m_PostHooks, true);
				}
			}
		}
#undef TMP_CHECK_LIST

		for (List<RemoveHookInfo>::iterator rmiter = hookstoremove.begin(); rmiter != hookstoremove.end(); ++rmiter)
			RemoveHook(*rmiter);

		m_HookMans.clear();
	}

	bool CSourceHookImpl::AddHook(Plugin plug, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan, ISHDelegate *handler, bool post)
	{
		void *adjustediface = reinterpret_cast<void*>(reinterpret_cast<char*>(iface) + thisptr_offs);

		// 1) Get info about the hook manager
		CHookManagerInfo tmp;
		if (myHookMan(HA_GetInfo, &tmp) != 0)
			return false;

		void **cur_vtptr = *reinterpret_cast<void***>(
			reinterpret_cast<char*>(adjustediface) + tmp.m_VtblOffs);
		void *cur_vfnptr = reinterpret_cast<void*>(cur_vtptr + tmp.m_VtblIdx);

		// Add the proposed hook manager to the _end_ of the list if the plugin doesn't have a hook manager
		// with this proto/vo/vi registered
		HookManInfoList::iterator hkmi_iter;
		for (hkmi_iter = m_HookMans.begin(); hkmi_iter != m_HookMans.end(); ++hkmi_iter)
		{
			if (hkmi_iter->m_Plug == plug && ProtosEquiv(hkmi_iter->m_Proto, tmp.m_Proto) &&
				hkmi_iter->m_VtblOffs == tmp.m_VtblOffs && hkmi_iter->m_VtblIdx == tmp.m_VtblIdx)
				break;
		}
		if (hkmi_iter == m_HookMans.end())
		{
			// No such hook manager from this plugin yet, add it!
			tmp.m_Func = myHookMan;
			tmp.m_Plug = plug;
			m_HookMans.push_back(tmp);
		}

		// Then, search for a suitable hook manager (from the beginning)
		HookManInfoList::iterator hookman = FindHookMan(m_HookMans.begin(), m_HookMans.end(), tmp.m_Proto,
			tmp.m_VtblOffs, tmp.m_VtblIdx);
		SH_ASSERT(hookman != m_HookMans.end(), ("No hookman found - but if there was none, we've just added one!"));

		// Check whether there is already an other hook manager for the same vfnptr but with other values before
		// If yes, it means that the two are incompatible, so return false.
		for (hkmi_iter = m_HookMans.begin(); hkmi_iter != m_HookMans.end(); ++hkmi_iter)
		{
			if (hkmi_iter != hookman)
			{
				if (hkmi_iter->m_VfnPtrs.find(cur_vfnptr) != hkmi_iter->m_VfnPtrs.end())
					return false;
			}
		}

		// Tell it to store the pointer if it's not already active
		if (hookman->m_VfnPtrs.empty())
			hookman->m_Func(HA_Register, &(*hookman));

		CHookManagerInfo::VfnPtrListIter vfnptr_iter = hookman->m_VfnPtrs.find(cur_vfnptr);

		if (vfnptr_iter == hookman->m_VfnPtrs.end())
		{
			// Add a new one
			CVfnPtr vfp(cur_vfnptr);

			// Alter vtable entry
			if (!SetMemAccess(cur_vtptr, sizeof(void*) * (tmp.m_VtblIdx + 1), SH_MEM_READ | SH_MEM_WRITE))
				return false;

			*reinterpret_cast<void**>(cur_vfnptr) = *reinterpret_cast<void**>(hookman->m_HookfuncVfnptr);

			hookman->m_VfnPtrs.push_back(vfp);

			// Make vfnptr_iter point to the new element
			vfnptr_iter = hookman->m_VfnPtrs.end();
			--vfnptr_iter;

			// Now that it is done, check whether we have to update any callclasses
			ApplyCallClassPatches(adjustediface, tmp.m_VtblOffs, tmp.m_VtblIdx, vfp.m_OrigEntry);
		}

		CVfnPtr::IfaceListIter iface_iter = vfnptr_iter->m_Ifaces.find(adjustediface);
		if (iface_iter == vfnptr_iter->m_Ifaces.end())
		{
			// Add a new one
			vfnptr_iter->m_Ifaces.push_back(CIface(adjustediface));

			// Make iface_iter point to the new element
			iface_iter = vfnptr_iter->m_Ifaces.end();
			--iface_iter;
		}

		// Add the hook
		HookInfo hookinfo;
		hookinfo.handler = handler;
		hookinfo.plug = plug;
		hookinfo.paused = false;
		hookinfo.thisptr_offs = thisptr_offs;
		if (post)
			iface_iter->m_PostHooks.m_List.push_back(hookinfo);
		else
			iface_iter->m_PreHooks.m_List.push_back(hookinfo);

		return true;
	}

	bool CSourceHookImpl::RemoveHook(RemoveHookInfo info)
	{
		return RemoveHook(info.plug, info.iface, info.thisptr_offs, info.hookman, info.handler, info.post);
	}

	bool CSourceHookImpl::RemoveHook(Plugin plug, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan, ISHDelegate *handler, bool post)
	{
		void *adjustediface = reinterpret_cast<void*>(reinterpret_cast<char*>(iface)+thisptr_offs);
		CHookManagerInfo tmp;
		if (myHookMan(HA_GetInfo, &tmp) != 0)
			return false;

		// Find the hook manager and the hook
		HookManInfoList::iterator hookman = FindHookMan(m_HookMans.begin(), m_HookMans.end(),
			tmp.m_Proto, tmp.m_VtblOffs, tmp.m_VtblIdx);
		if (hookman == m_HookMans.end())
			return false;

		if (!ModuleInMemory(reinterpret_cast<char*>(adjustediface) + tmp.m_VtblOffs,
			sizeof(void*) * (tmp.m_VtblIdx + 1)))
		{
			// The module the vtable was in is already unloaded.
			hookman->m_VfnPtrs.clear();
			hookman->m_Func(HA_Unregister, NULL);
			return true;
		}

		void **cur_vtptr = *reinterpret_cast<void***>(
			reinterpret_cast<char*>(adjustediface) + tmp.m_VtblOffs);
		void *cur_vfnptr = reinterpret_cast<void*>(cur_vtptr + tmp.m_VtblIdx);

		CHookManagerInfo::VfnPtrListIter vfnptr_iter = hookman->m_VfnPtrs.find(cur_vfnptr);

		if (vfnptr_iter == hookman->m_VfnPtrs.end())
			return false;

		for (CVfnPtr::IfaceListIter iface_iter = vfnptr_iter->m_Ifaces.begin();
			iface_iter != vfnptr_iter->m_Ifaces.end();)
		{
			List<HookInfo> &hooks =
				post ? iface_iter->m_PostHooks.m_List : iface_iter->m_PreHooks.m_List;

			bool erase;
			for (List<HookInfo>::iterator hookiter = hooks.begin();
				hookiter != hooks.end(); )
			{
				erase = hookiter->plug == plug && hookiter->handler->IsEqual(handler) &&
					hookiter->thisptr_offs == thisptr_offs;
				if (erase)
				{
					hookiter->handler->DeleteThis();			// Make the _plugin_ delete the handler object

					// Move all iterators pointing at this
					List<HookInfo>::iterator oldhookiter = hookiter;
					hookiter = hooks.erase(hookiter);
					List<HookInfo>::iterator newhookiter = hookiter;
					--newhookiter; // The hook loop will ++ it then
					CHookList::CIter *pItIter;
					for (pItIter = iface_iter->m_PreHooks.m_UsedIters; pItIter; pItIter = pItIter->m_pNext)
						if (pItIter->m_Iter == oldhookiter)
							pItIter->m_Iter = newhookiter;
				}
				else
					++hookiter;
			}
			if (iface_iter->m_PostHooks.m_List.empty() && iface_iter->m_PreHooks.m_List.empty())
			{
				// There are no hooks on this iface anymore...
				for (HookLoopInfoStack::iterator hli_iter = m_HLIStack.begin();
					hli_iter != m_HLIStack.end(); ++hli_iter)
				{
					if (hli_iter->pCurIface == static_cast<IIface*>(&(*iface_iter)))
						hli_iter->shouldContinue = false;
				}

				iface_iter = vfnptr_iter->m_Ifaces.erase(iface_iter);
				if (vfnptr_iter->m_Ifaces.empty())
				{
					// No ifaces at all -> Deactivate the hook
					*reinterpret_cast<void**>(vfnptr_iter->m_Ptr) = vfnptr_iter->m_OrigEntry;

					hookman->m_VfnPtrs.erase(vfnptr_iter);

					// Remove callclass patch
					for (Impl_CallClassList::iterator cciter = m_CallClasses.begin(); cciter != m_CallClasses.end(); ++cciter)
						if (cciter->m_Ptr == adjustediface)
							cciter->RemoveCallClassPatch(tmp.m_VtblOffs, tmp.m_VtblIdx);

					if (hookman->m_VfnPtrs.empty())
					{
						// Unregister the hook manager
						hookman->m_Func(HA_Unregister, NULL);
					}

					// Don't try to continue looping through ifaces
					// - the list is already invalid
					return true;
				}
			}
			else
				++iface_iter;
		}
		return true;
	}

	GenericCallClass *CSourceHookImpl::GetCallClass(void *iface, size_t size)
	{
		for (Impl_CallClassList::iterator cciter = m_CallClasses.begin(); cciter != m_CallClasses.end(); ++cciter)
		{
			if (cciter->m_Ptr == iface && cciter->m_ObjSize == size)
			{
				++cciter->m_RefCounter;
				return &(*cciter);
			}
		}

		// Make a new one

		CCallClassImpl tmp(iface, size);
		ApplyCallClassPatches(tmp);
		m_CallClasses.push_back(tmp);
		return &m_CallClasses.back();
	}

	void CSourceHookImpl::ReleaseCallClass(GenericCallClass *ptr)
	{
		Impl_CallClassList::iterator iter = m_CallClasses.find(ptr);
		if (iter == m_CallClasses.end())
			return;
		--iter->m_RefCounter;
		if (iter->m_RefCounter < 1)
			m_CallClasses.erase(iter);
	}

	void CSourceHookImpl::ApplyCallClassPatches(CCallClassImpl &cc)
	{
		for (HookManInfoList::iterator hookman = m_HookMans.begin(); hookman != m_HookMans.end(); ++hookman)
		{
			for (CHookManagerInfo::VfnPtrListIter vfnptr_iter = hookman->m_VfnPtrs.begin();
				vfnptr_iter != hookman->m_VfnPtrs.end(); ++vfnptr_iter)
			{
				for (CVfnPtr::IfaceListIter iface_iter = vfnptr_iter->m_Ifaces.begin();
					iface_iter != vfnptr_iter->m_Ifaces.end(); ++iface_iter)
				{
					if (iface_iter->m_Ptr >= cc.m_Ptr &&
						iface_iter->m_Ptr < (reinterpret_cast<char*>(cc.m_Ptr) + cc.m_ObjSize))
					{
						cc.ApplyCallClassPatch(static_cast<int>(reinterpret_cast<char*>(iface_iter->m_Ptr) -
							reinterpret_cast<char*>(cc.m_Ptr)) + hookman->m_VtblOffs,
							hookman->m_VtblIdx, vfnptr_iter->m_OrigEntry);
					}
				}
			}
		}
	}

	void CSourceHookImpl::ApplyCallClassPatches(void *ifaceptr, int vtbl_offs, int vtbl_idx, void *orig_entry)
	{
		for (Impl_CallClassList::iterator cc_iter = m_CallClasses.begin(); cc_iter != m_CallClasses.end();
			++cc_iter)
		{
			if (ifaceptr >= cc_iter->m_Ptr &&
				ifaceptr < (reinterpret_cast<char*>(cc_iter->m_Ptr) + cc_iter->m_ObjSize))
			{
				cc_iter->ApplyCallClassPatch(static_cast<int>(reinterpret_cast<char*>(ifaceptr) -
					reinterpret_cast<char*>(cc_iter->m_Ptr)) + vtbl_offs, vtbl_idx, orig_entry);
			}
		}
	}

	void CSourceHookImpl::RemoveCallClassPatches(void *ifaceptr, int vtbl_offs, int vtbl_idx)
	{
		for (Impl_CallClassList::iterator cc_iter = m_CallClasses.begin(); cc_iter != m_CallClasses.end();
			++cc_iter)
		{
			if (ifaceptr >= cc_iter->m_Ptr &&
				ifaceptr < (reinterpret_cast<char*>(cc_iter->m_Ptr) + cc_iter->m_ObjSize))
			{
				cc_iter->RemoveCallClassPatch(vtbl_offs, vtbl_idx);
			}
		}
	}

	CSourceHookImpl::HookManInfoList::iterator CSourceHookImpl::FindHookMan(HookManInfoList::iterator begin,
		HookManInfoList::iterator end, const char *proto, int vtblofs, int vtblidx)
	{
		HookManInfoList::iterator hookmaniter;
		for (hookmaniter = m_HookMans.begin(); hookmaniter != m_HookMans.end(); ++hookmaniter)
		{
			if (ProtosEquiv(hookmaniter->m_Proto, proto) && hookmaniter->m_VtblOffs == vtblofs &&
				hookmaniter->m_VtblIdx == vtblidx)
				break;
		}
		return hookmaniter;
	}

	void CSourceHookImpl::SetPluginPaused(Plugin plug, bool paused)
	{
		// Go through all hook managers, all interfaces, and set the status of all hooks of this plugin to paused
		for (HookManInfoList::iterator hookmaniter = m_HookMans.begin(); hookmaniter != m_HookMans.end(); ++hookmaniter)
			for (CHookManagerInfo::VfnPtrListIter vfnptr_iter = hookmaniter->m_VfnPtrs.begin();
				vfnptr_iter != hookmaniter->m_VfnPtrs.end(); ++vfnptr_iter)
				for (CVfnPtr::IfaceListIter ifaceiter = vfnptr_iter->m_Ifaces.begin();
					ifaceiter != vfnptr_iter->m_Ifaces.end(); ++ifaceiter)
				{
					for (List<HookInfo>::iterator hookiter = ifaceiter->m_PreHooks.m_List.begin();
						hookiter != ifaceiter->m_PreHooks.m_List.end(); ++hookiter)
						if (plug == hookiter->plug)
							hookiter->paused = paused;

					for (List<HookInfo>::iterator hookiter = ifaceiter->m_PostHooks.m_List.begin();
						hookiter != ifaceiter->m_PostHooks.m_List.end(); ++hookiter)
						if (plug == hookiter->plug)
							hookiter->paused = paused;
				}
	}
	void CSourceHookImpl::PausePlugin(Plugin plug)
	{
		SetPluginPaused(plug, true);
	}

	void CSourceHookImpl::UnpausePlugin(Plugin plug)
	{
		SetPluginPaused(plug, false);
	}

	void CSourceHookImpl::HookLoopBegin(IIface *pIface)
	{
		HookLoopInfo hli;
		hli.pCurIface = pIface;
		hli.shouldContinue = true;
		hli.recall = false;
		m_HLIStack.push(hli);
	}

	void CSourceHookImpl::HookLoopEnd()
	{
		m_HLIStack.pop();
	}

	void CSourceHookImpl::SetRes(META_RES res)
	{
		*m_HLIStack.front().pCurRes = res;
	}

	META_RES CSourceHookImpl::GetPrevRes()
	{
		return *m_HLIStack.front().pPrevRes;
	}

	META_RES CSourceHookImpl::GetStatus()
	{
		return *m_HLIStack.front().pStatus;
	}

	const void *CSourceHookImpl::GetOrigRet()
	{
		return m_HLIStack.front().pOrigRet;
	}

	const void *CSourceHookImpl::GetOverrideRet()
	{
		// pOverrideRet is always set since recalls were introduced
		// GetOverrideRetPtr was added; a function which always returns pOverrideRet
		// so that RETURN_META_VALUE_NEWPARAMS can provide an override result

		// This means that we have to filter GetOverrideRet:
		//  If the status variable is < MRES_OVERRIDE, return NULL.

		return (*m_HLIStack.front().pStatus < MRES_OVERRIDE) ?
			NULL : m_HLIStack.front().pOverrideRet;
	}

	void *CSourceHookImpl::GetOverrideRetPtr()
	{
		// As described in the comment above: always return pOverrideRet
		return m_HLIStack.front().pOverrideRet;
	}

	void *CSourceHookImpl::GetIfacePtr()
	{
		return *m_HLIStack.front().pIfacePtrPtr;
	}

	void CSourceHookImpl::SetCurResPtr(META_RES *mres)
	{
		m_HLIStack.front().pCurRes = mres;
	}

	void CSourceHookImpl::SetPrevResPtr(META_RES *mres)
	{
		m_HLIStack.front().pPrevRes = mres;

		// If we're recalling, drag the previous mres value to the new hookfunc
		if (m_HLIStack.size() > 1 && m_HLIStack.second().recall)
			*mres = *m_HLIStack.second().pPrevRes;
	}

	void CSourceHookImpl::SetStatusPtr(META_RES *mres)
	{
		m_HLIStack.front().pStatus = mres;

		// If we're recalling, drag the previous mres value to the new hookfunc
		if (m_HLIStack.size() > 1 && m_HLIStack.second().recall)
			*mres = *m_HLIStack.second().pStatus;
	}

	void CSourceHookImpl::SetIfacePtrPtr(void **pp)
	{
		m_HLIStack.front().pIfacePtrPtr = pp;
	}

	void CSourceHookImpl::SetOrigRetPtr(const void *ptr)
	{
		m_HLIStack.front().pOrigRet = ptr;
	}
	void CSourceHookImpl::SetOverrideRetPtr(void *ptr)
	{
		m_HLIStack.front().pOverrideRet = ptr;
	}

	// New function which does all of the above + more :)
	void *CSourceHookImpl::SetupHookLoop(META_RES *statusPtr, META_RES *prevResPtr, META_RES *curResPtr,
		void **ifacePtrPtr, const void *origRetPtr, void *overrideRetPtr)
	{
		HookLoopInfo &hli = m_HLIStack.front();
		hli.pStatus = statusPtr;
		hli.pPrevRes = prevResPtr;
		hli.pCurRes = curResPtr;
		hli.pIfacePtrPtr = ifacePtrPtr;
		hli.pOrigRet = origRetPtr;

		// Handle some recall stuff
		if (m_HLIStack.size() > 1 && m_HLIStack.second().recall)
		{
			HookLoopInfo &other = m_HLIStack.second();
			*statusPtr = *other.pStatus;
			*prevResPtr = *other.pStatus;
			hli.pOverrideRet = other.pOverrideRet;
		}
		else
			hli.pOverrideRet = overrideRetPtr;

		// Tell the hook func which override ret ptr to use
		return hli.pOverrideRet;
	}

	bool CSourceHookImpl::ShouldContinue()
	{
		// If recall is true, we shall not continue either.
		// This is because, if it's true and ShouldContinue is called, it suggests that the
		// actual recall is done and that we are back in the original handler which shall return
		// immediately.

		return m_HLIStack.front().shouldContinue && !m_HLIStack.front().recall;
	}

	void CSourceHookImpl::DoRecall()
	{
		if (!m_HLIStack.empty())
		{
			m_HLIStack.front().recall = true;
			CHookList *mlist = static_cast<CHookList*>(m_HLIStack.front().pCurIface->GetPreHooks());
			mlist->m_Recall = true;

			// The hookfunc usually do this, but it won't have a chance to see it, 
			// so for recalls, we update status here if it's required
			if (*m_HLIStack.front().pCurRes > *m_HLIStack.front().pStatus) 
				*m_HLIStack.front().pStatus = *m_HLIStack.front().pCurRes;

		}
	}

	////////////////////////////
	// CCallClassImpl
	////////////////////////////
	CSourceHookImpl::CCallClassImpl::CCallClassImpl(void *ptr, size_t size)
		: m_Ptr(ptr), m_ObjSize(size), m_RefCounter(1)
	{
	}
	CSourceHookImpl::CCallClassImpl::~CCallClassImpl()
	{
	}

	void *CSourceHookImpl::CCallClassImpl::GetThisPtr()
	{
		return m_Ptr;
	}
	void *CSourceHookImpl::CCallClassImpl::GetOrigFunc(int vtbloffs, int vtblidx)
	{
		OrigVTables::iterator iter = m_VT.find(vtbloffs);
		if (iter != m_VT.end() && iter->val.size() > (size_t)vtblidx)
		{
			return iter->val[vtblidx];
		}
		return NULL;
	}

	void CSourceHookImpl::CCallClassImpl::ApplyCallClassPatch(int vtbl_offs, int vtbl_idx, void *orig_entry)
	{
		OrigFuncs &tmpvec = m_VT[vtbl_offs];
		if (tmpvec.size() <= (size_t)vtbl_idx)
			tmpvec.resize(vtbl_idx+1);
		tmpvec[vtbl_idx] = orig_entry;
	}
	void CSourceHookImpl::CCallClassImpl::RemoveCallClassPatch(int vtbl_offs, int vtbl_idx)
	{
		OrigVTables::iterator iter = m_VT.find(vtbl_offs);
		if (iter != m_VT.end())
		{
			if (iter->val.size() > (size_t)vtbl_idx)
			{
				iter->val[vtbl_idx] = 0;

				OrigFuncs &of = iter->val;
				OrigFuncs::iterator lastused = of.end();
				for (OrigFuncs::iterator viter = of.begin(); viter != of.end(); ++viter)
				{
					if (*viter)
						lastused = viter;
				}
				if (lastused == of.end())
				{
					// No used element => Remove the whole vector
					m_VT.erase(iter);
					return;
				}
				of.resize(lastused - of.begin() + 1);
			}
		}
	}

	bool CSourceHookImpl::ProtosEquiv(const char *p1, const char *p2)
	{
		/*
		 Old protos look like this:

		 0_void:
			"attrib"
		 1_void:
			"attrib|param1_type"
		 2_void:
			"attrib|param1_type|param2_type
		 0:
			"attrib|ret_type"
		 1:
			"attrib|ret_type|param1_type"
		 2:
			"attrib|ret_type|param2_type"

		New protos are in fact pointers to the ProtoInfo sturcture (see sourcehook.h for details)

		 Old protos _never_ begin with a null character
		 New protos _always_ begin with a null character
		*/

		if (*p1 && *p2)			// Case1: Both old
		{
			// As in old versions
			return strcmp(p1, p2) == 0;
		}
		else if (!*p1 && !*p2)	// Case2: Both new
		{
			const ProtoInfo *pi1 = reinterpret_cast<const ProtoInfo*>(p1);
			const ProtoInfo *pi2 = reinterpret_cast<const ProtoInfo*>(p2);

			if (pi1->retTypeSize == pi2->retTypeSize &&
				pi1->numOfParams == pi2->numOfParams)
			{
				// params[0] is 0 for "normal" functions and -1 for vararg functions
				// params[1] is size of first parameter
				// params[2] is size of second parameter ...
				for (int i = 0; i <= pi1->numOfParams; ++i)
				{
					if (pi1->params[i] != pi2->params[i])
						return false;
				}
				return true;
			}
			else
			{
				return false;
			}
		}
		else					// Case3: Mixed old/new
		{
			// Trust the user
			return true;
		}
	}

	////////////////////////////
	// CHookManagerInfo
	////////////////////////////
	CSourceHookImpl::CHookManagerInfo::~CHookManagerInfo()
	{
	}

	IVfnPtr *CSourceHookImpl::CHookManagerInfo::FindVfnPtr(void *vfnptr)
	{
		VfnPtrListIter iter = m_VfnPtrs.find(vfnptr);
		return iter == m_VfnPtrs.end() ? NULL : &(*iter);
	}
	void CSourceHookImpl::CHookManagerInfo::SetInfo(int vtbl_offs, int vtbl_idx, const char *proto)
	{
		m_VtblOffs = vtbl_offs;
		m_VtblIdx = vtbl_idx;
		m_Proto = proto;
	}
	void CSourceHookImpl::CHookManagerInfo::SetHookfuncVfnptr(void *hookfunc_vfnptr)
	{
		m_HookfuncVfnptr = hookfunc_vfnptr;
	}

	////////////////////////////
	// CVfnPtr
	////////////////////////////


	// If you get a crash here, the ptr passed is invalid
	// This usually means a SH_DECL_MANUALHOOK* with wrong thisptroffs/vtbloffs/vtblidx
	CSourceHookImpl::CVfnPtr::CVfnPtr(void *ptr) : m_Ptr(ptr), m_OrigEntry(*reinterpret_cast<void**>(ptr))
	{
	}
	CSourceHookImpl::CVfnPtr::~CVfnPtr()
	{
	}

	void *CSourceHookImpl::CVfnPtr::GetVfnPtr()
	{
		return m_Ptr;
	}

	void *CSourceHookImpl::CVfnPtr::GetOrigEntry()
	{
		return m_OrigEntry;
	}

	IIface *CSourceHookImpl::CVfnPtr::FindIface(void *ptr)
	{
		IfaceListIter iter = m_Ifaces.find(ptr);
		return iter == m_Ifaces.end() ? NULL : &(*iter);
	}

	////////////////////////////
	// CIface
	////////////////////////////
	CSourceHookImpl::CIface::CIface(void *ptr) : m_Ptr(ptr)
	{
	}
	CSourceHookImpl::CIface::~CIface()
	{
	}

	void *CSourceHookImpl::CIface::GetPtr()
	{
		return m_Ptr;
	}

	IHookList *CSourceHookImpl::CIface::GetPreHooks()
	{
		return &m_PreHooks;
	}
	IHookList *CSourceHookImpl::CIface::GetPostHooks()
	{
		return &m_PostHooks;
	}

	////////////////////////////
	// CHookList
	////////////////////////////

	CSourceHookImpl::CHookList::CHookList() : m_FreeIters(NULL), m_UsedIters(NULL),
		m_Recall(false)
	{
	}
	CSourceHookImpl::CHookList::CHookList(const CHookList &other) : m_List(other.m_List),
		m_FreeIters(NULL), m_UsedIters(NULL), m_Recall(false)
	{
	}

	CSourceHookImpl::CHookList::~CHookList()
	{
		while (m_FreeIters)
		{
			CIter *iter = m_FreeIters->m_pNext;
			delete m_FreeIters;
			m_FreeIters = iter;
		}
		while (m_UsedIters)
		{
			CIter *iter = m_UsedIters->m_pNext;
			delete m_UsedIters;
			m_UsedIters = iter;
		}
	}
	IHookList::IIter *CSourceHookImpl::CHookList::GetIter()
	{
		CIter *ret;
		if (m_FreeIters)
		{
			ret = m_FreeIters;
			m_FreeIters = ret->m_pNext;
			ret->GoToBegin();
		}
		else
		{
			ret = new CIter(this);
		}
		
		// Muuuh, if we're recalling, it shall be a copy of the last iterator, incremented by one
		if (m_Recall && m_UsedIters)
		{
			ret->Set(m_UsedIters);		// m_UsedIters is the last returned and not released iterator
			ret->Next();				// Use next instead of directly incrementing its m_Iter:
										// skips paused plugins
		}

		ret->m_pNext = m_UsedIters;
		ret->m_pPrev = NULL;
		if (m_UsedIters)
			m_UsedIters->m_pPrev = ret;
		m_UsedIters = ret;

		m_Recall = false;

		return ret;
	}
	void CSourceHookImpl::CHookList::ReleaseIter(IIter *pIter)
	{
		CIter *pIter2 = static_cast<CIter*>(pIter);

		// Unlink from m_UsedIters

		if (pIter2->m_pNext)
			pIter2->m_pNext->m_pPrev = pIter2->m_pPrev;
		if (pIter2->m_pPrev)
			pIter2->m_pPrev->m_pNext = pIter2->m_pNext;
		if (pIter2 == m_UsedIters)
			m_UsedIters = NULL;

		// Link to m_FreeIters

		pIter2->m_pNext = m_FreeIters;

		m_FreeIters = pIter2;

		// Reset recall state.
		m_Recall = false;
	}

	CSourceHookImpl::CHookList::CIter::CIter(CHookList *pList) : m_pList(pList), m_pNext(NULL)
	{
		GoToBegin();
	}
	CSourceHookImpl::CHookList::CIter::~CIter()
	{
	}

	void CSourceHookImpl::CHookList::CIter::Set(CIter *pOther)
	{
		m_Iter = pOther->m_Iter;
	}

	void CSourceHookImpl::CHookList::CIter::GoToBegin()
	{
		m_Iter = m_pList->m_List.begin();
		SkipPaused();
	}

	bool CSourceHookImpl::CHookList::CIter::End()
	{
		if (!m_pList)
			return false;
		return m_Iter == m_pList->m_List.end();
	}
	void CSourceHookImpl::CHookList::CIter::Next()
	{
		if (!m_pList)
			return;
		++m_Iter;
		SkipPaused();
	}
	void CSourceHookImpl::CHookList::CIter::Clear()
	{
		m_pList = NULL;
	}
	void CSourceHookImpl::CHookList::CIter::SkipPaused()
	{
		while (m_Iter != m_pList->m_List.end() && m_Iter->paused)
			++m_Iter;
	}

	ISHDelegate *CSourceHookImpl::CHookList::CIter::Handler()
	{
		return m_Iter->handler;
	}
	int CSourceHookImpl::CHookList::CIter::ThisPtrOffs()
	{
		return m_Iter->thisptr_offs;
	}
}
