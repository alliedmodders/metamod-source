/* ======== SourceHook ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

/**
 * @file sourcehook.cpp
 * @brief Contains the implementation of the SourceHook API
*/

#include <algorithm>
#include "sourcehook_impl.h"

#if SH_SYS == SH_SYS_WIN32
# include <windows.h>
#elif SH_SYS == SH_SYS_LINUX
# include <sys/mman.h>
# include <limits.h>
# include <unistd.h>
# ifndef PAGESIZE
#  define PAGESIZE 4096
# endif
#endif

#if SH_RUNTIME_CODEGEN == 1
# if SH_COMP == SH_COMP_GCC
#  define SH_CCC_CODESIZE 15
	//	 :TODO:
	//void SH_CCC_MakeGate(void *origthisptr, unsigned char* &ptr, void *origvtable, int i)
	//{
	//}
# elif SH_COMP == SH_COMP_MSVC
#  define SH_CCC_CODESIZE 19
	void SH_CCC_MakeGate(void *origthisptr, void *ccthisptr, unsigned char* ptr, void *origfunc)
	{
		// --Subtract old this pointer to get the offset
		// 00417EDF 81 E9 A1 7B 33 01 sub         ecx,1337BA1h 
		*ptr++ = 0x81;
		*ptr++ = 0xE9;
		*reinterpret_cast<void**>(ptr) = ccthisptr;
		ptr += sizeof(void*);
		// --Add it to the new this pointer
		// 00417EE5 81 C1 37 13 37 13 add         ecx,13371337h 
		*ptr++ = 0x81;
		*ptr++ = 0xC1;
		*reinterpret_cast<void**>(ptr) = origthisptr;
		ptr += sizeof(void*);
		// --Prepare address
		// 00417EEB B8 EF BE AD DE   mov         eax,0DEADBEEFh 
		*ptr++ = 0xB8;
		*reinterpret_cast<void**>(ptr) = origfunc;
		ptr += sizeof(void*);
		// -- Do actual jump
		// 00417EF0 FF E0            jmp         eax  
		*ptr++ = 0xFF;
		*ptr++ = 0xE0;
	}
# endif
#endif


namespace SourceHook
{
	CSourceHookImpl::CSourceHookImpl()
	{
		// Get page size
#if SH_SYS == SH_SYS_WIN32
		SYSTEM_INFO sysinfo;
		GetSystemInfo(&sysinfo);
		m_PageSize = sysinfo.dwPageSize;
#elif SH_SYS == SH_SYS_LINUX
		m_PageSize = PAGESIZE;
#else
# error Unsupported system
#endif
	}

	CSourceHookImpl::~CSourceHookImpl()
	{

	}

	bool CSourceHookImpl::IsPluginInUse(Plugin plug)
	{
		// Iterate through all hook managers which are in this plugin
		// Iterate through their hooks
		// If a hook from an other plugin is found, return true
		// Return false otherwise
#define TMP_CHECK_LIST(name) \
	for (iter3 = iter2->name.begin(); iter3 != iter2->name.end(); ++iter3) \
		if (iter3->plug == plug) \
			return true;

		for (HookManInfoList::iterator iter = m_HookMans.begin(); iter != m_HookMans.end(); ++iter)
		{
			if (iter->plug == plug && !iter->ifaces.empty())
			{
				for (std::list<HookManagerInfo::Iface>::iterator iter2 = iter->ifaces.begin(); iter2 != iter->ifaces.end(); ++iter2)
				{
					std::list<HookManagerInfo::Iface::Hook>::iterator iter3;
					TMP_CHECK_LIST(hooks_pre);
					TMP_CHECK_LIST(hooks_post);
				}
			}
		}
#undef TMP_CHECK_LIST
		return false;
	}

	void CSourceHookImpl::UnloadPlugin(Plugin plug)
	{
		// 1) Manually remove all hooks by this plugin
		std::list<RemoveHookInfo> hookstoremove;

		for (HookManInfoList::iterator iter = m_HookMans.begin(); iter != m_HookMans.end(); ++iter)
		{
			for (std::list<HookManagerInfo::Iface>::iterator iter2 = iter->ifaces.begin(); iter2 != iter->ifaces.end();
				++iter2)
			{
				for (std::list<HookManagerInfo::Iface::Hook>::iterator iter3 = iter2->hooks_pre.begin(); iter3 != iter2->hooks_pre.end(); ++iter3)
					if (iter3->plug == plug)
						hookstoremove.push_back(RemoveHookInfo(iter3->plug, iter2->ptr, iter->func, iter3->handler, false));

				for (std::list<HookManagerInfo::Iface::Hook>::iterator iter3 = iter2->hooks_post.begin(); iter3 != iter2->hooks_post.end(); ++iter3)
					if (iter3->plug == plug)
						hookstoremove.push_back(RemoveHookInfo(iter3->plug, iter2->ptr, iter->func, iter3->handler, true));
			}
		}

		for (std::list<RemoveHookInfo>::iterator rmiter = hookstoremove.begin(); rmiter != hookstoremove.end(); ++rmiter)
			RemoveHook(rmiter->plug, rmiter->iface, rmiter->hpf, rmiter->handler, rmiter->post);

		// 2) Other plugins may use hook managers in this plugin.
		// Get a list of hook managers that are in this plugin and are used by other plugins
		// Delete all hook managers that are in this plugin

		HookManInfoList tmphookmans;
		bool erase = false;
		for (HookManInfoList::iterator iter = m_HookMans.begin(); iter != m_HookMans.end();
			erase ? iter=m_HookMans.erase(iter) : ++iter)
		{
			if (iter->plug == plug)
			{
				if (!iter->ifaces.empty())
				{
					// All hooks by this plugin are already removed
					// So if there is an iface, it has to be used by an other plugin
					tmphookmans.push_back(*iter);
				}
				erase = true;
			}
			else
				erase = false;
		}
		
		// For each hook manager:
		for (HookManInfoList::iterator iter = tmphookmans.begin(); iter != tmphookmans.end(); ++iter)
		{
			// Find a suitable hook manager in an other plugin
			HookManInfoList::iterator newHookMan = FindHookMan(m_HookMans.begin(), m_HookMans.end(),
				iter->proto, iter->vtbl_offs, iter->vtbl_idx, iter->thisptr_offs);
			if (newHookMan == m_HookMans.end())
			{
				// This should _never_ happen.
				// If there is a hook from an other plugin, the plugin must have provided a hook manager as well.
				SH_ASSERT(0);
			}

			// AddHook should make sure that every plugin only has _one_ hook manager for _one_ proto/vi/vo/thisptroffs
			SH_ASSERT(newHookMan->plug != plug);

			// The first hooker should be always used - so the new hook manager has to be empty
			SH_ASSERT(newHookMan->ifaces.empty());

			// Move the ifaces from the old hook manager to the new one
			newHookMan->ifaces = iter->ifaces;

			// Unregister the old one, register the new one
			iter->func(HA_Unregister, NULL);
			newHookMan->func(HA_Register, &(*newHookMan));
		}
	}

	void CSourceHookImpl::CompleteShutdown()
	{
		std::list<RemoveHookInfo> hookstoremove;

		for (HookManInfoList::iterator iter = m_HookMans.begin(); iter != m_HookMans.end(); ++iter)
		{
			for (std::list<HookManagerInfo::Iface>::iterator iter2 = iter->ifaces.begin(); iter2 != iter->ifaces.end();
				++iter2)
			{
				for (std::list<HookManagerInfo::Iface::Hook>::iterator iter3 = iter2->hooks_pre.begin(); iter3 != iter2->hooks_pre.end(); ++iter3)
					hookstoremove.push_back(RemoveHookInfo(iter3->plug, iter2->ptr, iter->func, iter3->handler, false));
				
				for (std::list<HookManagerInfo::Iface::Hook>::iterator iter3 = iter2->hooks_post.begin(); iter3 != iter2->hooks_post.end(); ++iter3)
					hookstoremove.push_back(RemoveHookInfo(iter3->plug, iter2->ptr, iter->func, iter3->handler, true));
			}
		}

		for (std::list<RemoveHookInfo>::iterator rmiter = hookstoremove.begin(); rmiter != hookstoremove.end(); ++rmiter)
			RemoveHook(rmiter->plug, rmiter->iface, rmiter->hpf, rmiter->handler, rmiter->post);

		m_HookMans.clear();
	}

	bool CSourceHookImpl::AddHook(Plugin plug, void *iface, int ifacesize, HookManagerPubFunc myHookMan, ISHDelegate *handler, bool post)
	{
		// 1) Get info about the hook manager
		HookManagerInfo tmp;
		if (myHookMan(HA_GetInfo, &tmp) != 0)
			return false;

		// Add the proposed hook manager to the _end_ of the list if the plugin doesn't have a hook manager with this proto/vo/vi registered
		HookManInfoList::iterator hookmaniter;
		for (hookmaniter = m_HookMans.begin(); hookmaniter != m_HookMans.end(); ++hookmaniter)
		{
			if (hookmaniter->plug == plug && strcmp(hookmaniter->proto, tmp.proto) == 0 &&
				hookmaniter->vtbl_offs == tmp.vtbl_offs && hookmaniter->vtbl_idx == tmp.vtbl_idx &&
				hookmaniter->thisptr_offs == tmp.thisptr_offs)
				break;
		}
		if (hookmaniter == m_HookMans.end())
		{
			// No such hook manager from this plugin yet, add it!
			tmp.func = myHookMan;
			tmp.plug = plug;
			m_HookMans.push_back(tmp);
		}

		// Then, search for a suitable hook manager (from the beginning)
		HookManInfoList::iterator hookman = FindHookMan(m_HookMans.begin(), m_HookMans.end(), tmp.proto, tmp.vtbl_offs, tmp.vtbl_idx, tmp.thisptr_offs);
		SH_ASSERT(hookman != m_HookMans.end());

		// Tell it to store the pointer if it's not already active
		if (hookman->ifaces.empty())
			hookman->func(HA_Register, &(*hookman));

		std::list<HookManagerInfo::Iface>::iterator ifsiter = std::find(hookman->ifaces.begin(), hookman->ifaces.end(), iface);

		if (ifsiter == hookman->ifaces.end())
		{
			HookManagerInfo::Iface ifs;
			ifs.ptr = iface;
			ifs.callclass = GetCallClass(iface, ifacesize);
			void *vtableptr = *reinterpret_cast<void**>(reinterpret_cast<char*>(iface) + hookman->vtbl_offs);
			SetMemAccess(vtableptr, sizeof(void*) * hookman->vtbl_idx, SH_MEM_READ | SH_MEM_WRITE);
			ifs.orig_entry = (*reinterpret_cast<void***>(reinterpret_cast<char*>(iface) + hookman->vtbl_offs))[hookman->vtbl_idx];
			(*reinterpret_cast<void***>(reinterpret_cast<char*>(iface) + hookman->vtbl_offs))[hookman->vtbl_idx] =
				(*reinterpret_cast<void***>(reinterpret_cast<char*>(hookman->hookfunc_inst) + hookman->hookfunc_vtbl_offs))[hookman->hookfunc_vtbl_idx];
			hookman->ifaces.push_back(ifs);
			ifsiter = hookman->ifaces.end();
			--ifsiter;
		}
		HookManagerInfo::Iface::Hook hookinfo;
		hookinfo.handler = handler;
		hookinfo.plug = plug;
		hookinfo.paused = false;
		if (post)
   			ifsiter->hooks_post.push_back(hookinfo);
		else
			ifsiter->hooks_pre.push_back(hookinfo);


		// Now that it is done, check whether we have to update any callclasses
		for (Impl_CallClassList::iterator cciter = m_CallClasses.begin(); cciter != m_CallClasses.end(); ++cciter)
		{
			if (cciter->iface == iface)
			{
				ApplyCallClassPatch(*cciter, tmp.vtbl_offs, tmp.vtbl_idx, ifsiter->orig_entry);

				// We can assume that there is no more callclass with the same iface
				break;
			}
		}

		return true;
	}

	bool CSourceHookImpl::RemoveHook(Plugin plug, void *iface, HookManagerPubFunc myHookMan, ISHDelegate *handler, bool post)
	{
		HookManagerInfo tmp;
		if (myHookMan(HA_GetInfo, &tmp) != 0)
			return false;

		// Find the hook manager and the hook
		HookManInfoList::iterator hookman = FindHookMan(m_HookMans.begin(), m_HookMans.end(), 
			tmp.proto, tmp.vtbl_offs, tmp.vtbl_idx, tmp.thisptr_offs);
		if (hookman == m_HookMans.end())
			return false;

		for (std::list<HookManagerInfo::Iface>::iterator ifaceiter = hookman->ifaces.begin(); ifaceiter != hookman->ifaces.end(); ++ifaceiter)
		{
			if (ifaceiter->ptr == iface)
			{
				std::list<HookManagerInfo::Iface::Hook> &hooks = post ? ifaceiter->hooks_post : ifaceiter->hooks_pre;
				bool erase;
				for (std::list<HookManagerInfo::Iface::Hook>::iterator hookiter = hooks.begin();
					hookiter != hooks.end(); erase ? hookiter = hooks.erase(hookiter) : ++hookiter)
				{
					erase = hookiter->plug == plug && hookiter->handler->IsEqual(handler);
					if (erase)
						hookiter->handler->DeleteThis();			// Make the _plugin_ delete the handler object
				}
				if (ifaceiter->hooks_post.empty() && ifaceiter->hooks_pre.empty())
				{
					// Deactivate the hook
					(*reinterpret_cast<void***>(reinterpret_cast<char*>(ifaceiter->ptr) +
						hookman->vtbl_offs))[hookman->vtbl_idx]= ifaceiter->orig_entry;

					// Release the callclass
					ReleaseCallClass(ifaceiter->callclass);

					// Remove the iface info
					hookman->ifaces.erase(ifaceiter);

					if (hookman->ifaces.empty())
						hookman->func(HA_Unregister, NULL);
				}
				// :TODO: Better return value? Or none?
				return true;
			}
		}
		return false;
	}

	void *CSourceHookImpl::GetCallClass(void *iface, size_t size)
	{
		for (Impl_CallClassList::iterator cciter = m_CallClasses.begin(); cciter != m_CallClasses.end(); ++cciter)
		{
			if (cciter->iface == iface)
			{
				++cciter->refcounter;
				return cciter->ptr;
			}
		}

		// The layout of callclasses is:
		//  Copy of the class; vtable entries pointing to gates
		//  Pointer to the corresponding CallClass object
		//  Copy of the class; vtable entries pointing to original functions

		CallClass tmp;
		char *ptr = new char[size * 2 + sizeof(void*)];
		tmp.ptr = reinterpret_cast<void*>(ptr);
		memcpy(reinterpret_cast<void*>(ptr), iface, size);
		memcpy(reinterpret_cast<void*>(ptr + size + sizeof(void*)), iface, size);
		tmp.iface = iface;
		tmp.size = size;
		tmp.refcounter = 1;

		// Go through _all_ hooks and apply any needed patches
		for (HookManInfoList::iterator hookman = m_HookMans.begin(); hookman != m_HookMans.end(); ++hookman)
		{
			for (std::list<HookManagerInfo::Iface>::iterator ifaceiter = hookman->ifaces.begin(); ifaceiter != hookman->ifaces.end(); ++ifaceiter)
			{
				if (ifaceiter->ptr == iface)
				{
					if (!ApplyCallClassPatch(tmp, hookman->vtbl_offs, hookman->vtbl_idx, ifaceiter->orig_entry))
					{
						FreeCallClass(tmp);
						return NULL;
					}
				}
			}
		}
		m_CallClasses.push_back(tmp);
		// The object has to be followed by the pointer to the vtable info object
		*reinterpret_cast<void**>(ptr + size) = &m_CallClasses.back();
		return tmp.ptr;
	}

	void CSourceHookImpl::ReleaseCallClass(void *ptr)
	{
		Impl_CallClassList::iterator iter = std::find(m_CallClasses.begin(), m_CallClasses.end(), ptr);
		if (iter == m_CallClasses.end())
			return;
		--iter->refcounter;
		if (iter->refcounter < 1)
		{
			FreeCallClass(*iter);
			m_CallClasses.erase(iter);
		}
	}

	void CSourceHookImpl::FreeCallClass(CallClass &cc)
	{
		for (CallClass::VTableList::iterator vtbliter = cc.vtables.begin(); vtbliter != cc.vtables.end(); ++vtbliter)
		{
#if SH_RUNTIME_CODEGEN == 1
			// Delete generated callgates
			for (std::list<int>::iterator cgiter = vtbliter->patches.begin(); cgiter != vtbliter->patches.end(); ++cgiter)
			{
				char *cgptr = reinterpret_cast<char**>(vtbliter->ptr)[*cgiter];
				delete [] cgptr;
			}
#endif
			delete [] reinterpret_cast<char*>(vtbliter->ptr);
		}
		delete [] reinterpret_cast<char*>(cc.ptr);
	}

	bool CSourceHookImpl::ApplyCallClassPatch(CallClass &cc, int vtbl_offs, int vtbl_idx, void *orig_entry)
	{
		char *ptr = reinterpret_cast<char*>(cc.ptr);
		void *vtable = *reinterpret_cast<void**>(ptr + vtbl_offs);

		// Check whether we already know that vtable
		CallClass::VTableList::iterator vtbliter = std::find(cc.vtables.begin(),
			cc.vtables.end(), reinterpret_cast<void*>(vtable));
		int actvtablesize=MAX_VTABLE_LEN;
		if (vtbliter == cc.vtables.end())
		{
			CallClass::VTable newvtableinfo;
			int pagesnum = (MAX_VTABLE_LEN % m_PageSize == 0) ? MAX_VTABLE_LEN / m_PageSize : MAX_VTABLE_LEN / m_PageSize + 1;
			// Set all pages in the range to readwrite
			// :TODO: Fix this!
			char *pagebegin = reinterpret_cast<char*>(vtable) - (reinterpret_cast<int>(vtable) % m_PageSize);
			for (int ipage = 0; ipage < pagesnum; ++ipage)
			{
				if (!SetMemAccess(reinterpret_cast<void*>(pagebegin + ipage * m_PageSize), 1,
					SH_MEM_READ | SH_MEM_WRITE))
				{
					// We can't go here anymore
					actvtablesize = static_cast<int>((pagebegin + ipage * m_PageSize) - reinterpret_cast<char*>(vtable));
					break;
				}
			}
			if (actvtablesize < 1)
			{
				// We can't access the vtable -> Quit
				return false;
			}
			// Create a new vtable
			newvtableinfo.ptr = reinterpret_cast<void*>(new char[actvtablesize]);
			// Fill it with the information from the old vtable
			memcpy(newvtableinfo.ptr, vtable, actvtablesize);
			newvtableinfo.actsize = actvtablesize;
			// Set the pointer in the object and add it to the list of already known vtables
			*reinterpret_cast<void**>(ptr + vtbl_offs) = newvtableinfo.ptr;
			cc.vtables.push_back(newvtableinfo);

			vtbliter = cc.vtables.end();
			--vtbliter;
			// :TODO: When not codegen, patch the other vtable?
		}

		// Check whether we already have this patch
		if (std::find(vtbliter->patches.begin(), vtbliter->patches.end(), vtbl_idx) == vtbliter->patches.end())
		{
			// No -> apply it

			// Get a call gate
			void *callgate = NULL;
#if SH_RUNTIME_CODEGEN == 1
			unsigned char *cggen = new unsigned char[SH_CCC_CODESIZE];
			//SH_CCC_MakeGate(cc.iface, cc.ptr, cggen, orig_entry);
			callgate = (void*)cggen;
			SetMemAccess(callgate, SH_CCC_CODESIZE, SH_MEM_READ | SH_MEM_WRITE | SH_MEM_EXEC);
#else
			// :TODO:
#error Not supported yet!
#endif
			vtbliter->patches.push_back(vtbl_idx);
			reinterpret_cast<void**>(vtbliter->ptr)[vtbl_idx] = callgate;
		}
		return true;
	}


	CSourceHookImpl::HookManInfoList::iterator CSourceHookImpl::FindHookMan(HookManInfoList::iterator begin,
		HookManInfoList::iterator end, const char *proto, int vtblofs, int vtblidx, int thisptrofs)
	{
		for (HookManInfoList::iterator hookmaniter = m_HookMans.begin(); hookmaniter != m_HookMans.end(); ++hookmaniter)
		{
			if (strcmp(hookmaniter->proto, proto) == 0 && hookmaniter->vtbl_offs == vtblofs && hookmaniter->vtbl_idx == vtblidx &&
				hookmaniter->thisptr_offs == thisptrofs)
				break;
		}
		return hookmaniter;
	}

	void CSourceHookImpl::PausePlugin(Plugin plug)
	{
		// Go through all hook managers, all interfaces, and set the status of all hooks of this plugin to paused
		for (HookManInfoList::iterator hookmaniter = m_HookMans.begin(); hookmaniter != m_HookMans.end(); ++hookmaniter)
			for (std::list<HookManagerInfo::Iface>::iterator ifaceiter = hookmaniter->ifaces.begin();
				ifaceiter != hookmaniter->ifaces.end(); ++ifaceiter)
			{
				for (std::list<HookManagerInfo::Iface::Hook>::iterator hookiter = ifaceiter->hooks_pre.begin();
					hookiter != ifaceiter->hooks_pre.end(); ++hookiter)
					if (plug == hookiter->plug)
						hookiter->paused = true;

				for (std::list<HookManagerInfo::Iface::Hook>::iterator hookiter = ifaceiter->hooks_post.begin();
					hookiter != ifaceiter->hooks_post.end(); ++hookiter)
					if (plug == hookiter->plug)
						hookiter->paused = true;
			}
	}

	void CSourceHookImpl::UnpausePlugin(Plugin plug)
	{
		// Go through all hook managers, all interfaces, and set the status of all hooks of this plugin to normal
		for (HookManInfoList::iterator hookmaniter = m_HookMans.begin(); hookmaniter != m_HookMans.end(); ++hookmaniter)
			for (std::list<HookManagerInfo::Iface>::iterator ifaceiter = hookmaniter->ifaces.begin();
				ifaceiter != hookmaniter->ifaces.end(); ++ifaceiter)
			{
				for (std::list<HookManagerInfo::Iface::Hook>::iterator hookiter = ifaceiter->hooks_pre.begin();
					hookiter != ifaceiter->hooks_pre.end(); ++hookiter)
					if (plug == hookiter->plug)
						hookiter->paused = false;

				for (std::list<HookManagerInfo::Iface::Hook>::iterator hookiter = ifaceiter->hooks_post.begin();
					hookiter != ifaceiter->hooks_post.end(); ++hookiter)
					if (plug == hookiter->plug)
						hookiter->paused = false;
			}
	}

	void CSourceHookImpl::SetRes(META_RES res)
	{
		m_CurRes = res;
	}

	META_RES CSourceHookImpl::GetPrevRes()
	{
		return m_PrevRes;
	}

	META_RES CSourceHookImpl::GetStatus()
	{
		return m_Status;
	}

	const void *CSourceHookImpl::GetOrigRet()
	{
		return m_OrigRet;
	}

	const void *CSourceHookImpl::GetOverrideRet()
	{
		return m_OverrideRet;
	}

	META_RES &CSourceHookImpl::GetCurResRef()
	{
		return m_CurRes;
	}

	META_RES &CSourceHookImpl::GetPrevResRef()
	{
		return m_PrevRes;
	}

	META_RES &CSourceHookImpl::GetStatusRef()
	{
		return m_Status;
	}

	void CSourceHookImpl::SetOrigRet(const void *ptr)
	{
		m_OrigRet = ptr;
	}

	void CSourceHookImpl::SetOverrideRet(const void *ptr)
	{
		m_OverrideRet = ptr;
	}

	void CSourceHookImpl::SetIfacePtr(void *ptr)
	{
		m_IfacePtr = ptr;
	}
	void *CSourceHookImpl::GetIfacePtr()
	{
		return m_IfacePtr;
	}
}
