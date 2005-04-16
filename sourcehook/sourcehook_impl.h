/* ======== SourceHook ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

#ifndef __SOURCEHOOK_IMPL_H__
#define __SOURCEHOOK_IMPL_H__

#include "sourcehook.h"

// Set this to 1 to enable runtime code generation (faster)
#define SH_RUNTIME_CODEGEN 1

namespace SourceHook
{
	/**
	*	@brief The SourceHook implementation class
	*/
	class CSourceHookImpl : public ISourceHook
	{
		/**
		*	@brief A hook can be removed if you have this information
		*/
		struct RemoveHookInfo
		{
			RemoveHookInfo(Plugin p, void *i, HookManagerPubFunc h, ISHDelegate *hd, bool ps)
				: plug(p), iface(i), hpf(h), handler(hd), post(ps)
			{
			}

			Plugin plug;
			void *iface;
			HookManagerPubFunc hpf;
			ISHDelegate *handler;
			bool post;
		};

		/**
		*	@brief A list of HookManagerInfo structures
		*/
		typedef std::list<HookManagerInfo> HookManInfoList;


		/**
		*	@brief A list of Impl_CallClass structures
		*/
		typedef std::list<CallClass> Impl_CallClassList;

		Impl_CallClassList m_CallClasses;			//!< A list of already generated callclasses
		HookManInfoList m_HookMans;					//!< A list of hook managers

		int m_PageSize;								//!< Stores the system's page size

		/**
		*	@brief Finds a hook manager for a function based on a text-prototype, a vtable offset and a vtable index
		*/
		HookManInfoList::iterator FindHookMan(HookManInfoList::iterator begin, HookManInfoList::iterator end, 
			const char *proto, int vtblofs, int vtblidx, int thisptrofs);

		void FreeCallClass(CallClass &cc);
		bool ApplyCallClassPatch(CallClass &cc, int vtbl_offs, int vtbl_idx, void *orig_entry);

		static const int MAX_VTABLE_LEN = 4096;		//!< Maximal vtable length in bytes

		META_RES m_Status, m_PrevRes, m_CurRes;
		const void *m_OrigRet;
		const void *m_OverrideRet;
		void *m_IfacePtr;
	public:
		CSourceHookImpl();
		~CSourceHookImpl();

		/**
		*	@brief Make sure that a plugin is not used by any other plugins anymore, and unregister all its hook managers
		*/
		void UnloadPlugin(Plugin plug);

		/**
		*	@brief Shut down the whole system, unregister all hook managers
		*/
		void CompleteShutdown();

		/**
		*	@brief Add a hook.
		*
		*	@return True if the function succeeded, false otherwise
		*
		*	@param plug The unique identifier of the plugin that calls this function
		*	@param iface The interface pointer
		*	@param ifacesize The size of the class iface points to
		*	@param myHookMan A hook manager function that should be capable of handling the function
		*	@param handler A pointer to a FastDelegate containing the hook handler
		*	@param post Set to true if you want a post handler
		*/
		bool AddHook(Plugin plug, void *iface, int ifacesize, HookManagerPubFunc myHookMan, ISHDelegate *handler, bool post);

		/**
		*	@brief Removes a hook.
		*
		*	@return True if the function succeeded, false otherwise
		*
		*	@param plug The unique identifier of the plugin that calls this function
		*	@param iface The interface pointer
		*	@param myHookMan A hook manager function that should be capable of handling the function
		*	@param handler A pointer to a FastDelegate containing the hook handler
		*	@param post Set to true if you want a post handler
		*/
		bool RemoveHook(Plugin plug, void *iface, HookManagerPubFunc myHookMan, ISHDelegate *handler, bool post);

		/**
		*	@brief Checks whether a plugin has (a) hook manager(s) that is/are currently used by other plugins
		*
		*	@param plug The unique identifier of the plugin in question
		*/
		bool IsPluginInUse(Plugin plug);

		/**
		*	@brief Pauses all hooks of a plugin
		*
		*	@param plug The unique identifier of the plugin
		*/
		void PausePlugin(Plugin plug);

		/**
		*	@brief Unpauses all hooks of a plugin
		*
		*	@param plug The unique identifier of the plugin
		*/
		void UnpausePlugin(Plugin plug);

		/**
		*	@brief Return a pointer to a callclass. Generate a new one if required.
		*
		*	@param iface The interface pointer
		*/
		void *GetCallClass(void *iface, size_t size);

		/**
		*	@brief Release a callclass
		*
		*	@param ptr Pointer to the callclass
		*/
		virtual void ReleaseCallClass(void *ptr);

		virtual void SetRes(META_RES res);				//!< Sets the meta result
		virtual META_RES GetPrevRes();					//!< Gets the meta result of the previously called handler
		virtual META_RES GetStatus();					//!< Gets the highest meta result
		virtual const void *GetOrigRet();				//!< Gets the original result. If not in post function, undefined
		virtual const void *GetOverrideRet();			//!< Gets the override result. If none is specified, NULL
		virtual void *GetIfacePtr();					//!< Gets the interface pointer

		//////////////////////////////////////////////////////////////////////////
		// For hook managers
		virtual META_RES &GetCurResRef();				//!< Gets the pointer to the current meta result
		virtual META_RES &GetPrevResRef();				//!< Gets the pointer to the previous meta result
		virtual META_RES &GetStatusRef();				//!< Gets the pointer to the status variable
		virtual void SetOrigRet(const void *ptr);		//!< Sets the original return pointer
		virtual void SetOverrideRet(const void *ptr);	//!< Sets the override result pointer
		virtual void SetIfacePtr(void *ptr);			//!< Sets the interface this pointer
	};
}

#endif