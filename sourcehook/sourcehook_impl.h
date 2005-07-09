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

#ifdef __linux__
#include <signal.h>
#include <setjmp.h>
#endif

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
			RemoveHookInfo(Plugin pplug, void *piface, int tpo, HookManagerPubFunc phookman,
				ISHDelegate *phandler, bool ppost)
				: plug(pplug), iface(piface), thisptr_offs(tpo),
				hookman(phookman), handler(phandler), post(ppost)
			{
			}

			Plugin plug;
			void *iface;
			int thisptr_offs;
			HookManagerPubFunc hookman;
			ISHDelegate *handler;
			bool post;
		};

		/**
		*	@brief A list of HookManagerInfo structures
		*/
		typedef std::list<HookManagerInfo> HookManInfoList;

		struct CallClassInfo
		{
			GenericCallClass cc;
			int refcounter;
			bool operator==(void *other)
			{
				return cc.ptr == other;
			}
		};
		/**
		*	@brief A list of CallClass structures
		*/
		typedef std::list<CallClassInfo> Impl_CallClassList;

		Impl_CallClassList m_CallClasses;			//!< A list of already generated callclasses
		HookManInfoList m_HookMans;					//!< A list of hook managers

		/**
		*	@brief Finds a hook manager for a function based on a text-prototype, a vtable offset and a vtable index
		*/
		HookManInfoList::iterator FindHookMan(HookManInfoList::iterator begin, HookManInfoList::iterator end, 
			const char *proto, int vtblofs, int vtblidx);

		void ApplyCallClassPatch(CallClassInfo &cc, int vtbl_offs, int vtbl_idx, void *orig_entry);
		void ApplyCallClassPatches(CallClassInfo &cc);
		void ApplyCallClassPatches(void *ifaceptr, int vtbl_offs, int vtbl_idx, void *orig_entry);
		void RemoveCallClassPatch(CallClassInfo &cc, int vtbl_offs, int vtbl_idx);
		void RemoveCallClassPatches(void *ifaceptr, int vtbl_offs, int vtbl_idx);

		META_RES m_Status, m_PrevRes, m_CurRes;
		const void *m_OrigRet;
		const void *m_OverrideRet;
		void *m_IfacePtr;
	public:
		CSourceHookImpl();
		~CSourceHookImpl();

		/**
		*	@brief Returns the interface version
		*/
		int GetIfaceVersion();

		/**
		*	@brief Returns the implemnetation version
		*/
		int GetImplVersion();

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
		bool AddHook(Plugin plug, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan, ISHDelegate *handler, bool post);

		/**
		*	@brief Removes a hook.
		*
		*	@return True if the function succeeded, false otherwise
		*
		*	@param plug The unique identifier of the plugin that calls this function
		*	@param iface The interface pointer
		*	@param thisptr_offs This pointer adjuster
		*	@param myHookMan A hook manager function that should be capable of handling the function
		*	@param handler A pointer to a FastDelegate containing the hook handler
		*	@param post Set to true if you want a post handler
		*/
		bool RemoveHook(Plugin plug, void *iface, int thisptr_offs, HookManagerPubFunc myHookMan, ISHDelegate *handler, bool post);

		/**
		*	@brief Removes a hook.
		*
		*	@ return True if the function succeeded, false otherwise
		*
		*	@param info A RemoveHookInfo structure, describing the hook
		*/
		bool RemoveHook(RemoveHookInfo info);

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
		*	@param size Size of the class instance
		*/
		GenericCallClass *GetCallClass(void *iface, size_t size);

		/**
		*	@brief Release a callclass
		*
		*	@param ptr Pointer to the callclass
		*/
		virtual void ReleaseCallClass(GenericCallClass *ptr);

		virtual void SetRes(META_RES res);				//!< Sets the meta result
		virtual META_RES GetPrevRes();					//!< Gets the meta result of the previously called handler
		virtual META_RES GetStatus();					//!< Gets the highest meta result
		virtual const void *GetOrigRet();				//!< Gets the original result. If not in post function, undefined
		virtual const void *GetOverrideRet();			//!< Gets the override result. If none is specified, NULL
		virtual void *GetIfacePtr();					//!< Gets the interface pointer

		//////////////////////////////////////////////////////////////////////////
		// For hook managers
		virtual META_RES &GetCurResRef();				//!< Gets the reference to the current meta result
		virtual META_RES &GetPrevResRef();				//!< Gets the reference to the previous meta result
		virtual META_RES &GetStatusRef();				//!< Gets the reference to the status variable
		virtual void* &GetIfacePtrRef();				//!< Gets the reference to the iface ptr
		virtual void SetOrigRet(const void *ptr);		//!< Sets the original return pointer
		virtual void SetOverrideRet(const void *ptr);	//!< Sets the override result pointer
	};

#ifdef __linux__
	/**
	*	@brief Checks to see if a memory value can be read from a given pointer.
	*
	*	@return True if the value can be read and false if it cannot.
	*
	*	@param ptr The pointer to the memory value.
	*	@param len The length of the memory value to be read from the pointer.
	*/
	bool IsBadReadPtr(const void *ptr, size_t len);
	void BadReadHandler(int sig);
#endif
}

#endif

