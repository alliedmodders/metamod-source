/* ======== SourceHook ========
* Copyright (C) 2004-2010 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

#ifndef __SOURCEHOOK_IMPL_CHOOK_H__
#define __SOURCEHOOK_IMPL_CHOOK_H__

namespace SourceHook
{
	namespace Impl
	{
		class CHook
		{
			// *** Data ***
			Plugin m_OwnerPlugin;
			int m_ThisPointerOffset;
			ISHDelegate *m_pHandler;
			int m_HookID;
			bool m_Paused;
		public:

			// *** Descriptor ***
			struct Descriptor
			{
				Plugin m_OwnerPlugin;
				int m_ThisPointerOffset;
				ISHDelegate *m_pHandler;

				Descriptor(Plugin ownerPlugin, int thisPtrOffset, ISHDelegate *pHandler)
					: m_OwnerPlugin(ownerPlugin), m_ThisPointerOffset(thisPtrOffset),
					m_pHandler(pHandler)
				{
				}
			};

			// *** Interface ***
			inline CHook(Plugin ownerPlugin, int thisPtrOffset, ISHDelegate *pHandler, int hookid, bool paused=false);
			inline bool operator==(const Descriptor &other) const;
			inline bool operator==(int hookid) const;
			inline Plugin GetOwnerPlugin() const;
			inline int GetThisPointerOffset() const;
			inline ISHDelegate *GetHandler() const;
			inline void SetPaused(bool value);
			inline bool IsPaused() const;
			inline int GetID() const;
		};

		// *** Implementation ***
		inline CHook::CHook(Plugin ownerPlugin, int thisPtrOffset, ISHDelegate *pHandler, int hookid, bool paused)
			: m_OwnerPlugin(ownerPlugin), m_ThisPointerOffset(thisPtrOffset),
			m_pHandler(pHandler), m_HookID(hookid), m_Paused(paused)
		{
		}

		inline bool CHook::operator==(const Descriptor &other) const
		{
			return m_OwnerPlugin == other.m_OwnerPlugin
				&& m_ThisPointerOffset == other.m_ThisPointerOffset
				&& m_pHandler == other.m_pHandler;
		}

		inline bool CHook::operator==(int hookid) const
		{
			return m_HookID == hookid;
		}

		inline Plugin CHook::GetOwnerPlugin() const
		{
			return m_OwnerPlugin;
		}

		inline int CHook::GetThisPointerOffset() const
		{
			return m_ThisPointerOffset;
		}

		inline ISHDelegate *CHook::GetHandler() const
		{
			return m_pHandler;
		}

		inline void CHook::SetPaused(bool value)
		{
			m_Paused = value;
		}

		inline bool CHook::IsPaused() const
		{
			return m_Paused;
		}

		inline int CHook::GetID() const
		{
			return m_HookID;
		}
	}
}

#endif

