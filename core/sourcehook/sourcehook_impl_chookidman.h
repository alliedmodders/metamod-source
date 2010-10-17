/* ======== SourceHook ========
* Copyright (C) 2004-2010 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

#ifndef __SOURCEHOOK_IMPL_CHOOKIDMAN_H__
#define __SOURCEHOOK_IMPL_CHOOKIDMAN_H__

#include "sh_vector.h"

namespace SourceHook
{
	namespace Impl
	{
		// Associates hook ids with info about the hooks
		// Also used to keep track of used hook ids
		class CHookIDManager
		{
		public:
			struct Entry
			{
				bool isfree;

				// hookman info
				CProto proto;
				int vtbl_offs;
				int vtbl_idx;

				// vfnptr
				void *vfnptr;

				// iface
				void* adjustediface;

				// hook
				Plugin plug;
				int thisptr_offs;
				ISHDelegate *handler;
				bool post;

				Entry(const CProto &pprt, int pvo, int pvi, void *pvp, void *pai, Plugin pplug, int pto,
					ISHDelegate *ph, bool ppost)
					: isfree(false), proto(pprt), vtbl_offs(pvo), vtbl_idx(pvi), vfnptr(pvp), 
					adjustediface(pai), plug(pplug), thisptr_offs(pto), handler(ph), post(ppost)
				{
				}
				Entry()
				{
				}
			};
		private:
			// Internally, hookid 1 is stored as m_Entries[0]

			CVector<Entry> m_Entries;
		public:
			CHookIDManager();
			int New(const CProto &proto, int vtbl_offs, int vtbl_idx, void *vfnptr, void *adjustediface,
				Plugin plug, int thisptr_offs, ISHDelegate *handler, bool post);
			bool Remove(int hookid);
			const Entry * QueryHook(int hookid);

			// Finds all hooks with the given info, and fills the hookids into output.
			void FindAllHooks(CVector<int> &output, const CProto &proto, int vtbl_offs, int vtbl_idx,
				void *adjustediface, Plugin plug, int thisptr_offs, ISHDelegate *handler, bool post);

			// Removes all hooks with a specified vfnptr
			void RemoveAll(void *vfnptr);

			void FindAllHooks(CVector<int> &output);
			void FindAllHooks(CVector<int> &output, Plugin plug);
		};
	}
}

#endif

