/* ======== SourceHook ========
* Copyright (C) 2004-2010 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

#ifndef __SOURCEHOOK_IMPL_CLEANUPTASK_H__
#define __SOURCEHOOK_IMPL_CLEANUPTASK_H__


namespace SourceHook
{
	namespace Impl
	{
		class ICleanupTask
		{
		public:
			virtual void CleanupAndDeleteThis() = 0;
		};
	}
}

// __SOURCEHOOK_IMPL_CLEANUPTASK_H__
#endif

