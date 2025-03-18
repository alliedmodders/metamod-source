/* ======== SourceHook ========
* Copyright (C) 2004-2010 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

#ifndef __SOURCEHOOK_HOOKMANGEN_H__
#define __SOURCEHOOK_HOOKMANGEN_H__

#include <list>
#include <memory>

#include "sourcehook_impl_cproto.h"
#include "sh_pagealloc.h"
#include "sh_asm.h"

namespace SourceHook
{
	namespace Impl
	{

		// Code gen stuff
#if SH_COMP == SH_COMP_GCC
#include <cstdint>
		typedef int8_t jit_int8_t;
		typedef uint8_t jit_uint8_t;
		typedef int32_t jit_int32_t;
		typedef uint32_t jit_uint32_t;
		typedef int64_t jit_int64_t;
		typedef uint64_t jit_uint64_t;
#elif SH_COMP == SH_COMP_MSVC
		typedef __int8 jit_int8_t;
		typedef unsigned __int8 jit_uint8_t;
		typedef __int32 jit_int32_t;
		typedef unsigned __int32 jit_uint32_t;
		typedef __int64 jit_int64_t;
		typedef unsigned __int64 jit_uint64_t;
#endif
		typedef unsigned int jitoffs_t;
		typedef signed int jitrel_t;

		class IGenContext
		{
		public:
			virtual ~IGenContext() {};

			virtual bool Equal(const CProto& proto, int vtbl_offs, int vtbl_idx) = 0;
			virtual bool Equal(HookManagerPubFunc other) = 0;

			virtual HookManagerPubFunc GetPubFunc() = 0;
		};

		class CHookManagerAutoGen : public IHookManagerAutoGen 
		{
			struct StoredContext
			{
				int m_RefCnt;
				std::unique_ptr<IGenContext> m_GenContext;
			};
			std::list<StoredContext> m_Contexts;
			ISourceHook *m_pSHPtr;

		public:
			CHookManagerAutoGen(ISourceHook *pSHPtr);
			~CHookManagerAutoGen();

			int GetIfaceVersion();
			int GetImplVersion();

			HookManagerPubFunc MakeHookMan(const ProtoInfo *proto, int vtbl_offs, int vtbl_idx);
			void ReleaseHookMan(HookManagerPubFunc pubFunc);
		};

	}
}


#endif
