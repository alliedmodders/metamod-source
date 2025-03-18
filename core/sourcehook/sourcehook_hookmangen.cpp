/* ======== SourceHook ========
* Copyright (C) 2004-2010 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* Contributor(s): Borja "faluco" Ferav  (many thanks for assitance!)
*				  David "BAILOPAN" Anderson
* ============================
*/

// recommended literature:
// http://www.cs.umbc.edu/~chang/cs313.s02/stack.shtml
// http://www.angelcode.com/dev/callconv/callconv.html
// http://www.arl.wustl.edu/~lockwood/class/cs306/books/artofasm/Chapter_6/CH06-1.html

#include <cstdio>
#include <cstdarg>							// we might need the address of vsnprintf

#include "sourcehook_impl.h"
#include "sourcehook_hookmangen.h"
#if defined( PLATFORM_64BITS ) && !defined( _LINUX )
#include "sourcehook_hookmangen_x86_64.h"
typedef SourceHook::Impl::x64GenContext SHGenContext;
#else
#include "sourcehook_hookmangen_x86.h"
typedef SourceHook::Impl::GenContext SHGenContext;
#endif
#include "sh_memory.h"

#if SH_COMP == SH_COMP_MSVC
# define GCC_ONLY(x)
# define MSVC_ONLY(x) x
#elif SH_COMP == SH_COMP_GCC
# define GCC_ONLY(x) x
# define MSVC_ONLY(x)
#endif

// :TODO: test BIG vtable indices

namespace SourceHook
{
	CPageAlloc Asm::GenBuffer::ms_Allocator(16);
	
	namespace Impl
	{
		// *********************************** class GenContextContainer
		CHookManagerAutoGen::CHookManagerAutoGen(ISourceHook *pSHPtr) : m_pSHPtr(pSHPtr) { }

		CHookManagerAutoGen::~CHookManagerAutoGen() { }

		int CHookManagerAutoGen::GetIfaceVersion()
		{
			return SH_HOOKMANAUTOGEN_IFACE_VERSION;
		}

		int CHookManagerAutoGen::GetImplVersion()
		{
			return SH_HOOKMANAUTOGEN_IMPL_VERSION;
		}

		HookManagerPubFunc CHookManagerAutoGen::MakeHookMan(const ProtoInfo *proto, int vtbl_offs, int vtbl_idx)
		{
#if defined( PLATFORM_64BITS ) && defined( _LINUX )
			return nullptr;
#else
			CProto mproto(proto);
			for (auto iter = m_Contexts.begin(); iter != m_Contexts.end(); ++iter)
			{
				if (iter->m_GenContext->Equal(mproto, vtbl_offs, vtbl_idx))
				{
					iter->m_RefCnt++;
					return iter->m_GenContext->GetPubFunc();
				}
			}

			// Not found yet -> new one
			StoredContext sctx;
			sctx.m_RefCnt = 1;
			sctx.m_GenContext = std::make_unique<SHGenContext>(proto, vtbl_offs, vtbl_idx, m_pSHPtr);

			auto pubFunc = sctx.m_GenContext->GetPubFunc();
			if (pubFunc != nullptr)
			{
				m_Contexts.emplace_back(std::move(sctx));
			}
			return pubFunc;
#endif
		}

		void CHookManagerAutoGen::ReleaseHookMan(HookManagerPubFunc pubFunc)
		{
			for (auto iter = m_Contexts.begin(); iter != m_Contexts.end(); ++iter)
			{
				if (iter->m_GenContext->Equal(pubFunc))
				{
					iter->m_RefCnt--;
					if (iter->m_RefCnt == 0)
					{
						iter = m_Contexts.erase(iter);
					}
					break;
				}
			}
		}
	}
}
