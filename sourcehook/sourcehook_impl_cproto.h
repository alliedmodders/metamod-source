/* ======== SourceHook ========
* Copyright (C) 2004-2007 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

#ifndef __SOURCEHOOK_IMPL_CPROTO_H__
#define __SOURCEHOOK_IMPL_CPROTO_H__

namespace SourceHook
{
	namespace Impl
	{

		class CProto
		{
			ProtoInfo *m_Proto;

			static bool Equal(const ProtoInfo *p1, const ProtoInfo *p2);
			static ProtoInfo *DupProto(const ProtoInfo *src);
			static void FreeProto(ProtoInfo *prot);
		public:
			CProto() : m_Proto(NULL)
			{
			}

			CProto(const ProtoInfo *pProto) : m_Proto(DupProto(pProto))
			{
			}

			CProto(const CProto &other) : m_Proto(DupProto(other.m_Proto))
			{
			}

			~CProto()
			{
				FreeProto(m_Proto);
				m_Proto = NULL;
			}

			void operator = (const ProtoInfo *pProto)
			{
				if (m_Proto)
					FreeProto(m_Proto);
				m_Proto = DupProto(pProto);
			}

			void operator = (const CProto &other)
			{
				if (m_Proto)
					FreeProto(m_Proto);
				m_Proto = DupProto(other.m_Proto);
			}

			bool operator == (const ProtoInfo *pProto) const
			{
				return Equal(pProto, m_Proto);
			}
			bool operator == (const CProto &other) const
			{
				return Equal(other.m_Proto, m_Proto);
			}

			const ProtoInfo *GetProto() const
			{
				return m_Proto;
			}
		};
	}
}

#endif
