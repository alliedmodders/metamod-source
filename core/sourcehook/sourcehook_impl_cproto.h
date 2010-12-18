/* ======== SourceHook ========
* Copyright (C) 2004-2010 Metamod:Source Development Team
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
		// Internal representation
		struct IntPassInfo
		{
			size_t size;
			int type;
			unsigned int flags;

			void *pNormalCtor;
			void *pCopyCtor;
			void *pDtor;
			void *pAssignOperator;

			bool operator == (const IntPassInfo &other) const
			{
				return size == other.size
					&& type == other.type
					&& flags == other.flags
					&& pNormalCtor == other.pNormalCtor
					&& pDtor == other.pDtor
					&& pAssignOperator == other.pAssignOperator;
			}

			bool operator != (const IntPassInfo &other) const
			{
				return !(*this == other);
			}
		};

		class CProto
		{
			int m_Version;				// -1 = invalid
			int m_NumOfParams;
			IntPassInfo m_RetPassInfo;
			CVector<IntPassInfo> m_ParamsPassInfo;
			int m_Convention;

			void Fill(const ProtoInfo *pProto);

			// For old sourcehook.h: flags 0 -> assume ByVal
			static unsigned int GetRealFlags(const PassInfo &info)
			{
				return (info.flags == 0) ? PassInfo::PassFlag_ByVal : info.flags;
			}

		public:
			CProto() : m_Version(-1)
			{
			}

			CProto(const ProtoInfo *pProto)
			{
				Fill(pProto);
			}

			CProto(const CProto &other) : m_Version(other.m_Version), m_NumOfParams(other.m_NumOfParams),
				m_RetPassInfo(other.m_RetPassInfo), m_ParamsPassInfo(other.m_ParamsPassInfo),
				m_Convention(other.m_Convention)
			{

			}

			~CProto()
			{
			}

			void operator = (const ProtoInfo *pProto)
			{
				Fill (pProto);
			}

			bool operator == (const CProto &other) const;

			bool ExactlyEqual(const CProto &other) const;

			int GetVersion() const
			{
				return m_Version;
			}

			int GetNumOfParams() const
			{
				return m_NumOfParams;
			}

			const IntPassInfo & GetParam(int i) const
			{
				return m_ParamsPassInfo[i];
			}

			IntPassInfo & GetParam(int i)
			{
				return m_ParamsPassInfo[i];
			}

			const IntPassInfo & GetRet() const
			{
				return m_RetPassInfo;
			}

			IntPassInfo & GetRet()
			{
				return m_RetPassInfo;
			}

			int GetConvention() const
			{
				return m_Convention;
			}

			static size_t GetRealSize(const IntPassInfo &info)
			{
				if (info.flags & PassInfo::PassFlag_ByRef)
				{
					return sizeof(void*);
				}

				return info.size;
			}
		};
	}
}

#endif

