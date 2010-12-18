/* ======== SourceHook ========
* Copyright (C) 2004-2010 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

#include "sourcehook_impl.h"

namespace SourceHook
{
	namespace Impl
	{
		void CProto::Fill(const ProtoInfo *pProto)
		{
			if (pProto == NULL)
				m_Version = -1;

			m_ParamsPassInfo.clear();

			if (pProto->paramsPassInfo[0].size == 0)
			{
				// Version 1
				m_Version = 0;
				m_Convention = pProto->convention;
				m_NumOfParams = pProto->numOfParams;

				m_RetPassInfo.size = pProto->retPassInfo.size;
				m_RetPassInfo.type = pProto->retPassInfo.type;
				m_RetPassInfo.flags = GetRealFlags(pProto->retPassInfo);

				m_RetPassInfo.pNormalCtor = NULL;
				m_RetPassInfo.pCopyCtor = NULL;
				m_RetPassInfo.pDtor = NULL;
				m_RetPassInfo.pAssignOperator = NULL;

				
				m_ParamsPassInfo.resize(pProto->numOfParams);

				for (int i = 1; i <= pProto->numOfParams; ++i)
				{
					m_ParamsPassInfo[i-1].size = pProto->paramsPassInfo[i].size;
					m_ParamsPassInfo[i-1].type = pProto->paramsPassInfo[i].type;
					m_ParamsPassInfo[i-1].flags = GetRealFlags(pProto->paramsPassInfo[i]);

					m_ParamsPassInfo[i-1].pNormalCtor = NULL;
					m_ParamsPassInfo[i-1].pCopyCtor = NULL;
					m_ParamsPassInfo[i-1].pDtor = NULL;
					m_ParamsPassInfo[i-1].pAssignOperator = NULL;
				}
			}
			else if (pProto->paramsPassInfo[0].size == 1)
			{
				// Version 2
				m_Version = 1;
				m_Convention = pProto->convention;
				m_NumOfParams = pProto->numOfParams;

				m_RetPassInfo.size = pProto->retPassInfo.size;
				m_RetPassInfo.type = pProto->retPassInfo.type;
				m_RetPassInfo.flags = pProto->retPassInfo.flags;

				m_RetPassInfo.pNormalCtor = pProto->retPassInfo2.pNormalCtor;
				m_RetPassInfo.pCopyCtor = pProto->retPassInfo2.pCopyCtor;
				m_RetPassInfo.pDtor = pProto->retPassInfo2.pDtor;
				m_RetPassInfo.pAssignOperator = pProto->retPassInfo2.pAssignOperator;

				m_ParamsPassInfo.resize(pProto->numOfParams);

				for (int i = 1; i <= pProto->numOfParams; ++i)
				{
					m_ParamsPassInfo[i-1].size = pProto->paramsPassInfo[i].size;
					m_ParamsPassInfo[i-1].type = pProto->paramsPassInfo[i].type;
					m_ParamsPassInfo[i-1].flags = pProto->paramsPassInfo[i].flags;

					m_ParamsPassInfo[i-1].pNormalCtor = pProto->paramsPassInfo2[i].pNormalCtor;
					m_ParamsPassInfo[i-1].pCopyCtor = pProto->paramsPassInfo2[i].pCopyCtor;
					m_ParamsPassInfo[i-1].pDtor = pProto->paramsPassInfo2[i].pDtor;
					m_ParamsPassInfo[i-1].pAssignOperator = pProto->paramsPassInfo2[i].pAssignOperator;
				}
			}
			else
			{
				// Unknown
				m_Version = -1;
			}
		}

		// Basic compat test
		// Other than this, we assume that the plugins know what they're doing
		bool CProto::operator == (const CProto &other) const
		{
			if (m_Version < 0 || other.GetVersion() < 0)
				return false;

			if (m_NumOfParams != other.GetNumOfParams())
				return false;

			if (m_Convention != ProtoInfo::CallConv_Unknown && other.GetConvention() != ProtoInfo::CallConv_Unknown &&
				m_Convention != other.GetConvention())
				return false;

			if (GetRealSize(GetRet()) != GetRealSize(other.GetRet()))
				return false;

			for (int i = 0; i < m_NumOfParams; ++i)
			{
				if (GetRealSize(GetParam(i)) != GetRealSize(other.GetParam(i)))
					return false;
				if (GetParam(i).type != PassInfo::PassType_Unknown && other.GetParam(i).type != PassInfo::PassType_Unknown)
				{
					if (GetParam(i).type != other.GetParam(i).type)
						return false;
					if (GetParam(i).flags != other.GetParam(i).flags)
						return false;
				}
			}

			return true;
		}

		bool CProto::ExactlyEqual(const CProto &other) const
		{
			if (m_Version != other.m_Version ||
				m_NumOfParams != other.m_NumOfParams ||
				m_Convention != other.m_Convention ||
				GetRet() != other.GetRet())
			{
				return false;
			}

			for (int i = 0; i < m_NumOfParams; ++i)
			{
				if(GetParam(i) != other.GetParam(i))
					return false;
			}

			return true;
		}
		
	}
}
