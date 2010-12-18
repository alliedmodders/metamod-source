/* ======== SourceHook ========
* Copyright (C) 2004-2010 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): Pavol "PM OnoTo" Marko
* ============================
*/

#ifndef __SOURCEHOOK_PIBUILDER_H__
#define __SOURCEHOOK_PIBUILDER_H__

#include "sourcehook.h"
#include "sh_vector.h"

namespace SourceHook
{
	// Helper class: protocol information builder
	class CProtoInfoBuilder
	{
		ProtoInfo m_PI;
		CVector<PassInfo> m_Params;
		CVector<PassInfo::V2Info> m_Params2;
	public:
		CProtoInfoBuilder(int cc)
		{
			memset(reinterpret_cast<void*>(&m_PI), 0, sizeof(ProtoInfo));
			m_PI.convention = cc;

			// dummy 0 params
			PassInfo dummy;
			PassInfo::V2Info dummy2;
			memset(reinterpret_cast<void*>(&dummy), 0, sizeof(PassInfo));
			memset(reinterpret_cast<void*>(&dummy2), 0, sizeof(PassInfo::V2Info));

			dummy.size = 1;		// Version1

			m_Params.push_back(dummy);
			m_Params2.push_back(dummy2);
		}

		void SetReturnType(size_t size, PassInfo::PassType type, int flags,
			void *pNormalCtor, void *pCopyCtor, void *pDtor, void *pAssignOperator)
		{
			if (pNormalCtor)
				flags |= PassInfo::PassFlag_OCtor;

			if (pCopyCtor)
				flags |= PassInfo::PassFlag_CCtor;

			if (pDtor)
				flags |= PassInfo::PassFlag_ODtor;

			if (pAssignOperator)
				flags |= PassInfo::PassFlag_AssignOp;

			m_PI.retPassInfo.size = size;
			m_PI.retPassInfo.type = type;
			m_PI.retPassInfo.flags = flags;
			m_PI.retPassInfo2.pNormalCtor = pNormalCtor;
			m_PI.retPassInfo2.pCopyCtor = pCopyCtor;
			m_PI.retPassInfo2.pDtor = pDtor;
			m_PI.retPassInfo2.pAssignOperator = pAssignOperator;
		}

		void AddParam(size_t size, PassInfo::PassType type, int flags,
			void *pNormalCtor, void *pCopyCtor, void *pDtor, void *pAssignOperator)
		{
			PassInfo pi;
			PassInfo::V2Info pi2;

			if (pNormalCtor)
				flags |= PassInfo::PassFlag_OCtor;

			if (pCopyCtor)
				flags |= PassInfo::PassFlag_CCtor;

			if (pDtor)
				flags |= PassInfo::PassFlag_ODtor;

			if (pAssignOperator)
				flags |= PassInfo::PassFlag_AssignOp;

			
			pi.size = size;
			pi.type = type;
			pi.flags = flags;
			pi2.pNormalCtor = pNormalCtor;
			pi2.pCopyCtor = pCopyCtor;
			pi2.pDtor = pDtor;
			pi2.pAssignOperator = pAssignOperator;

			m_Params.push_back(pi);
			m_Params2.push_back(pi2);
			++m_PI.numOfParams;
		}

		operator ProtoInfo*()
		{
			m_PI.paramsPassInfo = &(m_Params[0]);
			m_PI.paramsPassInfo2 = &(m_Params2[0]);
			return &m_PI;
		}
	};
}

#endif

