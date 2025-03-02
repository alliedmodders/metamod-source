/* ======== SourceHook ========
* vim: set ts=4 :
* Copyright (C) 2024 AlliedModders LLC.  All rights reserved.
* No warranties of any kind
*
* License: zlib/libpng
*
* ============================
*/

#ifndef __SOURCEHOOK_HOOKMANGEN_X86_64_H__
#define __SOURCEHOOK_HOOKMANGEN_X86_64_H__

#include <cstdint>
#include <climits>

#include "sh_asm_x86_64.h"

namespace SourceHook
{
	namespace Impl
	{
		class x64GenContext : public IGenContext
		{
		public:
			x64GenContext();
			x64GenContext(const ProtoInfo *proto, int vtbl_offs, int vtbl_idx, ISourceHook *pSHPtr);
			virtual ~x64GenContext();

			virtual bool Equal(const CProto &proto, int vtbl_offs, int vtbl_idx) override;
			virtual bool Equal(HookManagerPubFunc other) override;

			virtual HookManagerPubFunc GetPubFunc() override;
			HookManagerPubFunc Generate();
		protected:
			friend void foo_test();

			static const std::int32_t SIZE_PTR = sizeof(void*);

			std::int32_t AddVarToFrame(std::int32_t size);
			std::int32_t ComputeVarsSize();
			std::int32_t x64GenContext::GetRealSize(const IntPassInfo& info);
			std::int32_t AlignSize(std::int32_t x, std::int32_t boundary);
			std::int32_t GetParamStackSize(const IntPassInfo &info);

			void Clear();
			void AutoDetectRetType();
			void AutoDetectParamFlags();
			bool PassInfoSupported(const IntPassInfo& pi, bool is_ret);
			void BuildProtoInfo();
			bool MemRetWithTempObj();

			void* GeneratePubFunc();
			void* GenerateHookFunc();

			void CallSetupHookLoop(int v_orig_ret, int v_override_ret, int v_cur_res, int v_prev_res, int v_status, int v_vfnptr_origentry, int v_this, int v_pContext);
			void GenerateCallHooks(int v_status, int v_prev_res, int v_cur_res, int v_iter,
			int v_pContext, int v_plugin_ret, int v_mem_ret);
			void GenerateCallOrig(int v_status, int v_pContext, int v_this, int v_vfnptr_origentry, int v_orig_ret, int v_override_ret, int v_place_for_memret);
			void PrepareReturn(int v_status, int v_pContext, int v_retptr);
			void CallEndContext(int v_pContext);
			void DoReturn(int v_retptr, int v_memret_outaddr);

			std::int32_t PushParameters(int v_this, int v_ret);
			void SaveReturnValue(int v_mem_ret, int v_ret);

			HookManagerPubFunc m_GeneratedPubFunc;

			CProto m_OrigProto;
			CProto m_Proto;
			int m_VtblOffs;
			int m_VtblIdx;
			ISourceHook *m_SHPtr;

			Asm::x64JitWriter m_HookFunc;
			Asm::x64JitWriter m_PubFunc;

			ProtoInfo *m_BuiltPI;
			PassInfo *m_BuiltPI_Params;
			PassInfo::V2Info *m_BuiltPI_Params2;

			void **m_pHI;
			void **m_HookfuncVfnptr;

			std::int32_t m_HookFunc_FrameOffset;
			std::int32_t m_HookFunc_FrameVarsSize;
		};
	}
}

#endif //__SOURCEHOOK_HOOKMANGEN_X86_64_H__