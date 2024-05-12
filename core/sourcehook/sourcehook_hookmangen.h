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

#include "sh_pagealloc.h"
#include "sh_asm.h"
#include <list>
#include <memory>

namespace SourceHook
{
	namespace Impl
	{

		// Code gen stuff
#if SH_COMP == SH_COMP_GCC
#include <stdint.h>
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

		class GenContext : public IGenContext
		{
			const static int SIZE_MWORD = 4;
			const static int SIZE_PTR = sizeof(void*);
			const static int PassFlag_ForcedByRef = (1<<30);   // ByVal in source, but actually passed by reference (GCC) -> private pass, destruct

			HookManagerPubFunc m_GeneratedPubFunc;

			CProto m_OrigProto;			// original passed-in prototype
			CProto m_Proto;
			int m_VtblOffs;
			int m_VtblIdx;
			ISourceHook *m_SHPtr;

			Asm::GenBuffer m_HookFunc;
			Asm::GenBuffer m_PubFunc;

			ProtoInfo *m_BuiltPI;
			PassInfo *m_BuiltPI_Params;
			PassInfo::V2Info *m_BuiltPI_Params2;

			// For hookfunc
			void **m_pHI;
			void **m_HookfuncVfnptr;

			// Level 3 - Helpers
			int m_RegCounter;
			jit_int8_t NextRegEBX_ECX_EDX();

			int m_BytesPushedAfterInitialAlignment;
			enum AlignStackFlags
			{
				AlignStack_GCC_ThisOnStack = 1,
				AlignStack_MSVC_ThisOnStack = 2,
				AlignStack_MemRet = 4
			};
			jit_int32_t AlignStackBeforeCall(int paramsize, int flags);
			void AlignStackAfterCall(jit_int32_t numofbytes);
			void CheckAlignmentBeforeCall();

			// size info
			jit_int32_t GetRealSize(const IntPassInfo &info);			// checks for reference
			jit_int32_t AlignSize(jit_int32_t x, jit_int32_t boundary);	// align a size
			jit_int32_t GetParamStackSize(const IntPassInfo &info);		// get the size of a param in the param stack
			short GetParamsTotalStackSize();		// sum(GetParamStackSize(param[i]), 0 <= i < numOfParams)

			// Helpers
			void BitwiseCopy_Setup();
			void BitwiseCopy_Do(size_t size);


			// HookFunc frame
			jit_int32_t m_HookFunc_FrameOffset;
			jit_int32_t m_HookFunc_FrameVarsSize;

			void ResetFrame(jit_int32_t startOffset);
			jit_int32_t AddVarToFrame(jit_int32_t size);
			jit_int32_t ComputeVarsSize();

			// Param push
			short GetForcedByRefParamsSize();		// sum(param[i] is forcedbyref ? GetStackSize(param[i]) : 0, 0 <= i < numOfParams)
			short GetForcedByRefParamOffset(int p);		// sum(param[i] is forcedbyref ? GetStackSize(param[i]) : 0, 0 <= i < p)
			jit_int32_t PushParams(jit_int32_t param_base_offset, jit_int32_t save_ret_to, 
				jit_int32_t v_place_for_memret, jit_int32_t v_place_fbrr_base);		// save_ret_to and v_place_for_memret only used for memory returns
			jit_int32_t PushRef(jit_int32_t param_offset, const IntPassInfo &pi);
			jit_int32_t PushBasic(jit_int32_t param_offset, const IntPassInfo &pi);
			jit_int32_t PushFloat(jit_int32_t param_offset, const IntPassInfo &pi);
			jit_int32_t PushObject(jit_int32_t param_offset, const IntPassInfo &pi, jit_int32_t v_place_fbrr);
			jit_int32_t PushMemRetPtr(jit_int32_t save_ret_to, jit_int32_t v_place_for_memret);
			void DestroyParams(jit_int32_t fbrr_base);

			// Ret val processing
			void SaveRetVal(jit_int32_t v_where, jit_int32_t v_place_for_memret);
			void ProcessPluginRetVal(jit_int32_t v_cur_res, jit_int32_t v_pContext, jit_int32_t v_plugin_ret);

			void PrepareReturn(jit_int32_t v_status, jit_int32_t v_pContext, jit_int32_t v_retptr);
			void DoReturn(jit_int32_t v_retptr, jit_int32_t v_memret_outaddr);

			bool MemRetWithTempObj();			// do we do a memory return AND need a temporary place for it?

			// Call hooks
			void GenerateCallHooks(int v_status, int v_prev_res, int v_cur_res, int v_iter,
				int v_pContext, int base_param_offset, int v_plugin_ret, int v_place_for_memret, jit_int32_t v_place_fbrr_base, jit_int32_t v_va_buf);

			// Call orig
			void GenerateCallOrig(int v_status, int v_pContext, int param_base_offs, int v_this,
				int v_vfnptr_origentry, int v_orig_ret, int v_override_ret, int v_place_for_memret, jit_int32_t v_place_fbrr_base, jit_int32_t v_va_buf);

			// Hook loop
			void CallSetupHookLoop(int v_orig_ret, int v_override_ret, 
				int v_cur_res, int v_prev_res, int v_status, int v_vnfptr_origentry,
				int v_this, int v_pContext);

			void CallEndContext(int v_pContext);

			// Level 2 -> called from Generate()
			void AutoDetectRetType();
			void AutoDetectParamFlags();
			bool PassInfoSupported(const IntPassInfo &pi, bool is_ret);
			void Clear();
			void BuildProtoInfo();
			void *GenerateHookFunc();
			void *GeneratePubFunc();
			
			HookManagerPubFunc Generate();
		public:
			// Level 1 -> Public interface
			GenContext(const ProtoInfo *proto, int vtbl_offs, int vtbl_idx, ISourceHook *pSHPtr);
			virtual ~GenContext();

			virtual bool Equal(const CProto &proto, int vtbl_offs, int vtbl_idx) override;
			virtual bool Equal(HookManagerPubFunc other) override;

			virtual HookManagerPubFunc GetPubFunc() override;
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
