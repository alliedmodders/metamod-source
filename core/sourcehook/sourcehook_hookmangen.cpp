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

#include <stdio.h>
#include "sourcehook_impl.h"
#include "sourcehook_hookmangen.h"
#include "sourcehook_hookmangen_x86.h"
#include "sh_memory.h"
#include <stdarg.h>							// we might need the address of vsnprintf

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
	namespace Impl
	{
		CPageAlloc GenBuffer::ms_Allocator(16);

		template <class T>
		jit_int32_t DownCastPtr(T ptr)
		{
			return reinterpret_cast<jit_int32_t>(ptr);
		}

		jit_uint32_t DownCastSize(size_t size)
		{
			return static_cast<jit_uint32_t>(size);
		}

		GenContext::GenContext(const ProtoInfo *proto, int vtbl_offs, int vtbl_idx, ISourceHook *pSHPtr)
			: m_GeneratedPubFunc(NULL), m_OrigProto(proto), m_Proto(proto), m_VtblOffs(vtbl_offs),
			  m_VtblIdx(vtbl_idx), m_SHPtr(pSHPtr), m_pHI(NULL), m_HookfuncVfnptr(NULL), m_RegCounter(0)
		{
			m_pHI = new void*;
			m_HookfuncVfnptr = new void*;
			m_BuiltPI = new ProtoInfo;
			m_BuiltPI_Params = NULL;
			m_BuiltPI_Params2 = NULL;
		}

		GenContext::~GenContext()
		{
			Clear();
			delete m_pHI;
			delete m_HookfuncVfnptr;
			delete m_BuiltPI;
		}

		void GenContext::Clear()
		{
			m_HookFunc.clear();
			m_PubFunc.clear();
			if (m_BuiltPI_Params)
			{
				delete [] m_BuiltPI_Params;
				m_BuiltPI_Params = NULL;
			}
			if (m_BuiltPI_Params2)
			{
				delete [] m_BuiltPI_Params2;
				m_BuiltPI_Params2 = NULL;
			}
		}

		void GenContext::BuildProtoInfo()
		{
			m_BuiltPI->convention = m_Proto.GetConvention();
			m_BuiltPI->numOfParams = m_Proto.GetNumOfParams();

			m_BuiltPI->retPassInfo.size = m_Proto.GetRet().size;
			m_BuiltPI->retPassInfo.type = m_Proto.GetRet().type;
			m_BuiltPI->retPassInfo.flags = m_Proto.GetRet().flags;
			m_BuiltPI->retPassInfo2.pNormalCtor = m_Proto.GetRet().pNormalCtor;
			m_BuiltPI->retPassInfo2.pCopyCtor = m_Proto.GetRet().pCopyCtor;
			m_BuiltPI->retPassInfo2.pDtor = m_Proto.GetRet().pDtor;
			m_BuiltPI->retPassInfo2.pAssignOperator = m_Proto.GetRet().pAssignOperator;

			if (m_BuiltPI_Params)
				delete m_BuiltPI_Params;
			m_BuiltPI_Params = new PassInfo[m_BuiltPI->numOfParams + 1];
			if (m_BuiltPI_Params2)
				delete m_BuiltPI_Params2;
			m_BuiltPI_Params2 = new PassInfo::V2Info[m_BuiltPI->numOfParams + 1];

			m_BuiltPI_Params[0].size = 1;			// Version 1
			m_BuiltPI_Params[0].type = 0;
			m_BuiltPI_Params[0].flags = 0;
			
			for (int i = 0; i < m_Proto.GetNumOfParams(); ++i)
			{
				m_BuiltPI_Params[i+1].size = m_Proto.GetParam(i).size;
				m_BuiltPI_Params[i+1].type = m_Proto.GetParam(i).type;
				m_BuiltPI_Params[i+1].flags = m_Proto.GetParam(i).flags;

				m_BuiltPI_Params2[i+1].pNormalCtor = m_Proto.GetParam(i).pNormalCtor;
				m_BuiltPI_Params2[i+1].pCopyCtor = m_Proto.GetParam(i).pCopyCtor;
				m_BuiltPI_Params2[i+1].pDtor = m_Proto.GetParam(i).pDtor;
				m_BuiltPI_Params2[i+1].pAssignOperator = m_Proto.GetParam(i).pAssignOperator;
			}

			m_BuiltPI->paramsPassInfo = m_BuiltPI_Params;
			m_BuiltPI->paramsPassInfo2 = m_BuiltPI_Params2;
		}

		jit_int32_t GenContext::GetRealSize(const IntPassInfo &info)
		{
			if (info.flags & (PassInfo::PassFlag_ByRef | PassFlag_ForcedByRef))
			{
				return SIZE_PTR;
			}
			return static_cast<jit_int32_t>(info.size);
		}

		jit_int32_t GenContext::AlignSize(jit_int32_t x, jit_int32_t boundary)
		{
			if (x % boundary != 0)
				x = (x & ~(boundary-1)) + boundary;
			return x;
		}
		
		// Computes size on the stack
		jit_int32_t GenContext::GetParamStackSize(const IntPassInfo &info)
		{
			// Align up to 4 byte boundaries
			return AlignSize(GetRealSize(info), 4);
		}

		jit_int8_t GenContext::NextRegEBX_ECX_EDX()
		{
			switch ((m_RegCounter++) % 3)
			{
			case 0:
				return REG_EBX;
			case 1:
				return REG_ECX;
			case 2:
			default:
				m_RegCounter = 0;
				return REG_EDX;
			}
		}

		void GenContext::BitwiseCopy_Setup()
		{
			//cld
			//push edi
			//push esi

			IA32_Cld(&m_HookFunc);
			IA32_Push_Reg(&m_HookFunc, REG_EDI);
			IA32_Push_Reg(&m_HookFunc, REG_ESI);
		}

		void GenContext::BitwiseCopy_Do(size_t size)
		{
			jit_uint32_t dwords = DownCastSize(size) / 4;
			jit_uint32_t bytes = DownCastSize(size) % 4;

			//if dwords
			// mov ecx, <dwords>
			// rep movsd
			//if bytes
			// mov ecx, <bytes>
			// rep movsb
			//pop esi
			//pop edi

			if (dwords)
			{
				IA32_Mov_Reg_Imm32(&m_HookFunc, REG_ECX, dwords);
				IA32_Rep(&m_HookFunc);
				IA32_Movsd(&m_HookFunc);
			}
			if (bytes)
			{
				IA32_Mov_Reg_Imm32(&m_HookFunc, REG_ECX, bytes);
				IA32_Rep(&m_HookFunc);
				IA32_Movsb(&m_HookFunc);
			}
			IA32_Pop_Reg(&m_HookFunc, REG_ESI);
			IA32_Pop_Reg(&m_HookFunc, REG_EDI);
		}

		jit_int32_t GenContext::AlignStackBeforeCall(int paramsize, int flags)
		{
			paramsize +=
				GCC_ONLY(	((flags & AlignStack_GCC_ThisOnStack)!=0	? SIZE_PTR : 0) + )
				MSVC_ONLY(	((flags & AlignStack_MSVC_ThisOnStack)!=0	? SIZE_PTR : 0) + )
							((flags & AlignStack_MemRet)!=0				? SIZE_PTR : 0);

			// At the beginning of the hookfunc, the stack is aligned to a 16 bytes boundary.
			// Then, m_BytesPushedAfterInitialAlignment were pushed (can also be 0).
			
			// After this function is called, paramsize bytes will be pushed onto the stack
			// After that, the alignment has to be a 16 bytes boundary again.


			// How many bytes we would subtract if the alignment was alright now:
			int subtractFromEsp = 16 - (paramsize % 16);
			if (subtractFromEsp == 16)
				subtractFromEsp = 0;

			// But: there might be bytes pushed alreay!
			subtractFromEsp -= m_BytesPushedAfterInitialAlignment;

			// For example: paramsize was 0 and m_BytesPushedAfterInitialAlignment was 4.
			//	we then have to push another 12 bytes to reach 16 bytes alignment again.

			if (subtractFromEsp < 0)
				subtractFromEsp = 16 - ((-subtractFromEsp) % 16);

			if (subtractFromEsp != 0)
				IA32_Sub_Rm_ImmAuto(&m_HookFunc, REG_ESP, subtractFromEsp, MOD_REG);

			return subtractFromEsp;
		}

		void GenContext::AlignStackAfterCall(jit_int32_t numofbytes)
		{
			if (numofbytes != 0)
				IA32_Add_Rm_ImmAuto(&m_HookFunc, REG_ESP, numofbytes, MOD_REG);
		}

		void GenContext::CheckAlignmentBeforeCall()
		{
#if 0
			IA32_Test_Rm_Imm32(&m_HookFunc, REG_ESP, 15, MOD_REG);
			IA32_Jump_Cond_Imm8(&m_HookFunc, CC_Z, 1);
			IA32_Int3(&m_HookFunc);
#endif
		}

		short GenContext::GetParamsTotalStackSize()
		{
			short acc = 0;
			for (int i = 0; i < m_Proto.GetNumOfParams(); ++i)
			{
				acc += GetParamStackSize(m_Proto.GetParam(i));
			}

			return acc;
		}

		short GenContext::GetForcedByRefParamOffset(int p)
		{
			short off = 0;
			for (int i = 0; i < p; ++i)
			{
				if (m_Proto.GetParam(i).flags & PassFlag_ForcedByRef)
					off += AlignSize(static_cast<jit_int32_t>(m_Proto.GetParam(i).size), 4);
			}
			return off;
		}

		short GenContext::GetForcedByRefParamsSize()
		{
			return GetForcedByRefParamOffset(m_Proto.GetNumOfParams());
		}

		jit_int32_t GenContext::PushRef(jit_int32_t param_offset, const IntPassInfo &pi)
		{
			// push [ebp+<offset>]
			IA32_Push_Rm_DispAuto(&m_HookFunc, REG_EBP, param_offset);

			return SIZE_PTR;
		}

		jit_int32_t GenContext::PushBasic(jit_int32_t param_offset, const IntPassInfo &pi)
		{
			int reg;
			int reg2;

			switch (pi.size)
			{
			default:
				SH_ASSERT(0, ("Unsupported!"));
				return 0;
			case 1:
				reg = NextRegEBX_ECX_EDX();
				//movzx reg, BYTE PTR [ebp+<offset>]
				//push reg
				IA32_Movzx_Reg32_Rm8_DispAuto(&m_HookFunc, reg, REG_EBP, param_offset);
				IA32_Push_Reg(&m_HookFunc, reg);

				return 4;
			case 2:
				reg = NextRegEBX_ECX_EDX();
				//movzx reg, WORD PTR [ebp+<offset>]
				//push reg
				m_HookFunc.write_ubyte(IA32_16BIT_PREFIX);
				IA32_Movzx_Reg32_Rm16_DispAuto(&m_HookFunc, reg, REG_EBP, param_offset);
				IA32_Push_Reg(&m_HookFunc, reg);

				return 4;
			case 4:
				reg = NextRegEBX_ECX_EDX();
				//mov reg, DWORD PTR [ebp+<offset>]
				//push reg
				IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, reg, REG_EBP, param_offset);
				IA32_Push_Reg(&m_HookFunc, reg);

				return 4;
			case 8:
				reg = NextRegEBX_ECX_EDX();
				reg2 = NextRegEBX_ECX_EDX();
				//mov reg, DWORD PTR [ebp+<offset>+4]
				//mov reg2, DWORD PTR [ebp+<offset>]
				//push reg
				//push reg2

				IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, reg, REG_EBP, param_offset+4);
				IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, reg2, REG_EBP, param_offset);
				IA32_Push_Reg(&m_HookFunc, reg);
				IA32_Push_Reg(&m_HookFunc, reg2);

				return 8;
			}
		}

		jit_int32_t GenContext::PushFloat(jit_int32_t param_offset, const IntPassInfo &pi)
		{
			switch (pi.size)
			{
			default:
				SH_ASSERT(0, ("Unsupported!"));
				return 0;
			case 4:
				//fld DWORD PTR [ebp+<offset>]
				//push reg
				//fstp DWORD PTR [esp]
				IA32_Fld_Mem32_DispAuto(&m_HookFunc, REG_EBP, param_offset);
				IA32_Push_Reg(&m_HookFunc, NextRegEBX_ECX_EDX());
				IA32_Fstp_Mem32_ESP(&m_HookFunc);
				return 4;
			case 8:
				//fld QWORD PTR [ebp+<offset>]
				//sub esp, 8
				//fstp QWORD PTR [esp]
				IA32_Fld_Mem64_DispAuto(&m_HookFunc, REG_EBP, param_offset);
				IA32_Sub_Rm_Imm8(&m_HookFunc, REG_ESP, 8, MOD_REG);
				IA32_Fstp_Mem64_ESP(&m_HookFunc);
				return 8;
			}
		}

		jit_int32_t GenContext::PushObject(jit_int32_t param_offset, const IntPassInfo &pi, jit_int32_t place_fbrr)
		{
			if ((pi.flags & PassFlag_ForcedByRef) == 0)
			{
				// make room on the stack
				// sub esp, <size>
				IA32_Sub_Rm_ImmAuto(&m_HookFunc, REG_ESP, GetParamStackSize(pi), MOD_REG);
			}

			// if there is a copy constructor..
			if (pi.pCopyCtor)
			{
				// :TODO: alignment here?
				//  can't use normal alignment methods
				//  because an unknown number of bytes has been pushed already (the other params)

				// save eax
				IA32_Push_Reg(&m_HookFunc, REG_EAX);

				// compute dest addr to ECX
				// = forcedbyref ? ebp+place_fbrr : esp+12
				if (pi.flags & PassFlag_ForcedByRef)
					IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ECX, REG_EBP, place_fbrr);
				else
					IA32_Lea_Reg_DispRegMultImm8(&m_HookFunc, REG_ECX, REG_NOIDX, REG_ESP, NOSCALE, 4);
				
				// compute src addr to EAX
				IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_EAX, REG_EBP, param_offset);

				// push params (gcc: also this)
				IA32_Push_Reg(&m_HookFunc, REG_EAX);
				GCC_ONLY(IA32_Push_Reg(&m_HookFunc, REG_ECX));

				// call
				IA32_Mov_Reg_Imm32(&m_HookFunc, REG_EDX, DownCastPtr(pi.pCopyCtor));
				IA32_Call_Reg(&m_HookFunc, REG_EDX);

				// gcc: clean up stack
				GCC_ONLY(IA32_Add_Rm_ImmAuto(&m_HookFunc, REG_ESP, 2 * SIZE_PTR, MOD_REG));

				// restore eax
				IA32_Pop_Reg(&m_HookFunc, REG_EAX);
			}
			else
			{
				// bitwise copy
				
				BitwiseCopy_Setup();

				//if forcedbyref:
				//  lea edi, [ebp_place_fbrr]
				//else
				//  lea edi, [esp+8]			-  bc_setup pushed two regs onto the stack!
				//lea esi, [ebp+<offs>]
				if (pi.flags & PassFlag_ForcedByRef)
					IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_EDI, REG_EBP, place_fbrr);
				else
					IA32_Lea_Reg_DispRegMultImm8(&m_HookFunc, REG_EDI, REG_NOIDX, REG_ESP, NOSCALE, 8);

				IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ESI, REG_EBP, param_offset);

				BitwiseCopy_Do(pi.size);
			}

			// forcedref: push reference to ebp+place_fbrr
			if (pi.flags & PassFlag_ForcedByRef)
			{
				IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ECX, REG_EBP, place_fbrr);
				IA32_Push_Reg(&m_HookFunc, REG_ECX);
				return SIZE_PTR;
			}
			
			return GetParamStackSize(pi);
		}

		void GenContext::DestroyParams(jit_int32_t fbrr_base)
		{
			for (int i = m_Proto.GetNumOfParams() - 1; i >= 0; --i)
			{
				const IntPassInfo &pi = m_Proto.GetParam(i);
				if (pi.type == PassInfo::PassType_Object && (pi.flags & PassInfo::PassFlag_ODtor) &&
					(pi.flags & PassInfo::PassFlag_ByVal) && (pi.flags & PassFlag_ForcedByRef))
				{
					// Actually, this is only for GCC (see line above: ForcedByRef)
					jit_int32_t tmpAlign = AlignStackBeforeCall(0, AlignStack_GCC_ThisOnStack);
					IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ECX, REG_EBP, fbrr_base + GetForcedByRefParamOffset(i));
					IA32_Push_Reg(&m_HookFunc, REG_ECX);
					IA32_Mov_Reg_Imm32(&m_HookFunc, REG_EAX, DownCastPtr(pi.pDtor));
					CheckAlignmentBeforeCall();
					IA32_Call_Reg(&m_HookFunc, REG_EAX);
					IA32_Pop_Reg(&m_HookFunc, REG_ECX);

					AlignStackAfterCall(tmpAlign);
				}
			}
		}
		
		// May not touch eax!
		jit_int32_t GenContext::PushParams(jit_int32_t param_base_offset, jit_int32_t save_ret_to, int v_place_for_memret,
				jit_int32_t v_place_fbrr_base)
		{
			jit_int32_t added_to_stack = 0;
			jit_int32_t ret = 0;

			// compute the offset _after_ the last parameter
			jit_int32_t cur_offset = param_base_offset;
			for (int i = 0; i < m_Proto.GetNumOfParams(); ++i)
			{
				cur_offset += GetParamStackSize(m_Proto.GetParam(i));
			}

			// push parameters in reverse order
			for (int i = m_Proto.GetNumOfParams() - 1; i >= 0; --i)
			{
				const IntPassInfo &pi = m_Proto.GetParam(i);
				cur_offset -= GetParamStackSize(pi);
				if (pi.flags & PassInfo::PassFlag_ByVal)
				{
					switch (pi.type)
					{
					case PassInfo::PassType_Basic:
						ret = PushBasic(cur_offset, pi);
						break;
					case PassInfo::PassType_Float:
						ret = PushFloat(cur_offset, pi);
						break;
					case PassInfo::PassType_Object:
						ret = PushObject(cur_offset, pi, v_place_fbrr_base + GetForcedByRefParamOffset(i));
						break;
					}
				}
				else if (pi.flags & PassInfo::PassFlag_ByRef)
				{
					ret = PushRef(cur_offset, pi);
				}
				else
				{
					SH_ASSERT(0, ("Unsupported!"));
				}
				added_to_stack += ret;
			}

			return added_to_stack;
		}

		// It is IMPORTANT that PushMemRetPtr doesn't touch ecx and eax
		jit_int32_t GenContext::PushMemRetPtr(jit_int32_t save_ret_to, jit_int32_t v_place_for_memret)
		{
			// Memory return support
			if (m_Proto.GetRet().flags & PassInfo::PassFlag_RetMem)
			{
				// push address where to save it!
				int reg = REG_EDX;
				IA32_Lea_DispRegImmAuto(&m_HookFunc, reg, REG_EBP,
					MemRetWithTempObj() ? v_place_for_memret : save_ret_to);
				IA32_Push_Reg(&m_HookFunc, reg);

				return (SH_COMP==SH_COMP_MSVC) ? 4 : 0;			// varargs funcs on msvc might need this.
																// gcc doesn't: callee cleans the memret ptr, caller the other params :s
			}
			return 0;
		}

		void GenContext::SaveRetVal(int v_where, int v_place_for_memret)
		{
			size_t size = GetRealSize(m_Proto.GetRet());
			if (size == 0)
			{
				// No return value -> nothing
				return;
			}

			if (m_Proto.GetRet().flags & PassInfo::PassFlag_ByRef)
			{
				// mov [ebp + v_plugin_ret], eax
				IA32_Mov_Rm_Reg_DispAuto(&m_HookFunc, REG_EBP, REG_EAX, v_where);
				return;
			}
			// else: ByVal


			// Memory return:
			if (m_Proto.GetRet().flags & PassInfo::PassFlag_RetMem)
			{
				if (MemRetWithTempObj())
				{
					// *v_where = *v_place_for_memret

					// if we have an assign operator, use that
					if (m_Proto.GetRet().pAssignOperator)
					{
						// lea edx, [ebp + v_place_for_memret]  <-- src addr
						// lea ecx, [ebp + v_where]				<-- dest addr
						// push edx
						// gcc: push ecx
						// call it
						// gcc: clean up

						jit_int32_t tmpAlign = AlignStackBeforeCall(SIZE_PTR, AlignStack_GCC_ThisOnStack);

						IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_EDX, REG_EBP, v_place_for_memret);
						IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ECX, REG_EBP, v_where);
						IA32_Push_Reg(&m_HookFunc, REG_EDX);
						GCC_ONLY(IA32_Push_Reg(&m_HookFunc, REG_ECX));

						IA32_Mov_Reg_Imm32(&m_HookFunc, REG_EAX, DownCastPtr(m_Proto.GetRet().pAssignOperator));
						CheckAlignmentBeforeCall();
						IA32_Call_Reg(&m_HookFunc, REG_EAX);
						GCC_ONLY(IA32_Add_Rm_ImmAuto(&m_HookFunc, REG_ESP, 2 * SIZE_PTR, MOD_REG));

						AlignStackAfterCall(tmpAlign);
					}
					else
					{
						// bitwise copy
						BitwiseCopy_Setup();

						//lea edi, [evp+v_where]		<-- destination
						//lea esi, [ebp+v_place_for_memret]	<-- src
						IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_EDI, REG_EBP, v_where);
						IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ESI, REG_EBP, v_place_for_memret);

						BitwiseCopy_Do(m_Proto.GetRet().size);
					}

					// Then: destruct *v_place_for_memret if required
					if (m_Proto.GetRet().pDtor)
					{
						//lea ecx, [ebp+v_place_for_memret]
						//gcc: push ecx
						//call it
						//gcc: clean up

						jit_int32_t tmpAlign = AlignStackBeforeCall(0, AlignStack_GCC_ThisOnStack);

						IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ECX, REG_EBP, v_place_for_memret);
						GCC_ONLY(IA32_Push_Reg(&m_HookFunc, REG_ECX));
						IA32_Mov_Reg_Imm32(&m_HookFunc, REG_EAX, DownCastPtr(m_Proto.GetRet().pDtor));
						CheckAlignmentBeforeCall();
						IA32_Call_Reg(&m_HookFunc, REG_EAX);
						GCC_ONLY(IA32_Pop_Reg(&m_HookFunc, REG_ECX));

						AlignStackAfterCall(tmpAlign);
					}
				}
				else
				{
					// Already copied to correct address -> we're done
					return;
				}
			}

			if (m_Proto.GetRet().type == PassInfo::PassType_Float)
			{
				if (size == 4)
					IA32_Fstp_Mem32_DispAuto(&m_HookFunc, REG_EBP, v_where);
				else if (size == 8)
					IA32_Fstp_Mem64_DispAuto(&m_HookFunc, REG_EBP, v_where);
			}
			else if (m_Proto.GetRet().type == PassInfo::PassType_Basic)
			{
				if (size <= 4)
				{
					// size <= 4: return in EAX
					//  We align <4 sizes up to 4


					// mov [ebp + v_plugin_ret], eax
					IA32_Mov_Rm_Reg_DispAuto(&m_HookFunc, REG_EBP, REG_EAX, v_where);
				}
				else if (size <= 8)
				{
					// size <= 4: return in EAX:EDX
					//  We align 4<x<8 sizes up to 8

					// mov [ebp + v_plugin_ret], eax
					// mov [ebp + v_plugin_ret + 4], edx
					IA32_Mov_Rm_Reg_DispAuto(&m_HookFunc, REG_EBP, REG_EAX, v_where);
					IA32_Mov_Rm_Reg_DispAuto(&m_HookFunc, REG_EBP, REG_EDX, v_where + 4);
				}
			}
			else if (m_Proto.GetRet().type == PassInfo::PassType_Object)
			{
				if (m_Proto.GetRet().flags & PassInfo::PassFlag_RetReg)
				{
					if (size <= 4)
					{
						// size <= 4: return in EAX
						//  We align <4 sizes up to 4

						// mov [ebp + v_plugin_ret], eax
						IA32_Mov_Rm_Reg_DispAuto(&m_HookFunc, REG_EBP, REG_EAX, v_where);
					}
					else if (size <= 8)
					{
						// size <= 4: return in EAX:EDX
						//  We align 4<x<8 sizes up to 8

						// mov [ebp + v_plugin_ret], eax
						// mov [ebp + v_plugin_ret + 4], edx
						IA32_Mov_Rm_Reg_DispAuto(&m_HookFunc, REG_EBP, REG_EAX, v_where);
						IA32_Mov_Rm_Reg_DispAuto(&m_HookFunc, REG_EBP, REG_EDX, v_where + 4);
					}
					else
					{
						SH_ASSERT(0, ("RetReg and size > 8 !"));
					}
				}
			}
		}

		bool GenContext::MemRetWithTempObj()
		{
			// Memory return AND (has destructor OR has assign operator)
			return ((m_Proto.GetRet().flags & PassInfo::PassFlag_RetMem)
				&& (m_Proto.GetRet().flags & (PassInfo::PassFlag_ODtor | PassInfo::PassFlag_AssignOp)));
		}

		void GenContext::ProcessPluginRetVal(int v_cur_res, int v_pContext, int v_plugin_ret)
		{
			// only for non-void functions!
			if (m_Proto.GetRet().size == 0)
				return;

			// if (cur_res >= MRES_OVERRIDE)
			//   *reinterpret_cast<my_rettype*>(pContext->GetOverrideRetPtr()) = plugin_ret;

			//  eax = cur_res
			//  cmp eax,MRES_OVERRIDE
			//  jnge thelabel
			//    pContext->GetOverrideRetPtr()   -->  overrideretptr in eax
			//    *eax = plugin_ret
			//  thelabel:
			//

			jitoffs_t tmppos, counter;

			IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_EAX, REG_EBP, v_cur_res);

			IA32_Cmp_Rm_Imm32(&m_HookFunc, MOD_REG, REG_EAX, MRES_OVERRIDE);
			tmppos = IA32_Jump_Cond_Imm8(&m_HookFunc, CC_NGE, 0);
			m_HookFunc.start_count(counter);

			// eax = pContext->GetOverrideRetPtr()					no alignment needs
			//   ECX = pContext
			//   gcc: push ecx
			//   eax = [ecx]
			//   eax = [eax + 4]
			//   call eax
			//   gcc: clean up
			IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_ECX, REG_EBP, v_pContext);
			GCC_ONLY(IA32_Push_Reg(&m_HookFunc, REG_ECX));

			// vtbloffs=0, vtblidx=1
			IA32_Mov_Reg_Rm(&m_HookFunc, REG_EAX, REG_ECX, MOD_MEM_REG);
			IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_EAX, REG_EAX, 4);
			IA32_Call_Reg(&m_HookFunc, REG_EAX);
			GCC_ONLY(IA32_Pop_Reg(&m_HookFunc, REG_ECX));


			// *eax = plugin_ret
			if (m_Proto.GetRet().flags & PassInfo::PassFlag_ByRef)
			{
				// mov ecx, [ebp+v_plugin_ret]
				// mov [eax], ecx
				IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_ECX, REG_EBP, v_plugin_ret);
				IA32_Mov_Rm_Reg(&m_HookFunc, REG_EAX, REG_ECX, MOD_MEM_REG);
			}
			else
			{
				if (m_Proto.GetRet().pAssignOperator)
				{
					// lea edx, [ebp + v_plugin_ret]
					// push edx					<-- src addr
					// msvc: ecx = eax				<-- dest addr
					// gcc: push eax				<-- dest addr
					// call it
					// gcc: clean up

					jit_int32_t tmpAlign = AlignStackBeforeCall(SIZE_PTR, AlignStack_GCC_ThisOnStack);

					IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_EDX, REG_EBP, v_plugin_ret);
					IA32_Push_Reg(&m_HookFunc, REG_EDX);
#if SH_COMP == SH_COMP_MSVC
					IA32_Mov_Reg_Rm(&m_HookFunc, REG_ECX, REG_EAX, MOD_REG);
#elif SH_COMP == SH_COMP_GCC
					IA32_Push_Reg(&m_HookFunc, REG_EAX);
#endif

					IA32_Mov_Reg_Imm32(&m_HookFunc, REG_EAX, DownCastPtr(m_Proto.GetRet().pAssignOperator));
					CheckAlignmentBeforeCall();
					IA32_Call_Reg(&m_HookFunc, REG_EAX);
					GCC_ONLY(IA32_Add_Rm_ImmAuto(&m_HookFunc, REG_ESP, 2 * SIZE_PTR, MOD_REG));

					AlignStackAfterCall(tmpAlign);
				}
				else
				{
					// bitwise copy
					BitwiseCopy_Setup();

					//mov edi, eax					<-- destination
					//lea esi, [ebp+v_plugin_ret]	<-- src
					IA32_Mov_Reg_Rm(&m_HookFunc, REG_EDI, REG_EAX, MOD_REG);
					IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ESI, REG_EBP, v_plugin_ret);

					BitwiseCopy_Do(m_Proto.GetRet().size);
				}
			}

			m_HookFunc.end_count(counter);
			m_HookFunc.rewrite(tmppos, static_cast<jit_uint8_t>(counter));
		}

		void GenContext::PrepareReturn(int v_status, int v_pContext, int v_retptr)
		{
			// only for non-void functions!
			if (m_Proto.GetRet().size == 0)
				return;

			// retptr = status >= MRES_OVERRIDE ? pContext->GetOverrideRetPtr() : pContext->GetOrigRetPtr()

			// OverrideRetPtr: vtblidx = 1
			// OrigRetPtr: vtbldix = 2
			//  vtblidx = (status >= MRES_OVERRIDE) ? 1 : 2

			// 
			// eax = pContext->GetOverrideRetPtr()
			//   ECX = pContext
			//   gcc: push ecx

			// eax = (status < MRES_OVERRIDE) ? 1 : 0
			//   xor eax, eax
			//   cmp [ebp + v_status], MRES_OVERRIDE
			//   setl al								<-- setcc optimization for ternary operators, 
			

			//   lea eax, [4*eax + 0x4]

			//   edx = [ecx]
			//   add edx, eax
			//   mov edx, [edx]

			//   call edx
			// gcc: clean up

			IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_ECX, REG_EBP, v_pContext);
			GCC_ONLY(IA32_Push_Reg(&m_HookFunc, REG_ECX));
			IA32_Xor_Reg_Rm(&m_HookFunc, REG_EAX, REG_EAX, MOD_REG);
			IA32_Cmp_Rm_Disp8_Imm8(&m_HookFunc, REG_EBP, v_status, MRES_OVERRIDE);
			IA32_SetCC_Rm8(&m_HookFunc, REG_EAX, CC_L);

			IA32_Lea_Reg_RegMultImm32(&m_HookFunc, REG_EAX, REG_EAX, SCALE4, 4);

			IA32_Mov_Reg_Rm(&m_HookFunc, REG_EDX, REG_ECX, MOD_MEM_REG);
			IA32_Add_Reg_Rm(&m_HookFunc, REG_EDX, REG_EAX, MOD_REG);

			IA32_Mov_Reg_Rm(&m_HookFunc, REG_EDX, REG_EDX, MOD_MEM_REG);
			IA32_Call_Reg(&m_HookFunc, REG_EDX);
			GCC_ONLY(IA32_Pop_Reg(&m_HookFunc, REG_ECX));

			IA32_Mov_Rm_Reg_DispAuto(&m_HookFunc, REG_EBP, REG_EAX, v_retptr);
		}

		void GenContext::DoReturn(int v_retptr, int v_memret_outaddr)
		{
			size_t size = m_Proto.GetRet().size;
			if (!size)
				return;

			// Get real ret pointer into ecx
			// mov ecx, [ebp + v_ret_ptr]
			IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_ECX, REG_EBP, v_retptr);

			if (m_Proto.GetRet().flags & PassInfo::PassFlag_ByRef)
			{
				// mov eax, [ecx]
				IA32_Mov_Reg_Rm(&m_HookFunc, REG_EAX, REG_ECX, MOD_MEM_REG);
				return;
			}
			// else: byval

			if (m_Proto.GetRet().type == PassInfo::PassType_Float)
			{
				if (size == 4)
					IA32_Fld_Mem32(&m_HookFunc, REG_ECX);
				else if (size == 8)
					IA32_Fld_Mem64(&m_HookFunc, REG_ECX);
			}
			else if (m_Proto.GetRet().type == PassInfo::PassType_Basic || 
				((m_Proto.GetRet().type == PassInfo::PassType_Object) && (m_Proto.GetRet().flags & PassInfo::PassFlag_RetReg)) )
			{
				if (size <= 4)
				{
					// size <= 4: return in EAX
					//  We align <4 sizes up to 4

					// mov eax, [ecx]
					IA32_Mov_Reg_Rm(&m_HookFunc, REG_EAX, REG_ECX, MOD_MEM_REG);
				}
				else if (size <= 8)
				{
					// size <= 4: return in EAX:EDX
					//  We align 4<x<8 sizes up to 8

					// mov eax, [ecx]
					// mov edx, [ecx+4]
					IA32_Mov_Reg_Rm(&m_HookFunc, REG_EAX, REG_ECX, MOD_MEM_REG);
					IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_EDX, REG_ECX, 4);
				}
				else
				{
					// size >8: return in memory
					//  handled later
				}
			}

			if (m_Proto.GetRet().flags & PassInfo::PassFlag_RetMem)
			{
				// *memret_outaddr = plugin_ret
				if (m_Proto.GetRet().pCopyCtor)
				{
					// mov edx, ecx				<-- src	( we set ecx to [ebp+v_retptr] before )
					// push edx					<-- src addr
					// msvc: ecx = [ebp + v_memret_outaddr]				<-- dest addr
					// gcc: push [ebp + v_memret_outaddr]				<-- dest addr
					// call it
					// gcc: clean up

					jit_int32_t tmpAlign = AlignStackBeforeCall(SIZE_PTR, AlignStack_GCC_ThisOnStack);

					IA32_Mov_Reg_Rm(&m_HookFunc, REG_EDX, REG_ECX, MOD_REG);
					IA32_Push_Reg(&m_HookFunc, REG_EDX);

#if SH_COMP == SH_COMP_MSVC
					IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_ECX, REG_EBP, v_memret_outaddr);
#elif SH_COMP == SH_COMP_GCC
					IA32_Push_Rm_DispAuto(&m_HookFunc, REG_EBP, v_memret_outaddr);
#endif

					IA32_Mov_Reg_Imm32(&m_HookFunc, REG_EAX, DownCastPtr(m_Proto.GetRet().pCopyCtor));
					CheckAlignmentBeforeCall();
					IA32_Call_Reg(&m_HookFunc, REG_EAX);
					GCC_ONLY(IA32_Add_Rm_ImmAuto(&m_HookFunc, REG_ESP, 2 * SIZE_PTR, MOD_REG));

					AlignStackAfterCall(tmpAlign);
				}
				else
				{
					// bitwise copy
					BitwiseCopy_Setup();
					
					//mov edi, [ebp+v_memret_outaddr]		<-- destination
					//mov esi, ecx						<-- src	( we set ecx to [ebp+v_retptr] before )
					IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_EDI, REG_EBP, v_memret_outaddr);
					IA32_Mov_Reg_Rm(&m_HookFunc, REG_ESI, REG_ECX, MOD_REG);

					BitwiseCopy_Do(m_Proto.GetRet().size);
				}

				// In both cases: return the pointer in EAX
				// mov eax, [ebp + v_memret_outaddr]
				IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_EAX, REG_EBP, v_memret_outaddr);
			}
		}

		void GenContext::GenerateCallHooks(int v_status, int v_prev_res, int v_cur_res, int v_iter,
			int v_pContext, int base_param_offset, int v_plugin_ret, int v_place_for_memret, jit_int32_t v_place_fbrr_base, jit_int32_t v_va_buf)
		{
			jitoffs_t counter, tmppos;
			jitoffs_t counter2, tmppos2;

			jitoffs_t loop_begin_counter;

			// prev_res = MRES_IGNORED
			IA32_Mov_Rm_Imm32_Disp8(&m_HookFunc, REG_EBP, MRES_IGNORED, v_prev_res);

			m_HookFunc.start_count(loop_begin_counter);

			// eax = pContext->GetNext()
			//   ECX = pContext
			//   gcc: push ecx
			//   eax = [ecx]
			//   eax = [eax]
			//   call eax
			// gcc: clean up
			IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_ECX, REG_EBP, v_pContext);
			GCC_ONLY(IA32_Push_Reg(&m_HookFunc, REG_ECX));

			// vtbloffs=0, vtblidx=0
			IA32_Mov_Reg_Rm(&m_HookFunc, REG_EAX, REG_ECX, MOD_MEM_REG);
			IA32_Mov_Reg_Rm(&m_HookFunc, REG_EAX, REG_EAX, MOD_MEM_REG);
			IA32_Call_Reg(&m_HookFunc, REG_EAX);
			GCC_ONLY(IA32_Pop_Reg(&m_HookFunc, REG_ECX));

			// quit on zero
			//  test eax, eax
			//  jz exit
			IA32_Test_Rm_Reg(&m_HookFunc, REG_EAX, REG_EAX, MOD_REG);
			tmppos = IA32_Jump_Cond_Imm32(&m_HookFunc, CC_Z, 0);
			m_HookFunc.start_count(counter);

			// prev_res = MRES_IGNORED
			IA32_Mov_Rm_Imm32_Disp8(&m_HookFunc, REG_EBP, MRES_IGNORED, v_cur_res); 

			// iter->call()
			//  push params
			//   ecx = eax
			//   gcc: push ecx
			//   eax = [ecx]
			//   eax = [eax+2*SIZE_PTR]
			//   call eax
			//   gcc: clean up 
			
			jit_int32_t caller_clean_bytes = 0;			// gcc always, msvc never (hooks never have varargs!)

			jit_int32_t alignBytes = AlignStackBeforeCall(
				GetParamsTotalStackSize() + ((m_Proto.GetConvention() & ProtoInfo::CallConv_HasVafmt)!=0 ? SIZE_PTR : 0),
				AlignStack_GCC_ThisOnStack | ((m_Proto.GetRet().flags & PassInfo::PassFlag_RetMem) == 0 ? 0 : AlignStack_MemRet)
				);

			// vafmt: push va_buf
			if (m_Proto.GetConvention() & ProtoInfo::CallConv_HasVafmt)
			{
				IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ECX, REG_EBP, v_va_buf);
				IA32_Push_Reg(&m_HookFunc, REG_ECX);
				caller_clean_bytes += SIZE_PTR;
			}

			caller_clean_bytes += PushParams(base_param_offset, v_plugin_ret, v_place_for_memret, v_place_fbrr_base);

			IA32_Mov_Reg_Rm(&m_HookFunc, REG_ECX, REG_EAX, MOD_REG);
			if (SH_COMP == SH_COMP_GCC)
				IA32_Push_Reg(&m_HookFunc, REG_ECX);
			caller_clean_bytes += PushMemRetPtr(v_plugin_ret, v_place_for_memret);
			IA32_Mov_Reg_Rm(&m_HookFunc, REG_EAX, REG_ECX, MOD_MEM_REG);
			IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_EAX, REG_EAX, 2*SIZE_PTR);
			CheckAlignmentBeforeCall();
			IA32_Call_Reg(&m_HookFunc, REG_EAX);

			AlignStackAfterCall(alignBytes);
			
			// cleanup (gcc only)
			//   params + thisptr
			if (SH_COMP == SH_COMP_GCC)
				IA32_Add_Rm_ImmAuto(&m_HookFunc, REG_ESP, caller_clean_bytes + SIZE_PTR, MOD_REG);

			DestroyParams(v_place_fbrr_base);

			SaveRetVal(v_plugin_ret, v_place_for_memret);

			// process meta return:
			//  prev_res = cur_res
			//  if (cur_res > status) status = cur_res;
			//
			//  eax = cur_res
			//  edx = status
			//  prev_res = eax
			//  cmp eax,edx
			//  jng thelabel
			//  status = eax
			//  thelabel:
			//
			IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_EAX, REG_EBP, v_cur_res);
			IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_EDX, REG_EBP, v_status);
			IA32_Mov_Rm_Reg_Disp8(&m_HookFunc, REG_EBP, REG_EAX, v_prev_res);

			IA32_Cmp_Reg_Rm(&m_HookFunc, REG_EAX, REG_EDX, MOD_REG);
			tmppos2 = IA32_Jump_Cond_Imm8(&m_HookFunc, CC_NG, 0);
			m_HookFunc.start_count(counter2);

			IA32_Mov_Rm_Reg_Disp8(&m_HookFunc, REG_EBP, REG_EAX, v_status);

			m_HookFunc.end_count(counter2);
			m_HookFunc.rewrite(tmppos2, static_cast<jit_uint8_t>(counter2));

			// process retval for non-void functions
			ProcessPluginRetVal(v_cur_res, v_pContext, v_plugin_ret);

			// jump back to loop begin
			tmppos2 = IA32_Jump_Imm32(&m_HookFunc, 0);
			m_HookFunc.end_count(loop_begin_counter);
			m_HookFunc.rewrite(tmppos2, -static_cast<jit_int32_t>(loop_begin_counter));

			m_HookFunc.end_count(counter);
			m_HookFunc.rewrite(tmppos, static_cast<jit_int32_t>(counter));
		}

		void GenContext::GenerateCallOrig(int v_status, int v_pContext, int param_base_offs, int v_this,
			int v_vfnptr_origentry, int v_orig_ret, int v_override_ret, int v_place_for_memret, jit_int32_t v_place_fbrr_base, jit_int32_t v_va_buf)
		{
			jitoffs_t counter, tmppos;
			jitoffs_t counter2, tmppos2;
			jitoffs_t counter3, tmppos3;

			// if (status != MRES_SUPERCEDE && pConteext->ShouldCallOrig())
			//   *v_orig_ret = orig_call()
			// else
			//   *v_orig_ret = *v_override_ret

			// mov eax, status
			// cmp eax, MRES_SUPERCEDE
			// je dont_call
			// call pContext->ShouldCallOrig()
			// test al, al						!! important: al, not eax! bool is only stored in the LSbyte
			// jz dont_call
			//
			//  orig_call()
			//  SaveRet(v_orig_ret)
			//  jmp skip_dont_call:
			//
			// dont_call:
			//  *v_orig_ret = *v_override_ret
			// skip_dont_call:

			IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_EAX, REG_EBP, v_status);
			IA32_Cmp_Rm_Imm32(&m_HookFunc, MOD_REG, REG_EAX, MRES_SUPERCEDE);

			tmppos = IA32_Jump_Cond_Imm32(&m_HookFunc, CC_E, 0);
			m_HookFunc.start_count(counter);

			// eax = pContext->ShouldCallOrig()
			//   ECX = pContext
			//   gcc: push ecx
			//   eax = [ecx]
			//   eax = [eax + 3*SIZE_PTR]
			//   call eax
			// gcc: clean up
			IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_ECX, REG_EBP, v_pContext);
			GCC_ONLY(IA32_Push_Reg(&m_HookFunc, REG_ECX));

			// vtbloffs=0, vtblidx=3
			IA32_Mov_Reg_Rm(&m_HookFunc, REG_EAX, REG_ECX, MOD_MEM_REG);
			IA32_Mov_Reg_Rm_Disp8(&m_HookFunc, REG_EAX, REG_EAX, 3*SIZE_PTR);
			IA32_Call_Reg(&m_HookFunc, REG_EAX);
			GCC_ONLY(IA32_Pop_Reg(&m_HookFunc, REG_ECX));

			IA32_Test_Rm_Reg8(&m_HookFunc, REG_EAX, REG_EAX, MOD_REG);
			tmppos2 = IA32_Jump_Cond_Imm32(&m_HookFunc, CC_Z, 0);
			m_HookFunc.start_count(counter2);

			jit_int32_t caller_clean_bytes = 0;			// gcc always, msvc when cdecl-like (varargs)
		
			jit_int32_t alignBytes = AlignStackBeforeCall(
				GetParamsTotalStackSize()  + ((m_Proto.GetConvention() & ProtoInfo::CallConv_HasVafmt)!=0 ? 2*SIZE_PTR : 0),
				AlignStack_GCC_ThisOnStack |
				((m_Proto.GetRet().flags & PassInfo::PassFlag_RetMem) == 0 ? 0 : AlignStack_MemRet) |
				((m_Proto.GetConvention() & ProtoInfo::CallConv_HasVarArgs) == 0 ? 0 : AlignStack_MSVC_ThisOnStack)
				);

			// vafmt: push va_buf, then "%s"
			if (m_Proto.GetConvention() & ProtoInfo::CallConv_HasVafmt)
			{
				IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ECX, REG_EBP, v_va_buf);
				IA32_Push_Reg(&m_HookFunc, REG_ECX);
				IA32_Push_Imm32(&m_HookFunc, DownCastPtr("%s"));
				caller_clean_bytes += 2*SIZE_PTR;
			}

			// push params
			caller_clean_bytes += PushParams(param_base_offs, v_orig_ret, v_place_for_memret, v_place_fbrr_base);

			// thisptr
			IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_ECX, REG_EBP, v_this);
			if (SH_COMP == SH_COMP_GCC)
			{
				//  on gcc/mingw, this is the first parameter
				IA32_Push_Reg(&m_HookFunc, REG_ECX);
				//  on msvc without varargs, simply leave it in ecx

				// actually, if we're returning in memory, this pointer is the second param
				// and the memret pointer is the real first parameter
				caller_clean_bytes += PushMemRetPtr(v_orig_ret, v_place_for_memret);
			}
			else
			{
				// On msvc, if we're returning in memory, the memret pointer is the first parameter
				caller_clean_bytes += PushMemRetPtr(v_orig_ret, v_place_for_memret);

				// actually, with varargs, the this pointer is the first param and the memret ptr
				// is the second one
				if (m_Proto.GetConvention() & ProtoInfo::CallConv_HasVarArgs)
				{
					IA32_Push_Reg(&m_HookFunc, REG_ECX);
				}
			}

			// call
			IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_EAX, REG_EBP, v_vfnptr_origentry);
			CheckAlignmentBeforeCall();
			IA32_Call_Reg(&m_HookFunc, REG_EAX);

			AlignStackAfterCall(alignBytes);

			// cleanup
			if (SH_COMP == SH_COMP_GCC || (m_Proto.GetConvention() & ProtoInfo::CallConv_HasVarArgs))
				IA32_Add_Rm_ImmAuto(&m_HookFunc, REG_ESP, caller_clean_bytes + SIZE_PTR, MOD_REG);

			DestroyParams(v_place_fbrr_base);

			// save retval
			SaveRetVal(v_orig_ret, v_place_for_memret);

			// Skip don't call variant
			tmppos3 = IA32_Jump_Imm32(&m_HookFunc, 0);
			m_HookFunc.start_count(counter3);


			// don't call:
			m_HookFunc.end_count(counter);
			m_HookFunc.rewrite(tmppos, static_cast<jit_uint32_t>(counter));

			m_HookFunc.end_count(counter2);
			m_HookFunc.rewrite(tmppos2, static_cast<jit_uint32_t>(counter2));

			// *v_orig_ret = *v_override_ret
			if (m_Proto.GetRet().flags & PassInfo::PassFlag_ByRef)
			{
				// mov ecx, [ebp + v_override_ret]
				// mov [ebp + v_orig_ret], ecx

				IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_ECX, REG_EBP, v_override_ret);
				IA32_Mov_Rm_Reg_DispAuto(&m_HookFunc, REG_EBP, REG_ECX, v_orig_ret);
			}
			else
			{
				if (m_Proto.GetRet().pAssignOperator)
				{
					// lea edx, [ebp + v_override_ret]			<-- src addr
					// lea ecx, [ebp + v_orig_ret]				<-- dest addr
					// push edx					<-- src addr
					// gcc: push ecx
					// call it
					// gcc: clean up

					jit_int32_t tmpAlign = AlignStackBeforeCall(SIZE_PTR, AlignStack_GCC_ThisOnStack);

					IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_EDX, REG_EBP, v_override_ret);
					IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ECX, REG_EBP, v_orig_ret);
					IA32_Push_Reg(&m_HookFunc, REG_EDX);
					GCC_ONLY(IA32_Push_Reg(&m_HookFunc, REG_ECX));

					IA32_Mov_Reg_Imm32(&m_HookFunc, REG_EAX, DownCastPtr(m_Proto.GetRet().pAssignOperator));
					CheckAlignmentBeforeCall();
					IA32_Call_Reg(&m_HookFunc, REG_EAX);
					GCC_ONLY(IA32_Add_Rm_ImmAuto(&m_HookFunc, REG_ESP, 2*SIZE_PTR, MOD_REG));

					AlignStackAfterCall(tmpAlign);
				}
				else
				{
					// bitwise copy
					BitwiseCopy_Setup();

					//lea edi, [ebp+v_orig_ret]			<-- destination
					//lea esi, [ebp+v_override_ret]		<-- src
					IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_EDI, REG_EBP, v_orig_ret);
					IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ESI, REG_EBP, v_override_ret);

					BitwiseCopy_Do(m_Proto.GetRet().size);
				}
			}

			// skip don't call label target:
			m_HookFunc.end_count(counter3);
			m_HookFunc.rewrite(tmppos3, static_cast<jit_uint32_t>(counter3));
		}

		// Sets *v_pContext to return value
		void GenContext::CallSetupHookLoop(int v_orig_ret, int v_override_ret, 
			int v_cur_res, int v_prev_res, int v_status, int v_vfnptr_origentry,
			int v_this, int v_pContext)
		{
			// call shptr->SetupHookLoop(ms_HI, ourvfnptr, reinterpret_cast<void*>(this),
			//  &vfnptr_origentry, &status, &prev_res, &cur_res, &orig_ret, &override_ret);
			// The last two params are null for void funcs.

			if (m_Proto.GetRet().size == 0)
			{
				// void
				IA32_Push_Imm8(&m_HookFunc, 0);		// orig_ret
				IA32_Push_Imm8(&m_HookFunc, 0);		// override_ret
			}
			else
			{
				// orig_ret and override_ret
				IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_EAX, REG_EBP, v_override_ret);
				IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_EDX, REG_EBP, v_orig_ret);
				IA32_Push_Reg(&m_HookFunc, REG_EAX);
				IA32_Push_Reg(&m_HookFunc, REG_EDX);
			}

			// cur_res and prev_res
			IA32_Lea_DispRegImm8(&m_HookFunc, REG_EAX, REG_EBP, v_cur_res);
			IA32_Lea_DispRegImm8(&m_HookFunc, REG_EDX, REG_EBP, v_prev_res);
			IA32_Push_Reg(&m_HookFunc, REG_EAX);
			IA32_Push_Reg(&m_HookFunc, REG_EDX);

			// status and vfnptr_origentry
			IA32_Lea_DispRegImm8(&m_HookFunc, REG_EAX, REG_EBP, v_status);
			IA32_Lea_DispRegImm8(&m_HookFunc, REG_EDX, REG_EBP, v_vfnptr_origentry);
			IA32_Push_Reg(&m_HookFunc, REG_EAX);
			IA32_Push_Reg(&m_HookFunc, REG_EDX);

			// this
			IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_EAX, REG_EBP, v_this);
			IA32_Push_Reg(&m_HookFunc, REG_EAX);

			// our vfn ptr
			//  *(this + vtbloffs) + SIZE_PTR*vtblidx
			IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_ECX, REG_EBP, v_this);			// get this into ecx (gcc!)
			IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_EAX, REG_ECX, m_VtblOffs);
			IA32_Add_Rm_ImmAuto(&m_HookFunc, REG_EAX, m_VtblIdx * SIZE_PTR, MOD_REG);
			IA32_Push_Reg(&m_HookFunc, REG_EAX);

			// *m_pHI
			IA32_Mov_Rm_Imm32(&m_HookFunc, REG_EDX, DownCastPtr(m_pHI), MOD_REG);
			IA32_Mov_Reg_Rm(&m_HookFunc, REG_EAX, REG_EDX, MOD_MEM_REG);
			IA32_Push_Reg(&m_HookFunc, REG_EAX);

			// set up thisptr
#if SH_COMP == SH_COMP_GCC
			//  on gcc/mingw, this is the first parameter
			GCC_ONLY(IA32_Push_Imm32(&m_HookFunc, DownCastPtr(m_SHPtr)));
#elif SH_COMP == SH_COMP_MSVC
			//  on msvc, it's ecx
			IA32_Mov_Reg_Imm32(&m_HookFunc, REG_ECX, DownCastPtr(m_SHPtr));
#endif

			// call the function. vtbloffs = 0, vtblidx = 19
			// get vtptr into edx  --  we know shptr on jit time -> dereference it here!
			IA32_Mov_Reg_Imm32(&m_HookFunc, REG_EAX, 
				(*reinterpret_cast<jit_uint32_t**>(m_SHPtr))[19]);

			IA32_Call_Reg(&m_HookFunc, REG_EAX);

			// on gcc/mingw, we have to clean up after the call
			// 9 params + hidden thisptr param
			GCC_ONLY(IA32_Add_Rm_Imm8(&m_HookFunc, REG_ESP, 10*SIZE_PTR, MOD_REG));

			// store return value
			IA32_Mov_Rm_Reg_Disp8(&m_HookFunc, REG_EBP, REG_EAX, v_pContext);
		}

		void GenContext::CallEndContext(int v_pContext)
		{
			// call endcontext:
			// shptr->EndContext(pContex)
			IA32_Mov_Reg_Rm_DispAuto(&m_HookFunc, REG_EAX, REG_EBP, v_pContext);
			IA32_Push_Reg(&m_HookFunc, REG_EAX);

			// thisptr
#if SH_COMP == SH_COMP_GCC
			//  on gcc/mingw, this is the first parameter
			IA32_Push_Imm32(&m_HookFunc, DownCastPtr(m_SHPtr));
#elif SH_COMP == SH_COMP_MSVC
			//  on msvc, it's ecx
			IA32_Mov_Reg_Imm32(&m_HookFunc, REG_ECX, DownCastPtr(m_SHPtr));
#endif

			// get vtptr into edx  --  we know shptr on jit time -> dereference it here!
			IA32_Mov_Reg_Imm32(&m_HookFunc, REG_EAX, 
				(*reinterpret_cast<jit_uint32_t**>(m_SHPtr))[20]);

			IA32_Call_Reg(&m_HookFunc, REG_EAX);

			// on gcc/mingw, we have to clean up after the call
			// 1 param + hidden thisptr param
			GCC_ONLY(IA32_Add_Rm_Imm8(&m_HookFunc, REG_ESP, 2*SIZE_PTR, MOD_REG));
		}

		void GenContext::ResetFrame(jit_int32_t startOffset)
		{
			m_HookFunc_FrameOffset = startOffset;
			m_HookFunc_FrameVarsSize = 0;
		}
		
		jit_int32_t GenContext::AddVarToFrame(jit_int32_t size)
		{
			m_HookFunc_FrameOffset -= size;
			m_HookFunc_FrameVarsSize += size;
			return m_HookFunc_FrameOffset;
		}

		jit_int32_t GenContext::ComputeVarsSize()
		{
			return m_HookFunc_FrameVarsSize;
		}
		
		void * GenContext::GenerateHookFunc()
		{
			// prologue
			IA32_Push_Reg(&m_HookFunc, REG_EBP);
			IA32_Push_Reg(&m_HookFunc, REG_EBX);
			IA32_Mov_Reg_Rm(&m_HookFunc, REG_EBP, REG_ESP, MOD_REG);

			jit_int32_t v_this = 0;
			jit_int32_t param_base_offs = 0;
			if (SH_COMP == SH_COMP_GCC || (m_Proto.GetConvention() & ProtoInfo::CallConv_HasVarArgs))
			{
				// gcc or msvc with varargs:
				v_this = 12;		// first param
				param_base_offs = 16;
				ResetFrame(0);
			}
			else
			{
				// on msvc without varargs, save thisptr
				v_this = -4;
				param_base_offs = 12;
				IA32_Push_Reg(&m_HookFunc, REG_ECX);
				ResetFrame(-4);			// start placing local vars on offset -4
										// because there already is the thisptr variable
			}

			// ********************** stack frame **********************
			//															MSVC without varargs
			//   second param (gcc: first real param)	ebp + 16
			//   first param (gcc: thisptr)				ebp + 12
			//   ret address:							ebp + 8
			//   caller's ebp							ebp + 4
			//   saved ebx								ebp
			//   MSVC ONLY:	current this				ebp - 4
			//   void *vfnptr_origentry					ebp - 4			-4
			//   META_RES status = MRES_IGNORED			ebp - 8			-4
			//   META_RES prev_res						ebp - 12		-4
			//   META_RES cur_res						ebp - 16		-4
			//   IMyDelegate *iter						ebp - 20		-4
			//   IHookContext *pContext					ebp - 24		-4
			//  == 3 ptrs + 3 enums = 24 bytes
			//
			// non-void: add:
			//   my_rettype *ret_ptr					ebp - 28 		-4
			//   my_rettype orig_ret					ebp - 28 - sizeof(my_rettype)			-4
			//   my_rettype override_ret				ebp - 28 - sizeof(my_rettype)*2			-4
			//   my_rettype plugin_ret					ebp - 28 - sizeof(my_rettype)*3			-4
			//  == + 3 * sizeof(my_rettype) bytes

			// if required:
			//   my_rettype place_for_memret			ebp - 28 - sizeof(my_rettype)*4			-4

			// gcc only: if required:
			//   place forced byref params              ebp - 28 - sizeof(my_rettype)*{4 or 5}
			//
			// varargs:
			//   va_list argptr
			//   char va_buf[something];
			

			const jit_int8_t v_vfnptr_origentry =	AddVarToFrame(SIZE_PTR);
			const jit_int8_t v_status =				AddVarToFrame(sizeof(META_RES));
			const jit_int8_t v_prev_res =			AddVarToFrame(sizeof(META_RES));
			const jit_int8_t v_cur_res =			AddVarToFrame(sizeof(META_RES));
			const jit_int8_t v_iter =				AddVarToFrame(SIZE_PTR);
			const jit_int8_t v_pContext =			AddVarToFrame(SIZE_PTR);

			// Memory return: first param is the address
			jit_int32_t v_memret_addr = 0;
			if (m_Proto.GetRet().flags & PassInfo::PassFlag_RetMem)
			{
				if (SH_COMP == SH_COMP_GCC)
				{
					// gcc: now:	first param = mem ret addr
					//				second param = this pointer
					//				third param = actual first param
					v_memret_addr = 12;
					v_this += 4;
					param_base_offs += SIZE_PTR;
				}
				else   // MSVC
				{
					if (m_Proto.GetConvention() & ProtoInfo::CallConv_HasVarArgs)
					{
						// varargs -> cdecl
						// msvc: now:
						//     first param = this pointer
						//     second param = mem ret addr
						//     third param = actual first param

						// params_base_offs is already updated to point to after the this pointer
						v_memret_addr = param_base_offs;
						param_base_offs += SIZE_PTR;
					}
					else
					{
						v_memret_addr = param_base_offs;
						param_base_offs += SIZE_PTR;
					}
				}
			}

			jit_int32_t v_ret_ptr = 0;
			jit_int32_t v_orig_ret = 0;
			jit_int32_t v_override_ret = 0;
			jit_int32_t v_plugin_ret = 0;
			
			if (m_Proto.GetRet().size != 0)
			{
				v_ret_ptr =				AddVarToFrame(SIZE_PTR);
				v_orig_ret =			AddVarToFrame(GetParamStackSize(m_Proto.GetRet()));
				v_override_ret =		AddVarToFrame(GetParamStackSize(m_Proto.GetRet()));
				v_plugin_ret =			AddVarToFrame(GetParamStackSize(m_Proto.GetRet()));
			}

			jit_int32_t v_place_for_memret = 0;
			if (MemRetWithTempObj())
			{
				v_place_for_memret = 	AddVarToFrame(GetParamStackSize(m_Proto.GetRet()));
			}

			jit_int32_t v_place_fbrr_base  = 0;
			if (SH_COMP == SH_COMP_GCC && GetForcedByRefParamsSize())
			{
				v_place_fbrr_base =		AddVarToFrame(GetForcedByRefParamsSize());
			}

			// Only exists for varargs functions
			jit_int32_t v_va_argptr = 0;
			if (m_Proto.GetConvention() & ProtoInfo::CallConv_HasVarArgs)
			{
				v_va_argptr = AddVarToFrame(SIZE_PTR);
			}
			
			jit_int32_t v_va_buf = 0;
			if (m_Proto.GetConvention() & ProtoInfo::CallConv_HasVafmt)
			{
				v_va_buf = AddVarToFrame(SourceHook::STRBUF_LEN);
			}
			
			IA32_Sub_Rm_Imm32(&m_HookFunc, REG_ESP, ComputeVarsSize(), MOD_REG);

			// Initial stack alignment
			IA32_And_Rm_Imm32(&m_HookFunc, REG_ESP, MOD_REG, -16);
			m_BytesPushedAfterInitialAlignment = 0;

			// init status localvar
			IA32_Mov_Rm_Imm32_Disp8(&m_HookFunc, REG_EBP, MRES_IGNORED, v_status);

			// VarArgs: init argptr & format
			if (m_Proto.GetConvention() & ProtoInfo::CallConv_HasVarArgs)
			{
				// argptr = first vararg param
				// lea eax, [ebp + param_base_offs + paramssize]
				// mov argptr, eax

				IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_EAX, REG_EBP, param_base_offs + GetParamsTotalStackSize() + SIZE_PTR);	// +SIZE_PTR: last const char * is not in protoinfo
				IA32_Mov_Rm_Reg_DispAuto(&m_HookFunc, REG_EBP, REG_EAX, v_va_argptr);
			}
			if (m_Proto.GetConvention() & ProtoInfo::CallConv_HasVafmt)
			{
				// vsnprintf
				
				jit_int32_t tmpAlign = AlignStackBeforeCall(SIZE_PTR*3 + sizeof(size_t), 0);

				// push valist, fmt param, maxsize, buffer
				IA32_Push_Reg(&m_HookFunc, REG_EAX);
				IA32_Push_Rm_DispAuto(&m_HookFunc, REG_EBP, param_base_offs + GetParamsTotalStackSize());		// last given param (+4-4, see above)
				IA32_Push_Imm32(&m_HookFunc, SourceHook::STRBUF_LEN - 1);
				IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ECX, REG_EBP, v_va_buf);
				IA32_Push_Reg(&m_HookFunc, REG_ECX);
				
				// call
				IA32_Mov_Reg_Imm32(&m_HookFunc, REG_EAX, DownCastPtr(&vsnprintf));
				CheckAlignmentBeforeCall();
				IA32_Call_Reg(&m_HookFunc, REG_EAX);

				// Clean up (cdecl)
				IA32_Add_Rm_Imm32(&m_HookFunc, REG_ESP, 0x10, MOD_REG);

				AlignStackAfterCall(tmpAlign);

				// Set trailing zero
				IA32_Xor_Reg_Rm(&m_HookFunc, REG_EDX, REG_EDX, MOD_REG);
				IA32_Mov_Rm8_Reg8_DispAuto(&m_HookFunc, REG_EBP, REG_EDX, v_va_buf + SourceHook::STRBUF_LEN - 1);
			}
			
			// Call constructors for ret vars if required
			if((m_Proto.GetRet().flags & PassInfo::PassFlag_ByVal) &&
				m_Proto.GetRet().pNormalCtor)
			{
				jit_int32_t tmpAlign = AlignStackBeforeCall(0, AlignStack_GCC_ThisOnStack);

				// orig_reg
				IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ECX, REG_EBP, v_orig_ret);
				GCC_ONLY(IA32_Push_Reg(&m_HookFunc, REG_ECX));
				IA32_Mov_Reg_Imm32(&m_HookFunc, REG_EAX, DownCastPtr(m_Proto.GetRet().pNormalCtor));
				CheckAlignmentBeforeCall();
				IA32_Call_Reg(&m_HookFunc, REG_EAX);
				GCC_ONLY(IA32_Pop_Reg(&m_HookFunc, REG_ECX));

				// override_reg
				IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ECX, REG_EBP, v_override_ret);
				GCC_ONLY(IA32_Push_Reg(&m_HookFunc, REG_ECX));
				IA32_Mov_Reg_Imm32(&m_HookFunc, REG_EAX, DownCastPtr(m_Proto.GetRet().pNormalCtor));
				CheckAlignmentBeforeCall();
				IA32_Call_Reg(&m_HookFunc, REG_EAX);
				GCC_ONLY(IA32_Pop_Reg(&m_HookFunc, REG_ECX));

				// plugin_ret
				IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ECX, REG_EBP, v_plugin_ret);
				GCC_ONLY(IA32_Push_Reg(&m_HookFunc, REG_ECX));
				IA32_Mov_Reg_Imm32(&m_HookFunc, REG_EAX, DownCastPtr(m_Proto.GetRet().pNormalCtor));
				CheckAlignmentBeforeCall();
				IA32_Call_Reg(&m_HookFunc, REG_EAX);
				GCC_ONLY(IA32_Pop_Reg(&m_HookFunc, REG_ECX));

				AlignStackAfterCall(tmpAlign);

				// _don't_ call a constructor for v_place_for_memret !
			}

			// ********************** SetupHookLoop **********************
			CallSetupHookLoop(v_orig_ret, v_override_ret, v_cur_res, v_prev_res, v_status, v_vfnptr_origentry,
				v_this, v_pContext);

			// ********************** call pre hooks **********************
			GenerateCallHooks(v_status, v_prev_res, v_cur_res, v_iter, v_pContext, param_base_offs,
				v_plugin_ret, v_place_for_memret, v_place_fbrr_base, v_va_buf);

			// ********************** call orig func **********************
			GenerateCallOrig(v_status, v_pContext, param_base_offs, v_this, v_vfnptr_origentry, v_orig_ret,
				v_override_ret, v_place_for_memret, v_place_fbrr_base, v_va_buf);

			// ********************** call post hooks **********************
			GenerateCallHooks(v_status, v_prev_res, v_cur_res, v_iter, v_pContext, param_base_offs,
				v_plugin_ret, v_place_for_memret, v_place_fbrr_base, v_va_buf);

			// ********************** end context and return **********************

			PrepareReturn(v_status, v_pContext, v_ret_ptr);

			CallEndContext(v_pContext);

			// Call destructors of byval object params which have a destructor

			jit_int32_t tmpAlign = AlignStackBeforeCall(0, AlignStack_GCC_ThisOnStack);

			jit_int32_t cur_param_pos = param_base_offs;
			for (int i = 0; i < m_Proto.GetNumOfParams(); ++i)
			{
				const IntPassInfo &pi = m_Proto.GetParam(i);
				// GCC: NOT of forced byref params. the caller destructs those.
				if (pi.type == PassInfo::PassType_Object && (pi.flags & PassInfo::PassFlag_ODtor) &&
					(pi.flags & PassInfo::PassFlag_ByVal) && !(pi.flags & PassFlag_ForcedByRef))
				{
					IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ECX, REG_EBP, cur_param_pos);
					IA32_Mov_Reg_Imm32(&m_HookFunc, REG_EAX, DownCastPtr(pi.pDtor));
					GCC_ONLY(IA32_Push_Reg(&m_HookFunc, REG_ECX));
					CheckAlignmentBeforeCall();
					IA32_Call_Reg(&m_HookFunc, REG_EAX);
					GCC_ONLY(IA32_Pop_Reg(&m_HookFunc, REG_ECX));
				}
				cur_param_pos += GetParamStackSize(pi);
			}

			AlignStackAfterCall(tmpAlign);

			DoReturn(v_ret_ptr, v_memret_addr);

			// Call destructors of orig_ret/ ...
			if((m_Proto.GetRet().flags & PassInfo::PassFlag_ByVal) &&
				m_Proto.GetRet().pDtor)
			{
				// Preserve return value in EAX(:EDX)
				IA32_Push_Reg(&m_HookFunc, REG_EAX);
				IA32_Push_Reg(&m_HookFunc, REG_EDX);

				m_BytesPushedAfterInitialAlignment += 8;

				jit_int32_t tmpAlign = AlignStackBeforeCall(0, AlignStack_GCC_ThisOnStack);

				IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ECX, REG_EBP, v_plugin_ret);
				GCC_ONLY(IA32_Push_Reg(&m_HookFunc, REG_ECX));
				IA32_Mov_Reg_Imm32(&m_HookFunc, REG_EAX, DownCastPtr(m_Proto.GetRet().pDtor));
				CheckAlignmentBeforeCall();
				IA32_Call_Reg(&m_HookFunc, REG_EAX);
				GCC_ONLY(IA32_Pop_Reg(&m_HookFunc, REG_ECX));

				IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ECX, REG_EBP, v_override_ret);
				GCC_ONLY(IA32_Push_Reg(&m_HookFunc, REG_ECX));
				IA32_Mov_Reg_Imm32(&m_HookFunc, REG_EAX, DownCastPtr(m_Proto.GetRet().pDtor));
				CheckAlignmentBeforeCall();
				IA32_Call_Reg(&m_HookFunc, REG_EAX);
				GCC_ONLY(IA32_Pop_Reg(&m_HookFunc, REG_ECX));

				IA32_Lea_DispRegImmAuto(&m_HookFunc, REG_ECX, REG_EBP, v_orig_ret);
				GCC_ONLY(IA32_Push_Reg(&m_HookFunc, REG_ECX));
				IA32_Mov_Reg_Imm32(&m_HookFunc, REG_EAX, DownCastPtr(m_Proto.GetRet().pDtor));
				CheckAlignmentBeforeCall();
				IA32_Call_Reg(&m_HookFunc, REG_EAX);
				GCC_ONLY(IA32_Pop_Reg(&m_HookFunc, REG_ECX));

				AlignStackAfterCall(tmpAlign);

				IA32_Pop_Reg(&m_HookFunc, REG_EDX);
				IA32_Pop_Reg(&m_HookFunc, REG_EAX);

				m_BytesPushedAfterInitialAlignment -= 8;
			}

			// epilogue
			IA32_Mov_Reg_Rm(&m_HookFunc, REG_ESP, REG_EBP, MOD_REG);
			IA32_Pop_Reg(&m_HookFunc, REG_EBX);
			IA32_Pop_Reg(&m_HookFunc, REG_EBP);

			if (SH_COMP == SH_COMP_MSVC && !(m_Proto.GetConvention() & ProtoInfo::CallConv_HasVarArgs))
			{
				// msvc without varargs:
				//  callee cleans the stack

				short cleansize = GetParamsTotalStackSize();
				// Memory return: address is first param
				if (m_Proto.GetRet().flags & PassInfo::PassFlag_RetMem)
					cleansize += SIZE_PTR;
				
				IA32_Return_Popstack(&m_HookFunc, cleansize);
			}
			else
			{
				// gcc or msvc with varargs: caller cleans the stack
				//  exception: gcc removes the memret addr on memret: 
				if (SH_COMP == SH_COMP_GCC && (m_Proto.GetRet().flags & PassInfo::PassFlag_RetMem))
					IA32_Return_Popstack(&m_HookFunc, SIZE_PTR);
				else
					IA32_Return(&m_HookFunc);
			}
			
			
			// Store pointer for later use
			// m_HookfuncVfnPtr is a pointer to a void* because SH expects a pointer
			// into the hookman's vtable
			*m_HookfuncVfnptr = reinterpret_cast<void*>(m_HookFunc.GetData());

			m_HookFunc.SetRE();

			return m_HookFunc.GetData();
		}

		// Pre-condition: GenerateHookFunc() has been called!
		void * GenContext::GeneratePubFunc()
		{
			jitoffs_t counter, tmppos;

			// The pubfunc is a static cdecl function.
			// C Code:
			//  int HookManPubFunc(
			//     bool store,				ebp + 8
			//     IHookManagerInfo *hi		ebp + 12
			//     )
			//  {
			//    if (store)
			//      *m_pHI = hi;
			//    if (hi)
			//      hi->SetInfo(HOOKMAN_VERSION, m_VtblOffs, m_VtblIdx, m_Proto.GetProto(), m_HookfuncVfnptr)
			//  }

			// prologue
			IA32_Push_Reg(&m_PubFunc, REG_EBP);
			IA32_Mov_Reg_Rm(&m_PubFunc, REG_EBP, REG_ESP, MOD_REG);

			
			// save store in eax, hi in ecx
			IA32_Movzx_Reg32_Rm8_Disp8(&m_PubFunc, REG_EAX, REG_EBP, 8);
			IA32_Mov_Reg_Rm_DispAuto(&m_PubFunc, REG_ECX, REG_EBP, 12);

			// Check for store == 0
			IA32_Test_Rm_Reg8(&m_PubFunc, REG_EAX, REG_EAX, MOD_REG);
			tmppos = IA32_Jump_Cond_Imm8(&m_PubFunc, CC_Z, 0);
			m_PubFunc.start_count(counter);

			// nonzero -> store hi
			IA32_Mov_Rm_Imm32(&m_PubFunc, REG_EDX, DownCastPtr(m_pHI), MOD_REG);
			IA32_Mov_Rm_Reg(&m_PubFunc, REG_EDX, REG_ECX, MOD_MEM_REG);

			// zero
			m_PubFunc.end_count(counter);
			m_PubFunc.rewrite(tmppos, static_cast<jit_uint8_t>(counter));

			// check for hi == 0
			IA32_Test_Rm_Reg(&m_PubFunc, REG_ECX, REG_ECX, MOD_REG);
			tmppos = IA32_Jump_Cond_Imm8(&m_PubFunc, CC_Z, 0);
			m_PubFunc.start_count(counter);

			// nonzero -> call vfunc
			//  push params in reverse order
			IA32_Push_Imm32(&m_PubFunc, DownCastPtr(m_HookfuncVfnptr));
			IA32_Push_Imm32(&m_PubFunc, DownCastPtr(m_BuiltPI));
			IA32_Push_Imm32(&m_PubFunc, m_VtblIdx);
			IA32_Push_Imm32(&m_PubFunc, m_VtblOffs);
			IA32_Push_Imm32(&m_PubFunc, SH_HOOKMAN_VERSION);

			//  hi == this is in ecx
			//  on gcc/mingw, ecx is the first parameter
#if SH_COMP == SH_COMP_GCC
			IA32_Push_Reg(&m_PubFunc, REG_ECX);
#endif
			
			// call the function. vtbloffs = 0, vtblidx = 0
			// get vtptr into edx
			IA32_Mov_Reg_Rm(&m_PubFunc, REG_EDX, REG_ECX, MOD_MEM_REG);
			// get funcptr into eax
			IA32_Mov_Reg_Rm(&m_PubFunc, REG_EAX, REG_EDX, MOD_MEM_REG);

			IA32_Call_Reg(&m_PubFunc, REG_EAX);

			// on gcc/mingw, we have to clean up after the call
#if SH_COMP == SH_COMP_GCC
			// 5 params + hidden thisptr param
			IA32_Add_Rm_Imm8(&m_PubFunc, REG_ESP, 6*SIZE_MWORD, MOD_REG);
#endif

			// zero
			m_PubFunc.end_count(counter);
			m_PubFunc.rewrite(tmppos, static_cast<jit_uint8_t>(counter));

			// return value
			IA32_Xor_Reg_Rm(&m_PubFunc, REG_EAX, REG_EAX, MOD_REG);

			// epilogue
			IA32_Mov_Reg_Rm(&m_PubFunc, REG_ESP, REG_EBP, MOD_REG);
			IA32_Pop_Reg(&m_PubFunc, REG_EBP);
			IA32_Return(&m_PubFunc);

			m_PubFunc.SetRE();

			return m_PubFunc;
		}

		bool GenContext::PassInfoSupported(const IntPassInfo &pi, bool is_ret)
		{
			// :TODO: Error returns
			if (pi.type != PassInfo::PassType_Basic &&
				pi.type != PassInfo::PassType_Float &&
				pi.type != PassInfo::PassType_Object)
			{
				return false;
			}

			if (pi.type == PassInfo::PassType_Object &&
				(pi.flags & PassInfo::PassFlag_ByVal))
			{
				if ((pi.flags & PassInfo::PassFlag_CCtor) && !pi.pCopyCtor)
				{
					return false;
				}

				if ((pi.flags & PassInfo::PassFlag_ODtor) && !pi.pDtor)
				{
					return false;
				}
				
				if ((pi.flags & PassInfo::PassFlag_AssignOp) && !pi.pAssignOperator)
				{
					return false;
				}

				if ((pi.flags & PassInfo::PassFlag_OCtor) && !pi.pNormalCtor)
				{
					return false;
				}
			}

			if ((pi.flags & (PassInfo::PassFlag_ByVal | PassInfo::PassFlag_ByRef)) == 0)
			{
				return false;			 // Neither byval nor byref!
			}
			return true;
		}

		void GenContext::AutoDetectRetType()
		{
			IntPassInfo &pi = m_Proto.GetRet();

			// Only relevant for byval types
			if (pi.flags & PassInfo::PassFlag_ByVal)
			{
				// Basic + float: 
				if (pi.type == PassInfo::PassType_Basic ||
					pi.type == PassInfo::PassType_Float)
				{
					// <= 8 bytes:
					//    _always_ in registers, no matter what the user says
					if (pi.size <= 8)
					{
						pi.flags &= ~PassInfo::PassFlag_RetMem;
						pi.flags |= PassInfo::PassFlag_RetReg;
					}
					else
					{
						// Does this even exist? No idea, if it does: in memory!
						pi.flags &= ~PassInfo::PassFlag_RetReg;
						pi.flags |= PassInfo::PassFlag_RetMem;
					}
				}
				// Object: 
				else if (pi.type == PassInfo::PassType_Object)
				{
					// If the user says nothing, auto-detect
					if ((pi.flags & (PassInfo::PassFlag_RetMem | PassInfo::PassFlag_RetReg)) == 0)
					{
#if SH_COMP == SH_COMP_MSVC
						// MSVC seems to return _all_ structs, classes, unions in memory
						pi.flags |= PassInfo::PassFlag_RetMem;
#elif SH_COMP == SH_COMP_GCC
#if SH_SYS == SH_SYS_APPLE
						// Apple GCC returns in memory if size isn't a power of 2 or > 8
						if ((pi.size & (pi.size - 1)) == 0 && pi.size <= 8)
						{
							pi.flags |= PassInfo::PassFlag_RetReg;
						}
						else
#endif
						{
							// GCC on Linux does same thing as MSVC
							pi.flags |= PassInfo::PassFlag_RetMem;
						}
#endif
					}
				}
			}
			else
			{
				// byref: make sure that the flag is _not_ set
				pi.flags &= ~PassInfo::PassFlag_RetMem;
				pi.flags |= PassInfo::PassFlag_RetReg;
			}
		}

		void GenContext::AutoDetectParamFlags()
		{
#if SH_COMP == SH_COMP_GCC
			// On GCC, all objects are passed by reference if they have a destructor
			for (int i = 0; i < m_Proto.GetNumOfParams(); ++i)
			{
				IntPassInfo &pi = m_Proto.GetParam(i);
				if (pi.type == PassInfo::PassType_Object && (pi.flags & PassInfo::PassFlag_ByVal) && 
					(pi.flags & PassInfo::PassFlag_ODtor))
				{
					pi.flags |= PassFlag_ForcedByRef;
				}
			}
#endif
		}

		HookManagerPubFunc GenContext::Generate()
		{
			Clear();

			// Check conditions:
			// -1) good proto version
			//  0) we don't support unknown passtypes, convention, ...
			//  1) we don't support functions which return objects by value or take parameters by value
			//     that have a constructor, a destructor or an overloaded assignment op
			//     (we wouldn't know how to call it!)

			if (m_Proto.GetVersion() < 1)
			{
				return NULL;
			}

			AutoDetectRetType();
			AutoDetectParamFlags();

			// Basically, we only support ThisCall/thiscall with varargs
			if ((m_Proto.GetConvention() & (~ProtoInfo::CallConv_HasVafmt)) != ProtoInfo::CallConv_ThisCall)
			{
				return NULL;
			}


			if (m_Proto.GetRet().size != 0 && !PassInfoSupported(m_Proto.GetRet(), true))
			{
				return NULL;
			}

			for (int i = 0; i < m_Proto.GetNumOfParams(); ++i)
			{
				if (!PassInfoSupported(m_Proto.GetParam(i), false))
					return NULL;
			}

			BuildProtoInfo();
			GenerateHookFunc();
			return fastdelegate::detail::horrible_cast<HookManagerPubFunc>(GeneratePubFunc());
		}

		HookManagerPubFunc GenContext::GetPubFunc()
		{
			if (m_GeneratedPubFunc == 0)
				m_GeneratedPubFunc = Generate();

			return m_GeneratedPubFunc;
		}

		bool GenContext::Equal(const CProto &proto, int vtbl_offs, int vtbl_idx)
		{
			return (m_OrigProto.ExactlyEqual(proto) && m_VtblOffs == vtbl_offs && m_VtblIdx == vtbl_idx);
		}

		bool GenContext::Equal(HookManagerPubFunc other)
		{
			return m_GeneratedPubFunc == other;
		}

		// *********************************** class GenContextContainer
		CHookManagerAutoGen::CHookManagerAutoGen(ISourceHook *pSHPtr) : m_pSHPtr(pSHPtr)
		{
		}

		CHookManagerAutoGen::~CHookManagerAutoGen()
		{
			for (List<StoredContext>::iterator iter = m_Contexts.begin(); iter != m_Contexts.end(); ++iter)
			{
				delete iter->m_GenContext;
			}
		}

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
			CProto mproto(proto);
			for (List<StoredContext>::iterator iter = m_Contexts.begin(); iter != m_Contexts.end(); ++iter)
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
			sctx.m_GenContext = new GenContext(proto, vtbl_offs, vtbl_idx, m_pSHPtr);

			if (sctx.m_GenContext->GetPubFunc() == NULL)
			{
				return NULL;
			}
			else
			{
				m_Contexts.push_back(sctx);
				return sctx.m_GenContext->GetPubFunc();
			}
		}

		void CHookManagerAutoGen::ReleaseHookMan(HookManagerPubFunc pubFunc)
		{
			for (List<StoredContext>::iterator iter = m_Contexts.begin(); iter != m_Contexts.end(); ++iter)
			{
				if (iter->m_GenContext->Equal(pubFunc))
				{
					iter->m_RefCnt--;
					if (iter->m_RefCnt == 0)
					{
						delete iter->m_GenContext;
						m_Contexts.erase(iter);
					}
					break;
				}
			}
		}
	}
}
