/* ======== SourceHook ========
* Copyright (C) 2024 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): André "Kenzzer" Benoist
* ============================
*/

// recommended literature:
// https://www.felixcloutier.com/x86/
// http://ref.x86asm.net/coder64.html
// https://defuse.ca/online-x86-assembler.htm
// https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention
// https://refspecs.linuxbase.orgz/elf/x86_64-abi-0.99.pdf
// SystemV AMD64 https://gitlab.com/x86-psABIs/x86-64-ABI/-/jobs/artifacts/master/raw/x86-64-ABI/abi.pdf?job=build

#include <stdio.h>
#include <string>
#include "sourcehook_impl.h"
#include "sourcehook_hookmangen.h"
#include "sourcehook_hookmangen_x86_64.h"
#include "sourcehook_pibuilder.h"

#include "metamod_oslink.h"
#include "metamod.h"
#include <interface.h>
#include <eiface.h>
#include <metamod_version.h>
#include <metamod_provider.h>

extern SourceHook::ISourceHook *g_SHPtr;
extern SourceMM::IMetamodSourceProvider *provider;

#if SH_COMP == SH_COMP_MSVC
# define GCC_ONLY(x)
# define MSVC_ONLY(x) x
#elif SH_COMP == SH_COMP_GCC
# define GCC_ONLY(x) x
# define MSVC_ONLY(x)
#endif

// On Windows: this is the start of the shadow space.
// On Linux: this is the start of the stack args.
const std::int32_t OffsetToCallerStack = 16;

using namespace SourceHook::Asm;

namespace SourceHook
{
	namespace Impl
	{
		void PrintDebug(x64JitWriter& jit, const char* message) {
			static MemFuncInfo mfi = {false, -1, -1, -1};
			if (mfi.vtblindex == -1)
			{
				GetFuncInfo(&SourceMM::IMetamodSourceProvider::ConsolePrint, mfi);
				if (!mfi.isVirtual || mfi.thisptroffs != 0 || mfi.vtbloffs != 0 || mfi.vtblindex < 0)
				{
					mfi.vtblindex = -1;
					SH_ASSERT(0, ("Couldn't retrieve details of SourceMM::IMetamodSourceProvider::ConsolePrint!"));
					return;
				}
			}

			static MemFuncInfo mfi2 = {false, -1, -1, -1};
			if (mfi2.vtblindex == -1)
			{
				GetFuncInfo(&SourceMM::IMetamodSourceProvider::LogMessage, mfi2);
				if (!mfi2.isVirtual || mfi2.thisptroffs != 0 || mfi2.vtbloffs != 0 || mfi2.vtblindex < 0)
				{
					mfi2.vtblindex = -1;
					SH_ASSERT(0, ("Couldn't retrieve details of SourceMM::IMetamodSourceProvider::ConsolePrint!"));
					return;
				}
			}


			MSVC_ONLY(jit.mov(rcx, reinterpret_cast<std::uint64_t>(provider)));
			GCC_ONLY(jit.mov(rdi, reinterpret_cast<std::uint64_t>(provider)));
			
			MSVC_ONLY(jit.mov(rdx, reinterpret_cast<std::uint64_t>(message)));
			GCC_ONLY(jit.mov(rsi, reinterpret_cast<std::uint64_t>(message)));

			jit.mov(rax, reinterpret_cast<std::uint64_t>(provider));
			jit.mov(rax, rax(mfi.vtbloffs));
			jit.mov(rax, rax(sizeof(void*) * mfi.vtblindex));
			jit.call(rax);

			MSVC_ONLY(jit.mov(rcx, reinterpret_cast<std::uint64_t>(provider)));
			GCC_ONLY(jit.mov(rdi, reinterpret_cast<std::uint64_t>(provider)));
			
			MSVC_ONLY(jit.mov(rdx, reinterpret_cast<std::uint64_t>(message)));
			GCC_ONLY(jit.mov(rsi, reinterpret_cast<std::uint64_t>(message)));

			jit.mov(rax, reinterpret_cast<std::uint64_t>(provider));
			jit.mov(rax, rax(mfi2.vtbloffs));
			jit.mov(rax, rax(sizeof(void*) * mfi2.vtblindex));
			jit.call(rax);
		}

		x64GenContext::x64GenContext()
			: m_GeneratedPubFunc(nullptr), m_VtblOffs(0),
			  m_VtblIdx(666), m_SHPtr((ISourceHook*)0x1122334455667788), m_pHI(nullptr), m_HookfuncVfnptr(nullptr), m_HookFunc_FrameOffset(0), m_HookFunc_FrameVarsSize(0) {
			m_pHI = new void*;
			*m_pHI = (void*)0x77777777;
			m_HookfuncVfnptr = new void*;
			m_BuiltPI = new ProtoInfo;
			m_BuiltPI_Params = nullptr;
			m_BuiltPI_Params2 = nullptr;
		}

		x64GenContext::x64GenContext(const ProtoInfo *proto, int vtbl_offs, int vtbl_idx, ISourceHook *pSHPtr)
			: m_GeneratedPubFunc(nullptr), m_OrigProto(proto), m_Proto(proto), m_VtblOffs(vtbl_offs),
			  m_VtblIdx(vtbl_idx), m_SHPtr(pSHPtr), m_pHI(nullptr), m_HookfuncVfnptr(nullptr), m_HookFunc_FrameOffset(0), m_HookFunc_FrameVarsSize(0)
		{
			m_pHI = new void*;
			*m_pHI = (void*)0x77777777; // Magic number for debugging
			m_HookfuncVfnptr = new void*;
			m_BuiltPI = new ProtoInfo;
			m_BuiltPI_Params = nullptr;
			m_BuiltPI_Params2 = nullptr;
		}

		x64GenContext::~x64GenContext()
		{
			//Clear();
			delete m_pHI;
			delete m_HookfuncVfnptr;
			delete m_BuiltPI;
		}

		void x64GenContext::Clear()
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

		void x64GenContext::BuildProtoInfo()
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
		
		std::int32_t x64GenContext::AddVarToFrame(std::int32_t size)
		{
			m_HookFunc_FrameOffset -= size;
			m_HookFunc_FrameVarsSize += size;
			return m_HookFunc_FrameOffset;
		}

		std::int32_t x64GenContext::ComputeVarsSize()
		{
			return m_HookFunc_FrameVarsSize;
		}

		std::int32_t x64GenContext::GetRealSize(const IntPassInfo& info)
		{
			if ((info.flags & PassInfo::PassFlag_ByRef) == PassInfo::PassFlag_ByRef) {
				return SIZE_PTR;
			}
			return static_cast<std::int32_t>(info.size);
		}

		std::int32_t x64GenContext::AlignSize(std::int32_t x, std::int32_t boundary)
		{
			if (x % boundary != 0)
				x = (x & ~(boundary-1)) + boundary;
			return x;
		}
		
		// Computes size on the stack
		std::int32_t x64GenContext::GetParamStackSize(const IntPassInfo &info)
		{
			// Align up to 8 byte boundaries
			return AlignSize(GetRealSize(info), SIZE_PTR);
		}

		HookManagerPubFunc x64GenContext::Generate()
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
				return nullptr;
			}

			// Detect the pass flags (if they're missing) for return and parameters type
			if (!AutoDetectRetType())
			{
				return nullptr;
			}
			AutoDetectParamFlags();

			// Calling conventions are gone on x86_64, there's only one to call all functions
			// however act as if they still exist to avoid code duplication on the user's side
			// TO-DO: Handle microsoft's vectorcall
			if ((m_Proto.GetConvention() & (~ProtoInfo::CallConv_HasVafmt)) != ProtoInfo::CallConv_ThisCall)
			{
				return nullptr;
			}

			// Non void return, ensure we support it
			if (m_Proto.GetRet().size != 0 && !PassInfoSupported(m_Proto.GetRet(), true))
			{
				return nullptr;
			}

			// Ensure we support each param
			for (int i = 0; i < m_Proto.GetNumOfParams(); ++i)
			{
				if (!PassInfoSupported(m_Proto.GetParam(i), false))
				{
					return nullptr;
				}
			}

			BuildProtoInfo();
			GenerateHookFunc();
			return fastdelegate::detail::horrible_cast<HookManagerPubFunc>(GeneratePubFunc());
		}

		bool x64GenContext::PassInfoSupported(const IntPassInfo& pi, bool is_ret)
		{
			if (pi.type != PassInfo::PassType_Basic &&
				pi.type != PassInfo::PassType_Float &&
				pi.type != PassInfo::PassType_Object) {
				return false;
			}

			if (pi.type == PassInfo::PassType_Object &&
				(pi.flags & PassInfo::PassFlag_ByVal)) {
				if ((pi.flags & PassInfo::PassFlag_CCtor) && !pi.pCopyCtor) {
					return false;
				}

				if ((pi.flags & PassInfo::PassFlag_ODtor) && !pi.pDtor) {
					return false;
				}
				
				if ((pi.flags & PassInfo::PassFlag_AssignOp) && !pi.pAssignOperator) {
					return false;
				}

				if ((pi.flags & PassInfo::PassFlag_OCtor) && !pi.pNormalCtor) {
					return false;
				}
			}

			if ((pi.flags & (PassInfo::PassFlag_ByVal | PassInfo::PassFlag_ByRef)) == 0) {
				return false;			 // Neither byval nor byref!
			}
			return true;
		}

		void* x64GenContext::GenerateHookFunc()
		{
			const auto& retInfo = m_Proto.GetRet();
			m_HookFunc.breakpoint();

			// For the time being, we only consider xmm0-xmm15 registers
			// are only used to store 64bits worth of data, despite being
			// able to store up to 128bits

			// Linux uses RBP as the frame pointer while Windows mostly doesn't*.
			// It's a good register to index stack variables so we'll still use it like a Linux frame pointer.
			// It'll probably help Accelerator's crash-logging too (at least on Linux).
			//
			// *: MSVC does not support the frame pointer option (/Oy-) in x64!
			//    alloca() and some exception handling things will use it though.
			//    Their usage is also weird: https://stackoverflow.com/q/75722486

			// Save our frame pointer.
			// This also realigns the stack to 16 bytes.
			m_HookFunc.push(rbp);
			m_HookFunc.mov(rbp, rsp);

			// *********** stack frame *************

			// MSVC ONLY START
			// rbp + ??                             end of stack args
			// rbp + 48                             start of stack args
			// rbp + 40                             shadow space 4
			// rbp + 32                             shadow space 3
			// rbp + 24                             shadow space 2
			// rbp + 16                             shadow space 1
			// MSVC ONLY END
			//
			// GCC ONLY START
			// rbp + ??                             end of stack args
			// rbp + 16                             start of stack args
			// GCC ONLY END
			//
			// rbp + 8                              return address
			// rbp - 0                              original rbp
			// rbp - 8                              this ptr
			// rbp - 16                             vfnptr_origentry
			// rbp - 24                             status
			// rbp - 32                             prev_res
			// rbp - 40                             cur_res
			// rbp - 48                             iter
			// rbp - 56                             context
			// [Non void functions:]
			// rbp - 64                             ret ptr
			// rbp - 72                             memret ptr
			// rbp - 80 - sizeof(returntype)        original return
			// rbp - 80 - sizeof(returntype) * 2    override return
			// rbp - 80 - sizeof(returntype) * 3    plugin return
			//
			// - 64                                 end of unused padding
			// - 128                                start of unused padding
			//
			// MSVC ONLY START
			// - 128                                end of 80 bytes of shadow space
			// - 208                                start of 80 bytes of shadow space
			// MSVC ONLY END
			//
			// GCC ONLY START
			// - 128                                end of stack arg copies
			// - ???                                start of stack arg copies
			// GCC ONLY END

			const std::int8_t v_this =               AddVarToFrame(SIZE_PTR); // -8
			const std::int8_t v_vfnptr_origentry =   AddVarToFrame(SIZE_PTR); // -16
			const std::int8_t v_status =             AddVarToFrame(SIZE_PTR /*sizeof(META_RES)*/); // -24
			const std::int8_t v_prev_res =           AddVarToFrame(SIZE_PTR /*sizeof(META_RES)*/); // -32
			const std::int8_t v_cur_res =            AddVarToFrame(SIZE_PTR /*sizeof(META_RES)*/); // -40
			const std::int8_t v_iter =               AddVarToFrame(SIZE_PTR); // -48
			const std::int8_t v_pContext =           AddVarToFrame(SIZE_PTR); // -56

			// Non void return, track the values
			std::int8_t v_ret_ptr =       0;
			std::int8_t v_memret_ptr =    0;
			std::int32_t v_orig_ret =     0;
			std::int32_t v_override_ret = 0;
			std::int32_t v_plugin_ret =   0;
			std::int32_t v_mem_ret =      0;
			if (m_Proto.GetRet().size != 0)
			{
				v_ret_ptr =      AddVarToFrame(SIZE_PTR); // -64
				v_memret_ptr =   AddVarToFrame(SIZE_PTR); // -72
				// Did you know that 80 is 5*16? I'm gonna be sick...
				v_orig_ret =     AddVarToFrame(AlignSize(GetParamStackSize(retInfo), 16)); // -80 // 16 bytes aligned
				v_override_ret = AddVarToFrame(AlignSize(GetParamStackSize(retInfo), 16));
				v_plugin_ret =   AddVarToFrame(AlignSize(GetParamStackSize(retInfo), 16));
				v_mem_ret =      AddVarToFrame(AlignSize(GetParamStackSize(retInfo), 16));
			}

			// TODO: Only added to *maybe* prevent crashes when people fuck up their parameters.
			//       Probably shouldn't be kept...
			std::int32_t v_padding_after_ret = AddVarToFrame(64);

#if SH_COMP == SH_COMP_MSVC
			// Regular shadow space + 6 stack args for CallSetupHookLoop.
			// Don't actually use this variable, just index `rsp` instead.
			std::int32_t v_local_shadow_space = AddVarToFrame(32 + 6*8);
#endif

#if SH_COMP == SH_COMP_MSVC
			std::int32_t stack_frame_size = AlignSize(ComputeVarsSize(), 16);
			m_HookFunc.sub(rsp, stack_frame_size);

			// MSVC ONLY - Save the registers into shadow space
			const x86_64_Reg params_reg[] = { rcx, rdx, r8, r9 };
			const x86_64_FloatReg params_floatreg[] = { xmm0, xmm1, xmm2, xmm3 };

			int reg_index = 0;

			// retrieve this ptr
			m_HookFunc.mov(rbp(v_this), params_reg[reg_index]);
			m_HookFunc.mov(rbp(reg_index * 8 + OffsetToCallerStack), params_reg[reg_index]);
			reg_index++;

			// Non standard return size, a ptr has been passed into rcx. Shifting all the parameters
			if ((retInfo.flags & PassInfo::PassFlag_RetMem) == PassInfo::PassFlag_RetMem) {
				m_HookFunc.mov(rbp(reg_index * 8 + OffsetToCallerStack), params_reg[reg_index]);
				m_HookFunc.mov(rbp(v_memret_ptr), params_reg[reg_index]);
				reg_index++;
			}

			// START DEBUG HELPERS
			m_HookFunc.mov(rax, m_Proto.GetNumOfParams());
			m_HookFunc.mov(rax, reg_index);
			// END DEBUG HELPERS
			m_HookFunc.mov(rax, retInfo.size);

			for (int i = 0; i < m_Proto.GetNumOfParams() && reg_index < 4; reg_index++, i++) {
				auto& info = m_Proto.GetParam(i);
				if (info.type == PassInfo::PassType_Float && (info.flags & PassInfo::PassFlag_ByRef) != PassInfo::PassFlag_ByRef) {
					m_HookFunc.movsd(rbp(reg_index * 8 + OffsetToCallerStack), params_floatreg[reg_index]);
				} else {
					m_HookFunc.mov(rbp(reg_index * 8 + OffsetToCallerStack), params_reg[reg_index]);
				}
			}
#else
			const x86_64_Reg params_reg[] = { rdi, rsi, rdx, rcx, r8, r9 };
			const x86_64_FloatReg params_floatreg[] = { xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7 };
			int num_reg = sizeof(params_reg) / sizeof(params_reg[0]);
			int num_floatreg = sizeof(params_floatreg) / sizeof(params_floatreg[0]);

			int reg_index = 0;
			int floatreg_index = 0;

			// Retmem is in RDI even when thiscall!
			if ((retInfo.flags & PassInfo::PassFlag_RetMem) == PassInfo::PassFlag_RetMem) {
				m_HookFunc.mov(rbp(v_memret_ptr), params_reg[reg_index]);
				reg_index++;
			}

			m_HookFunc.mov(rbp(v_this), params_reg[reg_index]);
			reg_index++;

			// Linux objects can be passed
			// - inline on the stack
			// - unpacked into registers
			// - as a pointer
			// This is pretty complicated and we can't know what will happen without knowing the object layout.
			// And we don't know the object layout...
			//
			// From Agner Fog's Calling conventions pdf:
			// >Objects with inheritance, member functions, or constructors can be passed in registers [and on stack].
			// >Objects with copy constructor, destructor, or virtual are passed by pointers.
			//
			// For now, we'll assume that any object passed inline on the stack can be safely copied around.
			// This probably doesn't hold in 100% of cases but for now it's the easiest.
			//
			// Another option would be to fuck up our stack frame:
			// - move all offsets and data we use like 32KiB or 64KiB deeper into the stack
			// - `pop` the real return address off the stack and store it in the deep stack (until we need it)
			// This would allow to use the original objects that are inlined on the stack in.

			// TODO: Allow 2 floats per custom_register.
			//       Allow 2 ints per custom_register too!
			//       This helps with objects being unpacked into registers.

			// We're going to backup ALL parameter registers.
			std::int32_t v_sysv_floatreg = AddVarToFrame(num_floatreg * 8);
			std::int32_t v_sysv_reg = AddVarToFrame(num_reg * 8);

			auto orig_reg_index = reg_index;
			// Next we need to figure out how must stack space for stack args...
			std::int32_t stack_args_size = 0;
			for (int i = 0; i < m_Proto.GetNumOfParams(); ++i) {
				const IntPassInfo &pi = m_Proto.GetParam(i);

				if (pi.type == PassInfo::PassType_Basic) {
					if (++reg_index >= num_reg) {
						stack_args_size += 8;
					}
				} else if (pi.type == PassInfo::PassType_Float) {
					if (++floatreg_index >= num_floatreg) {
						stack_args_size += 8;
					}
				} else if (pi.type == PassInfo::PassType_Object) {
					if (pi.flags & PassInfo::PassFlag_ByRef) {
						if (++reg_index >= num_reg) {
							stack_args_size += 8;
						}
					} else {
						stack_args_size += pi.size;
					}
				}
			}
			std::int32_t v_sysv_stack_copy = AddVarToFrame(stack_args_size);

			std::int32_t stack_frame_size = AlignSize(ComputeVarsSize(), 16);
			m_HookFunc.sub(rsp, stack_frame_size);

			for (int i = 0; i < num_floatreg; i++) {
				m_HookFunc.movsd(rbp(v_sysv_floatreg + i*8), params_floatreg[i]);
			}
			for (int i = 0; i < num_reg; i++) {
				m_HookFunc.mov(rbp(v_sysv_reg + i*8), params_reg[i]);
			}

			// Now let's copy stack params!
			reg_index = orig_reg_index;
			floatreg_index = 0;
			std::int32_t stack_offset = 0;
			for (int i = 0; i < m_Proto.GetNumOfParams(); ++i) {
				const IntPassInfo &pi = m_Proto.GetParam(i);

				if (pi.type == PassInfo::PassType_Basic) {
					if (++reg_index >= num_reg) {
						m_HookFunc.lea(rax, rbp(OffsetToCallerStack + stack_offset));
						m_HookFunc.mov(rax, rax());
						m_HookFunc.mov(rsp(stack_offset), rax);
						stack_offset += 8;
					}
				} else if (pi.type == PassInfo::PassType_Float) {
					if (++floatreg_index >= num_floatreg) {
						m_HookFunc.lea(rax, rbp(OffsetToCallerStack + stack_offset));
						m_HookFunc.mov(rax, rax());
						m_HookFunc.mov(rsp(stack_offset), rax);
						stack_offset += 8;
					}
				} else if (pi.type == PassInfo::PassType_Object) {
					if (pi.flags & PassInfo::PassFlag_ByRef) {
						if (++reg_index >= num_reg) {
							m_HookFunc.lea(rax, rbp(OffsetToCallerStack + stack_offset));
							m_HookFunc.mov(rax, rax());
							m_HookFunc.mov(rsp(stack_offset), rax);
							stack_offset += 8;
						}
					} else {
						if (pi.pAssignOperator || pi.pCopyCtor) {
							// 1st parameter (this)
							m_HookFunc.lea(rdi, rbp(OffsetToCallerStack + stack_offset));
							// 2nd parameter (copy)
							m_HookFunc.lea(rsi, rsp(stack_offset));
							// Move address and call
							m_HookFunc.mov(rax, reinterpret_cast<std::uint64_t>(
								pi.pAssignOperator ? pi.pAssignOperator : pi.pCopyCtor));
							m_HookFunc.call(rax);
						} else {
							// from
							m_HookFunc.lea(rsi, rbp(OffsetToCallerStack + stack_offset));
							// to
							m_HookFunc.lea(rdi, rsp(stack_offset));
							// size
							m_HookFunc.mov(rcx, pi.size);
							// do the copy
							m_HookFunc.rep_movs_bytes();
						}

						stack_offset += pi.size;
					}
				}
			}
#endif

			// From this point on, no matter what. RSP should be aligned on 16 bytes boundary

			// If return value has a constructor, call it
			if ((retInfo.flags & PassInfo::PassFlag_ByVal) && retInfo.pNormalCtor != nullptr)
			{
				std::int32_t v_ret_vals[] = {v_orig_ret, v_override_ret, v_plugin_ret};

				for (int i = 0; i < 3; i++) {
					// First param is this
					MSVC_ONLY(m_HookFunc.lea(rcx, rbp(v_ret_vals[i])));
					GCC_ONLY(m_HookFunc.lea(rdi, rbp(v_ret_vals[i])));

					// We've saved (or not) r8 value, use the freed register to store function ptr
					m_HookFunc.mov(r8, reinterpret_cast<std::uint64_t>(retInfo.pNormalCtor));
					m_HookFunc.call(r8);
				}
			}

			m_HookFunc.mov(rbp(v_status), MRES_IGNORED);
			m_HookFunc.mov(rbp(v_cur_res), MRES_IGNORED);
			m_HookFunc.mov(rbp(v_prev_res), MRES_IGNORED);

			// ********************** SetupHookLoop **********************
			//PrintDebug(m_HookFunc, "Call - SetupHookLoop\n");

			CallSetupHookLoop(v_orig_ret, v_override_ret, v_cur_res, v_prev_res, v_status, v_vfnptr_origentry,
				v_this, v_pContext);

			//PrintDebug(m_HookFunc, "Call - SetupHookLoop - END\n");

			// ********************** call pre hooks **********************
			//PrintDebug(m_HookFunc, "Call - CallHooks [PRE]\n");

			GenerateCallHooks(v_status, v_prev_res, v_cur_res, v_iter, v_pContext, v_plugin_ret, v_mem_ret);

			//PrintDebug(m_HookFunc, "Call - CallHooks [PRE] - END\n");

			// ********************** call orig func **********************
			//PrintDebug(m_HookFunc, "Call - CallOrig\n");

			GenerateCallOrig(v_status, v_pContext, v_this, v_vfnptr_origentry, v_orig_ret, v_override_ret, v_mem_ret);

			//PrintDebug(m_HookFunc, "Call - CallOrig - END\n");

			// ********************** call post hooks **********************
			//PrintDebug(m_HookFunc, "Call - Hooks [POST]\n");

			GenerateCallHooks(v_status, v_prev_res, v_cur_res, v_iter, v_pContext, v_plugin_ret, v_mem_ret);

			//PrintDebug(m_HookFunc, "Call - Hooks [POST] - END\n");

			// ********************** end context and return **********************

			PrepareReturn(v_status, v_pContext, v_ret_ptr);

			CallEndContext(v_pContext);

			// Call destructors of byval object params which have a destructor
			int stack_index = 1; // account this pointer
			if ((retInfo.flags & PassInfo::PassFlag_RetMem) == PassInfo::PassFlag_RetMem) {
				// Non trivial return value
				stack_index++;
			}

			// TODO: Linux will need this to be using offsets into the original stack
			for (int i = 0; i < m_Proto.GetNumOfParams(); ++i, ++stack_index) {
				const IntPassInfo &pi = m_Proto.GetParam(i);
				if (pi.type == PassInfo::PassType_Object && (pi.flags & PassInfo::PassFlag_ODtor) &&
					(pi.flags & PassInfo::PassFlag_ByVal)) {
					// All non-trivial types are passed as a pointer to a special dedicated space
					MSVC_ONLY(m_HookFunc.mov(rcx, rbp(OffsetToCallerStack + stack_index * 8)));
					GCC_ONLY(m_HookFunc.mov(rdi, rbp(OffsetToCallerStack + stack_index * 8)));

					m_HookFunc.mov(rax, reinterpret_cast<std::uint64_t>(pi.pDtor));
					m_HookFunc.call(rax);
				}
			}

			DoReturn(v_ret_ptr, v_memret_ptr);
			// TODO: VERY IMPORTANT! The destructor below can clobber RAX/XMM0!
			//       It should be safe to move DoReturn() right before .leave().
			//       FIGURE IT OUT THOUGH!!!!
			// From then on, rax cannot be used as a general register
			// Use r8 or r9 instead

			// If return value type has a destructor, call it
			if ((retInfo.flags & PassInfo::PassFlag_ByVal) && retInfo.pDtor != nullptr)
			{
				std::int32_t v_ret_vals[] = {v_orig_ret, v_override_ret, v_plugin_ret};

				for (int i = 0; i < 3; i++) {
					// First param is this
					MSVC_ONLY(m_HookFunc.lea(rcx, rbp(v_ret_vals[i])));
					GCC_ONLY(m_HookFunc.lea(rdi, rbp(v_ret_vals[i])));

					m_HookFunc.mov(r8, reinterpret_cast<std::uint64_t>(retInfo.pDtor));
					m_HookFunc.call(r8);
				}
			}

			// Restore RSP and RBP
			// (same as `mov rsp, rbp` + `pop rbp`)
			m_HookFunc.leave();

			m_HookFunc.retn();
			
			// Store pointer for later use
			// m_HookfuncVfnPtr is a pointer to a void* because SH expects a pointer
			// into the hookman's vtable
			*m_HookfuncVfnptr = reinterpret_cast<void*>(m_HookFunc.GetData());

			m_HookFunc.SetRE();

			return m_HookFunc.GetData();
		}

		void x64GenContext::CallSetupHookLoop(int v_orig_ret, int v_override_ret, 
			int v_cur_res, int v_prev_res, int v_status, int v_vfnptr_origentry,
			int v_this, int v_pContext)
		{
			// IHookContext *shptr->SetupHookLoop(IHookManagerInfo *hi, void *vfnptr, void *thisptr, void **origCallAddr, META_RES *statusPtr,
			// META_RES *prevResPtr, META_RES *curResPtr, const void *origRetPtr, void *overrideRetPtr);

			static MemFuncInfo mfi = {false, -1, -1, -1};
			if (mfi.vtblindex == -1)
			{
				GetFuncInfo(&ISourceHook::SetupHookLoop, mfi);
				// The function is somehow not virtual, or has a non trivial this ptr
				if (!mfi.isVirtual || mfi.thisptroffs != 0 || mfi.vtbloffs != 0 || mfi.vtblindex < 0)
				{
					mfi.vtblindex = -1; // Ensure we go through there again on subsequent calls
					SH_ASSERT(0, ("Couldn't retrieve details of ISourceHook::SetupHookLoop!"));
					return;
				}
			}

			// Allocate the necessary stack space
			// (we already allocated enough shadow space for Windows)
			GCC_ONLY(m_HookFunc.sub(rsp, 32));

			// 1st parameter (this)
			GCC_ONLY(m_HookFunc.mov(rdi, reinterpret_cast<std::uintptr_t>(m_SHPtr)));
			MSVC_ONLY(m_HookFunc.mov(rcx, reinterpret_cast<std::uintptr_t>(m_SHPtr)));
			// 2nd parameter - IHookManagerInfo* hi
			GCC_ONLY(m_HookFunc.mov(rsi, reinterpret_cast<std::uintptr_t>(m_pHI)));
			GCC_ONLY(m_HookFunc.mov(rsi, rsi()));
			MSVC_ONLY(m_HookFunc.mov(rdx, reinterpret_cast<std::uintptr_t>(m_pHI)));
			MSVC_ONLY(m_HookFunc.mov(rdx, rdx()));
			// 3rd parameter - void* vfnptr
			GCC_ONLY(m_HookFunc.mov(rdx, rbp(v_this)));
			GCC_ONLY(m_HookFunc.mov(rdx, rdx(m_VtblOffs))); // *(this + m_VtblOffs)
			GCC_ONLY(m_HookFunc.add(rdx, SIZE_PTR * m_VtblIdx)); // vtable + m_VtblIdx

			MSVC_ONLY(m_HookFunc.mov(r8, rbp(v_this)));
			MSVC_ONLY(m_HookFunc.mov(r8, r8(m_VtblOffs))); // *(this + m_VtblOffs)
			MSVC_ONLY(m_HookFunc.add(r8, SIZE_PTR * m_VtblIdx)); // vtable + m_VtblIdx
			// 4th parameter - void* thisptr
			GCC_ONLY(m_HookFunc.mov(rcx, rbp(v_this)));
			MSVC_ONLY(m_HookFunc.mov(r9, rbp(v_this)));
			// 5th argument - void** original call address
			GCC_ONLY(m_HookFunc.lea(r8, rbp(v_vfnptr_origentry)));
			MSVC_ONLY(m_HookFunc.lea(rax, rbp(v_vfnptr_origentry)));
			MSVC_ONLY(m_HookFunc.mov(rsp(0x20), rax));
			// 6th argument - META_RES* statusPtr
			GCC_ONLY(m_HookFunc.lea(r9, rbp(v_status)));
			MSVC_ONLY(m_HookFunc.lea(rax, rbp(v_status)));
			MSVC_ONLY(m_HookFunc.mov(rsp(0x28), rax));
			// 7th argument - META_RES* prevResPtr
			m_HookFunc.lea(rax, rbp(v_prev_res));
			MSVC_ONLY(m_HookFunc.mov(rsp(0x30), rax));
			GCC_ONLY(m_HookFunc.mov(rsp(0x00), rax));
			// 8th argument - META_RES* curResPtr
			m_HookFunc.lea(rax, rbp(v_cur_res));
			MSVC_ONLY(m_HookFunc.mov(rsp(0x38), rax));
			GCC_ONLY(m_HookFunc.mov(rsp(0x08), rax));
			if (m_Proto.GetRet().size == 0) // void return function
			{
				// nullptr
				m_HookFunc.xor_reg(rax, rax);
				// 9th argument - const void* origRetPtr
				MSVC_ONLY(m_HookFunc.mov(rsp(0x40), rax));
				GCC_ONLY(m_HookFunc.mov(rsp(0x10), rax));
				// 10th argument - void* overrideRetPtr
				MSVC_ONLY(m_HookFunc.mov(rsp(0x48), rax));
				GCC_ONLY(m_HookFunc.mov(rsp(0x18), rax));
			}
			else
			{
				// 9th argument - const void* origRetPtr
				m_HookFunc.lea(rax, rbp(v_orig_ret));
				MSVC_ONLY(m_HookFunc.mov(rsp(0x40), rax));
				GCC_ONLY(m_HookFunc.mov(rsp(0x10), rax));
				// 10th argument - void* overrideRetPtr
				m_HookFunc.lea(rax, rbp(v_override_ret));
				MSVC_ONLY(m_HookFunc.mov(rsp(0x48), rax));
				GCC_ONLY(m_HookFunc.mov(rsp(0x18), rax));
			}

			// Retrieve the function address
			m_HookFunc.mov(rax, (*reinterpret_cast<std::uintptr_t**>(m_SHPtr))[mfi.vtblindex]);
			m_HookFunc.call(rax);

			// Store the return value
			m_HookFunc.mov(rbp(v_pContext), rax);

			// Restore the rsp value
			GCC_ONLY(m_HookFunc.add(rsp, 32));
		}

		// Extension of MAKE_DELEG macro
		struct IMyDelegate : ::SourceHook::ISHDelegate { virtual void Call() = 0; };

		void x64GenContext::GenerateCallHooks(int v_status, int v_prev_res, int v_cur_res, int v_iter,
			int v_pContext, int v_plugin_ret, int v_mem_ret)
		{
			static MemFuncInfo getNext = {false, -1, -1, -1};
			if (getNext.vtblindex == -1)
			{
				GetFuncInfo(&IHookContext::GetNext, getNext);
				// The function is somehow not virtual, or has a non trivial this ptr
				if (!getNext.isVirtual || getNext.thisptroffs != 0 || getNext.vtbloffs != 0 || getNext.vtblindex < 0)
				{
					getNext.vtblindex = -1; // Ensure we go through there again on subsequent calls
					SH_ASSERT(0, ("Unexpected compilation of IHookContext::GetNext!"));
					return;
				}
			}

			static MemFuncInfo callMfi = {false, -1, -1, -1};
			if (callMfi.vtblindex == -1)
			{
				GetFuncInfo(&IMyDelegate::Call, callMfi);
				// The function is somehow not virtual, or has a non trivial this ptr
				if (!callMfi.isVirtual || callMfi.thisptroffs != 0 || callMfi.vtbloffs != 0 || callMfi.vtblindex < 0)
				{
					callMfi.vtblindex = -1; // Ensure we go through there again on subsequent calls
					SH_ASSERT(0, ("Unexpected compilation of IMyDelegate::Call!"));
					return;
				}
			}

			static MemFuncInfo getOverrideRetPtrMfi = {false, -1, -1, -1};
			if (getOverrideRetPtrMfi.vtblindex == -1)
			{
				GetFuncInfo(&IHookContext::GetOverrideRetPtr, getOverrideRetPtrMfi);
				// The function is somehow not virtual, or has a non trivial this ptr
				if (!getOverrideRetPtrMfi.isVirtual || getOverrideRetPtrMfi.thisptroffs != 0 || getOverrideRetPtrMfi.vtbloffs != 0 || getOverrideRetPtrMfi.vtblindex < 0)
				{
					getOverrideRetPtrMfi.vtblindex = -1; // Ensure we go through there again on subsequent calls
					SH_ASSERT(0, ("Unexpected compilation of IHookContext::GetOverrideRetPtr!"));
					return;
				}
			}

			//prev_res = MRES_IGNORED;
			//while ( (iter = static_cast<IMyDelegate*>(pContext->GetNext())) )
			//{
			//	cur_res = MRES_IGNORED;
			//	plugin_ret = iter->Call params;
			//	prev_res = cur_res;
			//	if (cur_res > status)
			//		status = cur_res;
			//	if (cur_res >= MRES_OVERRIDE)
			//		*reinterpret_cast<my_rettype*>(pContext->GetOverrideRetPtr()) = plugin_ret;
			//}

			// prev_res = MRES_IGNORED;
			//m_HookFunc.breakpoint();

			m_HookFunc.mov(rbp(v_prev_res), MRES_IGNORED);

			auto startLoop = m_HookFunc.get_outputpos();
			// while ( (iter = static_cast<IMyDelegate*>(pContext->GetNext())) )
			m_HookFunc.mov(rax, rbp(v_pContext));
			m_HookFunc.mov(rax, rax()); // *this (vtable)
			m_HookFunc.mov(rax, rax(getNext.vtblindex * SIZE_PTR)); // vtable[vtblindex]

			GCC_ONLY(m_HookFunc.mov(rdi, rbp(v_pContext)));
			MSVC_ONLY(m_HookFunc.mov(rcx, rbp(v_pContext)));
			m_HookFunc.call(rax); // pContext->GetNext()

			// store into iter
			m_HookFunc.mov(rbp(v_iter), rax);

			// null check iter
			m_HookFunc.test(rax, rax);
			m_HookFunc.jz(0x0); // Leave loop if nullptr
			std::int32_t jumpOff = m_HookFunc.get_outputpos();

			// cur_res = MRES_IGNORED;
			m_HookFunc.mov(rbp(v_cur_res), MRES_IGNORED);

			// prev_res = cur_res;
			m_HookFunc.mov(rax, rbp(v_cur_res));
			m_HookFunc.mov(rbp(v_prev_res), rax);

			// call
			std::int32_t stackSpace = PushParameters(v_iter, MemRetWithTempObj() ? v_mem_ret : v_plugin_ret);
			m_HookFunc.mov(rax, rbp(v_iter));
			m_HookFunc.mov(rax, rax()); // *this (vtable)
			m_HookFunc.mov(rax, rax(callMfi.vtblindex * SIZE_PTR)); // vtable[vtblindex] iter -> Call
			m_HookFunc.call(rax);
			// epilog free the stack
			m_HookFunc.add(rsp, stackSpace);

			SaveReturnValue(v_mem_ret, v_plugin_ret);

			// if (cur_res > status)
			m_HookFunc.mov(rax, rbp(v_cur_res));
			m_HookFunc.cmp(rax, rbp(v_status));
			// status = cur_res;
			m_HookFunc.mov(rax, rbp(v_status));
			m_HookFunc.cmovg(rax, rbp(v_cur_res));
			m_HookFunc.mov(rbp(v_status), rax);

			// Are we dealing with a non void function ?
			auto retInfo = m_Proto.GetRet();
			if (retInfo.size != 0)
			{
				// if (cur_res >= MRES_OVERRIDE)
				m_HookFunc.mov(rax, MRES_OVERRIDE);
				m_HookFunc.cmp(rbp(v_cur_res), rax);

				m_HookFunc.jl(0x0);
				std::int32_t earlyLoopBack = m_HookFunc.get_outputpos() - startLoop;
				m_HookFunc.rewrite<std::int32_t>(m_HookFunc.get_outputpos() - sizeof(std::int32_t), -earlyLoopBack);

				m_HookFunc.mov(rax, rbp(v_pContext));
				m_HookFunc.mov(rax, rax()); // *this (vtable)
				m_HookFunc.mov(rax, rax(getOverrideRetPtrMfi.vtblindex * SIZE_PTR)); // vtable[vtblindex]

				GCC_ONLY(m_HookFunc.mov(rdi, rbp(v_pContext)));
				MSVC_ONLY(m_HookFunc.mov(rcx, rbp(v_pContext)));
				m_HookFunc.call(rax); // pContext->GetOverrideRetPtr()

				// *reinterpret_cast<my_rettype*>(pContext->GetOverrideRetPtr()) = plugin_ret;

				// byref is always a pointer underneath
				if (retInfo.flags & PassInfo::PassFlag_ByRef)
				{
					m_HookFunc.mov(r8, rbp(v_plugin_ret));
					m_HookFunc.mov(rax(), r8);
				}
				else
				{
					// custom assignment operator, so call it
					if (retInfo.pAssignOperator)
					{
						// 1st parameter (this)
						GCC_ONLY(m_HookFunc.mov(rdi, rax));
						MSVC_ONLY(m_HookFunc.mov(rcx, rax));

						// 2nd parameter (copy)
						GCC_ONLY(m_HookFunc.lea(rsi, rbp(v_plugin_ret)));
						MSVC_ONLY(m_HookFunc.lea(rdx, rbp(v_plugin_ret)));

						// Move address and call
						m_HookFunc.mov(rax, reinterpret_cast<std::uint64_t>(retInfo.pAssignOperator));
						m_HookFunc.call(rax);
					}
					else
					{
						m_HookFunc.push(rdi);
						m_HookFunc.push(rsi);
						m_HookFunc.push(rcx);

						m_HookFunc.mov(rcx, retInfo.size);
						m_HookFunc.mov(rdi, rax);
						m_HookFunc.lea(rsi, rbp(v_plugin_ret));

						m_HookFunc.rep_movs_bytes();

						m_HookFunc.pop(rcx);
						m_HookFunc.pop(rsi);
						m_HookFunc.pop(rdi);
					}
				}
			}

			m_HookFunc.jump(0x0);
			std::int32_t loopBack = m_HookFunc.get_outputpos() - startLoop;
			m_HookFunc.rewrite<std::int32_t>(m_HookFunc.get_outputpos() - sizeof(std::int32_t), -loopBack);

			m_HookFunc.rewrite<std::int32_t>(jumpOff - sizeof(std::int32_t), m_HookFunc.get_outputpos() - jumpOff);
		}

		void x64GenContext::GenerateCallOrig(int v_status, int v_pContext, int v_this, int v_vfnptr_origentry, int v_orig_ret, int v_override_ret, int v_place_for_memret)
		{
			static MemFuncInfo shouldCallOrigMfi = {false, -1, -1, -1};
			if (shouldCallOrigMfi.vtblindex == -1)
			{
				GetFuncInfo(&IHookContext::ShouldCallOrig, shouldCallOrigMfi);
				// The function is somehow not virtual, or has a non trivial this ptr
				if (!shouldCallOrigMfi.isVirtual || shouldCallOrigMfi.thisptroffs != 0 || shouldCallOrigMfi.vtbloffs != 0 || shouldCallOrigMfi.vtblindex < 0)
				{
					shouldCallOrigMfi.vtblindex = -1; // Ensure we go through there again on subsequent calls
					SH_ASSERT(0, ("Unexpected compilation of IHookContext::ShouldCallOrig!"));
					return;
				}
			}

			//if (status != MRES_SUPERCEDE && pContext->ShouldCallOrig())
			//{
			//	rettype (EmptyClass::*mfp)paramtypes;
			//	SH_SETUP_MFP(mfp);
			//	orig_ret = (reinterpret_cast<EmptyClass*>(this)->*mfp)params;
			//}
			//else
			//	orig_ret = override_ret;
			//m_HookFunc.breakpoint();

			m_HookFunc.mov(rax, rbp(v_status));
			m_HookFunc.cmp(rax, MRES_SUPERCEDE);
			m_HookFunc.je(0x0);
			auto statusCmpOff = m_HookFunc.get_outputpos();

			m_HookFunc.mov(rax, rbp(v_pContext));

			// 1st parameter (this)
			GCC_ONLY(m_HookFunc.mov(rdi, rax));
			MSVC_ONLY(m_HookFunc.mov(rcx, rax));

			m_HookFunc.mov(rax, rax());
			m_HookFunc.mov(rax, rax(SIZE_PTR * shouldCallOrigMfi.vtblindex));

			m_HookFunc.call(rax); // pContext->ShouldCallOrig()

			// Don't have the lower register yet, so this will do for now
			m_HookFunc.test(rax, 0x1);
			m_HookFunc.jz(0x0);
			auto shouldCallOff = m_HookFunc.get_outputpos();

			// original call
			std::int32_t stackSpace = PushParameters(v_this, MemRetWithTempObj() ? v_place_for_memret : v_orig_ret);
			m_HookFunc.mov(rax, rbp(v_vfnptr_origentry));
			m_HookFunc.call(rax);
			// epilog free the stack
			m_HookFunc.add(rsp, stackSpace);

			SaveReturnValue(v_place_for_memret, v_orig_ret);

			m_HookFunc.jump(0x0);
			auto callOriginalOff = m_HookFunc.get_outputpos();

			// else
			auto elseStartOff = m_HookFunc.get_outputpos();
			m_HookFunc.rewrite(statusCmpOff - sizeof(std::int32_t), static_cast<std::int32_t>(elseStartOff - statusCmpOff));
			m_HookFunc.rewrite(shouldCallOff - sizeof(std::int32_t), static_cast<std::int32_t>(elseStartOff - shouldCallOff));

			auto retInfo = m_Proto.GetRet();
			if (retInfo.size != 0)
			{
				if (retInfo.flags & PassInfo::PassFlag_ByRef)
				{
					m_HookFunc.mov(rax, rbp(v_override_ret));
					m_HookFunc.mov(rbp(v_orig_ret), rax);
				}
				else
				{
					// custom assignment operator, so call it
					if (retInfo.pAssignOperator)
					{
						// 1st parameter (this)
						GCC_ONLY(m_HookFunc.lea(rdi, rbp(v_orig_ret)));
						MSVC_ONLY(m_HookFunc.lea(rcx, rbp(v_orig_ret)));

						// 2nd parameter (copy)
						GCC_ONLY(m_HookFunc.lea(rsi, rbp(v_override_ret)));
						MSVC_ONLY(m_HookFunc.lea(rdx, rbp(v_override_ret)));

						// Move address and call
						m_HookFunc.mov(rax, reinterpret_cast<std::uint64_t>(retInfo.pAssignOperator));
						m_HookFunc.call(rax);
					}
					else
					{
						m_HookFunc.push(rdi);
						m_HookFunc.push(rsi);
						m_HookFunc.push(rcx);

						m_HookFunc.mov(rcx, retInfo.size);
						m_HookFunc.lea(rdi, rbp(v_orig_ret));
						m_HookFunc.lea(rsi, rbp(v_override_ret));

						m_HookFunc.rep_movs_bytes();

						m_HookFunc.pop(rcx);
						m_HookFunc.pop(rsi);
						m_HookFunc.pop(rdi);
					}
				}
			}

			m_HookFunc.rewrite(callOriginalOff - sizeof(std::int32_t), static_cast<std::int32_t>(m_HookFunc.get_outputpos() - callOriginalOff));
		}

		std::int32_t x64GenContext::PushParameters(int v_this, int v_ret)
		{
			auto retInfo = m_Proto.GetRet();
			std::int32_t stackSpace = 0;

#if SH_COMP == SH_COMP_MSVC
			const x86_64_Reg params_reg[] = { rcx, rdx, r8, r9 };
			const x86_64_FloatReg params_floatreg[] = { xmm0, xmm1, xmm2, xmm3 };

			int reg_index = 0;

			// setup this parameter
			m_HookFunc.mov(params_reg[reg_index], rbp(v_this));
			reg_index++;

			// Non standard return
			if (retInfo.size != 0 && (retInfo.flags & PassInfo::PassFlag_RetMem) == PassInfo::PassFlag_RetMem) {
				m_HookFunc.lea(params_reg[reg_index], rbp(v_ret));
				reg_index++;
			}

			// We've backed up the parameters into the shadow space
			int parameter_index = 0;
			for (; parameter_index < m_Proto.GetNumOfParams() && reg_index < 4; reg_index++, parameter_index++) {
				auto& info = m_Proto.GetParam(parameter_index);

				if (info.type == PassInfo::PassType_Float && (info.flags & PassInfo::PassFlag_ByRef) != PassInfo::PassFlag_ByRef) {
					m_HookFunc.movsd(params_floatreg[reg_index], rbp(reg_index * 8 + 8));
				} else {
					m_HookFunc.mov(params_reg[reg_index], rbp(reg_index * 8 + 8));
				}
			}

			// Allocate the shadow space
			stackSpace += 32;
			int parameters_on_stack = m_Proto.GetNumOfParams() - parameter_index;
			stackSpace += parameters_on_stack * 8;
			// And it needs to be 16-byte aligned...
			m_HookFunc.sub(rsp, AlignSize(stackSpace, 16));

			for (int i = 0; parameter_index < m_Proto.GetNumOfParams(); parameter_index++, i++) {
				m_HookFunc.mov(rax, rbp(OffsetToCallerStack + (8 * 4) + (8 * i))); // We need to skip the shadow space
				m_HookFunc.mov(rsp(32 + (8 * i)), rax);
			}

			return stackSpace;
#else
			// TODO: Fix up this shit for objects passed inline on the stack

			const x86_64_Reg params_reg[] = { rdi, rsi, rdx, rcx, r8, r9 };
			const x86_64_FloatReg params_floatreg[] = { xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7 };
			const std::uint8_t num_reg = sizeof(params_reg) / sizeof(params_reg[0]);
			const std::uint8_t num_floatreg = sizeof(params_floatreg) / sizeof(params_floatreg[0]);

			int reg_index = 0;
			int floatreg_index = 0;

			// TODO: What am I doing here?
			// Non standard return
			if (retInfo.size != 0 && (retInfo.flags & PassInfo::PassFlag_RetMem) == PassInfo::PassFlag_RetMem) {
				m_HookFunc.lea(params_reg[reg_index], rbp(v_ret));
				reg_index++;
			}

			// setup this parameter
			m_HookFunc.mov(params_reg[reg_index], rbp(v_this));
			reg_index++;

			// TODO: Doesn't handle custom_register at all........

			int parameters_on_stack = 0;

			// Pass to calculate stack space...
			for (int i = 0, tmp_reg_index = reg_index, tmp_floatreg_index = floatreg_index; i < m_Proto.GetNumOfParams(); i++) {
				auto& info = m_Proto.GetParam(i);
				if (info.type == PassInfo::PassType_Float && (info.flags & PassInfo::PassFlag_ByRef) != PassInfo::PassFlag_ByRef) {
					if (++tmp_floatreg_index >= num_floatreg) {
						parameters_on_stack++;
					}
				} else {
					if (++tmp_reg_index >= num_reg) {
						parameters_on_stack++;
					}
				}
			}

			stackSpace = AlignSize(parameters_on_stack * 8, 16);
			m_HookFunc.sub(rsp, stackSpace);

			// Actually push registers to stack...
			for (int i = 0, pushed_stack_parameters = 0; i < m_Proto.GetNumOfParams(); i++) {
				auto& info = m_Proto.GetParam(i);
				// TODO: These offsets are WRONG
				if (info.type == PassInfo::PassType_Float && (info.flags & PassInfo::PassFlag_ByRef) != PassInfo::PassFlag_ByRef) {
					if (++floatreg_index >= num_floatreg) {
						m_HookFunc.mov(rax, rbp(OffsetToCallerStack + (8 * pushed_stack_parameters)));
						m_HookFunc.mov(rsp(0 + (8 * pushed_stack_parameters)), rax);
						pushed_stack_parameters++;
					}
				} else {
					if (++reg_index >= num_reg) {
						m_HookFunc.mov(rax, rbp(OffsetToCallerStack + (8 * pushed_stack_parameters)));
						m_HookFunc.mov(rsp(0 + (8 * pushed_stack_parameters)), rax);
						pushed_stack_parameters++;
					}
				}
			}

			return stackSpace;
#endif
		}

		void x64GenContext::SaveReturnValue(int v_mem_ret, int v_ret) {
			const auto& retInfo = m_Proto.GetRet();
			// void, early return
			if (retInfo.size == 0) {
				return;
			}

			if ((retInfo.flags & PassInfo::PassFlag_ByRef) == PassInfo::PassFlag_ByRef) {
				m_HookFunc.mov(rbp(v_ret), rax);
				return;
			}

			// TOOD: handle Vector3f into XMM0 & XMM1 for Linux64 here....

			// ByVal
			if (retInfo.type == PassInfo::PassType_Float) {
				m_HookFunc.movsd(rbp(v_ret), xmm0);
			} else if (retInfo.type == PassInfo::PassType_Basic) {
				m_HookFunc.mov(rbp(v_ret), rax);
			} else if ((retInfo.flags & PassInfo::PassFlag_RetMem) == PassInfo::PassFlag_RetMem) {
				if (MemRetWithTempObj()) {
					if (retInfo.pAssignOperator) {
						// 1st parameter (this)
						GCC_ONLY(m_HookFunc.lea(rdi, rbp(v_ret)));
						MSVC_ONLY(m_HookFunc.lea(rcx, rbp(v_ret)));

						// 2nd parameter (copy)
						GCC_ONLY(m_HookFunc.lea(rsi, rbp(v_mem_ret)));
						MSVC_ONLY(m_HookFunc.lea(rdx, rbp(v_mem_ret)));

						// Move address and call
						m_HookFunc.mov(rax, reinterpret_cast<std::uint64_t>(retInfo.pAssignOperator));
						m_HookFunc.call(rax);
					}
					else {
						m_HookFunc.push(rdi);
						m_HookFunc.push(rsi);
						m_HookFunc.push(rcx);

						m_HookFunc.mov(rcx, retInfo.size);
						m_HookFunc.lea(rdi, rbp(v_ret));
						m_HookFunc.lea(rsi, rbp(v_mem_ret));

						m_HookFunc.rep_movs_bytes();

						m_HookFunc.pop(rcx);
						m_HookFunc.pop(rsi);
						m_HookFunc.pop(rdi);
					}

					if (retInfo.pDtor) {
						// 1st parameter (this)
						GCC_ONLY(m_HookFunc.lea(rdi, rbp(v_mem_ret)));
						MSVC_ONLY(m_HookFunc.lea(rcx, rbp(v_mem_ret)));

						// Move address and call
						m_HookFunc.mov(rax, reinterpret_cast<std::uint64_t>(retInfo.pDtor));
						m_HookFunc.call(rax);
					}

				} else {
					// Already copied in the proper location
					return;
				}
			} else {
				SH_ASSERT(0, ("Unknown handling of return type!"));
				return;
			}
		}

		void x64GenContext::PrepareReturn(int v_status, int v_pContext, int v_retptr)
		{
			const auto& retInfo = m_Proto.GetRet();
			if (retInfo.size == 0) {
				return;
			}

			static MemFuncInfo getOverrideRetPtrMfi = {false, -1, -1, -1};
			if (getOverrideRetPtrMfi.vtblindex == -1) {
				GetFuncInfo(&IHookContext::GetOverrideRetPtr, getOverrideRetPtrMfi);
				if (!getOverrideRetPtrMfi.isVirtual || getOverrideRetPtrMfi.thisptroffs != 0 || getOverrideRetPtrMfi.vtbloffs != 0 || getOverrideRetPtrMfi.vtblindex < 0) {
					getOverrideRetPtrMfi.vtblindex = -1;
					SH_ASSERT(0, ("Unexpected compilation of IHookContext::GetOverrideRetPtr!"));
					return;
				}
			}

			static MemFuncInfo getOrigRetPtrMfi = {false, -1, -1, -1};
			if (getOrigRetPtrMfi.vtblindex == -1) {
				GetFuncInfo(&IHookContext::GetOrigRetPtr, getOrigRetPtrMfi);
				if (!getOrigRetPtrMfi.isVirtual || getOrigRetPtrMfi.thisptroffs != 0 || getOrigRetPtrMfi.vtbloffs != 0 || getOrigRetPtrMfi.vtblindex < 0) {
					getOrigRetPtrMfi.vtblindex = -1;
					SH_ASSERT(0, ("Unexpected compilation of IHookContext::GetOverrideRetPtr!"));
					return;
				}
			}

			//const my_rettype *retptr = reinterpret_cast<const my_rettype*>(
			//(status >= MRES_OVERRIDE) ? pContext->GetOverrideRetPtr() : pContext->GetOrigRetPtr());

			m_HookFunc.mov(rax, rbp(v_pContext));
			m_HookFunc.mov(rax, rax());
			m_HookFunc.mov(r8, rax);

			m_HookFunc.mov(rax, rax(getOrigRetPtrMfi.vtblindex * SIZE_PTR));
			m_HookFunc.mov(r8, r8(getOverrideRetPtrMfi.vtblindex * SIZE_PTR));

			m_HookFunc.mov(r9, rbp(v_status));
			m_HookFunc.cmp(r9, MRES_OVERRIDE);

			m_HookFunc.cmovge(rax, r8);

			// 1st parameter (this)
			GCC_ONLY(m_HookFunc.mov(rdi, rbp(v_pContext)));
			MSVC_ONLY(m_HookFunc.mov(rcx, rbp(v_pContext)));

			m_HookFunc.call(rax);

			m_HookFunc.mov(rbp(v_retptr), rax);
		}

		void x64GenContext::DoReturn(int v_retptr, int v_memret_outaddr)
		{
			const auto& retInfo = m_Proto.GetRet();
			if (retInfo.size == 0) {
				return;
			}

			m_HookFunc.mov(r8, rbp(v_retptr));

			// TOOD: handle Vector3f into XMM0 & XMM1 for Linux64 here....

			if (retInfo.flags & PassInfo::PassFlag_ByRef) {
				m_HookFunc.mov(rax, r8());
				return;
			}
			// else: byval

			if (retInfo.type == PassInfo::PassType_Float) {
				m_HookFunc.movsd(xmm0, r8());
			}
			else if (retInfo.type == PassInfo::PassType_Basic || 
				((retInfo.type == PassInfo::PassType_Object) && (retInfo.flags & PassInfo::PassFlag_RetReg)) ) {
				m_HookFunc.mov(rax, r8());
			}

			if (retInfo.flags & PassInfo::PassFlag_RetMem)
			{
				// *memret_outaddr = plugin_ret
				if (retInfo.pCopyCtor)
				{
					// 1st parameter (this)
					GCC_ONLY(m_HookFunc.mov(rdi, rbp(v_memret_outaddr)));
					MSVC_ONLY(m_HookFunc.mov(rcx, rbp(v_memret_outaddr)));

					// 2nd parameter (copy)
					GCC_ONLY(m_HookFunc.mov(rsi, r8));
					MSVC_ONLY(m_HookFunc.mov(rdx, r8));

					// Move address and call
					m_HookFunc.mov(rax, reinterpret_cast<std::uint64_t>(retInfo.pCopyCtor));
					m_HookFunc.call(rax);
				}
				else
				{
					m_HookFunc.push(rdi);
					m_HookFunc.push(rsi);
					m_HookFunc.push(rcx);

					m_HookFunc.mov(rcx, retInfo.size);
					m_HookFunc.mov(rdi, rbp(v_memret_outaddr));
					m_HookFunc.mov(rsi, r8);

					m_HookFunc.rep_movs_bytes();

					m_HookFunc.pop(rcx);
					m_HookFunc.pop(rsi);
					m_HookFunc.pop(rdi);
				}
				m_HookFunc.mov(rax, rbp(v_memret_outaddr));
			}
		}

		void x64GenContext::CallEndContext(int v_pContext) {
			static MemFuncInfo mfi = {false, -1, -1, -1};
			if (mfi.vtblindex == -1)
			{
				GetFuncInfo(&ISourceHook::EndContext, mfi);
				// The function is somehow not virtual, or has a non trivial this ptr
				if (!mfi.isVirtual || mfi.thisptroffs != 0 || mfi.vtbloffs != 0 || mfi.vtblindex < 0)
				{
					mfi.vtblindex = -1; // Ensure we go through there again on subsequent calls
					SH_ASSERT(0, ("Couldn't retrieve details of ISourceHook::EndContext!"));
					return;
				}
			}

			// 1st parameter (this)
			GCC_ONLY(m_HookFunc.mov(rdi, reinterpret_cast<std::uintptr_t>(m_SHPtr)));
			MSVC_ONLY(m_HookFunc.mov(rcx, reinterpret_cast<std::uintptr_t>(m_SHPtr)));

			// 2nd param
			GCC_ONLY(m_HookFunc.mov(rsi, rbp(v_pContext)));
			MSVC_ONLY(m_HookFunc.mov(rdx, rbp(v_pContext)));

			// Move address and call
			m_HookFunc.mov(rax, (*reinterpret_cast<std::uintptr_t**>(m_SHPtr))[mfi.vtblindex]);
			m_HookFunc.call(rax);
		}

		bool x64GenContext::MemRetWithTempObj() {
			const auto& retInfo = m_Proto.GetRet();
			// Memory return AND (has destructor OR has assign operator)
			return ((retInfo.flags & PassInfo::PassFlag_RetMem)
				&& (retInfo.flags & (PassInfo::PassFlag_ODtor | PassInfo::PassFlag_AssignOp)));
		}
		
		bool x64GenContext::AutoDetectRetType() {
			auto& pi = m_Proto.GetRet();
			// Void return, ignore
			if (pi.size == 0) {
				return true;
			}

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
						// MSVC has various criteria for passing in memory
						// if object doesn't fit on 8, 16, 32, or 64 bits. It's in memory
						// if object has a constructor or destructor. It's in memory
						bool unconventionalsize = (pi.size == 3 || (pi.size != 8 && pi.size > 4));
						bool hasSpecialFunctions = (pi.flags & (PassInfo::PassFlag_OCtor|PassInfo::PassFlag_ODtor|PassInfo::PassFlag_CCtor)) != 0;

						if (unconventionalsize || hasSpecialFunctions) {
							pi.flags |= PassInfo::PassFlag_RetMem;
						} else {
							pi.flags |= PassInfo::PassFlag_RetReg;
						}
#elif SH_COMP == SH_COMP_GCC
						// "If the size of an object is larger than eight eightbytes, or it contains unaligned fields, it has class MEMORY".
						//
						// "If a C++ object is non-trivial for the purpose of calls, as specified in the C++ ABI[16], it is passed by invisible reference (the object is replaced in the parameter list by a pointer that has class INTEGER)[17]."
						// "[17]An object whose type is non-trivial for the purpose of calls cannot be passed by value because such objects must have the same address in the caller and the callee. Similar issues apply when returning an object from a function."
						//
						// source: System V AMD64 psABI section 3.2.3 Parameter Passing
						// https://gitlab.com/x86-psABIs/x86-64-ABI/-/jobs/artifacts/master/raw/x86-64-ABI/abi.pdf?job=build)
						//
						// "A type is considered non-trivial for the purposes of call if:
						// - it has a non-trivial copy constructor, move constructor, or destructor, or
						// - all of its copy and move constructors are deleted."
						// source: https://itanium-cxx-abi.github.io/cxx-abi/abi.html (yes, System V copied this definition from Itanium...)
						//
						//
						//   typedef struct __attribute__((packed)) { char a; int b; } thing;
						//   = memory (5 bytes)
						//
						//   typedef struct __attribute__((packed)) { int a; char b; } thing;
						//   = register (5 bytes)
						//
						//   typedef struct __attribute__((packed)) { char a; short b; char c; } thing;
						//   = memory (6 bytes)
						//
						//   typedef struct __attribute__((packed)) { char a; short b; int c; char d; } thing;
						//   = memory (8 bytes)
						//
						//
						// Result: we cannot detect if it should be register or memory without knowing the layout of the object.

						bool tooBig = (pi.size > (8 * 8));
						bool hasSpecialFunctions = (pi.flags & (PassInfo::PassFlag_ODtor|PassInfo::PassFlag_CCtor)) != 0;

						bool probablyVector = (pi.size == 12);

						if (hasSpecialFunctions || tooBig || probablyVector)
						{
							pi.flags |= PassInfo::PassFlag_RetMem;
							return true;
						}
						else
						{
							return false;
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
			return true;
		}

		bool x64GenContext::AutoDetectParamFlags()
		{
		}

		void* x64GenContext::GeneratePubFunc()
		{
			// The pubfunc is a static cdecl function.
			// C Code:
			//  int HookManPubFunc(
			//     bool store,				// rdi (AMD) rcx (microsoft)
			//     IHookManagerInfo *hi		// rsi (AMD) rdx (microsoft)
			//     )
			//  {
			//    if (store)
			//      *m_pHI = hi;
			//    if (hi)
			//      hi->SetInfo(HOOKMAN_VERSION, m_VtblOffs, m_VtblIdx, m_Proto.GetProto(), m_HookfuncVfnptr)
			//  }
			
			// Save our frame pointer. (somewhat needlessly on Windows...)
			// This also realigns the stack to 16 bytes.
			m_PubFunc.push(rbp);
			m_PubFunc.mov(rbp, rsp);

			// prologue
			MSVC_ONLY(m_PubFunc.sub(rsp, 0x30)); // Shadow space 32 bytes + 2 * 8 bytes (for our parameters)

			// Both Microsoft and AMD uses r8 and r9 as argument parameters
			// Therefore they need not to be preserved across function calls
			// Let's use them as local variables, this will make writing the
			// rest of the function much easier

			// Store 'store' into r8
			GCC_ONLY(m_PubFunc.mov(r8, rdi));
			MSVC_ONLY(m_PubFunc.mov(r8, rcx));

			// Store 'hi' into r9
			GCC_ONLY(m_PubFunc.mov(r9, rsi));
			MSVC_ONLY(m_PubFunc.mov(r9, rdx));

			// If 'store' is true, store hi into rax
			m_PubFunc.test(r8, 0x1);
			m_PubFunc.jz(0x0);
			
			auto storeOff = m_PubFunc.get_outputpos();
			m_PubFunc.mov(rax, reinterpret_cast<std::uint64_t>(m_pHI));
			m_PubFunc.mov(rax(), r9);

			m_PubFunc.rewrite<std::int32_t>(storeOff - sizeof(std::int32_t), m_PubFunc.get_outputpos() - storeOff);

			// If 'hi' is not null, call SetInfo
			m_PubFunc.test(r9, r9);
			m_PubFunc.jz(0x0); // We will write the real offset later
			auto jumpOff = m_PubFunc.get_outputpos();

			static MemFuncInfo mfi = {false, -1, -1, -1};
			if (mfi.vtblindex == -1)
			{
				GetFuncInfo(&IHookManagerInfo::SetInfo, mfi);
				// The function is somehow not virtual, or has a non trivial this ptr
				if (!mfi.isVirtual || mfi.thisptroffs != 0 || mfi.vtbloffs != 0 || mfi.vtblindex < 0)
				{
					mfi.vtblindex = -1; // Ensure we go through there again on subsequent calls
					return nullptr;
				}
			}

			// Obtain the vtable
			m_PubFunc.mov(rax, r9());
			m_PubFunc.mov(rax, rax(SIZE_PTR * mfi.vtblindex));

			// 1st parameter (this)
			GCC_ONLY(m_PubFunc.mov(rdi, r9));
			MSVC_ONLY(m_PubFunc.mov(rcx, r9));
			// 2nd parameter
			GCC_ONLY(m_PubFunc.mov(rsi, SH_HOOKMAN_VERSION));
			MSVC_ONLY(m_PubFunc.mov(rdx, SH_HOOKMAN_VERSION));
			// 3rd parameter
			GCC_ONLY(m_PubFunc.mov(rdx, m_VtblOffs));
			MSVC_ONLY(m_PubFunc.mov(r8, m_VtblOffs));
			// 4th parameter
			GCC_ONLY(m_PubFunc.mov(rcx, m_VtblIdx));
			MSVC_ONLY(m_PubFunc.mov(r9, m_VtblIdx));
			// 5th argument
			GCC_ONLY(m_PubFunc.mov(r8, reinterpret_cast<std::uint64_t>(m_BuiltPI)));
			MSVC_ONLY(m_PubFunc.mov(r10, reinterpret_cast<std::uint64_t>(m_BuiltPI)));
			MSVC_ONLY(m_PubFunc.mov(rsp(0x20), r10));
			// 6th argument
			GCC_ONLY(m_PubFunc.mov(r9, reinterpret_cast<std::uint64_t>(m_HookfuncVfnptr)));
			MSVC_ONLY(m_PubFunc.mov(r10, reinterpret_cast<std::uint64_t>(m_HookfuncVfnptr)));
			MSVC_ONLY(m_PubFunc.mov(rsp(0x28), r10));

			m_PubFunc.call(rax);

			// Now that we've written the conditional branch
			// we can move set the offset at our earlier jump
			std::int32_t endOff = static_cast<std::int32_t>(m_PubFunc.get_outputpos()) - jumpOff;
			m_PubFunc.rewrite<std::int32_t>(jumpOff - sizeof(std::int32_t), endOff);

			// epilogue
			// Restore RSP and RBP
			// (same as `mov rsp, rbp` + `pop rbp`)
			m_PubFunc.leave();

			// Return 0
			m_PubFunc.xor_reg(rax, rax);

			m_PubFunc.retn();

			m_PubFunc.SetRE();
			return m_PubFunc;
		}

		HookManagerPubFunc x64GenContext::GetPubFunc()
		{
			if (m_GeneratedPubFunc == nullptr)
			{
				// Try generating the function
				m_GeneratedPubFunc = Generate();
			}
			return m_GeneratedPubFunc;
		}

		bool x64GenContext::Equal(const CProto &proto, int vtbl_offs, int vtbl_idx)
		{
			return (m_OrigProto.ExactlyEqual(proto) && m_VtblOffs == vtbl_offs && m_VtblIdx == vtbl_idx);
		}

		bool x64GenContext::Equal(HookManagerPubFunc other)
		{
			return m_GeneratedPubFunc == other;
		}
	}
}
