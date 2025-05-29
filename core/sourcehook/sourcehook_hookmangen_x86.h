/* ======== SourceHook ========
* vim: set ts=4 :
* Copyright (C) 2004-2010 AlliedModders LLC.  All rights reserved.
* No warranties of any kind
*
* License: zlib/libpng
*
* from the SourcePawn JIT SDK
* ============================
*/

#ifndef __SOURCEHOOK_HOOKMANGEN_X86_H__
#define __SOURCEHOOK_HOOKMANGEN_X86_H__

#include <climits>

#include "sourcehook_hookmangen.h"
#include "sh_asm.h"

#undef REG_EAX
#undef REG_ECX
#undef REG_EDX
#undef REG_EBX
#undef REG_ESP
#undef REG_SIB
#undef REG_EBP
#undef REG_ESI
#undef REG_EDI

//MOD R/M
#define MOD_MEM_REG	0
#define MOD_DISP8	1
#define MOD_DISP32	2
#define MOD_REG		3

//SIB
#define NOSCALE		0
#define	SCALE2		1
#define	SCALE4		2
#define SCALE8		3

//Register codes
#define REG_EAX			0
#define REG_ECX			1
#define REG_EDX			2
#define REG_EBX			3
#define	REG_ESP			4
#define	REG_SIB			4
#define REG_NOIDX		4
#define REG_IMM_BASE	5
#define REG_EBP			5
#define REG_ESI			6
#define REG_EDI			7

#define IA32_16BIT_PREFIX	0x66

//condition codes (for example, Jcc opcodes)
#define CC_B	0x2
#define CC_NAE	CC_B
#define CC_NB	0x3
#define CC_AE	CC_NB
#define CC_E	0x4
#define CC_Z	CC_E
#define CC_NE	0x5
#define CC_NZ	CC_NE
#define CC_NA	0x6
#define CC_BE	CC_NA
#define CC_A	0x7
#define CC_NBE	CC_A
#define CC_L	0xC
#define CC_NGE	CC_L
#define CC_NL	0xD
#define CC_GE	CC_NL
#define CC_NG	0xE
#define CC_LE	CC_NG
#define CC_G	0xF
#define CC_NLE	CC_G

//Opcodes with encoding information
#define IA32_XOR_RM_REG			0x31	// encoding is /r
#define IA32_XOR_REG_RM			0x33	// encoding is /r
#define IA32_XOR_EAX_IMM32		0x35	// encoding is /r
#define IA32_XOR_RM_IMM32		0x81	// encoding is /6
#define IA32_XOR_RM_IMM8		0x83	// encoding is /6
#define IA32_ADD_RM_REG			0x01	// encoding is /r
#define IA32_ADD_REG_RM			0x03	// encoding is /r
#define IA32_ADD_RM_IMM32		0x81	// encoding is /0
#define IA32_ADD_RM_IMM8		0x83	// encoding is /0
#define IA32_ADD_EAX_IMM32		0x05	// no extra encoding
#define IA32_SUB_RM_REG			0x29	// encoding is /r
#define IA32_SUB_REG_RM			0x2B	// encoding is /r
#define IA32_SUB_RM_IMM8		0x83	// encoding is /5 <imm8>
#define IA32_SUB_RM_IMM32		0x81	// encoding is /5 <imm32>
#define IA32_SBB_REG_RM			0x1B	// encoding is /r
#define IA32_SBB_RM_IMM8		0x83	// encoding is <imm32>
#define IA32_JMP_IMM32			0xE9	// encoding is imm32
#define IA32_JMP_IMM8			0xEB	// encoding is imm8
#define IA32_JMP_RM				0xFF	// encoding is /4
#define IA32_CALL_IMM32			0xE8	// relative call, <imm32>
#define IA32_CALL_RM			0xFF	// encoding is /2
#define IA32_MOV_REG_IMM		0xB8	// encoding is +r <imm32>
#define	IA32_MOV_RM8_REG		0x88	// encoding is /r
#define	IA32_MOV_RM_REG			0x89	// encoding is /r
#define	IA32_MOV_REG_RM			0x8B	// encoding is /r
#define IA32_MOV_REG8_RM8		0x8A	// encoding is /r
#define IA32_MOV_RM8_REG8		0x88	// encoding is /r
#define IA32_MOV_RM_IMM32		0xC7	// encoding is /0
#define IA32_CMP_RM_IMM32		0x81	// encoding is /7 <imm32>
#define IA32_CMP_RM_IMM8		0x83	// encoding is /7 <imm8>
#define IA32_CMP_AL_IMM32		0x3C	// no extra encoding
#define IA32_CMP_EAX_IMM32		0x3D	// no extra encoding
#define IA32_CMP_RM_REG			0x39	// encoding is /r
#define IA32_CMP_REG_RM			0x3B	// encoding is /r
#define IA32_CMPSB				0xA6	// no extra encoding
#define IA32_TEST_RM_REG8		0x84	// encoding is /r
#define IA32_TEST_RM_REG		0x85	// encoding is /r
#define IA32_TEST_RM_IMM32		0xF7	// encoding is /0 <imm32>
#define IA32_JCC_IMM			0x70	// encoding is +cc <imm8>
#define IA32_JCC_IMM32_1		0x0F	// opcode part 1
#define IA32_JCC_IMM32_2		0x80	// encoding is +cc <imm32>
#define IA32_RET				0xC3	// no extra encoding
#define IA32_RETN				0xC2	// encoding is <imm16> 
#define IA32_NEG_RM				0xF7	// encoding is /3
#define IA32_INC_REG			0x40	// encoding is +r
#define IA32_INC_RM				0xFF	// encoding is /0
#define IA32_DEC_REG			0x48	// encoding is +r
#define IA32_DEC_RM				0xFF	// encoding is /1
#define IA32_OR_REG_RM			0x0B	// encoding is /r
#define IA32_AND_REG_RM			0x23	// encoding is /r
#define IA32_AND_EAX_IMM32		0x25	// encoding is <imm32>
#define IA32_AND_RM_IMM32		0x81	// encoding is /4
#define IA32_AND_RM_IMM8		0x83	// encoding is /4
#define IA32_NOT_RM				0xF7	// encoding is /2
#define IA32_DIV_RM				0xF7	// encoding is /6
#define IA32_MUL_RM				0xF7	// encoding is /4
#define IA32_IDIV_RM			0xF7	// encoding is /7
#define IA32_IMUL_RM			0xF7	// encoding is /5
#define IA32_IMUL_REG_IMM32		0x69	// encoding is /r
#define IA32_IMUL_REG_IMM8		0x6B	// encoding is /r
#define IA32_IMUL_REG_RM_1		0x0F	// encoding is _2
#define IA32_IMUL_REG_RM_2		0xAF	// encoding is /r
#define IA32_SHR_RM_IMM8		0xC1	// encoding is /5 <ib>
#define IA32_SHR_RM_1			0xD1	// encoding is /5
#define IA32_SHL_RM_IMM8		0xC1	// encoding is /4 <ib>
#define IA32_SHL_RM_1			0xD1	// encoding is /4
#define IA32_SAR_RM_CL			0xD3	// encoding is /7
#define IA32_SAR_RM_1			0xD1	// encoding is /7
#define IA32_SHR_RM_CL			0xD3	// encoding is /5
#define IA32_SHL_RM_CL			0xD3	// encoding is /4
#define IA32_SAR_RM_IMM8		0xC1	// encoding is /7 <ib>
#define IA32_SETCC_RM8_1		0x0F	// opcode part 1
#define IA32_SETCC_RM8_2		0x90	// encoding is +cc /0 (8bits)
#define IA32_CMOVCC_RM_1		0x0F	// opcode part 1
#define IA32_CMOVCC_RM_2		0x40	// encoding is +cc /r
#define IA32_XCHG_EAX_REG		0x90	// encoding is +r
#define IA32_LEA_REG_MEM		0x8D	// encoding is /r
#define IA32_POP_REG			0x58	// encoding is +r
#define IA32_PUSH_REG			0x50	// encoding is +r
#define IA32_PUSH_RM			0xFF	// encoding is /6
#define IA32_PUSH_IMM32			0x68	// encoding is <imm32>
#define IA32_PUSH_IMM8			0x6A	// encoding is <imm8>
#define IA32_REP				0xF3	// no extra encoding
#define IA32_MOVSD				0xA5	// no extra encoding
#define IA32_MOVSB				0xA4	// no extra encoding
#define IA32_STOSD				0xAB	// no extra encoding
#define IA32_CLD				0xFC	// no extra encoding
#define IA32_PUSHAD				0x60	// no extra encoding
#define IA32_POPAD				0x61	// no extra encoding
#define IA32_NOP				0x90	// no extra encoding
#define IA32_INT3				0xCC	// no extra encoding
#define IA32_FSTP_MEM32			0xD9	// encoding is /3
#define IA32_FSTP_MEM64			0xDD	// encoding is /3
#define IA32_FLD_MEM32			0xD9	// encoding is /0
#define IA32_FLD_MEM64			0xDD	// encoding is /0
#define IA32_FILD_MEM32			0xDB	// encoding is /0
#define IA32_FADD_MEM32			0xD8	// encoding is /0
#define IA32_FADD_FPREG_ST0_1	0xDC	// opcode part 1
#define IA32_FADD_FPREG_ST0_2	0xC0	// encoding is +r
#define IA32_FSUB_MEM32			0xD8	// encoding is /4
#define IA32_FMUL_MEM32			0xD8	// encoding is /1
#define IA32_FDIV_MEM32			0xD8	// encoding is /6
#define IA32_FSTCW_MEM16_1		0x9B	// opcode part 1
#define IA32_FSTCW_MEM16_2		0xD9	// encoding is /7
#define IA32_FLDCW_MEM16		0xD9	// encoding is /5
#define IA32_FISTP_MEM32		0xDB	// encoding is /3
#define IA32_FUCOMIP_1			0xDF	// opcode part 1
#define IA32_FUCOMIP_2			0xE8	// encoding is +r
#define IA32_FSTP_FPREG_1		0xDD	// opcode part 1
#define IA32_FSTP_FPREG_2		0xD8	// encoding is +r
#define IA32_MOVZX_R32_RM8_1	0x0F	// opcode part 1
#define IA32_MOVZX_R32_RM8_2	0xB6	// encoding is /r
#define IA32_MOVZX_R32_RM16_1	0x0F	// opcode part 1
#define IA32_MOVZX_R32_RM16_2	0xB7	// encoding is /r


namespace SourceHook
{
	namespace Impl
	{
		typedef Asm::GenBuffer JitWriter;

		inline jit_uint8_t ia32_modrm(jit_uint8_t mode, jit_uint8_t reg, jit_uint8_t rm)
		{
			jit_uint8_t modrm = (mode << 6);

			modrm |= (reg << 3);
			modrm |= (rm);

			return modrm;
		}

		//mode is the scaling method - NOSCALE ... SCALE8
		//index is the register that is scaled
		//base is the base register
		inline jit_uint8_t ia32_sib(jit_uint8_t mode, jit_uint8_t index, jit_uint8_t base)
		{
			jit_uint8_t sib = (mode << 6);

			sib |= (index << 3);
			sib |= (base);

			return sib;
		}

		inline void IA32_Int3(JitWriter *jit)
		{
			jit->write_ubyte(IA32_INT3);
		}

		/***********************
		* INCREMENT/DECREMENT *
		***********************/

		inline void IA32_Inc_Reg(JitWriter *jit, jit_uint8_t reg)
		{
			jit->write_ubyte(IA32_INC_REG+reg);
		}

		inline void IA32_Inc_Rm_Disp8(JitWriter *jit, jit_uint8_t reg, jit_int8_t disp)
		{
			jit->write_ubyte(IA32_INC_RM);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 0, reg));
			jit->write_byte(disp);
		}

		inline void IA32_Inc_Rm_Disp32(JitWriter *jit, jit_uint8_t reg, jit_int32_t disp)
		{
			jit->write_ubyte(IA32_INC_RM);
			jit->write_ubyte(ia32_modrm(MOD_DISP32, 0, reg));
			jit->write_int32(disp);
		}

		inline void IA32_Inc_Rm_Disp_Reg(JitWriter *jit, jit_uint8_t base, jit_uint8_t reg, jit_uint8_t scale)
		{
			jit->write_ubyte(IA32_INC_RM);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, 0, REG_SIB));
			jit->write_ubyte(ia32_sib(scale, reg, base));
		}

		inline void IA32_Dec_Reg(JitWriter *jit, jit_uint8_t reg)
		{
			jit->write_ubyte(IA32_DEC_REG+reg);
		}

		inline void IA32_Dec_Rm_Disp8(JitWriter *jit, jit_uint8_t reg, jit_int8_t disp)
		{
			jit->write_ubyte(IA32_DEC_RM);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 1, reg));
			jit->write_byte(disp);
		}

		inline void IA32_Dec_Rm_Disp32(JitWriter *jit, jit_uint8_t reg, jit_int32_t disp)
		{
			jit->write_ubyte(IA32_DEC_RM);
			jit->write_ubyte(ia32_modrm(MOD_DISP32, 1, reg));
			jit->write_int32(disp);
		}

		inline void IA32_Dec_Rm_Disp_Reg(JitWriter *jit, jit_uint8_t base, jit_uint8_t reg, jit_uint8_t scale)
		{
			jit->write_ubyte(IA32_DEC_RM);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, 1, REG_SIB));
			jit->write_ubyte(ia32_sib(scale, reg, base));
		}

		/****************
		* BINARY LOGIC *
		****************/

		inline void IA32_Xor_Rm_Reg(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t dest_mode)
		{
			jit->write_ubyte(IA32_XOR_RM_REG);
			jit->write_ubyte(ia32_modrm(dest_mode, src, dest));
		}

		inline void IA32_Xor_Reg_Rm(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t dest_mode)
		{
			jit->write_ubyte(IA32_XOR_REG_RM);
			jit->write_ubyte(ia32_modrm(dest_mode, dest, src));
		}

		inline void IA32_Xor_Rm_Imm8(JitWriter *jit, jit_uint8_t reg, jit_uint8_t mode, jit_int8_t value)
		{
			jit->write_ubyte(IA32_XOR_RM_IMM8);
			jit->write_ubyte(ia32_modrm(mode, 6, reg));
			jit->write_byte(value);
		}

		inline void IA32_Xor_Rm_Imm32(JitWriter *jit, jit_uint8_t reg, jit_uint8_t mode, jit_int32_t value)
		{
			jit->write_ubyte(IA32_XOR_RM_IMM32);
			jit->write_ubyte(ia32_modrm(mode, 6, reg));
			jit->write_int32(value);
		}

		inline void IA32_Xor_Eax_Imm32(JitWriter *jit, jit_int32_t value)
		{
			jit->write_ubyte(IA32_XOR_EAX_IMM32);
			jit->write_int32(value);
		}

		inline void IA32_Neg_Rm(JitWriter *jit, jit_uint8_t reg, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_NEG_RM);
			jit->write_ubyte(ia32_modrm(mode, 3, reg));
		}

		inline void IA32_Or_Reg_Rm(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_OR_REG_RM);
			jit->write_ubyte(ia32_modrm(mode, dest, src));
		}

		inline void IA32_And_Reg_Rm(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_AND_REG_RM);
			jit->write_ubyte(ia32_modrm(mode, dest, src));
		}

		inline void IA32_And_Rm_Imm32(JitWriter *jit, jit_uint8_t reg, jit_uint8_t mode, jit_int32_t value)
		{
			jit->write_ubyte(IA32_AND_RM_IMM32);
			jit->write_ubyte(ia32_modrm(mode, 4, reg));
			jit->write_int32(value);
		}

		inline void IA32_And_Rm_Imm8(JitWriter *jit, jit_uint8_t reg, jit_uint8_t mode, jit_int8_t value)
		{
			jit->write_ubyte(IA32_AND_RM_IMM8);
			jit->write_ubyte(ia32_modrm(mode, 4, reg));
			jit->write_byte(value);
		}

		inline void IA32_And_Eax_Imm32(JitWriter *jit, jit_int32_t value)
		{
			jit->write_ubyte(IA32_AND_EAX_IMM32);
			jit->write_int32(value);
		}

		inline void IA32_Not_Rm(JitWriter *jit, jit_uint8_t reg, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_NOT_RM);
			jit->write_ubyte(ia32_modrm(mode, 2, reg));
		}

		inline void IA32_Shr_Rm_Imm8(JitWriter *jit, jit_uint8_t dest, jit_uint8_t value, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_SHR_RM_IMM8);
			jit->write_ubyte(ia32_modrm(mode, 5, dest));
			jit->write_ubyte(value);
		}

		inline void IA32_Shr_Rm_1(JitWriter *jit, jit_uint8_t dest, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_SHR_RM_1);
			jit->write_ubyte(ia32_modrm(mode, 5, dest));
		}

		inline void IA32_Shl_Rm_Imm8(JitWriter *jit, jit_uint8_t dest, jit_uint8_t value, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_SHL_RM_IMM8);
			jit->write_ubyte(ia32_modrm(mode, 4, dest));
			jit->write_ubyte(value);
		}

		inline void IA32_Shl_Rm_1(JitWriter *jit, jit_uint8_t dest, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_SHL_RM_1);
			jit->write_ubyte(ia32_modrm(mode, 4, dest));
		}

		inline void IA32_Sar_Rm_Imm8(JitWriter *jit, jit_uint8_t dest, jit_uint8_t value, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_SAR_RM_IMM8);
			jit->write_ubyte(ia32_modrm(mode, 7, dest));
			jit->write_ubyte(value);
		}

		inline void IA32_Sar_Rm_CL(JitWriter *jit, jit_uint8_t reg, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_SAR_RM_CL);
			jit->write_ubyte(ia32_modrm(mode, 7, reg));
		}

		inline void IA32_Sar_Rm_1(JitWriter *jit, jit_uint8_t dest, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_SAR_RM_1);
			jit->write_ubyte(ia32_modrm(mode, 7, dest));
		}

		inline void IA32_Shr_Rm_CL(JitWriter *jit, jit_uint8_t reg, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_SHR_RM_CL);
			jit->write_ubyte(ia32_modrm(mode, 5, reg));
		}

		inline void IA32_Shl_Rm_CL(JitWriter *jit, jit_uint8_t reg, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_SHL_RM_CL);
			jit->write_ubyte(ia32_modrm(mode, 4, reg));
		}

		inline void IA32_Xchg_Eax_Reg(JitWriter *jit, jit_uint8_t reg)
		{
			jit->write_ubyte(IA32_XCHG_EAX_REG+reg);
		}

		/**********************
		* ARITHMETIC (BASIC) *
		**********************/

		inline void IA32_Add_Rm_Reg(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_ADD_RM_REG);
			jit->write_ubyte(ia32_modrm(mode, src, dest));
		}

		inline void IA32_Add_Reg_Rm(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_ADD_REG_RM);
			jit->write_ubyte(ia32_modrm(mode, dest, src));
		}

		inline void IA32_Add_Rm_Imm8(JitWriter *jit, jit_uint8_t reg, jit_int8_t value, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_ADD_RM_IMM8);
			jit->write_ubyte(ia32_modrm(mode, 0, reg));
			jit->write_byte(value);
		}

		inline void IA32_Add_Rm_Imm32(JitWriter *jit, jit_uint8_t reg, jit_int32_t value, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_ADD_RM_IMM32);
			jit->write_ubyte(ia32_modrm(mode, 0, reg));
			jit->write_int32(value);
		}

		inline void IA32_Add_Rm_ImmAuto(JitWriter *jit, jit_uint8_t reg, jit_int32_t value, jit_uint8_t mode)
		{
			if (value >= SCHAR_MIN && value <= SCHAR_MAX)
				IA32_Add_Rm_Imm8(jit, reg, static_cast<jit_int8_t>(value), mode);
			else
				IA32_Add_Rm_Imm32(jit, reg, value, mode);
		}

		inline void IA32_Add_Eax_Imm32(JitWriter *jit, jit_int32_t value)
		{
			jit->write_ubyte(IA32_ADD_EAX_IMM32);
			jit->write_int32(value);
		}

		inline void IA32_Sub_Rm_Reg(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_SUB_RM_REG);
			jit->write_ubyte(ia32_modrm(mode, src, dest));
		}

		inline void IA32_Sub_Reg_Rm(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_SUB_REG_RM);
			jit->write_ubyte(ia32_modrm(mode, dest, src));
		}

		inline void IA32_Sub_Reg_Rm_Disp8(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int8_t disp8)
		{
			jit->write_ubyte(IA32_SUB_REG_RM);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, dest, src));
			jit->write_byte(disp8);
		}

		inline void IA32_Sub_Rm_Reg_Disp8(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int8_t disp8)
		{
			jit->write_ubyte(IA32_SUB_RM_REG);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, src, dest));
			jit->write_byte(disp8);
		}

		inline void IA32_Sub_Rm_Imm8(JitWriter *jit, jit_uint8_t reg, jit_int8_t val, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_SUB_RM_IMM8);
			jit->write_ubyte(ia32_modrm(mode, 5, reg));
			jit->write_byte(val);
		}

		inline void IA32_Sub_Rm_Imm32(JitWriter *jit, jit_uint8_t reg, jit_int32_t val, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_SUB_RM_IMM32);
			jit->write_ubyte(ia32_modrm(mode, 5, reg));
			jit->write_int32(val);
		}

		inline void IA32_Sub_Rm_ImmAuto(JitWriter *jit, jit_uint8_t reg, jit_int32_t val, jit_uint8_t mode)
		{
			if (val >= SCHAR_MIN && val <= SCHAR_MAX)
				IA32_Sub_Rm_Imm8(jit, reg, static_cast<jit_int8_t>(val), mode);
			else
				IA32_Sub_Rm_Imm32(jit, reg, val, mode);
		}

		inline void IA32_Sbb_Reg_Rm(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_SBB_REG_RM);
			jit->write_ubyte(ia32_modrm(mode, dest, src));
		}

		inline void IA32_Sbb_Rm_Imm8(JitWriter *jit, jit_uint8_t dest, jit_int8_t value, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_SBB_RM_IMM8);
			jit->write_ubyte(ia32_modrm(mode, 3, dest));
			jit->write_byte(value);
		}

		inline void IA32_Div_Rm(JitWriter *jit, jit_uint8_t reg, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_DIV_RM);
			jit->write_ubyte(ia32_modrm(mode, 6, reg));
		}

		inline void IA32_IDiv_Rm(JitWriter *jit, jit_uint8_t reg, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_IDIV_RM);
			jit->write_ubyte(ia32_modrm(mode, 7, reg));
		}

		inline void IA32_Mul_Rm(JitWriter *jit, jit_uint8_t reg, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_MUL_RM);
			jit->write_ubyte(ia32_modrm(mode, 4, reg));
		}

		inline void IA32_IMul_Rm(JitWriter *jit, jit_uint8_t reg, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_IMUL_RM);
			jit->write_ubyte(ia32_modrm(mode, 5, reg));
		}

		inline void IA32_IMul_Reg_Imm8(JitWriter *jit, jit_uint8_t reg, jit_uint8_t mode, jit_int8_t value)
		{
			jit->write_ubyte(IA32_IMUL_REG_IMM8);
			jit->write_ubyte(ia32_modrm(mode, 0, reg));
			jit->write_byte(value);
		}

		inline void IA32_IMul_Reg_Imm32(JitWriter *jit, jit_uint8_t reg, jit_uint8_t mode, jit_int32_t value)
		{
			jit->write_ubyte(IA32_IMUL_REG_IMM32);
			jit->write_ubyte(ia32_modrm(mode, 0, reg));
			jit->write_int32(value);
		}

		inline void IA32_IMul_Reg_Rm(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_IMUL_REG_RM_1);
			jit->write_ubyte(IA32_IMUL_REG_RM_2);
			jit->write_ubyte(ia32_modrm(mode, dest, src));
		}

		inline void IA32_Add_Rm_Reg_Disp8(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int8_t disp)
		{
			jit->write_ubyte(IA32_ADD_RM_REG);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, src, dest));
			jit->write_byte(disp);
		}

		inline void IA32_Add_Reg_Rm_Disp8(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int8_t disp)
		{
			jit->write_ubyte(IA32_ADD_REG_RM);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, dest, src));
			jit->write_byte(disp);
		}

		inline void IA32_Add_Rm_Imm8_Disp8(JitWriter *jit, 
									jit_uint8_t dest, 
									jit_int8_t val, 
									jit_int8_t disp8)
		{
			jit->write_ubyte(IA32_ADD_RM_IMM8);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 0, dest));
			jit->write_byte(disp8);
			jit->write_byte(val);
		}

		inline void IA32_Add_Rm_Imm32_Disp8(JitWriter *jit, 
									jit_uint8_t dest, 
									jit_int32_t val, 
									jit_int8_t disp8)
		{
			jit->write_ubyte(IA32_ADD_RM_IMM32);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 0, dest));
			jit->write_byte(disp8);
			jit->write_int32(val);
		}

		inline jitoffs_t IA32_Add_Rm_Imm32_Later(JitWriter *jit, 
											jit_uint8_t dest, 
											jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_ADD_RM_IMM32);
			jit->write_ubyte(ia32_modrm(mode, 0, dest));
			jitoffs_t ptr = jit->get_outputpos();
			jit->write_int32(0);
			return ptr;
		}

		inline void IA32_Add_Rm_Imm8_Disp32(JitWriter *jit, 
									jit_uint8_t dest, 
									jit_int8_t val, 
									jit_int32_t disp32)
		{
			jit->write_ubyte(IA32_ADD_RM_IMM8);
			jit->write_ubyte(ia32_modrm(MOD_DISP32, 0, dest));
			jit->write_int32(disp32);
			jit->write_byte(val);
		}

		inline void IA32_Add_RmEBP_Imm8_Disp_Reg(JitWriter *jit, 
										jit_uint8_t dest_base, 
										jit_uint8_t dest_index,
										jit_uint8_t dest_scale,
										jit_int8_t val)
		{
			jit->write_ubyte(IA32_ADD_RM_IMM8);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 0, REG_SIB));
			jit->write_ubyte(ia32_sib(dest_scale, dest_index, dest_base));
			jit->write_byte(0);
			jit->write_byte(val);
		}

		inline void IA32_Sub_Rm_Imm8_Disp8(JitWriter *jit, 
									jit_uint8_t dest, 
									jit_int8_t val, 
									jit_int8_t disp8)
		{
			jit->write_ubyte(IA32_SUB_RM_IMM8);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 5, dest));
			jit->write_byte(disp8);
			jit->write_byte(val);
		}

		inline void IA32_Sub_Rm_Imm8_Disp32(JitWriter *jit, 
									jit_uint8_t dest, 
									jit_int8_t val, 
									jit_int32_t disp32)
		{
			jit->write_ubyte(IA32_SUB_RM_IMM8);
			jit->write_ubyte(ia32_modrm(MOD_DISP32, 5, dest));
			jit->write_int32(disp32);
			jit->write_byte(val);
		}

		inline void IA32_Sub_RmEBP_Imm8_Disp_Reg(JitWriter *jit, 
									jit_uint8_t dest_base, 
									jit_uint8_t dest_index,
									jit_uint8_t dest_scale,
									jit_int8_t val)
		{
			jit->write_ubyte(IA32_SUB_RM_IMM8);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 5, REG_SIB));
			jit->write_ubyte(ia32_sib(dest_scale, dest_index, dest_base));
			jit->write_byte(0);
			jit->write_byte(val);
		}

		/**
		* Memory Instructions
		*/

		inline void IA32_Lea_Reg_DispRegMult(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src_base, jit_uint8_t src_index, jit_uint8_t scale)
		{
			jit->write_ubyte(IA32_LEA_REG_MEM);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, dest, REG_SIB));
			jit->write_ubyte(ia32_sib(scale, src_index, src_base));
		}

		inline void IA32_Lea_Reg_DispEBPRegMult(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src_base, jit_uint8_t src_index, jit_uint8_t scale)
		{
			jit->write_ubyte(IA32_LEA_REG_MEM);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, dest, REG_SIB));
			jit->write_ubyte(ia32_sib(scale, src_index, src_base));
			jit->write_byte(0);
		}

		inline void IA32_Lea_Reg_DispRegMultImm8(JitWriter *jit, 
									jit_uint8_t dest, 
									jit_uint8_t src_base, 
									jit_uint8_t src_index, 
									jit_uint8_t scale, 
									jit_int8_t val)
		{
			jit->write_ubyte(IA32_LEA_REG_MEM);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, dest, REG_SIB));
			jit->write_ubyte(ia32_sib(scale, src_index, src_base));
			jit->write_byte(val);
		}

		inline void IA32_Lea_Reg_DispRegMultImm32(JitWriter *jit, 
												jit_uint8_t dest, 
												jit_uint8_t src_base, 
												jit_uint8_t src_index, 
												jit_uint8_t scale, 
												jit_int32_t val)
		{
			jit->write_ubyte(IA32_LEA_REG_MEM);
			jit->write_ubyte(ia32_modrm(MOD_DISP32, dest, REG_SIB));
			jit->write_ubyte(ia32_sib(scale, src_index, src_base));
			jit->write_int32(val);
		}

		inline void IA32_Lea_Reg_RegMultImm32(JitWriter *jit, 
												jit_uint8_t dest, 
												jit_uint8_t src_index, 
												jit_uint8_t scale, 
												jit_int32_t val)
		{
			jit->write_ubyte(IA32_LEA_REG_MEM);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, dest, REG_SIB));
			jit->write_ubyte(ia32_sib(scale, src_index, REG_IMM_BASE));
			jit->write_int32(val);
		}

		inline void IA32_Lea_DispRegImm8(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src_base, jit_int8_t val)
		{
			jit->write_ubyte(IA32_LEA_REG_MEM);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, dest, src_base));
			jit->write_byte(val);
		}

		inline void IA32_Lea_DispRegImm32(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src_base, jit_int32_t val)
		{
			jit->write_ubyte(IA32_LEA_REG_MEM);
			jit->write_ubyte(ia32_modrm(MOD_DISP32, dest, src_base));
			jit->write_int32(val);
		}

		inline void IA32_Lea_DispRegImmAuto(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src_base, jit_int32_t val)
		{
			if (val >= SCHAR_MIN && val <= SCHAR_MAX)
				IA32_Lea_DispRegImm8(jit, dest, src_base, static_cast<jit_int8_t>(val));
			else
				IA32_Lea_DispRegImm32(jit, dest, src_base, val);
		}

		/**
		* Stack Instructions
		*/

		inline void IA32_Pop_Reg(JitWriter *jit, jit_uint8_t reg)
		{
			jit->write_ubyte(IA32_POP_REG+reg);
		}

		inline void IA32_Push_Reg(JitWriter *jit, jit_uint8_t reg)
		{
			jit->write_ubyte(IA32_PUSH_REG+reg);
		}

		inline void IA32_Push_Imm8(JitWriter *jit, jit_int8_t val)
		{
			jit->write_ubyte(IA32_PUSH_IMM8);
			jit->write_byte(val);
		}

		inline void IA32_Push_Imm32(JitWriter *jit, jit_int32_t val)
		{
			jit->write_ubyte(IA32_PUSH_IMM32);
			jit->write_int32(val);
		}

		inline void IA32_Pushad(JitWriter *jit)
		{
			jit->write_ubyte(IA32_PUSHAD);
		}

		inline void IA32_Popad(JitWriter *jit)
		{
			jit->write_ubyte(IA32_POPAD);
		}

		inline void IA32_Push_Rm_Disp8(JitWriter *jit, jit_uint8_t reg, jit_int8_t disp8)
		{
			jit->write_ubyte(IA32_PUSH_RM);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 6, reg));
			jit->write_byte(disp8);
		}

		inline void IA32_Push_Rm_Disp32(JitWriter *jit, jit_uint8_t reg, jit_int32_t disp32)
		{
			jit->write_ubyte(IA32_PUSH_RM);
			jit->write_ubyte(ia32_modrm(MOD_DISP32, 6, reg));
			jit->write_int32(disp32);
		}

		inline void IA32_Push_Rm_DispAuto(JitWriter *jit, jit_uint8_t reg, jit_int32_t disp)
		{
			if (disp >= SCHAR_MIN && disp <= SCHAR_MAX)
				IA32_Push_Rm_Disp8(jit, reg, static_cast<jit_int8_t>(disp));
			else
				IA32_Push_Rm_Disp32(jit, reg, disp);
		}

		inline void IA32_Push_Rm_Disp8_ESP(JitWriter *jit, jit_int8_t disp8)
		{
			jit->write_ubyte(IA32_PUSH_RM);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 6, REG_SIB));
			jit->write_ubyte(ia32_sib(NOSCALE, REG_NOIDX, REG_ESP));
			jit->write_byte(disp8);
		}

		/**
		* Moving from REGISTER/MEMORY to REGISTER
		*/

		inline void IA32_Mov_Reg_Rm(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_MOV_REG_RM);
			jit->write_ubyte(ia32_modrm(mode, dest, src));
		}

		inline void IA32_Mov_Reg8_Rm8(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_MOV_REG8_RM8);
			jit->write_ubyte(ia32_modrm(mode, dest, src));
		}

		inline void IA32_Mov_Reg_RmESP(JitWriter *jit, jit_uint8_t dest)
		{
			jit->write_ubyte(IA32_MOV_REG_RM);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, dest, REG_ESP));
			jit->write_ubyte(ia32_sib(NOSCALE, REG_NOIDX, REG_ESP));
		}

		inline void IA32_Mov_Reg_Rm_Disp8(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int8_t disp)
		{
			jit->write_ubyte(IA32_MOV_REG_RM);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, dest, src));
			jit->write_byte(disp);
		}

		inline void IA32_Mov_Reg8_Rm8_Disp8(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int8_t disp)
		{
			jit->write_ubyte(IA32_MOV_REG8_RM8);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, dest, src));
			jit->write_byte(disp);
		}

		inline void IA32_Mov_Reg_Esp_Disp8(JitWriter *jit, jit_uint8_t dest, jit_int8_t disp)
		{
			jit->write_ubyte(IA32_MOV_REG_RM);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, dest, REG_SIB));
			jit->write_ubyte(ia32_sib(NOSCALE, REG_NOIDX, REG_ESP));
			jit->write_byte(disp);
		}

		inline void IA32_Mov_Reg_Rm_Disp32(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int32_t disp)
		{
			jit->write_ubyte(IA32_MOV_REG_RM);
			jit->write_ubyte(ia32_modrm(MOD_DISP32, dest, src));
			jit->write_int32(disp);
		}

		inline void IA32_Mov_Reg8_Rm8_Disp32(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int32_t disp)
		{
			jit->write_ubyte(IA32_MOV_REG8_RM8);
			jit->write_ubyte(ia32_modrm(MOD_DISP32, dest, src));
			jit->write_int32(disp);
		}

		inline void IA32_Mov_Reg_Rm_Disp_Reg(JitWriter *jit, 
									jit_uint8_t dest, 
									jit_uint8_t src_base, 
									jit_uint8_t src_index,
									jit_uint8_t src_scale)
		{
			jit->write_ubyte(IA32_MOV_REG_RM);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, dest, REG_SIB));
			jit->write_ubyte(ia32_sib(src_scale, src_index, src_base));
		}

		inline void IA32_Mov_Reg_Rm_Disp_Reg_Disp8(JitWriter *jit, 
											jit_uint8_t dest, 
											jit_uint8_t src_base, 
											jit_uint8_t src_index,
											jit_uint8_t src_scale,
											jit_int8_t disp8)
		{
			jit->write_ubyte(IA32_MOV_REG_RM);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, dest, REG_SIB));
			jit->write_ubyte(ia32_sib(src_scale, src_index, src_base));
			jit->write_byte(disp8);
		}

		inline void IA32_Mov_Reg_RmEBP_Disp_Reg(JitWriter *jit, 
											jit_uint8_t dest, 
											jit_uint8_t src_base, 
											jit_uint8_t src_index,
											jit_uint8_t src_scale)
		{
			jit->write_ubyte(IA32_MOV_REG_RM);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, dest, REG_SIB));
			jit->write_ubyte(ia32_sib(src_scale, src_index, src_base));
			jit->write_byte(0);
		}

		inline void IA32_Mov_Reg_Rm_DispAuto(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int32_t disp)
		{
			if (disp == 0)
				IA32_Mov_Reg_Rm(jit, dest, src, MOD_MEM_REG);
			else if (disp <= SCHAR_MAX)
				IA32_Mov_Reg_Rm_Disp8(jit, dest, src, static_cast<jit_int8_t>(disp));
			else
				IA32_Mov_Reg_Rm_Disp32(jit, dest, src, disp);
		}

		/**
		* Moving from REGISTER to REGISTER/MEMORY
		*/

		inline void IA32_Mov_Rm_Reg(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_MOV_RM_REG);
			jit->write_ubyte(ia32_modrm(mode, src, dest));
		}

		inline void IA32_Mov_Rm8_Reg8(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_MOV_RM8_REG8);
			jit->write_ubyte(ia32_modrm(mode, src, dest));
		}

		inline void IA32_Mov_Rm8_Reg8_Disp8(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int8_t disp)
		{
			jit->write_ubyte(IA32_MOV_RM8_REG8);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, src, dest));
			jit->write_byte(disp);
		}

		inline void IA32_Mov_Rm8_Reg8_Disp32(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int32_t disp)
		{
			jit->write_ubyte(IA32_MOV_RM8_REG8);
			jit->write_ubyte(ia32_modrm(MOD_DISP32, src, dest));
			jit->write_int32(disp);
		}

		inline void IA32_Mov_Rm8_Reg8_DispAuto(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int32_t disp)
		{
			if (disp >= SCHAR_MIN && disp <= SCHAR_MAX)
				IA32_Mov_Rm8_Reg8_Disp8(jit, dest, src, static_cast<jit_int8_t>(disp));
			else
				IA32_Mov_Rm8_Reg8_Disp32(jit, dest, src, disp);
		}
		
		inline void IA32_Mov_RmESP_Reg(JitWriter *jit, jit_uint8_t src)
		{
			jit->write_ubyte(IA32_MOV_RM_REG);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, src, REG_ESP));
			jit->write_ubyte(ia32_sib(NOSCALE, REG_NOIDX, REG_ESP));
		}

		inline void IA32_Mov_Rm_Reg_Disp8(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int8_t disp)
		{
			jit->write_ubyte(IA32_MOV_RM_REG);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, src, dest));
			jit->write_byte(disp);
		}

		inline void IA32_Mov_Rm_Reg_Disp32(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int32_t disp)
		{
			jit->write_ubyte(IA32_MOV_RM_REG);
			jit->write_ubyte(ia32_modrm(MOD_DISP32, src, dest));
			jit->write_int32(disp);
		}

		inline void IA32_Mov_Rm_Reg_DispAuto(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int32_t disp)
		{
			if (disp >= SCHAR_MIN && disp <= SCHAR_MAX)
				IA32_Mov_Rm_Reg_Disp8(jit, dest, src, static_cast<jit_int8_t>(disp));
			else
				IA32_Mov_Rm_Reg_Disp32(jit, dest, src, disp);
		}

		inline void IA32_Mov_RmEBP_Reg_Disp_Reg(JitWriter *jit, 
									jit_uint8_t dest_base, 
									jit_uint8_t dest_index,
									jit_uint8_t dest_scale,
									jit_uint8_t src)
		{
			jit->write_ubyte(IA32_MOV_RM_REG);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, src, REG_SIB));
			jit->write_ubyte(ia32_sib(dest_scale, dest_index, dest_base));
			jit->write_byte(0);
		}

		inline void IA32_Mov_Rm8EBP_Reg_Disp_Reg(JitWriter *jit, 
											jit_uint8_t dest_base, 
											jit_uint8_t dest_index,
											jit_uint8_t dest_scale,
											jit_uint8_t src)
		{
			jit->write_ubyte(IA32_MOV_RM8_REG);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, src, REG_SIB));
			jit->write_ubyte(ia32_sib(dest_scale, dest_index, dest_base));
			jit->write_byte(0);
		}

		inline void IA32_Mov_Rm16EBP_Reg_Disp_Reg(JitWriter *jit, 
											jit_uint8_t dest_base, 
											jit_uint8_t dest_index,
											jit_uint8_t dest_scale,
											jit_uint8_t src)
		{
			jit->write_ubyte(IA32_16BIT_PREFIX);
			jit->write_ubyte(IA32_MOV_RM_REG);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, src, REG_SIB));
			jit->write_ubyte(ia32_sib(dest_scale, dest_index, dest_base));
			jit->write_byte(0);
		}

		/**
		* Moving from IMMEDIATE to REGISTER
		*/

		inline jitoffs_t IA32_Mov_Reg_Imm32(JitWriter *jit, jit_uint8_t dest, jit_int32_t num)
		{
			jitoffs_t offs;
			jit->write_ubyte(IA32_MOV_REG_IMM+dest);
			offs = jit->get_outputpos();
			jit->write_int32(num);
			return offs;
		}

		inline void IA32_Mov_Rm_Imm32(JitWriter *jit, jit_uint8_t dest, jit_int32_t val, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_MOV_RM_IMM32);
			jit->write_ubyte(ia32_modrm(mode, 0, dest));
			jit->write_int32(val);
		}

		inline void IA32_Mov_Rm_Imm32_Disp8(JitWriter *jit, 
									jit_uint8_t dest, 
									jit_int32_t val, 
									jit_int8_t disp8)
		{
			jit->write_ubyte(IA32_MOV_RM_IMM32);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 0, dest));
			jit->write_byte(disp8);
			jit->write_int32(val);
		}

		inline void IA32_Mov_Rm_Imm32_Disp32(JitWriter *jit, 
									jit_uint8_t dest, 
									jit_int32_t val, 
									jit_int32_t disp32)
		{
			jit->write_ubyte(IA32_MOV_RM_IMM32);
			jit->write_ubyte(ia32_modrm(MOD_DISP32, 0, dest));
			jit->write_int32(disp32);
			jit->write_int32(val);
		}

		inline void IA32_Mov_RmEBP_Imm32_Disp_Reg(JitWriter *jit, 
										jit_uint8_t dest_base, 
										jit_uint8_t dest_index, 
										jit_uint8_t dest_scale, 
										jit_int32_t val)
		{
			jit->write_ubyte(IA32_MOV_RM_IMM32);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 0, REG_SIB));
			jit->write_ubyte(ia32_sib(dest_scale, dest_index, dest_base));
			jit->write_byte(0);
			jit->write_int32(val);
		}

		inline void IA32_Mov_ESP_Disp8_Imm32(JitWriter *jit, jit_int8_t disp8, jit_int32_t val)
		{
			jit->write_ubyte(IA32_MOV_RM_IMM32);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 0, REG_SIB));
			jit->write_ubyte(ia32_sib(NOSCALE, REG_NOIDX, REG_ESP));
			jit->write_byte(disp8);
			jit->write_int32(val);
		}

		/**
		* Floating Point Instructions
		*/

		inline void IA32_Fstcw_Mem16_ESP(JitWriter *jit)
		{
			jit->write_ubyte(IA32_FSTCW_MEM16_1);
			jit->write_ubyte(IA32_FSTCW_MEM16_2);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, 7, REG_SIB));
			jit->write_ubyte(ia32_sib(NOSCALE, REG_NOIDX, REG_ESP));
		}

		inline void IA32_Fldcw_Mem16_ESP(JitWriter *jit)
		{
			jit->write_ubyte(IA32_FLDCW_MEM16);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, 5, REG_SIB));
			jit->write_ubyte(ia32_sib(NOSCALE, REG_NOIDX, REG_ESP));
		}

		inline void IA32_Fldcw_Mem16_Disp8_ESP(JitWriter *jit, jit_int8_t disp8)
		{
			jit->write_ubyte(IA32_FLDCW_MEM16);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 5, REG_SIB));
			jit->write_ubyte(ia32_sib(NOSCALE, REG_NOIDX, REG_ESP));
			jit->write_byte(disp8);
		}

		inline void IA32_Fistp_Mem32_ESP(JitWriter *jit)
		{
			jit->write_ubyte(IA32_FISTP_MEM32);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, 3, REG_SIB));
			jit->write_ubyte(ia32_sib(NOSCALE, REG_NOIDX, REG_ESP));
		}

		inline void IA32_Fistp_Mem32_Disp8_Esp(JitWriter *jit, jit_int8_t disp8)
		{
			jit->write_ubyte(IA32_FISTP_MEM32);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 3, REG_SIB));
			jit->write_ubyte(ia32_sib(NOSCALE, REG_NOIDX, REG_ESP));
			jit->write_byte(disp8);
		}

		inline void IA32_Fucomip_ST0_FPUreg(JitWriter *jit, jit_uint8_t reg)
		{
			jit->write_ubyte(IA32_FUCOMIP_1);
			jit->write_ubyte(IA32_FUCOMIP_2+reg);
		}

		inline void IA32_Fadd_FPUreg_ST0(JitWriter *jit, jit_uint8_t reg)
		{
			jit->write_ubyte(IA32_FADD_FPREG_ST0_1);
			jit->write_ubyte(IA32_FADD_FPREG_ST0_2+reg);
		}

		inline void IA32_Fadd_Mem32_Disp8(JitWriter *jit, jit_uint8_t src, jit_int8_t val)
		{
			jit->write_ubyte(IA32_FADD_MEM32);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 0, src));
			jit->write_byte(val);
		}

		inline void IA32_Fadd_Mem32_ESP(JitWriter *jit)
		{
			jit->write_ubyte(IA32_FADD_MEM32);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, 0, REG_SIB));
			jit->write_ubyte(ia32_sib(NOSCALE, REG_NOIDX, REG_ESP));
		}

		inline void IA32_Fsub_Mem32_Disp8(JitWriter *jit, jit_uint8_t src, jit_int8_t val)
		{
			jit->write_ubyte(IA32_FSUB_MEM32);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 4, src));
			jit->write_byte(val);
		}

		inline void IA32_Fmul_Mem32_Disp8(JitWriter *jit, jit_uint8_t src, jit_int8_t val)
		{
			jit->write_ubyte(IA32_FMUL_MEM32);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 1, src));
			jit->write_byte(val);
		}

		inline void IA32_Fdiv_Mem32_Disp8(JitWriter *jit, jit_uint8_t src, jit_int8_t val)
		{
			jit->write_ubyte(IA32_FDIV_MEM32);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 6, src));
			jit->write_byte(val);
		}

		inline void IA32_Fild_Mem32(JitWriter *jit, jit_uint8_t src)
		{
			jit->write_ubyte(IA32_FILD_MEM32);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, 0, src));
		}

		inline void IA32_Fstp_Mem32(JitWriter *jit, jit_uint8_t dest)
		{
			jit->write_ubyte(IA32_FSTP_MEM32);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, 3, dest));
		}

		inline void IA32_Fstp_Mem32_DispAuto(JitWriter *jit, jit_uint8_t dest, jit_int32_t disp)
		{
			jit->write_ubyte(IA32_FSTP_MEM32);

			if (disp > SCHAR_MIN && disp < SCHAR_MAX)
			{
				jit->write_ubyte(ia32_modrm(MOD_DISP8, 3, dest));
				jit->write_byte(static_cast<jit_int8_t>(disp));
			}
			else
			{
				jit->write_ubyte(ia32_modrm(MOD_DISP32, 3, dest));
				jit->write_byte(disp);
			}
		}

		inline void IA32_Fstp_Mem64(JitWriter *jit, jit_uint8_t dest)
		{
			jit->write_ubyte(IA32_FSTP_MEM64);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, 3, dest));
		}

		inline void IA32_Fstp_Mem64_DispAuto(JitWriter *jit, jit_uint8_t dest, jit_int32_t disp)
		{
			jit->write_ubyte(IA32_FSTP_MEM64);

			if (disp > SCHAR_MIN && disp < SCHAR_MAX)
			{
				jit->write_ubyte(ia32_modrm(MOD_DISP8, 3, dest));
				jit->write_byte(static_cast<jit_int8_t>(disp));
			}
			else
			{
				jit->write_ubyte(ia32_modrm(MOD_DISP32, 3, dest));
				jit->write_byte(disp);
			}
		}

		inline void IA32_Fstp_Mem32_ESP(JitWriter *jit)
		{
			jit->write_ubyte(IA32_FSTP_MEM32);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, 3, REG_SIB));
			jit->write_ubyte(ia32_sib(NOSCALE, REG_NOIDX, REG_ESP));
		}

		inline void IA32_Fstp_Mem64_ESP(JitWriter *jit)
		{
			jit->write_ubyte(IA32_FSTP_MEM64);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, 3, REG_SIB));
			jit->write_ubyte(ia32_sib(NOSCALE, REG_NOIDX, REG_ESP));
		}

		inline void IA32_Fstp_FPUreg(JitWriter *jit, jit_uint8_t reg)
		{
			jit->write_ubyte(IA32_FSTP_FPREG_1);
			jit->write_ubyte(IA32_FSTP_FPREG_2+reg);
		}

		inline void IA32_Fld_Mem32(JitWriter *jit, jit_uint8_t src)
		{
			jit->write_ubyte(IA32_FLD_MEM32);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, 0, src));
		}

		inline void IA32_Fld_Mem64(JitWriter *jit, jit_uint8_t src)
		{
			jit->write_ubyte(IA32_FLD_MEM64);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, 0, src));
		}

		inline void IA32_Fld_Mem32_Disp8(JitWriter *jit, jit_uint8_t src, jit_int8_t val)
		{
			jit->write_ubyte(IA32_FLD_MEM32);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 0, src));
			jit->write_byte(val);
		}

		inline void IA32_Fld_Mem64_Disp8(JitWriter *jit, jit_uint8_t src, jit_int8_t val)
		{
			jit->write_ubyte(IA32_FLD_MEM64);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 0, src));
			jit->write_byte(val);
		}

		inline void IA32_Fld_Mem32_Disp32(JitWriter *jit, jit_uint8_t src, jit_int32_t val)
		{
			jit->write_ubyte(IA32_FLD_MEM32);
			jit->write_ubyte(ia32_modrm(MOD_DISP32, 0, src));
			jit->write_int32(val);
		}

		inline void IA32_Fld_Mem64_Disp32(JitWriter *jit, jit_uint8_t src, jit_int32_t val)
		{
			jit->write_ubyte(IA32_FLD_MEM64);
			jit->write_ubyte(ia32_modrm(MOD_DISP32, 0, src));
			jit->write_int32(val);
		}

		inline void IA32_Fld_Mem32_DispAuto(JitWriter *jit, jit_uint8_t src, jit_int32_t disp)
		{
			if (disp == 0)
				IA32_Fld_Mem32(jit, src);
			else if (disp <= SCHAR_MAX)
				IA32_Fld_Mem32_Disp8(jit, src, static_cast<jit_int8_t>(disp));
			else
				IA32_Fld_Mem32_Disp32(jit, src, disp);
		}

		inline void IA32_Fld_Mem64_DispAuto(JitWriter *jit, jit_uint8_t src, jit_int32_t disp)
		{
			if (disp == 0)
				IA32_Fld_Mem64(jit, src);
			else if (disp <= SCHAR_MAX)
				IA32_Fld_Mem64_Disp8(jit, src, static_cast<jit_int8_t>(disp));
			else
				IA32_Fld_Mem64_Disp32(jit, src, disp);
		}

		/**
		* Move data with zero extend
		*/

		inline void IA32_Movzx_Reg32_Rm8(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_MOVZX_R32_RM8_1);
			jit->write_ubyte(IA32_MOVZX_R32_RM8_2);
			jit->write_ubyte(ia32_modrm(mode, dest, src));
		}

		inline void IA32_Movzx_Reg32_Rm8_Disp8(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int8_t disp)
		{
			jit->write_ubyte(IA32_MOVZX_R32_RM8_1);
			jit->write_ubyte(IA32_MOVZX_R32_RM8_2);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, dest, src));
			jit->write_byte(disp);
		}

		inline void IA32_Movzx_Reg32_Rm8_Disp32(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int32_t disp)
		{
			jit->write_ubyte(IA32_MOVZX_R32_RM8_1);
			jit->write_ubyte(IA32_MOVZX_R32_RM8_2);
			jit->write_ubyte(ia32_modrm(MOD_DISP32, dest, src));
			jit->write_int32(disp);
		}

		inline void IA32_Movzx_Reg32_Rm8_DispAuto(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int32_t disp)
		{
			if (disp >= SCHAR_MIN && disp <= SCHAR_MAX)
				IA32_Movzx_Reg32_Rm8_Disp8(jit, dest, src, static_cast<jit_int8_t>(disp));
			else
				IA32_Movzx_Reg32_Rm8_Disp32(jit, dest, src, disp);
		}

		inline void IA32_Movzx_Reg32_Rm16(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_MOVZX_R32_RM16_1);
			jit->write_ubyte(IA32_MOVZX_R32_RM16_2);
			jit->write_ubyte(ia32_modrm(mode, dest, src));
		}

		inline void IA32_Movzx_Reg32_Rm16_Disp8(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int8_t disp)
		{
			jit->write_ubyte(IA32_MOVZX_R32_RM16_1);
			jit->write_ubyte(IA32_MOVZX_R32_RM16_2);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, dest, src));
			jit->write_byte(disp);
		}

		inline void IA32_Movzx_Reg32_Rm16_Disp32(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int32_t disp)
		{
			jit->write_ubyte(IA32_MOVZX_R32_RM16_1);
			jit->write_ubyte(IA32_MOVZX_R32_RM16_2);
			jit->write_ubyte(ia32_modrm(MOD_DISP32, dest, src));
			jit->write_int32(disp);
		}

		inline void IA32_Movzx_Reg32_Rm16_DispAuto(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_int32_t disp)
		{
			if (disp >= SCHAR_MIN && disp <= SCHAR_MAX)
				IA32_Movzx_Reg32_Rm16_Disp8(jit, dest, src, static_cast<jit_int8_t>(disp));
			else
				IA32_Movzx_Reg32_Rm16_Disp32(jit, dest, src, disp);
		}

		/**
		* Branching/Jumping
		*/

		inline jitoffs_t IA32_Jump_Cond_Imm8(JitWriter *jit, jit_uint8_t cond, jit_int8_t disp)
		{
			jitoffs_t ptr;
			jit->write_ubyte(IA32_JCC_IMM+cond);
			ptr = jit->get_outputpos();
			jit->write_byte(disp);
			return ptr;
		}

		inline jitoffs_t IA32_Jump_Imm32(JitWriter *jit, jit_int32_t disp)
		{
			jitoffs_t ptr;
			jit->write_ubyte(IA32_JMP_IMM32);
			ptr = jit->get_outputpos();
			jit->write_int32(disp);
			return ptr;
		}

		inline jitoffs_t IA32_Jump_Imm8(JitWriter *jit, jit_int8_t disp)
		{
			jitoffs_t ptr;
			jit->write_ubyte(IA32_JMP_IMM8);
			ptr = jit->get_outputpos();
			jit->write_byte(disp);
			return ptr;
		}

		inline jitoffs_t IA32_Jump_Cond_Imm32(JitWriter *jit, jit_uint8_t cond, jit_int32_t disp)
		{
			jitoffs_t ptr;
			jit->write_ubyte(IA32_JCC_IMM32_1);
			jit->write_ubyte(IA32_JCC_IMM32_2+cond);
			ptr = jit->get_outputpos();
			jit->write_int32(disp);
			return ptr;
		}

		inline void IA32_Jump_Reg(JitWriter *jit, jit_uint8_t reg)
		{
			jit->write_ubyte(IA32_JMP_RM);
			jit->write_ubyte(ia32_modrm(MOD_REG, 4, reg));
		}

		inline void IA32_Jump_Rm(JitWriter *jit, jit_uint8_t reg, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_JMP_RM);
			jit->write_ubyte(ia32_modrm(mode, 4, reg));
		}

		inline jitoffs_t IA32_Call_Imm32(JitWriter *jit, jit_int32_t disp)
		{
			jitoffs_t ptr;
			jit->write_ubyte(IA32_CALL_IMM32);
			ptr = jit->get_outputpos();
			jit->write_int32(disp);
			return ptr;
		}

		inline void IA32_Call_Reg(JitWriter *jit, jit_uint8_t reg)
		{
			jit->write_ubyte(IA32_CALL_RM);
			jit->write_ubyte(ia32_modrm(MOD_REG, 2, reg));
		}

		inline void IA32_Call_Rm(JitWriter *jit, jit_uint8_t reg, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_CALL_RM);
			jit->write_ubyte(ia32_modrm(mode, 4, reg));
		}

		inline void IA32_Return(JitWriter *jit)
		{
			jit->write_ubyte(IA32_RET);
		}

		inline void IA32_Return_Popstack(JitWriter *jit, unsigned short bytes)
		{
			jit->write_ubyte(IA32_RETN);
			jit->write_ushort(bytes);
		}

		inline void IA32_Test_Rm_Reg(JitWriter *jit, jit_uint8_t reg1, jit_uint8_t reg2, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_TEST_RM_REG);
			jit->write_ubyte(ia32_modrm(mode, reg2, reg1));
		}

		inline void IA32_Test_Rm_Reg8(JitWriter *jit, jit_uint8_t reg1, jit_uint8_t reg2, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_TEST_RM_REG8);
			jit->write_ubyte(ia32_modrm(mode, reg2, reg1));
		}

		inline void IA32_Test_Rm_Imm32(JitWriter *jit, jit_uint8_t operand1, jit_int32_t imm_operand, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_TEST_RM_IMM32);
			jit->write_ubyte(ia32_modrm(mode, 0, operand1));
			jit->write_int32(imm_operand);
		}

		inline void IA32_Cmp_Rm_Reg(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_CMP_RM_REG);
			jit->write_ubyte(ia32_modrm(mode, src, dest));
		}

		inline void IA32_Cmp_Reg_Rm(JitWriter *jit, jit_uint8_t dest, jit_uint8_t src, jit_uint8_t mode)
		{
			jit->write_ubyte(IA32_CMP_REG_RM);
			jit->write_ubyte(ia32_modrm(mode, dest, src));
		}

		inline void IA32_Cmp_Reg_Rm_ESP(JitWriter *jit, jit_uint8_t cmpreg)
		{
			jit->write_ubyte(IA32_CMP_REG_RM);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, cmpreg, REG_SIB));
			jit->write_ubyte(ia32_sib(NOSCALE, REG_NOIDX, REG_ESP));
		}

		inline void IA32_Cmp_Reg_Rm_Disp8(JitWriter *jit, jit_uint8_t reg1, jit_uint8_t reg2, jit_int8_t disp8)
		{
			jit->write_ubyte(IA32_CMP_REG_RM);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, reg1, reg2));
			jit->write_byte(disp8);
		}

		inline void IA32_Cmp_Rm_Imm8(JitWriter *jit, jit_uint8_t mode, jit_uint8_t rm, jit_int8_t imm8)
		{
			jit->write_ubyte(IA32_CMP_RM_IMM8);
			jit->write_ubyte(ia32_modrm(mode, 7, rm));
			jit->write_byte(imm8);
		}

		inline void IA32_Cmp_Rm_Imm32(JitWriter *jit, jit_uint8_t mode, jit_uint8_t rm, jit_int32_t imm32)
		{
			jit->write_ubyte(IA32_CMP_RM_IMM32);
			jit->write_ubyte(ia32_modrm(mode, 7, rm));
			jit->write_int32(imm32);
		}

		inline void IA32_Cmp_Rm_Imm32_Disp8(JitWriter *jit, jit_uint8_t reg, jit_int8_t disp8, jit_int32_t imm32)
		{
			jit->write_ubyte(IA32_CMP_RM_IMM32);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 7, reg));
			jit->write_byte(disp8);
			jit->write_int32(imm32);
		}

		inline void IA32_Cmp_Rm_Disp8_Imm8(JitWriter *jit, jit_uint8_t reg, jit_int8_t disp, jit_int8_t imm8)
		{
			jit->write_ubyte(IA32_CMP_RM_IMM8);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 7, reg));
			jit->write_byte(disp);
			jit->write_byte(imm8);
		}

		inline void IA32_Cmp_Al_Imm8(JitWriter *jit, jit_int8_t value)
		{
			jit->write_ubyte(IA32_CMP_AL_IMM32);
			jit->write_byte(value);
		}

		inline void IA32_Cmp_Eax_Imm32(JitWriter *jit, jit_int32_t value)
		{
			jit->write_ubyte(IA32_CMP_EAX_IMM32);
			jit->write_int32(value);
		}

		inline void IA32_SetCC_Rm8(JitWriter *jit, jit_uint8_t reg, jit_uint8_t cond)
		{
			jit->write_ubyte(IA32_SETCC_RM8_1);
			jit->write_ubyte(IA32_SETCC_RM8_2+cond);
			jit->write_ubyte(ia32_modrm(MOD_REG, 0, reg));
		}

		inline void IA32_CmovCC_Rm(JitWriter *jit, jit_uint8_t src, jit_uint8_t cond)
		{
			jit->write_ubyte(IA32_CMOVCC_RM_1);
			jit->write_ubyte(IA32_CMOVCC_RM_2+cond);
			jit->write_ubyte(ia32_modrm(MOD_MEM_REG, 0, src));
		}

		inline void IA32_CmovCC_Rm_Disp8(JitWriter *jit, jit_uint8_t src, jit_uint8_t cond, jit_int8_t disp)
		{
			jit->write_ubyte(IA32_CMOVCC_RM_1);
			jit->write_ubyte(IA32_CMOVCC_RM_2+cond);
			jit->write_ubyte(ia32_modrm(MOD_DISP8, 0, src));
			jit->write_byte(disp);
		}

		inline void IA32_Cmpsb(JitWriter *jit)
		{
			jit->write_ubyte(IA32_CMPSB);
		}

		inline void IA32_Rep(JitWriter *jit)
		{
			jit->write_ubyte(IA32_REP);
		}

		inline void IA32_Movsd(JitWriter *jit)
		{
			jit->write_ubyte(IA32_MOVSD);
		}

		inline void IA32_Movsb(JitWriter *jit)
		{
			jit->write_ubyte(IA32_MOVSB);
		}

		inline void IA32_Stosd(JitWriter *jit)
		{
			jit->write_ubyte(IA32_STOSD);
		}

		inline void IA32_Cld(JitWriter *jit)
		{
			jit->write_ubyte(IA32_CLD);
		}

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
	}
}

#endif //_INCLUDE_JIT_X86_MACROS_H
