#pragma once

#include <cstdint>
#include "sh_pagealloc.h"
#include "sh_asm.h"

namespace SourceHook
{
	namespace Asm
	{
		enum x8664Reg : std::uint8_t {
			RAX =  0,
			RCX =  1,
			RDX =  2,
			RBX =  3,
			RSP =  4,
			RBP =  5,
			RSI =  6,
			RDI =  7,
			R8  =  8,
			R9  =  9,
			R10 =  10,
			R11 =  11,
			R12 =  12,
			R13 =  13,
			R14 =  14,
			R15 =  15
		};

		enum x8664FloatReg : std::uint8_t {
			XMM0  =  0,
			XMM1  =  1,
			XMM2  =  2,
			XMM3  =  3,
			XMM4  =  4,
			XMM5  =  5,
			XMM6  =  6,
			XMM7  =  7,
			XMM8  =  8,
			XMM9  =  9,
			XMM10 =  10,
			XMM11 =  11,
			XMM12 =  12,
			XMM13 =  13,
			XMM14 =  14,
			XMM15 =  15
		};

		class x86_64_FloatReg {
		public:
			constexpr x86_64_FloatReg(x8664FloatReg op) : code(op) { }

			constexpr bool operator==(x86_64_FloatReg a) const { return code == a.code; }
			constexpr bool operator!=(x86_64_FloatReg a) const { return code != a.code; }

			constexpr bool extended() const { return ((code & 0x8) == 0x8); }
			constexpr std::uint8_t low() const { return code & 0x7; }
			constexpr operator x8664FloatReg() const { return code; }
		protected:
			x8664FloatReg code;
		};

		static const x86_64_FloatReg xmm0 =  { x8664FloatReg::XMM0 };
		static const x86_64_FloatReg xmm1 =  { x8664FloatReg::XMM1 };
		static const x86_64_FloatReg xmm2 =  { x8664FloatReg::XMM2 };
		static const x86_64_FloatReg xmm3 =  { x8664FloatReg::XMM3 };
		static const x86_64_FloatReg xmm4 =  { x8664FloatReg::XMM4 };
		static const x86_64_FloatReg xmm5 =  { x8664FloatReg::XMM5 };
		static const x86_64_FloatReg xmm6 =  { x8664FloatReg::XMM6 };
		static const x86_64_FloatReg xmm7 =  { x8664FloatReg::XMM7 };

		static const x86_64_FloatReg xmm8 =  { x8664FloatReg::XMM8  };
		static const x86_64_FloatReg xmm9 =  { x8664FloatReg::XMM9  };
		static const x86_64_FloatReg xmm10 = { x8664FloatReg::XMM10 };
		static const x86_64_FloatReg xmm11 = { x8664FloatReg::XMM11 };
		static const x86_64_FloatReg xmm12 = { x8664FloatReg::XMM12 };
		static const x86_64_FloatReg xmm13 = { x8664FloatReg::XMM13 };
		static const x86_64_FloatReg xmm14 = { x8664FloatReg::XMM14 };
		static const x86_64_FloatReg xmm15 = { x8664FloatReg::XMM15 };
		
		enum MOD_MODRM : std::uint8_t {
			DISP0 = 0b00,
			DISP8 = 0b01,
			DISP32 = 0b10,
			REG = 0b11
		};

		class x86_64_RegRm {
		public:
			friend class x86_64_Reg;

			inline std::uint8_t sib() {
				// For the time being, we don't support multiple register
				return (0 << 6) | (this->low() << 3) | this->low();
			}

			inline std::uint8_t modrm(x8664Reg reg) {
				return (mod << 6) | ((reg & 0x7) << 3) | this->low();
			}

			inline std::uint8_t modrm(x8664FloatReg reg) {
				return (mod << 6) | ((reg & 0x7) << 3) | this->low();
			}

			inline std::uint8_t modrm() {
				return (mod << 6) | (0x0 << 3) | this->low();
			}

			void write_modrm(GenBuffer* buffer);
			void write_modrm(GenBuffer* buffer, x8664Reg op);
			void write_modrm(GenBuffer* buffer, x8664FloatReg op);

			bool extended() const { return ((rm & 0x8) == 0x8); }
			std::uint8_t low() const { return rm & 0x7; }
			constexpr operator x8664Reg() const { return rm; }

		protected:
			x86_64_RegRm(x8664Reg reg, std::int32_t disp) : rm(reg), disp(disp) {
				Setup();
			}

			x86_64_RegRm(x8664Reg reg) : rm(reg), disp(0) {
				Setup();
			}

			void Setup();

			x8664Reg rm;
			std::int32_t disp;
			MOD_MODRM mod;
		};

		class x86_64_Reg {
		public:
			constexpr x86_64_Reg(x8664Reg op) : code(op) { }

			constexpr bool operator==(x86_64_Reg a) const { return code == a.code; }
			constexpr bool operator!=(x86_64_Reg a) const { return code != a.code; }
			x86_64_RegRm operator()() const { return x86_64_RegRm(*this, 0); }
			x86_64_RegRm operator()(std::int32_t disp) const { return x86_64_RegRm(*this, disp); }

			constexpr bool extended() const { return ((code & 0x8) == 0x8); }
			constexpr std::uint8_t low() const { return code & 0x7; }
			constexpr operator x8664Reg() const { return code; }
		protected:
			x8664Reg code;
		};

		static const x86_64_Reg rax = { x8664Reg::RAX };
		static const x86_64_Reg rcx = { x8664Reg::RCX };
		static const x86_64_Reg rdx = { x8664Reg::RDX };
		static const x86_64_Reg rbx = { x8664Reg::RBX };
		static const x86_64_Reg rsp = { x8664Reg::RSP };
		static const x86_64_Reg rbp = { x8664Reg::RBP };
		static const x86_64_Reg rsi = { x8664Reg::RSI };
		static const x86_64_Reg rdi = { x8664Reg::RDI };

		static const x86_64_Reg r8 =  { x8664Reg::R8  };
		static const x86_64_Reg r9 =  { x8664Reg::R9  };
		static const x86_64_Reg r10 = { x8664Reg::R10 };
		static const x86_64_Reg r11 = { x8664Reg::R11 };
		static const x86_64_Reg r12 = { x8664Reg::R12 };
		static const x86_64_Reg r13 = { x8664Reg::R13 };
		static const x86_64_Reg r14 = { x8664Reg::R14 };
		static const x86_64_Reg r15 = { x8664Reg::R15 };

		enum REX : std::uint8_t {
			BASE = 0x40,
			B = 0x41,
			X = 0x42,
			XB = 0x43,
			R = 0x44,
			RB = 0x45,
			RX = 0x46,
			RXB = 0x47,
			W = 0x48,
			WB = 0x49,
			WX = 0x4A,
			WXB = 0x4B,
			WR = 0x4C,
			WRB = 0x4D,
			WRX = 0x4E,
			WRXB = 0x4F
		};

		constexpr inline std::uint8_t w_rex(x8664Reg reg, x8664Reg rm) {
			return REX::W | ((static_cast<std::int8_t>((reg & 0x8) == 0x8) << 2) | static_cast<std::int8_t>((rm & 0x8) == 0x8));
		}

		constexpr inline std::uint8_t w_rex(x8664FloatReg reg, x8664Reg rm) {
			return REX::W | ((static_cast<std::int8_t>((reg & 0x8) == 0x8) << 2) | static_cast<std::int8_t>((rm & 0x8) == 0x8));
		}

		constexpr inline std::uint8_t w_rex(x8664Reg reg, x8664FloatReg rm) {
			return REX::W | ((static_cast<std::int8_t>((reg & 0x8) == 0x8) << 2) | static_cast<std::int8_t>((rm & 0x8) == 0x8));
		}

		constexpr inline std::uint8_t modrm(x8664Reg reg, x8664Reg rm) {
			return (MOD_MODRM::REG << 6) | ((reg & 0x7) << 3) | (rm & 0x7);
		}

		constexpr inline std::uint8_t modrm_rm(x8664Reg rm, std::uint8_t base) {
			return (MOD_MODRM::REG << 6) | (base << 3) | (rm & 0x7);
		}

		inline void x86_64_RegRm::Setup() {
			if (disp == 0 && rm != x8664Reg::RBP && rm != x8664Reg::R13) {
				mod = DISP0;
			}
			else if (disp >= SCHAR_MIN && disp <= SCHAR_MAX) {
				mod = DISP8;
			} else {
				mod = DISP32;
			}
		}

		inline void x86_64_RegRm::write_modrm(GenBuffer* buffer) {
			// modrm
			buffer->write_ubyte(modrm());

			// Special register we need a sib byte
			if (rm == x8664Reg::RSP || rm == x8664Reg::R12) { // rsp/r12
				buffer->write_ubyte(sib());
			}

			// Special disp mod
			if (mod != DISP0) {
				if (mod == DISP8) {
					buffer->write_byte(disp);
				} else if (mod == DISP32) {
					buffer->write_int32(disp);
				}
			}
		}

		inline void x86_64_RegRm::write_modrm(GenBuffer* buffer, x8664Reg reg) {
			// modrm
			buffer->write_ubyte(modrm(reg));

			// Special register we need a sib byte
			if (rm == x8664Reg::RSP || rm == x8664Reg::R12) { // rsp/r12
				buffer->write_ubyte(sib());
			}

			// Special disp mod
			if (mod != DISP0) {
				if (mod == DISP8) {
					buffer->write_byte(disp);
				} else if (mod == DISP32) {
					buffer->write_int32(disp);
				}
			}
		}

		inline void x86_64_RegRm::write_modrm(GenBuffer* buffer, x8664FloatReg reg) {
			// modrm
			buffer->write_ubyte(modrm(reg));

			// Special register we need a sib byte
			if (rm == x8664Reg::RSP || rm == x8664Reg::R12) { // rsp/r12
				buffer->write_ubyte(sib());
			}

			// Special disp mod
			if (mod != DISP0) {
				if (mod == DISP8) {
					buffer->write_byte(disp);
				} else if (mod == DISP32) {
					buffer->write_int32(disp);
				}
			}
		}
		
		class x64JitWriter : public GenBuffer {
		public:
			void breakpoint() {
				this->write_ubyte(0xCC);
			}

			void rep_movs_bytes() {
				this->write_ubyte(0xF3);
				this->write_ubyte(0x48);
				this->write_ubyte(0xA4);
			}

			void call(x86_64_Reg reg) {
				if (reg.extended()) {
					this->write_ubyte(REX::B);
				}
				this->write_ubyte(0xFF);
				this->write_ubyte(0xD0 + reg.low());
			}

			void jump(x86_64_Reg reg) {
				if (reg.extended()) {
					this->write_ubyte(REX::B);
				}
				this->write_ubyte(0xFF);
				this->write_ubyte(0xE0 + reg.low());
			}

			void jump(std::int32_t off) {
				this->write_ubyte(0xE9);
				this->write_int32(off);
			}

			void jz(std::int32_t off) {
				this->write_ubyte(0x0F);
				this->write_ubyte(0x84);
				this->write_int32(off);
			}

			void jl(std::int32_t off) {
				this->write_ubyte(0x0F);
				this->write_ubyte(0x8C);
				this->write_int32(off);
			}

			void jle(std::int32_t off) {
				this->write_ubyte(0x0F);
				this->write_ubyte(0x8E);
				this->write_int32(off);
			}

			void je(std::int32_t off) {
				this->write_ubyte(0x0F);
				this->write_ubyte(0x84);
				this->write_int32(off);
			}

			void jne(std::int32_t off) {
				this->write_ubyte(0x0F);
				this->write_ubyte(0x85);
				this->write_int32(off);
			}

			void push(x86_64_Reg reg) {
				if (reg.extended()) {
					this->write_ubyte(REX::B);
				}
				this->write_ubyte(0x50 + reg.low());
			}

			void push(std::int32_t val) {
				if (val >= SCHAR_MIN && val <= SCHAR_MAX) {
					this->write_ubyte(0x6A);
					this->write_byte(std::int8_t(val));
				} else {
					this->write_ubyte(0x68);
					this->write_int32(val);
				}
			}

			void pop(x86_64_Reg reg) {
				if (reg.extended()) {
					this->write_ubyte(REX::B);
				}
				this->write_ubyte(0x58 + reg.low());
			}

			// mov_rm
			void mov(x86_64_Reg dst, x86_64_Reg src) {
				this->write_ubyte(w_rex(src, dst));
				this->write_ubyte(0x89);
				this->write_ubyte(modrm(src, dst));
			}

			void mov(x86_64_RegRm rm, x86_64_Reg reg) {
				this->write_ubyte(w_rex(reg, rm));
				this->write_ubyte(0x89);
				rm.write_modrm(this, reg);
			}

			void mov(x86_64_Reg reg, x86_64_RegRm rm) {
				this->write_ubyte(w_rex(reg, rm));
				this->write_ubyte(0x8B);
				rm.write_modrm(this, reg);
			}

			void mov(x86_64_Reg dst, std::int32_t imm) {
				if (dst.extended()) {
					this->write_ubyte(REX::B);
				}
				this->write_ubyte(0xB8 + dst.low());
				this->write_int32(imm);
			}

			void mov(x86_64_RegRm dst, std::int32_t imm) {
				if (dst.extended()) {
					this->write_ubyte(REX::WB);
				} else {
					this->write_ubyte(REX::W);
				}
				this->write_ubyte(0xC7);
				dst.write_modrm(this);
				this->write_int32(imm);
			}

			void mov(x86_64_Reg dst, std::uint64_t imm) {
				if (imm <= UINT32_MAX) {
					this->mov(dst, std::int32_t(imm));
					return;
				}

				if (dst.extended()) {
					this->write_ubyte(REX::WB);
				} else {
					this->write_ubyte(REX::W);
				}
				this->write_ubyte(0xB8 + dst.low());
				this->write_uint64(imm);
			}

			void movsd(x86_64_FloatReg reg, x86_64_RegRm rm) {
				this->write_ubyte(0xF2);
				this->write_ubyte(w_rex(reg, rm));
				this->write_ubyte(0x0F);
				this->write_ubyte(0x10);
				rm.write_modrm(this, reg);
			}

			void movsd(x86_64_RegRm rm, x86_64_FloatReg reg) {
				this->write_ubyte(0xF2);
				if (reg.extended() || rm.extended()) {
					this->write_ubyte(w_rex(reg, rm));
				}
				this->write_ubyte(0x0F);
				this->write_ubyte(0x11);
				rm.write_modrm(this, reg);
			}


			void add(x86_64_Reg dst, x86_64_Reg src) {
				this->write_ubyte(w_rex(src, dst));
				this->write_ubyte(0x01);
				this->write_ubyte(modrm(src, dst));
			}

			void add(x86_64_Reg dst, int32_t imm) {
				if (dst.extended()) {
					this->write_ubyte(REX::WB);
				} else {
					this->write_ubyte(REX::W);
				}
				this->write_ubyte(0x81);
				this->write_ubyte(modrm_rm(dst, 0));
				this->write_int32(imm);
			}

			void sub(x86_64_Reg dst, x86_64_Reg src) {
				this->write_ubyte(w_rex(src, dst));
				this->write_ubyte(0x29);
				this->write_ubyte(modrm(src, dst));
			}

			void sub(x86_64_Reg dst, int32_t imm) {
				if (dst.extended()) {
					this->write_ubyte(REX::WB);
				} else {
					this->write_ubyte(REX::W);
				}
				this->write_ubyte(0x81);
				this->write_ubyte(modrm_rm(dst, 5));
				this->write_int32(imm);
			}

			void xor(x86_64_Reg dst, x86_64_Reg src) {
				this->write_ubyte(w_rex(src, dst));
				this->write_ubyte(0x31);
				this->write_ubyte(modrm(src, dst));
			}

			void test(x86_64_Reg dst, x86_64_Reg src) {
				this->write_ubyte(w_rex(src, dst));
				this->write_ubyte(0x85);
				this->write_ubyte(modrm(src, dst));
			}

			void test(x86_64_Reg reg, int32_t imm) {
				if (reg.extended()) {
					this->write_ubyte(REX::WB);
				} else {
					this->write_ubyte(REX::W);
				}
				this->write_ubyte(0xF7);
				this->write_ubyte(modrm_rm(reg, 0));
				this->write_int32(imm);
			}

			void cmovne(x86_64_Reg reg, x86_64_RegRm rm) {
				this->write_ubyte(w_rex(reg, rm));
				this->write_ubyte(0x0F);
				this->write_ubyte(0x45);
				rm.write_modrm(this, reg);
			}

			void cmovne(x86_64_Reg reg, x86_64_Reg rm) {
				this->write_ubyte(w_rex(reg, rm));
				this->write_ubyte(0x0F);
				this->write_ubyte(0x45);
				this->write_ubyte(modrm(reg, rm));
			}

			void cmovnz(x86_64_Reg reg, x86_64_RegRm rm) {
				this->write_ubyte(w_rex(reg, rm));
				this->write_ubyte(0x0F);
				this->write_ubyte(0x45);
				rm.write_modrm(this, reg);
			}

			void cmovnz(x86_64_Reg reg, x86_64_Reg rm) {
				this->write_ubyte(w_rex(reg, rm));
				this->write_ubyte(0x0F);
				this->write_ubyte(0x45);
				this->write_ubyte(modrm(reg, rm));
			}

			void cmovge(x86_64_Reg reg, x86_64_RegRm rm) {
				this->write_ubyte(w_rex(reg, rm));
				this->write_ubyte(0x0F);
				this->write_ubyte(0x4D);
				rm.write_modrm(this, reg);
			}

			void cmovge(x86_64_Reg reg, x86_64_Reg rm) {
				this->write_ubyte(w_rex(reg, rm));
				this->write_ubyte(0x0F);
				this->write_ubyte(0x4D);
				this->write_ubyte(modrm(reg, rm));
			}

			void cmovg(x86_64_Reg reg, x86_64_RegRm rm) {
				this->write_ubyte(w_rex(reg, rm));
				this->write_ubyte(0x0F);
				this->write_ubyte(0x4F);
				rm.write_modrm(this, reg);
			}

			void cmovg(x86_64_Reg reg, x86_64_Reg rm) {
				this->write_ubyte(w_rex(reg, rm));
				this->write_ubyte(0x0F);
				this->write_ubyte(0x4F);
				this->write_ubyte(modrm(reg, rm));
			}

			void lea(x86_64_Reg reg, x86_64_RegRm rm) {
				this->write_ubyte(w_rex(reg, rm));
				this->write_ubyte(0x8D);
				rm.write_modrm(this, reg);
			}

			void cmp(x86_64_Reg reg, x86_64_RegRm rm) {
				this->write_ubyte(w_rex(reg, rm));
				this->write_ubyte(0x3B);
				rm.write_modrm(this, reg);
			}

			void cmp(x86_64_RegRm rm, x86_64_Reg reg) {
				this->write_ubyte(w_rex(reg, rm));
				this->write_ubyte(0x39);
				rm.write_modrm(this, reg);
			}

			void cmp(x86_64_Reg reg, x86_64_Reg rm) {
				this->write_ubyte(w_rex(reg, rm));
				this->write_ubyte(0x3B);
				this->write_ubyte(modrm(reg, rm));
			}

			void cmp(x86_64_Reg dst, int32_t imm) {
				if (dst.extended()) {
					this->write_ubyte(REX::WB);
				} else {
					this->write_ubyte(REX::W);
				}
				this->write_ubyte(0x81);
				this->write_ubyte(modrm_rm(dst, 7));
				this->write_int32(imm);
			}

			void retn() {
				this->write_ubyte(0xC3);
			}
		};
	}
}