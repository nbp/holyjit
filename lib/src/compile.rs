//! Compiles the LIR representation into executable code using the dynasm
//! plugin and library. The use of dynasm is only for having a working proof
//! of concept, but is unlikely to be part of the final solution.

use std::collections::HashMap;

use dynasmrt;
use dynasmrt::x64;
use dynasmrt::{DynasmApi, DynasmLabelApi};

use bincode;

use lir;

/// Structure used to store the generated code.
pub struct JitCode {
    code: dynasmrt::ExecutableBuffer,
    start: dynasmrt::AssemblyOffset,

}

impl JitCode {
    /// Compile a function based on the vector of bytes and the table of
    /// statics.
    pub fn compile(bytes: &[u8], defs: *const ()) -> Result<JitCode, Error> {
        let c = Compiler::new(defs.clone());
        c.compile(bytes)
    }

    /// Cast the current code into a fake class which implement the proper
    /// trait.
    pub fn get_fn<'ctx, Args, Output>(&self) -> &'ctx Fn<Args, Output = Output> {
        panic!("NYI!!!")
    }
}

#[derive(Debug)]
pub enum Error {
    /// Issue caused when deserializing the buffer which contains the Lir.
    Deserialize,

    /// The number of live registers exceeds the number of the architecture
    /// registers.  This should be handled by a better register allocator in
    /// the future.
    NotEnoughRegisters,

    /// Cannot finalize the code, and generate an executable page.
    Finalize,

    /// Cannot reuse deleted register mapping
    MissingRegisterMap,

    /// This is still a prototype.
    NYI,
}

impl<'tcx> From<Box<bincode::ErrorKind>> for Error {
    fn from(err: Box<bincode::ErrorKind>) -> Error {
        println!("bincode::ErrorKind = {}", err);
        Error::Deserialize
    }
}

mod asm {
    use dynasmrt;
    use dynasmrt::{DynasmApi, DynasmLabelApi};

    #[derive(Copy, Clone, Debug, PartialEq)]
    #[repr(u8)]
    pub enum Register {
        Rax = 0,
             Rcx, Rdx, Rbx,
        Rsp, Rbp, Rsi, Rdi,
        R8 , R9 , R10, R11,
        R12, R13, R14, R15,
    }

    // Conditions for CMP instructions (CMPSS, CMPSD, CMPPS, CMPPD, etc).
    #[derive(Copy, Clone, Debug)]
    #[repr(u8)]
    enum FpCondition {
        Eq    = 0x0,
        Lt    = 0x1,
        Le    = 0x2,
        UnOrd = 0x3,
        Neq   = 0x4,
        Nlt   = 0x5,
        Nle   = 0x6,
        Ord   = 0x7,
    }

    #[derive(Copy, Clone, Debug)]
    #[repr(u8)]
    pub enum Condition {
        O    = 0x0,
        NO,
        B,  // Carry
        AE, // no Carry
        E,
        NE,
        BE,
        A,
        S,
        NS,
        P,
        NP,
        L,
        GE,
        LE,
        G,
    }

    // These enumerated values are following the Intel documentation Volume 2C [1],
    // Appendix A.2 and Appendix A.3.
    //
    // Operand size/types as listed in the Appendix A.2.  Tables of the instructions
    // and their operands can be found in the Appendix A.3.
    //
    // E = reg/mem
    // G = reg (reg field of ModR/M)
    // U = xmm (R/M field of ModR/M)
    // V = xmm (reg field of ModR/M)
    // W = xmm/mem64
    // I = immediate
    // O = offset
    //
    // b = byte (8-bit)
    // w = word (16-bit)
    // v = register size
    // d = double (32-bit)
    // dq = double-quad (128-bit) (xmm)
    // ss = scalar float 32 (xmm)
    // ps = packed float 32 (xmm)
    // sd = scalar double (xmm)
    // pd = packed double (xmm)
    // z = 16/32/64-bit
    // vqp = (*)
    //
    // (*) Some website [2] provides a convenient list of all instructions, but be
    // aware that they do not follow the Intel documentation naming, as the
    // following enumeration does. Do not use these names as a reference for adding
    // new instructions.
    //
    // [1] http://www.intel.com/content/www/us/en/architecture-and-technology/64-ia-32-architectures-software-developer-manual-325462.html
    // [2] http://ref.x86asm.net/geek.html
    //
    // OPn_NAME_DstSrc
    #[derive(Copy, Clone, Debug)]
    enum OneByteOpcodeID {
        OP_NOP_00,
        OP_ADD_EbGb,
        OP_ADD_EvGv,
        OP_ADD_GvEv,
        OP_ADD_EAXIv,
        OP_OR_EbGb,
        OP_OR_EvGv,
        OP_OR_GvEv,
        OP_OR_EAXIv,
        OP_2BYTE_ESCAPE,
        OP_NOP_0F,
        OP_ADC_GvEv,
        OP_SBB_GvEv,
        OP_NOP_1F,
        OP_AND_EbGb,
        OP_AND_EvGv,
        OP_AND_GvEv,
        OP_AND_EAXIv,
        OP_SUB_EbGb,
        OP_SUB_EvGv,
        OP_SUB_GvEv,
        OP_SUB_EAXIv,
        PRE_PREDICT_BRANCH_NOT_TAKEN,
        OP_XOR_EbGb,
        OP_XOR_EvGv,
        OP_XOR_GvEv,
        OP_XOR_EAXIv,
        OP_CMP_EvGv,
        OP_CMP_GvEv,
        OP_CMP_EAXIv,
        PRE_REX,
        OP_NOP_40,
        OP_NOP_44,
        OP_PUSH_EAX,
        OP_POP_EAX,
        OP_PUSHA,
        OP_POPA,
        OP_MOVSXD_GvEv,
        PRE_OPERAND_SIZE,
        PRE_SSE_66,
        OP_NOP_66,
        OP_PUSH_Iz,
        OP_IMUL_GvEvIz,
        OP_PUSH_Ib,
        OP_IMUL_GvEvIb,
        OP_JCC_rel8,
        OP_GROUP1_EbIb,
        OP_NOP_80,
        OP_GROUP1_EvIz,
        OP_GROUP1_EvIb,
        OP_TEST_EbGb,
        OP_NOP_84,
        OP_TEST_EvGv,
        OP_XCHG_GbEb,
        OP_XCHG_GvEv,
        OP_MOV_EbGv,
        OP_MOV_EvGv,
        OP_MOV_GvEb,
        OP_MOV_GvEv,
        OP_LEA,
        OP_GROUP1A_Ev,
        OP_NOP,
        OP_PUSHFLAGS,
        OP_POPFLAGS,
        OP_CDQ,
        OP_MOV_EAXOv,
        OP_MOV_OvEAX,
        OP_TEST_EAXIb,
        OP_TEST_EAXIv,
        OP_MOV_EbIb,
        OP_MOV_EAXIv,
        OP_GROUP2_EvIb,
        OP_ADDP_ST0_ST1,
        OP_RET_Iz,
        PRE_VEX_C4,
        PRE_VEX_C5,
        OP_RET,
        OP_GROUP11_EvIb,
        OP_GROUP11_EvIz,
        OP_INT3,
        OP_GROUP2_Ev1,
        OP_GROUP2_EvCL,
        OP_FPU6,
        OP_FPU6_F32,
        OP_FPU6_ADDP,
        OP_FILD,
        OP_CALL_rel32,
        OP_JMP_rel32,
        OP_JMP_rel8,
        PRE_LOCK,
        PRE_SSE_F2,
        PRE_SSE_F3,
        OP_HLT,
        OP_GROUP3_EbIb,
        OP_GROUP3_Ev,
        OP_GROUP3_EvIz, // OP_GROUP3_Ev has an immediate, when instruction is a test.
        OP_GROUP5_Ev
    }

    #[derive(Copy, Clone, Debug)]
    enum TwoByteOpcodeID {
        OP2_UD2,
        OP2_MOVSD_VsdWsd,
        OP2_MOVPS_VpsWps,
        OP2_MOVSD_WsdVsd,
        OP2_MOVPS_WpsVps,
        OP2_MOVDDUP_VqWq,
        OP2_MOVHLPS_VqUq,
        OP2_MOVSLDUP_VpsWps,
        OP2_UNPCKLPS_VsdWsd,
        OP2_UNPCKHPS_VsdWsd,
        OP2_MOVLHPS_VqUq,
        OP2_MOVSHDUP_VpsWps,
        OP2_MOVAPD_VsdWsd,
        OP2_MOVAPS_VsdWsd,
        OP2_MOVAPS_WsdVsd,
        OP2_CVTSI2SD_VsdEd,
        OP2_CVTTSD2SI_GdWsd,
        OP2_UCOMISD_VsdWsd,
        OP2_CMOVZ_GvEv,
        OP2_MOVMSKPD_EdVd,
        OP2_ANDPS_VpsWps,
        OP2_ANDNPS_VpsWps,
        OP2_ORPS_VpsWps,
        OP2_XORPS_VpsWps,
        OP2_ADDSD_VsdWsd,
        OP2_ADDPS_VpsWps,
        OP2_MULSD_VsdWsd,
        OP2_MULPS_VpsWps,
        OP2_CVTSS2SD_VsdEd,
        OP2_CVTSD2SS_VsdEd,
        OP2_CVTTPS2DQ_VdqWps,
        OP2_CVTDQ2PS_VpsWdq,
        OP2_SUBSD_VsdWsd,
        OP2_SUBPS_VpsWps,
        OP2_MINSD_VsdWsd,
        OP2_MINSS_VssWss,
        OP2_MINPS_VpsWps,
        OP2_DIVSD_VsdWsd,
        OP2_DIVPS_VpsWps,
        OP2_MAXSD_VsdWsd,
        OP2_MAXSS_VssWss,
        OP2_MAXPS_VpsWps,
        OP2_SQRTSD_VsdWsd,
        OP2_SQRTSS_VssWss,
        OP2_SQRTPS_VpsWps,
        OP2_RSQRTPS_VpsWps,
        OP2_RCPPS_VpsWps,
        OP2_ANDPD_VpdWpd,
        OP2_ORPD_VpdWpd,
        OP2_XORPD_VpdWpd,
        OP2_PUNPCKLDQ,
        OP2_PCMPGTB_VdqWdq,
        OP2_PCMPGTW_VdqWdq,
        OP2_PCMPGTD_VdqWdq,
        OP2_MOVD_VdEd,
        OP2_MOVDQ_VsdWsd,
        OP2_MOVDQ_VdqWdq,
        OP2_PSHUFD_VdqWdqIb,
        OP2_PSHUFLW_VdqWdqIb,
        OP2_PSHUFHW_VdqWdqIb,
        OP2_PSLLW_UdqIb,
        OP2_PSRAW_UdqIb,
        OP2_PSRLW_UdqIb,
        OP2_PSLLD_UdqIb,
        OP2_PSRAD_UdqIb,
        OP2_PSRLD_UdqIb,
        OP2_PSRLDQ_Vd,
        OP2_PCMPEQB_VdqWdq,
        OP2_PCMPEQW_VdqWdq,
        OP2_PCMPEQD_VdqWdq,
        OP2_HADDPD,
        OP2_MOVD_EdVd,
        OP2_MOVQ_VdWd,
        OP2_MOVDQ_WdqVdq,
        OP2_JCC_rel32,
        OP_SETCC,
        OP2_SHLD,
        OP2_SHLD_GvEv,
        OP2_SHRD,
        OP2_SHRD_GvEv,
        OP_FENCE,
        OP2_IMUL_GvEv,
        OP2_CMPXCHG_GvEb,
        OP2_CMPXCHG_GvEw,
        OP2_POPCNT_GvEv,
        OP2_BSF_GvEv,
        OP2_BSR_GvEv,
        OP2_MOVSX_GvEb,
        OP2_MOVSX_GvEw,
        OP2_MOVZX_GvEb,
        OP2_MOVZX_GvEw,
        OP2_XADD_EbGb,
        OP2_XADD_EvGv,
        OP2_CMPPS_VpsWps,
        OP2_PINSRW,
        OP2_PEXTRW_GdUdIb,
        OP2_SHUFPS_VpsWpsIb,
        OP2_PSRLW_VdqWdq,
        OP2_PSRLD_VdqWdq,
        OP2_PMULLW_VdqWdq,
        OP2_MOVQ_WdVd,
        OP2_PMOVMSKB_EdVd,
        OP2_PSUBUSB_VdqWdq,
        OP2_PSUBUSW_VdqWdq,
        OP2_PANDDQ_VdqWdq,
        OP2_PADDUSB_VdqWdq,
        OP2_PADDUSW_VdqWdq,
        OP2_PANDNDQ_VdqWdq,
        OP2_PSRAW_VdqWdq,
        OP2_PSRAD_VdqWdq,
        OP2_PSUBSB_VdqWdq,
        OP2_PSUBSW_VdqWdq,
        OP2_PORDQ_VdqWdq,
        OP2_PADDSB_VdqWdq,
        OP2_PADDSW_VdqWdq,
        OP2_PXORDQ_VdqWdq,
        OP2_PSLLW_VdqWdq,
        OP2_PSLLD_VdqWdq,
        OP2_PMULUDQ_VdqWdq,
        OP2_PSUBB_VdqWdq,
        OP2_PSUBW_VdqWdq,
        OP2_PSUBD_VdqWdq,
        OP2_PADDB_VdqWdq,
        OP2_PADDW_VdqWdq,
        OP2_PADDD_VdqWdq
    }

    #[derive(Copy, Clone, Debug)]
    enum ThreeByteOpcodeID {
        OP3_PSHUFB_VdqWdq,
        OP3_ROUNDSS_VsdWsd,
        OP3_ROUNDSD_VsdWsd,
        OP3_BLENDVPS_VdqWdq,
        OP3_PEXTRB_EdVdqIb,
        OP3_PEXTRD_EdVdqIb,
        OP3_BLENDPS_VpsWpsIb,
        OP3_PTEST_VdVd,
        OP3_PINSRB_VdqEdIb,
        OP3_INSERTPS_VpsUps,
        OP3_PINSRD_VdqEdIb,
        OP3_PMULLD_VdqWdq,
        OP3_VBLENDVPS_VdqWdq
    }

    enum GroupOpcodeID {
        NOGROUP_OP_SETCC,

        GROUP1_OP_ADD,
        GROUP1_OP_OR,
        GROUP1_OP_ADC,
        GROUP1_OP_SBB,
        GROUP1_OP_AND,
        GROUP1_OP_SUB,
        GROUP1_OP_XOR,
        GROUP1_OP_CMP,

        GROUP1A_OP_POP,

        GROUP2_OP_ROL,
        GROUP2_OP_ROR,
        GROUP2_OP_SHL,
        GROUP2_OP_SHR,
        GROUP2_OP_SAR,

        GROUP3_OP_TEST,
        GROUP3_OP_NOT,
        GROUP3_OP_NEG,
        GROUP3_OP_MUL,
        GROUP3_OP_IMUL,
        GROUP3_OP_DIV,
        GROUP3_OP_IDIV,

        GROUP5_OP_INC,
        GROUP5_OP_DEC,
        GROUP5_OP_CALLN,
        GROUP5_OP_JMPN,
        GROUP5_OP_PUSH,

        FILD_OP_64,

        FPU6_OP_FLD,
        FPU6_OP_FISTTP,
        FPU6_OP_FSTP,
        FPU6_OP_FLDCW,
        FPU6_OP_FISTP,

        GROUP11_MOV
    }

    impl Into<u8> for OneByteOpcodeID {
        fn into(self) -> u8 {
            use self::OneByteOpcodeID::*;
            match self {
                OP_NOP_00                       => 0x00,
                OP_ADD_EbGb                     => 0x00,
                OP_ADD_EvGv                     => 0x01,
                OP_ADD_GvEv                     => 0x03,
                OP_ADD_EAXIv                    => 0x05,
                OP_OR_EbGb                      => 0x08,
                OP_OR_EvGv                      => 0x09,
                OP_OR_GvEv                      => 0x0B,
                OP_OR_EAXIv                     => 0x0D,
                OP_2BYTE_ESCAPE                 => 0x0F,
                OP_NOP_0F                       => 0x0F,
                OP_ADC_GvEv                     => 0x13,
                OP_SBB_GvEv                     => 0x1B,
                OP_NOP_1F                       => 0x1F,
                OP_AND_EbGb                     => 0x20,
                OP_AND_EvGv                     => 0x21,
                OP_AND_GvEv                     => 0x23,
                OP_AND_EAXIv                    => 0x25,
                OP_SUB_EbGb                     => 0x28,
                OP_SUB_EvGv                     => 0x29,
                OP_SUB_GvEv                     => 0x2B,
                OP_SUB_EAXIv                    => 0x2D,
                PRE_PREDICT_BRANCH_NOT_TAKEN    => 0x2E,
                OP_XOR_EbGb                     => 0x30,
                OP_XOR_EvGv                     => 0x31,
                OP_XOR_GvEv                     => 0x33,
                OP_XOR_EAXIv                    => 0x35,
                OP_CMP_EvGv                     => 0x39,
                OP_CMP_GvEv                     => 0x3B,
                OP_CMP_EAXIv                    => 0x3D,
                PRE_REX                         => 0x40,
                OP_NOP_40                       => 0x40,
                OP_NOP_44                       => 0x44,
                OP_PUSH_EAX                     => 0x50,
                OP_POP_EAX                      => 0x58,
                OP_PUSHA                        => 0x60,
                OP_POPA                         => 0x61,
                OP_MOVSXD_GvEv                  => 0x63,
                PRE_OPERAND_SIZE                => 0x66,
                PRE_SSE_66                      => 0x66,
                OP_NOP_66                       => 0x66,
                OP_PUSH_Iz                      => 0x68,
                OP_IMUL_GvEvIz                  => 0x69,
                OP_PUSH_Ib                      => 0x6a,
                OP_IMUL_GvEvIb                  => 0x6b,
                OP_JCC_rel8                     => 0x70,
                OP_GROUP1_EbIb                  => 0x80,
                OP_NOP_80                       => 0x80,
                OP_GROUP1_EvIz                  => 0x81,
                OP_GROUP1_EvIb                  => 0x83,
                OP_TEST_EbGb                    => 0x84,
                OP_NOP_84                       => 0x84,
                OP_TEST_EvGv                    => 0x85,
                OP_XCHG_GbEb                    => 0x86,
                OP_XCHG_GvEv                    => 0x87,
                OP_MOV_EbGv                     => 0x88,
                OP_MOV_EvGv                     => 0x89,
                OP_MOV_GvEb                     => 0x8A,
                OP_MOV_GvEv                     => 0x8B,
                OP_LEA                          => 0x8D,
                OP_GROUP1A_Ev                   => 0x8F,
                OP_NOP                          => 0x90,
                OP_PUSHFLAGS                    => 0x9C,
                OP_POPFLAGS                     => 0x9D,
                OP_CDQ                          => 0x99,
                OP_MOV_EAXOv                    => 0xA1,
                OP_MOV_OvEAX                    => 0xA3,
                OP_TEST_EAXIb                   => 0xA8,
                OP_TEST_EAXIv                   => 0xA9,
                OP_MOV_EbIb                     => 0xB0,
                OP_MOV_EAXIv                    => 0xB8,
                OP_GROUP2_EvIb                  => 0xC1,
                OP_ADDP_ST0_ST1                 => 0xC1,
                OP_RET_Iz                       => 0xC2,
                PRE_VEX_C4                      => 0xC4,
                PRE_VEX_C5                      => 0xC5,
                OP_RET                          => 0xC3,
                OP_GROUP11_EvIb                 => 0xC6,
                OP_GROUP11_EvIz                 => 0xC7,
                OP_INT3                         => 0xCC,
                OP_GROUP2_Ev1                   => 0xD1,
                OP_GROUP2_EvCL                  => 0xD3,
                OP_FPU6                         => 0xDD,
                OP_FPU6_F32                     => 0xD9,
                OP_FPU6_ADDP                    => 0xDE,
                OP_FILD                         => 0xDF,
                OP_CALL_rel32                   => 0xE8,
                OP_JMP_rel32                    => 0xE9,
                OP_JMP_rel8                     => 0xEB,
                PRE_LOCK                        => 0xF0,
                PRE_SSE_F2                      => 0xF2,
                PRE_SSE_F3                      => 0xF3,
                OP_HLT                          => 0xF4,
                OP_GROUP3_EbIb                  => 0xF6,
                OP_GROUP3_Ev                    => 0xF7,
                OP_GROUP3_EvIz                  => 0xF7, // OP_GROUP3_Ev has an immediate, when instruction is a test.
                OP_GROUP5_Ev                    => 0xFF
            }
        }
    }

    impl Into<u8> for TwoByteOpcodeID {
        fn into(self) -> u8 {
            use self::TwoByteOpcodeID::*;
            match self {
                OP2_UD2             => 0x0B,
                OP2_MOVSD_VsdWsd    => 0x10,
                OP2_MOVPS_VpsWps    => 0x10,
                OP2_MOVSD_WsdVsd    => 0x11,
                OP2_MOVPS_WpsVps    => 0x11,
                OP2_MOVDDUP_VqWq    => 0x12,
                OP2_MOVHLPS_VqUq    => 0x12,
                OP2_MOVSLDUP_VpsWps => 0x12,
                OP2_UNPCKLPS_VsdWsd => 0x14,
                OP2_UNPCKHPS_VsdWsd => 0x15,
                OP2_MOVLHPS_VqUq    => 0x16,
                OP2_MOVSHDUP_VpsWps => 0x16,
                OP2_MOVAPD_VsdWsd   => 0x28,
                OP2_MOVAPS_VsdWsd   => 0x28,
                OP2_MOVAPS_WsdVsd   => 0x29,
                OP2_CVTSI2SD_VsdEd  => 0x2A,
                OP2_CVTTSD2SI_GdWsd => 0x2C,
                OP2_UCOMISD_VsdWsd  => 0x2E,
                OP2_CMOVZ_GvEv      => 0x44,
                OP2_MOVMSKPD_EdVd   => 0x50,
                OP2_ANDPS_VpsWps    => 0x54,
                OP2_ANDNPS_VpsWps   => 0x55,
                OP2_ORPS_VpsWps     => 0x56,
                OP2_XORPS_VpsWps    => 0x57,
                OP2_ADDSD_VsdWsd    => 0x58,
                OP2_ADDPS_VpsWps    => 0x58,
                OP2_MULSD_VsdWsd    => 0x59,
                OP2_MULPS_VpsWps    => 0x59,
                OP2_CVTSS2SD_VsdEd  => 0x5A,
                OP2_CVTSD2SS_VsdEd  => 0x5A,
                OP2_CVTTPS2DQ_VdqWps => 0x5B,
                OP2_CVTDQ2PS_VpsWdq => 0x5B,
                OP2_SUBSD_VsdWsd    => 0x5C,
                OP2_SUBPS_VpsWps    => 0x5C,
                OP2_MINSD_VsdWsd    => 0x5D,
                OP2_MINSS_VssWss    => 0x5D,
                OP2_MINPS_VpsWps    => 0x5D,
                OP2_DIVSD_VsdWsd    => 0x5E,
                OP2_DIVPS_VpsWps    => 0x5E,
                OP2_MAXSD_VsdWsd    => 0x5F,
                OP2_MAXSS_VssWss    => 0x5F,
                OP2_MAXPS_VpsWps    => 0x5F,
                OP2_SQRTSD_VsdWsd   => 0x51,
                OP2_SQRTSS_VssWss   => 0x51,
                OP2_SQRTPS_VpsWps   => 0x51,
                OP2_RSQRTPS_VpsWps  => 0x52,
                OP2_RCPPS_VpsWps    => 0x53,
                OP2_ANDPD_VpdWpd    => 0x54,
                OP2_ORPD_VpdWpd     => 0x56,
                OP2_XORPD_VpdWpd    => 0x57,
                OP2_PUNPCKLDQ       => 0x62,
                OP2_PCMPGTB_VdqWdq  => 0x64,
                OP2_PCMPGTW_VdqWdq  => 0x65,
                OP2_PCMPGTD_VdqWdq  => 0x66,
                OP2_MOVD_VdEd       => 0x6E,
                OP2_MOVDQ_VsdWsd    => 0x6F,
                OP2_MOVDQ_VdqWdq    => 0x6F,
                OP2_PSHUFD_VdqWdqIb => 0x70,
                OP2_PSHUFLW_VdqWdqIb => 0x70,
                OP2_PSHUFHW_VdqWdqIb => 0x70,
                OP2_PSLLW_UdqIb     => 0x71,
                OP2_PSRAW_UdqIb     => 0x71,
                OP2_PSRLW_UdqIb     => 0x71,
                OP2_PSLLD_UdqIb     => 0x72,
                OP2_PSRAD_UdqIb     => 0x72,
                OP2_PSRLD_UdqIb     => 0x72,
                OP2_PSRLDQ_Vd       => 0x73,
                OP2_PCMPEQB_VdqWdq  => 0x74,
                OP2_PCMPEQW_VdqWdq  => 0x75,
                OP2_PCMPEQD_VdqWdq  => 0x76,
                OP2_HADDPD          => 0x7C,
                OP2_MOVD_EdVd       => 0x7E,
                OP2_MOVQ_VdWd       => 0x7E,
                OP2_MOVDQ_WdqVdq    => 0x7F,
                OP2_JCC_rel32       => 0x80,
                OP_SETCC            => 0x90,
                OP2_SHLD            => 0xA4,
                OP2_SHLD_GvEv       => 0xA5,
                OP2_SHRD            => 0xAC,
                OP2_SHRD_GvEv       => 0xAD,
                OP_FENCE            => 0xAE,
                OP2_IMUL_GvEv       => 0xAF,
                OP2_CMPXCHG_GvEb    => 0xB0,
                OP2_CMPXCHG_GvEw    => 0xB1,
                OP2_POPCNT_GvEv     => 0xB8,
                OP2_BSF_GvEv        => 0xBC,
                OP2_BSR_GvEv        => 0xBD,
                OP2_MOVSX_GvEb      => 0xBE,
                OP2_MOVSX_GvEw      => 0xBF,
                OP2_MOVZX_GvEb      => 0xB6,
                OP2_MOVZX_GvEw      => 0xB7,
                OP2_XADD_EbGb       => 0xC0,
                OP2_XADD_EvGv       => 0xC1,
                OP2_CMPPS_VpsWps    => 0xC2,
                OP2_PINSRW          => 0xC4,
                OP2_PEXTRW_GdUdIb   => 0xC5,
                OP2_SHUFPS_VpsWpsIb => 0xC6,
                OP2_PSRLW_VdqWdq    => 0xD1,
                OP2_PSRLD_VdqWdq    => 0xD2,
                OP2_PMULLW_VdqWdq   => 0xD5,
                OP2_MOVQ_WdVd       => 0xD6,
                OP2_PMOVMSKB_EdVd   => 0xD7,
                OP2_PSUBUSB_VdqWdq  => 0xD8,
                OP2_PSUBUSW_VdqWdq  => 0xD9,
                OP2_PANDDQ_VdqWdq   => 0xDB,
                OP2_PADDUSB_VdqWdq  => 0xDC,
                OP2_PADDUSW_VdqWdq  => 0xDD,
                OP2_PANDNDQ_VdqWdq  => 0xDF,
                OP2_PSRAW_VdqWdq    => 0xE1,
                OP2_PSRAD_VdqWdq    => 0xE2,
                OP2_PSUBSB_VdqWdq   => 0xE8,
                OP2_PSUBSW_VdqWdq   => 0xE9,
                OP2_PORDQ_VdqWdq    => 0xEB,
                OP2_PADDSB_VdqWdq   => 0xEC,
                OP2_PADDSW_VdqWdq   => 0xED,
                OP2_PXORDQ_VdqWdq   => 0xEF,
                OP2_PSLLW_VdqWdq    => 0xF1,
                OP2_PSLLD_VdqWdq    => 0xF2,
                OP2_PMULUDQ_VdqWdq  => 0xF4,
                OP2_PSUBB_VdqWdq    => 0xF8,
                OP2_PSUBW_VdqWdq    => 0xF9,
                OP2_PSUBD_VdqWdq    => 0xFA,
                OP2_PADDB_VdqWdq    => 0xFC,
                OP2_PADDW_VdqWdq    => 0xFD,
                OP2_PADDD_VdqWdq    => 0xFE
            }
        }
    }

    impl Into<u8> for ThreeByteOpcodeID {
        fn into(self) -> u8 {
            use self::ThreeByteOpcodeID::*;
            match self {
                OP3_PSHUFB_VdqWdq   => 0x00,
                OP3_ROUNDSS_VsdWsd  => 0x0A,
                OP3_ROUNDSD_VsdWsd  => 0x0B,
                OP3_BLENDVPS_VdqWdq => 0x14,
                OP3_PEXTRB_EdVdqIb  => 0x14,
                OP3_PEXTRD_EdVdqIb  => 0x16,
                OP3_BLENDPS_VpsWpsIb => 0x0C,
                OP3_PTEST_VdVd      => 0x17,
                OP3_PINSRB_VdqEdIb  => 0x20,
                OP3_INSERTPS_VpsUps => 0x21,
                OP3_PINSRD_VdqEdIb  => 0x22,
                OP3_PMULLD_VdqWdq   => 0x40,
                OP3_VBLENDVPS_VdqWdq => 0x4A
            }
        }
    }

    impl Into<u8> for GroupOpcodeID {
        fn into(self) -> u8 {
            use self::GroupOpcodeID::*;
            match self {
                NOGROUP_OP_SETCC => 0,

                GROUP1_OP_ADD => 0,
                GROUP1_OP_OR  => 1,
                GROUP1_OP_ADC => 2,
                GROUP1_OP_SBB => 3,
                GROUP1_OP_AND => 4,
                GROUP1_OP_SUB => 5,
                GROUP1_OP_XOR => 6,
                GROUP1_OP_CMP => 7,

                GROUP1A_OP_POP => 0,

                GROUP2_OP_ROL => 0,
                GROUP2_OP_ROR => 1,
                GROUP2_OP_SHL => 4,
                GROUP2_OP_SHR => 5,
                GROUP2_OP_SAR => 7,

                GROUP3_OP_TEST => 0,
                GROUP3_OP_NOT  => 2,
                GROUP3_OP_NEG  => 3,
                GROUP3_OP_MUL  => 4,
                GROUP3_OP_IMUL => 5,
                GROUP3_OP_DIV  => 6,
                GROUP3_OP_IDIV => 7,

                GROUP5_OP_INC   => 0,
                GROUP5_OP_DEC   => 1,
                GROUP5_OP_CALLN => 2,
                GROUP5_OP_JMPN  => 4,
                GROUP5_OP_PUSH  => 6,

                FILD_OP_64      => 5,

                FPU6_OP_FLD     => 0,
                FPU6_OP_FISTTP  => 1,
                FPU6_OP_FSTP    => 3,
                FPU6_OP_FLDCW   => 5,
                FPU6_OP_FISTP   => 7,

                GROUP11_MOV => 0
            }
        }
    }

    #[derive(Copy, Clone, Debug, PartialEq)]
    #[repr(u8)]
    enum ModRm {
        MemoryNoDisp = 0,
        MemoryDisp8,
        MemoryDisp32,
        Register
    }

    impl Register {
        fn requires_rex(&self) -> bool {
            *self as u8 >= Register::R8 as u8
        }
        fn byte_requires_rex(&self) -> bool {
            *self as u8 >= Register::Rsp as u8
        }
    }

    fn can_sign_extend_from_i8(value: i32) -> bool {
        value as i8 as i32 == value
    }

    pub struct Assembler {
        pub backend: dynasmrt::x64::Assembler,
    }

    const noBase : Register = Register::Rbp;
    const hasSib : Register = Register::Rsp;
    const noIndex : Register = Register::Rsp;

    const noBase2 : Register = Register::R13;
    const hasSib2 : Register = Register::R12;

    impl Assembler {
        fn emit_rex(&mut self, w: bool, r: Register, x: Register, b: Register) {
            let w = w as u8;
            let r = r as u8;
            let x = x as u8;
            let b = b as u8;
            self.backend.push(OneByteOpcodeID::PRE_REX as u8 | (w << 3) | ((r >> 3) << 2) | ((x >> 3) << 1) | (b >> 3));
        }
        fn emit_rex_w(&mut self, r: Register, x: Register, b: Register) {
            self.emit_rex(true, r, x, b);
        }
        fn emit_rex_if(&mut self, cond: bool, r: Register, x: Register, b: Register) {
            if cond || r.requires_rex() || x.requires_rex() || b.requires_rex() {
                self.emit_rex(false, r, x, b);
            }
        }
        fn emit_rex_if_needed(&mut self, r: Register, x: Register, b: Register) {
            self.emit_rex_if(false, r, x, b);
        }

        fn put_modrm_u8(&mut self, mode: ModRm, rm: u8, reg: u8)
        {
            let mut byte : u8 = (mode as u8) << 6;
            byte = byte | (reg & 7) << 3;
            byte = byte | rm & 7;
            self.backend.push(byte);
        }

        fn put_modrm(&mut self, mode: ModRm, rm: Register, reg: Register)
        {
            self.put_modrm_u8(mode, rm as u8, reg as u8)
        }

        fn put_modrm_grp(&mut self, mode: ModRm, rm: Register, grp: GroupOpcodeID)
        {
            self.put_modrm_u8(mode, rm as u8, grp as u8)
        }

        fn put_modrm_sib(&mut self, mode: ModRm, base: Register, index: Register, scale: u8, reg: Register)
        {
            assert_ne!(mode, ModRm::Register);

            self.put_modrm(mode, hasSib, reg);
            let mut byte : u8 = scale << 6;
            byte = byte | ((index as u8) & 7) << 3;
            byte = byte | (base as u8) & 7;
            self.backend.push(byte);
        }

        fn register_modrm(&mut self, rm: Register, reg: Register)
        {
            self.put_modrm(ModRm::Register, rm, reg);
        }
        fn memory_modrm(&mut self, offset: i32, base: Register, reg: Register)
        {
            // A base of esp or r12 would be interpreted as a sib, so force a
            // sib with no index & put the base in there.
            if (base == hasSib) || (base == hasSib2) {
                if offset == 0 {
                    // No need to check if the base is noBase, since we know it is hasSib!
                    self.put_modrm_sib(ModRm::MemoryNoDisp, base, noIndex, 0, reg);
                } else if can_sign_extend_from_i8(offset) {
                    self.put_modrm_sib(ModRm::MemoryDisp8, base, noIndex, 0, reg);
                    self.backend.push_i8(offset as i8);
                } else {
                    self.put_modrm_sib(ModRm::MemoryDisp32, base, noIndex, 0, reg);
                    self.backend.push_i32(offset);
                }
            } else {
                if offset == 0 && (base != noBase) && (base != noBase2) {
                    self.put_modrm(ModRm::MemoryNoDisp, base, reg);
                } else if can_sign_extend_from_i8(offset) {
                    self.put_modrm(ModRm::MemoryDisp8, base, reg);
                    self.backend.push_i8(offset as i8);
                } else {
                    self.put_modrm(ModRm::MemoryDisp32, base, reg);
                    self.backend.push_i32(offset);
                }
            }
        }

        fn one_op(&mut self, opcode: OneByteOpcodeID) {
            self.backend.push(opcode.into());
        }
        fn one_op_reg(&mut self, opcode: OneByteOpcodeID, reg: Register) {
            self.emit_rex_if_needed(Register::Rax, Register::Rax, reg);
            self.backend.push(opcode.into());
        }
        fn one_op_rm_reg(&mut self, opcode: OneByteOpcodeID, rm: Register, reg: Register) {
            self.emit_rex_if_needed(reg, Register::Rax, rm);
            self.backend.push(opcode.into());
            self.register_modrm(rm, reg)
        }
        fn one_op64_rm_reg(&mut self, opcode: OneByteOpcodeID, rm: Register, reg: Register) {
            self.emit_rex_w(reg, Register::Rax, rm);
            self.backend.push(opcode.into());
            self.register_modrm(rm, reg)
        }
        fn one_op8_mm_reg(&mut self, opcode: OneByteOpcodeID, base: Register, offset: i32, reg: Register) {
            self.emit_rex_if(reg.byte_requires_rex(), reg, Register::Rax, base);
            self.backend.push(opcode.into());
            self.memory_modrm(offset, base, reg)
        }
        fn one_op_mm_reg(&mut self, opcode: OneByteOpcodeID, base: Register, offset: i32, reg: Register) {
            self.emit_rex_if_needed(reg, Register::Rax, base);
            self.backend.push(opcode.into());
            self.memory_modrm(offset, base, reg)
        }
        fn one_op64_mm_reg(&mut self, opcode: OneByteOpcodeID, base: Register, offset: i32, reg: Register) {
            self.emit_rex_w(reg, Register::Rax, base);
            self.backend.push(opcode.into());
            self.memory_modrm(offset, base, reg)
        }
        fn two_op8_cc_reg(&mut self, opcode: TwoByteOpcodeID, cond: Condition, rm: Register, grp: GroupOpcodeID) {
            self.emit_rex_if(rm.byte_requires_rex(), Register::Rax, Register::Rax, rm);
            self.backend.push(OneByteOpcodeID::OP_2BYTE_ESCAPE.into());
            let mut opcode : u8 = opcode.into();
            opcode += cond as u8;
            self.backend.push(opcode);
            self.put_modrm_grp(ModRm::Register, rm, grp);
        }
        fn two_op64_rm_reg(&mut self, opcode: TwoByteOpcodeID, rm: Register, reg: Register) {
            self.emit_rex_w(reg, Register::Rax, rm);
            self.backend.push(OneByteOpcodeID::OP_2BYTE_ESCAPE.into());
            self.backend.push(opcode.into());
            self.register_modrm(rm, reg);
        }

        // Set of Instructions used by the compiler.

        pub fn addq_rr(&mut self, src: Register, dst: Register) {
            self.one_op64_rm_reg(OneByteOpcodeID::OP_ADD_GvEv, src, dst)
        }
        pub fn imulq_rr(&mut self, src: Register, dst: Register) {
            self.two_op64_rm_reg(TwoByteOpcodeID::OP2_IMUL_GvEv, src, dst)
        }
        /// Compare 16 bits registers
        pub fn cmpw_rr(&mut self, lhs: Register, rhs: Register) {
            // Note, inverted arguments to make them logical.
            self.backend.push(OneByteOpcodeID::PRE_OPERAND_SIZE.into());
            self.one_op_rm_reg(OneByteOpcodeID::OP_CMP_GvEv, rhs, lhs)
        }
        /// Compare 32 bits registers
        pub fn cmpl_rr(&mut self, lhs: Register, rhs: Register) {
            // Note, inverted arguments to make them logical.
            self.one_op_rm_reg(OneByteOpcodeID::OP_CMP_GvEv, rhs, lhs)
        }
        /// Compare 64 bits registers
        pub fn cmpq_rr(&mut self, lhs: Register, rhs: Register) {
            // Note, inverted arguments to make them logical.
            self.one_op64_rm_reg(OneByteOpcodeID::OP_CMP_GvEv, rhs, lhs)
        }
        /// Load 8 bits
        pub fn movb_mr(&mut self, base: Register, offset: i32, dst: Register) {
            self.one_op8_mm_reg(OneByteOpcodeID::OP_MOV_GvEb, base, offset, dst)
        }
        /// Load 16 bits
        pub fn movw_mr(&mut self, base: Register, offset: i32, dst: Register) {
            self.backend.push(OneByteOpcodeID::PRE_OPERAND_SIZE.into());
            self.one_op_mm_reg(OneByteOpcodeID::OP_MOV_GvEv, base, offset, dst)
        }
        /// Load 32 bits
        pub fn movl_mr(&mut self, base: Register, offset: i32, dst: Register) {
            self.one_op_mm_reg(OneByteOpcodeID::OP_MOV_GvEv, base, offset, dst)
        }
        /// Load 64 bits
        pub fn movq_mr(&mut self, base: Register, offset: i32, dst: Register) {
            self.one_op64_mm_reg(OneByteOpcodeID::OP_MOV_GvEv, base, offset, dst)
        }

        /// Store 8 bits
        pub fn movb_rm(&mut self, src: Register, base: Register, offset: i32) {
            self.one_op8_mm_reg(OneByteOpcodeID::OP_MOV_EbGv, base, offset, src)
        }
        /// Store 16 bits
        pub fn movw_rm(&mut self, src: Register, base: Register, offset: i32) {
            self.backend.push(OneByteOpcodeID::PRE_OPERAND_SIZE.into());
            self.one_op_mm_reg(OneByteOpcodeID::OP_MOV_EvGv, base, offset, src)
        }
        /// Store 32 bits
        pub fn movl_rm(&mut self, src: Register, base: Register, offset: i32) {
            self.one_op_mm_reg(OneByteOpcodeID::OP_MOV_EvGv, base, offset, src)
        }
        /// Store 64 bits
        pub fn movq_rm(&mut self, src: Register, base: Register, offset: i32) {
            self.one_op64_mm_reg(OneByteOpcodeID::OP_MOV_EvGv, base, offset, src)
        }
        /// Store condition flag in a 8 bits register, the upper bits of the
        /// destination register remain unchanged.
        pub fn setcc_r(&mut self, cc: Condition, out: Register) {
            self.two_op8_cc_reg(TwoByteOpcodeID::OP_SETCC, cc, out, GroupOpcodeID::NOGROUP_OP_SETCC);
        }
    }
}

use self::asm::Assembler;
use self::asm::Register;
use self::asm::Condition;

#[derive(Copy, Clone, Debug)]
struct AllocInfo {
    reg: Register,
    off: usize,
    sz: usize,
}
type Allocation = Vec<AllocInfo>;

/// The compiler state.
struct Compiler {
    /// The underlying assembler.
    asm: Assembler,
    /// The offset of the starting instruction.
    start: dynasmrt::AssemblyOffset,
    /// Set of labels for linking blocks with each others.
    bb_labels: Vec<dynasmrt::DynamicLabel>,
    /// Register map
    reg_map: HashMap<lir::Reg, Allocation>,
    free_regs: Vec<Register>,
    /// List of static variables stored in a raw pointer.
    statics: *const (),
}

impl Compiler {
    fn new(statics: *const ()) -> Compiler {
        use compile::asm::Register::*;
        let asm = x64::Assembler::new();
        let start = asm.offset();
        Compiler {
            asm: Assembler { backend: asm },
            start: start,
            bb_labels: vec![],
            reg_map: HashMap::new(),
            // Rbp and Rsp are reserved as a frame pointer and the stack
            // pointer.
            free_regs: vec![Rax, Rcx, Rdx, Rbx,           Rsi, Rdi,
                            R8 , R9 , R10, R11, R12, R13, R14, R15],
            statics: statics,
        }
    }

    fn compile(mut self, bytes: &[u8]) -> Result<JitCode, Error> {
        let cu : lir::CompilationUnit = bincode::deserialize(bytes)?;

        // For each block, create a new dynamic label which identify the
        // entry of each block.
        for _ in 0..cu.blocks.len() {
            let label = self.asm.backend.new_dynamic_label();
            self.bb_labels.push(label);
        }

        for block in cu.blocks.iter() {
            self.compile_block(block)?
        }

        self.finalize()
    }

    fn compile_block(&mut self, block: &lir::BasicBlockData) -> Result<(), Error> {
        for inst in block.insts.iter() {
            self.compile_inst(inst)?
        }

        self.compile_terminator(&block.end)?;

        Ok(())
    }

    fn allocate(&mut self, sz: usize) -> Result<Register, Error> {
        match self.free_regs.pop() {
            Some(reg) => Ok(reg),
            None => Err(Error::NotEnoughRegisters)
        }
    }

    fn register(&mut self, r: lir::Reg, alloc: Allocation) {
        self.reg_map.insert(r, alloc);
    }

    fn free(&mut self, r: lir::Reg) {
        match self.reg_map.remove(&r) {
            Some(l) => {
                for allocInfo in l {
                    self.free_regs.push(allocInfo.reg);
                }
            }
            None => (),
        }
    }

    fn reuse(&mut self, from: lir::Reg, to: lir::Reg) -> Result<(), Error> {
        match self.reg_map.remove(&from) {
            Some(alloc) => {
                self.register(to, alloc);
                Ok(())
            }
            None => Err(Error::MissingRegisterMap),
        }
    }

    fn reuse_append(&mut self, from: lir::Reg, to: lir::Reg, mut rest: Allocation) -> Result<(), Error> {
        match self.reg_map.remove(&from) {
            Some(mut alloc) => {
                alloc.append(&mut rest);
                self.register(to, alloc);
                Ok(())
            }
            None => Err(Error::MissingRegisterMap),
        }
    }

    fn compile_inst(&mut self, inst: &lir::Inst) -> Result<(), Error> {
        use lir::Inst::*;
        match inst {
            &SetFramePtr(fp, sz, stack_size) => {
                dynasm!(self.asm.backend
                        ; push rbp
                        ; mov rbp, rsp
                        ; sub rsp, stack_size as _
                );
                self.register(fp, vec![AllocInfo{ reg: Register::Rbp, off: 0, sz: 8 }]);
            },
            &Static(out, static_offset, static_size) => {
                // TODO: Handle various sizes, and save the loaded content
                // somewhere.
                let reg = self.allocate(static_size)?;
                self.register(out, vec![AllocInfo{ reg, off: 0, sz: static_size }]);
            },
            &CopyImm(out, value, sz) => {
                let reg = self.allocate(sz)?;
                self.register(out, vec![AllocInfo{ reg, sz, off: 0 }]);
                // TODO: move the value to the given registers.
                match sz {
                    _ => return Err(Error::NYI),
                }
            }
            &Resize(out, input, sz) => {
                let input = self.reg_map[&input].clone();
                let reg = self.allocate(sz)?;
                self.register(out, vec![AllocInfo{ reg, sz, off: 0 }]);
                // TODO: sign-extend?
            },
            &Add(out, lhs, rhs) => {
                let lhs_alloc = self.reg_map[&lhs].clone();
                let rhs_alloc = self.reg_map[&rhs].clone();
                assert_eq!(lhs_alloc.len(), 1);
                assert_eq!(rhs_alloc.len(), 1);
                let AllocInfo{ reg: lr, sz: ls, .. } = lhs_alloc[0];
                let AllocInfo{ reg: rr, sz: rs, .. } = rhs_alloc[0];
                // Dirty: Assumes that operands are always consumed, except
                // for the frame pointer.
                if lr != Register::Rbp {
                    self.asm.addq_rr(lr, rr);
                    self.reuse(lhs, out);
                } else {
                    self.asm.addq_rr(rr, lr);
                    self.reuse(rhs, out);
                }
            },
            &Sub(out, lhs, rhs) => { return Err(Error::NYI) },
            &Mul(out, lhs, rhs) => {
                let lhs_alloc = self.reg_map[&lhs].clone();
                let rhs_alloc = self.reg_map[&rhs].clone();
                assert_eq!(lhs_alloc.len(), 1);
                assert_eq!(rhs_alloc.len(), 1);
                let AllocInfo{ reg: lr, sz: ls, .. } = lhs_alloc[0];
                let AllocInfo{ reg: rr, sz: rs, .. } = rhs_alloc[0];
                // Dirty: Assumes that operands are always consumed, except
                // for the frame pointer.
                if lr != Register::Rbp {
                    self.asm.imulq_rr(lr, rr);
                    self.reuse(lhs, out);
                } else {
                    self.asm.imulq_rr(rr, lr);
                    self.reuse(rhs, out);
                }
            },
            &Div(out, lhs, rhs) => { return Err(Error::NYI) },
            &Rem(out, lhs, rhs) => { return Err(Error::NYI) },
            &BitXor(out, lhs, rhs) => { return Err(Error::NYI) },
            &BitAnd(out, lhs, rhs) => { return Err(Error::NYI) },
            &BitOr(out, lhs, rhs) => { return Err(Error::NYI) },
            &Shl(out, lhs, rhs) => { return Err(Error::NYI) },
            &Shr(out, lhs, rhs) => { return Err(Error::NYI) },
            &Eq(out, lhs, rhs) => {
                let lhs_alloc = self.reg_map[&lhs].clone();
                let rhs_alloc = self.reg_map[&rhs].clone();
                let reg = self.allocate(1)?;
                // TODO: handle size 1 separately, as the content might not
                // be signed-extended, such as with the setcc instruction.
                self.asm.cmpq_rr(lhs_alloc[0].reg, rhs_alloc[0].reg);
                self.asm.setcc_r(Condition::E, reg);
                self.register(out, vec![AllocInfo{ reg, off:0, sz: 1 }]);
            },
            &Lt(out, lhs, rhs) => { return Err(Error::NYI) },
            &Le(out, lhs, rhs) => { return Err(Error::NYI) },
            &Ne(out, lhs, rhs) => { return Err(Error::NYI) },
            &Gt(out, lhs, rhs) => { return Err(Error::NYI) },
            &Ge(out, lhs, rhs) => { return Err(Error::NYI) },

            &Chk(outAndFlags, out) => {
                // Warning: This instructions assumes that no other
                // intruction got added in-between.
                let out_alloc = self.reg_map[&out].clone();
                assert_eq!(out_alloc.len(), 1);
                let reg = self.allocate(1)?;
                self.asm.setcc_r(Condition::O, reg);
                self.reuse_append(out, outAndFlags, vec![AllocInfo{ reg, sz: 1, off: out_alloc[0].sz }]);
            },
            &Store(addr, value, sz) => {
                let addr_alloc = self.reg_map[&addr].clone();
                let value_alloc = self.reg_map[&value].clone();
                assert_eq!(addr_alloc.len(), 1);
                assert_eq!(value_alloc.len(), 1);
                let AllocInfo{ reg: ar, .. } = addr_alloc[0];
                let AllocInfo{ reg: vr, .. } = value_alloc[0];
                match sz {
                    1 => self.asm.movb_rm(vr, ar, 0),
                    2 => self.asm.movw_rm(vr, ar, 0),
                    4 => self.asm.movl_rm(vr, ar, 0),
                    8 => self.asm.movq_rm(vr, ar, 0),
                    _ => return Err(Error::NYI),
                }
            },
            &Load(out, addr, sz) => {
                let addr_alloc = self.reg_map[&addr].clone();
                assert_eq!(addr_alloc.len(), 1);
                let AllocInfo{ reg: ar, .. } = addr_alloc[0];
                let reg = self.allocate(sz)?;
                match sz {
                    1 => self.asm.movb_mr(ar, 0, reg),
                    2 => self.asm.movw_mr(ar, 0, reg),
                    4 => self.asm.movl_mr(ar, 0, reg),
                    8 => self.asm.movq_mr(ar, 0, reg),
                    _ => return Err(Error::NYI),
                }
                self.register(out, vec![AllocInfo{ reg, sz, off: 0 }]);
            },

            &StoreInto(data_reg, value, offset, sz) => { return Err(Error::NYI) },
            &LoadFrom(out, data_reg, offset, sz) => { return Err(Error::NYI) },

            &Live(_) => {},
            &Dead(reg) => {
                // Note, the register might have already been freed by one
                // of call to reuse or reuse_append functions.
                self.free(reg);
            },
        };

        Ok(())
    }

    fn compile_terminator(&mut self, term: &lir::Terminator) -> Result<(), Error> {
        use lir::Terminator::*;
        match term {
            &Return { value } => {}
            &Unwind => {}
            &Unreachable => {}
            &Goto { target } => {}
            &SwitchInt { value, range, ref targets, otherwise } => {}
            &Call { function, ref args, return_target, unwind_target } => {}
        }
        Ok(())
    }

    fn finalize(self) -> Result<JitCode, Error> {
        match self.asm.backend.finalize() {
            // TODO: transmute and return a class which implements Fn types.
            Ok(buf) => Ok(JitCode { code: buf, start: self.start }),
            Err(_) => Err(Error::Finalize),
        }
    }
}
