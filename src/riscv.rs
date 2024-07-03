use log::info;

pub(crate) struct RiskVCode {
    insts: Vec<String>,
}

impl RiskVCode {
    pub(crate) fn new() -> RiskVCode {
        RiskVCode { insts: Vec::new() }
    }

    pub(crate) fn append<V: Into<String>>(&mut self, inst: V) {
        self.insts.push(inst.into());
    }

    pub(crate) fn function_begin(&mut self, name: &str) {
        self.append("  .text");
        self.append(format!("  .globl {}", name));
        self.append(format!("{}:", name));
    }

    pub(crate) fn generate_result(self) -> String {
        self.insts.join("\n")
    }

    pub(crate) fn last_inst_check<V: Into<String>>(&self, inst: V) -> bool {
        self.insts
            .last()
            .map(|s| s.as_str())
            .map_or(false, |s| s == inst.into())
    }

    pub(crate) fn pop(&mut self) -> Option<String> {
        self.insts.pop()
    }
}

/// Instruction
///
/// 默认第一个元素是dst
pub(crate) enum Instruction<'a> {
    Slt(&'a str, &'a str, &'a str),

    Xor(&'a str, &'a str, &'a str),
    // dst = ls xor rs
    // rs should be a number
    // Common paradigms
    // xori a0, a0, 1, when a0 is 0 or 1, means a0 = !a0
    Xori(&'a str, &'a str, i32),

    // set dst to 1 if src is zero
    Seqz(&'a str, &'a str),
    // set dst to 1 if src is not zero
    Snez(&'a str, &'a str),

    Add(&'a str, &'a str, &'a str),
    Addi(&'a str, &'a str, i32),
    Sub(&'a str, &'a str, &'a str),
    Mul(&'a str, &'a str, &'a str),
    Div(&'a str, &'a str, &'a str),
    /// get remainder
    Rem(&'a str, &'a str, &'a str),

    And(&'a str, &'a str, &'a str),
    Or(&'a str, &'a str, &'a str),

    Li(&'a str, i32),
    /// Mov(dst, src)
    Mov(&'a str, &'a str),

    /// Sw(dst, src)
    /// write src to dst
    /// dst should be like 0(sp)
    Sw(&'a str, &'a str),

    /// Lw(dst, src)
    /// read src to dst
    /// src should be like 0(sp)
    Lw(&'a str, &'a str),

    /// La(dst, src)
    /// should be La(register, var_name)
    /// it will load the adress of var_name to register
    La(&'a str, &'a str),

    /// bnez cond, target
    /// if cond != 0, jump to target
    Bnez(&'a str, &'a str),

    /// j target
    /// Jump to target
    Jump(&'a str),

    /// call func
    Call(&'a str),
    Ret,
}

impl<'a> Into<String> for Instruction<'a> {
    fn into(self) -> String {
        match self {
            Instruction::Slt(dst, lhs, rhs) => format!("  slt {dst}, {lhs}, {rhs}"),

            Instruction::Xori(ds, ls, rs) => format!("  xori {ds}, {ls}, {rs}"),
            Instruction::Xor(dst, lhs, rhs) => format!("  xor {dst}, {lhs}, {rhs}"),

            Instruction::Seqz(dst, src) => format!("  sltiu {dst}, {src}, 1"),
            // sltu dst, x0, src means dst = 0 < src
            Instruction::Snez(dst, src) => format!("  sltu {dst}, x0, {src}"),

            Instruction::Add(dst, lhs, rhs) => format!("  add {dst}, {lhs}, {rhs}"),
            Instruction::Addi(dst, lhs, rhs) => format!("  addi {dst}, {lhs}, {rhs}"),
            Instruction::Sub(dst, lhs, rhs) => format!("  sub {dst}, {lhs}, {rhs}"),
            Instruction::Mul(dst, lhs, rhs) => format!("  mul {dst}, {lhs}, {rhs}"),
            Instruction::Div(dst, lhs, rhs) => format!("  div {dst}, {lhs}, {rhs}"),
            Instruction::Rem(dst, lhs, rhs) => format!("  rem {dst}, {lhs}, {rhs}"),

            Instruction::And(dst, lhs, rhs) => format!("  and {dst}, {lhs}, {rhs}"),
            Instruction::Or(dst, lhs, rhs) => format!("  or {dst}, {lhs}, {rhs}"),

            Instruction::Li(dst, src) => format!("  li {dst}, {src}"),
            Instruction::Mov(dst, src) => format!("  mv {dst}, {src}"),
            Instruction::Sw(dst, src) => {
                if src == "a10" {
                    info!("sw a10");
                }
                format!("  sw {src}, {dst}")
            },
            Instruction::Lw(dst, src) => format!("  lw {dst}, {src}"),
            Instruction::La(dst, src) => format!("  la {dst}, {src}"),

            Instruction::Bnez(cond, target) => format!("  bnez {cond}, {target}"),
            Instruction::Jump(target) => format!("  j {target}"),
            Instruction::Call(target) => format!("  call {target}"),
            Instruction::Ret => format!("  ret"),
        }
    }
}
