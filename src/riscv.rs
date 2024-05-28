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

    pub(crate) fn append_basic(&mut self) {
        self.append("  .text");
        self.append("  .global main");
    }

    pub(crate) fn generate_result(self) -> String {
        self.insts.join("\n")
    }
}

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

    Add(&'a str, &'a str, &'a str),

    Mul(&'a str, &'a str, &'a str),

    Sub(&'a str, &'a str, &'a str),

    Li(&'a str, i32),

    Mov(&'a str, &'a str),

    Ret,
}

impl<'a> Into<String> for Instruction<'a> {
    fn into(self) -> String {
        match self {
            Instruction::Slt(dst, lhs, rhs) => format!("  slt {dst}, {lhs}, {rhs}"),
            Instruction::Xori(ds, ls, rs) => format!("  xori {ds}, {ls}, {rs}"),
            Instruction::Xor(dst, lhs, rhs) => format!("  xor {dst}, {lhs}, {rhs}"),
            Instruction::Seqz(dst, src) => format!("  sltiu {dst}, {src}, 1"),
            Instruction::Add(dst, lhs, rhs) => format!("  add {dst}, {lhs}, {rhs}"),
            Instruction::Mul(dst, lhs, rhs) => format!("  mul {dst}, {lhs}, {rhs}"),
            Instruction::Sub(dst, lhs, rhs) => format!("  sub {dst}, {lhs}, {rhs}"),
            Instruction::Li(dst, src) => format!("  li {dst}, {src}"),
            Instruction::Mov(dst, src) => format!("  mv {dst}, {src}"),
            Instruction::Ret => format!("  ret"),
        }
    }
}
