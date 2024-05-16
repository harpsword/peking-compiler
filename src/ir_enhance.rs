use koopa::ir::{entities::ValueData, values, FunctionData, Program, ValueKind};

pub(crate) trait GenerateRiscV {
    fn generate_riscv(&self) -> String;
}

impl GenerateRiscV for Program {
    fn generate_riscv(&self) -> String {
        let mut result = String::new();

        result.push_str("  .text\n");
        result.push_str("  .global main\n");
        for &func in self.func_layout() {
            result.push_str(&self.func(func).generate_riscv());
        }
        result
    }
}

impl GenerateRiscV for FunctionData {
    fn generate_riscv(&self) -> String {
        let mut result = String::new();
        result.push_str("main:\n");
        for (&bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                let value_data = self.dfg().value(inst);

                match value_data.kind() {
                    koopa::ir::ValueKind::Integer(int) => {}
                    koopa::ir::ValueKind::Return(r) => {
                        result.push_str(&r.generate_riscv());
                    }
                    _ => unreachable!(),
                }
            }
        }
        result
    }
}

impl GenerateRiscV for koopa::ir::ValueKind {
    fn generate_riscv(&self) -> String {
        match self {
            ValueKind::Return(r) => r.generate_riscv(),
            ValueKind::Integer(i) => i.generate_riscv(),
            _ => unreachable!(),
        }
    }
}

impl GenerateRiscV for values::Integer {
    fn generate_riscv(&self) -> String {
        todo!()
    }
}

impl GenerateRiscV for values::Return {
    fn generate_riscv(&self) -> String {
        return "  li a0, 0\n  ret".to_owned();
    }
}
