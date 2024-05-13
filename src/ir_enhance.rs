use koopa::ir::{FunctionData, Program};

pub(crate) trait GenerateRiscV {
    fn generate_riscv(&self);
}

impl GenerateRiscV for Program {
    fn generate_riscv(&self) {
        for &func in self.func_layout() {
            let func_data = self.func(func);
            for (&bb, node) in func_data.layout().bbs() {
                
            }
        }
    }
}

impl GenerateRiscV for FunctionData {
    fn generate_riscv(&self) {
        todo!()
    }
}


