use std::collections::HashMap;

use koopa::ir::{
    entities::BasicBlockData, layout::BasicBlockNode, values, BasicBlock, Function, FunctionData, Program, Value, ValueKind
};
use log::info;
use once_cell::sync::Lazy;

use crate::riscv::{Instruction, RiskVCode};

pub(crate) mod ir_builder;

pub(crate) fn generate_riscv(program: Program) -> String {
    let generator = RiscvGenerator::new();

    return generator.generate_riscv(program);
}

struct RiscvGenerator {
    register_occupition: HashMap<String, bool>,
    registers: Vec<String>,
    instruction_results: HashMap<Value, InstructionExplanation>,
    result: RiskVCode,
    stack_manager: StackManager,
}

#[derive(Debug)]
struct StackManager {
    size: usize,
    used: usize,
}

impl StackManager {
    fn new() -> Self {
        Self { size: 0, used: 0 }
    }

    fn set_size(&mut self, size: usize) {
        self.size = size;
    }

    fn assign(&mut self, size: usize) -> Option<usize> {
        if self.used + size > self.size {
            return None;
        }
        let offset = self.used;
        self.used = self.used + size;
        Some(offset)
    }
}

#[derive(Debug, Clone)]
enum InstructionResult {
    Register(String),
    Stack(usize),
}

impl InstructionResult {
    fn to_string(self) -> String {
        match self {
            InstructionResult::Register(reg) => reg,
            InstructionResult::Stack(offset) => format!("{}(sp)", offset),
        }
    }
}

#[derive(Clone)]
struct InstructionExplanation {
    dst: Option<InstructionResult>,
    is_finished: bool,
}

static REGISTER_LIST: Lazy<Vec<String>> = Lazy::new(|| {
    vec![
        "t0".to_string(),
        "t1".to_string(),
        "t2".to_string(),
        "t3".to_string(),
        "t4".to_string(),
        "t5".to_string(),
        "t6".to_string(),
        "t7".to_string(),
    ]
});

const X0: &str = "x0";

impl RiscvGenerator {
    fn new() -> Self {
        Self {
            register_occupition: HashMap::new(),
            registers: REGISTER_LIST.to_vec(),
            result: RiskVCode::new(),
            instruction_results: HashMap::new(),
            stack_manager: StackManager::new(),
        }
    }

    fn get_instruction_result(&self, value: Value) -> InstructionExplanation {
        if let Some(result) = self.instruction_results.get(&value) {
            return result.clone();
        }
        InstructionExplanation {
            is_finished: false,
            dst: None,
        }
    }

    fn assign_register(&mut self) -> String {
        for register in self.registers.iter() {
            let occupation = self.register_occupition.get_mut(register);

            if let Some(occupation) = occupation {
                if !(*occupation) {
                    *occupation = true;
                    return register.to_string();
                }
            } else {
                // means not used
                self.register_occupition.insert(register.to_string(), true);
                return register.to_string();
            }
        }

        panic!("there are no registers left");
    }

    // release register
    fn release_register(&mut self, register: String) {
        if register == X0 {
            return;
        }
        let occuption = self.register_occupition.insert(register, false);
        // occuption should be exist and true
        assert!(occuption.is_some());
        assert!(occuption.unwrap_or(false));
    }

    fn generate_riscv(mut self, program: Program) -> String {
        self.result.append_basic();

        let mut program_iter = program.func_layout().iter();
        while let Some(func) = program_iter.next() {
            self.generate_riscv_for_func(&program, *func);
        }

        self.result.generate_result()
    }

    fn extract_func_name_from_koopa_func(func_name: &str) -> &str {
        if func_name.starts_with("@") {
            return &func_name[1..];
        }
        return func_name;
    }

    fn generate_riscv_for_func(&mut self, program: &Program, func: Function) {
        let func_data = program.func(func);

        let func_name = Self::extract_func_name_from_koopa_func(func_data.name());
        self.result.append(format!("{}:", func_name));

        let mut size = 0;
        for (_, node) in func_data.layout().bbs() {
            for &inst in node.insts().keys() {
                let value_data = func_data.dfg().value(inst);
                info!(
                    "name: {:?}, ty: {}, size: {}",
                    value_data.name(),
                    value_data.ty(),
                    value_data.ty().size()
                );
                size = size + value_data.ty().size();
            }
        }
        size = (size + 15) / 16 * 16;
        self.stack_manager.set_size(size);
        let size: i32 = size.try_into().unwrap();

        // assign stack space first
        if size > 0 {
            self.result.append(Instruction::Addi("sp", "sp", -size));
        }

        // deal with other instructions
        for (bb, node) in func_data.layout().bbs() {

            // TODO delete duplicate code
            let bb_name = tools::get_bb_name(func_data, *bb);
            if let Some(bb_name) = bb_name {
                self.result.append(format!("{}:", bb_name));
            }
            for &inst in node.insts().keys() {
                self.generate_riscv_for_instruction(func_data, inst);
            }
        }

        self.stack_manager.set_size(0);
    }

    // used to load data from register or stack
    // for register, return register name
    // for stack, return dst of stack loading data
    fn load_value(
        &mut self,
        func: &FunctionData,
        value: Value,
        stack_load_to_register: bool,
    ) -> String {
        let value = self.generate_riscv_for_instruction(func, value).unwrap();
        match value {
            InstructionResult::Register(register) => register,
            InstructionResult::Stack(_) => {
                if stack_load_to_register {
                    let register = self.assign_register();
                    self.result
                        .append(Instruction::Lw(&register, &value.to_string()));
                    return register;
                } else {
                    return value.to_string();
                }
            }
        }
    }

    // generate riscv for instruction
    // return dst
    fn generate_riscv_for_instruction(
        &mut self,
        func: &FunctionData,
        value: Value,
    ) -> Option<InstructionResult> {
        let instr_result = self.get_instruction_result(value);
        if instr_result.is_finished {
            return instr_result.dst;
        }

        let value_data = func.dfg().value(value);
        let size = value_data.ty().size();
        let dst = match value_data.kind() {
            ValueKind::Alloc(_) => {
                let dst = self.stack_manager.assign(size);
                dst.map(|x| InstructionResult::Stack(x))
            }
            ValueKind::Store(s) => {
                let value = self.load_value(func, s.value(), true);
                let dst = self.load_value(func, s.dest(), false);
                self.result.append(Instruction::Sw(&dst, &value));

                self.release_register(value);
                None
            }
            ValueKind::Load(load) => {
                // load src to register
                let src = self.load_value(func, load.src(), true);

                // write register to dst
                let dst = self
                    .stack_manager
                    .assign(size)
                    .expect("should have stack space");
                let dst = InstructionResult::Stack(dst);
                self.result
                    .append(Instruction::Sw(&dst.clone().to_string(), &src));

                self.release_register(src);
                Some(dst)
            }
            ValueKind::Integer(x) => {
                let register = if x.value() == 0 {
                    "x0".to_string()
                } else {
                    let register = self.assign_register();
                    self.result.append(Instruction::Li(&register, x.value()));
                    register
                };
                Some(InstructionResult::Register(register))
            }
            ValueKind::Return(r) => {
                if let Some(return_value) = r.value() {
                    let src = self
                        .generate_riscv_for_instruction(func, return_value)
                        .unwrap();
                    match src {
                        InstructionResult::Register(register) => {
                            self.result.append(Instruction::Mov("a0", &register));
                        }
                        InstructionResult::Stack(_) => {
                            self.result.append(Instruction::Lw("a0", &src.to_string()));
                        }
                    }
                }

                let size: i32 = self.stack_manager.size.try_into().unwrap();
                if size > 0 {
                    self.result.append(Instruction::Addi("sp", "sp", size));
                }

                self.result.append(Instruction::Ret);
                None
            }
            ValueKind::Binary(binary) => {
                let lhs = self.load_value(func, binary.lhs(), true);
                let rhs = self.load_value(func, binary.rhs(), true);
                let destination_register = self.assign_register();
                match binary.op() {
                    values::BinaryOp::Eq => {
                        self.result
                            .append(Instruction::Xor(&destination_register, &lhs, &rhs));
                        self.result.append(Instruction::Seqz(
                            &destination_register,
                            &destination_register,
                        ));
                    }
                    values::BinaryOp::NotEq => {
                        self.result
                            .append(Instruction::Xor(&destination_register, &lhs, &rhs));
                        self.result.append(Instruction::Snez(
                            &destination_register,
                            &destination_register,
                        ));
                    }
                    values::BinaryOp::Sub => {
                        self.result
                            .append(Instruction::Sub(&destination_register, &lhs, &rhs));
                    }
                    values::BinaryOp::Add => {
                        self.result
                            .append(Instruction::Add(&destination_register, &lhs, &rhs));
                    }
                    values::BinaryOp::Mul => {
                        self.result
                            .append(Instruction::Mul(&destination_register, &lhs, &rhs));
                    }
                    values::BinaryOp::Div => {
                        self.result
                            .append(Instruction::Div(&destination_register, &lhs, &rhs));
                    }
                    values::BinaryOp::Mod => {
                        self.result
                            .append(Instruction::Rem(&destination_register, &lhs, &rhs));
                    }
                    values::BinaryOp::Or => {
                        self.result
                            .append(Instruction::Or(&destination_register, &lhs, &rhs));
                        self.result.append(Instruction::Snez(
                            &destination_register,
                            &destination_register,
                        ));
                    }
                    values::BinaryOp::And => {
                        self.result.append(Instruction::Snez(&lhs, &lhs));
                        self.result.append(Instruction::Snez(&rhs, &rhs));
                        self.result
                            .append(Instruction::And(&destination_register, &lhs, &rhs));
                    }

                    values::BinaryOp::Gt => {
                        self.result
                            .append(Instruction::Slt(&destination_register, &rhs, &lhs));
                    }
                    values::BinaryOp::Lt => {
                        self.result
                            .append(Instruction::Slt(&destination_register, &lhs, &rhs));
                    }
                    values::BinaryOp::Ge => {
                        self.result
                            .append(Instruction::Slt(&destination_register, &lhs, &rhs));
                        self.result.append(Instruction::Xori(
                            &destination_register,
                            &destination_register,
                            1,
                        ));
                    }
                    values::BinaryOp::Le => {
                        self.result
                            .append(Instruction::Slt(&destination_register, &rhs, &lhs));
                        self.result.append(Instruction::Xori(
                            &destination_register,
                            &destination_register,
                            1,
                        ));
                    }
                    _ => unreachable!(),
                }

                let destination = InstructionResult::Stack(
                    self.stack_manager
                        .assign(size)
                        .expect("should have stack space"),
                );

                self.result.append(Instruction::Sw(
                    &destination.clone().to_string(),
                    &destination_register,
                ));
                self.release_register(lhs);
                self.release_register(rhs);
                self.release_register(destination_register);
                Some(destination)
            }
            ValueKind::Branch(branch) => {
                let cond = self.load_value(func, branch.cond(), true);

                let then_bb = tools::get_bb_name(func, branch.true_bb()).expect("then bb should have name");
                let else_bb = tools::get_bb_name(func, branch.false_bb()).expect("else bb should have name");

                self.result.append(Instruction::Bnez(&cond, then_bb));
                self.result.append(Instruction::Jump(else_bb));

                None
            }
            ValueKind::Jump(jump) => {
                let target_bb = tools::get_bb_name(func, jump.target()).expect("jump target bb should have name");
                self.result.append(Instruction::Jump(target_bb));

                None
            }
            _ => unreachable!(),
        };

        self.instruction_results.insert(
            value,
            InstructionExplanation {
                is_finished: true,
                dst: dst.clone(),
            },
        );

        dst
    }
}

mod tools {
    use koopa::ir::{BasicBlock, FunctionData};


    pub(crate) fn get_bb_name(func: &FunctionData, bb: BasicBlock) -> Option<&str> {
        let name = func.dfg().bb(bb).name().as_ref();
        if let Some(name) = name {
            if name.starts_with("@") {
                return Some(&name[1..]);
            }
            return Some(&name);
        }
        None
    }
}