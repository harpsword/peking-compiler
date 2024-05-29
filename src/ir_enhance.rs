use std::{collections::HashMap, fmt::format, hash::Hash, result};

use koopa::ir::{
    entities::{BasicBlockData, ValueData},
    values::{self, Binary},
    Function, FunctionData, Program, Value, ValueKind,
};
use once_cell::sync::Lazy;

use crate::riscv::{Instruction, RiskVCode};

pub(crate) mod ir_builder;

pub(crate) fn generate_riscv(program: Program) -> String {
    let mut generator = RiscvGenerator::new();

    return generator.generate_riscv(program);
}

struct RiscvGenerator {
    register_occupition: HashMap<String, bool>,
    registers: Vec<String>,
    instruction_results: HashMap<Value, InstructionExplanation>,
    result: RiskVCode,
}

#[derive(Clone)]
struct InstructionExplanation {
    result_register: Option<String>,
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

impl RiscvGenerator {
    fn new() -> Self {
        Self {
            register_occupition: HashMap::new(),
            registers: REGISTER_LIST.to_vec(),
            result: RiskVCode::new(),
            instruction_results: HashMap::new(),
        }
    }

    fn get_instruction_result(&self, value: Value) -> InstructionExplanation {
        if let Some(result) = self.instruction_results.get(&value) {
            return result.clone();
        }
        InstructionExplanation {
            is_finished: false,
            result_register: None,
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
    // TODO when need to call this ?
    fn release_register(&mut self, register: String) {
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

        for (_, node) in func_data.layout().bbs() {
            for &inst in node.insts().keys() {
                self.generate_riscv_for_instruction(func_data, inst);
            }
        }
    }

    // generate riscv for instruction
    // return register name
    fn generate_riscv_for_instruction(
        &mut self,
        func: &FunctionData,
        value: Value,
    ) -> Option<String> {
        let instr_result = self.get_instruction_result(value);
        if instr_result.is_finished {
            return instr_result.result_register;
        }

        let value_data = func.dfg().value(value);
        let value_result_register = match value_data.kind() {
            ValueKind::Integer(x) => {
                if x.value() == 0 {
                    Some("x0".to_string())
                } else {
                    let register = self.assign_register();
                    self.result.append(Instruction::Li(&register, x.value()));
                    Some(register)
                }
            }
            ValueKind::Return(r) => {
                if let Some(return_value) = r.value() {
                    let register = self
                        .generate_riscv_for_instruction(func, return_value)
                        .unwrap();
                    self.result.append(Instruction::Mov("a0", &register));
                }
                self.result.append(Instruction::Ret);
                None
            }
            ValueKind::Binary(binary) => {
                // only need to support Eq and Sub
                let lhs_register = self
                    .generate_riscv_for_instruction(func, binary.lhs())
                    .unwrap();
                let rhs_register = self
                    .generate_riscv_for_instruction(func, binary.rhs())
                    .unwrap();
                let destination_register = if lhs_register == "x0" && rhs_register == "x0" {
                    self.assign_register()
                } else if lhs_register == "x0" {
                    rhs_register.clone()
                } else {
                    // rhs_register == "x0"
                    // or all is not "x0"
                    lhs_register.clone()
                };
                match binary.op() {
                    values::BinaryOp::Eq => {
                        self.result.append(Instruction::Xor(
                            &destination_register,
                            &lhs_register,
                            &rhs_register,
                        ));
                        self.result.append(Instruction::Seqz(
                            &destination_register,
                            &destination_register,
                        ));
                        Some(destination_register)
                    }
                    values::BinaryOp::NotEq => {
                        self.result.append(Instruction::Xor(
                            &destination_register,
                            &lhs_register,
                            &rhs_register,
                        ));
                        self.result.append(Instruction::Snez(
                            &destination_register,
                            &destination_register,
                        ));
                        Some(destination_register)
                    }
                    values::BinaryOp::Sub => {
                        self.result.append(Instruction::Sub(
                            &destination_register,
                            &lhs_register,
                            &rhs_register,
                        ));
                        Some(destination_register)
                    }
                    values::BinaryOp::Add => {
                        self.result.append(Instruction::Add(
                            &destination_register,
                            &lhs_register,
                            &rhs_register,
                        ));
                        Some(destination_register)
                    }
                    values::BinaryOp::Mul => {
                        self.result.append(Instruction::Mul(
                            &destination_register,
                            &lhs_register,
                            &rhs_register,
                        ));
                        Some(destination_register)
                    }
                    values::BinaryOp::Div => {
                        self.result.append(Instruction::Div(
                            &destination_register,
                            &lhs_register,
                            &rhs_register,
                        ));
                        Some(destination_register)
                    }
                    values::BinaryOp::Mod => {
                        self.result.append(Instruction::Rem(
                            &destination_register,
                            &lhs_register,
                            &rhs_register,
                        ));
                        Some(destination_register)
                    }
                    values::BinaryOp::Or => {
                        self.result.append(Instruction::Or(
                            &destination_register,
                            &lhs_register,
                            &rhs_register,
                        ));
                        Some(destination_register)
                    }
                    values::BinaryOp::And => {
                        self.result.append(Instruction::And(
                            &destination_register,
                            &lhs_register,
                            &rhs_register,
                        ));
                        Some(destination_register)
                    }

                    values::BinaryOp::Gt => {
                        self.result.append(Instruction::Slt(
                            &destination_register,
                            &rhs_register,
                            &lhs_register,
                        ));
                        Some(destination_register)
                    }
                    values::BinaryOp::Lt => {
                        self.result.append(Instruction::Slt(
                            &destination_register,
                            &lhs_register,
                            &rhs_register,
                        ));
                        Some(destination_register)
                    }
                    values::BinaryOp::Ge => {
                        self.result.append(Instruction::Slt(
                            &destination_register,
                            &lhs_register,
                            &rhs_register,
                        ));
                        self.result.append(Instruction::Xori(
                            &destination_register,
                            &destination_register,
                            1,
                        ));
                        Some(destination_register)
                    }
                    values::BinaryOp::Le => {
                        self.result.append(Instruction::Slt(
                            &destination_register,
                            &rhs_register,
                            &lhs_register,
                        ));
                        self.result.append(Instruction::Xori(
                            &destination_register,
                            &destination_register,
                            1,
                        ));
                        Some(destination_register)
                    }

                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        };

        self.instruction_results.insert(
            value,
            InstructionExplanation {
                is_finished: true,
                result_register: value_result_register.clone(),
            },
        );

        value_result_register
    }
}
