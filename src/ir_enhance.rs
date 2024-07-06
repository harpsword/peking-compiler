use std::{borrow::Borrow, collections::HashMap, fmt::format, ops::Index, thread::panicking};

use env_logger::init;
use function_ctx::FuncCtx;
use koopa::ir::{values, Function, FunctionData, Program, TypeKind, Value, ValueKind};
use log::info;
use once_cell::sync::Lazy;

use crate::riscv::{Instruction, RiskVCode};

pub(crate) mod ir_builder;
pub(crate) mod function_ctx;

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

    fn reset(&mut self) {
        self.used = 0;
        self.size = 0;
    }

    // when used = 0, assign 0(sp)
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
    Var(String),
}

impl InstructionResult {
    fn to_string(self) -> String {
        match self {
            InstructionResult::Register(reg) => reg,
            InstructionResult::Stack(offset) => format!("{}(sp)", offset),
            InstructionResult::Var(reg) => format!("0({})", reg),
        }
    }
}

#[derive(Clone)]
struct InstructionExplanation {
    dst: Option<InstructionResult>,
    // 记录了这条指令是否已经完成
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
        if register.starts_with("a") {
            // used for argument and return values
            return;
        }
        let occuption = self.register_occupition.insert(register, false);
        if occuption.is_none() || !occuption.unwrap_or(false) {
            info!("log");
        }
        // occuption should be exist and true
        // assert!(occuption.is_some());
        // assert!(occuption.unwrap_or(false));
    }

    fn get_func_arg_register(index: usize) -> String {
        format!("a{}", index)
    }
}

impl RiscvGenerator {
    fn generate_riscv(mut self, program: Program) -> String {
        self.deal_with_gloabl_var(&program);

        let mut program_iter = program.func_layout().iter();
        while let Some(func) = program_iter.next() {
            self.generate_riscv_for_func(&program, *func);
        }

        self.result.generate_result()
    }

    fn get_global_var_name(&self, value: &Value) -> String {
        // value.
        let value_print = format!("{:?}", value);
        let value_print_len = value_print.len();
        let value_id = value_print
            .get(6..value_print_len - 1)
            .expect("should get correct value id");
        format!("var{}", value_id)
    }

    fn deal_with_gloabl_var(&mut self, program: &Program) {
        for value in program.inst_layout().iter() {
            let value_data = program.borrow_value(*value);
            let global_var_name = self.get_global_var_name(value);
            let init_value = match value_data.kind() {
                ValueKind::GlobalAlloc(global_var) => {
                    let init_value_data = program.borrow_value(global_var.init());
                    if let ValueKind::Integer(v) = init_value_data.kind() {
                        v.value()
                    } else {
                        panic!("should be integer for global init value");
                    }
                }
                _ => panic!("should be all global allocs"),
            };
            self.result.append("  .data");
            self.result.append(format!("  .global {}", global_var_name));
            self.result.append(format!("{}:", global_var_name));
            if init_value == 0 {
                self.result.append("  .zero 4");
            } else {
                self.result.append(format!(".word {}", init_value));
            }
            self.result.append("");
        }
    }

    fn generate_riscv_for_func(&mut self, program: &Program, func: Function) {
        let func_data = program.func(func);
        let mut func_ctx = FuncCtx::new(func_data);

        if func_ctx.is_decl_func() {
            // decl function, just return
            return;
        }

        self.result.function_begin(func_ctx.get_name());

        func_ctx.func_stack_size_calculation(program);
        let calculation_result = &func_ctx.stack_result;

        let size = calculation_result.stack_size;
        self.stack_manager.set_size(size);
        // assign parameter size first
        self.stack_manager.assign(calculation_result.parameter_size);

        let size: i32 = size.try_into().unwrap();
        // prologue
        if size > 0 {
            self.result.append(Instruction::Addi("sp", "sp", -size));
        }
        if calculation_result.need_save_ra {
            self.result
                .append(Instruction::Sw(&format!("{}(sp)", size - 4), "ra"));
        }

        // deal with other instructions
        for (bb, node) in func_ctx.func_data.layout().bbs() {
            let bb_name = func_ctx.get_bb_name(*bb);
            if let Some(bb_name) = bb_name {
                if bb_name != "entry" {
                    self.result.append(format!("{}:", bb_name));
                }
            }
            for &inst in node.insts().keys() {
                self.generate_riscv_for_instruction(program, &func_ctx, inst);
            }
        }
        
        self.result.append("");

        self.stack_manager.reset();
    }

    // used to load data from register or stack
    // for register, return register name
    // for stack, return dst of stack loading data
    fn load_value(
        &mut self,
        program: &Program,
        func_ctx: &FuncCtx,
        value: Value,
        stack_load_to_register: bool,
    ) -> String {
        let value = self
            .generate_riscv_for_instruction(program, func_ctx, value);

        if value.is_none() {
            info!("test");
        }
        let value = value.unwrap();
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
            },
            InstructionResult::Var(ref var_register) if stack_load_to_register => {
                let var_register = var_register.clone();
                let value_str = value.to_string();
                self.result.append(Instruction::Lw(&var_register, &value_str));
                var_register.clone()
            },
            InstructionResult::Var(_) => {
                value.to_string()
            }
        }
    }

    // generate riscv for instruction
    // return dst
    fn generate_riscv_for_instruction(
        &mut self,
        program: &Program,
        func_ctx: &FuncCtx,
        value: Value,
    ) -> Option<InstructionResult> {
        let func = func_ctx.func_data;
        let instr_result = self.get_instruction_result(value);
        if instr_result.is_finished {
            return instr_result.dst;
        }

        let value_data = match value.is_global() {
            true => &program.borrow_value(value),
            false => func.dfg().value(value),
        };

        let size = value_data.ty().size();
        let mut is_finished = true;
        let dst = match value_data.kind() {
            ValueKind::FuncArgRef(func_arg_ref) => {
                let index = func_arg_ref.index();
                let result = if (index < 8) {
                    InstructionResult::Register(
                        Self::get_func_arg_register(func_arg_ref.index()),
                    )
                } else {
                    let offset = func_ctx.stack_result.stack_size + (index-8)*4;
                    InstructionResult::Stack(offset)
                };
                Some(result)
            },
            ValueKind::Alloc(_) => {
                let dst = self.stack_manager.assign(size);
                dst.map(|x| InstructionResult::Stack(x))
            }
            ValueKind::GlobalAlloc(global_alloc) => {
                let global_var_name = self.get_global_var_name(&value);
                let register = self.assign_register();
                self.result.append(Instruction::La(&register, &global_var_name));
                // never finished, need to load var address to register again
                is_finished = false;

                Some(InstructionResult::Var(register))
            }
            ValueKind::Store(s) => {
                let value = self.load_value(program, func_ctx, s.value(), true);
                let dst = self.load_value(program, func_ctx, s.dest(), false);
                self.result.append(Instruction::Sw(&dst, &value));

                self.release_register(value);
                None
            }
            ValueKind::Load(load) => {
                let src = self.load_value(program, func_ctx, load.src(), true);

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
                        .generate_riscv_for_instruction(program, func_ctx, return_value)
                        .unwrap();
                    match src {
                        InstructionResult::Register(register) => {
                            self.result.append(Instruction::Mov("a0", &register));
                        }
                        InstructionResult::Stack(_) | InstructionResult::Var(_) => {
                            self.result.append(Instruction::Lw("a0", &src.to_string()));
                        }
                    }
                }
                let stack_size = func_ctx.stack_result.stack_size;

                if func_ctx.stack_result.need_save_ra {
                    self.result
                .append(Instruction::Lw("ra", &format!("{}(sp)", stack_size - 4)));
                }
                if stack_size > 0 {
                    self.result.append(Instruction::Addi("sp", "sp", stack_size.try_into().unwrap()));
                }
                self.result.append(Instruction::Ret);

                None
            }
            ValueKind::Binary(binary) => {
                let lhs = self.load_value(program, func_ctx, binary.lhs(), true);
                let rhs = self.load_value(program, func_ctx, binary.rhs(), true);
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
                let cond = self.load_value(program, func_ctx, branch.cond(), true);

                let then_bb =
                    func_ctx.get_bb_name(branch.true_bb()).expect("then bb should have name");
                let else_bb =
                func_ctx.get_bb_name(branch.false_bb()).expect("else bb should have name");

                self.result.append(Instruction::Bnez(&cond, then_bb));
                self.result.append(Instruction::Jump(else_bb));

                self.release_register(cond);
                None
            }
            ValueKind::Jump(jump) => {
                let target_bb =func_ctx.get_bb_name(jump.target())
                    .expect("jump target bb should have name");
                self.result.append(Instruction::Jump(target_bb));

                None
            }
            ValueKind::Call(call) => {
                // load arg to related place
                // like a0-a7 for first 8 parameters
                // rest parameters will be on stack
                let mut arg_count = 0;
                let mut param_stack_used_size = 0;
                info!("start call: {:#?}", call);
                for arg in call.args() {
                    info!("call arg: {:#?}, func: {:#?}", arg, call.callee());
                    let arg_value = self.load_value(program, func_ctx, *arg, true);
                    // TODO, optimize performance
                    if arg_count < 8 {
                        self.result.append(Instruction::Mov(
                            &Self::get_func_arg_register(arg_count),
                            &arg_value,
                        ));
                    } else {
                        self.result.append(Instruction::Sw(
                            &format!("{}(sp)", param_stack_used_size),
                            &arg_value,
                        ));
                        // let arg_size = {
                        //     let value_data = func.dfg().value(*arg);
                        //     value_data.ty().size()
                        // };
                        let arg_size = 4;
                        param_stack_used_size += arg_size;
                    }

                    self.release_register(arg_value);
                    arg_count += 1;
                }

                // call function
                let (callee_func_name, return_value_size) = {
                    let callee_func_data = program.func(call.callee());

                    let func_name = tools::from_ir_name(callee_func_data.name());
                    let return_value_size = match callee_func_data.ty().kind() {
                        TypeKind::Function(_, ret) => ret.size(),
                        _ => unreachable!(),
                    };

                    (func_name, return_value_size)
                };
                self.result.append(Instruction::Call(callee_func_name));

                // return value will also be placed on register and stack
                // from a0-a7 and related stack space
                // but now, only support one return value
                // so we will store it to stack from a0

                let call_result = {
                    if return_value_size > 0 {
                        let dst = self
                            .stack_manager
                            .assign(return_value_size)
                            .expect("should have stack space");
                        let dst = InstructionResult::Stack(dst);
                        self.result
                            .append(Instruction::Sw(&dst.clone().to_string(), "a0"));
                        Some(dst)
                    } else {
                        None
                    }
                };
                call_result
            }
            _ => unreachable!(),
        };

        self.instruction_results.insert(
            value,
            InstructionExplanation {
                is_finished: is_finished,
                dst: dst.clone(),
            },
        );

        dst
    }
}

pub(crate) mod tools {
    use koopa::ir::{BasicBlock, FunctionData};

    pub(crate) fn turn_into_ir_name(name: &str) -> String {
        format!("@{}", name)
    }

    pub(crate) fn from_ir_name(name: &str) -> &str {
        if name.starts_with("@") {
            return &name[1..];
        }
        name
    }
}
