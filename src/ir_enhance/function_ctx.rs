use koopa::ir::{BasicBlock, FunctionData, Program, TypeKind, ValueKind};
use log::info;


pub(crate) struct FuncCtx<'a> {
    pub stack_result: FuncStackSizeCalculationResult,
    pub func_data: &'a FunctionData,
}

impl<'a> FuncCtx<'a> {
    
    pub(crate) fn new(func_data: &'a FunctionData) -> Self {
        Self {
            func_data: func_data,
            stack_result: FuncStackSizeCalculationResult::new(),
        }
    }

    pub(crate) fn is_decl_func(&self) -> bool {
        self.func_data.layout().entry_bb().is_none()
    }
    
    pub(crate) fn get_name(&self) -> &str {
        let func_name = self.func_data.name();
        if func_name.starts_with("@") {
            return &func_name[1..];
        }
        return func_name;
    }

    pub(crate) fn get_bb_name(&self, bb: BasicBlock) -> Option<&str> {
        let name = self.func_data.dfg().bb(bb).name().as_ref();
        if let Some(name) = name {
            if name.starts_with("@") {
                return Some(&name[1..]);
            }
            return Some(&name);
        }
        None
    }

    /// calculate the stack size of a function
    pub fn func_stack_size_calculation(
        &mut self,
        program: &Program,
    ) {
        let func_data: &FunctionData = self.func_data;
        let mut size = 0;
        let mut ra_space = 0;
        let mut parameter_count_bigger_than_8 = 0;
        for (_, node) in func_data.layout().bbs() {
            for &inst in node.insts().keys() {
                let value_data = func_data.dfg().value(inst);
                size = size + value_data.ty().size();

                match value_data.kind() {
                    ValueKind::Call(call) => {
                        ra_space = 4;

                        let param_counts = call.args().len();
                        if param_counts > 8 {
                            parameter_count_bigger_than_8 =
                                std::cmp::max(param_counts - 8, parameter_count_bigger_than_8);
                        }

                        let callee_func_data = program.func(call.callee());
                        let return_value_size = match callee_func_data.ty().kind() {
                            TypeKind::Function(_, ret) => ret.size(),
                            _ => unreachable!(),
                        };
                        size = size + return_value_size;
                    }
                    _ => {}
                }
            }
        }
        let mut stack_size = size + parameter_count_bigger_than_8 * 4 + ra_space;
        stack_size = (stack_size + 15) / 16 * 16;
        self.stack_result = FuncStackSizeCalculationResult {
            stack_size: stack_size,
            local_var_size: size,
            parameter_size: parameter_count_bigger_than_8 * 4,
            need_save_ra: ra_space > 0,
        };

        info!("func: {}, calculation result: {:?}", self.get_name(), self.stack_result);
    }
}

#[derive(Debug)]
pub(crate) struct FuncStackSizeCalculationResult {
    pub stack_size: usize,
    pub local_var_size: usize,
    pub parameter_size: usize,
    pub need_save_ra: bool,
}

impl FuncStackSizeCalculationResult {
    pub(crate) fn new() -> Self {
        Self {
            stack_size: 0,
            local_var_size: 0,
            parameter_size: 0,
            need_save_ra: false,
        }
    }
}