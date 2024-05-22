use koopa::{
    front::ast::Block,
    ir::{
        builder::{BasicBlockBuilder, LocalBuilder},
        BasicBlock, FunctionData, Program, Value,
    },
};

struct ProgramBuilder {
    program: Program,
}

impl ProgramBuilder {
    pub(crate) fn new() -> Self {
        ProgramBuilder {
            program: Program::new(),
        }
    }

    pub(crate) fn new_function(&mut self, func_data: FunctionData) -> FunctionBuilder {
        let func = self.program.new_func(func_data);

        FunctionBuilder::new(self.program.func_mut(func))
    }

    pub(crate) fn build(self) -> Program {
        return self.program;
    }
}

struct FunctionBuilder<'a> {
    func_data: &'a mut FunctionData,
}

impl<'a> FunctionBuilder<'a> {
    pub(crate) fn new(func_data: &'a mut FunctionData) -> FunctionBuilder<'a> {
        FunctionBuilder {
            func_data: func_data,
        }
    }

    pub(crate) fn new_block(&mut self, name: Option<String>) -> BlockBuilder {
        let bb = self.func_data.dfg_mut().new_bb().basic_block(name);
        let _ = self.func_data.layout_mut().bbs_mut().push_key_back(bb);
        BlockBuilder {
            func_data: self.func_data,
            bb: bb,
        }
    }
}

struct BlockBuilder<'a> {
    func_data: &'a mut FunctionData,
    bb: BasicBlock,
}

impl<'a> BlockBuilder<'a> {
    pub(crate) fn new_value(&mut self) -> LocalBuilder {
        self.func_data.dfg_mut().new_value()
    }

    pub(crate) fn extend<I: IntoIterator<Item = Value>>(&mut self, iter: I) {
        self.func_data
            .layout_mut()
            .bb_mut(self.bb)
            .insts_mut()
            .extend(iter);
    }
}

mod tests {
    use koopa::{
        back::KoopaGenerator,
        ir::{
            builder::{LocalInstBuilder, ValueBuilder},
            BinaryOp, FunctionData, Type,
        },
    };

    use super::{BlockBuilder, FunctionBuilder, ProgramBuilder};

    #[test]
    fn simple_ir_builder() {
        let mut program_builder = ProgramBuilder::new();

        let func_data = FunctionData::new("@main".into(), Vec::new(), Type::get_i32());

        let mut function_builder = program_builder.new_function(func_data);

        let mut block_builder = function_builder.new_block(Some("@text".into()));
        let lhs = block_builder.new_value().integer(11);
        let rhs = block_builder.new_value().integer(31);
        let add = block_builder.new_value().binary(BinaryOp::Add, lhs, rhs);
        let ret = block_builder.new_value().ret(Some(add));

        block_builder.extend([add, ret]);

        let program = program_builder.build();
        let mut gen = KoopaGenerator::new(Vec::new());
        gen.generate_on(&program).unwrap();
        let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();

        println!("{}", text_form_ir);
    }
}
