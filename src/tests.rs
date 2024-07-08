mod test {
    use std::str::from_utf8;

    use env_logger::Env;
    use koopa::back::KoopaGenerator;
    use log::{info, log_enabled};

    use crate::{
        ast::{Block, BlockItem, CompUnit, Decl, FuncDef},
        compiler_define::{self, semantic_analysis, symbol_table::SymbolTable, SysyCompiler},
        ir_enhance::generate_riscv,
    };

    use koopa::ir::{builder::EntityInfoQuerier, builder_traits::*, *};

    #[test]
    fn test_parse() {
        let env = Env::default().filter_or("LOG_LEVEL", "info");
        env_logger::init_from_env(env);

        let mode = "-koopa".to_owned();
        // let input = "input/lv8/11_short_circuit.c".to_owned();
        let input = "input/lv6/short_circut.c".to_owned();
        let mut parser = SysyCompiler::new(input.into()).unwrap();

        parser.generate_ast();

        let ast = parser.ast.as_ref().unwrap();
        println!("ast: {:#?}", ast);

        {
            let ir = parser.get_ir().unwrap();
            let mut gen = KoopaGenerator::new(Vec::new());
            gen.generate_on(&ir).unwrap();
            let text_form_ir = from_utf8(&gen.writer()).unwrap().to_string();

            println!("IR: \n {}", text_form_ir);

            let riscv = generate_riscv(ir);
            println!("RISC-V: \n{}", riscv);
        }
    }

    #[test]
    fn test_program() {
        let mut program = Program::new();
        let main = program.new_func(FunctionData::new(
            "@main".into(),
            Vec::new(),
            Type::get_i32(),
        ));
        let main_data = program.func_mut(main);

        let bb = main_data.dfg_mut().new_bb().basic_block(None);
        let _ = main_data.layout_mut().bbs_mut().push_key_back(bb);

        let lhs = main_data.dfg_mut().new_value().integer(11);
        let rhs = main_data.dfg_mut().new_value().integer(31);
        let add = main_data
            .dfg_mut()
            .new_value()
            .binary(BinaryOp::Add, lhs, rhs);
        let ret = main_data.dfg_mut().new_value().ret(Some(add));
        main_data
            .layout_mut()
            .bb_mut(bb)
            .insts_mut()
            .extend([add, ret]);

        {
            let bb = main_data.dfg_mut().new_bb().basic_block(None);
            let _ = main_data.layout_mut().bbs_mut().push_key_back(bb);

            let variable = main_data.dfg_mut().new_value().alloc(Type::get_i32());
            let store_value = main_data.dfg_mut().new_value().integer(10);
            let store = main_data.dfg_mut().new_value().store(store_value, variable);
            main_data
                .layout_mut()
                .bb_mut(bb)
                .insts_mut()
                .extend([variable, store]);

            let varaible1 = main_data.dfg_mut().new_value().load(variable);
            let add = main_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::Add, varaible1, store_value);

            main_data
                .layout_mut()
                .bb_mut(bb)
                .insts_mut()
                .extend([varaible1, add]);

            // let lhs = main_data.dfg_mut().new_value().integer(11);
            // let rhs = main_data.dfg_mut().new_value().integer(31);
            // let add = main_data
            //     .dfg_mut()
            //     .new_value()
            //     .binary(BinaryOp::Add, lhs, rhs);
            // let ret = main_data.dfg_mut().new_value().ret(Some(add));
            // main_data
            //     .layout_mut()
            //     .bb_mut(bb)
            //     .insts_mut()
            //     .extend([add, ret]);
        }

        let mut gen = KoopaGenerator::new(Vec::new());
        gen.generate_on(&program).unwrap();
        let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();
        println!("{}", text_form_ir);
    }
}
