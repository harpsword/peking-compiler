mod test {
    use std::str::from_utf8;

    use koopa::back::KoopaGenerator;

    use crate::parser::SysyParser;

    use koopa::ir::{builder::EntityInfoQuerier, builder_traits::*, *};

    #[test]
    fn test_block() {
        let mode = "-koopa".to_owned();
        let input = "hello.c".to_owned();
        let mut parser = SysyParser::new(input.into()).unwrap();

        parser.generate_ast();

        let ast = parser.ast.as_ref().unwrap();
        println!("{:#?}", ast);

        {
            let ir = parser.get_ir().unwrap();
            let mut gen = KoopaGenerator::new(Vec::new());
            gen.generate_on(&ir).unwrap();
            let text_form_ir = from_utf8(&gen.writer()).unwrap().to_string();

            println!("IR: \n {}", text_form_ir);
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
        main_data.layout_mut().bbs_mut().push_key_back(bb);

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

        let mut gen = KoopaGenerator::new(Vec::new());
        gen.generate_on(&program).unwrap();
        let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();
        println!("{}", text_form_ir);
    }
}
