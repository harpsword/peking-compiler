use std::env::Args;
use std::fs::File;
use std::io::{Result, Write};
use std::path::PathBuf;
use std::str::from_utf8;
use std::{env::args};

use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;

use crate::compiler_define::SysyCompiler;

mod ast;
mod ir_enhance;
mod compiler_define;
pub(crate) mod riscv;

mod tests;

lalrpop_mod!(sysy);

#[derive(Debug)]
struct CompilerArgs {
    output_file: Option<PathBuf>,
    output_riscv: bool,
    input_file: Option<PathBuf>,
}

impl CompilerArgs {
    fn new(mut args: Args) -> Self {
        let mut ca = CompilerArgs {
            output_file: None,
            output_riscv: false,
            input_file: None,
        };
        args.next();

        while let Some(value) = args.next() {
            match &value[..] {
                "-o" => {
                    ca.output_file = args.next().map(|v| v.into());
                }
                "-riscv" => ca.output_riscv = true,
                v => {
                    ca.input_file = Some(v.into());
                }
            }
        }

        return ca;
    }
}

fn main() -> Result<()> {
    let args = args();
    let args = CompilerArgs::new(args);
    if args.input_file.is_none() {
        panic!("need input source code path");
    }

    let mut compiler = SysyCompiler::new(args.input_file.unwrap())?;

    compiler.generate_ast();

    // let ast = parser.ast.as_ref().unwrap();
    // println!("{:#?}", ast);

    let ir = compiler.get_ir().unwrap();

    let output = if args.output_riscv {
        ir_enhance::generate_riscv(ir)
    } else {
        let mut gen = KoopaGenerator::new(Vec::new());
        gen.generate_on(&ir).unwrap();
        let text_form_ir = from_utf8(&gen.writer()).unwrap().to_string();
        text_form_ir
    };

    if let Some(output_path) = args.output_file {
        let mut file = File::create(output_path).expect("cloud not create file");
        let _ = file.write_all(output.as_bytes());
    } else {
        println!("output: \n{}", output);
    }

    Ok(())
}
