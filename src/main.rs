use std::str::from_utf8;
use std::{env::args, fs::read_to_string};
use std::io::Result;

use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;

use crate::ir_enhance::GenerateRiscV;
use crate::parser::SysyParser;

mod ast;
mod parser;
mod ir_enhance;

mod tests;

lalrpop_mod!(sysy);

fn main() -> Result<()> {
    let mut args = args();
    args.next();
    let mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    let mut parser = SysyParser::new(mode, input)?;

    parser.generate_ast();

    let ast = parser.ast.as_ref().unwrap();
    println!("{:#?}", ast);

    let ir = parser.get_ir().unwrap();
    let mut gen = KoopaGenerator::new(Vec::new());
    gen.generate_on(&ir).unwrap();
    let text_form_ir = from_utf8(&gen.writer()).unwrap().to_string();

    println!("IR: \n {}", text_form_ir);

    println!("riskv: \n {}", ir.generate_riscv());


    Ok(())

}
