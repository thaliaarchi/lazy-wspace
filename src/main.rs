use std::fs::File;
use std::io::Read;
use std::{env, path::PathBuf};

use lazy_wspace::ast::{self, Inst, Lexer, Parser};
use lazy_wspace::error::Error;
use lazy_wspace::ir;

fn main() {
    let mut args = env::args_os();
    let wspace_path = args.next().map(|p| PathBuf::from(p));
    let wspace = wspace_path
        .as_ref()
        .and_then(|p| p.file_name())
        .map(|p| p.to_string_lossy())
        .unwrap_or("wspace".into());
    if args.len() != 1 {
        Error::Usage.to_haskell(&wspace, "").handle();
    }
    let filename = args.next().unwrap();

    let mut f = File::open(&filename).unwrap();
    let mut src = Vec::<u8>::new();
    f.read_to_end(&mut src).unwrap();

    let prog: Vec<_> = Parser::new(Lexer::new(&src)).collect();
    println!("===== AST =====\n");
    for inst in &prog {
        match inst {
            Inst::Label(l) => println!("{l}:"),
            _ => println!("    {inst}"),
        }
    }

    let mut ast_cfg = ast::Cfg::new(&prog);
    println!("\n\n===== AST CFG =====\n");
    print!("{ast_cfg}");

    ast_cfg.eliminate_dead();
    println!("\n\n===== AST CFG after dead-code elimination =====\n");
    print!("{ast_cfg}");

    let ir_cfg = ir::Cfg::new(&ast_cfg);
    println!("\n\n===== IR =====\n");
    print!("{ir_cfg}");
}
