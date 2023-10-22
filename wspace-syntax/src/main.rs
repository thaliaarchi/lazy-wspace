use std::env;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::process;

use wspace_syntax::ws::{
    ast::{Inst, Parser},
    lex::StdLexer,
};

fn main() {
    let mut args = env::args_os().skip(1);
    if args.len() != 2 {
        eprintln!("Usage: wspace-syntax disasm <filename>");
        process::exit(2);
    }
    let mode = args.next().unwrap().into_string().unwrap();
    let filename = PathBuf::from(args.next().unwrap());
    if mode != "disasm" {
        eprintln!("Supported modes: disasm");
        process::exit(2);
    }

    let mut f = File::open(&filename).unwrap();
    let mut src = Vec::new();
    f.read_to_end(&mut src).unwrap();

    let lex = StdLexer::from_utf8_lazy(&src);
    let prog = Parser::new(lex).map(|(inst, _)| inst).collect::<Vec<_>>();

    for inst in &prog {
        if let Inst::Label(l) = inst {
            println!("{l}:");
        } else {
            println!("    {inst}");
        }
    }
}
