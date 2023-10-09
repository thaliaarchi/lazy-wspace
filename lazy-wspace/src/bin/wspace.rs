use std::fs::File;
use std::io::{self, Read};
use std::{env, path::PathBuf};

use lazy_wspace::ast::Parser;
use lazy_wspace::error::Error;
use lazy_wspace::vm::VM;
use wspace_syntax::ws::lex::{ByteMatcher, Lexer};

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
    let lex = ByteMatcher::default().lex(&src).into_extended();
    let prog = Parser::new(lex).map(|(inst, _)| inst).collect::<Vec<_>>();

    let mut stdin = io::stdin().lock();
    let mut stdout = io::stdout().lock();
    let mut vm = VM::new(prog, &mut stdin, &mut stdout);
    if let Err(err) = vm.execute() {
        err.to_haskell(&wspace, &filename.to_string_lossy())
            .handle();
    }
}
