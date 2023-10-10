use std::env;
use std::ffi::OsString;
use std::fs::File;
use std::io::{self, Read};

use lazy_wspace::ast::Parser;
use lazy_wspace::error::{Error, HaskellError};
use lazy_wspace::vm::VM;
use wspace_syntax::ws::lex::StdLexer;

fn main() {
    let args = env::args_os();
    if args.len() != 2 {
        Error::Usage
            .to_haskell(&HaskellError::current_exe(), &OsString::new())
            .handle();
    }
    let filename = args.skip(1).next().unwrap();

    let mut f = File::open(&filename).unwrap();
    let mut src = Vec::<u8>::new();
    f.read_to_end(&mut src).unwrap();
    let lex = StdLexer::from(&*src);
    let prog = Parser::new(lex).map(|(inst, _)| inst).collect::<Vec<_>>();

    let mut stdin = io::stdin().lock();
    let mut stdout = io::stdout().lock();
    let mut vm = VM::new(prog, &mut stdin, &mut stdout);
    if let Err(err) = vm.execute() {
        err.to_haskell(&HaskellError::current_exe(), filename.as_ref())
            .handle();
    }
}
