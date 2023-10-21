use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::path::PathBuf;

use lazy_wspace::error::{Error, HaskellError, UsageError};
use lazy_wspace::vm::VM;
use wspace_syntax::ws::{ast::Parser, lex::StdLexer};

fn main() {
    if let Err((err, filename)) = try_main() {
        err.to_haskell(&HaskellError::current_exe(), filename.as_ref())
            .handle();
    }
}

fn try_main() -> Result<(), (Error, PathBuf)> {
    let args = env::args_os();
    if args.len() != 2 {
        return Err((UsageError::ArgumentCount.into(), PathBuf::new()));
    }
    let filename = PathBuf::from(args.skip(1).next().unwrap());

    let mut src = Vec::new();
    if let Err(err) = File::open(&filename).and_then(|mut f| f.read_to_end(&mut src)) {
        let err = match err.kind() {
            io::ErrorKind::NotFound => UsageError::NotFound,
            io::ErrorKind::PermissionDenied => UsageError::PermissionDenied,
            // TODO: Use io::ErrorKind::IsADirectory and perhaps InvalidFilename,
            // once stabilized. See https://github.com/rust-lang/rust/issues/86442.
            _ if filename.is_dir() => UsageError::IsADirectory,
            _ => panic!("unhandled error: {err}"),
        };
        return Err((err.into(), filename));
    }

    let lex = StdLexer::from_utf8_lazy(&src);
    let prog = Parser::new(lex).map(|(inst, _)| inst).collect::<Vec<_>>();

    let mut stdin = io::stdin().lock();
    let mut stdout = io::stdout().lock();
    let mut vm = VM::new(prog, &mut stdin, &mut stdout);
    vm.execute().map_err(|err| (err, filename))
}
