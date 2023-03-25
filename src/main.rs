use std::fs::File;
use std::io::Read;
use std::{env, path::PathBuf};

use lazy_wspace::ast::{self, Lexer, Parser};
use lazy_wspace::error::Error;
use lazy_wspace::ir::{self, DisplayCfgWithDag};

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

    let mut cfg = ast::Cfg::new(&prog);
    print!("===== AST CFG =====\n\n{}", cfg);
    cfg.eliminate_dead();
    print!("\n\n===== After dead-code elimination =====\n\n{}", cfg);
    let cfg = ir::Cfg::new(&cfg);
    print!("\n\n===== CFG =====\n\n{}", cfg);
    print!(
        "\n\n===== CFG with DAG =====\n\n{}",
        DisplayCfgWithDag(&cfg)
    );
}
