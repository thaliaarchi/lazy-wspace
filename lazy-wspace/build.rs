use std::env;
use std::fs;
use std::path::Path;

use lazy_wspace_generate::{pretty, Node};

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("rewrites.rs");
    let src = pretty(Node::check_exhaustive()).unwrap();
    fs::write(&dest_path, &src).unwrap();
    println!("cargo:rerun-if-changed=build.rs");
}
