use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());

    Command::new("ghc")
        .arg("-o")
        .arg(out_dir.join("read_integer"))
        .arg("-odir")
        .arg(&out_dir)
        .arg("-hidir")
        .arg(&out_dir)
        .arg("tests/read_integer.hs")
        .status()
        .unwrap();

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=tests/read_integer.hs");
}
