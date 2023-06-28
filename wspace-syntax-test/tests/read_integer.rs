use std::env;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::str::from_utf8;

use rug::Integer;
use wspace_syntax::parse::ReadIntegerError;

struct Test {
    input: &'static str,
    output: Result<Integer, ReadIntegerError>,
}

impl Test {
    fn ok(input: &'static str, output: &'static str) -> Self {
        Test {
            input,
            output: Ok(Integer::parse(output).unwrap().into()),
        }
    }

    fn err(input: &'static str, err: ReadIntegerError) -> Self {
        Test {
            input,
            output: Err(err),
        }
    }
}

fn get_tests() -> Vec<Test> {
    vec![
        Test::ok("1", "1"),
        Test::err("bad", ReadIntegerError::NoParse),
    ]
}

#[test]
fn compare_with_haskell() {
    for test in get_tests() {
        assert_eq!(
            test.output,
            run_read_integer(test.input),
            "read {:?} :: Integer",
            test.input,
        );
    }
}

fn run_read_integer<B: AsRef<[u8]>>(b: B) -> Result<Integer, ReadIntegerError> {
    let mut path = PathBuf::from(env!("OUT_DIR"));
    path.push("read_integer");
    let mut cmd = Command::new(path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    cmd.stdin.as_mut().unwrap().write_all(b.as_ref()).unwrap();
    let out = cmd.wait_with_output().unwrap();
    let code = out.status.code().unwrap();

    if out.stderr.len() != 0 {
        if out.stdout.len() != 0 {
            panic!("Wrote to stdout and stderr");
        }
        let err = match out.stderr.as_slice() {
            b"read_integer: Prelude.read: no parse\n" if code == 1 => ReadIntegerError::NoParse,
            msg => match from_utf8(msg) {
                Ok(msg) => panic!("Unknown error with code {code}: {msg:?}"),
                Err(_) => {
                    let msg_str = String::from_utf8_lossy(msg);
                    panic!("Unknown error with code {code}: {msg_str:?} ({msg:?})");
                }
            },
        };
        return Err(err);
    }
    if code != 0 {
        panic!("Nonzero exit");
    }

    Ok(Integer::parse(out.stdout).unwrap().into())
}
