use std::fs;
use std::path::Path;

use bitvec::prelude::*;
use wspace_syntax::ws::{
    ast::{Inst, LabelLit, Parser},
    lex::StdLexer,
};

use crate::error::{EagerError, Error, ParseError};
use crate::vm::Vm;

fn parse<P: AsRef<Path>>(path: P) -> Vec<Inst> {
    let full_path = Path::new("../tests").join(path);
    let src = fs::read(&full_path).unwrap_or_else(|err| panic!("read {full_path:?}: {err}"));
    let lex = StdLexer::from(&*src);
    Parser::new(lex).map(|(inst, _)| inst).collect()
}

fn test<P: AsRef<Path>, I: AsRef<[u8]>, R: Into<TestResult>, O: AsRef<[u8]>>(
    path: P,
    stdin: I,
    res: R,
    stdout: O,
) {
    let path = path.as_ref();
    let mut stdin = stdin.as_ref();
    let stdout = stdout.as_ref();
    let res = match res.into() {
        TestResult::Success => Ok(()),
        TestResult::Error(err) => Err(Error::from(err)),
    };

    let prog = parse(path);
    let mut actual_stdout = Vec::new();
    let mut vm = Vm::new(prog, &mut stdin, &mut actual_stdout);
    let actual_res = vm.execute();

    assert_eq!(actual_res, res, "{path:?}");
    drop(vm);
    assert_eq!(actual_stdout, stdout, "{path:?}");
}

enum TestResult {
    Success,
    Error(Error),
}

impl<T: Into<Error>> From<T> for TestResult {
    fn from(err: T) -> Self {
        TestResult::Error(err.into())
    }
}

mod io {
    use std::io::{self, Write};

    use super::*;

    struct ReadOnly;

    impl Write for ReadOnly {
        fn write(&mut self, _buf: &[u8]) -> io::Result<usize> {
            Err(io::ErrorKind::PermissionDenied.into())
        }

        fn flush(&mut self) -> io::Result<()> {
            Err(io::ErrorKind::PermissionDenied.into())
        }
    }

    #[test]
    fn buffer_size() {
        let mut stdin = &b""[..];
        let mut stdout = ReadOnly;

        let prog = parse("io/buffer_size/printi_8191_bytes.ws");
        let res = Vm::new(prog, &mut stdin, &mut stdout).execute();
        assert_eq!(res, Err(EagerError::FlushPermissionDenied.into()));

        let prog = parse("io/buffer_size/printi_8192_bytes.ws");
        let res = Vm::new(prog, &mut stdin, &mut stdout).execute();
        assert_eq!(res, Err(EagerError::PrintPermissionDenied.into()));
    }
}

#[rustfmt::skip]
mod parse {
    use super::*;

    #[test]
    fn incomplete_opcode() {
        test("parse/incomplete_opcode/s.ws", b"", ParseError::IncompleteOpcode, b"");
        test("parse/incomplete_opcode/st.ws", b"", ParseError::IncompleteOpcode, b"");
        test("parse/incomplete_opcode/sl.ws", b"", ParseError::IncompleteOpcode, b"");
        test("parse/incomplete_opcode/t.ws", b"", ParseError::IncompleteOpcode, b"");
        test("parse/incomplete_opcode/ts.ws", b"", ParseError::IncompleteOpcode, b"");
        test("parse/incomplete_opcode/tss.ws", b"", ParseError::IncompleteOpcode, b"");
        test("parse/incomplete_opcode/tst.ws", b"", ParseError::IncompleteOpcode, b"");
        test("parse/incomplete_opcode/tt.ws", b"", ParseError::IncompleteOpcode, b"");
        test("parse/incomplete_opcode/tl.ws", b"", ParseError::IncompleteOpcode, b"");
        test("parse/incomplete_opcode/tls.ws", b"", ParseError::IncompleteOpcode, b"");
        test("parse/incomplete_opcode/tlt.ws", b"", ParseError::IncompleteOpcode, b"");
        test("parse/incomplete_opcode/l.ws", b"", ParseError::IncompleteOpcode, b"");
        test("parse/incomplete_opcode/ll.ws", b"", ParseError::IncompleteOpcode, b"");
        test("parse/incomplete_opcode/ls.ws", b"", ParseError::IncompleteOpcode, b"");
        test("parse/incomplete_opcode/lt.ws", b"", ParseError::IncompleteOpcode, b"");
    }

    #[test]
    fn unrecognized_opcode() {
        test("parse/unrecognized_opcode/lls.ws", b"", ParseError::UnrecognizedOpcode, b"");
        test("parse/unrecognized_opcode/llt.ws", b"", ParseError::UnrecognizedOpcode, b"");
        test("parse/unrecognized_opcode/stt.ws", b"", ParseError::UnrecognizedOpcode, b"");
        test("parse/unrecognized_opcode/tll.ws", b"", ParseError::UnrecognizedOpcode, b"");
        test("parse/unrecognized_opcode/tlsl.ws", b"", ParseError::UnrecognizedOpcode, b"");
        test("parse/unrecognized_opcode/tltl.ws", b"", ParseError::UnrecognizedOpcode, b"");
        test("parse/unrecognized_opcode/tsl.ws", b"", ParseError::UnrecognizedOpcode, b"");
        test("parse/unrecognized_opcode/tstl.ws", b"", ParseError::UnrecognizedOpcode, b"");
        test("parse/unrecognized_opcode/ttl.ws", b"", ParseError::UnrecognizedOpcode, b"");
    }

    #[test]
    fn unterminated_arg() {
        test("parse/unterminated_arg/push_empty.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/push_+.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/push_0.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/push_1.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/push_-.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/push_-0.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/push_-1.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/copy_empty.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/copy_+.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/copy_0.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/copy_1.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/copy_-.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/copy_-0.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/copy_-1.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/slide_empty.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/slide_+.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/slide_0.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/slide_1.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/slide_-.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/slide_-0.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/slide_-1.ws", b"", ParseError::UnterminatedInteger, b"");
        test("parse/unterminated_arg/label_empty.ws", b"", ParseError::UnterminatedLabel, b"");
        test("parse/unterminated_arg/label_0.ws", b"", ParseError::UnterminatedLabel, b"");
        test("parse/unterminated_arg/label_1.ws", b"", ParseError::UnterminatedLabel, b"");
        test("parse/unterminated_arg/call_empty.ws", b"", ParseError::UnterminatedLabel, b"");
        test("parse/unterminated_arg/call_0.ws", b"", ParseError::UnterminatedLabel, b"");
        test("parse/unterminated_arg/call_1.ws", b"", ParseError::UnterminatedLabel, b"");
        test("parse/unterminated_arg/jmp_empty.ws", b"", ParseError::UnterminatedLabel, b"");
        test("parse/unterminated_arg/jmp_0.ws", b"", ParseError::UnterminatedLabel, b"");
        test("parse/unterminated_arg/jmp_1.ws", b"", ParseError::UnterminatedLabel, b"");
        test("parse/unterminated_arg/jz_0.ws", b"", ParseError::UnterminatedLabel, b"");
        test("parse/unterminated_arg/jz_1.ws", b"", ParseError::UnterminatedLabel, b"");
        test("parse/unterminated_arg/jz_empty.ws", b"", ParseError::UnterminatedLabel, b"");
        test("parse/unterminated_arg/jn_empty.ws", b"", ParseError::UnterminatedLabel, b"");
        test("parse/unterminated_arg/jn_0.ws", b"", ParseError::UnterminatedLabel, b"");
        test("parse/unterminated_arg/jn_1.ws", b"", ParseError::UnterminatedLabel, b"");
    }

    #[test]
    fn undefined_label() {
        test("parse/undefined_label/call.ws", b"", ParseError::UndefinedLabel(LabelLit::new(bitvec![1, 0])), b"");
        test("parse/undefined_label/jmp.ws", b"", ParseError::UndefinedLabel(LabelLit::new(bitvec![1, 0])), b"");
        test("parse/undefined_label/jz_true.ws", b"", ParseError::UndefinedLabel(LabelLit::new(bitvec![1, 0])), b"");
        test("parse/undefined_label/jz_false.ws", b"", TestResult::Success, b"");
        test("parse/undefined_label/jn_true.ws", b"", ParseError::UndefinedLabel(LabelLit::new(bitvec![1, 0])), b"");
        test("parse/undefined_label/jn_false.ws", b"", TestResult::Success, b"");
    }

    #[test]
    fn invalid_utf8() {
        test("parse/invalid_utf8/bad_byte_1.ws", b"", ParseError::InvalidUtf8, b".");
        test("parse/invalid_utf8/bad_byte_2.ws", b"", ParseError::InvalidUtf8, b".");
        test("parse/invalid_utf8/bad_byte_3.ws", b"", ParseError::InvalidUtf8, b".");
        test("parse/invalid_utf8/bad_byte_4.ws", b"", ParseError::InvalidUtf8, b".");
        test("parse/invalid_utf8/incomplete_2.ws", b"", ParseError::InvalidUtf8, b".");
        test("parse/invalid_utf8/incomplete_3.ws", b"", ParseError::InvalidUtf8, b".");
        test("parse/invalid_utf8/incomplete_4.ws", b"", ParseError::InvalidUtf8, b".");
        test("parse/invalid_utf8/overlong.ws", b"", ParseError::InvalidUtf8, b".");
        test("parse/invalid_utf8/too_large.ws", b"", ParseError::InvalidUtf8, b".");
        test("parse/invalid_utf8/unexpected_continuation.ws", b"", ParseError::InvalidUtf8, b".");
        test("parse/invalid_utf8/unpaired_high_surrogate.ws", b"", ParseError::InvalidUtf8, b".");
        test("parse/invalid_utf8/unpaired_low_surrogate.ws", b"", ParseError::InvalidUtf8, b".");
    }

    #[test]
    fn implicit_end() {
        test("parse/empty_file.ws", b"", ParseError::ImplicitEnd, b"");
        test("parse/implicit_end.ws", b"", ParseError::ImplicitEnd, b".");
    }
}
