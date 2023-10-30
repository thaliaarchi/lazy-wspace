use std::fs;
use std::path::Path;

use bitvec::prelude::*;
use wspace_syntax::ws::{
    ast::{Inst, Parser},
    lex::StdLexer,
};

use crate::error::{EagerError, Error, ParseError, ValueError};
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

mod lazy {
    use super::*;

    #[test]
    fn arith() {
        const LHS_FIRST: ValueError = ValueError::CopyLarge;
        const RHS_FIRST: ValueError = ValueError::RetrieveNegative;
        test("lazy/arith/add.ws", b"", RHS_FIRST, b".");
        test("lazy/arith/sub.ws", b"", RHS_FIRST, b".");
        test("lazy/arith/mul.ws", b"", LHS_FIRST, b".");
        test("lazy/arith/div.ws", b"", RHS_FIRST, b".");
        test("lazy/arith/mod.ws", b"", RHS_FIRST, b".");
        test("lazy/arith/div_0.ws", b"", ValueError::DivModZero, b".");
        test("lazy/arith/mod_0.ws", b"", ValueError::DivModZero, b".");
    }

    mod slide_empty {
        use super::*;

        const NO_FORCE: ParseError = ParseError::ImplicitEnd;
        const FORCE: ValueError = ValueError::EmptyLit;

        /// Tests each instruction, when the stack has the value
        /// `drop (last []) []` in the reference interpreter. The combination of
        /// `push 2` and `store` transforms the stack from
        /// `1 : drop (last []) []` to `drop (last []) []`, without forcing the
        /// error. Only `push` does not for evaluation of the error from slide,
        /// because it appears before the first pattern match on the stack in
        /// `doInstr`.
        ///
        /// ```wsa
        ///     push 1
        ///     slide <empty>
        ///     push 2
        ///     store
        ///     OP
        /// ```
        #[test]
        fn size0() {
            test("lazy/slide_empty/size0/push.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size0/dup.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/copy.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/swap.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/drop.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/slide.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/add.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/sub.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/mul.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/div.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/mod.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/store.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/retrieve.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/label.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/call.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/jmp.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/jz.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/jn.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/ret.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/end.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/printc.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/printi.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/readc.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size0/readi.ws", b"", FORCE, b"");
        }

        /// Tests each instruction, when the stack has the value
        /// `1 : drop (last []) []` in the reference interpreter. Instructions
        /// matched after the `Swap` case in `doInstr` force evaluation of the
        /// error from slide.
        ///
        /// ```wsa
        ///     push 1
        ///     slide <empty>
        ///     OP
        /// ````
        #[test]
        fn size1() {
            test("lazy/slide_empty/size1/push.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size1/dup.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size1/copy.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size1/swap.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/drop.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/slide.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size1/add.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/sub.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/mul.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/div.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/mod.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/store.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/retrieve.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/label.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/call.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/jmp.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/jz.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/jn.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/ret.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/end.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/printc.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/printi.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/readc.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/readi.ws", b"", FORCE, b"");
            test("lazy/slide_empty/size1/eof.ws", b"", NO_FORCE, b"");
        }

        /// Tests each instruction, when the stack has the value
        /// `2 : 1 : drop (last []) []` in the reference interpreter. No
        /// instructions force evaluation of the error from slide.
        ///
        /// ```wsa
        ///     push 1
        ///     slide <empty>
        ///     push 2
        ///     OP
        /// ````
        #[rustfmt::skip]
        #[test]
        fn size2() {
            test("lazy/slide_empty/size2/push.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size2/dup.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size2/copy.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size2/swap.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size2/drop.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size2/slide.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size2/add.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size2/sub.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size2/mul.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size2/div.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size2/mod.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size2/store.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size2/retrieve.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size2/label.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size2/call.ws", b"", ParseError::UndefinedLabel(bitvec![1, 1].into()), b"");
            test("lazy/slide_empty/size2/jmp.ws", b"", ParseError::UndefinedLabel(bitvec![1, 1].into()), b"");
            test("lazy/slide_empty/size2/jz.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size2/jn.ws", b"", NO_FORCE, b"");
            test("lazy/slide_empty/size2/ret.ws", b"", EagerError::RetUnderflow, b"");
            test("lazy/slide_empty/size2/end.ws", b"", TestResult::Success, b"");
            test("lazy/slide_empty/size2/printc.ws", b"", NO_FORCE, b"\x02");
            test("lazy/slide_empty/size2/printi.ws", b"", NO_FORCE, b"2");
            test("lazy/slide_empty/size2/readc.ws", b".", NO_FORCE, b"");
            test("lazy/slide_empty/size2/readi.ws", b"42", NO_FORCE, b"");
        }
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
        test("parse/undefined_label/call.ws", b"", ParseError::UndefinedLabel(bitvec![1, 0].into()), b"");
        test("parse/undefined_label/jmp.ws", b"", ParseError::UndefinedLabel(bitvec![1, 0].into()), b"");
        test("parse/undefined_label/jz_true.ws", b"", ParseError::UndefinedLabel(bitvec![1, 0].into()), b"");
        test("parse/undefined_label/jz_false.ws", b"", TestResult::Success, b"");
        test("parse/undefined_label/jn_true.ws", b"", ParseError::UndefinedLabel(bitvec![1, 0].into()), b"");
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
