use std::fs;
use std::path::{Path, PathBuf};

use bitvec::prelude::*;
use wspace_syntax::ws::{
    ast::{Inst, IntegerLit, Parser},
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

struct TestGroup {
    dir: PathBuf,
}

impl TestGroup {
    fn new<P: Into<PathBuf>>(dir: P) -> Self {
        TestGroup { dir: dir.into() }
    }

    fn test<P: AsRef<Path>, I: AsRef<[u8]>, R: Into<TestResult>, O: AsRef<[u8]>>(
        &self,
        path: P,
        stdin: I,
        res: R,
        stdout: O,
    ) {
        test(self.dir.join(path.as_ref()), stdin, res, stdout);
    }
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
use TestResult::Success;

impl<T: Into<Error>> From<T> for TestResult {
    fn from(err: T) -> Self {
        TestResult::Error(err.into())
    }
}

mod bounds {
    use super::*;

    #[test]
    fn copy() {
        // Valid for 64-bit.
        let g = TestGroup::new("bounds/copy");
        g.test("copy_2^29-1.ws", b"", ValueError::CopyLarge, b"");
        g.test("copy_2^29.ws", b"", ValueError::CopyLarge, b"");
        g.test("copy_2^31-1.ws", b"", ValueError::CopyLarge, b"");
        g.test("copy_2^31.ws", b"", ValueError::CopyLarge, b"");
        g.test("copy_2^32-1.ws", b"", ValueError::CopyLarge, b"");
        g.test("copy_2^32.ws", b"", ValueError::CopyLarge, b"");
        g.test("copy_2^63-1.ws", b"", ValueError::CopyLarge, b"");
        g.test("copy_2^63.ws", b"", ValueError::CopyNegative, b"");
        g.test("copy_2^64-1.ws", b"", ValueError::CopyNegative, b"");
        g.test("copy_2^64.ws", b"", Success, b"42");
    }

    #[test]
    fn slide() {
        // Valid for 64-bit.
        let err = EagerError::Underflow(Inst::Printi);
        let g = TestGroup::new("bounds/slide");
        g.test("slide_2^29-1.ws", b"", err.clone(), b"");
        g.test("slide_2^29.ws", b"", err.clone(), b"");
        g.test("slide_2^31-1.ws", b"", err.clone(), b"");
        g.test("slide_2^31.ws", b"", err.clone(), b"");
        g.test("slide_2^32-1.ws", b"", err.clone(), b"");
        g.test("slide_2^32.ws", b"", err.clone(), b"");
        g.test("slide_2^63-1.ws", b"", err.clone(), b"");
        g.test("slide_2^63.ws", b"", Success, b"1");
        g.test("slide_2^64-1.ws", b"", Success, b"1");
        g.test("slide_2^64.ws", b"", Success, b"1");
    }
}

mod io {
    use std::io::{self, Write};
    use std::rc::Rc;

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

    #[test]
    fn utf8() {
        let g = TestGroup::new("io/utf8");
        g.test(
            "printc_low_surrogate.ws",
            b"",
            EagerError::PrintcSurrogate(0xDFFF),
            b"",
        );
        g.test(
            "printc_high_surrogate.ws",
            b"",
            EagerError::PrintcSurrogate(0xD800),
            b"",
        );
        g.test(
            "printc_negative.ws",
            b"",
            EagerError::PrintcInvalidRange(Rc::new((-1).into())),
            b"",
        );
        g.test("printc_maximum.ws", b"", Success, "\u{10FFFF}".as_bytes());
        g.test(
            "printc_too_large.ws",
            b"",
            EagerError::PrintcInvalidRange(Rc::new(0x110000.into())),
            b"",
        );
    }
}

mod lazy {
    use super::*;

    #[test]
    fn arith() {
        const LHS_FIRST: ValueError = ValueError::CopyLarge;
        const RHS_FIRST: ValueError = ValueError::RetrieveNegative;
        let g = TestGroup::new("lazy/arith");
        g.test("add.ws", b"", RHS_FIRST, b".");
        g.test("sub.ws", b"", RHS_FIRST, b".");
        g.test("mul.ws", b"", LHS_FIRST, b".");
        g.test("div.ws", b"", RHS_FIRST, b".");
        g.test("mod.ws", b"", RHS_FIRST, b".");
        g.test("div_0.ws", b"", ValueError::DivModZero, b".");
        g.test("mod_0.ws", b"", ValueError::DivModZero, b".");
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
            let g = TestGroup::new("lazy/slide_empty/size0");
            g.test("push.ws", b"", NO_FORCE, b"");
            g.test("dup.ws", b"", FORCE, b"");
            g.test("copy.ws", b"", FORCE, b"");
            g.test("swap.ws", b"", FORCE, b"");
            g.test("drop.ws", b"", FORCE, b"");
            g.test("slide.ws", b"", FORCE, b"");
            g.test("add.ws", b"", FORCE, b"");
            g.test("sub.ws", b"", FORCE, b"");
            g.test("mul.ws", b"", FORCE, b"");
            g.test("div.ws", b"", FORCE, b"");
            g.test("mod.ws", b"", FORCE, b"");
            g.test("store.ws", b"", FORCE, b"");
            g.test("retrieve.ws", b"", FORCE, b"");
            g.test("label.ws", b"", FORCE, b"");
            g.test("call.ws", b"", FORCE, b"");
            g.test("jmp.ws", b"", FORCE, b"");
            g.test("jz.ws", b"", FORCE, b"");
            g.test("jn.ws", b"", FORCE, b"");
            g.test("ret.ws", b"", FORCE, b"");
            g.test("end.ws", b"", FORCE, b"");
            g.test("printc.ws", b"", FORCE, b"");
            g.test("printi.ws", b"", FORCE, b"");
            g.test("readc.ws", b"", FORCE, b"");
            g.test("readi.ws", b"", FORCE, b"");
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
            let g = TestGroup::new("lazy/slide_empty/size1");
            g.test("push.ws", b"", NO_FORCE, b"");
            g.test("dup.ws", b"", NO_FORCE, b"");
            g.test("copy.ws", b"", NO_FORCE, b"");
            g.test("swap.ws", b"", FORCE, b"");
            g.test("drop.ws", b"", FORCE, b"");
            g.test("slide.ws", b"", NO_FORCE, b"");
            g.test("add.ws", b"", FORCE, b"");
            g.test("sub.ws", b"", FORCE, b"");
            g.test("mul.ws", b"", FORCE, b"");
            g.test("div.ws", b"", FORCE, b"");
            g.test("mod.ws", b"", FORCE, b"");
            g.test("store.ws", b"", FORCE, b"");
            g.test("retrieve.ws", b"", FORCE, b"");
            g.test("label.ws", b"", FORCE, b"");
            g.test("call.ws", b"", FORCE, b"");
            g.test("jmp.ws", b"", FORCE, b"");
            g.test("jz.ws", b"", FORCE, b"");
            g.test("jn.ws", b"", FORCE, b"");
            g.test("ret.ws", b"", FORCE, b"");
            g.test("end.ws", b"", FORCE, b"");
            g.test("printc.ws", b"", FORCE, b"");
            g.test("printi.ws", b"", FORCE, b"");
            g.test("readc.ws", b"", FORCE, b"");
            g.test("readi.ws", b"", FORCE, b"");
            g.test("eof.ws", b"", NO_FORCE, b"");
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
        #[test]
        fn size2() {
            let lbl = ParseError::UndefinedLabel(bitvec![1, 1].into());
            let g = TestGroup::new("lazy/slide_empty/size2");
            g.test("push.ws", b"", NO_FORCE, b"");
            g.test("dup.ws", b"", NO_FORCE, b"");
            g.test("copy.ws", b"", NO_FORCE, b"");
            g.test("swap.ws", b"", NO_FORCE, b"");
            g.test("drop.ws", b"", NO_FORCE, b"");
            g.test("slide.ws", b"", NO_FORCE, b"");
            g.test("add.ws", b"", NO_FORCE, b"");
            g.test("sub.ws", b"", NO_FORCE, b"");
            g.test("mul.ws", b"", NO_FORCE, b"");
            g.test("div.ws", b"", NO_FORCE, b"");
            g.test("mod.ws", b"", NO_FORCE, b"");
            g.test("store.ws", b"", NO_FORCE, b"");
            g.test("retrieve.ws", b"", NO_FORCE, b"");
            g.test("label.ws", b"", NO_FORCE, b"");
            g.test("call.ws", b"", lbl.clone(), b"");
            g.test("jmp.ws", b"", lbl.clone(), b"");
            g.test("jz.ws", b"", NO_FORCE, b"");
            g.test("jn.ws", b"", NO_FORCE, b"");
            g.test("ret.ws", b"", EagerError::RetUnderflow, b"");
            g.test("end.ws", b"", Success, b"");
            g.test("printc.ws", b"", NO_FORCE, b"\x02");
            g.test("printi.ws", b"", NO_FORCE, b"2");
            g.test("readc.ws", b".", NO_FORCE, b"");
            g.test("readi.ws", b"42", NO_FORCE, b"");
        }
    }

    #[test]
    fn other() {
        use ValueError::*;
        let g = TestGroup::new("lazy");
        g.test("copy_empty.ws", b"", EmptyLit, b".");
        g.test("copy_negative.ws", b"", CopyNegative, b".");
        g.test("copy_too_large.ws", b"", CopyLarge, b".");
        g.test("retrieve_negative.ws", b"", RetrieveNegative, b".");
        g.test("retrieve_too_large.ws", b"", RetrieveLarge, b".");
    }
}

mod parse {
    use super::*;

    #[test]
    fn incomplete_opcode() {
        let g = TestGroup::new("parse/incomplete_opcode");
        g.test("s.ws", b"", ParseError::IncompleteOpcode, b"");
        g.test("st.ws", b"", ParseError::IncompleteOpcode, b"");
        g.test("sl.ws", b"", ParseError::IncompleteOpcode, b"");
        g.test("t.ws", b"", ParseError::IncompleteOpcode, b"");
        g.test("ts.ws", b"", ParseError::IncompleteOpcode, b"");
        g.test("tss.ws", b"", ParseError::IncompleteOpcode, b"");
        g.test("tst.ws", b"", ParseError::IncompleteOpcode, b"");
        g.test("tt.ws", b"", ParseError::IncompleteOpcode, b"");
        g.test("tl.ws", b"", ParseError::IncompleteOpcode, b"");
        g.test("tls.ws", b"", ParseError::IncompleteOpcode, b"");
        g.test("tlt.ws", b"", ParseError::IncompleteOpcode, b"");
        g.test("l.ws", b"", ParseError::IncompleteOpcode, b"");
        g.test("ll.ws", b"", ParseError::IncompleteOpcode, b"");
        g.test("ls.ws", b"", ParseError::IncompleteOpcode, b"");
        g.test("lt.ws", b"", ParseError::IncompleteOpcode, b"");
    }

    #[test]
    fn unrecognized_opcode() {
        let g = TestGroup::new("parse/unrecognized_opcode");
        g.test("lls.ws", b"", ParseError::UnrecognizedOpcode, b"");
        g.test("llt.ws", b"", ParseError::UnrecognizedOpcode, b"");
        g.test("stt.ws", b"", ParseError::UnrecognizedOpcode, b"");
        g.test("tll.ws", b"", ParseError::UnrecognizedOpcode, b"");
        g.test("tlsl.ws", b"", ParseError::UnrecognizedOpcode, b"");
        g.test("tltl.ws", b"", ParseError::UnrecognizedOpcode, b"");
        g.test("tsl.ws", b"", ParseError::UnrecognizedOpcode, b"");
        g.test("tstl.ws", b"", ParseError::UnrecognizedOpcode, b"");
        g.test("ttl.ws", b"", ParseError::UnrecognizedOpcode, b"");
    }

    #[test]
    fn unterminated_arg() {
        let g = TestGroup::new("parse/unterminated_arg");
        g.test("push_empty.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("push_+.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("push_0.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("push_1.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("push_-.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("push_-0.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("push_-1.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("copy_empty.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("copy_+.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("copy_0.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("copy_1.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("copy_-.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("copy_-0.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("copy_-1.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("slide_empty.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("slide_+.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("slide_0.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("slide_1.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("slide_-.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("slide_-0.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("slide_-1.ws", b"", ParseError::UnterminatedInteger, b"");
        g.test("label_empty.ws", b"", ParseError::UnterminatedLabel, b"");
        g.test("label_0.ws", b"", ParseError::UnterminatedLabel, b"");
        g.test("label_1.ws", b"", ParseError::UnterminatedLabel, b"");
        g.test("call_empty.ws", b"", ParseError::UnterminatedLabel, b"");
        g.test("call_0.ws", b"", ParseError::UnterminatedLabel, b"");
        g.test("call_1.ws", b"", ParseError::UnterminatedLabel, b"");
        g.test("jmp_empty.ws", b"", ParseError::UnterminatedLabel, b"");
        g.test("jmp_0.ws", b"", ParseError::UnterminatedLabel, b"");
        g.test("jmp_1.ws", b"", ParseError::UnterminatedLabel, b"");
        g.test("jz_0.ws", b"", ParseError::UnterminatedLabel, b"");
        g.test("jz_1.ws", b"", ParseError::UnterminatedLabel, b"");
        g.test("jz_empty.ws", b"", ParseError::UnterminatedLabel, b"");
        g.test("jn_empty.ws", b"", ParseError::UnterminatedLabel, b"");
        g.test("jn_0.ws", b"", ParseError::UnterminatedLabel, b"");
        g.test("jn_1.ws", b"", ParseError::UnterminatedLabel, b"");
    }

    #[test]
    fn undefined_label() {
        let err = ParseError::UndefinedLabel(bitvec![1, 0].into());
        let g = TestGroup::new("parse/undefined_label");
        g.test("call.ws", b"", err.clone(), b"");
        g.test("jmp.ws", b"", err.clone(), b"");
        g.test("jz_true.ws", b"", err.clone(), b"");
        g.test("jz_false.ws", b"", Success, b"");
        g.test("jn_true.ws", b"", err.clone(), b"");
        g.test("jn_false.ws", b"", Success, b"");
    }

    #[test]
    fn invalid_utf8() {
        use ParseError::InvalidUtf8;
        let g = TestGroup::new("parse/invalid_utf8");
        g.test("bad_byte_1.ws", b"", InvalidUtf8, b".");
        g.test("bad_byte_2.ws", b"", InvalidUtf8, b".");
        g.test("bad_byte_3.ws", b"", InvalidUtf8, b".");
        g.test("bad_byte_4.ws", b"", InvalidUtf8, b".");
        g.test("incomplete_2.ws", b"", InvalidUtf8, b".");
        g.test("incomplete_3.ws", b"", InvalidUtf8, b".");
        g.test("incomplete_4.ws", b"", InvalidUtf8, b".");
        g.test("overlong.ws", b"", InvalidUtf8, b".");
        g.test("too_large.ws", b"", InvalidUtf8, b".");
        g.test("unexpected_continuation.ws", b"", InvalidUtf8, b".");
        g.test("unpaired_high_surrogate.ws", b"", InvalidUtf8, b".");
        g.test("unpaired_low_surrogate.ws", b"", InvalidUtf8, b".");
    }

    #[test]
    fn implicit_end() {
        let g = TestGroup::new("parse");
        g.test("empty_file.ws", b"", ParseError::ImplicitEnd, b"");
        g.test("implicit_end.ws", b"", ParseError::ImplicitEnd, b".");
    }
}

#[test]
fn underflow() {
    let g = TestGroup::new("underflow");
    g.test("dup.ws", b"", EagerError::Underflow(Inst::Dup), b"");
    g.test("swap.ws", b"", EagerError::Underflow(Inst::Swap), b"");
    g.test("swap_lhs.ws", b"", EagerError::Underflow(Inst::Swap), b"");
    g.test("drop.ws", b"", EagerError::Underflow(Inst::Drop), b"");
    g.test(
        "slide.ws",
        b"",
        EagerError::Underflow(Inst::Slide(IntegerLit::new(1))),
        b"",
    );
    g.test("slide_empty.ws", b"", ValueError::EmptyLit, b"");
    g.test(
        "slide_zero.ws",
        b"",
        EagerError::Underflow(Inst::Slide(IntegerLit::new(0))),
        b"",
    );
    g.test(
        "slide_negative.ws",
        b"",
        EagerError::Underflow(Inst::Slide(IntegerLit::new(-1))),
        b"",
    );
    g.test("add.ws", b"", EagerError::Underflow(Inst::Add), b"");
    g.test("add_lhs.ws", b"", EagerError::Underflow(Inst::Add), b"");
    g.test("sub.ws", b"", EagerError::Underflow(Inst::Sub), b"");
    g.test("sub_lhs.ws", b"", EagerError::Underflow(Inst::Sub), b"");
    g.test("mul.ws", b"", EagerError::Underflow(Inst::Mul), b"");
    g.test("mul_lhs.ws", b"", EagerError::Underflow(Inst::Mul), b"");
    g.test("div.ws", b"", EagerError::Underflow(Inst::Div), b"");
    g.test("div_lhs.ws", b"", EagerError::Underflow(Inst::Div), b"");
    g.test("mod.ws", b"", EagerError::Underflow(Inst::Mod), b"");
    g.test("mod_lhs.ws", b"", EagerError::Underflow(Inst::Mod), b"");
    g.test("store.ws", b"", EagerError::Underflow(Inst::Store), b"");
    g.test("store_lhs.ws", b"", EagerError::Underflow(Inst::Store), b"");
    g.test(
        "retrieve.ws",
        b"",
        EagerError::Underflow(Inst::Retrieve),
        b"",
    );
    g.test(
        "jz.ws",
        b"",
        EagerError::Underflow(Inst::Jz(bitvec![1, 0].into())),
        b"",
    );
    g.test(
        "jn.ws",
        b"",
        EagerError::Underflow(Inst::Jn(bitvec![1, 0].into())),
        b"",
    );
    g.test("ret.ws", b"", EagerError::RetUnderflow, b"");
    g.test("printc.ws", b"", EagerError::Underflow(Inst::Printc), b"");
    g.test("printi.ws", b"", EagerError::Underflow(Inst::Printi), b"");
    g.test("readc.ws", b"", EagerError::Underflow(Inst::Readc), b"");
    g.test("readi.ws", b"", EagerError::Underflow(Inst::Readi), b"");
}
