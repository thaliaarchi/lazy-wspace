use std::path::Path;

use crate::error::ParseError;
use crate::inst::ArgKind;
use crate::vm::execute_file;

#[test]
fn execute_expected() {
    let base: &Path = "../yspace/tests".as_ref();
    macro_rules! test(($path:literal, $stdin:literal => $res:expr, $stdout:literal) => {{
        let (res, stdout) = execute_file(base.join($path), &$stdin[..]);
        assert_eq!($res, res);
        assert_eq!($stdout, stdout.as_slice());
    }});

    test!("parse/incomplete_s.ws", b"" => Err(ParseError::IncompleteInst.into()), b"");
    test!("parse/incomplete_st.ws", b"" => Err(ParseError::IncompleteInst.into()), b"");
    test!("parse/incomplete_sl.ws", b"" => Err(ParseError::IncompleteInst.into()), b"");
    test!("parse/incomplete_t.ws", b"" => Err(ParseError::IncompleteInst.into()), b"");
    test!("parse/incomplete_ts.ws", b"" => Err(ParseError::IncompleteInst.into()), b"");
    test!("parse/incomplete_tss.ws", b"" => Err(ParseError::IncompleteInst.into()), b"");
    test!("parse/incomplete_tst.ws", b"" => Err(ParseError::IncompleteInst.into()), b"");
    test!("parse/incomplete_tt.ws", b"" => Err(ParseError::IncompleteInst.into()), b"");
    test!("parse/incomplete_tl.ws", b"" => Err(ParseError::IncompleteInst.into()), b"");
    test!("parse/incomplete_tls.ws", b"" => Err(ParseError::IncompleteInst.into()), b"");
    test!("parse/incomplete_tlt.ws", b"" => Err(ParseError::IncompleteInst.into()), b"");
    test!("parse/incomplete_l.ws", b"" => Err(ParseError::IncompleteInst.into()), b"");
    test!("parse/incomplete_ll.ws", b"" => Err(ParseError::IncompleteInst.into()), b"");
    test!("parse/incomplete_ls.ws", b"" => Err(ParseError::IncompleteInst.into()), b"");
    test!("parse/incomplete_lt.ws", b"" => Err(ParseError::IncompleteInst.into()), b"");

    test!("parse/unrecognized_lls.ws", b"" => Err(ParseError::UnrecognizedInst.into()), b"");
    test!("parse/unrecognized_llt.ws", b"" => Err(ParseError::UnrecognizedInst.into()), b"");
    test!("parse/unrecognized_stt.ws", b"" => Err(ParseError::UnrecognizedInst.into()), b"");
    test!("parse/unrecognized_tll.ws", b"" => Err(ParseError::UnrecognizedInst.into()), b"");
    test!("parse/unrecognized_tlsl.ws", b"" => Err(ParseError::UnrecognizedInst.into()), b"");
    test!("parse/unrecognized_tltl.ws", b"" => Err(ParseError::UnrecognizedInst.into()), b"");
    test!("parse/unrecognized_tsl.ws", b"" => Err(ParseError::UnrecognizedInst.into()), b"");
    test!("parse/unrecognized_tstl.ws", b"" => Err(ParseError::UnrecognizedInst.into()), b"");
    test!("parse/unrecognized_ttl.ws", b"" => Err(ParseError::UnrecognizedInst.into()), b"");

    test!("parse/unterminated_push.ws", b"" => Err(ParseError::UnterminatedArg(ArgKind::Number).into()), b"");
    test!("parse/unterminated_copy.ws", b"" => Err(ParseError::UnterminatedArg(ArgKind::Number).into()), b"");
    test!("parse/unterminated_slide.ws", b"" => Err(ParseError::UnterminatedArg(ArgKind::Number).into()), b"");
    test!("parse/unterminated_label.ws", b"" => Err(ParseError::UnterminatedArg(ArgKind::Label).into()), b"");
    test!("parse/unterminated_call.ws", b"" => Err(ParseError::UnterminatedArg(ArgKind::Label).into()), b"");
    test!("parse/unterminated_jmp.ws", b"" => Err(ParseError::UnterminatedArg(ArgKind::Label).into()), b"");
    test!("parse/unterminated_jz.ws", b"" => Err(ParseError::UnterminatedArg(ArgKind::Label).into()), b"");
    test!("parse/unterminated_jn.ws", b"" => Err(ParseError::UnterminatedArg(ArgKind::Label).into()), b"");
}
