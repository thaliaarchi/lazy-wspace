use std::env;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};

use rug::Integer;
use wspace_syntax::hs::{ReadIntegerBase, ReadIntegerError, ReadIntegerLit};

struct Test {
    input: String,
    output: Result<ReadIntegerLit, ReadIntegerError>,
}

impl Test {
    fn ok<S: Into<String>>(
        input: S,
        output: &'static str,
        is_negative: bool,
        base: ReadIntegerBase,
        leading_zeros: usize,
    ) -> Self {
        Test {
            input: input.into(),
            output: Ok(ReadIntegerLit {
                value: Integer::parse(output).unwrap().into(),
                is_negative,
                base,
                leading_zeros,
            }),
        }
    }

    fn err<S: Into<String>>(input: S, err: ReadIntegerError) -> Self {
        Test {
            input: input.into(),
            output: Err(err),
        }
    }
}

fn get_tests() -> Vec<Test> {
    use ReadIntegerBase::{Decimal as Dec, Hexadecimal as Hex, Octal as Oct};
    use ReadIntegerError::*;
    const POS: bool = false;
    const NEG: bool = true;

    let mut tests = vec![
        Test::ok("42", "42", POS, Dec, 0),
        // C-style bases
        Test::ok("0o42", "34", POS, Oct, 0),
        Test::ok("0O42", "34", POS, Oct, 0),
        Test::ok("0xff", "255", POS, Hex, 0),
        Test::ok("0Xff", "255", POS, Hex, 0),
        Test::ok("0Xff", "255", POS, Hex, 0),
        Test::err("0b101", InvalidDigit),
        Test::err("0B101", InvalidDigit),
        // Leading zeros
        Test::ok("000", "0", POS, Dec, 3),
        Test::ok("042", "42", POS, Dec, 1),
        Test::ok("00042", "42", POS, Dec, 3),
        Test::ok("0o00042", "34", POS, Oct, 3),
        Test::ok("0x000ff", "255", POS, Hex, 3),
        // Other styles
        Test::err("0d42", InvalidDigit),
        Test::err("2#101", InvalidDigit),
        Test::err("2#101#", InvalidDigit),
        Test::err("&b101", InvalidDigit),
        Test::err("&o42", InvalidDigit),
        Test::err("&hff", InvalidDigit),
        // Signs
        Test::ok("-42", "-42", NEG, Dec, 0),
        Test::err("+42", InvalidDigit),
        // Parentheses
        Test::ok("(42)", "42", POS, Dec, 0),
        Test::ok("((42))", "42", POS, Dec, 0),
        Test::ok("(((42)))", "42", POS, Dec, 0),
        Test::ok(" ( ( ( 42 ) ) ) ", "42", POS, Dec, 0),
        Test::ok("(-42)", "-42", NEG, Dec, 0),
        Test::err("-(42)", IllegalNeg),
        Test::err("-(-42)", IllegalNeg),
        Test::err("(--42)", InvalidDigit),
        Test::err("(- -42)", InvalidDigit),
        Test::err("(-(-42))", IllegalNeg),
        Test::err("(42", UnpairedParen),
        Test::err("42)", UnpairedParen),
        Test::err("((42)", UnpairedParen),
        Test::err("(42))", UnpairedParen),
        // Exponent
        Test::err("1e3", InvalidDigit),
        // Decimal point
        Test::err("3.14", InvalidDigit),
        // Digit separators
        Test::err("1_000", InvalidDigit),
        Test::err("1 000", InvalidDigit),
        Test::err("1,000", InvalidDigit),
        Test::err("1'000", InvalidDigit),
        Test::err("0o_42", InvalidDigit),
        Test::err("0Xf_f", InvalidDigit),
        Test::err("0O42_", InvalidDigit),
        // Larger than 128 bits
        Test::ok(
            "31415926535897932384626433832795028841971693993751",
            "31415926535897932384626433832795028841971693993751",
            POS,
            Dec,
            0,
        ),
        // Empty
        Test::err("", NoDigits),
        Test::err("-", NoDigits),
        // Operations
        Test::err("1+2", InvalidDigit),
        Test::err("1-2", InvalidDigit),
        Test::err("1*2", InvalidDigit),
        Test::err("1/2", InvalidDigit),
        Test::err("1%2", InvalidDigit),
        // Non-digits
        Test::err("9000over", InvalidDigit),
        Test::err("invalid", InvalidDigit),
    ];

    // All characters with the Unicode property White_Space, excluding non-ASCII
    // line-breaks, are allowed before or after the digits, or between the `-`
    // sign and the digits.
    let ok_spaces = [
        // Unicode White_Space
        '\t',       // Tab
        '\n',       // Line feed
        '\x0b',     // Vertical tab
        '\x0c',     // Form feed
        '\r',       // Carriage return
        ' ',        // Space
        '\u{00A0}', // No-break space
        '\u{1680}', // Ogham space mark
        '\u{2000}', // En quad
        '\u{2001}', // Em quad
        '\u{2002}', // En space
        '\u{2003}', // Em space
        '\u{2004}', // Three-per-em space
        '\u{2005}', // Four-per-em space
        '\u{2006}', // Six-per-em space
        '\u{2007}', // Figure space
        '\u{2008}', // Punctuation space
        '\u{2009}', // Thin space
        '\u{200A}', // Hair space
        '\u{202F}', // Narrow no-break space
        '\u{205F}', // Medium mathematical space
        '\u{3000}', // Ideographic space
    ];
    let err_spaces = [
        // Unicode White_Space
        '\u{0085}', // Next line
        '\u{2028}', // Line separator
        '\u{2029}', // Paragraph separator
        // Related Unicode characters
        '\u{180E}', // Mongolian vowel separator
        '\u{200B}', // Zero width space
        '\u{200C}', // Zero width non-joiner
        '\u{200D}', // Zero width joiner
        '\u{200E}', // Left-to-right mark
        '\u{200F}', // Right-to-left mark
        '\u{2060}', // Word joiner
        '\u{FEFF}', // Zero width non-breaking space
    ];
    for space in ok_spaces {
        tests.push(Test::err(format!("{space}"), NoDigits));
        tests.push(Test::ok(format!("{space}-42"), "-42", NEG, Dec, 0));
        tests.push(Test::ok(format!("-{space}42"), "-42", NEG, Dec, 0));
        tests.push(Test::err(format!("-4{space}2"), InvalidDigit));
        tests.push(Test::ok(format!("-42{space}"), "-42", NEG, Dec, 0));
    }
    for space in err_spaces {
        tests.push(Test::err(format!("{space}"), InvalidDigit));
        tests.push(Test::err(format!("{space}-42"), InvalidDigit));
        tests.push(Test::err(format!("-{space}42"), InvalidDigit));
        tests.push(Test::err(format!("-4{space}2"), InvalidDigit));
        tests.push(Test::err(format!("-42{space}"), InvalidDigit));
    }

    tests
}

#[test]
fn test_rust() {
    let mut digits = Vec::new();
    for test in get_tests() {
        assert_eq!(
            test.output,
            ReadIntegerLit::parse_with_buffer(&test.input, &mut digits),
            "{:?}.parse::<ReadIntegerLit>()",
            test.input,
        );
    }
}

#[test]
fn compare_with_haskell() {
    for test in get_tests() {
        assert_eq!(
            test.output.map(|lit| lit.value).ok(),
            run_read_integer(&test.input),
            "read {:?} :: Integer",
            test.input,
        );
    }
}

fn run_read_integer(s: &str) -> Option<Integer> {
    let mut path = PathBuf::from(env!("OUT_DIR"));
    path.push("read_integer");
    let mut cmd = Command::new(path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    cmd.stdin.as_mut().unwrap().write_all(s.as_ref()).unwrap();
    let out = cmd.wait_with_output().unwrap();
    let code = out.status.code().unwrap();

    if out.stderr.len() != 0 {
        if out.stdout.len() != 0 {
            panic!("Wrote to stdout and stderr");
        }
        // Since there's only one known error, Option is sufficient
        if out.stderr == b"read_integer: Prelude.read: no parse\n" && code == 1 {
            return None;
        }
        let msg = String::from_utf8_lossy(&out.stderr);
        panic!("Unknown error: {msg} with code {code}");
    }
    if code != 0 {
        panic!("Nonzero exit");
    }

    Some(Integer::parse(out.stdout).unwrap().into())
}
