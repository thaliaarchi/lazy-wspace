mod tutorial {
    use bitvec::prelude::*;

    use crate::ws::lex::{ByteMatcher, BytesMatcher};
    use crate::ws::{Builder, FormatTokens, Inst, IntegerLit, LabelLit, Mapping, Token};
    use Token::*;

    const SRC_STL: &str = "   \t\n\n   \t    \t\t\n \n \t\n \t   \t \t \n\t\n     \t\n\t    \n    \t \t\t\n\t  \t\n\t  \t   \t \t\n\n \n \t    \t\t\n\n   \t   \t \t\n \n\n\n\n\n";

    // https://web.archive.org/web/20150618184706/http://compsoc.dur.ac.uk/whitespace/tutorial.php
    const SRC_ANNOTATED: &str = "[Space][Space][Space][Tab][LF]                                            Put a 1 on the stack
[LF][Space][Space][Space][Tab][Space][Space] [Space][Space][Tab][Tab][LF] Set a Label at this point
[Space][LF][Space]                                                        Duplicate the top stack item
[Tab][LF][Space][Tab]                                                     Output the current value
[Space][Space][Space][Tab][Space][Tab][Space][LF]                         Put 10 (newline) on the stack...
[Tab][LF][Space][Space]                                                   ...and output the newline
[Space][Space][Space][Tab][LF]                                            Put a 1 on the stack
[Tab][Space][Space][Space]                                                Addition. This increments our current value.
[Space][LF][Space]                                                        Duplicate that value so we can test it
[Space][Space][Space][Tab][Space][Tab][Tab][LF]                           Push 11 onto the stack
[Tab][Space][Space][Tab]                                                  Subtraction. So if we've reached the end, we have a zero on the stack.
[LF][Tab][Space][Space][Tab][Space][Space] [Space][Tab][Space][Tab][LF]   If we have a zero, jump to the end
[LF][Space][LF][Space][Tab][Space] [Space][Space][Space][Tab][Tab][LF]    Jump to the start
[LF][Space][Space][Space][Tab][Space] [Space][Space][Tab][Space][Tab][LF] Set the end label
[Space][LF][LF]                                                           Discard our accumulator, to be tidy
[LF][LF][LF]                                                              Finish
";

    #[rustfmt::skip]
    const TOKENS: &[Token] = &[
        S, S, S, T, L,
        L, S, S, S, T, S, S, S, S, T, T, L,
        S, L, S,
        T, L, S, T,
        S, S, S, T, S, T, S, L,
        T, L, S, S,
        S, S, S, T, L,
        T, S, S, S,
        S, L, S,
        S, S, S, T, S, T, T, L,
        T, S, S, T,
        L, T, S, S, T, S, S, S, T, S, T, L,
        L, S, L, S, T, S, S, S, S, T, T, L,
        L, S, S, S, T, S, S, S, T, S, T, L,
        S, L, L,
        L, L, L,
    ];

    #[test]
    fn scan_byte() {
        let syntax = ByteMatcher::default();
        let toks = syntax
            .lex(SRC_STL.as_bytes())
            .map(|(tok, _)| tok)
            .collect::<Vec<_>>();
        assert_eq!(TOKENS, toks);
    }

    #[test]
    fn scan_string() {
        let syntax = BytesMatcher::new(b"[Space]", b"[Tab]", b"[LF]").unwrap();
        let toks = syntax
            .lex(SRC_ANNOTATED.as_bytes())
            .map(|(tok, _)| tok)
            .collect::<Vec<_>>();
        assert_eq!(TOKENS, toks);
    }

    #[test]
    fn build() {
        let label_c = LabelLit(bitvec![0, 1, 0, 0, 0, 0, 1, 1]);
        let label_e = LabelLit(bitvec![0, 1, 0, 0, 0, 1, 0, 1]);
        let prog = &[
            Inst::Push(IntegerLit::Pos(bitvec![1])),
            Inst::Label(label_c.clone()),
            Inst::Dup,
            Inst::Printi,
            Inst::Push(IntegerLit::Pos(bitvec![1, 0, 1, 0])),
            Inst::Printc,
            Inst::Push(IntegerLit::Pos(bitvec![1])),
            Inst::Add,
            Inst::Dup,
            Inst::Push(IntegerLit::Pos(bitvec![1, 0, 1, 1])),
            Inst::Sub,
            Inst::Jz(label_e.clone()),
            Inst::Jmp(label_c),
            Inst::Label(label_e),
            Inst::Drop,
            Inst::End,
        ];
        let map = Mapping::default();
        let mut w = map.writer();
        let mut b = Builder::new(&mut w);
        for inst in prog {
            inst.fmt_tokens(&mut b);
        }
        assert_eq!(SRC_STL.as_bytes(), w.finish());
    }
}
