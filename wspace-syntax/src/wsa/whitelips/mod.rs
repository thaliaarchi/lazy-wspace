use logos::{Lexer, Logos};

#[derive(Clone, Copy, Debug, Logos, PartialEq, Eq, Hash)]
#[logos(skip r"[ \t\n\r]+")]
pub enum Token {
    #[regex(r"[a-zA-Z_$.][a-zA-Z0-9_$.]*")]
    Word,
    #[regex(r"[a-zA-Z_$.][a-zA-Z0-9_$.]*:")]
    #[regex(r"[+-]?\d+:")]
    Label,
    #[regex(r"[+-]?\d+")]
    Number,
    #[regex(r#""([^"\n\\]|\\.)*""#)]
    #[regex(r#"'([^'\n\\]|\\.)*'"#)]
    String,
    #[regex(r"(;|#|--)[^\n]*\n?")]
    #[token("{-", lex_block_comment)]
    Comment,
}

fn lex_block_comment(lex: &mut Lexer<Token>) -> bool {
    #[derive(Logos)]
    enum CommentToken {
        #[token("{-")]
        Start,
        #[token("-}")]
        End,
        #[regex(".")]
        Other,
    }

    let mut sub = CommentToken::lexer(lex.remainder());
    let mut depth = 1usize;
    while let Some(res) = sub.next() {
        match res {
            Ok(CommentToken::Start) => depth += 1,
            Ok(CommentToken::End) => {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            Ok(CommentToken::Other) => {}
            Err(()) => break,
        }
    }
    lex.bump(sub.span().end);
    depth == 0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn block_comments() {
        let mut lex = Token::lexer(" {-} ");
        assert_eq!(
            (lex.next(), lex.span(), lex.slice()),
            (Some(Err(())), 1..5, "{-} ")
        );
        assert_eq!(lex.next(), None);

        let mut lex = Token::lexer(" {-{{-}--}}-} --xyz\n");
        assert_eq!(
            (lex.next(), lex.span(), lex.slice()),
            (Some(Ok(Token::Comment)), 1..13, "{-{{-}--}}-}")
        );
        assert_eq!(
            (lex.next(), lex.span(), lex.slice()),
            (Some(Ok(Token::Comment)), 14..20, "--xyz\n")
        );
        assert_eq!(lex.next(), None);
    }
}
