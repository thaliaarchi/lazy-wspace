use crate::ws::lex::PatternLexer;
use crate::ws::Token;

pub fn gorispace_ja() -> PatternLexer {
    PatternLexer::new(
        Token::L,
        "ウ(?:[^ウッホーイ]*ホ)+[^ウッホーイ]*ー[^ウッホーイ]*イ",
        Token::T,
        "ウ[^ウッホーイ]*ッ(?:[^ウッホーイ]*ホ)+",
        Token::S,
        "ウ(?:[^ウッホーイ]*ホ)+",
    )
    .unwrap()
}

pub fn gorispace_en() -> PatternLexer {
    PatternLexer::new(
        Token::L,
        "w[^hoswragh]*r(?:[^hoswragh]*a){2,}[^hoswragh]*g[^hoswragh]*h",
        Token::T,
        "h(?:[^hoswragh]*o){2,}[^hoswragh]*s",
        Token::S,
        "h(?:[^hoswragh]*o){2,}",
    )
    .unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    #[test]
    fn lex_gorispace_ja() {
        // Excerpt from https://github.com/technohippy/gorispace/blob/master/samples/fact.gs
        let src = "
        ウホホウホホホウホホホ、ウホ。ウホホホホーイウホホホ、ウホウホホホウッホホウホウホホホホウホホ、
        ウッホホウホホホウッホホウホーイ。ウッホホホ。ウッホホホホウホホ。ウホホ、ウホホホホ。ウホホホホ
        ウッホホホウホーイ。ウホホホ、ウホホホホ。ウホウッホホ。ウッホホ、ウホウッホホ。ウッホ、ウッホホホホ
        ウホウホホホホーイウッホホホ。ウッホホ、ウホホホホウホホホウホホホウホホホ。ウッホホホウホホホホ。
        ウホホホホーイウホホホホ。ウホホホホウホウッホホホホ。ウッホホホホ、ウッホホホウホウッホ。
        ";
        let toks = vec![
            S, S, S, S, L, S, S, S, T, S, S, S, T, S, T, L, T, T, S, S, S, S, T, L, S, S, S, T, T,
            S, T, T, T, S, L, T, T, S, S, S, S, T, S, L, S, S, S, T, T, T, S, T,
        ];
        let lexed_toks = gorispace_ja().lex(src).collect::<Vec<_>>();
        assert_eq!(toks, lexed_toks);
    }

    #[test]
    fn lex_gorispace_en() {
        // Excerpt from https://github.com/technohippy/gorispace/blob/master/samples/en/fact.gs
        let src = "
        hoooo, hoooo hoo hooo. wraagh. hoooo, hoo hooo hooos. hooo. hoo, hoooo hoooos,
        hoo hooos. wraaaagh hooos hooos, hooo, hoo hooo hooo hooos wraaagh, hooo hoooo,
        hooo hooos hoooos hoooo hoooos hooos. hooos hoooo wraaagh. hooos, hooos, hoooo
        hoo hoo, hoo hooos, hoooo. wraaaagh, hooo hooo hooo, hooos hoos hoooos hoooo
        hoooos, hoo. hoooo, wraaaagh, hooos hooos hoooo hoo, hoo hoooo. hooos hoooos,
        ";
        let toks = vec![
            S, S, S, S, L, S, S, S, T, S, S, S, T, S, T, L, T, T, S, S, S, S, T, L, S, S, S, T, T,
            S, T, T, T, S, L, T, T, S, S, S, S, T, S, L, S, S, S, T, T, T, S, T, S, S, L, T, T, S,
            S, S, S, T, T,
        ];
        let lexed_toks = gorispace_en().lex(src).collect::<Vec<_>>();
        assert_eq!(toks, lexed_toks);
    }

    #[test]
    fn gorispace_error() {
        // Gorispace ignores invalid sequences of [ウッホーイ]+ or [hoswragh]+
        let lex = gorispace_en();
        assert_eq!(vec![S, T], lex.lex("hoo howro, hoos").collect::<Vec<_>>());
    }
}
