use std::str::FromStr;

use phf::{phf_map, Map};
use rug::Integer;
use strum::EnumDiscriminants;
use uncased::UncasedStr;

use crate::parse::read_integer_haskell;
use crate::wsa::burghard::Word;

/// Instruction in the Burghard Whitespace assembly dialect.
#[derive(Clone, Debug, EnumDiscriminants, PartialEq, Eq, Hash)]
#[strum_discriminants(name(Opcode), derive(Hash))]
pub enum Inst<'a> {
    // Standard instructions
    Push(ParsedInteger<'a>),
    Doub,
    Swap,
    Pop,
    Add(Option<ParsedInteger<'a>>),
    Sub(Option<ParsedInteger<'a>>),
    Mul(Option<ParsedInteger<'a>>),
    Div(Option<ParsedInteger<'a>>),
    Mod(Option<ParsedInteger<'a>>),
    Store(Option<ParsedInteger<'a>>),
    Retrive(Option<ParsedInteger<'a>>),
    Label(Word<'a>),
    Call(Word<'a>),
    Jump(Word<'a>),
    JumpZ(Word<'a>),
    JumpN(Word<'a>),
    Ret,
    Exit,
    OutC,
    OutN,
    InC,
    InN,
    // Extension instructions
    DebugPrintStack,
    DebugPrintHeap,
    // Macro instructions
    PushS(ParsedString<'a>),
    JumpP(Word<'a>),
    JumpNZ(Word<'a>),
    JumpPZ(Word<'a>),
    JumpNP(Word<'a>),
    Include(Word<'a>),
    Test(ParsedInteger<'a>),
    ValueI(ParsedVariable<'a>, ParsedInteger<'a>),
    ValueS(ParsedVariable<'a>, ParsedString<'a>),
    // Preprocessor instructions
    Option(Word<'a>),
    IfOption(Word<'a>),
    ElseIfOption(Word<'a>),
    ElseOption,
    EndOption,
    /// A malformed instruction
    Error(InstError<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]

pub enum InstError<'a> {
    Arity(Vec<Word<'a>>),
}

impl Inst<'_> {
    #[inline]
    pub fn opcode(&self) -> Opcode {
        Opcode::from(self)
    }
}

impl FromStr for Opcode {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        static MNEMONICS: Map<&'static UncasedStr, Opcode> = phf_map! {
            UncasedStr::new("push") => Opcode::Push,
            UncasedStr::new("doub") => Opcode::Doub,
            UncasedStr::new("swap") => Opcode::Swap,
            UncasedStr::new("pop") => Opcode::Pop,
            UncasedStr::new("add") => Opcode::Add,
            UncasedStr::new("sub") => Opcode::Sub,
            UncasedStr::new("mul") => Opcode::Mul,
            UncasedStr::new("div") => Opcode::Div,
            UncasedStr::new("mod") => Opcode::Mod,
            UncasedStr::new("store") => Opcode::Store,
            UncasedStr::new("retrive") => Opcode::Retrive,
            UncasedStr::new("label") => Opcode::Label,
            UncasedStr::new("call") => Opcode::Call,
            UncasedStr::new("jump") => Opcode::Jump,
            UncasedStr::new("jumpz") => Opcode::JumpZ,
            UncasedStr::new("jumpn") => Opcode::JumpN,
            UncasedStr::new("ret") => Opcode::Ret,
            UncasedStr::new("exit") => Opcode::Exit,
            UncasedStr::new("outc") => Opcode::OutC,
            UncasedStr::new("outn") => Opcode::OutN,
            UncasedStr::new("inc") => Opcode::InC,
            UncasedStr::new("inn") => Opcode::InN,
            UncasedStr::new("debug_printstack") => Opcode::DebugPrintStack,
            UncasedStr::new("debug_printheap") => Opcode::DebugPrintHeap,
            UncasedStr::new("pushs") => Opcode::PushS,
            UncasedStr::new("jumpp") => Opcode::JumpP,
            UncasedStr::new("jumpnz") => Opcode::JumpNZ,
            UncasedStr::new("jumppz") => Opcode::JumpPZ,
            // JumpNP has two mnemonics:
            UncasedStr::new("jumpnp") => Opcode::JumpNP,
            UncasedStr::new("jumppn") => Opcode::JumpNP,
            UncasedStr::new("include") => Opcode::Include,
            UncasedStr::new("test") => Opcode::Test,
            UncasedStr::new("valueinteger") => Opcode::ValueI,
            UncasedStr::new("valuestring") => Opcode::ValueS,
            UncasedStr::new("option") => Opcode::Option,
            UncasedStr::new("ifoption") => Opcode::IfOption,
            UncasedStr::new("elseifoption") => Opcode::ElseIfOption,
            UncasedStr::new("elseoption") => Opcode::ElseOption,
            UncasedStr::new("endoption") => Opcode::EndOption,
        };

        MNEMONICS.get(&UncasedStr::new(s)).copied().ok_or(())
    }
}

impl Opcode {
    pub fn mnemonic(&self) -> Option<&'static str> {
        match *self {
            Opcode::Push => Some("push"),
            Opcode::Doub => Some("doub"),
            Opcode::Swap => Some("swap"),
            Opcode::Pop => Some("pop"),
            Opcode::Add => Some("add"),
            Opcode::Sub => Some("sub"),
            Opcode::Mul => Some("mul"),
            Opcode::Div => Some("div"),
            Opcode::Mod => Some("mod"),
            Opcode::Store => Some("store"),
            Opcode::Retrive => Some("retrive"),
            Opcode::Label => Some("label"),
            Opcode::Call => Some("call"),
            Opcode::Jump => Some("jump"),
            Opcode::JumpZ => Some("jumpz"),
            Opcode::JumpN => Some("jumpn"),
            Opcode::Ret => Some("ret"),
            Opcode::Exit => Some("exit"),
            Opcode::OutC => Some("outc"),
            Opcode::OutN => Some("outn"),
            Opcode::InC => Some("inc"),
            Opcode::InN => Some("inn"),
            Opcode::DebugPrintStack => Some("debug_printstack"),
            Opcode::DebugPrintHeap => Some("debug_printheap"),
            Opcode::PushS => Some("pushs"),
            Opcode::JumpP => Some("jumpp"),
            Opcode::JumpNZ => Some("jumpnz"),
            Opcode::JumpPZ => Some("jumppz"),
            Opcode::JumpNP => Some("jumpnp"),
            Opcode::Include => Some("include"),
            Opcode::Test => Some("test"),
            Opcode::ValueI => Some("valueinteger"),
            Opcode::ValueS => Some("valuestring"),
            Opcode::Option => Some("option"),
            Opcode::IfOption => Some("ifoption"),
            Opcode::ElseIfOption => Some("elseifoption"),
            Opcode::ElseOption => Some("elseoption"),
            Opcode::EndOption => Some("endoption"),
            Opcode::Error => None,
        }
    }

    pub fn with_args<'a>(self, words: &mut Vec<Word<'a>>) -> Inst<'a> {
        let mut words = words.drain(..);
        macro_rules! next(() => { words.next().unwrap().into() });
        match (self, words.len()) {
            (Opcode::Push, 1) => Inst::Push(next!()),
            (Opcode::PushS, 1) => Inst::PushS(next!()),
            (Opcode::Doub, 0) => Inst::Doub,
            (Opcode::Swap, 0) => Inst::Swap,
            (Opcode::Pop, 0) => Inst::Pop,
            (Opcode::Add, 0) => Inst::Add(None),
            (Opcode::Sub, 0) => Inst::Sub(None),
            (Opcode::Mul, 0) => Inst::Mul(None),
            (Opcode::Div, 0) => Inst::Div(None),
            (Opcode::Mod, 0) => Inst::Mod(None),
            (Opcode::Add, 1) => Inst::Add(Some(next!())),
            (Opcode::Sub, 1) => Inst::Sub(Some(next!())),
            (Opcode::Mul, 1) => Inst::Mul(Some(next!())),
            (Opcode::Div, 1) => Inst::Div(Some(next!())),
            (Opcode::Mod, 1) => Inst::Mod(Some(next!())),
            (Opcode::Store, 0) => Inst::Store(None),
            (Opcode::Retrive, 0) => Inst::Retrive(None),
            (Opcode::Store, 1) => Inst::Store(Some(next!())),
            (Opcode::Retrive, 1) => Inst::Retrive(Some(next!())),
            (Opcode::Label, 1) => Inst::Label(next!()),
            (Opcode::Call, 1) => Inst::Call(next!()),
            (Opcode::Jump, 1) => Inst::Jump(next!()),
            (Opcode::JumpZ, 1) => Inst::JumpZ(next!()),
            (Opcode::JumpN, 1) => Inst::JumpN(next!()),
            (Opcode::JumpP, 1) => Inst::JumpP(next!()),
            (Opcode::JumpNZ, 1) => Inst::JumpNZ(next!()),
            (Opcode::JumpPZ, 1) => Inst::JumpPZ(next!()),
            (Opcode::JumpNP, 1) => Inst::JumpNP(next!()),
            (Opcode::Ret, 0) => Inst::Ret,
            (Opcode::Exit, 0) => Inst::Exit,
            (Opcode::OutC, 0) => Inst::OutC,
            (Opcode::OutN, 0) => Inst::OutN,
            (Opcode::InC, 0) => Inst::InC,
            (Opcode::InN, 0) => Inst::InN,
            (Opcode::DebugPrintStack, 0) => Inst::DebugPrintStack,
            (Opcode::DebugPrintHeap, 0) => Inst::DebugPrintHeap,
            (Opcode::Include, 1) => Inst::Include(next!()),
            (Opcode::Test, 1) => Inst::Test(next!()),
            (Opcode::ValueI, 2) => Inst::ValueI(next!(), next!()),
            (Opcode::ValueS, 2) => Inst::ValueS(next!(), next!()),
            (Opcode::Option, 1) => Inst::Option(next!()),
            (Opcode::IfOption, 1) => Inst::IfOption(next!()),
            (Opcode::ElseIfOption, 1) => Inst::ElseIfOption(next!()),
            (Opcode::ElseOption, 0) => Inst::ElseOption,
            (Opcode::EndOption, 0) => Inst::EndOption,
            _ => Inst::Error(InstError::Arity(words.collect())),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ParsedWord<'a, T> {
    pub word: Word<'a>,
    pub value: T,
}

type ParsedInteger<'a> = ParsedWord<'a, IntegerValue>;
type ParsedString<'a> = ParsedWord<'a, StringValue>;
type ParsedVariable<'a> = ParsedWord<'a, VariableValue>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IntegerValue {
    Literal(Integer),
    LiteralError,
    Variable,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StringValue {
    Literal,
    Variable,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum VariableValue {
    Variable,
    Error,
}

impl Word<'_> {
    pub fn is_variable(&self) -> bool {
        matches!(self.text.as_bytes(), [b'_', ..])
    }
}

impl<'a> From<Word<'a>> for ParsedInteger<'a> {
    fn from(word: Word<'a>) -> Self {
        let value = if word.is_variable() {
            IntegerValue::Variable
        } else {
            match read_integer_haskell(&word.text) {
                Ok(n) => IntegerValue::Literal(n),
                Err(_) => IntegerValue::LiteralError,
            }
        };
        ParsedInteger { word, value }
    }
}

impl<'a> From<Word<'a>> for ParsedString<'a> {
    fn from(word: Word<'a>) -> Self {
        let value = if word.is_variable() {
            StringValue::Variable
        } else {
            StringValue::Literal
        };
        ParsedString { word, value }
    }
}

impl<'a> From<Word<'a>> for ParsedVariable<'a> {
    fn from(word: Word<'a>) -> Self {
        let value = if word.is_variable() {
            VariableValue::Variable
        } else {
            VariableValue::Error
        };
        ParsedVariable { word, value }
    }
}
