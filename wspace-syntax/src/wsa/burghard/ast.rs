use std::borrow::Cow;
use std::str::FromStr;

use phf::{phf_map, Map};
use rug::Integer;
use uncased::UncasedStr;

use crate::parse::{read_integer_haskell, ReadIntegerError};

macro_rules! map(
    ( , $then:tt) => {};
    ($optional:tt, $then:tt) => { $then };
);

macro_rules! insts {
    (
        $(#[$inst_attr:meta])* $inst_vis:vis enum $Inst:ident
        $(#[$opcode_attr:meta])* $opcode_vis:vis enum $Opcode:ident
        $([$mnemonic:literal $(, $mnemonic_alias:literal)*] =>
            $Op:ident $(($($param:ty),+))?),* $(,)?
    ) => {
        $(#[$inst_attr])*
        #[derive(Clone, Debug, PartialEq, Eq, Hash)]
        $inst_vis enum $Inst {
            $($Op $(($($param),+))?),*
        }

        $(#[$opcode_attr])*
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        $opcode_vis enum $Opcode {
            $($Op),*
        }

        impl Inst {
            pub fn opcode(&self) -> Opcode {
                match self {
                    $($Inst::$Op $(($(map!($param, _)),+))? => $Opcode::$Op,)*
                }
            }
        }

        impl FromStr for $Opcode {
            type Err = ();

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                static MNEMONICS: Map<&'static UncasedStr, $Opcode> = phf_map! {
                    $(UncasedStr::new($mnemonic) => $Opcode::$Op,
                        $(UncasedStr::new($mnemonic_alias) => $Opcode::$Op,)*)*
                };

                MNEMONICS.get(&UncasedStr::new(s)).copied().ok_or(())
            }
        }

        impl AsRef<str> for $Opcode {
            fn as_ref(&self) -> &str {
                match *self {
                    $($Opcode::$Op => $mnemonic,)*
                }
            }
        }
    };
}

insts! {
    /// Instruction in the Burghard Whitespace assembly dialect.
    pub enum Inst

    /// Instruction opcode in the Burghard Whitespace assembly dialect.
    ///
    /// This is [`Inst`] without arguments.
    pub enum Opcode

    // Standard instructions
    ["push"] => Push(IntegerValue),
    ["doub"] => Doub,
    ["swap"] => Swap,
    ["pop"] => Pop,
    ["add"] => Add(Option<IntegerValue>),
    ["sub"] => Sub(Option<IntegerValue>),
    ["mul"] => Mul(Option<IntegerValue>),
    ["div"] => Div(Option<IntegerValue>),
    ["mod"] => Mod(Option<IntegerValue>),
    ["store"] => Store(Option<IntegerValue>),
    ["retrive"] => Retrive(Option<IntegerValue>),
    ["label"] => Label(String),
    ["call"] => Call(String),
    ["jump"] => Jump(String),
    ["jumpz"] => JumpZ(String),
    ["jumpn"] => JumpN(String),
    ["ret"] => Ret,
    ["exit"] => Exit,
    ["outc"] => OutC,
    ["outn"] => OutN,
    ["inc"] => InC,
    ["inn"] => InN,
    // Extension instructions
    ["debug_printstack"] => DebugPrintStack,
    ["debug_printheap"] => DebugPrintHeap,
    // Macro instructions
    ["pushs"] => PushS(StringValue),
    ["jumpp"] => JumpP(String),
    ["jumpnz"] => JumpNZ(String),
    ["jumppz"] => JumpPZ(String),
    ["jumpnp", "jumppn"] => JumpNP(String),
    ["include"] => Include(String),
    ["test"] => Test(IntegerValue),
    ["valueinteger"] => ValueI(Variable, IntegerValue),
    ["valuestring"] => ValueS(Variable, StringValue),
    // Preprocessor instructions
    ["option"] => Option(String),
    ["ifoption"] => IfOption(String),
    ["elseifoption"] => ElseIfOption(String),
    ["elseoption"] => ElseOption,
    ["endoption"] => EndOption,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Word<'a>(Cow<'a, str>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IntegerValue {
    Literal(Integer),
    Variable(Variable),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StringValue {
    Literal(String),
    Variable(Variable),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Variable(String);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ArgError {
    Arity,
    ParseInteger(ReadIntegerError),
    IdentMissingUnderscore,
}

impl Opcode {
    pub fn with_args(self, lhs: Option<Word<'_>>, rhs: Option<Word<'_>>) -> Result<Inst, ArgError> {
        match (self, lhs, rhs) {
            (Opcode::Push, Some(n), None) => Ok(Inst::Push(n.try_into()?)),
            (Opcode::PushS, Some(s), None) => Ok(Inst::PushS(s.into())),
            (Opcode::Doub, None, None) => Ok(Inst::Doub),
            (Opcode::Swap, None, None) => Ok(Inst::Swap),
            (Opcode::Pop, None, None) => Ok(Inst::Pop),
            (Opcode::Add, n, None) => Ok(Inst::Add(n.map(Word::try_into).transpose()?)),
            (Opcode::Sub, n, None) => Ok(Inst::Sub(n.map(Word::try_into).transpose()?)),
            (Opcode::Mul, n, None) => Ok(Inst::Mul(n.map(Word::try_into).transpose()?)),
            (Opcode::Div, n, None) => Ok(Inst::Div(n.map(Word::try_into).transpose()?)),
            (Opcode::Mod, n, None) => Ok(Inst::Mod(n.map(Word::try_into).transpose()?)),
            (Opcode::Store, n, None) => Ok(Inst::Store(n.map(Word::try_into).transpose()?)),
            (Opcode::Retrive, n, None) => Ok(Inst::Retrive(n.map(Word::try_into).transpose()?)),
            (Opcode::Label, Some(l), None) => Ok(Inst::Label(l.into())),
            (Opcode::Call, Some(l), None) => Ok(Inst::Call(l.into())),
            (Opcode::Jump, Some(l), None) => Ok(Inst::Jump(l.into())),
            (Opcode::JumpZ, Some(l), None) => Ok(Inst::JumpZ(l.into())),
            (Opcode::JumpN, Some(l), None) => Ok(Inst::JumpN(l.into())),
            (Opcode::JumpP, Some(l), None) => Ok(Inst::JumpP(l.into())),
            (Opcode::JumpNZ, Some(l), None) => Ok(Inst::JumpNZ(l.into())),
            (Opcode::JumpPZ, Some(l), None) => Ok(Inst::JumpPZ(l.into())),
            (Opcode::JumpNP, Some(l), None) => Ok(Inst::JumpNP(l.into())),
            (Opcode::Ret, None, None) => Ok(Inst::Ret),
            (Opcode::Exit, None, None) => Ok(Inst::Exit),
            (Opcode::OutC, None, None) => Ok(Inst::OutC),
            (Opcode::OutN, None, None) => Ok(Inst::OutN),
            (Opcode::InC, None, None) => Ok(Inst::InC),
            (Opcode::InN, None, None) => Ok(Inst::InN),
            (Opcode::DebugPrintStack, None, None) => Ok(Inst::DebugPrintStack),
            (Opcode::DebugPrintHeap, None, None) => Ok(Inst::DebugPrintHeap),
            (Opcode::Include, Some(f), None) => Ok(Inst::Include(f.into())),
            (Opcode::Test, Some(n), None) => Ok(Inst::Test(n.try_into()?)),
            (Opcode::ValueI, Some(v), Some(n)) => Ok(Inst::ValueI(v.try_into()?, n.try_into()?)),
            (Opcode::ValueS, Some(v), Some(s)) => Ok(Inst::ValueS(v.try_into()?, s.into())),
            (Opcode::Option, Some(opt), None) => Ok(Inst::Option(opt.into())),
            (Opcode::IfOption, Some(opt), None) => Ok(Inst::IfOption(opt.into())),
            (Opcode::ElseIfOption, Some(opt), None) => Ok(Inst::ElseIfOption(opt.into())),
            (Opcode::ElseOption, None, None) => Ok(Inst::ElseOption),
            (Opcode::EndOption, None, None) => Ok(Inst::EndOption),
            _ => Err(ArgError::Arity),
        }
    }
}

impl TryFrom<Word<'_>> for Variable {
    type Error = ArgError;

    fn try_from(word: Word<'_>) -> Result<Self, Self::Error> {
        if let [b'_', ..] = word.0.as_bytes() {
            Ok(Variable(word.0[1..].into()))
        } else {
            Err(ArgError::IdentMissingUnderscore)
        }
    }
}

impl From<Word<'_>> for String {
    fn from(word: Word<'_>) -> Self {
        word.0.into()
    }
}

impl TryFrom<Word<'_>> for IntegerValue {
    type Error = ArgError;

    fn try_from(word: Word<'_>) -> Result<Self, Self::Error> {
        Variable::try_from(word.clone())
            .map(IntegerValue::Variable)
            .or_else(|_| Ok(IntegerValue::Literal(read_integer_haskell(&word.0)?)))
    }
}

impl From<Word<'_>> for StringValue {
    fn from(word: Word<'_>) -> Self {
        Variable::try_from(word.clone())
            .map(StringValue::Variable)
            .unwrap_or_else(|_| StringValue::Literal(word.0.into()))
    }
}

impl From<ReadIntegerError> for ArgError {
    fn from(err: ReadIntegerError) -> Self {
        ArgError::ParseInteger(err)
    }
}
