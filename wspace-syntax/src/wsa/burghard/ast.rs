use phf::{phf_map, Map};
use uncased::UncasedStr;

use crate::wsa::{Arith, Cond, Dialect, Dump, Opcode};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BurghardDialect;

impl Dialect for BurghardDialect {
    fn from_mnemonic(mnemonic: &str) -> Option<Opcode> {
        static MNEMONICS: Map<&'static UncasedStr, Opcode> = phf_map! {
            UncasedStr::new("push") => Opcode::Push,
            UncasedStr::new("pushs") => Opcode::PushString,
            UncasedStr::new("doub") => Opcode::Dup,
            UncasedStr::new("swap") => Opcode::Swap,
            UncasedStr::new("pop") => Opcode::Drop,
            UncasedStr::new("add") => Opcode::Arith(Arith::Add),
            UncasedStr::new("sub") => Opcode::Arith(Arith::Sub),
            UncasedStr::new("mul") => Opcode::Arith(Arith::Mul),
            UncasedStr::new("div") => Opcode::Arith(Arith::Div),
            UncasedStr::new("mod") => Opcode::Arith(Arith::Mod),
            UncasedStr::new("store") => Opcode::Store,
            UncasedStr::new("retrive") => Opcode::Retrieve,
            UncasedStr::new("label") => Opcode::Label,
            UncasedStr::new("call") => Opcode::Call,
            UncasedStr::new("jump") => Opcode::Jmp,
            UncasedStr::new("jumpz") => Opcode::Branch(Cond::Zero),
            UncasedStr::new("jumpn") => Opcode::Branch(Cond::Neg),
            UncasedStr::new("jumpp") => Opcode::Branch(Cond::Pos),
            UncasedStr::new("jumpnp") => Opcode::Branch(Cond::NonZero),
            UncasedStr::new("jumppn") => Opcode::Branch(Cond::NonZero),
            UncasedStr::new("jumppz") => Opcode::Branch(Cond::NonNeg),
            UncasedStr::new("jumpnz") => Opcode::Branch(Cond::NonPos),
            UncasedStr::new("ret") => Opcode::Ret,
            UncasedStr::new("exit") => Opcode::End,
            UncasedStr::new("outc") => Opcode::Printc,
            UncasedStr::new("outn") => Opcode::Printi,
            UncasedStr::new("inc") => Opcode::Readc,
            UncasedStr::new("inn") => Opcode::Readi,
            UncasedStr::new("debug_printstack") => Opcode::Dump(Dump::DebugPrintStack),
            UncasedStr::new("debug_printheap") => Opcode::Dump(Dump::DebugPrintHeap),
            UncasedStr::new("test") => Opcode::Test,
            UncasedStr::new("include") => Opcode::Include,
            UncasedStr::new("valueinteger") => Opcode::DefineInteger,
            UncasedStr::new("valuestring") => Opcode::DefineString,
            UncasedStr::new("option") => Opcode::DefineOption,
            UncasedStr::new("ifoption") => Opcode::IfOption,
            UncasedStr::new("elseifoption") => Opcode::ElseIfOption,
            UncasedStr::new("elseoption") => Opcode::ElseOption,
            UncasedStr::new("endoption") => Opcode::EndOption,
        };

        MNEMONICS.get(UncasedStr::new(mnemonic)).copied()
    }

    fn to_mnemonic(opcode: Opcode) -> Option<&'static str> {
        match opcode {
            Opcode::Push => Some("push"),
            Opcode::PushString => Some("pushs"),
            Opcode::Dup => Some("doub"),
            Opcode::Swap => Some("swap"),
            Opcode::Drop => Some("pop"),
            Opcode::Arith(Arith::Add) => Some("add"),
            Opcode::Arith(Arith::Sub) => Some("sub"),
            Opcode::Arith(Arith::Mul) => Some("mul"),
            Opcode::Arith(Arith::Div) => Some("div"),
            Opcode::Arith(Arith::Mod) => Some("mod"),
            Opcode::Store => Some("store"),
            Opcode::Retrieve => Some("retrive"),
            Opcode::Label => Some("label"),
            Opcode::Call => Some("call"),
            Opcode::Jmp => Some("jump"),
            Opcode::Branch(Cond::Zero) => Some("jumpz"),
            Opcode::Branch(Cond::Neg) => Some("jumpn"),
            Opcode::Branch(Cond::Pos) => Some("jumpp"),
            Opcode::Branch(Cond::NonZero) => Some("jumpnp"),
            Opcode::Branch(Cond::NonNeg) => Some("jumppz"),
            Opcode::Branch(Cond::NonPos) => Some("jumpnz"),
            Opcode::Ret => Some("ret"),
            Opcode::End => Some("exit"),
            Opcode::Printc => Some("outc"),
            Opcode::Printi => Some("outn"),
            Opcode::Readc => Some("inc"),
            Opcode::Readi => Some("inn"),
            Opcode::Dump(Dump::DebugPrintStack) => Some("debug_printstack"),
            Opcode::Dump(Dump::DebugPrintHeap) => Some("debug_printheap"),
            Opcode::Test => Some("test"),
            Opcode::Include => Some("include"),
            Opcode::DefineInteger => Some("valueinteger"),
            Opcode::DefineString => Some("valuestring"),
            Opcode::DefineOption => Some("option"),
            Opcode::IfOption => Some("ifoption"),
            Opcode::ElseIfOption => Some("elseifoption"),
            Opcode::ElseOption => Some("elseoption"),
            Opcode::EndOption => Some("endoption"),
            _ => None,
        }
    }
}
