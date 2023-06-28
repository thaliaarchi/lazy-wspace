use std::str::FromStr;

use phf::{phf_map, Map};
use uncased::UncasedStr;

macro_rules! opcodes {
    ($(#[doc = $inst_doc:literal])? enum $Inst:ident
            $(#[doc = $opcode_doc:literal])? enum $Opcode:ident
            $([$mnemonic:literal $(, $mnemonic_alias:literal)*] =>
            $Op:ident $($($param:ty)+)?),* $(,)?) => {
        $(#[doc = $inst_doc])?
        #[derive(Clone, Debug, PartialEq, Eq, Hash)]
        pub enum $Inst {
            $($Op $(($($param),*))?),*
        }

        $(#[doc = $opcode_doc])?
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub enum $Opcode {
            $($Op),*
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

opcodes! {
    /// Instruction in the Burghard Whitespace assembly dialect.
    enum Inst
    /// Instruction opcode in the Burghard Whitespace assembly dialect.
    enum Opcode

    // Standard instructions
    ["push"] => Push IntegerValue,
    ["doub"] => Doub,
    ["swap"] => Swap,
    ["pop"] => Pop,
    ["add"] => Add Option<IntegerValue>,
    ["sub"] => Sub Option<IntegerValue>,
    ["mul"] => Mul Option<IntegerValue>,
    ["div"] => Div Option<IntegerValue>,
    ["mod"] => Mod Option<IntegerValue>,
    ["store"] => Store Option<IntegerValue>,
    ["retrive"] => Retrive Option<IntegerValue>,
    ["label"] => Label String,
    ["call"] => Call String,
    ["jump"] => Jump String,
    ["jumpz"] => JumpZ String,
    ["jumpn"] => JumpN String,
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
    ["pushs"] => PushS StringValue,
    ["jumpp"] => JumpP String,
    ["jumpnz"] => JumpNZ String,
    ["jumppz"] => JumpPZ String,
    ["jumpnp", "jumppn"] => JumpNP,
    ["include"] => Include String,
    ["test"] => Test IntegerValue,
    ["valuestring"] => ValueS StringValue,
    ["valueinteger"] => ValueI IntegerValue,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IntegerValue {
    Literal,
    Variable,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StringValue {
    Literal,
    Variable,
}
