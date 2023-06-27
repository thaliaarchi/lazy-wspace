use std::str::FromStr;

use phf::{phf_map, Map};
use uncased::UncasedStr;

macro_rules! opcodes {
    (enum $Enum:ident
            $([$mnemonic:literal $(, $mnemonic_alias:literal)*] => $Opcode:ident),* $(,)?) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub enum $Enum {
            $($Opcode),*
        }

        impl FromStr for $Enum {
            type Err = ();

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                static MNEMONICS: Map<&'static UncasedStr, $Enum> = phf_map! {
                    $(UncasedStr::new($mnemonic) => $Enum::$Opcode,
                        $(UncasedStr::new($mnemonic_alias) => $Enum::$Opcode,)*)*
                };

                MNEMONICS.get(&UncasedStr::new(s)).copied().ok_or(())
            }
        }

        impl AsRef<str> for $Enum {
            fn as_ref(&self) -> &str {
                match *self {
                    $($Enum::$Opcode => $mnemonic,)*
                }
            }
        }
    };
}

opcodes! { enum Opcode
    // Standard instructions
    ["push"] => Push,
    ["doub"] => Doub,
    ["swap"] => Swap,
    ["pop"] => Pop,
    ["add"] => Add,
    ["sub"] => Sub,
    ["mul"] => Mul,
    ["div"] => Div,
    ["mod"] => Mod,
    ["store"] => Store,
    ["retrive"] => Retrive,
    ["label"] => Label,
    ["call"] => Call,
    ["jump"] => Jump,
    ["jumpz"] => JumpZ,
    ["jumpn"] => JumpN,
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
    ["pushs"] => PushS,
    ["jumpp"] => JumpP,
    ["jumpnz"] => JumpNZ,
    ["jumppz"] => JumpPZ,
    ["jumpnp", "jumppn"] => JumpNP,
    ["include"] => Include,
    ["test"] => Test,
    ["valuestring"] => ValueS,
    ["valueinteger"] => ValueI,
}
