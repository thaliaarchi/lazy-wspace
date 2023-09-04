use crate::wsa::Opcode;

pub trait Dialect {
    fn from_mnemonic(mnemonic: &str) -> Option<Opcode>;

    fn to_mnemonic(opcode: Opcode) -> Option<&'static str>;
}
