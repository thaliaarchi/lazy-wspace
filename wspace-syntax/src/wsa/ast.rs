use std::path::PathBuf;

use rug::Integer;

use crate::source::{FileId, Span};

// TODO:
// - Interning: The AST should not depend on slices of the source, because that
//   gets messy once multiple sources are combined.

/// Interoperable abstract syntax tree for Whitespace assembly dialects.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Module {
    /// Module prefix for identifiers.
    pub name: Option<String>,
    pub block: Block,
    pub partitions: IdentPartition,
}

/// Sequence of instructions.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Block {
    pub insts: Vec<Inst>,
}

/// Instruction.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Inst {
    /// Instruction with a mnemonic.
    Mnemonic { mnemonic: Word, kind: InstKind },
    /// Bare label.
    Label(IdentArg),
}

/// Mnemonic-style instruction.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InstKind {
    Push(IntegerArg),
    PushString(StringArg),
    Dup,
    Copy(IntegerArg),
    Swap,
    Drop,
    Slide(IntegerArg),
    Arith {
        op: Arith,
        lhs: Option<IntegerArg>,
        rhs: Option<IntegerArg>,
    },
    Store {
        address: Option<IntegerArg>,
        value: Option<IntegerArg>,
    },
    Retrieve {
        address: Option<IntegerArg>,
    },
    Label(IdentArg),
    Call(IdentArg),
    Jmp(IdentArg),
    Branch(Cond, IdentArg),
    Ret,
    End,
    Printc {
        value: Option<IntegerArg>,
    },
    Printi {
        value: Option<IntegerArg>,
    },
    Readc {
        address: Option<IntegerArg>,
    },
    Readi {
        address: Option<IntegerArg>,
    },
    Dump(Dump),
    /// (Burghard `test`).
    Test(IntegerArg),
    /// (Burghard `include`).
    Include {
        path: PathArg,
        scope: IncludeScope,
    },
    /// (Burghard `valueinteger`).
    DefineInteger(IdentArg, IntegerArg),
    /// (Burghard `valuestring`).
    DefineString(IdentArg, StringArg),
    /// (Burghard `option`).
    DefineOption(IdentArg),
    /// (Burghard `ifoption`, `elseifoption`, `elseoption`, and `endoption`).
    IfOption {
        if_block: IfOption,
        elseif_blocks: Vec<IfOption>,
        else_block: Option<Box<Block>>,
    },
    Error(InstError),
}

/// If-then block for conditional compilation.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IfOption {
    pub option: Option<IdentArg>,
    pub block: Box<Block>,
}

/// Erroneous instruction.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InstError {
    Arity { op: Opcode, args: Vec<Arg<Word>> },
}

type IntegerArg = Arg<IntegerValue>;
type StringArg = Arg<StringValue>;
type PathArg = Arg<PathLit>;
type IdentArg = Arg<Ident>;

/// Instruction opcode.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Opcode {
    Push,
    PushString,
    Dup,
    Copy,
    Swap,
    Drop,
    Slide,
    Arith(Arith),
    Store,
    Retrieve,
    Label,
    Call,
    Jmp,
    Branch(Cond),
    Ret,
    End,
    Printc,
    Printi,
    Readc,
    Readi,
    Dump(Dump),
    Test,
    Include,
    DefineInteger,
    DefineString,
    DefineOption,
    IfOption,
    ElseOption,
    ElseIfOption,
    EndOption,
    Error,
}

/// Atom in Whitespace assembly syntax.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Word {
    pub text: String,
    pub format: WordFormat,
    pub span: Span,
}

/// Format of a word.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub enum WordFormat {
    /// Typical bare word.
    #[default]
    Bare,
    /// `"`-quoted word, which may contain spaces. It is unwrapped and treated
    /// identically to unquoted words after lexing (Burghard).
    DoubleQuoted,
    /// Consecutive unquoted words, that are spliced together by block comments
    /// without adjacent spaces (Burghard).
    Spliced,
}

/// Wrapper for an instruction argument.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Arg<T> {
    pub value: T,
    pub space_before: Option<Span>,
}

/// Integer literal or variable reference.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IntegerValue {
    Lit(IntegerLit),
    Var(Ident),
    Error(Word),
}

/// String literal or variable reference.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StringValue {
    Lit(StringLit),
    Var(Ident),
    Error(Word),
}

/// Integer literal.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IntegerLit {
    pub word: Word,
    pub value: Integer,
    pub base: Base,
}

/// Base (radix) for an integer literal.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub enum Base {
    #[default]
    Dec,
    Bin,
    Oct,
    Hex,
}

/// String literal.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StringLit {
    pub word: Word,
    pub value: Vec<u8>,
}

/// File path literal.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PathLit {
    pub word: Word,
    pub value: PathBuf,
}

/// Identifier.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ident {
    pub word: Word,
    pub ident: String,
    pub sigil: IdentSigil,
    pub scope: IdentScope,
}

/// Sigil preceding an identifier.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub enum IdentSigil {
    /// None (Whitelips, rdebath, and Respace).
    #[default]
    None,
    /// `.` (Lime and littleBugHunter).
    Dot,
    /// `_` (Burghard).
    Underscore,
    /// `@` (Palaiologos label definitions).
    At,
    /// `%` (Palaiologos label references).
    Percent,
}

/// Partition strategy for identifier resolution.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum IdentPartition {
    /// Variables are in one partition.
    Mixed,
    /// Variables of different types are in disjoint partitions and identifiers
    /// do not conflict between partitions (Burghard `valueinteger` and
    /// `valuestring`).
    Typed,
}

/// Scope for identifier resolution.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum IdentScope {
    /// Identifiers are resolved in the global scope.
    Global,
    /// Identifiers are resolved in a per-file scope.
    File(FileId),
}

/// Scope for identifiers introduced by `include`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IncludeScope {
    /// Identifiers are introduced into the global scope (Burghard).
    Global,
    /// Identifiers are introduced into the global scope with a prefix.
    GlobalPrefixed(String),
    /// Identifiers are not exported.
    Private,
}

/// Arithmetic binary operation.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Arith {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

/// Condition for a branch.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Cond {
    /// Jump if zero (`jz`).
    Zero,
    /// Jump if negative (`jn`).
    Neg,
    /// Jump if positive (Burghard `jumpp`).
    Pos,
    /// Jump if non-zero (Burghard `jumpnp` or `jumppn`).
    NonZero,
    /// Jump if non-negative, i.e., positive or zero (Burghard `jumppz`).
    NonNeg,
    /// Jump if non-positive, i.e., negative or zero (Burghard `jumpnz`).
    NonPos,
}

/// Kind of debugging instruction.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Dump {
    /// Dump the stack (Burghard `debug_printstack`).
    DebugPrintStack,
    /// Dump the heap (Burghard `debug_printheap`).
    DebugPrintHeap,
    /// Dump the program, labels, stack, heap, program counter, and call stack
    /// (pywhitespace `Trace`).
    Trace,
    /// Dump the program counter, stack, and heap (JBanana `x-dump`).
    XDump,
}

impl InstKind {
    #[inline]
    pub fn opcode(&self) -> Opcode {
        match self {
            InstKind::Push(_) => Opcode::Push,
            InstKind::PushString(_) => Opcode::PushString,
            InstKind::Dup => Opcode::Dup,
            InstKind::Copy(_) => Opcode::Copy,
            InstKind::Swap => Opcode::Swap,
            InstKind::Drop => Opcode::Drop,
            InstKind::Slide(_) => Opcode::Slide,
            InstKind::Arith { op, .. } => Opcode::Arith(*op),
            InstKind::Store { .. } => Opcode::Store,
            InstKind::Retrieve { .. } => Opcode::Retrieve,
            InstKind::Label(_) => Opcode::Label,
            InstKind::Call(_) => Opcode::Call,
            InstKind::Jmp(_) => Opcode::Jmp,
            InstKind::Branch(cond, _) => Opcode::Branch(*cond),
            InstKind::Ret => Opcode::Ret,
            InstKind::End => Opcode::End,
            InstKind::Printc { .. } => Opcode::Printc,
            InstKind::Printi { .. } => Opcode::Printi,
            InstKind::Readc { .. } => Opcode::Readc,
            InstKind::Readi { .. } => Opcode::Readi,
            InstKind::Dump(kind) => Opcode::Dump(*kind),
            InstKind::Test(_) => Opcode::Test,
            InstKind::Include { .. } => Opcode::Include,
            InstKind::DefineInteger(_, _) => Opcode::DefineInteger,
            InstKind::DefineString(_, _) => Opcode::DefineString,
            InstKind::DefineOption(_) => Opcode::DefineOption,
            InstKind::IfOption { .. } => Opcode::IfOption,
            InstKind::Error(_) => Opcode::Error,
        }
    }
}
