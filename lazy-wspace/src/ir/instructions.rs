use std::fmt::{self, Debug, Display, Formatter};
use std::ops::Deref;
use std::slice;

use rug::Integer as Mpz;
use strum::Display;

use crate::error::{Error, ValueError};
use crate::ir::{BBlockId, Cfg, NodeRef};

/// IR instruction.
///
/// Compare to Cranelift [`cranelift_codegen::ir::instructions::InstructionData`](https://docs.rs/cranelift-codegen/latest/cranelift_codegen/ir/instructions/enum.InstructionData.html).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Inst {
    /// Nullary operation.
    Nullary { opcode: Opcode },
    /// Unary operation with an `Mpz` immediate.
    UnaryImmZ {
        opcode: Opcode,
        // Boxed to keep the size smaller.
        imm: Box<Mpz>,
    },
    /// Unary operation with an `i32` immediate.
    UnaryImmI { opcode: Opcode, imm: i32 },
    /// Unary operation with a `u32` immediate.
    UnaryImmU { opcode: Opcode, imm: u32 },
    /// Unary operation with a stack index immediate.
    //  TODO: Replace usize with hs::Int.
    UnaryImmIndex { opcode: Opcode, imm: usize },
    /// Unary operation with an error immediate.
    UnaryImmError { opcode: Opcode, imm: ValueError },
    /// Unary operation.
    Unary { opcode: Opcode, arg: Value },
    /// Binary operation.
    ///
    /// The RHS is evaluated first, then the LHS.
    Binary { opcode: Opcode, args: [Value; 2] },
    /// Stack reference.
    GuardedIndex {
        opcode: Opcode,
        // TODO: Replace with hs::Int
        index: usize,
        guard: NodeRef,
    },
    /// Read from stdin.
    Read { opcode: Opcode, format: IoFormat },
    /// Print to stdout.
    Print {
        opcode: Opcode,
        format: IoFormat,
        arg: Value,
    },
    /// Unconditional jump.
    Jmp { opcode: Opcode, target: BBlockId },
    /// Call.
    Call {
        opcode: Opcode,
        target: BBlockId,
        next: BBlockId,
    },
    /// Conditional branch.
    Br {
        opcode: Opcode,
        cond: Cond,
        arg: Value,
        if_true: BBlockId,
        if_false: BBlockId,
    },
    /// Trap.
    Trap {
        opcode: Opcode,
        // Boxed to keep the size smaller.
        error: Box<Error>,
    },
}

/// Reference to a value-producing node.
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Value {
    node: NodeRef,
}

/// Format for I/O.
#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
#[strum(serialize_all = "snake_case")]
pub enum IoFormat {
    Char,
    Int,
}

/// Condition for a branch.
#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
#[strum(serialize_all = "snake_case")]
pub enum Cond {
    Zero,
    Neg,
}

/// IR instruction opcode.
///
/// Compare to Cranelift [`cranelift_codegen::ir::instructions::Opcode`](https://docs.rs/cranelift-codegen/latest/cranelift_codegen/ir/instructions/enum.Opcode.html).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Opcode {
    // Constants
    //
    /// Arbitrary-precision integer constant (UnaryImmZ : Mpz).
    /// `%r = constz $n`.
    ConstZ,
    /// 32-bit signed integer constant (UnaryImmI : i32).
    /// `%r = consti $n`.
    ConstI,
    /// 32-bit unsigned integer constant (UnaryImmU : u32).
    /// `%r = constu $n`.
    ConstU,
    /// Error thunk constant (UnaryImmError : ValueError).
    /// `%r = const_error $error`.
    ConstError,

    // Arithmetic operations
    //
    /// Addition (Binary : Mpz, Mpz → Mpz).
    /// `%r = add %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_add`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fadd).
    ///
    /// **See also**:
    /// Graal [`AddNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/AddNode.java);
    /// HotSpot [`AddNode`, `AddINode`, `AddLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/addnode.hpp).
    Add,
    /// Addition (Binary : Mpz, u32 → Mpz).
    /// `%r = addzu %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_add_ui`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fadd_005fui).
    AddZU,
    /// Subtraction (Binary : Mpz, Mpz → Mpz).
    /// `%r = sub %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_sub`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fsub).
    ///
    /// **See also**:
    /// Graal [`SubNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/SubNode.java);
    /// HotSpot [`SubINode`, `SubLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/subnode.hpp).
    Sub,
    /// Subtraction (Binary : Mpz, i32 → Mpz).
    /// `%r = subzi %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_sub_ui`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fsub_005fui).
    SubZI,
    /// Subtraction (Binary : i32, Mpz → Mpz).
    /// `%r = subiz %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_ui_sub`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fui_005fsub).
    SubIZ,
    /// Multiplication (Binary : Mpz, Mpz → Mpz).
    /// `%r = mul %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_mul`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fmul).
    ///
    /// **See also**:
    /// Graal [`MulNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/MulNode.java);
    /// HotSpot [`MulINode`, `MulLNode`, `MulHiLNode`, `UMulHiLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/mulnode.hpp).
    Mul,
    /// Multiplication (Binary : Mpz, Mpz → Mpz).
    /// `%r = mulzi %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_mul_si`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fmul_005fsi).
    MulZI,
    /// Multiplication (Binary : Mpz, u32 → Mpz).
    /// `%r = mulzu %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_mul_ui`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fmul_005fui).
    MulZU,
    /// Division (Binary : Mpz, Mpz → Mpz).
    /// `%r = div %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_fdiv_q`](https://gmplib.org/manual/Integer-Division#index-mpz_005ffdiv_005fq).
    ///
    /// **See also**:
    /// Graal [`IntegerDivRemNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/IntegerDivRemNode.java),
    ///   [`SignedDivNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/SignedDivNode.java),
    ///   [`UnsignedDivNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/UnsignedDivNode.java),
    ///   [`FloatingIntegerDivRemNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/FloatingIntegerDivRemNode.java),
    ///   [`SignedFloatingIntegerDivNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/SignedFloatingIntegerDivNode.java);
    /// HotSpot [`DivINode`, `DivLNode`, `UDivINode`, `UDivLNode`,
    ///   `DivModINode`, `DivModLNode`, `UDivModINode`, `UDivModLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/divnode.hpp).
    Div,
    /// Division (Binary : Mpz, u32 → Mpz).
    /// `%r = divzu %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_fdiv_q_ui`](https://gmplib.org/manual/Integer-Division#index-mpz_005ffdiv_005fq_005fui).
    DivZU,
    /// Modulo (Binary : Mpz, Mpz → Mpz).
    /// `%r = mod %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_fdiv_r`](https://gmplib.org/manual/Integer-Division#index-mpz_005ffdiv_005fr).
    ///
    /// **See also**:
    /// Graal [`RemNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/RemNode.java),
    ///   [`IntegerDivRemNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/IntegerDivRemNode.java),
    ///   [`SignedRemNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/SignedRemNode.java),
    ///   [`UnsignedRemNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/UnsignedRemNode.java),
    ///   [`FloatingIntegerDivRemNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/FloatingIntegerDivRemNode.java),
    ///   [`SignedFloatingIntegerRemNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/SignedFloatingIntegerRemNode.java);
    /// HotSpot [`ModINode`, `ModLNode`, `UModINode`, `UModLNode`,
    ///   `DivModINode`, `DivModLNode`, `UDivModINode`, `UDivModLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/divnode.hpp).
    Mod,
    /// Modulo (Binary : Mpz, u32 → u32).
    /// `%r = modzu %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_fdiv_ui`](https://gmplib.org/manual/Integer-Division#index-mpz_005ffdiv_005fui).
    //  TODO: Alternatively lower to [`mpz_fdiv_r_ui`](https://gmplib.org/manual/Integer-Division#index-mpz_005ffdiv_005fr_005fui),
    //  when an Mpz output is desired.
    ModZU,
    /// Negation (Unary : Mpz → Mpz).
    /// `%r = neg %val`.
    ///
    /// Lowers to GMP [`mpz_neg`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fneg).
    ///
    /// **See also**:
    /// Graal [`NegateNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/NegateNode.java);
    /// HotSpot [`NegINode`, `NegLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/subnode.hpp).
    Neg,
    /// Absolute value (Unary : Mpz → Mpz).
    /// `%r = abs %val`.
    ///
    /// Lowers to GMP [`mpz_abs`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fabs).
    ///
    /// **See also**:
    /// Graal [`AbsNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/AbsNode.java);
    /// HotSpot [`AbsINode`, `AbsLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/subnode.hpp).
    Abs,

    // Bitwise operations
    //
    /// Bitwise AND (Binary : Mpz, Mpz → Mpz).
    /// `%r = and %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_and`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005fand).
    ///
    /// **See also**:
    /// Graal [`AndNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/AndNode.java);
    /// HotSpot [`AndINode`, `AndLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/mulnode.hpp).
    And,
    /// Bitwise OR (Binary : Mpz, Mpz → Mpz).
    /// `%r = or %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_ior`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005fior).
    ///
    /// **See also**:
    /// Graal [`OrNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/OrNode.java);
    /// HotSpot [`OrINode`, `OrLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/addnode.hpp).
    Or,
    /// Bitwise XOR (Binary : Mpz, Mpz → Mpz).
    /// `%r = xor %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_xor`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005fxor).
    ///
    /// **See also**:
    /// Graal [`XorNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/XorNode.java);
    /// HotSpot [`XorINode`, `XOrLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/addnode.hpp).
    Xor,
    /// Left shift (Binary : Mpz, u32 → Mpz).
    /// `%r = shl %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_mul_2exp`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fmul_005f2exp).
    ///
    /// **See also**:
    /// Graal [`LeftShiftNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/LeftShiftNode.java);
    /// HotSpot [`LShiftINode`, LShiftLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/mulnode.hpp).
    Shl,
    /// Arithmetic right shift (Binary : Mpz, u32 → Mpz).
    /// `%r = ashr %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_fdiv_q_2exp`](https://gmplib.org/manual/Integer-Division#index-mpz_005ffdiv_005fq_005f2exp).
    //  TODO: Verify that this is the correct function.
    ///
    /// **See also**:
    /// Graal [`RightShiftNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/RightShiftNode.java);
    /// HotSpot [`RShiftINode`, `RShiftLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/mulnode.hpp).
    AShr,
    /// Logical right shift (Binary : Mpz, u32 → Mpz).
    /// `%r = lshr %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_tdiv_q_2exp`](https://gmplib.org/manual/Integer-Division#index-mpz_005ftdiv_005fq_005f2exp).
    //  TODO: Verify that this is the correct function.
    ///
    /// **See also**:
    /// Graal [`UnsignedRightShiftNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/UnsignedRightShiftNode.java);
    /// HotSpot [`URShiftINode`, `URShiftINode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/mulnode.hpp).
    LShr,
    /// Bitwise complement (Unary : Mpz → Mpz).
    /// `%r = not %val`.
    ///
    /// Lowers to GMP [`mpz_com`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005fcom).
    Not,
    /// Test bit at index (Binary : Mpz, u32 → u32).
    /// `%r = test_bit %val, %bit`.
    ///
    /// Lowers to GMP [`mpz_tstbit`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005ftstbit).
    TestBit,
    /// Length of the absolute value in bits (Unary : Mpz → u32).
    /// `%r = count_bits %val`.
    ///
    /// Lowers to GMP [`mpz_sizeinbase(arg, 2) - 1`](https://gmplib.org/manual/Miscellaneous-Integer-Functions#index-mpz_005fsizeinbase).
    /// This matches Go [`big.Int.BitLen`](https://pkg.go.dev/math/big#Int.BitLen).
    CountBits,
    /// Count leading zeros (Unary : Mpz → u32).
    /// `%r = count_lz %val`.
    ///
    /// **See also**:
    /// HotSpot [`CountLeadingZerosINode`, `CountLeadingZerosLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/countbitsnode.hpp).
    //
    //  TODO: How to define it for negative values? Go math/big does not provide
    //  this operation.
    CountLz,
    /// Count trailing zeros (Unary : Mpz → u32).
    /// `%r = count_tz %val`.
    ///
    /// Lowers to GMP [`mpz_scan1(arg, 0)`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005fscan1).
    ///
    /// **See also**:
    /// HotSpot [`CountTrailingZerosINode`, `CountTrailingZerosLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/countbitsnode.hpp).
    CountTz,
    /// Population count (Unary : Mpz → u32).
    /// `%r = pop_count %val`.
    ///
    /// Lowers to GMP [`mpz_popcount`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005fpopcount),
    /// but negative values are first complemented, so the input is always
    /// non-negative. This matches Java [`BigInteger.bitCount`](https://docs.oracle.com/en/java/javase/20/docs/api/java.base/java/math/BigInteger.html#bitCount())
    /// and Common Lisp [`logcount`](http://www.lispworks.com/documentation/HyperSpec/Body/f_logcou.htm).
    /// Haskell [`popCount`](https://hackage.haskell.org/package/base/docs/Data-Bits.html#v:popCount)
    /// is `-popCount(-n)` for negative `Integer` values.
    ///
    /// **See also**:
    /// HotSpot [`PopCountINode`, `PopCountLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/countbitsnode.hpp).
    PopCount,

    // Logical operations
    //
    /// Logical not (Unary : Mpz → Mpz).
    /// `%r = lnot %val`.
    //
    //  TODO: Should operate on `u1` instead of `Mpz`.
    //  TODO: See also.
    LNot,

    // Error propagation
    //
    /// Evaluate a value thunk and possibly trap (Unary : Value →).
    /// `eval %val`.
    Eval,
    /// Select the LHS, if it's an error, otherwise the RHS
    /// (Binary : Value, Value → Value).
    /// `%r = error_or %maybe_error %or_value`.
    ErrorOr,

    // Stack operations
    //
    /// Unchecked stack reference, that must be first guarded with `guard_stack`
    /// (GuardedIndex).
    /// `%r = stack_ref $index %guard`.
    StackRef,
    /// Checked stack reference (UnaryImmIndex).
    /// `%r = checked_stack_ref $index`.
    CheckedStackRef,
    /// Assert that the stack has at least `length` elements (UnaryImmIndex).
    /// `%guard = guard_stack $length`.
    GuardStack,
    /// Push a value to the stack (Unary).
    /// `push %val`.
    Push,
    /// Drop `count` elements from the stack (UnaryImmIndex).
    /// `drop $count`.
    Drop,
    /// Lazily drop `count` elements from the stack (UnaryImmIndex).
    /// `drop_lazy $count`.
    DropLazy,

    // Heap operations
    //
    /// Heap reference (Unary).
    /// `%r = heap_ref %address`.
    //
    //  TODO: Guard heap size.
    HeapRef,
    /// Store a value at a heap address (Binary).
    /// `store %address, %val`.
    Store,

    // I/O instructions
    //
    /// Read a value from stdin (Read).
    /// `%r = read $format`.
    Read,
    /// Print a value to stdout (Print).
    /// `print $format, %val`.
    Print,

    // Control flow instructions
    //
    /// Call (Call).
    /// `call @target, @next`.
    Call,
    /// Unconditional jump (Jmp).
    /// `jmp @target`.
    Jmp,
    /// Conditional branch (Br).
    /// `br $cond, %val, @if_true, @if_false`.
    Br,
    /// Return from a call (Nullary).
    /// `ret`.
    Ret,
    /// Exit the program (Nullary).
    /// `exit`.
    Exit,
    /// Exit the program with an error (Trap).
    /// `trap $error`.
    Trap,

    // Deprecated operations
    //
    /// Bitwise AND NOT, i.e., `lhs & ~rhs` (Binary : Mpz, Mpz → Mpz).
    /// `%r = andnot %lhs, %rhs`.
    AndNot,
    /// Bitwise NOT AND, i.e., `~lhs & rhs` (Binary : Mpz, Mpz → Mpz).
    /// `%r = notand %lhs, %rhs`.
    NotAnd,
    /// Bitwise NAND (Binary : Mpz, Mpz → Mpz).
    /// `%r = nand %lhs, %rhs`.
    Nand,
    /// Bitwise NOR (Binary : Mpz, Mpz → Mpz).
    /// `%r = nor %lhs, %rhs`.
    Nor,
    /// Bitwise XNOR (Binary : Mpz, Mpz → Mpz).
    /// `%r = xnor %lhs, %rhs`.
    Xnor,
    /// Bitwise negated AND NOT (Binary : Mpz, Mpz → Mpz).
    /// `%r = nandnot %lhs, %rhs`.
    NandNot,
    /// Bitwise negated NOT AND (Binary : Mpz, Mpz → Mpz).
    /// `%r = nnotand %lhs, %rhs`.
    NNotAnd,
    /// Negated bit test (Binary : Mpz, u32 → u32).
    /// `%r = ntestbit %val, %bit`.
    NTestBit,
}

// TODO GMP functions:
// - [`mpz_addmul`{,_ui}](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005faddmul)
// - [`mpz_submul`{,_ui}](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fsubmul)
// - [`mpz_fdiv_qr`{,_ui}](https://gmplib.org/manual/Integer-Division#index-mpz_005ffdiv_005fqr)
// - [`mpz_fdiv_r_2exp`](https://gmplib.org/manual/Integer-Division#index-mpz_005ffdiv_005fr_005f2exp)
// - [`mpz_tdiv_r_2exp`](https://gmplib.org/manual/Integer-Division#index-mpz_005ftdiv_005fr_005f2exp)
// - [`mpz_setbit`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005fsetbit)
// - [`mpz_clrbit`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005fclrbit)
// - [`mpz_combit`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005fcombit)

impl Opcode {
    pub fn mnemonic(&self) -> &'static str {
        match self {
            Opcode::ConstZ => "constz",
            Opcode::ConstI => "consti",
            Opcode::ConstU => "constu",
            Opcode::ConstError => "const_error",
            Opcode::Add => "add",
            Opcode::AddZU => "addzu",
            Opcode::Sub => "sub",
            Opcode::SubZI => "subzi",
            Opcode::SubIZ => "subiz",
            Opcode::Mul => "mul",
            Opcode::MulZI => "mulzi",
            Opcode::MulZU => "mulzu",
            Opcode::Div => "div",
            Opcode::DivZU => "divzu",
            Opcode::Mod => "mod",
            Opcode::ModZU => "modzu",
            Opcode::Neg => "neg",
            Opcode::Abs => "abs",
            Opcode::And => "and",
            Opcode::Or => "or",
            Opcode::Xor => "xor",
            Opcode::Shl => "shl",
            Opcode::AShr => "ashr",
            Opcode::LShr => "lshr",
            Opcode::Not => "not",
            Opcode::TestBit => "test_bit",
            Opcode::CountBits => "count_bits",
            Opcode::CountLz => "count_lz",
            Opcode::CountTz => "count_tz",
            Opcode::PopCount => "pop_count",
            Opcode::LNot => "lnot",
            Opcode::Eval => "eval",
            Opcode::ErrorOr => "error_or",
            Opcode::StackRef => "stack_ref",
            Opcode::CheckedStackRef => "checked_stack_ref",
            Opcode::GuardStack => "guard_stack",
            Opcode::Push => "push",
            Opcode::Drop => "drop",
            Opcode::DropLazy => "drop_lazy",
            Opcode::HeapRef => "heap_ref",
            Opcode::Store => "store",
            Opcode::Read => "read",
            Opcode::Print => "print",
            Opcode::Call => "call",
            Opcode::Jmp => "jmp",
            Opcode::Br => "br",
            Opcode::Ret => "ret",
            Opcode::Exit => "exit",
            Opcode::Trap => "trap",
            Opcode::AndNot => "andnot",
            Opcode::NotAnd => "notand",
            Opcode::Nand => "nand",
            Opcode::Nor => "nor",
            Opcode::Xnor => "xnor",
            Opcode::NandNot => "nandnot",
            Opcode::NNotAnd => "nnotand",
            Opcode::NTestBit => "ntestbit",
        }
    }

    pub fn is_value(&self) -> bool {
        match self {
            Opcode::ConstZ
            | Opcode::ConstI
            | Opcode::ConstU
            | Opcode::ConstError
            | Opcode::Add
            | Opcode::AddZU
            | Opcode::Sub
            | Opcode::SubZI
            | Opcode::SubIZ
            | Opcode::Mul
            | Opcode::MulZI
            | Opcode::MulZU
            | Opcode::Div
            | Opcode::DivZU
            | Opcode::Mod
            | Opcode::ModZU
            | Opcode::Neg
            | Opcode::Abs
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
            | Opcode::Shl
            | Opcode::AShr
            | Opcode::LShr
            | Opcode::Not
            | Opcode::TestBit
            | Opcode::CountBits
            | Opcode::CountLz
            | Opcode::CountTz
            | Opcode::PopCount
            | Opcode::LNot
            | Opcode::ErrorOr
            | Opcode::StackRef
            | Opcode::CheckedStackRef
            | Opcode::HeapRef
            | Opcode::Read
            | Opcode::AndNot
            | Opcode::NotAnd
            | Opcode::Nand
            | Opcode::Nor
            | Opcode::Xnor
            | Opcode::NandNot
            | Opcode::NNotAnd
            | Opcode::NTestBit => true,
            Opcode::Eval // TODO: make eval a value
            | Opcode::GuardStack
            | Opcode::Push
            | Opcode::Drop
            | Opcode::DropLazy
            | Opcode::Store
            | Opcode::Print
            | Opcode::Call
            | Opcode::Jmp
            | Opcode::Br
            | Opcode::Ret
            | Opcode::Exit
            | Opcode::Trap => false,
        }
    }
}

macro_rules! unary(($func:ident ($opcode:expr, $val:ident)) => {
    #[inline]
    pub fn $func($val: Value) -> Self {
        Inst::Unary {
            opcode: $opcode,
            arg: $val,
        }
    }
});
macro_rules! unary_imm(($func:ident ($opcode:expr, Inst::$unary:ident, $imm:ident: $imm_ty:ty)) => {
    #[inline]
    pub fn $func($imm: $imm_ty) -> Self {
        Inst::$unary {
            opcode: $opcode,
            imm: $imm,
        }
    }
});
macro_rules! binary(($func:ident ($opcode:expr, $lhs:ident, $rhs:ident)) => {
    #[inline]
    pub fn $func($lhs: Value, $rhs: Value) -> Self {
        Inst::Binary {
            opcode: $opcode,
            args: [$lhs, $rhs],
        }
    }
});

impl Inst {
    #[inline]
    pub fn opcode(&self) -> Opcode {
        match self {
            Inst::UnaryImmZ { opcode, .. }
            | Inst::UnaryImmI { opcode, .. }
            | Inst::UnaryImmU { opcode, .. }
            | Inst::UnaryImmIndex { opcode, .. }
            | Inst::UnaryImmError { opcode, .. }
            | Inst::Unary { opcode, .. }
            | Inst::Binary { opcode, .. }
            | Inst::GuardedIndex { opcode, .. }
            | Inst::Read { opcode, .. }
            | Inst::Nullary { opcode, .. }
            | Inst::Print { opcode, .. }
            | Inst::Jmp { opcode, .. }
            | Inst::Call { opcode, .. }
            | Inst::Br { opcode, .. }
            | Inst::Trap { opcode, .. } => *opcode,
        }
    }

    pub fn args(&self) -> Option<&[Value]> {
        match self {
            Inst::Unary { arg, .. } | Inst::Print { arg, .. } | Inst::Br { arg, .. } => {
                Some(slice::from_ref(arg))
            }
            Inst::Binary { args, .. } => Some(args),
            Inst::Nullary { .. }
            | Inst::UnaryImmZ { .. }
            | Inst::UnaryImmI { .. }
            | Inst::UnaryImmU { .. }
            | Inst::UnaryImmIndex { .. }
            | Inst::UnaryImmError { .. }
            | Inst::GuardedIndex { .. }
            | Inst::Read { .. }
            | Inst::Jmp { .. }
            | Inst::Call { .. }
            | Inst::Trap { .. } => None,
        }
    }

    pub fn uses(&self) -> &[NodeRef] {
        match self {
            Inst::GuardedIndex { guard, .. } => slice::from_ref(guard),
            _ => {
                let uses = self.args().unwrap_or_default();
                // SAFETY: Value is repr(transparent) over NodeRef.
                unsafe { &*(uses as *const [Value] as *const [NodeRef]) }
            }
        }
    }

    #[inline]
    pub fn is_value(&self) -> bool {
        self.opcode().is_value()
    }

    #[inline]
    pub fn as_display<'s, 'a>(&'s self, cfg: &'a Cfg<'_>) -> impl Display + 's
    where
        'a: 's,
    {
        InstDisplay { inst: self, cfg }
    }

    #[inline]
    pub fn constz<T: Into<Mpz>>(n: T) -> Self {
        Inst::UnaryImmZ {
            opcode: Opcode::ConstZ,
            imm: Box::new(n.into()),
        }
    }
    unary_imm!(consti(Opcode::ConstI, Inst::UnaryImmI, n: i32));
    unary_imm!(constu(Opcode::ConstU, Inst::UnaryImmU, n: u32));
    unary_imm!(const_error(Opcode::ConstError, Inst::UnaryImmError, error: ValueError));
    binary!(add(Opcode::Add, lhs, rhs));
    binary!(addzu(Opcode::AddZU, lhs, rhs));
    binary!(sub(Opcode::Sub, lhs, rhs));
    binary!(subzi(Opcode::SubZI, lhs, rhs));
    binary!(subiz(Opcode::SubIZ, lhs, rhs));
    binary!(mul(Opcode::Mul, lhs, rhs));
    binary!(mulzi(Opcode::MulZI, lhs, rhs));
    binary!(mulzu(Opcode::MulZU, lhs, rhs));
    binary!(div(Opcode::Div, lhs, rhs));
    binary!(divzu(Opcode::DivZU, lhs, rhs));
    binary!(mod_(Opcode::Mod, lhs, rhs));
    binary!(modzu(Opcode::ModZU, lhs, rhs));
    unary!(neg(Opcode::Neg, val));
    unary!(abs(Opcode::Abs, val));
    binary!(and(Opcode::And, lhs, rhs));
    binary!(or(Opcode::Or, lhs, rhs));
    binary!(xor(Opcode::Xor, lhs, rhs));
    binary!(shl(Opcode::Shl, lhs, rhs));
    binary!(ashr(Opcode::AShr, lhs, rhs));
    binary!(lshr(Opcode::LShr, lhs, rhs));
    unary!(not(Opcode::Not, val));
    binary!(test_bit(Opcode::TestBit, lhs, rhs));
    unary!(count_bits(Opcode::CountBits, val));
    unary!(count_lz(Opcode::CountLz, val));
    unary!(count_tz(Opcode::CountTz, val));
    unary!(pop_count(Opcode::PopCount, val));
    unary!(lnot(Opcode::LNot, val));
    unary!(eval(Opcode::Eval, val));
    binary!(error_or(Opcode::ErrorOr, maybe_error, or_value));
    #[inline]
    pub fn stack_ref(index: usize, guard: NodeRef) -> Self {
        Inst::GuardedIndex {
            opcode: Opcode::StackRef,
            index,
            guard,
        }
    }
    unary_imm!(checked_stack_ref(Opcode::CheckedStackRef, Inst::UnaryImmIndex, index: usize));
    unary_imm!(guard_stack(Opcode::GuardStack, Inst::UnaryImmIndex, length: usize));
    unary!(push(Opcode::Push, val));
    unary_imm!(drop(Opcode::Drop, Inst::UnaryImmIndex, count: usize));
    unary_imm!(drop_lazy(Opcode::DropLazy, Inst::UnaryImmIndex, count: usize));
    unary!(heap_ref(Opcode::HeapRef, address));
    binary!(store(Opcode::Store, address, val));
    #[inline]
    pub fn read(format: IoFormat) -> Self {
        Inst::Read {
            opcode: Opcode::Read,
            format,
        }
    }
    #[inline]
    pub fn print(format: IoFormat, val: Value) -> Self {
        Inst::Print {
            opcode: Opcode::Print,
            format,
            arg: val,
        }
    }
    #[inline]
    pub fn call(target: BBlockId, next: BBlockId) -> Self {
        Inst::Call {
            opcode: Opcode::Call,
            target,
            next,
        }
    }
    #[inline]
    pub fn jmp(target: BBlockId) -> Self {
        Inst::Jmp {
            opcode: Opcode::Jmp,
            target,
        }
    }
    #[inline]
    pub fn br(cond: Cond, val: Value, if_true: BBlockId, if_false: BBlockId) -> Self {
        Inst::Br {
            opcode: Opcode::Br,
            cond,
            arg: val,
            if_true,
            if_false,
        }
    }
    #[inline]
    pub fn ret() -> Self {
        Inst::Nullary {
            opcode: Opcode::Ret,
        }
    }
    #[inline]
    pub fn exit() -> Self {
        Inst::Nullary {
            opcode: Opcode::Exit,
        }
    }
    #[inline]
    pub fn trap<T: Into<Error>>(error: T) -> Self {
        Inst::Trap {
            opcode: Opcode::Trap,
            error: Box::new(error.into()),
        }
    }
    binary!(andnot(Opcode::AndNot, lhs, rhs));
    binary!(notand(Opcode::NotAnd, lhs, rhs));
    binary!(nand(Opcode::Nand, lhs, rhs));
    binary!(nor(Opcode::Nor, lhs, rhs));
    binary!(xnor(Opcode::Xnor, lhs, rhs));
    binary!(nandnot(Opcode::NandNot, lhs, rhs));
    binary!(nnotand(Opcode::NNotAnd, lhs, rhs));
    binary!(ntest_bit(Opcode::NTestBit, lhs, rhs));
}

impl Value {
    // TODO: Reduce usage of this.
    pub(crate) fn new(node: NodeRef) -> Self {
        Value { node }
    }

    #[inline]
    pub fn node(&self) -> NodeRef {
        self.node
    }
}

impl Deref for Value {
    type Target = NodeRef;

    fn deref(&self) -> &NodeRef {
        &self.node
    }
}

impl PartialEq<Mpz> for Inst {
    #[inline]
    fn eq(&self, other: &Mpz) -> bool {
        if let Inst::UnaryImmZ {
            opcode: Opcode::ConstZ,
            imm: n,
        } = self
        {
            &**n == other
        } else {
            false
        }
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.mnemonic())
    }
}

struct InstDisplay<'a> {
    inst: &'a Inst,
    cfg: &'a Cfg<'a>,
}

impl Display for InstDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inst.opcode())?;
        if !matches!(self.inst, Inst::Nullary { .. }) {
            f.write_str(" ")?;
        }
        match *self.inst {
            Inst::Nullary { opcode: _ } => {}
            Inst::UnaryImmZ { opcode: _, ref imm } => write!(f, "{imm}")?,
            Inst::UnaryImmI { opcode: _, imm } => write!(f, "{imm}")?,
            Inst::UnaryImmU { opcode: _, imm } => write!(f, "{imm}")?,
            Inst::UnaryImmIndex { opcode: _, imm } => write!(f, "{imm}")?,
            Inst::UnaryImmError { opcode: _, imm } => write!(f, "{imm:?}")?,
            Inst::Unary { opcode: _, arg } => write!(f, "{arg}")?,
            Inst::Binary {
                opcode: _,
                args: [lhs, rhs],
            } => write!(f, "{lhs}, {rhs}")?,
            Inst::GuardedIndex {
                opcode: _,
                index,
                guard,
            } => write!(f, "{index}, {guard}")?,
            Inst::Read { opcode: _, format } => write!(f, "{format}")?,
            Inst::Print {
                opcode: _,
                format,
                arg,
            } => write!(f, "{format}, {arg}")?,
            Inst::Jmp { opcode: _, target } => write!(f, "{}", self.cfg[target])?,
            Inst::Call {
                opcode: _,
                target,
                next,
            } => write!(f, "{}, {}", self.cfg[target], self.cfg[next])?,
            Inst::Br {
                opcode: _,
                cond,
                arg,
                if_true,
                if_false,
            } => write!(
                f,
                "{cond}, {arg}, {}, {}",
                self.cfg[if_true], self.cfg[if_false],
            )?,
            Inst::Trap {
                opcode: _,
                ref error,
            } => write!(f, "{error:?}")?,
        }
        Ok(())
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.node, f)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.node, f)
    }
}
