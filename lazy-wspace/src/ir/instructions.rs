use rug::Integer as Mpz;
use strum::Display;

use crate::error::{Error, NumberError};
use crate::hs;
use crate::ir::BBlockId;

/// IR instruction.
///
/// Compare to Cranelift [`cranelift_codegen::ir::instructions::InstructionData`](https://docs.rs/cranelift-codegen/latest/cranelift_codegen/ir/instructions/enum.InstructionData.html).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Inst {
    // Values
    //
    /// Unary operation with an `Mpz` immediate.
    UnaryImmZ { opcode: Opcode, imm: Mpz },
    /// Unary operation with an `i32` immediate.
    UnaryImmI { opcode: Opcode, imm: i32 },
    /// Unary operation with a `u32` immediate.
    UnaryImmU { opcode: Opcode, imm: u32 },
    /// Unary operation with a stack index immediate.
    UnaryImmIndex { opcode: Opcode, imm: hs::Int },
    /// Unary operation with an error immediate.
    UnaryImmError { opcode: Opcode, imm: NumberError },
    /// Unary operation.
    Unary { opcode: Opcode, arg: Value },
    /// Binary operation.
    Binary { opcode: Opcode, args: [Value; 2] },
    /// Stack reference.
    GuardedIndex {
        opcode: Opcode,
        index: hs::Int,
        // TODO: Should control dependencies be separate from Value?
        guard: Value,
    },
    /// Read from stdin.
    Read { opcode: Opcode, format: IoFormat },

    // Statements
    //
    /// Unary statement.
    UnaryStmt { opcode: Opcode, arg: Value },
    /// Unary statement with a stack index immediate.
    UnaryImmIndexStmt { opcode: Opcode, imm: hs::Int },
    /// Nullary statement.
    NullaryStmt { opcode: Opcode },
    /// Print to stdout.
    PrintStmt {
        opcode: Opcode,
        format: IoFormat,
        arg: Value,
    },

    // Control flow instructions
    //
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

/// TODO
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Value;

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
    /// Error thunk constant (UnaryImmError : NumberError).
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
    /// `%r = popcount %val`.
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
    /// Evaluate a value thunk and possibly trap (UnaryStmt : Value →).
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
    /// Push a value to the stack (UnaryStmt).
    /// `push %val`.
    Push,
    /// Drop `count` elements from the stack (UnaryImmIndexStmt).
    /// `drop $count`.
    Drop,
    /// Lazily drop `count` elements from the stack (UnaryImmIndexStmt).
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
    /// Print a value to stdout (PrintStmt).
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
    /// Return from a call (NullaryStmt).
    /// `ret`.
    Ret,
    /// Exit the program (NullaryStmt).
    /// `exit`.
    Exit,
    /// Exit the program with an error (Trap).
    /// `trap $error`.
    Trap,

    // Deprecated operations
    //
    /// Bitwise AND NOT, i.e., `lhs & ~rhs` (Binary : Mpz, Mpz → Mpz).
    /// `%r = andnot %lhs, %rhs`.
    DeprecatedAndNot,
    /// Bitwise NOT AND, i.e., `~lhs & rhs` (Binary : Mpz, Mpz → Mpz).
    /// `%r = notand %lhs, %rhs`.
    DeprecatedNotAnd,
    /// Bitwise NAND (Binary : Mpz, Mpz → Mpz).
    /// `%r = nand %lhs, %rhs`.
    DeprecatedNand,
    /// Bitwise NOR (Binary : Mpz, Mpz → Mpz).
    /// `%r = nor %lhs, %rhs`.
    DeprecatedNor,
    /// Bitwise XNOR (Binary : Mpz, Mpz → Mpz).
    /// `%r = xnor %lhs, %rhs`.
    DeprecatedXnor,
    /// Bitwise negated AND NOT (Binary : Mpz, Mpz → Mpz).
    /// `%r = nandnot %lhs, %rhs`.
    DeprecatedNandNot,
    /// Bitwise negated NOT AND (Binary : Mpz, Mpz → Mpz).
    /// `%r = nnotand %lhs, %rhs`.
    DeprecatedNNotAnd,
    /// Negated bit test (Binary : Mpz, u32 → u32).
    /// `%r = ntestbit %val, %bit`.
    DeprecatedNTestBit,
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
            | Opcode::DeprecatedAndNot
            | Opcode::DeprecatedNotAnd
            | Opcode::DeprecatedNand
            | Opcode::DeprecatedNor
            | Opcode::DeprecatedXnor
            | Opcode::DeprecatedNandNot
            | Opcode::DeprecatedNNotAnd
            | Opcode::DeprecatedNTestBit => true,
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
            | Inst::UnaryStmt { opcode, .. }
            | Inst::UnaryImmIndexStmt { opcode, .. }
            | Inst::NullaryStmt { opcode, .. }
            | Inst::PrintStmt { opcode, .. }
            | Inst::Jmp { opcode, .. }
            | Inst::Call { opcode, .. }
            | Inst::Br { opcode, .. }
            | Inst::Trap { opcode, .. } => *opcode,
        }
    }

    #[inline]
    pub fn is_value(&self) -> bool {
        self.opcode().is_value()
    }

    #[inline]
    pub fn constz(n: Mpz) -> Self {
        Inst::UnaryImmZ {
            opcode: Opcode::ConstZ,
            imm: n,
        }
    }
    #[inline]
    pub fn consti(n: i32) -> Self {
        Inst::UnaryImmI {
            opcode: Opcode::ConstI,
            imm: n,
        }
    }
    #[inline]
    pub fn constu(n: u32) -> Self {
        Inst::UnaryImmU {
            opcode: Opcode::ConstU,
            imm: n,
        }
    }
    #[inline]
    pub fn const_error(error: NumberError) -> Self {
        Inst::UnaryImmError {
            opcode: Opcode::ConstError,
            imm: error,
        }
    }
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
    unary!(popcount(Opcode::PopCount, val));
    unary!(lnot(Opcode::LNot, val));
    #[inline]
    pub fn eval(val: Value) -> Self {
        Inst::UnaryStmt {
            opcode: Opcode::Eval,
            arg: val,
        }
    }
    binary!(error_or(Opcode::ErrorOr, maybe_error, or_value));
    #[inline]
    pub fn stack_ref(index: hs::Int, guard: Value) -> Self {
        Inst::GuardedIndex {
            opcode: Opcode::StackRef,
            index,
            guard,
        }
    }
    #[inline]
    pub fn checked_stack_ref(index: hs::Int) -> Self {
        Inst::UnaryImmIndex {
            opcode: Opcode::CheckedStackRef,
            imm: index,
        }
    }
    #[inline]
    pub fn guard_stack(length: hs::Int) -> Self {
        Inst::UnaryImmIndex {
            opcode: Opcode::GuardStack,
            imm: length,
        }
    }
    #[inline]
    pub fn push(val: Value) -> Self {
        Inst::UnaryStmt {
            opcode: Opcode::Push,
            arg: val,
        }
    }
    #[inline]
    pub fn drop(count: hs::Int) -> Self {
        Inst::UnaryImmIndexStmt {
            opcode: Opcode::Drop,
            imm: count,
        }
    }
    #[inline]
    pub fn drop_lazy(count: hs::Int) -> Self {
        Inst::UnaryImmIndexStmt {
            opcode: Opcode::DropLazy,
            imm: count,
        }
    }
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
        Inst::PrintStmt {
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
        Inst::NullaryStmt {
            opcode: Opcode::Ret,
        }
    }
    #[inline]
    pub fn exit() -> Self {
        Inst::NullaryStmt {
            opcode: Opcode::Exit,
        }
    }
    #[inline]
    pub fn trap(error: Error) -> Self {
        Inst::Trap {
            opcode: Opcode::Trap,
            error: Box::new(error),
        }
    }
    binary!(deprecated_andnot(Opcode::DeprecatedAndNot, lhs, rhs));
    binary!(deprecated_notand(Opcode::DeprecatedNotAnd, lhs, rhs));
    binary!(deprecated_nand(Opcode::DeprecatedNand, lhs, rhs));
    binary!(deprecated_nor(Opcode::DeprecatedNor, lhs, rhs));
    binary!(deprecated_xnor(Opcode::DeprecatedXnor, lhs, rhs));
    binary!(deprecated_nandnot(Opcode::DeprecatedNandNot, lhs, rhs));
    binary!(deprecated_nnotand(Opcode::DeprecatedNNotAnd, lhs, rhs));
    binary!(deprecated_ntestbit(Opcode::DeprecatedNTestBit, lhs, rhs));
}
