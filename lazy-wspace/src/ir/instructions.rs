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
    /// `%r = const $n`.
    Const,
    /// 32-bit signed integer constant (UnaryImmI : i32).
    /// `%r = consti $n`.
    ConstI,
    /// 32-bit unsigned integer constant (UnaryImmU : u32).
    /// `%r = constu $n`.
    ConstU,
    /// Error thunk constant (UnaryImmError : NumberError).
    /// `%r = consterror $error`.
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
    /// `%r = testbit %val, %bit`.
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
    /// `%r = countlz %val`.
    ///
    /// **See also**:
    /// HotSpot [`CountLeadingZerosINode`, `CountLeadingZerosLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/countbitsnode.hpp).
    //
    //  TODO: How to define it for negative values? Go math/big does not provide
    //  this operation.
    CountLz,
    /// Count trailing zeros (Unary : Mpz → u32).
    /// `%r = counttz %val`.
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
