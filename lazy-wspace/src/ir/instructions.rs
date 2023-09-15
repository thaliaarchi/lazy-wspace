/// IR instruction.
///
/// Compare to Cranelift [`cranelift_codegen::ir::instructions::InstructionData`](https://docs.rs/cranelift-codegen/latest/cranelift_codegen/ir/instructions/enum.InstructionData.html).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Inst {
    /// Binary operation with arbitrary-precision operands.
    Binary { opcode: Opcode, args: [Value; 2] },
    /// Binary operation with an arbitrary-precision LHS and `i32` RHS.
    BinaryZI {
        opcode: Opcode,
        lhs: Value,
        rhs: i32,
    },
    /// Binary operation with an `i32` LHS and arbitrary-precision RHS.
    BinaryIZ {
        opcode: Opcode,
        lhs: i32,
        rhs: Value,
    },
    /// Binary operation with an arbitrary-precision LHS and `u32` RHS.
    BinaryZU {
        opcode: Opcode,
        lhs: Value,
        rhs: u32,
    },
    /// Binary operation with an `u32` LHS and arbitrary-precision RHS.
    BinaryUZ {
        opcode: Opcode,
        lhs: u32,
        rhs: Value,
    },
    /// Unary operation with arbitrary-precision operand.
    Unary { opcode: Opcode, arg: Value },
}

/// TODO
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Value;

/// IR instruction opcode.
///
/// Compare to Cranelift [`cranelift_codegen::ir::instructions::Opcode`](https://docs.rs/cranelift-codegen/latest/cranelift_codegen/ir/instructions/enum.Opcode.html).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Opcode {
    // Arithmetic operations
    //
    /// Addition (*Binary*).
    /// `%r = add %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_add`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fadd).
    ///
    /// **See also**:
    /// Graal [`AddNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/AddNode.java);
    /// HotSpot [`AddNode`, `AddINode`, `AddLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/addnode.hpp).
    Add,
    /// Addition (*BinaryZU*).
    /// `%r = addzu %lhs, $rhs`.
    ///
    /// Lowers to GMP [`mpz_add_ui`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fadd_005fui).
    AddZU,
    /// Subtraction (*Binary*).
    /// `%r = sub %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_sub`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fsub).
    ///
    /// **See also**:
    /// Graal [`SubNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/SubNode.java);
    /// HotSpot [`SubINode`, `SubLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/subnode.hpp).
    Sub,
    /// Subtraction (*BinaryZI*).
    /// `%r = subzi %lhs, $rhs`.
    ///
    /// Lowers to GMP [`mpz_sub_ui`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fsub_005fui).
    SubZI,
    /// Subtraction (*BinaryIZ*).
    /// `%r = subiz $lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_ui_sub`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fui_005fsub).
    SubIZ,
    /// Multiplication (*Binary*).
    /// `%r = mul %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_mul`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fmul).
    ///
    /// **See also**:
    /// Graal [`MulNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/MulNode.java);
    /// HotSpot [`MulINode`, `MulLNode`, `MulHiLNode`, `UMulHiLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/mulnode.hpp).
    Mul,
    /// Multiplication (*BinaryZI*).
    /// `%r = mulzi %lhs, $rhs`.
    ///
    /// Lowers to GMP [`mpz_mul_si`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fmul_005fsi).
    MulZI,
    /// Multiplication (*BinaryZU*).
    /// `%r = mulzu %lhs, $rhs`.
    ///
    /// Lowers to GMP [`mpz_mul_ui`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fmul_005fui).
    MulZU,
    /// Division (*Binary*).
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
    /// Division (*BinaryZU*).
    /// `%r = divzu %lhs, $rhs`.
    ///
    /// Lowers to GMP [`mpz_fdiv_q_ui`](https://gmplib.org/manual/Integer-Division#index-mpz_005ffdiv_005fq_005fui).
    DivZU,
    /// Modulo (*Binary*).
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
    /// Modulo (*BinaryZU*).
    /// `%r = modzu %lhs, $rhs`.
    ///
    /// Lowers to GMP [`mpz_fdiv_r_ui`](https://gmplib.org/manual/Integer-Division#index-mpz_005ffdiv_005fr_005fui).
    //  TODO: Lower to [`mpz_fdiv_ui`](https://gmplib.org/manual/Integer-Division#index-mpz_005ffdiv_005fui),
    //  once `u32` values are first-class.
    ModZU,
    /// Negation (*Unary*).
    /// `%r = neg %arg`.
    ///
    /// Lowers to GMP [`mpz_neg`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fneg).
    ///
    /// **See also**:
    /// Graal [`NegateNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/NegateNode.java);
    /// HotSpot [`NegINode`, `NegLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/subnode.hpp).
    Neg,
    /// Absolute value (*Unary*).
    /// `%r = abs %arg`.
    ///
    /// Lowers to GMP [`mpz_abs`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fabs).
    ///
    /// **See also**:
    /// Graal [`AbsNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/AbsNode.java);
    /// HotSpot [`AbsINode`, `AbsLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/subnode.hpp).
    Abs,

    // Bitwise operations
    //
    /// Bitwise AND (*Binary*).
    /// `%r = and %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_and`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005fand).
    ///
    /// **See also**:
    /// Graal [`AndNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/AndNode.java);
    /// HotSpot [`AndINode`, `AndLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/mulnode.hpp).
    And,
    /// Bitwise OR (*Binary*).
    /// `%r = or %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_ior`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005fior).
    ///
    /// **See also**:
    /// Graal [`OrNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/OrNode.java);
    /// HotSpot [`OrINode`, `OrLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/addnode.hpp).
    Or,
    /// Bitwise XOR (*Binary*).
    /// `%r = xor %lhs, %rhs`.
    ///
    /// Lowers to GMP [`mpz_xor`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005fxor).
    ///
    /// **See also**:
    /// Graal [`XorNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/XorNode.java);
    /// HotSpot [`XorINode`, `XOrLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/addnode.hpp).
    Xor,
    /// Left shift (*BinaryZU*).
    /// `%r = shl %lhs, $rhs`.
    ///
    /// Lowers to GMP [`mpz_mul_2exp`](https://gmplib.org/manual/Integer-Arithmetic#index-mpz_005fmul_005f2exp).
    ///
    /// **See also**:
    /// Graal [`LeftShiftNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/LeftShiftNode.java);
    /// HotSpot [`LShiftINode`, LShiftLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/mulnode.hpp).
    Shl,
    /// Arithmetic right shift (*BinaryZU*).
    /// `%r = ashr %lhs, $rhs`.
    ///
    /// Lowers to GMP [`mpz_fdiv_q_2exp`](https://gmplib.org/manual/Integer-Division#index-mpz_005ffdiv_005fq_005f2exp).
    //  TODO: Verify that this is the correct function.
    ///
    /// **See also**:
    /// Graal [`RightShiftNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/RightShiftNode.java);
    /// HotSpot [`RShiftINode`, `RShiftLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/mulnode.hpp).
    AShr,
    /// Logical right shift (*BinaryZU*).
    /// `%r = lshr %lhs, $rhs`.
    ///
    /// Lowers to GMP [`mpz_tdiv_q_2exp`](https://gmplib.org/manual/Integer-Division#index-mpz_005ftdiv_005fq_005f2exp).
    //  TODO: Verify that this is the correct function.
    ///
    /// **See also**:
    /// Graal [`UnsignedRightShiftNode`](https://github.com/oracle/graal/blob/master/compiler/src/jdk.internal.vm.compiler/src/org/graalvm/compiler/nodes/calc/UnsignedRightShiftNode.java);
    /// HotSpot [`URShiftINode`, `URShiftINode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/mulnode.hpp).
    LShr,
    /// Bitwise complement (*Unary*).
    /// `%r = not %arg`.
    ///
    /// Lowers to GMP [`mpz_com`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005fcom).
    Not,
    /// Test bit at index (*BinaryZU*).
    /// `%r = testbit %arg, $bit`.
    ///
    /// Lowers to GMP [`mpz_tstbit`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005ftstbit).
    TestBit,
    /// Length of the absolute value in bits (*Unary*).
    /// `%r = count_bits %arg`.
    ///
    /// Lowers to GMP [`mpz_sizeinbase(arg, 2) - 1`](https://gmplib.org/manual/Miscellaneous-Integer-Functions#index-mpz_005fsizeinbase).
    /// This matches Go [`big.Int.BitLen`](https://pkg.go.dev/math/big#Int.BitLen).
    CountBits,
    /// Count leading zeros (*Unary*).
    /// `%r = countlz %arg`.
    ///
    /// **See also**:
    /// HotSpot [`CountLeadingZerosINode`, `CountLeadingZerosLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/countbitsnode.hpp).
    //
    //  TODO: How to define it for negative values? Go math/big does not provide
    //  this operation.
    CountLz,
    /// Count trailing zeros (*Unary*).
    /// `%r = counttz %arg`.
    ///
    /// Lowers to GMP [`mpz_scan1(arg, 0)`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005fscan1).
    ///
    /// **See also**:
    /// HotSpot [`CountTrailingZerosINode`, `CountTrailingZerosLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/countbitsnode.hpp).
    CountTz,
    /// Population count (*Unary*).
    /// `%r = popcount %arg`.
    ///
    /// Lowers to GMP [`mpz_popcount`](https://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling#index-mpz_005fpopcount),
    /// but negative values are first complemented, so the input is always
    /// non-negative. This matches Java [`BigInteger.bitCount`](https://docs.oracle.com/en/java/javase/20/docs/api/java.base/java/math/BigInteger.html#bitCount())
    /// and Common Lisp [`logcount`](https://novaspec.org/cl/f_logcount).
    /// Haskell [`popCount`](https://hackage.haskell.org/package/base/docs/Data-Bits.html#v:popCount)
    /// is `-popCount(-n)` for negative `Integer` values.
    ///
    /// **See also**:
    /// HotSpot [`PopCountINode`, `PopCountLNode`](https://github.com/openjdk/jdk/blob/master/src/hotspot/share/opto/countbitsnode.hpp).
    PopCount,

    // Logical operations
    //
    /// Logical not (*Unary*).
    /// `%r = lnot %arg`.
    //
    //  TODO: Should operate on `u1` instead of MPZ.
    //  TODO: See also.
    LNot,

    // Deprecated operations
    //
    /// Bitwise AND NOT, i.e., `lhs & ~rhs` (*Binary*).
    /// `%r = andnot %lhs, %rhs`.
    DeprecatedAndNot,
    /// Bitwise NOT AND, i.e., `~lhs & rhs` (*Binary*).
    /// `%r = notand %lhs, %rhs`.
    DeprecatedNotAnd,
    /// Bitwise NAND (*Binary*).
    /// `%r = nand %lhs, %rhs`.
    DeprecatedNand,
    /// Bitwise NOR (*Binary*).
    /// `%r = nor %lhs, %rhs`.
    DeprecatedNor,
    /// Bitwise XNOR (*Binary*).
    /// `%r = xnor %lhs, %rhs`.
    DeprecatedXnor,
    /// Bitwise negated AND NOT (*Binary*).
    /// `%r = nandnot %lhs, %rhs`.
    DeprecatedNandNot,
    /// Bitwise negated NOT AND (*Binary*).
    /// `%r = nnotand %lhs, %rhs`.
    DeprecatedNNotAnd,
    /// Negated bit test (*BinaryZU*).
    /// `%r = ntestbit %arg, $bit`.
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
