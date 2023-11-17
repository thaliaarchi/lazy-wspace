use rug::ops::{DivRounding, RemRounding};
use rug::{Complete, Integer as Mpz};

use crate::error::ValueError;
use crate::ir::instructions::{Inst, Opcode, Value};
use crate::ir::{NodeRef, NodeTable};

enum Action {
    New,
    Insert(Inst),
    Use(NodeRef),
}

macro_rules! constz(($n:pat) => {
    Inst::UnaryImmZ {
        opcode: Opcode::ConstZ,
        imm: $n,
    }
});
macro_rules! constu(($n:pat) => {
    Inst::UnaryImmU {
        opcode: Opcode::ConstU,
        imm: $n,
    }
});
macro_rules! const_error(($error:pat) => {
    Inst::UnaryImmError {
        opcode: Opcode::ConstError,
        imm: $error,
    }
});
macro_rules! unary(
    ($opcode:ident, $val:pat) => {
        Inst::Unary {
            opcode: Opcode::$opcode,
            arg: $val,
        }
    };
    ($opcode:ident, ..) => {
        Inst::Unary {
            opcode: Opcode::$opcode,
            ..
        }
    };
);
macro_rules! binary(
    ($opcode:ident, $lhs:pat, $rhs:pat) => {
        Inst::Binary {
            opcode: Opcode::$opcode,
            args: [$lhs, $rhs],
        }
    };
    ($opcode:ident, ..) => {
        Inst::Binary {
            opcode: Opcode::$opcode,
            ..
        }
    };
);
macro_rules! add(($($args:tt)*) => { binary!(Add, $($args)*) });
macro_rules! sub(($($args:tt)*) => { binary!(Sub, $($args)*) });
macro_rules! mul(($($args:tt)*) => { binary!(Mul, $($args)*) });
macro_rules! div(($($args:tt)*) => { binary!(Div, $($args)*) });
macro_rules! mod_(($($args:tt)*) => { binary!(Mod, $($args)*) });
macro_rules! and(($($args:tt)*) => { binary!(And, $($args)*) });
macro_rules! or(($($args:tt)*) => { binary!(Or, $($args)*) });
macro_rules! xor(($($args:tt)*) => { binary!(Xor, $($args)*) });
macro_rules! andnot(($($args:tt)*) => { binary!(AndNot, $($args)*) });
macro_rules! notand(($($args:tt)*) => { binary!(NotAnd, $($args)*) });
macro_rules! nand(($($args:tt)*) => { binary!(Nand, $($args)*) });
macro_rules! nor(($($args:tt)*) => { binary!(Nor, $($args)*) });
macro_rules! xnor(($($args:tt)*) => { binary!(Xnor, $($args)*) });
macro_rules! nandnot(($($args:tt)*) => { binary!(NandNot, $($args)*) });
macro_rules! nnotand(($($args:tt)*) => { binary!(NNotAnd, $($args)*) });
macro_rules! shl(($($args:tt)*) => { binary!(Shl, $($args)*) });
macro_rules! ashr(($($args:tt)*) => { binary!(AShr, $($args)*) });
macro_rules! test_bit(($($args:tt)*) => { binary!(TestBit, $($args)*) });
macro_rules! ntest_bit(($($args:tt)*) => { binary!(NTestBit, $($args)*) });
macro_rules! neg(($($args:tt)*) => { unary!(Neg, $($args)*) });
macro_rules! pop_count(($($args:tt)*) => { unary!(PopCount, $($args)*) });

impl NodeTable<'_> {
    pub fn insert_peephole(&mut self, inst: Inst) -> NodeRef {
        if inst.is_value() {
            match inst {
                Inst::Unary {
                    opcode: _,
                    arg: val,
                } => return self.insert_unary(inst, val),
                Inst::Binary {
                    opcode: _,
                    args: [lhs, rhs],
                } => return self.insert_binary(inst, lhs, rhs),
                _ => {}
            }
        }
        self.insert(inst)
    }

    fn insert_binary(&mut self, inst: Inst, lhs: Value, rhs: Value) -> NodeRef {
        use Action::*;

        macro_rules! getbit_op(
            ($inst:ident ($x:expr, $y:expr)) => {
                match (&*self[*$x], &*self[*$y]) {
                    (test_bit!(x, bx), test_bit!(y, by)) if bx == by => {
                        let b = *bx;
                        Insert(Inst::test_bit(Value::new(self.insert_peephole(Inst::$inst(*x, *y))), b))
                    },
                    _ => New,
                }
            };
            ($inst:ident ($x:expr, $y:expr), $b:expr) => {
                {
                    let b = *$b;
                    Insert(Inst::test_bit(Value::new(self.insert_peephole(Inst::$inst(*$x, *$y))), b))
                }
            };
        );

        let action = match (&inst, &*self[lhs], &*self[rhs]) {
            // Constant expressions
            (_, constz!(lhs), constz!(rhs)) => {
                let (lhs, rhs) = (lhs.as_ref(), rhs.as_ref());
                let res = match &inst {
                    add!(..) => Ok((lhs + rhs).complete()),
                    sub!(..) => Ok((lhs - rhs).complete()),
                    mul!(..) => Ok((lhs * rhs).complete()),
                    div!(..) if rhs.is_zero() => Err(ValueError::DivModZero),
                    div!(..) => Ok(lhs.div_floor(rhs).complete()),
                    mod_!(..) if rhs.is_zero() => Err(ValueError::DivModZero),
                    mod_!(..) => Ok(lhs.rem_floor(rhs).complete()),
                    and!(..) => Ok((lhs & rhs).complete()),
                    or!(..) => Ok((lhs | rhs).complete()),
                    xor!(..) => Ok((lhs ^ rhs).complete()),
                    _ => panic!("not a binary operator (Mpz, Mpz): {inst:?}"),
                };
                let inst = match res {
                    Ok(r) => Inst::constz(r),
                    Err(err) => Inst::const_error(err),
                };
                Insert(inst)
            }
            (_, constz!(lhs), constu!(rhs)) => {
                let (lhs, rhs) = (lhs.as_ref(), *rhs);
                let n = match &inst {
                    shl!(..) => (lhs << rhs).complete(),
                    ashr!(..) => (lhs >> rhs).complete(),
                    test_bit!(..) => lhs.get_bit(rhs).into(),
                    ntest_bit!(..) => (!lhs.get_bit(rhs)).into(),
                    _ => panic!("not a binary operator (Mpz, u32): {inst:?}"),
                };
                Insert(Inst::constz(n))
            }

            // Errors
            (add!(..) | sub!(..) | mul!(..) | div!(..) | mod_!(..), _, const_error!(_)) => {
                Use(*rhs)
            }
            (
                add!(..) | sub!(..) | mul!(..) | div!(..) | mod_!(..),
                const_error!(_),
                constz!(_),
            ) => Use(*lhs),

            // Move constants right
            (add!(..), constz!(_), _) => Insert(Inst::add(rhs, lhs)),
            (mul!(..), constz!(_), _) => Insert(Inst::mul(rhs, lhs)),
            (and!(..), constz!(_), _) => Insert(Inst::and(rhs, lhs)),
            (or!(..), constz!(_), _) => Insert(Inst::or(rhs, lhs)),
            (xor!(..), constz!(_), _) => Insert(Inst::xor(rhs, lhs)),

            // Identities
            (add!(..) | sub!(..), _, constz!(rhs)) if rhs.is_zero() => Use(*lhs),
            (mul!(..) | div!(..), _, constz!(rhs)) if **rhs == 1 => Use(*lhs),
            (and!(..) | or!(..), _, _) if lhs == rhs => Use(*lhs),

            // Division by 0
            (div!(..) | mod_!(..), _, constz!(rhs)) if rhs.is_zero() => {
                Insert(Inst::const_error(ValueError::DivModZero))
            }

            // Negation
            (sub!(..), constz!(lhs), _) if lhs.is_zero() => Insert(Inst::neg(rhs)),
            (add!(..), _, neg!(rhs)) => Insert(Inst::sub(lhs, *rhs)),
            (sub!(..), _, neg!(rhs)) => Insert(Inst::add(lhs, *rhs)),

            // Negation
            (sub!(..), constz!(one), test_bit!(x, b)) if **one == 1 => {
                Insert(Inst::ntest_bit(*x, *b))
            }
            (sub!(..), constz!(one), ntest_bit!(x, b)) if **one == 1 => {
                Insert(Inst::test_bit(*x, *b))
            }

            // Shifts
            (mul!(..), _, constz!(rhs)) if rhs.to_u32().is_some_and(u32::is_power_of_two) => {
                let rhs = self.insert_value(Inst::constz(rhs.to_u32().unwrap().ilog2()));
                Insert(Inst::shl(lhs, rhs))
            }
            (div!(..), _, constz!(rhs)) if rhs.to_u32().is_some_and(u32::is_power_of_two) => {
                let rhs = self.insert_value(Inst::constz(rhs.to_u32().unwrap().ilog2()));
                Insert(Inst::ashr(lhs, rhs))
            }

            // Single-bit AND
            // x * y == x & y
            (mul!(..), test_bit!(_, bx), test_bit!(_, by)) if bx == by => {
                Insert(Inst::and(lhs, rhs))
            }
            // (x + y) / 2 == x & y
            (div!(..), add!(x, y), constz!(rhs)) if **rhs == 2 => {
                getbit_op!(and(x, y))
            }

            // Single-bit OR
            // (x + y) - (x & y) = x | y
            (sub!(..), add!(x1, y1), and!(x2, y2))
                if x1 == x2 && y1 == y2 || x1 == y2 && y1 == x2 =>
            {
                getbit_op!(or(x2, y2))
            }
            // x + (y - (x & y)) = x | y
            (add!(..), test_bit!(..), sub!(y1, z)) => {
                let x1 = &lhs;
                match &*self[*z] {
                    and!(x2, y2) if x1 == x2 && y1 == y2 || x1 == y2 && y1 == x2 => {
                        getbit_op!(or(x2, y2))
                    }
                    _ => New,
                }
            }

            // Single-bit XOR
            // (x + y) % 2 == x ^ y
            (mod_!(..), add!(x, y), constz!(rhs)) if **rhs == 2 => {
                getbit_op!(xor(x, y))
            }
            // (x + y) * !(x & y) == x ^ y
            (mul!(..), add!(x1, y1), nand!(x2, y2))
                if x1 == x2 && y1 == y2 || x1 == y2 && y1 == x2 =>
            {
                getbit_op!(xor(x2, y2))
            }

            // Single-bit ANDNOT
            // x * !y == x & !y
            (mul!(..), test_bit!(x, bx), ntest_bit!(y, by)) if bx == by => {
                getbit_op!(andnot(x, y), bx)
            }

            // Single-bit NOTAND
            // !x * y == !x & y
            (mul!(..), ntest_bit!(x, bx), test_bit!(y, by)) if bx == by => {
                getbit_op!(notand(x, y), bx)
            }

            // Single-bit NOR
            // !x * !y == !x & !y == !(x | y)
            (mul!(..), ntest_bit!(x, bx), ntest_bit!(y, by)) if bx == by => {
                getbit_op!(nor(x, y), bx)
            }

            // Bit identities
            (test_bit!(..), test_bit!(..) | ntest_bit!(..), constu!(0)) => Use(*lhs),
            (ntest_bit!(..), test_bit!(x, b), constu!(0)) => Insert(Inst::ntest_bit(*x, *b)),
            (ntest_bit!(..), ntest_bit!(x, b), constu!(0)) => Insert(Inst::test_bit(*x, *b)),

            // Bit extraction
            (test_bit!(..), ashr!(v, bit), constu!(0)) => Insert(Inst::test_bit(*v, *bit)),
            (ntest_bit!(..), ashr!(v, bit), constu!(0)) => Insert(Inst::ntest_bit(*v, *bit)),
            (shl!(..), test_bit!(v, bit), constu!(b)) if rhs == *bit => Insert(Inst::and(
                *v,
                self.insert_value(Inst::constz(Mpz::ONE << b)),
            )),

            // Get LSB
            // (Must be after single-bit XOR)
            (mod_!(..), _, constz!(rhs)) if **rhs == 2 => {
                Insert(Inst::test_bit(lhs, self.insert_mpz(&Mpz::ZERO)))
            }
            (and!(..), _, constz!(rhs)) if **rhs == 1 => {
                Insert(Inst::test_bit(lhs, self.insert_mpz(&Mpz::ZERO)))
            }

            // Bit negation
            (ntest_bit!(..), and!(x, y), _) => Insert(Inst::test_bit(
                Value::new(self.insert_peephole(Inst::nand(*x, *y))),
                rhs,
            )),
            (ntest_bit!(..), or!(x, y), _) => Insert(Inst::test_bit(
                Value::new(self.insert_peephole(Inst::nor(*x, *y))),
                rhs,
            )),
            (ntest_bit!(..), xor!(x, y), _) => Insert(Inst::test_bit(
                Value::new(self.insert_peephole(Inst::xnor(*x, *y))),
                rhs,
            )),
            (ntest_bit!(..), andnot!(x, y), _) => Insert(Inst::test_bit(
                Value::new(self.insert_peephole(Inst::nandnot(*x, *y))),
                rhs,
            )),
            (ntest_bit!(..), notand!(x, y), _) => Insert(Inst::test_bit(
                Value::new(self.insert_peephole(Inst::nnotand(*x, *y))),
                rhs,
            )),
            (ntest_bit!(..), nand!(x, y), _) => Insert(Inst::test_bit(
                Value::new(self.insert_peephole(Inst::and(*x, *y))),
                rhs,
            )),
            (ntest_bit!(..), nor!(x, y), _) => Insert(Inst::test_bit(
                Value::new(self.insert_peephole(Inst::or(*x, *y))),
                rhs,
            )),
            (ntest_bit!(..), xnor!(x, y), _) => Insert(Inst::test_bit(
                Value::new(self.insert_peephole(Inst::xor(*x, *y))),
                rhs,
            )),
            (ntest_bit!(..), nandnot!(x, y), _) => Insert(Inst::test_bit(
                Value::new(self.insert_peephole(Inst::andnot(*x, *y))),
                rhs,
            )),
            (ntest_bit!(..), nnotand!(x, y), _) => Insert(Inst::test_bit(
                Value::new(self.insert_peephole(Inst::notand(*x, *y))),
                rhs,
            )),

            // Single-bit distribution
            (and!(..), test_bit!(x, bx), test_bit!(y, by)) if bx == by => {
                let bits = *bx;
                Insert(Inst::test_bit(
                    Value::new(self.insert_peephole(Inst::and(*x, *y))),
                    bits,
                ))
            }
            (or!(..), test_bit!(x, bx), test_bit!(y, by)) if bx == by => {
                let bits = *bx;
                Insert(Inst::test_bit(
                    Value::new(self.insert_peephole(Inst::or(*x, *y))),
                    bits,
                ))
            }
            (xor!(..), test_bit!(x, bx), test_bit!(y, by)) if bx == by => {
                let bits = *bx;
                Insert(Inst::test_bit(
                    Value::new(self.insert_peephole(Inst::xor(*x, *y))),
                    bits,
                ))
            }
            (andnot!(..), test_bit!(x, bx), test_bit!(y, by)) if bx == by => {
                let bits = *bx;
                Insert(Inst::test_bit(
                    Value::new(self.insert_peephole(Inst::andnot(*x, *y))),
                    bits,
                ))
            }
            (nand!(..), test_bit!(x, bx), test_bit!(y, by)) if bx == by => {
                let bits = *bx;
                Insert(Inst::test_bit(
                    Value::new(self.insert_peephole(Inst::nand(*x, *y))),
                    bits,
                ))
            }
            (nor!(..), test_bit!(x, bx), test_bit!(y, by)) if bx == by => {
                let bits = *bx;
                Insert(Inst::test_bit(
                    Value::new(self.insert_peephole(Inst::nor(*x, *y))),
                    bits,
                ))
            }
            (xnor!(..), test_bit!(x, bx), test_bit!(y, by)) if bx == by => {
                let bits = *bx;
                Insert(Inst::test_bit(
                    Value::new(self.insert_peephole(Inst::xnor(*x, *y))),
                    bits,
                ))
            }
            (nandnot!(..), test_bit!(x, bx), test_bit!(y, by)) if bx == by => {
                let bits = *bx;
                Insert(Inst::test_bit(
                    Value::new(self.insert_peephole(Inst::nandnot(*x, *y))),
                    bits,
                ))
            }
            (and!(..), ntest_bit!(x, bx), ntest_bit!(y, by)) if bx == by => {
                let bits = *bx;
                Insert(Inst::ntest_bit(
                    Value::new(self.insert_peephole(Inst::and(*x, *y))),
                    bits,
                ))
            }
            (or!(..), ntest_bit!(x, bx), ntest_bit!(y, by)) if bx == by => {
                let bits = *bx;
                Insert(Inst::ntest_bit(
                    Value::new(self.insert_peephole(Inst::or(*x, *y))),
                    bits,
                ))
            }
            (xor!(..), ntest_bit!(x, bx), ntest_bit!(y, by)) if bx == by => {
                let bits = *bx;
                Insert(Inst::ntest_bit(
                    Value::new(self.insert_peephole(Inst::xor(*x, *y))),
                    bits,
                ))
            }
            (andnot!(..), ntest_bit!(x, bx), ntest_bit!(y, by)) if bx == by => {
                let bits = *bx;
                Insert(Inst::ntest_bit(
                    Value::new(self.insert_peephole(Inst::andnot(*x, *y))),
                    bits,
                ))
            }
            (nand!(..), ntest_bit!(x, bx), ntest_bit!(y, by)) if bx == by => {
                let bits = *bx;
                Insert(Inst::ntest_bit(
                    Value::new(self.insert_peephole(Inst::nand(*x, *y))),
                    bits,
                ))
            }
            (nor!(..), ntest_bit!(x, bx), ntest_bit!(y, by)) if bx == by => {
                let bits = *bx;
                Insert(Inst::ntest_bit(
                    Value::new(self.insert_peephole(Inst::nor(*x, *y))),
                    bits,
                ))
            }
            (xnor!(..), ntest_bit!(x, bx), ntest_bit!(y, by)) if bx == by => {
                let bits = *bx;
                Insert(Inst::ntest_bit(
                    Value::new(self.insert_peephole(Inst::xnor(*x, *y))),
                    bits,
                ))
            }
            (nandnot!(..), ntest_bit!(x, bx), ntest_bit!(y, by)) if bx == by => {
                let bits = *bx;
                Insert(Inst::ntest_bit(
                    Value::new(self.insert_peephole(Inst::nandnot(*x, *y))),
                    bits,
                ))
            }

            // Multi-bit AND
            (mod_!(..), _, constz!(rhs)) if rhs.is_power_of_two() => {
                Insert(Inst::and(lhs, self.insert_value(Inst::constz(&**rhs - 1))))
            }
            (add!(..), and!(x1, m1), and!(x2, m2)) if x1 == x2 => {
                match (&*self[*m1], &*self[*m2]) {
                    (constz!(m), constz!(n)) if (&**m & &**n).complete().is_zero() => {
                        Insert(Inst::and(*x1, self.insert_value(Inst::constz(&**m | &**n))))
                    }
                    _ => New,
                }
            }
            (add!(..), test_bit!(x1, bit), and!(x2, m))
            | (add!(..), and!(x2, m), test_bit!(x1, bit))
                if x1 == x2 =>
            {
                match (&*self[*bit], &*self[*m]) {
                    (constu!(0), constz!(m)) if !m.get_bit(0) => {
                        Insert(Inst::and(*x1, self.insert_value(Inst::constz(&**m | 1))))
                    }
                    _ => New,
                }
            }
            (or!(..), and!(x1, m1), and!(x2, m2)) if x1 == x2 => match (&*self[*m1], &*self[*m2]) {
                (constz!(m), constz!(n)) => {
                    Insert(Inst::and(*x1, self.insert_value(Inst::constz(&**m | &**n))))
                }
                _ => New,
            },
            (or!(..), test_bit!(x1, bit), and!(x2, m))
            | (add!(..), and!(x2, m), test_bit!(x1, bit))
                if x1 == x2 =>
            {
                match (&*self[*bit], &*self[*m]) {
                    (constu!(0), constz!(m)) => {
                        Insert(Inst::and(*x1, self.insert_value(Inst::constz(&**m | 1))))
                    }
                    _ => New,
                }
            }

            // Popcount
            (add!(..), test_bit!(x1, bit1), test_bit!(x2, bit2)) if x1 == x2 && bit1 != bit2 => {
                match (&*self[*bit1], &*self[*bit2]) {
                    (constu!(bit1), constu!(bit2)) => {
                        let x = *x1;
                        let m = self.insert_value(Inst::constz(
                            (Mpz::ONE << bit1).complete() | (Mpz::ONE << bit2).complete(),
                        ));
                        Insert(Inst::pop_count(Value::new(
                            self.insert_peephole(Inst::and(x, m)),
                        )))
                    }
                    _ => New,
                }
            }
            (add!(..), pop_count!(a), test_bit!(x2, bit))
            | (add!(..), test_bit!(x2, bit), pop_count!(a)) => match (&*self[*a], &*self[*bit]) {
                (and!(x1, m), constu!(bit)) if x1 == x2 => match &*self[*m] {
                    constz!(m) if !m.get_bit(*bit) => {
                        let x = *x1;
                        let m =
                            self.insert_value(Inst::constz(&**m | (Mpz::ONE << bit).complete()));
                        Insert(Inst::pop_count(Value::new(
                            self.insert_peephole(Inst::and(x, m)),
                        )))
                    }
                    _ => New,
                },
                _ => New,
            },
            (add!(..), pop_count!(a), pop_count!(b)) => match (&*self[*a], &*self[*b]) {
                (and!(x1, m), and!(x2, n)) if x1 == x2 => match (&*self[*m], &*self[*n]) {
                    (constz!(m), constz!(n)) if (&**m & &**n).complete().is_zero() => {
                        let x = *x1;
                        let mn = self.insert_value(Inst::constz(&**m | &**n));
                        Insert(Inst::pop_count(Value::new(
                            self.insert_peephole(Inst::and(x, mn)),
                        )))
                    }
                    _ => New,
                },
                _ => New,
            },
            (or!(..), test_bit!(x1, bit1), test_bit!(x2, bit2)) if x1 == x2 => {
                match (&*self[*bit1], &*self[*bit2]) {
                    (constu!(bit1), constu!(bit2)) => {
                        let x = *x1;
                        let m = self.insert_value(Inst::constz(
                            (Mpz::ONE << bit1).complete() | (Mpz::ONE << bit2).complete(),
                        ));
                        Insert(Inst::pop_count(Value::new(
                            self.insert_peephole(Inst::and(x, m)),
                        )))
                    }
                    _ => New,
                }
            }
            (or!(..), pop_count!(a), test_bit!(x2, bit))
            | (or!(..), test_bit!(x2, bit), pop_count!(a)) => match (&*self[*a], &*self[*bit]) {
                (and!(x1, m), constu!(bit)) if x1 == x2 => match &*self[*m] {
                    constz!(m) => {
                        let x = *x1;
                        let m =
                            self.insert_value(Inst::constz(&**m | (Mpz::ONE << bit).complete()));
                        Insert(Inst::pop_count(Value::new(
                            self.insert_peephole(Inst::and(x, m)),
                        )))
                    }
                    _ => New,
                },
                _ => New,
            },
            (or!(..), pop_count!(a), pop_count!(b)) => match (&*self[*a], &*self[*b]) {
                (and!(x1, m), and!(x2, n)) if x1 == x2 => match (&*self[*m], &*self[*n]) {
                    (constz!(m), constz!(n)) => {
                        let x = *x1;
                        let mn = self.insert_value(Inst::constz(&**m | &**n));
                        Insert(Inst::pop_count(Value::new(
                            self.insert_peephole(Inst::and(x, mn)),
                        )))
                    }
                    _ => New,
                },
                _ => New,
            },

            _ => New,
        };

        match action {
            Action::New => self.insert(inst),
            Action::Insert(inst) => self.insert_peephole(inst),
            Action::Use(i) => i,
        }
    }

    fn insert_unary(&mut self, inst: Inst, v: Value) -> NodeRef {
        use Action::*;

        let action = match (&inst, &*self[v]) {
            // Constant expressions
            (_, constz!(n)) => {
                let n = &**n;
                let r = match inst {
                    neg!(_) => (-n).complete(),
                    pop_count!(_) => n.count_ones().unwrap().into(), // TODO: only valid for n >= 0
                    _ => panic!("not a unary operator: {inst:?}"),
                };
                Insert(Inst::constz(r))
            }

            (_, const_error!(_)) => Use(*v),
            (neg!(_), neg!(v)) => Use(**v),

            _ => New,
        };

        match action {
            Action::New => self.insert(inst),
            Action::Insert(inst) => self.insert_peephole(inst),
            Action::Use(i) => i,
        }
    }
}
