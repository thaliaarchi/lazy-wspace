use std::cmp::Ordering;

use rug::ops::{DivRounding, RemRounding};
use rug::{Complete, Integer};

use crate::error::NumberError;
use crate::ir::{Inst, NodeRef, NodeTable};

include!(concat!(env!("OUT_DIR"), "/rewrites.rs"));

enum Action {
    New,
    Insert(Inst),
    Use(NodeRef),
}

impl NodeTable<'_> {
    pub fn insert_peephole(&mut self, inst: Inst) -> NodeRef {
        match inst {
            Inst::Add(lhs, rhs)
            | Inst::Sub(lhs, rhs)
            | Inst::Mul(lhs, rhs)
            | Inst::Div(lhs, rhs)
            | Inst::Mod(lhs, rhs)
            | Inst::And(lhs, rhs)
            | Inst::Or(lhs, rhs)
            | Inst::Xor(lhs, rhs)
            | Inst::AndNot(lhs, rhs)
            | Inst::NotAnd(lhs, rhs)
            | Inst::Nand(lhs, rhs)
            | Inst::Nor(lhs, rhs)
            | Inst::Xnor(lhs, rhs)
            | Inst::NandNot(lhs, rhs)
            | Inst::NNotAnd(lhs, rhs) => self.insert_op2(inst, lhs, rhs),
            Inst::Shl(lhs, rhs)
            | Inst::Shr(lhs, rhs)
            | Inst::GetBit(lhs, rhs)
            | Inst::NGetBit(lhs, rhs) => self.insert_op2_u32(inst, lhs, rhs),
            Inst::Neg(v) | Inst::Popcnt(v) => self.insert_op1(inst, v),
            Inst::Number(_)
            | Inst::Error(_)
            | Inst::Eval(_)
            | Inst::StackRef(_, _)
            | Inst::CheckedStackRef(_)
            | Inst::GuardStack(_)
            | Inst::Push(_)
            | Inst::Drop(_)
            | Inst::DropLazy(_)
            | Inst::HeapRef(_)
            | Inst::Store(_, _)
            | Inst::Print(_, _)
            | Inst::Read(_)
            | Inst::Call(_, _)
            | Inst::Jmp(_)
            | Inst::Br(_, _, _, _)
            | Inst::Ret
            | Inst::Exit
            | Inst::Panic(_) => self.insert(inst),
        }
    }

    fn insert_op2(&mut self, inst: Inst, lhs: NodeRef, rhs: NodeRef) -> NodeRef {
        use Action::*;
        use Inst::*;

        macro_rules! getbit_op(
            ($Inst:ident ($x:expr, $y:expr)) => {
                match (&*self[*$x], &*self[*$y]) {
                    (GetBit(x, bx), GetBit(y, by)) if bx == by => {
                        let b = *bx;
                        Insert(GetBit(self.insert_peephole($Inst(*x, *y)), b))
                    }
                    _ => New,
                }
            };
            ($Inst:ident ($x:expr, $y:expr), $b:expr) => {
                {
                    let b = *$b;
                    Insert(GetBit(self.insert_peephole($Inst(*$x, *$y)), b))
                }
            };
        );

        let action = match (&inst, &*self[lhs], &*self[rhs]) {
            // Constant expressions
            (_, Number(lhs), Number(rhs)) => {
                let (lhs, rhs) = (lhs.as_ref(), rhs.as_ref());
                let res = match &inst {
                    Add(..) => Ok((lhs + rhs).complete()),
                    Sub(..) => Ok((lhs - rhs).complete()),
                    Mul(..) => Ok((lhs * rhs).complete()),
                    Div(..) if rhs.cmp0() == Ordering::Equal => Err(NumberError::DivModZero),
                    Div(..) => Ok(lhs.div_floor(rhs).into()),
                    Mod(..) if rhs.cmp0() == Ordering::Equal => Err(NumberError::DivModZero),
                    Mod(..) => Ok(lhs.rem_floor(rhs).into()),
                    And(..) => Ok((lhs & rhs).complete()),
                    Or(..) => Ok((lhs | rhs).complete()),
                    Xor(..) => Ok((lhs ^ rhs).complete()),
                    AndNot(..) => Ok(lhs & (!rhs).complete()),
                    Nand(..) => Ok(!(lhs & rhs).complete()),
                    Nor(..) => Ok(!(lhs | rhs).complete()),
                    Xnor(..) => Ok(!(lhs ^ rhs).complete()),
                    NandNot(..) => Ok(!(lhs & (!rhs).complete())),
                    _ => panic!("not a binary operator: {inst}"),
                };
                let inst = match res {
                    Ok(r) => Number(Box::new(r)),
                    Err(err) => Error(err),
                };
                Insert(inst)
            }

            // Errors
            (Add(..) | Sub(..) | Mul(..) | Div(..) | Mod(..), _, Error(_)) => Use(rhs),
            (Add(..) | Sub(..) | Mul(..) | Div(..) | Mod(..), Error(_), Number(_)) => Use(lhs),

            // Move constants right
            (Add(..), Number(_), _) => Insert(Add(rhs, lhs)),
            (Mul(..), Number(_), _) => Insert(Mul(rhs, lhs)),
            (And(..), Number(_), _) => Insert(And(rhs, lhs)),
            (Or(..), Number(_), _) => Insert(Or(rhs, lhs)),
            (Xor(..), Number(_), _) => Insert(Xor(rhs, lhs)),

            // Identities
            (Add(..) | Sub(..), _, Number(rhs)) if rhs.cmp0() == Ordering::Equal => Use(lhs),
            (Mul(..) | Div(..), _, Number(rhs)) if **rhs == 1 => Use(lhs),
            (And(..) | Or(..), _, _) if lhs == rhs => Use(lhs),

            // Division by 0
            (Div(..) | Mod(..), _, Number(rhs)) if rhs.cmp0() == Ordering::Equal => {
                Insert(Error(NumberError::DivModZero))
            }

            // Negation
            (Sub(..), Number(lhs), _) if lhs.cmp0() == Ordering::Equal => Insert(Neg(rhs)),
            (Add(..), _, Neg(rhs)) => Insert(Sub(lhs, *rhs)),
            (Sub(..), _, Neg(rhs)) => Insert(Add(lhs, *rhs)),

            // Negation
            (Sub(..), Number(one), GetBit(x, b)) if **one == 1 => Insert(NGetBit(*x, *b)),
            (Sub(..), Number(one), NGetBit(x, b)) if **one == 1 => Insert(GetBit(*x, *b)),

            // Shifts
            (Mul(..), _, Number(rhs)) if rhs.to_u32().is_some_and(|r| r.is_power_of_two()) => {
                Insert(Shl(lhs, rhs.to_u32().unwrap().ilog2()))
            }
            (Div(..), _, Number(rhs)) if rhs.to_u32().is_some_and(|r| r.is_power_of_two()) => {
                Insert(Shr(lhs, rhs.to_u32().unwrap().ilog2()))
            }

            // Single-bit AND
            // x * y == x & y
            (Mul(..), GetBit(_, bx), GetBit(_, by)) if bx == by => Insert(And(lhs, rhs)),
            // (x + y) / 2 == x & y
            (Div(..), Add(x, y), Number(rhs)) if **rhs == 2 => {
                getbit_op!(And(x, y))
            }

            // Single-bit OR
            // (x + y) - (x & y) = x | y
            (Sub(..), Add(x1, y1), And(x2, y2)) if x1 == x2 && y1 == y2 || x1 == y2 && y1 == x2 => {
                getbit_op!(Or(x2, y2))
            }
            // x + (y - (x & y)) = x | y
            (Add(..), GetBit(..), Sub(y1, z)) => {
                let x1 = &lhs;
                match &*self[*z] {
                    And(x2, y2) if x1 == x2 && y1 == y2 || x1 == y2 && y1 == x2 => {
                        getbit_op!(Or(x2, y2))
                    }
                    _ => New,
                }
            }

            // Single-bit XOR
            // (x + y) % 2 == x ^ y
            (Mod(..), Add(x, y), Number(rhs)) if **rhs == 2 => {
                getbit_op!(Xor(x, y))
            }
            // (x + y) * !(x & y) == x ^ y
            (Mul(..), Add(x1, y1), Nand(x2, y2))
                if x1 == x2 && y1 == y2 || x1 == y2 && y1 == x2 =>
            {
                getbit_op!(Xor(x2, y2))
            }

            // Single-bit ANDNOT
            // x * !y == x & !y
            (Mul(..), GetBit(x, bx), NGetBit(y, by)) if bx == by => {
                getbit_op!(AndNot(x, y), bx)
            }

            // Single-bit NOTAND
            // !x * y == !x & y
            (Mul(..), NGetBit(x, bx), GetBit(y, by)) if bx == by => {
                getbit_op!(NotAnd(x, y), bx)
            }

            // Single-bit NOR
            // !x * !y == !x & !y == !(x | y)
            (Mul(..), NGetBit(x, bx), NGetBit(y, by)) if bx == by => {
                getbit_op!(Nor(x, y), bx)
            }

            // Get LSB
            // (Must be after single-bit XOR)
            (Mod(..), _, Number(rhs)) if **rhs == 2 => Insert(GetBit(lhs, 0)),
            (And(..), _, Number(rhs)) if **rhs == 1 => Insert(GetBit(lhs, 0)),

            // Single-bit distribution
            (And(..), GetBit(x, bx), GetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(GetBit(self.insert_peephole(And(*x, *y)), bits))
            }
            (Or(..), GetBit(x, bx), GetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(GetBit(self.insert_peephole(Or(*x, *y)), bits))
            }
            (Xor(..), GetBit(x, bx), GetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(GetBit(self.insert_peephole(Xor(*x, *y)), bits))
            }
            (AndNot(..), GetBit(x, bx), GetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(GetBit(self.insert_peephole(AndNot(*x, *y)), bits))
            }
            (Nand(..), GetBit(x, bx), GetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(GetBit(self.insert_peephole(Nand(*x, *y)), bits))
            }
            (Nor(..), GetBit(x, bx), GetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(GetBit(self.insert_peephole(Nor(*x, *y)), bits))
            }
            (Xnor(..), GetBit(x, bx), GetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(GetBit(self.insert_peephole(Xnor(*x, *y)), bits))
            }
            (NandNot(..), GetBit(x, bx), GetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(GetBit(self.insert_peephole(NandNot(*x, *y)), bits))
            }
            (And(..), NGetBit(x, bx), NGetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(NGetBit(self.insert_peephole(And(*x, *y)), bits))
            }
            (Or(..), NGetBit(x, bx), NGetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(NGetBit(self.insert_peephole(Or(*x, *y)), bits))
            }
            (Xor(..), NGetBit(x, bx), NGetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(NGetBit(self.insert_peephole(Xor(*x, *y)), bits))
            }
            (AndNot(..), NGetBit(x, bx), NGetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(NGetBit(self.insert_peephole(AndNot(*x, *y)), bits))
            }
            (Nand(..), NGetBit(x, bx), NGetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(NGetBit(self.insert_peephole(Nand(*x, *y)), bits))
            }
            (Nor(..), NGetBit(x, bx), NGetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(NGetBit(self.insert_peephole(Nor(*x, *y)), bits))
            }
            (Xnor(..), NGetBit(x, bx), NGetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(NGetBit(self.insert_peephole(Xnor(*x, *y)), bits))
            }
            (NandNot(..), NGetBit(x, bx), NGetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(NGetBit(self.insert_peephole(NandNot(*x, *y)), bits))
            }

            // Multi-bit AND
            (Mod(..), _, Number(rhs)) if rhs.is_power_of_two() => {
                Insert(And(lhs, self.insert(Inst::number(&**rhs - 1))))
            }
            (Add(..), And(x1, m1), And(x2, m2)) if x1 == x2 => match (&*self[*m1], &*self[*m2]) {
                (Number(m), Number(n)) if (&**m & &**n).complete().cmp0() == Ordering::Equal => {
                    Insert(And(*x1, self.insert(Inst::number(&**m | &**n))))
                }
                _ => New,
            },
            (Add(..), GetBit(x1, 0), And(x2, m)) | (Add(..), And(x2, m), GetBit(x1, 0))
                if x1 == x2 =>
            {
                match &*self[*m] {
                    Number(m) if !m.get_bit(0) => {
                        Insert(And(*x1, self.insert(Inst::number(&**m | 1))))
                    }
                    _ => New,
                }
            }
            (Or(..), And(x1, m1), And(x2, m2)) if x1 == x2 => match (&*self[*m1], &*self[*m2]) {
                (Number(m), Number(n)) => Insert(And(*x1, self.insert(Inst::number(&**m | &**n)))),
                _ => New,
            },
            (Or(..), GetBit(x1, 0), And(x2, m)) | (Add(..), And(x2, m), GetBit(x1, 0))
                if x1 == x2 =>
            {
                match &*self[*m] {
                    Number(m) => Insert(And(*x1, self.insert(Inst::number(&**m | 1)))),
                    _ => New,
                }
            }

            // Popcnt
            (Add(..), GetBit(x1, bit1), GetBit(x2, bit2)) if x1 == x2 && bit1 != bit2 => {
                let x = *x1;
                let m = self.insert(Inst::number(
                    (Integer::ONE << bit1).complete() | (Integer::ONE << bit2).complete(),
                ));
                Insert(Popcnt(self.insert_peephole(And(x, m))))
            }
            (Add(..), Popcnt(a), GetBit(x2, bit)) | (Add(..), GetBit(x2, bit), Popcnt(a)) => {
                match &*self[*a] {
                    And(x1, m) if x1 == x2 => match &*self[*m] {
                        Number(m) if !m.get_bit(*bit) => {
                            let x = *x1;
                            let m =
                                self.insert(Inst::number(&**m | (Integer::ONE << bit).complete()));
                            Insert(Popcnt(self.insert_peephole(And(x, m))))
                        }
                        _ => New,
                    },
                    _ => New,
                }
            }
            (Add(..), Popcnt(a), Popcnt(b)) => match (&*self[*a], &*self[*b]) {
                (And(x1, m), And(x2, n)) if x1 == x2 => match (&*self[*m], &*self[*n]) {
                    (Number(m), Number(n))
                        if (&**m & &**n).complete().cmp0() == Ordering::Equal =>
                    {
                        let x = *x1;
                        let mn = self.insert(Inst::number(&**m | &**n));
                        Insert(Popcnt(self.insert_peephole(And(x, mn))))
                    }
                    _ => New,
                },
                _ => New,
            },
            (Or(..), GetBit(x1, bit1), GetBit(x2, bit2)) if x1 == x2 => {
                let x = *x1;
                let m = self.insert(Inst::number(
                    (Integer::ONE << bit1).complete() | (Integer::ONE << bit2).complete(),
                ));
                Insert(Popcnt(self.insert_peephole(And(x, m))))
            }
            (Or(..), Popcnt(a), GetBit(x2, bit)) | (Or(..), GetBit(x2, bit), Popcnt(a)) => {
                match &*self[*a] {
                    And(x1, m) if x1 == x2 => match &*self[*m] {
                        Number(m) => {
                            let x = *x1;
                            let m =
                                self.insert(Inst::number(&**m | (Integer::ONE << bit).complete()));
                            Insert(Popcnt(self.insert_peephole(And(x, m))))
                        }
                        _ => New,
                    },
                    _ => New,
                }
            }
            (Or(..), Popcnt(a), Popcnt(b)) => match (&*self[*a], &*self[*b]) {
                (And(x1, m), And(x2, n)) if x1 == x2 => match (&*self[*m], &*self[*n]) {
                    (Number(m), Number(n)) => {
                        let x = *x1;
                        let mn = self.insert(Inst::number(&**m | &**n));
                        Insert(Popcnt(self.insert_peephole(And(x, mn))))
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
            Action::Use(i) => return i,
        }
    }

    fn insert_op2_u32(&mut self, inst: Inst, lhs: NodeRef, rhs: u32) -> NodeRef {
        use Action::*;
        use Inst::*;

        let action = match (&inst, &*self[lhs], rhs) {
            // Constant expressions
            (Shl(..), Number(lhs), _) => Insert(Inst::number(&**lhs << rhs)),
            (Shr(..), Number(lhs), _) => Insert(Inst::number(&**lhs >> rhs)),
            (GetBit(_, bit), Number(lhs), _) => Insert(Inst::number(lhs.get_bit(*bit))),
            (NGetBit(_, bit), Number(lhs), _) => Insert(Inst::number(!lhs.get_bit(*bit))),

            // Identities
            (GetBit(..), GetBit(..) | NGetBit(..), 0) => Use(lhs),
            (NGetBit(..), GetBit(x, b), 0) => Insert(NGetBit(*x, *b)),
            (NGetBit(..), NGetBit(x, b), 0) => Insert(GetBit(*x, *b)),

            // Bit extraction
            (GetBit(..), Shr(v, bit), 0) => Insert(GetBit(*v, *bit)),
            (NGetBit(..), Shr(v, bit), 0) => Insert(NGetBit(*v, *bit)),
            (Shl(..), GetBit(v, bit1), bit) if bit == *bit1 => {
                Insert(And(*v, self.insert(Inst::number(Integer::ONE << bit1))))
            }

            // Negation
            (NGetBit(..), And(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(Nand(*x, *y)), bit))
            }
            (NGetBit(..), Or(x, y), bit) => Insert(GetBit(self.insert_peephole(Nor(*x, *y)), bit)),
            (NGetBit(..), Xor(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(Xnor(*x, *y)), bit))
            }
            (NGetBit(..), AndNot(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(NandNot(*x, *y)), bit))
            }
            (NGetBit(..), NotAnd(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(NNotAnd(*x, *y)), bit))
            }
            (NGetBit(..), Nand(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(And(*x, *y)), bit))
            }
            (NGetBit(..), Nor(x, y), bit) => Insert(GetBit(self.insert_peephole(Or(*x, *y)), bit)),
            (NGetBit(..), Xnor(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(Xor(*x, *y)), bit))
            }
            (NGetBit(..), NandNot(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(AndNot(*x, *y)), bit))
            }
            (NGetBit(..), NNotAnd(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(NotAnd(*x, *y)), bit))
            }

            _ => New,
        };

        match action {
            Action::New => self.insert(inst),
            Action::Insert(inst) => self.insert_peephole(inst),
            Action::Use(i) => return i,
        }
    }

    fn insert_op1(&mut self, inst: Inst, v: NodeRef) -> NodeRef {
        use Action::*;
        use Inst::*;

        let action = match (&inst, &*self[v]) {
            // Constant expressions
            (_, Number(n)) => {
                let n = &**n;
                let r = match inst {
                    Neg(_) => (-n).complete(),
                    Popcnt(_) => n.count_ones().unwrap().into(), // TODO: only valid for n >= 0
                    _ => panic!("not a unary operator: {inst}"),
                };
                Insert(Number(r.into()))
            }

            (_, Error(_)) => Use(v),
            (Neg(_), Neg(v)) => Use(*v),

            _ => New,
        };

        match action {
            Action::New => self.insert(inst),
            Action::Insert(inst) => self.insert_peephole(inst),
            Action::Use(i) => return i,
        }
    }
}
