use std::cmp::Ordering;
use std::rc::Rc;

use rug::ops::{DivRounding, RemRounding};
use rug::{Complete, Integer};

use crate::error::NumberError;
use crate::ir::{Node, NodeOp1, NodeOp2, NodeOp2U32, NodeRef, NodeTable};

include!(concat!(env!("OUT_DIR"), "/rewrites.rs"));

enum Action {
    New,
    Insert(Node),
    Use(NodeRef),
}

impl NodeTable<'_> {
    pub fn insert_peephole(&mut self, node: Node) -> NodeRef {
        match node {
            NodeOp2!(lhs, rhs) => self.insert_op2(node, lhs, rhs),
            NodeOp2U32!(lhs, rhs) => self.insert_op2_u32(node, lhs, rhs),
            NodeOp1!(v) => self.insert_op1(node, v),
            _ => self.insert(node),
        }
    }

    fn insert_op2(&mut self, node: Node, lhs: NodeRef, rhs: NodeRef) -> NodeRef {
        use Action::*;
        use Node::*;

        macro_rules! getbit_op(
            ($Node:ident ($x:expr, $y:expr)) => {
                match (&self[*$x], &self[*$y]) {
                    (GetBit(x, bx), GetBit(y, by)) if bx == by => {
                        let b = *bx;
                        Insert(GetBit(self.insert_peephole($Node(*x, *y)), b))
                    }
                    _ => New,
                }
            };
            ($Node:ident ($x:expr, $y:expr), $b:expr) => {
                {
                    let b = *$b;
                    Insert(GetBit(self.insert_peephole($Node(*$x, *$y)), b))
                }
            };
        );

        let action = match (&node, &self[lhs], &self[rhs]) {
            // Constant expressions
            (_, Number(lhs), Number(rhs)) => {
                let (lhs, rhs) = (lhs.as_ref(), rhs.as_ref());
                let res = match &node {
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
                    _ => panic!("not a binary operator: {node}"),
                };
                let node = match res {
                    Ok(r) => Number(Rc::new(r)),
                    Err(err) => Error(err),
                };
                Insert(node)
            }

            // Division by 0
            (Div(..) | Mod(..), _, Number(rhs)) if rhs.cmp0() == Ordering::Equal => {
                Insert(Error(NumberError::DivModZero))
            }

            // Errors
            (Add(..) | Sub(..) | Mul(..) | Div(..) | Mod(..), _, Error(_)) => Use(rhs),
            (Add(..) | Sub(..) | Mul(..) | Div(..) | Mod(..), Error(_), Number(_)) => Use(lhs),

            // Identities
            (Add(..), Number(lhs), _) if lhs.cmp0() == Ordering::Equal => Use(rhs),
            (Add(..) | Sub(..), _, Number(rhs)) if rhs.cmp0() == Ordering::Equal => Use(lhs),
            (Mul(..), Number(lhs), _) if **lhs == 1 => Use(rhs),
            (Mul(..) | Div(..), _, Number(rhs)) if **rhs == 1 => Use(lhs),

            // Negation
            (Sub(..), Number(lhs), _) if lhs.cmp0() == Ordering::Equal => Insert(Neg(rhs)),
            (Add(..), _, Neg(rhs)) => Insert(Sub(lhs, *rhs)),
            (Sub(..), _, Neg(rhs)) => Insert(Add(lhs, *rhs)),

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
                match &self[*z] {
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
            (Mul(..), GetBit(x, bx), NotGetBit(y, by)) if bx == by => {
                getbit_op!(AndNot(x, y), bx)
            }

            // Single-bit NOTAND
            // !x * y == !x & y
            (Mul(..), NotGetBit(x, bx), GetBit(y, by)) if bx == by => {
                getbit_op!(NotAnd(x, y), bx)
            }

            // Single-bit NOR
            // !x * !y == !x & !y == !(x | y)
            (Mul(..), NotGetBit(x, bx), NotGetBit(y, by)) if bx == by => {
                getbit_op!(Nor(x, y), bx)
            }

            // Negation
            (Sub(..), Number(one), GetBit(x, b)) if **one == 1 => Insert(NotGetBit(*x, *b)),
            (Sub(..), Number(one), NotGetBit(x, b)) if **one == 1 => Insert(GetBit(*x, *b)),

            // Distributive
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
            (And(..), NotGetBit(x, bx), NotGetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(NotGetBit(self.insert_peephole(And(*x, *y)), bits))
            }
            (Or(..), NotGetBit(x, bx), NotGetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(NotGetBit(self.insert_peephole(Or(*x, *y)), bits))
            }
            (Xor(..), NotGetBit(x, bx), NotGetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(NotGetBit(self.insert_peephole(Xor(*x, *y)), bits))
            }
            (AndNot(..), NotGetBit(x, bx), NotGetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(NotGetBit(self.insert_peephole(AndNot(*x, *y)), bits))
            }
            (Nand(..), NotGetBit(x, bx), NotGetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(NotGetBit(self.insert_peephole(Nand(*x, *y)), bits))
            }
            (Nor(..), NotGetBit(x, bx), NotGetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(NotGetBit(self.insert_peephole(Nor(*x, *y)), bits))
            }
            (Xnor(..), NotGetBit(x, bx), NotGetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(NotGetBit(self.insert_peephole(Xnor(*x, *y)), bits))
            }
            (NandNot(..), NotGetBit(x, bx), NotGetBit(y, by)) if bx == by => {
                let bits = *bx;
                Insert(NotGetBit(self.insert_peephole(NandNot(*x, *y)), bits))
            }

            // Get LSB
            (And(..), _, Number(rhs)) if **rhs == 1 => Insert(GetBit(lhs, 0)),
            (Mod(..), _, Number(rhs)) if **rhs == 2 => Insert(GetBit(lhs, 0)),

            // Bitwise shifts
            (Mul(..), _, Number(rhs)) if rhs.to_u32().is_some_and(|r| r.is_power_of_two()) => {
                Insert(Shl(lhs, rhs.to_u32().unwrap().ilog2()))
            }
            (Mul(..), Number(lhs), _) if lhs.to_u32().is_some_and(|r| r.is_power_of_two()) => {
                Insert(Shl(rhs, lhs.to_u32().unwrap().ilog2()))
            }
            (Div(..), _, Number(rhs)) if rhs.to_u32().is_some_and(|r| r.is_power_of_two()) => {
                Insert(Shr(lhs, rhs.to_u32().unwrap().ilog2()))
            }

            // Multi-bit AND
            (Mod(..), _, Number(rhs)) if rhs.is_power_of_two() => {
                Insert(And(lhs, self.insert(Node::number(&**rhs - 1))))
            }
            (Add(..), And(x1, m1), And(x2, m2)) if x1 == x2 => match (&self[*m1], &self[*m2]) {
                (Number(m1), Number(m2))
                    if (&**m1 & &**m2).complete().cmp0() == Ordering::Equal =>
                {
                    Insert(And(*x1, self.insert(Node::number(&**m1 | &**m2))))
                }
                _ => New,
            },
            (Add(..), GetBit(x1, 0), And(x2, m)) | (Add(..), And(x2, m), GetBit(x1, 0))
                if x1 == x2 =>
            {
                match &self[*m] {
                    Number(m) if m.cmp0() != Ordering::Equal => {
                        Insert(And(*x1, self.insert(Node::number(&**m | 1))))
                    }
                    _ => New,
                }
            }

            // Popcnt
            (Add(..), GetBit(x1, bit1), GetBit(x2, bit2)) if x1 == x2 && bit1 != bit2 => {
                let x = *x1;
                let m = self.insert(Node::number(
                    (Integer::ONE << bit1).complete() | (Integer::ONE << bit2).complete(),
                ));
                Insert(Popcnt(self.insert_peephole(And(x, m))))
            }
            (Add(..), Popcnt(a), GetBit(x2, bit)) => match &self[*a] {
                And(x1, m) if x1 == x2 => match &self[*m] {
                    Number(m) if !m.get_bit(*bit) => {
                        let x = *x1;
                        let m = self.insert(Node::number(&**m | (Integer::ONE << bit).complete()));
                        Insert(Popcnt(self.insert_peephole(And(x, m))))
                    }
                    _ => New,
                },
                _ => New,
            },

            _ => New,
        };

        match action {
            Action::New => self.insert(node),
            Action::Insert(node) => self.insert_peephole(node),
            Action::Use(i) => return i,
        }
    }

    fn insert_op2_u32(&mut self, node: Node, lhs: NodeRef, rhs: u32) -> NodeRef {
        use Action::*;
        use Node::*;

        let action = match (&node, &self[lhs], rhs) {
            // Constant expressions
            (Shl(..), Number(lhs), _) => Insert(Node::number(&**lhs << rhs)),
            (Shr(..), Number(lhs), _) => Insert(Node::number(&**lhs >> rhs)),
            (GetBit(_, bit), Number(lhs), _) => Insert(Node::number(lhs.get_bit(*bit))),
            (NotGetBit(_, bit), Number(lhs), _) => Insert(Node::number(!lhs.get_bit(*bit))),

            // Identities
            (GetBit(..), GetBit(..) | NotGetBit(..), 0) => Use(lhs),
            (NotGetBit(..), GetBit(x, b), 0) => Insert(NotGetBit(*x, *b)),
            (NotGetBit(..), NotGetBit(x, b), 0) => Insert(GetBit(*x, *b)),

            // Bit extraction
            (GetBit(..), Shr(v, bit), 0) => Insert(GetBit(*v, *bit)),
            (NotGetBit(..), Shr(v, bit), 0) => Insert(NotGetBit(*v, *bit)),
            (Shl(..), GetBit(v, bit1), bit) if bit == *bit1 => {
                Insert(And(*v, self.insert(Node::number(Integer::ONE << bit1))))
            }

            // Negation
            (NotGetBit(..), And(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(Nand(*x, *y)), bit))
            }
            (NotGetBit(..), Or(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(Nor(*x, *y)), bit))
            }
            (NotGetBit(..), Xor(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(Xnor(*x, *y)), bit))
            }
            (NotGetBit(..), AndNot(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(NandNot(*x, *y)), bit))
            }
            (NotGetBit(..), NotAnd(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(NNotAnd(*x, *y)), bit))
            }
            (NotGetBit(..), Nand(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(And(*x, *y)), bit))
            }
            (NotGetBit(..), Nor(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(Or(*x, *y)), bit))
            }
            (NotGetBit(..), Xnor(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(Xor(*x, *y)), bit))
            }
            (NotGetBit(..), NandNot(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(AndNot(*x, *y)), bit))
            }
            (NotGetBit(..), NNotAnd(x, y), bit) => {
                Insert(GetBit(self.insert_peephole(NotAnd(*x, *y)), bit))
            }

            _ => New,
        };

        match action {
            Action::New => self.insert(node),
            Action::Insert(node) => self.insert_peephole(node),
            Action::Use(i) => return i,
        }
    }

    fn insert_op1(&mut self, node: Node, v: NodeRef) -> NodeRef {
        use Action::*;
        use Node::*;

        let action = match (&node, &self[v]) {
            // Constant expressions
            (_, Number(n)) => {
                let n = &**n;
                let r = match node {
                    Neg(_) => (-n).complete(),
                    Popcnt(_) => n.count_ones().unwrap().into(), // TODO: only valid for n >= 0
                    _ => panic!("not a unary operator: {node}"),
                };
                Insert(Number(r.into()))
            }

            (_, Error(_)) => Use(v),
            (Neg(_), Neg(v)) => Use(*v),

            _ => New,
        };

        match action {
            Action::New => self.insert(node),
            Action::Insert(node) => self.insert_peephole(node),
            Action::Use(i) => return i,
        }
    }
}
