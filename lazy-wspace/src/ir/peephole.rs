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

        macro_rules! is_getbit(($x:expr, $y:expr, $node:expr) => {
            match (&self[*$x], &self[*$y]) {
                (GetBit(_, bx), GetBit(_, by)) if bx == by => Insert($node),
                _ => New,
            }
        });

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
            (Add(..) | Sub(..) | Div(..) | Mod(..), _, Error(_)) => Use(rhs),
            (Add(..) | Sub(..) | Div(..) | Mod(..), Error(_), Number(_)) => Use(lhs),
            (Mul(..), Error(_), _) => Use(lhs),
            (Mul(..), Number(_), Error(_)) => Use(rhs),

            // Identities
            (Add(..), Number(lhs), _) if lhs.cmp0() == Ordering::Equal => Use(rhs),
            (Add(..) | Sub(..), _, Number(rhs)) if rhs.cmp0() == Ordering::Equal => Use(lhs),
            (Mul(..) | Div(..), Number(lhs), _) if **lhs == 1 => Use(rhs),
            (Mul(..) | Div(..), _, Number(rhs)) if **rhs == 1 => Use(lhs),

            // Single-bit AND
            // x * y == x & y
            (Mul(..), GetBit(_, bx), GetBit(_, by)) if bx == by => Insert(And(lhs, rhs)),
            // (x + y) / 2 == x & y
            (Div(..), Add(x, y), Number(rhs)) if **rhs == 2 => {
                is_getbit!(x, y, And(*x, *y))
            }

            // Single-bit OR
            // (x + y) - (x & y) = x | y
            (Sub(..), Add(x, y), And(x2, y2)) if x == x2 && y == y2 || x == y2 && y == x2 => {
                is_getbit!(x, y, Or(*x, *y))
            }
            // x + (y - (x & y)) = x | y
            (Add(..), GetBit(..), Sub(y, z)) => {
                let x = &lhs;
                match &self[*z] {
                    And(x2, y2) if x == x2 && y == y2 || x == y2 && y == x2 => {
                        is_getbit!(x, y, Or(*x, *y))
                    }
                    _ => New,
                }
            }

            // Single-bit XOR
            // (x + y) % 2 == x ^ y
            (Mod(..), Add(x, y), Number(rhs)) if **rhs == 2 => {
                is_getbit!(x, y, Xor(*x, *y))
            }
            // (x + y) * !(x & y) == x ^ y
            (Mul(..), Add(x, y), Nand(x2, y2)) if x == x2 && y == y2 || x == y2 && y == x2 => {
                is_getbit!(x, y, Xor(*x, *y))
            }

            // Single-bit ANDNOT
            // x * !y == x & !y
            (Mul(..), GetBit(_, bx), NotGetBit(y, by)) if bx == by => {
                Insert(AndNot(lhs, self.insert_peephole(Node::GetBit(*y, *by))))
            }
            // !x * y == y & !x
            (Mul(..), NotGetBit(x, bx), GetBit(_, by)) if bx == by => {
                Insert(AndNot(rhs, self.insert_peephole(Node::GetBit(*x, *bx))))
            }

            // Single-bit NOR
            // !x * !y == !x & !y == !(x | y)
            (Mul(..), NotGetBit(x, bx), NotGetBit(y, by)) if bx == by => {
                let (x, y, bits) = (*x, *y, *bx);
                let x = self.insert_peephole(Node::GetBit(x, bits));
                let y = self.insert_peephole(Node::GetBit(y, bits));
                Insert(Nor(x, y))
            }

            // Negation
            (Sub(..), Number(one), And(x, y)) if **one == 1 => {
                is_getbit!(x, y, Nand(*x, *y))
            }
            (Sub(..), Number(one), Or(x, y)) if **one == 1 => {
                is_getbit!(x, y, Nor(*x, *y))
            }
            (Sub(..), Number(one), Xor(x, y)) if **one == 1 => {
                is_getbit!(x, y, Xnor(*x, *y))
            }
            (Sub(..), Number(one), AndNot(x, y)) if **one == 1 => {
                is_getbit!(x, y, NandNot(*x, *y))
            }
            (Sub(..), Number(one), GetBit(v, bit)) if **one == 1 => Insert(NotGetBit(*v, *bit)),
            (Sub(..), Number(one), Nand(x, y)) if **one == 1 => {
                is_getbit!(x, y, And(*x, *y))
            }
            (Sub(..), Number(one), Nor(x, y)) if **one == 1 => {
                is_getbit!(x, y, Or(*x, *y))
            }
            (Sub(..), Number(one), Xnor(x, y)) if **one == 1 => {
                is_getbit!(x, y, Xor(*x, *y))
            }
            (Sub(..), Number(one), NandNot(x, y)) if **one == 1 => {
                is_getbit!(x, y, AndNot(*x, *y))
            }
            (Sub(..), Number(one), NotGetBit(v, bit)) if **one == 1 => Insert(GetBit(*v, *bit)),

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

        let action = match (&node, &self[lhs]) {
            // Constant expressions
            (Shl(..), Number(lhs)) => Insert(Node::number(&**lhs << rhs)),
            (Shr(..), Number(lhs)) => Insert(Node::number(&**lhs >> rhs)),

            (GetBit(_, bit), Number(lhs)) => Insert(Node::number(lhs.get_bit(*bit))),
            (GetBit(_, 0), GetBit(..)) => Use(lhs),
            (GetBit(_, 0), Shr(v, bit)) => Insert(Node::GetBit(*v, *bit)),
            (Shl(_, bit), GetBit(v, bit1)) if bit == bit1 => {
                Insert(And(*v, self.insert(Node::number(Integer::ONE << bit1))))
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
