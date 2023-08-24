use std::cmp::Ordering;
use std::rc::Rc;

use rug::ops::{DivRounding, RemRounding};
use rug::Complete;

use crate::error::NumberError;
use crate::ir::{Node, NodeOp1, NodeOp2, NodeRef, NodeTable};

enum Action {
    New,
    Insert(Node),
    Use(NodeRef),
}

impl NodeTable<'_> {
    pub fn insert_peephole(&mut self, node: Node) -> NodeRef {
        match node {
            NodeOp2!(lhs, rhs) => self.insert_op2(node, lhs, rhs),
            NodeOp1!(v) => self.insert_op1(node, v),
            _ => self.insert(node),
        }
    }

    fn insert_op2(&mut self, node: Node, lhs: NodeRef, rhs: NodeRef) -> NodeRef {
        use Action::*;
        use Node::*;

        macro_rules! match_lsb(($x:expr, $y:expr, $node:expr) => {
            match (&self[*$x], &self[*$y]) {
                (Lsb(_), Lsb(_)) => Insert($node),
                _ => New,
            }
        });

        let action = match (&node, &self[lhs], &self[rhs]) {
            // Constant expressions
            (_, Number(lhs), Number(rhs)) => {
                let (lhs, rhs) = (lhs.as_ref(), rhs.as_ref());
                let res = match &node {
                    Add(_, _) => Ok((lhs + rhs).complete()),
                    Sub(_, _) => Ok((lhs - rhs).complete()),
                    Mul(_, _) => Ok((lhs * rhs).complete()),
                    Div(_, _) if rhs.cmp0() == Ordering::Equal => Err(NumberError::DivModZero),
                    Div(_, _) => Ok(lhs.div_floor(rhs).into()),
                    Mod(_, _) if rhs.cmp0() == Ordering::Equal => Err(NumberError::DivModZero),
                    Mod(_, _) => Ok(lhs.rem_floor(rhs).into()),
                    And(_, _) => Ok((lhs & rhs).complete()),
                    Or(_, _) => Ok((lhs | rhs).complete()),
                    Xor(_, _) => Ok((lhs ^ rhs).complete()),
                    AndNot(_, _) => Ok(lhs & (!rhs).complete()),
                    Nand(_, _) => Ok(!(lhs & rhs).complete()),
                    Nor(_, _) => Ok(!(lhs | rhs).complete()),
                    Xnor(_, _) => Ok(!(lhs ^ rhs).complete()),
                    NandNot(_, _) => Ok(!(lhs & (!rhs).complete())),
                    Shl(_, _) => Ok((lhs << rhs.to_usize().unwrap()).complete()), // TODO: handle overflow
                    Shr(_, _) => Ok((lhs >> rhs.to_usize().unwrap()).complete()), // TODO: handle overflow
                    _ => panic!("not a binary operator: {node}"),
                };
                let node = match res {
                    Ok(r) => Number(Rc::new(r)),
                    Err(err) => Error(err),
                };
                Insert(node)
            }

            // Division by 0
            (Div(_, _) | Mod(_, _), _, Number(rhs)) if rhs.cmp0() == Ordering::Equal => {
                Insert(Error(NumberError::DivModZero))
            }

            // Errors
            (Add(_, _) | Sub(_, _) | Div(_, _) | Mod(_, _), _, Error(_)) => Use(rhs),
            (Add(_, _) | Sub(_, _) | Div(_, _) | Mod(_, _), Error(_), Number(_)) => Use(lhs),
            (Mul(_, _), Error(_), _) => Use(lhs),
            (Mul(_, _), Number(_), Error(_)) => Use(rhs),

            // Identities
            (Add(_, _), Number(lhs), _) if lhs.cmp0() == Ordering::Equal => Use(rhs),
            (Add(_, _) | Sub(_, _), _, Number(rhs)) if rhs.cmp0() == Ordering::Equal => Use(lhs),
            (Mul(_, _) | Div(_, _), Number(lhs), _) if **lhs == 1 => Use(rhs),
            (Mul(_, _) | Div(_, _), _, Number(rhs)) if **rhs == 1 => Use(lhs),

            // Bitwise operations on the LSB

            // AND on LSB
            // x * y == x & y
            (Mul(_, _), Lsb(_), Lsb(_)) => Insert(And(lhs, rhs)),
            // (x + y) / 2 == x & y
            (Div(_, _), Add(x, y), Number(rhs)) if **rhs == 2 => {
                match_lsb!(x, y, And(*x, *y))
            }
            // 1 - !(x & y) == x & y
            (Sub(_, _), Number(one), Nand(x, y)) if **one == 1 => {
                match_lsb!(x, y, And(*x, *y))
            }

            // OR on LSB
            // (x + y) - (x & y) = x | y
            (Sub(_, _), Add(x, y), And(x2, y2)) if x == x2 && y == y2 || x == y2 && y == x2 => {
                match_lsb!(x, y, Or(*x, *y))
            }
            // x + (y - (x & y)) = x | y
            (Add(_, _), Lsb(_), Sub(y, z)) => {
                let x = &lhs;
                match &self[*z] {
                    And(x2, y2) if x == x2 && y == y2 || x == y2 && y == x2 => {
                        match_lsb!(x, y, Or(*x, *y))
                    }
                    _ => New,
                }
            }
            // 1 - !(x | y) = !(!x & !y) = x | y
            (Sub(_, _), Number(one), Nor(x, y)) if **one == 1 => {
                match_lsb!(x, y, Or(*x, *y))
            }

            // XOR on LSB
            // (x + y) % 2 == x ^ y
            (Mod(_, _), Add(x, y), Number(rhs)) if **rhs == 2 => {
                match_lsb!(x, y, Xor(*x, *y))
            }
            // (x + y) * !(x & y) == x ^ y
            (Mul(_, _), Add(x, y), Nand(x2, y2)) if x == x2 && y == y2 || x == y2 && y == x2 => {
                match_lsb!(x, y, Xor(*x, *y))
            }
            // 1 - !(x ^ y) == x ^ y
            (Sub(_, _), Number(one), Xnor(x, y)) if **one == 1 => {
                match_lsb!(x, y, Xor(*x, *y))
            }

            // ANDNOT on LSB
            // x * (1 - y) == x & !y
            (Mul(_, _), Lsb(_), Sub(one, y)) => match &self[*one] {
                Number(one) if **one == 1 => {
                    match_lsb!(&lhs, y, AndNot(lhs, *y))
                }
                _ => New,
            },
            // (1 - x) * y == !x & y
            (Mul(_, _), Sub(one, x), Lsb(_)) => match &self[*one] {
                Number(one) if **one == 1 => {
                    match_lsb!(&rhs, x, AndNot(rhs, *x))
                }
                _ => New,
            },
            // 1 - !(x & !y) == x & !y
            (Sub(_, _), Number(one), NandNot(x, y)) if **one == 1 => {
                match_lsb!(x, y, AndNot(*x, *y))
            }

            // NAND on LSB
            // 1 - (x & y) == !(x & y)
            (Sub(_, _), Number(one), And(x, y)) if **one == 1 => {
                match_lsb!(x, y, Nand(*x, *y))
            }

            // NOR on LSB
            // 1 - (x | y) == !(x | y)
            (Sub(_, _), Number(one), Or(x, y)) if **one == 1 => {
                match_lsb!(x, y, Nor(*x, *y))
            }
            // (1 - x) * (1 - y) == !x & !y == !(x | y)
            (Mul(_, _), Sub(one1, x), Sub(one2, y)) => match (&self[*one1], &self[*one2]) {
                (Number(one1), Number(one2)) if **one1 == 1 && **one2 == 1 => {
                    match_lsb!(x, y, Nor(*x, *y))
                }
                _ => New,
            },
            // (1 - (x + y)) - (x & y) == !(x | y)
            (Sub(_, _), Sub(a, b), And(x2, y2)) => match (&self[*a], &self[*b]) {
                (Number(one), Add(x, y))
                    if **one == 1 && (x == x2 && y == y2 || x == y2 && y == x2) =>
                {
                    match_lsb!(x, y, Nor(*x, *y))
                }
                _ => New,
            },

            // XNOR on LSB
            // 1 - (x ^ y) == !(x ^ y)
            (Sub(_, _), Number(one), Xor(x, y)) if **one == 1 => {
                match_lsb!(x, y, Xnor(*x, *y))
            }

            // NANDNOT on LSB
            // 1 - (x & !y) == !(x & !y)
            (Sub(_, _), Number(one), AndNot(x, y)) if **one == 1 => {
                match_lsb!(x, y, NandNot(*x, *y))
            }

            // LSB
            (Mod(_, _), _, Number(rhs)) if **rhs == 2 => Insert(Lsb(lhs)),

            _ => New,
        };

        let node = match action {
            Action::New => node,
            Action::Insert(node) => node,
            Action::Use(i) => return i,
        };
        self.insert(node)
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
                    Lsb(_) => n.get_bit(0).into(),
                    _ => panic!("not a unary operator: {node}"),
                };
                Insert(Number(r.into()))
            }
            // Errors
            (_, Error(_)) => Use(v),
            // Inverse identities
            (Neg(_), Neg(v)) => Use(*v),
            (Lsb(_), Lsb(v)) => Use(*v),
            _ => New,
        };

        let node = match action {
            Action::New => node,
            Action::Insert(node) => node,
            Action::Use(i) => return i,
        };
        self.insert(node)
    }
}
