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
            Node::Shl(lhs, rhs) | Node::Shr(lhs, rhs) => {
                let node = if let Node::Number(lhs) = &self[lhs] {
                    let lhs = &**lhs;
                    match node {
                        Node::Shl(..) => Node::Number((lhs << rhs).complete().into()),
                        Node::Shr(..) => Node::Number((lhs >> rhs).complete().into()),
                        _ => node,
                    }
                } else {
                    node
                };
                self.insert(node)
            }
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

            // Bitwise operations on the LSB

            // AND on LSB
            // x * y == x & y
            (Mul(..), Lsb(_), Lsb(_)) => Insert(And(lhs, rhs)),
            // (x + y) / 2 == x & y
            (Div(..), Add(x, y), Number(rhs)) if **rhs == 2 => {
                match_lsb!(x, y, And(*x, *y))
            }
            // 1 - !(x & y) == x & y
            (Sub(..), Number(one), Nand(x, y)) if **one == 1 => {
                match_lsb!(x, y, And(*x, *y))
            }

            // OR on LSB
            // (x + y) - (x & y) = x | y
            (Sub(..), Add(x, y), And(x2, y2)) if x == x2 && y == y2 || x == y2 && y == x2 => {
                match_lsb!(x, y, Or(*x, *y))
            }
            // x + (y - (x & y)) = x | y
            (Add(..), Lsb(_), Sub(y, z)) => {
                let x = &lhs;
                match &self[*z] {
                    And(x2, y2) if x == x2 && y == y2 || x == y2 && y == x2 => {
                        match_lsb!(x, y, Or(*x, *y))
                    }
                    _ => New,
                }
            }
            // 1 - !(x | y) = !(!x & !y) = x | y
            (Sub(..), Number(one), Nor(x, y)) if **one == 1 => {
                match_lsb!(x, y, Or(*x, *y))
            }

            // XOR on LSB
            // (x + y) % 2 == x ^ y
            (Mod(..), Add(x, y), Number(rhs)) if **rhs == 2 => {
                match_lsb!(x, y, Xor(*x, *y))
            }
            // (x + y) * !(x & y) == x ^ y
            (Mul(..), Add(x, y), Nand(x2, y2)) if x == x2 && y == y2 || x == y2 && y == x2 => {
                match_lsb!(x, y, Xor(*x, *y))
            }
            // 1 - !(x ^ y) == x ^ y
            (Sub(..), Number(one), Xnor(x, y)) if **one == 1 => {
                match_lsb!(x, y, Xor(*x, *y))
            }

            // ANDNOT on LSB
            // x * (1 - y) == x & !y
            (Mul(..), Lsb(_), Sub(one, y)) => match &self[*one] {
                Number(one) if **one == 1 => {
                    match_lsb!(&lhs, y, AndNot(lhs, *y))
                }
                _ => New,
            },
            // (1 - x) * y == !x & y
            (Mul(..), Sub(one, x), Lsb(_)) => match &self[*one] {
                Number(one) if **one == 1 => {
                    match_lsb!(&rhs, x, AndNot(rhs, *x))
                }
                _ => New,
            },
            // 1 - !(x & !y) == x & !y
            (Sub(..), Number(one), NandNot(x, y)) if **one == 1 => {
                match_lsb!(x, y, AndNot(*x, *y))
            }

            // NAND on LSB
            // 1 - (x & y) == !(x & y)
            (Sub(..), Number(one), And(x, y)) if **one == 1 => {
                match_lsb!(x, y, Nand(*x, *y))
            }

            // NOR on LSB
            // 1 - (x | y) == !(x | y)
            (Sub(..), Number(one), Or(x, y)) if **one == 1 => {
                match_lsb!(x, y, Nor(*x, *y))
            }
            // (1 - x) * (1 - y) == !x & !y == !(x | y)
            (Mul(..), Sub(one1, x), Sub(one2, y)) => match (&self[*one1], &self[*one2]) {
                (Number(one1), Number(one2)) if **one1 == 1 && **one2 == 1 => {
                    match_lsb!(x, y, Nor(*x, *y))
                }
                _ => New,
            },
            // (1 - (x + y)) - (x & y) == !(x | y)
            (Sub(..), Sub(a, b), And(x2, y2)) => match (&self[*a], &self[*b]) {
                (Number(one), Add(x, y))
                    if **one == 1 && (x == x2 && y == y2 || x == y2 && y == x2) =>
                {
                    match_lsb!(x, y, Nor(*x, *y))
                }
                _ => New,
            },

            // XNOR on LSB
            // 1 - (x ^ y) == !(x ^ y)
            (Sub(..), Number(one), Xor(x, y)) if **one == 1 => {
                match_lsb!(x, y, Xnor(*x, *y))
            }

            // NANDNOT on LSB
            // 1 - (x & !y) == !(x & !y)
            (Sub(..), Number(one), AndNot(x, y)) if **one == 1 => {
                match_lsb!(x, y, NandNot(*x, *y))
            }

            // LSB
            (Mod(..), _, Number(rhs)) if **rhs == 2 => Insert(Lsb(lhs)),

            // Bitwise operations on more bits

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

        match action {
            Action::New => self.insert(node),
            Action::Insert(node) => self.insert_peephole(node),
            Action::Use(i) => return i,
        }
    }
}
