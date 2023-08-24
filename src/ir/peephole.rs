use std::cmp::Ordering;
use std::rc::Rc;

use rug::ops::{DivRounding, RemRounding};
use rug::Complete;

use crate::error::NumberError;
use crate::ir::{Node, NodeRef, NodeTable, Op1, Op2};

impl NodeTable<'_> {
    pub fn insert_peephole(&mut self, node: Node) -> NodeRef {
        match node {
            Node::Op2(op, lhs, rhs) => self.insert_op2(op, lhs, rhs),
            Node::Op1(op, v) => self.insert_op1(op, v),
            _ => self.insert(node),
        }
    }

    fn insert_op2(&mut self, op: Op2, lhs: NodeRef, rhs: NodeRef) -> NodeRef {
        use self::Op1::*;
        use self::Op2::*;
        use Action::*;
        use Node::*;

        enum Action {
            New,
            Insert(Node),
            Use(NodeRef),
        }

        macro_rules! match_lsb(($x:expr, $y:expr, $node:expr) => {
            match (&self[*$x], &self[*$y]) {
                (Op1(Lsb, _), Op1(Lsb, _)) => Insert($node),
                _ => New,
            }
        });

        let action = match (op, &self[lhs], &self[rhs]) {
            // Constant expressions
            (_, Number(lhs), Number(rhs)) => {
                let (lhs, rhs) = (lhs.as_ref(), rhs.as_ref());
                let res = match op {
                    Add => Ok((lhs + rhs).complete()),
                    Sub => Ok((lhs - rhs).complete()),
                    Mul => Ok((lhs * rhs).complete()),
                    Div if rhs.cmp0() == Ordering::Equal => Err(NumberError::DivModZero),
                    Div => Ok(lhs.div_floor(rhs).into()),
                    Mod if rhs.cmp0() == Ordering::Equal => Err(NumberError::DivModZero),
                    Mod => Ok(lhs.rem_floor(rhs).into()),
                    And => Ok((lhs & rhs).complete()),
                    Or => Ok((lhs | rhs).complete()),
                    Xor => Ok((lhs ^ rhs).complete()),
                    AndNot => Ok(lhs & (!rhs).complete()),
                    Nand => Ok(!(lhs & rhs).complete()),
                    Nor => Ok(!(lhs | rhs).complete()),
                    Xnor => Ok(!(lhs ^ rhs).complete()),
                    NandNot => Ok(!(lhs & (!rhs).complete())),
                    Shl => Ok((lhs << rhs.to_usize().unwrap()).complete()), // TODO: handle overflow
                    Shr => Ok((lhs >> rhs.to_usize().unwrap()).complete()), // TODO: handle overflow
                };
                let node = match res {
                    Ok(r) => Number(Rc::new(r)),
                    Err(err) => Error(err),
                };
                Insert(node)
            }

            // Division by 0
            (Div | Mod, _, Number(rhs)) if rhs.cmp0() == Ordering::Equal => {
                Insert(Error(NumberError::DivModZero))
            }

            // Errors
            (Add | Sub | Div | Mod, _, Error(_)) => Use(rhs),
            (Add | Sub | Div | Mod, Error(_), Number(_)) => Use(lhs),
            (Mul, Error(_), _) => Use(lhs),
            (Mul, Number(_), Error(_)) => Use(rhs),

            // Identities
            (Add, Number(lhs), _) if lhs.cmp0() == Ordering::Equal => Use(rhs),
            (Add | Sub, _, Number(rhs)) if rhs.cmp0() == Ordering::Equal => Use(lhs),
            (Mul | Div, Number(lhs), _) if **lhs == 1 => Use(rhs),
            (Mul | Div, _, Number(rhs)) if **rhs == 1 => Use(lhs),

            // Bitwise operations on the LSB

            // AND on LSB
            // x * y == x & y
            (Mul, Op1(Lsb, _), Op1(Lsb, _)) => Insert(Op2(And, lhs, rhs)),
            // (x + y) / 2 == x & y
            (Div, Op2(Add, x, y), Number(rhs)) if **rhs == 2 => {
                match_lsb!(x, y, Op2(And, *x, *y))
            }
            // 1 - !(x & y) == x & y
            (Sub, Number(one), Op2(Nand, x, y)) if **one == 1 => {
                match_lsb!(x, y, Op2(And, *x, *y))
            }

            // OR on LSB
            // (x + y) - (x * y) = x | y
            (Sub, Op2(Add, x, y), Op2(Mul | And, x2, y2))
                if x == x2 && y == y2 || x == y2 && y == x2 =>
            {
                match_lsb!(x, y, Op2(Or, *x, *y))
            }
            // x + (y - (x * y)) = x | y
            (Add, Op1(Lsb, _), Op2(Sub, y, z)) => {
                let x = &lhs;
                match &self[*z] {
                    Op2(Mul | And, x2, y2) if x == x2 && y == y2 || x == y2 && y == x2 => {
                        match_lsb!(x, y, Op2(Or, *x, *y))
                    }
                    _ => New,
                }
            }
            // 1 - !(x | y) = !(!x & !y) = x | y
            (Sub, Number(one), Op2(Nor, x, y)) if **one == 1 => {
                match_lsb!(x, y, Op2(Or, *x, *y))
            }

            // XOR on LSB
            // (x + y) % 2 == x ^ y
            (Mod, Op2(Add, x, y), Number(rhs)) if **rhs == 2 => {
                match_lsb!(x, y, Op2(Xor, *x, *y))
            }
            // (x + y) * !(x & y) == x ^ y
            (Mul, Op2(Add, x, y), Op2(Nand, x2, y2))
                if x == x2 && y == y2 || x == y2 && y == x2 =>
            {
                match_lsb!(x, y, Op2(Xor, *x, *y))
            }
            // 1 - !(x ^ y) == x ^ y
            (Sub, Number(one), Op2(Xnor, x, y)) if **one == 1 => {
                match_lsb!(x, y, Op2(Xor, *x, *y))
            }

            // ANDNOT on LSB
            // x * (1 - y) == x & !y
            (Mul, Op1(Lsb, _), Op2(Sub, one, y)) => match &self[*one] {
                Number(one) if **one == 1 => {
                    match_lsb!(&lhs, y, Op2(AndNot, lhs, *y))
                }
                _ => New,
            },
            // (1 - x) * y == !x & y
            (Mul, Op2(Sub, one, x), Op1(Lsb, _)) => match &self[*one] {
                Number(one) if **one == 1 => {
                    match_lsb!(&rhs, x, Op2(AndNot, rhs, *x))
                }
                _ => New,
            },
            // 1 - !(x & !y) == x & !y
            (Sub, Number(one), Op2(NandNot, x, y)) if **one == 1 => {
                match_lsb!(x, y, Op2(AndNot, *x, *y))
            }

            // NAND on LSB
            // 1 - (x & y) == !(x & y)
            (Sub, Number(one), Op2(And, x, y)) if **one == 1 => {
                match_lsb!(x, y, Op2(Nand, *x, *y))
            }

            // NOR on LSB
            // 1 - (x | y) == !(x | y)
            (Sub, Number(one), Op2(Or, x, y)) if **one == 1 => {
                match_lsb!(x, y, Op2(Nor, *x, *y))
            }
            // (1 - x) * (1 - y) == !x & !y == !(x | y)
            (Mul, Op2(Sub, one1, x), Op2(Sub, one2, y)) => match (&self[*one1], &self[*one2]) {
                (Number(one1), Number(one2)) if **one1 == 1 && **one2 == 1 => {
                    match_lsb!(x, y, Op2(Nor, *x, *y))
                }
                _ => New,
            },
            // (1 - (x + y)) - (x * y) == !(x | y)
            (Sub, Op2(Sub, a, b), Op2(Mul, x2, y2)) => match (&self[*a], &self[*b]) {
                (Number(one), Op2(Add, x, y))
                    if **one == 1 && (x == x2 && y == y2 || x == y2 && y == x2) =>
                {
                    match_lsb!(x, y, Op2(Nor, *x, *y))
                }
                _ => New,
            },

            // XNOR on LSB
            // 1 - (x ^ y) == !(x ^ y)
            (Sub, Number(one), Op2(Xor, x, y)) if **one == 1 => {
                match_lsb!(x, y, Op2(Xnor, *x, *y))
            }

            // NANDNOT on LSB
            // 1 - (x & !y) == !(x & !y)
            (Sub, Number(one), Op2(AndNot, x, y)) if **one == 1 => {
                match_lsb!(x, y, Op2(NandNot, *x, *y))
            }

            // LSB
            (Mod, _, Number(rhs)) if **rhs == 2 => Insert(Op1(Lsb, lhs)),

            _ => New,
        };

        let node = match action {
            Action::New => Node::Op2(op, lhs, rhs),
            Action::Insert(node) => node,
            Action::Use(i) => return i,
        };
        self.insert(node)
    }

    fn insert_op1(&mut self, op: Op1, v: NodeRef) -> NodeRef {
        match (op, &self[v]) {
            // Constant expressions
            (_, Node::Number(n)) => {
                let n = &**n;
                let r = match op {
                    Op1::Neg => (-n).complete(),
                    Op1::Popcnt => n.count_ones().unwrap().into(), // TODO: only valid for n >= 0
                    Op1::Lsb => n.get_bit(0).into(),
                };
                self.insert(Node::Number(r.into()))
            }
            // Errors
            (_, Node::Error(_)) => v,
            // Inverse identities
            (Op1::Neg, Node::Op1(Op1::Neg, v)) => *v,
            (Op1::Lsb, Node::Op1(Op1::Lsb, v)) => *v,
            _ => self.insert(Node::Op1(op, v)),
        }
    }
}
