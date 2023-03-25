use std::collections::HashMap;

use crate::ast::Inst;
use crate::error::ParseError;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cfg {
    blocks: Vec<BasicBlock>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BasicBlock {
    insts: Vec<Inst>,
    term: TermInst,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TermInst {
    Call(usize, usize),
    Jmp(usize),
    Jz(usize, usize),
    Jn(usize, usize),
    Ret,
    End,
    Error(ParseError),
}

impl Cfg {
    pub fn new(prog: &[Inst]) -> Self {
        let mut first_error = prog.len();
        for (pc, inst) in prog.iter().enumerate() {
            match inst {
                Inst::ParseError(_) => {
                    first_error = pc;
                    break;
                }
                _ => {}
            }
        }
        // Ignore any instructions after the first parse error, to avoid jumping
        // past it.
        let prog = &prog[..first_error];

        let mut labels = HashMap::new();
        let mut block_count = 0;
        let mut first_label = true; // First label in a block
        for inst in prog {
            if let Inst::Label(l) = inst {
                labels.insert(l.clone(), block_count);
                if first_label {
                    block_count += 1;
                }
                first_label = false;
            } else {
                first_label = true;
            }
        }
        macro_rules! get_label(($l:expr) => {
            labels.get($l).copied().unwrap_or(block_count)
        });

        let mut blocks = Vec::with_capacity(block_count);
        let mut curr_block = Vec::new();
        for (pc, inst) in prog.iter().enumerate() {
            let term = match inst {
                Inst::Call(l) => TermInst::Call(get_label!(l), pc + 1),
                Inst::Jmp(l) => TermInst::Jmp(get_label!(l)),
                Inst::Jz(l) => TermInst::Jz(get_label!(l), pc + 1),
                Inst::Jn(l) => TermInst::Jn(get_label!(l), pc + 1),
                Inst::Ret => TermInst::Ret,
                Inst::End => TermInst::End,
                Inst::ParseError(err) => TermInst::Error(err.clone()),
                _ => {
                    curr_block.push(inst.clone());
                    continue;
                }
            };
            blocks.push(BasicBlock {
                insts: curr_block,
                term,
            });
            curr_block = Vec::new();
        }

        Cfg { blocks }
    }
}
