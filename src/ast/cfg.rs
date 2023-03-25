use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

use crate::ast::Inst;
use crate::error::ParseError;

/// Control-flow graph for AST instructions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cfg {
    blocks: Vec<BasicBlock>,
}

/// Basic block for AST instructions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BasicBlock {
    insts: Vec<Inst>,
    term: TermInst,
}

/// Terminator instruction in a basic block.
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
        let mut prog_len = prog.len();
        for (pc, inst) in prog.iter().enumerate() {
            match inst {
                Inst::ParseError(_) => {
                    prog_len = pc + 1;
                    break;
                }
                _ => {}
            }
        }
        // Ignore any instructions after the first parse error, to avoid jumping
        // past it.
        let prog = &prog[..prog_len];

        let mut labels = HashMap::new();
        let mut block_count = 0;
        for (pc, inst) in prog.iter().enumerate() {
            match inst {
                Inst::Label(l) => {
                    if pc != 0 && !matches!(prog[pc - 1], Inst::Label(_)) {
                        block_count += 1;
                    }
                    labels.insert(l.clone(), block_count);
                }
                _ => {
                    if inst.is_terminator() {
                        block_count += 1;
                    }
                }
            }
        }
        if prog.len() == 0 || !prog[prog.len() - 1].can_end_program() {
            block_count += 1;
        }

        macro_rules! get_label(($l:expr) => {
            labels.get($l).copied().unwrap_or(block_count - 1)
        });

        let mut blocks = Vec::with_capacity(block_count);
        let mut curr_block = Vec::new();
        for (pc, inst) in prog.iter().enumerate() {
            let term = match inst {
                Inst::Label(l) => {
                    if pc != 0 && !matches!(prog[pc - 1], Inst::Label(_)) {
                        TermInst::Jmp(get_label!(l))
                    } else {
                        continue;
                    }
                }
                Inst::Call(l) => TermInst::Call(get_label!(l), blocks.len() + 1),
                Inst::Jmp(l) => TermInst::Jmp(get_label!(l)),
                Inst::Jz(l) => TermInst::Jz(get_label!(l), blocks.len() + 1),
                Inst::Jn(l) => TermInst::Jn(get_label!(l), blocks.len() + 1),
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
        if prog.len() == 0 || !prog[prog.len() - 1].can_end_program() {
            blocks.push(BasicBlock {
                insts: curr_block,
                term: TermInst::Error(ParseError::ImplicitEnd),
            });
        }
        assert_eq!(block_count, blocks.len());

        Cfg { blocks }
    }

    #[inline]
    pub fn blocks(&self) -> &[BasicBlock] {
        &self.blocks
    }
}

impl BasicBlock {
    #[inline]
    pub fn insts(&self) -> &[Inst] {
        &self.insts
    }

    #[inline]
    pub fn term(&self) -> &TermInst {
        &self.term
    }
}

impl Display for Cfg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (id, block) in self.blocks.iter().enumerate() {
            if id != 0 {
                writeln!(f)?;
            }
            writeln!(f, "bb{id}:")?;
            Display::fmt(block, f)?;
        }
        Ok(())
    }
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for inst in &self.insts {
            writeln!(f, "    {inst}")?;
        }
        writeln!(f, "    {}", self.term)
    }
}

impl Display for TermInst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TermInst::Call(l1, l2) => write!(f, "call bb{l1} bb{l2}"),
            TermInst::Jmp(l) => write!(f, "jmp bb{l}"),
            TermInst::Jz(l1, l2) => write!(f, "jz bb{l1} bb{l2}"),
            TermInst::Jn(l1, l2) => write!(f, "jn bb{l1} bb{l2}"),
            TermInst::Ret => write!(f, "ret"),
            TermInst::End => write!(f, "end"),
            TermInst::Error(err) => write!(f, "error {err:?}"),
        }
    }
}
