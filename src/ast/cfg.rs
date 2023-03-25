use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

use crate::ast::{Inst, LabelLit};
use crate::error::ParseError;

/// Control-flow graph for AST instructions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cfg {
    bbs: Vec<BasicBlock>,
}

/// Basic block for AST instructions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BasicBlock {
    id: usize,
    label: Option<LabelLit>,
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
        let mut bbs_count = 0;
        for (pc, inst) in prog.iter().enumerate() {
            match inst {
                Inst::Label(l) => {
                    if pc != 0 && !prog[pc - 1].is_control_flow() {
                        bbs_count += 1;
                    }
                    labels.insert(l.clone(), bbs_count);
                }
                _ => {
                    if inst.is_control_flow() {
                        bbs_count += 1;
                    }
                }
            }
        }
        if prog.len() == 0 || !prog[prog.len() - 1].can_end_program() {
            bbs_count += 1;
        }

        macro_rules! get_label(($l:expr) => {
            labels.get($l).copied().unwrap_or(bbs_count - 1)
        });

        let mut bbs = Vec::with_capacity(bbs_count);
        let mut curr_label = None;
        let mut curr_block = Vec::new();
        for (pc, inst) in prog.iter().enumerate() {
            let mut next_label = None;
            let term = match inst {
                Inst::Label(l) => {
                    if pc != 0 && !prog[pc - 1].is_control_flow() {
                        next_label = Some(l.clone());
                        TermInst::Jmp(get_label!(l))
                    } else {
                        curr_label = curr_label.or_else(|| Some(l.clone()));
                        continue;
                    }
                }
                Inst::Call(l) => TermInst::Call(get_label!(l), bbs.len() + 1),
                Inst::Jmp(l) => TermInst::Jmp(get_label!(l)),
                Inst::Jz(l) => TermInst::Jz(get_label!(l), bbs.len() + 1),
                Inst::Jn(l) => TermInst::Jn(get_label!(l), bbs.len() + 1),
                Inst::Ret => TermInst::Ret,
                Inst::End => TermInst::End,
                Inst::ParseError(err) => TermInst::Error(err.clone()),
                _ => {
                    curr_block.push(inst.clone());
                    continue;
                }
            };
            bbs.push(BasicBlock {
                id: bbs.len(),
                label: curr_label,
                insts: curr_block,
                term,
            });
            curr_label = next_label;
            curr_block = Vec::new();
        }
        if prog.len() == 0 || !prog[prog.len() - 1].can_end_program() {
            bbs.push(BasicBlock {
                id: bbs.len(),
                label: curr_label,
                insts: curr_block,
                term: TermInst::Error(ParseError::ImplicitEnd),
            });
        }
        assert_eq!(bbs_count, bbs.len());

        Cfg { bbs }
    }

    #[inline]
    pub fn bbs(&self) -> &[BasicBlock] {
        &self.bbs
    }
}

impl BasicBlock {
    #[inline]
    pub fn id(&self) -> usize {
        self.id
    }

    #[inline]
    pub fn label(&self) -> Option<&LabelLit> {
        self.label.as_ref()
    }

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
        for (id, bb) in self.bbs.iter().enumerate() {
            if id != 0 {
                writeln!(f)?;
            }
            writeln!(f, "{bb}:")?;
            for inst in &bb.insts {
                writeln!(f, "    {inst}")?;
            }
            write!(f, "    ")?;
            match &bb.term {
                TermInst::Call(l1, l2) => write!(f, "call {} {}", self.bbs[*l1], self.bbs[*l2]),
                TermInst::Jmp(l) => write!(f, "jmp {}", self.bbs[*l]),
                TermInst::Jz(l1, l2) => write!(f, "jz {} {}", self.bbs[*l1], self.bbs[*l2]),
                TermInst::Jn(l1, l2) => write!(f, "jn {} {}", self.bbs[*l1], self.bbs[*l2]),
                TermInst::Ret => write!(f, "ret"),
                TermInst::End => write!(f, "end"),
                TermInst::Error(err) => write!(f, "error {err:?}"),
            }?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.label {
            Some(l) => write!(f, "{l}"),
            None => write!(f, "bb{}", self.id),
        }
    }
}
