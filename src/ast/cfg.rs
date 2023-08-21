use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

use crate::ast::{Inst, LabelLit};
use crate::error::ParseError;

/// Control-flow graph of AST basic blocks.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cfg<'a> {
    bbs: Vec<BBlock<'a>>,
}

/// Basic block for AST instructions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BBlock<'a> {
    id: usize,
    label: Option<&'a LabelLit>,
    insts: &'a [Inst],
    exit: ExitInst<'a>,
}

/// Exit instruction in a basic block (the terminator).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExitInst<'a> {
    Call(usize, usize),
    Jmp(usize),
    Jz(usize, usize, &'a Inst),
    Jn(usize, usize, &'a Inst),
    Ret,
    End,
    Error(ParseError),
}

impl<'a> Cfg<'a> {
    pub fn new(prog: &'a [Inst]) -> Self {
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
        let mut block_start = 0;
        for (pc, inst) in prog.iter().enumerate() {
            let mut next_label = None;
            let exit = match inst {
                Inst::Label(l) => {
                    if block_start == pc {
                        curr_label = curr_label.or_else(|| Some(l));
                        block_start = pc + 1;
                        continue;
                    } else {
                        next_label = Some(l);
                        ExitInst::Jmp(get_label!(l))
                    }
                }
                Inst::Call(l) => ExitInst::Call(get_label!(l), bbs.len() + 1),
                Inst::Jmp(l) => ExitInst::Jmp(get_label!(l)),
                Inst::Jz(l) => ExitInst::Jz(get_label!(l), bbs.len() + 1, inst),
                Inst::Jn(l) => ExitInst::Jn(get_label!(l), bbs.len() + 1, inst),
                Inst::Ret => ExitInst::Ret,
                Inst::End => ExitInst::End,
                Inst::ParseError(err) => ExitInst::Error(err.clone()),
                _ => continue,
            };
            bbs.push(BBlock {
                id: bbs.len(),
                label: curr_label,
                insts: &prog[block_start..pc],
                exit,
            });
            curr_label = next_label;
            block_start = pc + 1;
        }
        if prog.len() == 0 || !prog[prog.len() - 1].can_end_program() {
            bbs.push(BBlock {
                id: bbs.len(),
                label: curr_label,
                insts: &prog[block_start..],
                exit: ExitInst::Error(ParseError::ImplicitEnd),
            });
        }
        assert_eq!(bbs_count, bbs.len());

        Cfg { bbs }
    }

    #[inline]
    pub fn bbs(&self) -> &[BBlock] {
        &self.bbs
    }
}

impl<'a> BBlock<'a> {
    #[inline]
    pub fn id(&self) -> usize {
        self.id
    }

    #[inline]
    pub fn label(&self) -> Option<&'a LabelLit> {
        self.label
    }

    #[inline]
    pub fn insts(&self) -> &'a [Inst] {
        self.insts
    }

    #[inline]
    pub fn exit(&self) -> &ExitInst {
        &self.exit
    }
}

impl Display for Cfg<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut first = true;
        for bb in &self.bbs {
            if !first {
                writeln!(f)?;
            }
            first = false;
            writeln!(f, "{bb}:")?;
            for inst in bb.insts {
                writeln!(f, "    {inst}")?;
            }
            macro_rules! bb(($l:ident) => { self.bbs[*$l] });
            write!(f, "    ")?;
            match &bb.exit {
                ExitInst::Call(l1, l2) => write!(f, "call {} {}", bb!(l1), bb!(l2)),
                ExitInst::Jmp(l) => write!(f, "jmp {}", bb!(l)),
                ExitInst::Jz(l1, l2, _) => write!(f, "jz {} {}", bb!(l1), bb!(l2)),
                ExitInst::Jn(l1, l2, _) => write!(f, "jn {} {}", bb!(l1), bb!(l2)),
                ExitInst::Ret => write!(f, "ret"),
                ExitInst::End => write!(f, "end"),
                ExitInst::Error(err) => write!(f, "error {err:?}"),
            }?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Display for BBlock<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.label {
            Some(l) => write!(f, "{l}"),
            None => write!(f, "bb{}", self.id),
        }
    }
}
