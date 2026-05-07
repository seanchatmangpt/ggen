use crate::ids::TermId;
use crate::types::*;
use anyhow::{anyhow, Result};

pub struct Prolog8Compiler {}

impl Prolog8Compiler {
    pub fn compile_atom(pred_id: u32, args: &[TermId]) -> Result<Atom8> {
        if args.len() > 8 {
            return Err(anyhow!("E_ARITY_CAP_EXCEEDED"));
        }
        let mut atom_args = [0; 8];
        for (i, &arg) in args.iter().enumerate() {
            atom_args[i] = arg;
        }
        Ok(Atom8 {
            pred_id,
            arity: args.len() as u8,
            binding_mask: 0,
            args: atom_args,
        })
    }

    pub fn compile_rule(rule_id: u32, head: Atom8, body: &[Atom8]) -> Result<Rule8> {
        if body.len() > 8 {
            return Err(anyhow!("E_RULE_BODY_CAP_EXCEEDED"));
        }
        let mut rule_body = [head; 8];
        for (i, atom) in body.iter().enumerate() {
            rule_body[i] = *atom;
        }
        Ok(Rule8 {
            rule_id,
            head,
            body: rule_body,
            body_len: body.len() as u8,
            body_mask: ((1 << body.len()) - 1) as u8,
            negation_mask: 0,
            builtin_mask: 0,
            var_count: 0,
            var_live_mask: 0,
            feature_mask: 0,
            proof_mask: 0,
            plan_id: 0,
        })
    }
}
