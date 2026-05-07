use crate::types::*;
use anyhow::Result;

pub struct Prolog8Doctor {}

impl Prolog8Doctor {
    pub fn check_atom(atom: &Atom8) -> Result<()> {
        if atom.arity > 8 {
            anyhow::bail!("E_ARITY_CAP_EXCEEDED");
        }
        Ok(())
    }

    pub fn check_rule(rule: &Rule8) -> Result<()> {
        if rule.body_len > 8 {
            anyhow::bail!("E_RULE_BODY_CAP_EXCEEDED");
        }
        Ok(())
    }
}
