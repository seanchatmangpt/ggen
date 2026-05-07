//! DX Macros for Prolog8
//!
//! Provides convenient ways to construct atoms and rules without boilerplate.

#[macro_export]
macro_rules! atom {
    ($pred:expr $(, $arg:expr)*) => {
        {
            let mut args = [0; 8];
            let mut i = 0;
            $(
                args[i] = $arg;
                i += 1;
            )*
            $crate::types::Atom8 {
                pred_id: $pred,
                arity: i as u8,
                binding_mask: ((1 << i) - 1) as u8,
                args,
            }
        }
    };
}

#[macro_export]
macro_rules! rule {
    ($rule_id:expr, $head:expr $(, $body:expr)*) => {
        {
            let mut body = [$head.clone(); 8]; // placeholder initialization
            let mut i = 0;
            $(
                body[i] = $body;
                i += 1;
            )*
            $crate::types::Rule8 {
                rule_id: $rule_id,
                head: $head,
                body,
                body_len: i as u8,
                body_mask: ((1 << i) - 1) as u8,
                negation_mask: 0,
                builtin_mask: 0,
                var_count: 0,
                var_live_mask: 0,
                feature_mask: 0,
                proof_mask: 0,
                plan_id: 0,
            }
        }
    };
}

#[macro_export]
macro_rules! query {
    ($atom:expr) => {
        $crate::types::QueryAtom8 {
            atom: $atom,
            output_mask: 0,
            proof_mode: $crate::types::ProofMode::Positive,
            epoch: 0,
        }
    };
    ($atom:expr, $mode:expr) => {
        $crate::types::QueryAtom8 {
            atom: $atom,
            output_mask: 0,
            proof_mode: $mode,
            epoch: 0,
        }
    };
}
