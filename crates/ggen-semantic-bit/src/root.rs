//! Root Fields: The fundamental 256-state semantic automata

use crate::law::Law;
use serde::{Deserialize, Serialize};

// ============================================================================
// StatusField
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum StatusCondition {
    Invalid,
    Ready,
    Busy,
    Error,
    Held,
    Completed,
}

pub struct StatusLaw;

impl Law for StatusLaw {
    type Field = u8;
    type Condition = StatusCondition;
    type Admitted = StatusCondition;
    type Receipt = String;
    type Exit = u8;
    type Error = crate::Error;

    fn validate(field: u8) -> std::result::Result<u8, Self::Error> {
        if field == 0 {
            return Err(crate::Error::ValidationFailed(
                "Zero field is invalid".to_string(),
            ));
        }
        Ok(field)
    }

    fn select(field: u8) -> Self::Condition {
        match field {
            1 => StatusCondition::Ready,
            2 => StatusCondition::Busy,
            4 => StatusCondition::Error,
            8 => StatusCondition::Held,
            16 => StatusCondition::Completed,
            _ => StatusCondition::Invalid,
        }
    }

    fn admit(_field: u8, condition: Self::Condition) -> Self::Admitted {
        condition
    }

    fn receipt(admitted: Self::Admitted, sequence: u64) -> Self::Receipt {
        format!("STATUS:{:?}:{}", admitted, sequence)
    }

    fn exit(_receipt: Self::Receipt) -> Self::Exit {
        0
    }
}

// ============================================================================
// AuthorityField
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AuthorityCondition {
    None,
    Operator,
    System,
    Root,
    Denied,
}

pub struct AuthorityLaw;

impl Law for AuthorityLaw {
    type Field = u8;
    type Condition = AuthorityCondition;
    type Admitted = AuthorityCondition;
    type Receipt = String;
    type Exit = u8;
    type Error = crate::Error;

    fn validate(field: u8) -> std::result::Result<u8, Self::Error> {
        Ok(field)
    }

    fn select(field: u8) -> Self::Condition {
        match field {
            0 => AuthorityCondition::None,
            1 => AuthorityCondition::Operator,
            2 => AuthorityCondition::System,
            4 => AuthorityCondition::Root,
            _ => AuthorityCondition::Denied,
        }
    }

    fn admit(_field: u8, condition: Self::Condition) -> Self::Admitted {
        condition
    }

    fn receipt(admitted: Self::Admitted, sequence: u64) -> Self::Receipt {
        format!("AUTH:{:?}:{}", admitted, sequence)
    }

    fn exit(_receipt: Self::Receipt) -> Self::Exit {
        0
    }
}
