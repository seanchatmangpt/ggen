//! Compound Machines: 64-bit semantic words coordinating multiple fields

use crate::semantic_bit::law::Law;
use crate::semantic_bit::root::{AuthorityCondition, AuthorityLaw, StatusCondition, StatusLaw};
use serde::{Deserialize, Serialize};

/// A 64-bit word coordinating Status and Authority
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct JobControl {
    pub status: u8,
    pub authority: u8,
    pub spare: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum JobCondition {
    Admissible,
    Denied,
    NeedsReview,
}

pub struct JobControlLaw;

impl Law for JobControlLaw {
    type Field = JobControl;
    type Condition = JobCondition;
    type Admitted = JobCondition;
    type Receipt = String;
    type Exit = u8;
    type Error = crate::semantic_bit::Error;

    fn validate(field: JobControl) -> std::result::Result<JobControl, Self::Error> {
        StatusLaw::validate(field.status)?;
        AuthorityLaw::validate(field.authority)?;
        Ok(field)
    }

    fn select(field: JobControl) -> Self::Condition {
        let status = StatusLaw::select(field.status);
        let auth = AuthorityLaw::select(field.authority);

        match (status, auth) {
            (StatusCondition::Ready, AuthorityCondition::Root) => JobCondition::Admissible,
            (StatusCondition::Ready, AuthorityCondition::System) => JobCondition::Admissible,
            (StatusCondition::Ready, AuthorityCondition::Operator) => JobCondition::NeedsReview,
            _ => JobCondition::Denied,
        }
    }

    fn admit(_field: JobControl, condition: Self::Condition) -> Self::Admitted {
        condition
    }

    fn receipt(admitted: Self::Admitted, sequence: u64) -> Self::Receipt {
        format!("JOB:{:?}:{}", admitted, sequence)
    }

    fn exit(_receipt: Self::Receipt) -> Self::Exit {
        0
    }
}
