//! Lifecycle: Coordinating multiple compound machines into an operational arc

use crate::law::Law;
use serde::{Deserialize, Serialize};

pub struct BatchLifecycle;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BatchStage {
    Declared,
    DatasetBound,
    WindowOpen,
    Authorized,
    CheckpointReady,
    Admitted,
    Exited,
}

pub struct BatchLifecycleLaw;

impl Law for BatchLifecycleLaw {
    type Field = BatchStage;
    type Condition = BatchStage;
    type Admitted = BatchStage;
    type Receipt = String;
    type Exit = u8;
    type Error = crate::Error;

    fn validate(field: BatchStage) -> std::result::Result<BatchStage, Self::Error> {
        Ok(field)
    }

    fn select(field: BatchStage) -> Self::Condition {
        field
    }

    fn admit(_field: BatchStage, condition: Self::Condition) -> Self::Admitted {
        condition
    }

    fn receipt(admitted: Self::Admitted, sequence: u64) -> Self::Receipt {
        format!("BATCH:{:?}:{}", admitted, sequence)
    }

    fn exit(_receipt: Self::Receipt) -> Self::Exit {
        0
    }
}
