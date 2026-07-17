#![doc = "Fused Law Object abstraction: obligation + lifecycle + receipt + OCEL."]
#![deny(unsafe_code)]
#![warn(missing_docs)]

pub mod arazzo;
pub mod default_law;
pub mod error;
pub mod graphlaw_authority;
pub mod law;
pub mod lifecycle;
pub mod ocel_export;
pub mod quarantine;
pub mod receipt_record;
pub mod receipt_store;
pub mod receipt_validator;
pub mod refusal;
pub mod replay_adapter;
pub mod verify;

#[cfg(feature = "ocel")]
pub mod ocel;

#[cfg(feature = "signed")]
pub mod signing;

pub use arazzo::ArazzoProjectionReceipt;
pub use default_law::DefaultLaw;
pub use law::{Admit, Andon, Judge, LawObject, Obligation};
pub use quarantine::{BoundarySchema, JsonBoundarySchema, QuarantineError, RiceQuarantine};
pub use receipt_record::ReceiptRecord;
pub use receipt_store::ReceiptStore;
pub use receipt_validator::{Clock, FixedClock, ReceiptValidator, SystemClock, Verdict};
pub use refusal::{
    compose_denials, denial_lane, scenario_for_denial_lane, RefusalCategory, RefusalScenario,
};
