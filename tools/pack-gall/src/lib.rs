mod io;
mod model;
mod observe;
mod resolver;
mod verify;

pub use io::{parse_args, read_observation, write_json};
pub use model::{
    Catalog, Observation, Receipt, ResolutionEvidence, VerifierReport, OBSERVATION_SCHEMA,
    VERIFIER_SCHEMA,
};
pub use observe::observe;
pub use resolver::resolve;
pub use verify::{issue_receipt, verify};
