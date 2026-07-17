#![allow(missing_docs)]
#![allow(dead_code)]
use crate::observability::ocel::types::OcelLog;
use crate::observability::ocel::wasm4pm::TestSuiteWitness;
use wasm4pm_compat::{Admitted, Evidence};

/// Projects an admitted OCEL log into the `Projected` stage.
#[must_use]
pub fn project_admission_events(
    log: Evidence<OcelLog, Admitted, TestSuiteWitness>,
) -> Evidence<OcelLog, wasm4pm_compat::Projected, TestSuiteWitness> {
    log.into_projected()
}
