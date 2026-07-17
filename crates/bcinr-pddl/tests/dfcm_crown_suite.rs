//! 5-second wall-clock gate for the DfCM crown suite (see
//! docs/DFCM_CONTRACT.md). Asserts that the full bounded (ops × capacity)
//! matrix — topology, planning, analysis, admission, receipt, replay, all
//! within the 8/64 bound — composes inside one fixed wall-clock envelope.

use bcinr_pddl::run_dfcm_crown_suite;

#[test]
fn dfcm_crown_suite_completes_under_5_seconds() {
    let start = std::time::Instant::now();

    let receipt = run_dfcm_crown_suite().expect("dfcm crown suite must run to completion");

    let elapsed = start.elapsed().as_secs_f64();
    assert!(
        elapsed <= 5.0,
        "DfCM crown suite exceeded 5s wall-clock gate: {elapsed}s"
    );
    assert!(
        receipt.suite_passed_5s_gate,
        "DfcmBenchReceipt.suite_passed_5s_gate must be true: {receipt:?}"
    );
    assert!(
        receipt.max_ops <= 64,
        "no benchmark cell may exceed 64 tape ops: max_ops={}",
        receipt.max_ops
    );
}
