#![cfg(test)]

use super::Refusal;

/// `StageSealMismatch` (PROJ-SEC-04) has no triggering plumbing yet — the
/// per-transition sealing mechanism it will guard is separate follow-up
/// work (PROJ-SEC-01). This test only proves the variant is constructible,
/// carries its context payload through `name()`, and formats via the
/// `#[error(...)]` `Display` impl exactly as declared; it does not
/// fabricate integration plumbing to "trigger" it end-to-end.
#[test]
fn stage_seal_mismatch_constructs_names_and_displays() {
    let refusal = Refusal::StageSealMismatch(
        "stage S3 recomputed seal does not match seal threaded from S2".to_string(),
    );
    assert_eq!(refusal.name(), "StageSealMismatch");
    assert_eq!(
        refusal.to_string(),
        "stage seal mismatch: stage S3 recomputed seal does not match seal threaded from S2"
    );
}

/// `UnlawfulActuation` is the general boundary-actuation refusal, distinct
/// from `N3ActuationRefused` (N3-specific) and
/// `BoundaryRequestMissingReceipt` (boundary-specific). This unit test
/// proves the variant is constructible, distinguishable by name from its
/// siblings, and formats via `Display` as declared.
#[test]
fn unlawful_actuation_constructs_names_and_displays() {
    let refusal = Refusal::UnlawfulActuation(
        "actuation attempted at machine-state boundary without a receipt-verified, \
         sealed admitted transition"
            .to_string(),
    );
    assert_eq!(refusal.name(), "UnlawfulActuation");
    assert_ne!(refusal.name(), "N3ActuationRefused");
    assert_ne!(refusal.name(), "BoundaryRequestMissingReceipt");
    assert_eq!(
        refusal.to_string(),
        "unlawful actuation: actuation attempted at machine-state boundary without a \
         receipt-verified, sealed admitted transition"
    );
}
