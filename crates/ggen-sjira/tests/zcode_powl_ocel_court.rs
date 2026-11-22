//! Chicago court: zcode-cli POWL/OCEL autonomic remediation contract (IO-free).
//!
//! Asserts the typed interface/refusal surface without mounting the sibling
//! or invoking bun/pnpm. Crown ALIVE for the external tree remains a separate
//! claim — see `docs/rfc/v27.1.1/ZCODE-POWL-OCEL-STANDING.md`.
//!
//! Falsifier:
//! `cargo test -p ggen-sjira --test zcode_powl_ocel_court`

use ggen_sjira::zcode_powl_ocel::{
    adjudicate, standing_label, PREDECESSOR_IGNITER_VERIFIER, SUBJECT_REPO, UI_LAYOUT_COURT_COMMAND,
    VERIFIER_COMMAND, ZcodeRemediationObservation, ZcodeRemediationRefusal,
    ZcodeRemediationStandingLabel,
};

#[test]
fn verifier_pins_chicago_court_and_predecessor_igniter() {
    // Arrange / Act / Assert — pin hop-5 and hop-4 verifier strings
    assert_eq!(VERIFIER_COMMAND, "bun test test/powl-ocel-chicago.test.ts");
    assert_eq!(UI_LAYOUT_COURT_COMMAND, "pnpm test -- ui-layout-court");
    assert_eq!(PREDECESSOR_IGNITER_VERIFIER, "git diff --exit-code");
    assert_eq!(SUBJECT_REPO, "seanchatmangpt/zcode-cli");
}

#[test]
fn unmounted_checkout_refuses_alive_promotion() {
    // Arrange — this cloud workspace does not mount zcode-cli
    let obs = ZcodeRemediationObservation {
        subject: SUBJECT_REPO.into(),
        chicago_court_exit_code: None,
    };

    // Act
    let result = adjudicate(&obs);
    let label = standing_label(&obs);

    // Assert — typed refusal; standing BLOCKED (not ALIVE, not fabricated)
    match result {
        Err(ZcodeRemediationRefusal::CheckoutUnmounted) => {}
        other => panic!("expected CheckoutUnmounted, got {other:?}"),
    }
    assert_eq!(label, ZcodeRemediationStandingLabel::Blocked);
    let msg = ZcodeRemediationRefusal::CheckoutUnmounted.to_string();
    assert!(
        msg.starts_with("REFUSED_ZCODE_CHECKOUT_UNMOUNTED:"),
        "refusal must be typed with REFUSED_ prefix: {msg}"
    );
    assert!(
        msg.contains(VERIFIER_COMMAND),
        "refusal must name the Chicago verifier"
    );
    assert!(
        msg.contains(UI_LAYOUT_COURT_COMMAND),
        "refusal must name the ui-layout companion"
    );
}

#[test]
fn court_nonzero_exit_is_refused() {
    // Arrange — mounted probe that failed closed
    let obs = ZcodeRemediationObservation {
        subject: SUBJECT_REPO.into(),
        chicago_court_exit_code: Some(1),
    };

    // Act
    let result = adjudicate(&obs);

    // Assert
    match result {
        Err(ZcodeRemediationRefusal::CourtFailed { exit_code }) => assert_eq!(exit_code, 1),
        other => panic!("expected CourtFailed(1), got {other:?}"),
    }
    assert_eq!(standing_label(&obs), ZcodeRemediationStandingLabel::RefusedCourt);
    let msg = result.unwrap_err().to_string();
    assert!(msg.starts_with("REFUSED_ZCODE_COURT_FAILED:"));
}

#[test]
fn clean_probe_exit_zero_admits_alive_receipt_shape() {
    // Arrange — only an observed exit 0 may promote
    let obs = ZcodeRemediationObservation {
        subject: SUBJECT_REPO.into(),
        chicago_court_exit_code: Some(0),
    };

    // Act
    let alive = adjudicate(&obs).expect("exit 0 must admit");

    // Assert — receipt shape only; this test does not claim live sibling ALIVE
    assert_eq!(alive.subject, SUBJECT_REPO);
    assert_eq!(alive.verifier, VERIFIER_COMMAND);
    assert_eq!(standing_label(&obs), ZcodeRemediationStandingLabel::Alive);
}

#[test]
fn wrong_subject_is_refused_not_silently_retargeted() {
    // Arrange — even exit 0 on ggen or igniter must not mint zcode ALIVE
    let obs = ZcodeRemediationObservation {
        subject: "seanchatmangpt/ggen".into(),
        chicago_court_exit_code: Some(0),
    };

    // Act
    let result = adjudicate(&obs);

    // Assert
    match result {
        Err(ZcodeRemediationRefusal::SubjectMismatch { observed }) => {
            assert_eq!(observed, "seanchatmangpt/ggen");
        }
        other => panic!("expected SubjectMismatch, got {other:?}"),
    }
    assert_eq!(standing_label(&obs), ZcodeRemediationStandingLabel::Unknown);
    let msg = result.unwrap_err().to_string();
    assert!(msg.starts_with("REFUSED_ZCODE_SUBJECT_MISMATCH:"));
    assert!(
        msg.contains(PREDECESSOR_IGNITER_VERIFIER),
        "mismatch remediation must point operators at hop-4 igniter verifier"
    );
}

#[test]
fn igniter_subject_with_git_diff_exit_zero_still_mismatches_zcode() {
    // Arrange — predecessor hop-4 success is not hop-5 evidence
    let obs = ZcodeRemediationObservation {
        subject: "seanchatmangpt/ggen_igniter".into(),
        chicago_court_exit_code: Some(0),
    };

    // Act
    let result = adjudicate(&obs);

    // Assert
    match result {
        Err(ZcodeRemediationRefusal::SubjectMismatch { observed }) => {
            assert_eq!(observed, "seanchatmangpt/ggen_igniter");
        }
        other => panic!("expected SubjectMismatch for igniter subject, got {other:?}"),
    }
}
