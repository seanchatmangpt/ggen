//! Chicago court: ggen_igniter clean-room contract (IO-free).
//!
//! Asserts the typed interface/refusal surface without mounting the sibling
//! or invoking git. Crown ALIVE for the external tree remains a separate
//! claim — see `docs/rfc/v27.1.1/IGNITER-CLEANROOM-STANDING.md`.
//!
//! Falsifier:
//! `cargo test -p ggen-sjira --test igniter_cleanroom_court`

use ggen_sjira::igniter_cleanroom::{
    adjudicate, standing_label, CleanRoomObservation, CleanRoomRefusal, CleanRoomStandingLabel,
    SUBJECT_REPO, VERIFIER_COMMAND,
};

#[test]
fn verifier_command_is_exact_git_diff_exit_code() {
    // Arrange / Act / Assert — pin the crown verifier string
    assert_eq!(VERIFIER_COMMAND, "git diff --exit-code");
    assert_eq!(SUBJECT_REPO, "seanchatmangpt/ggen_igniter");
}

#[test]
fn unmounted_checkout_refuses_alive_promotion() {
    // Arrange — this cloud workspace does not mount ggen_igniter
    let obs = CleanRoomObservation {
        subject: SUBJECT_REPO.into(),
        git_diff_exit_code: None,
    };

    // Act
    let result = adjudicate(&obs);
    let label = standing_label(&obs);

    // Assert — typed refusal; standing BLOCKED (not ALIVE, not fabricated)
    match result {
        Err(CleanRoomRefusal::CheckoutUnmounted) => {}
        other => panic!("expected CheckoutUnmounted, got {other:?}"),
    }
    assert_eq!(label, CleanRoomStandingLabel::Blocked);
    let msg = CleanRoomRefusal::CheckoutUnmounted.to_string();
    assert!(
        msg.starts_with("REFUSED_CLEANROOM_CHECKOUT_UNMOUNTED:"),
        "refusal must be typed with REFUSED_ prefix: {msg}"
    );
    assert!(
        msg.contains(VERIFIER_COMMAND),
        "refusal must name the verifier"
    );
}

#[test]
fn dirty_tree_nonzero_exit_is_refused() {
    // Arrange — historical IG-1 posture (~98 dirty files) as a nonzero probe
    let obs = CleanRoomObservation {
        subject: SUBJECT_REPO.into(),
        git_diff_exit_code: Some(1),
    };

    // Act
    let result = adjudicate(&obs);

    // Assert
    match result {
        Err(CleanRoomRefusal::DirtyTree { exit_code }) => assert_eq!(exit_code, 1),
        other => panic!("expected DirtyTree(1), got {other:?}"),
    }
    assert_eq!(standing_label(&obs), CleanRoomStandingLabel::RefusedDirty);
    let msg = result.unwrap_err().to_string();
    assert!(msg.starts_with("REFUSED_CLEANROOM_DIRTY_TREE:"));
}

#[test]
fn clean_probe_exit_zero_admits_alive_receipt_shape() {
    // Arrange — only an observed exit 0 may promote
    let obs = CleanRoomObservation {
        subject: SUBJECT_REPO.into(),
        git_diff_exit_code: Some(0),
    };

    // Act
    let alive = adjudicate(&obs).expect("exit 0 must admit");

    // Assert
    assert_eq!(alive.subject, SUBJECT_REPO);
    assert_eq!(alive.verifier, VERIFIER_COMMAND);
    assert_eq!(standing_label(&obs), CleanRoomStandingLabel::Alive);
}

#[test]
fn wrong_subject_is_refused_not_silently_retargeted() {
    // Arrange
    let obs = CleanRoomObservation {
        subject: "seanchatmangpt/ggen".into(),
        git_diff_exit_code: Some(0),
    };

    // Act
    let result = adjudicate(&obs);

    // Assert — even exit 0 on the wrong repo must not mint igniter ALIVE
    match result {
        Err(CleanRoomRefusal::SubjectMismatch { observed }) => {
            assert_eq!(observed, "seanchatmangpt/ggen");
        }
        other => panic!("expected SubjectMismatch, got {other:?}"),
    }
}
