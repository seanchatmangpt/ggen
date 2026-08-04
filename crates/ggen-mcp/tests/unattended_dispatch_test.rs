//! CP33 real end-to-end proof: a fixture project with one
//! `unattended_write_eligible` template whose target does not yet exist --
//! `try_unattended_apply` must write the real file to disk, with zero
//! LLM/human call anywhere in this test. Plus the negative cases the
//! plan's own CP33 verification section calls for: a target that already
//! exists (falls through, file untouched), and an eligible rule sitting
//! inside a project with an unrelated refusal elsewhere (whole-project-clean
//! requirement holds).

use ggen_mcp::tools::unattended_dispatch::{try_unattended_apply, CircuitBreaker, UnattendedApplyOutcome};
use std::time::Duration;
use tempfile::TempDir;

fn write_eligible_fixture(root: &std::path::Path) {
    std::fs::write(
        root.join("ggen.toml"),
        "[project]\nname = \"unattended-dispatch-e2e\"\n\
         [ontology]\nsource = \"model.ttl\"\n\
         [templates]\ndir = \"templates\"\n",
    )
    .expect("write ggen.toml");
    std::fs::write(
        root.join("model.ttl"),
        "@prefix ex: <http://example.org/> .\nex:owner a ex:Person .\n",
    )
    .expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(
        root.join("templates/eligible.tmpl"),
        "---\n\
         to: out/eligible.txt\n\
         unless_exists: true\n\
         unattended_write_eligible: true\n\
         sparql:\n  people: SELECT ?p WHERE { ?p a <http://example.org/Person> }\n\
         ---\n\
         generated content\n",
    )
    .expect("write eligible template");
}

#[tokio::test]
async fn eligible_target_is_applied_with_zero_decision_step() {
    let tmp = TempDir::new().expect("tempdir");
    write_eligible_fixture(tmp.path());
    let breaker = CircuitBreaker::default();

    assert!(
        !tmp.path().join("out/eligible.txt").exists(),
        "precondition: target must not exist before dispatch"
    );

    let outcome = try_unattended_apply(tmp.path(), &breaker).await;
    match outcome {
        UnattendedApplyOutcome::Applied(written) => {
            assert!(
                written.iter().any(|w| w.contains("eligible.txt")),
                "expected eligible.txt among written paths, got {written:?}"
            );
        }
        UnattendedApplyOutcome::NotEligible(reason) => {
            panic!("expected Applied, got NotEligible: {reason}");
        }
    }

    let real_path = tmp.path().join("out/eligible.txt");
    assert!(real_path.exists(), "real file must exist on disk after dispatch");
    let content = std::fs::read_to_string(&real_path).expect("read written file");
    assert!(
        content.contains("generated content"),
        "written file must contain the real rendered body, got: {content}"
    );

    let audit_log = tmp.path().join(".ggen/unattended-dispatch-log.jsonl");
    assert!(audit_log.exists(), "audit log must be written on a successful apply");
    let log_content = std::fs::read_to_string(&audit_log).expect("read audit log");
    assert!(
        log_content.contains("\"outcome\":\"applied\""),
        "audit log must record the applied outcome, got: {log_content}"
    );
}

/// CP34: a real end-to-end proof of the circuit breaker firing against real
/// dispatch calls (not just the isolated unit test), matching the plan's own
/// wording -- 6 real eligible signals for one root, the 6th refused as
/// rate-limited, not for any other reason.
#[tokio::test]
async fn sixth_real_dispatch_in_the_window_is_rate_limited() {
    let tmp = TempDir::new().expect("tempdir");
    std::fs::write(
        tmp.path().join("ggen.toml"),
        "[project]\nname = \"rate-limit-e2e\"\n\
         [ontology]\nsource = \"model.ttl\"\n\
         [templates]\ndir = \"templates\"\n",
    )
    .expect("write ggen.toml");
    std::fs::write(
        tmp.path().join("model.ttl"),
        "@prefix ex: <http://example.org/> .\nex:owner a ex:Person .\n",
    )
    .expect("write ontology");
    std::fs::create_dir_all(tmp.path().join("templates")).expect("mkdir templates");

    // Six distinct eligible templates, each with its own unique target, so
    // each dispatch call has exactly one real thing to apply and the ONLY
    // reason a later call can refuse is the rate limit itself.
    for i in 0..6 {
        std::fs::write(
            tmp.path().join(format!("templates/rule_{i}.tmpl")),
            format!(
                "---\nto: out/rule_{i}.txt\nunless_exists: true\n\
                 unattended_write_eligible: true\n---\nbody {i}\n"
            ),
        )
        .expect("write rule template");
    }

    // Small breaker: 5 per a long window, so the 6th call in quick
    // succession is the one that must be refused.
    let breaker = CircuitBreaker::new(5, Duration::from_secs(60));

    // Remove all but rule_0's template before each call so each dispatch's
    // whole-project-eligible-write-set is exactly {rule_i.txt} -- this
    // mirrors real usage where the dispatcher fires once per real signal,
    // not once for the whole project's remaining eligible set at once.
    let mut applied = 0;
    let mut last_outcome = None;
    for i in 0..6 {
        for other in 0..6 {
            let path = tmp.path().join(format!("templates/rule_{other}.tmpl"));
            if other == i {
                if !path.exists() {
                    std::fs::write(
                        &path,
                        format!(
                            "---\nto: out/rule_{other}.txt\nunless_exists: true\n\
                             unattended_write_eligible: true\n---\nbody {other}\n"
                        ),
                    )
                    .expect("restore rule template");
                }
            } else {
                let _ = std::fs::remove_file(&path);
            }
        }
        let outcome = try_unattended_apply(tmp.path(), &breaker).await;
        if matches!(outcome, UnattendedApplyOutcome::Applied(_)) {
            applied += 1;
        }
        last_outcome = Some(outcome);
    }

    assert_eq!(applied, 5, "exactly 5 of 6 real dispatches should have applied");
    match last_outcome.expect("six iterations ran") {
        UnattendedApplyOutcome::NotEligible(reason) => {
            assert!(
                reason.contains("rate-limited"),
                "6th refusal must be the rate limit specifically, got: {reason}"
            );
        }
        UnattendedApplyOutcome::Applied(_) => panic!("6th dispatch should have been rate-limited"),
    }
}

#[tokio::test]
async fn already_existing_target_falls_through_untouched() {
    let tmp = TempDir::new().expect("tempdir");
    write_eligible_fixture(tmp.path());
    std::fs::create_dir_all(tmp.path().join("out")).expect("mkdir out");
    std::fs::write(tmp.path().join("out/eligible.txt"), "hand-written content, do not touch")
        .expect("pre-create target");

    let breaker = CircuitBreaker::default();
    let outcome = try_unattended_apply(tmp.path(), &breaker).await;
    assert!(
        matches!(outcome, UnattendedApplyOutcome::NotEligible(_)),
        "expected NotEligible when the target already exists, got {outcome:?}"
    );

    let content = std::fs::read_to_string(tmp.path().join("out/eligible.txt")).expect("read");
    assert_eq!(
        content, "hand-written content, do not touch",
        "existing file must be completely untouched by a refused dispatch attempt"
    );
}

#[tokio::test]
async fn eligible_rule_inside_a_project_with_an_unrelated_refusal_is_not_eligible() {
    let tmp = TempDir::new().expect("tempdir");
    write_eligible_fixture(tmp.path());
    // A second, non-eligible template with a real config error: `when:`
    // references a SPARQL ASK query that never resolves because the
    // referenced binding name is nonsensical -- forces the project into an
    // unrelated FM-* refusal state unrelated to the eligible rule itself.
    std::fs::write(
        tmp.path().join("templates/broken.tmpl"),
        "---\n\
         to: out/broken.txt\n\
         sparql:\n  rows: SELECT ?s WHERE { ?s a <http://example.org/NoSuchClass> }\n\
         when: \"{{ this_binding_does_not_exist }}\"\n\
         ---\n\
         broken\n",
    )
    .expect("write broken template");

    let breaker = CircuitBreaker::default();
    let outcome = try_unattended_apply(tmp.path(), &breaker).await;
    assert!(
        matches!(outcome, UnattendedApplyOutcome::NotEligible(_)),
        "an unrelated project-wide issue must block the whole dispatch, got {outcome:?}"
    );
    assert!(
        !tmp.path().join("out/eligible.txt").exists(),
        "the eligible rule must not be applied piecemeal when the whole-project-clean \
         requirement fails elsewhere"
    );
}
