//! Chicago court: ECOSYSTEM-SPEC-27.1.1 §11 R-01…R-07 resolution contract.
//!
//! AAA, no mocks. Real on-disk RFC + standing docs + admitted register fixture;
//! real typed stub. Falsifier: `cargo test -p ggen-graph --test r01_r07_resolution_court`

use std::path::PathBuf;

use ggen_graph::ecosystem_risk::{
    current_body_standing, is_known_risk_id, resolve_closure, risk_body_is_admitted, ClosureClaim,
    ResolutionRefusal, RiskBodyStanding, ADMITTED_RISK_REGISTER_REL, NEXT_VERIFIER_RISK_REGISTER,
    RISK_IDS,
};

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crates/")
        .parent()
        .expect("workspace root")
        .to_path_buf()
}

#[test]
fn risk_id_set_is_exactly_r01_through_r07() {
    // Arrange
    let expected = ["R-01", "R-02", "R-03", "R-04", "R-05", "R-06", "R-07"];

    // Act / Assert
    assert_eq!(RISK_IDS, expected);
    for id in expected {
        assert!(is_known_risk_id(id));
        assert!(risk_body_is_admitted(id));
    }
    assert!(!is_known_risk_id("R-08"));
    assert!(!is_known_risk_id("R-00"));
}

#[test]
fn closed_without_admitted_body_is_refused_ungrounded_for_every_r() {
    // Arrange — fence still refuses when caller lies about admission
    let claim = ClosureClaim::Closed {
        evidence: "invented-closure".into(),
    };

    // Act / Assert
    for id in RISK_IDS {
        let err = resolve_closure(id, claim.clone(), false).expect_err("must refuse");
        assert!(
            matches!(
                &err,
                ResolutionRefusal::Ungrounded { risk_id } if risk_id == id
            ),
            "expected REFUSED_UNGROUNDED for {id}, got {err:?}"
        );
        let msg = err.to_string();
        assert!(
            msg.contains("REFUSED_UNGROUNDED"),
            "refusal display must carry REFUSED_UNGROUNDED; got {msg}"
        );
    }
}

#[test]
fn admitted_bodies_open_except_r06_closed() {
    // Arrange / Act / Assert — register mount admits all; only R-06 closed
    for id in RISK_IDS {
        resolve_closure(id, ClosureClaim::Open, true).expect("open must be ok");
        let standing = current_body_standing(id).expect("known id");
        if id == "R-06" {
            assert_eq!(standing, RiskBodyStanding::Closed);
            resolve_closure(
                id,
                ClosureClaim::Closed {
                    evidence: "zcode-cli@0cff19a+7b2c181;powl-ocel-chicago:11/EXIT0".into(),
                },
                true,
            )
            .expect("R-06 closed with admitted body");
        } else {
            assert_eq!(standing, RiskBodyStanding::AdmittedOpen);
        }
    }
}

#[test]
fn closed_with_admitted_body_clears_ungrounded_gate() {
    // Arrange
    let claim = ClosureClaim::Closed {
        evidence: "register-digest:example".into(),
    };

    // Act / Assert
    resolve_closure("R-03", claim, true).expect("admitted body clears ungrounded fence");
}

#[test]
fn unknown_risk_id_is_refused() {
    // Arrange / Act
    let err = resolve_closure("R-99", ClosureClaim::Open, false).expect_err("unknown");

    // Assert
    assert!(matches!(err, ResolutionRefusal::UnknownId(_)));
    assert!(err.to_string().contains("REFUSED_UNKNOWN_RISK_ID"));
}

#[test]
fn admitted_risk_register_fixture_mounts_all_seven_bodies() {
    // Arrange — real admitted register on disk (not invented)
    let path = workspace_root().join(ADMITTED_RISK_REGISTER_REL);
    let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path:?}: {e}"));

    // Act / Assert
    assert!(
        text.contains("zcode-cli"),
        "fixture must cite zcode-cli provenance"
    );
    for id in RISK_IDS {
        assert!(
            text.contains(id),
            "admitted register must mount body prose for {id}"
        );
    }
    assert!(
        text.contains("018FD7-382078-0D52EF"),
        "R-07 body must retain billing hold id from admitted source"
    );
    assert!(
        text.contains("0cff19a") && text.contains("7b2c181"),
        "R-06 body must retain CLOSED commit pins from admitted source"
    );
}

#[test]
fn ecosystem_spec_pins_resolution_contract_and_refused_ungrounded() {
    // Arrange — real RFC on disk
    let path = workspace_root().join("docs/rfc/v27.1.1/ECOSYSTEM-SPEC-27.1.1.md");
    let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path:?}: {e}"));

    // Act / Assert — §11 contract surface
    assert!(
        text.contains("## 11. Unresolved risks R-01 … R-07"),
        "ECOSYSTEM-SPEC must retain §11 R-01…R-07 heading"
    );
    assert!(
        text.contains("REFUSED_UNGROUNDED"),
        "ECOSYSTEM-SPEC must pin REFUSED_UNGROUNDED for ungrounded closed claims"
    );
    assert!(
        text.contains("typed receipt or typed refusal before GA"),
        "ECOSYSTEM-SPEC must pin typed receipt/refusal closure rule"
    );
    assert!(
        text.contains(ADMITTED_RISK_REGISTER_REL)
            || text.contains("r01-r07-sovereign-risk-register.md"),
        "ECOSYSTEM-SPEC §11 must cite admitted register mount"
    );
    for id in RISK_IDS {
        assert!(text.contains(id), "ECOSYSTEM-SPEC must enumerate {id}");
    }
}

#[test]
fn standing_doc_records_admitted_bodies_and_r06_closed() {
    // Arrange — real standing companion on disk
    let path = workspace_root().join("docs/rfc/v27.1.1/R01-R07-STANDING.md");
    let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {path:?}: {e}"));

    // Act / Assert
    assert!(
        text.contains("Document ID"),
        "standing doc must identify itself"
    );
    assert!(
        text.contains(NEXT_VERIFIER_RISK_REGISTER)
            || text.contains("r01-r07-sovereign-risk-register"),
        "standing doc must name the admitted register next verifier"
    );
    for id in RISK_IDS {
        assert!(text.contains(id), "standing doc must list {id}");
        let needle = format!("| {id} |");
        assert!(
            text.contains(&needle),
            "standing table must have a row for {id}"
        );
    }
    assert!(
        text.contains("PARTIAL_ALIVE") && text.contains("REFUSED_UNGROUNDED"),
        "standing doc must record the in-repo resolution-contract fence as PARTIAL_ALIVE"
    );
    assert!(
        text.contains("R-06") && text.contains("**CLOSED**") || text.contains("| **CLOSED** |"),
        "standing doc must mark R-06 body CLOSED with evidence"
    );
    assert!(
        text.contains("AdmittedOpen")
            || text.contains("ADMITTED_OPEN")
            || text.contains("**ADMITTED**"),
        "standing doc must mark remaining bodies ADMITTED/open"
    );
}
