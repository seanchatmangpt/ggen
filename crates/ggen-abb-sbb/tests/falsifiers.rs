//! Chicago-style falsifiers for RFC v26.9.26 ABB/SBB manufacture (real kernel values,
//! state assertions only). Each test names the RFC DoD item it can falsify.

use ggen_abb_sbb::*;

const FIXTURE: &str = include_str!("../fixtures/ea-graph.json");

fn gen() -> Generator {
    Generator {
        id: "ggen-abb-sbb".into(),
        version: "26.9.26".into(),
    }
}

fn req(g: &EaGraph, sbb: &str) -> Request {
    Request {
        abb: "abb:event-ingest".into(),
        sbb: sbb.into(),
        requested_authority: Authority::Construct,
        expected_graph_digest: Some(g.digest()),
    }
}

fn graph() -> EaGraph {
    parse_graph(FIXTURE).expect("fixture admits")
}

fn resign(s: &mut CandidateSbb) {
    s.digest = Some(sbb_content_digest(s));
}

// --- fixture is a projection of the generator, not hand-edited -------------------

#[test]
fn fixture_is_byte_identical_to_its_generator() {
    let regenerated = serde_json::to_string_pretty(&synthetic_graph(2, 3)).unwrap() + "\n";
    assert_eq!(
        FIXTURE, regenerated,
        "re-run: cargo run --example emit_fixture > fixtures/ea-graph.json"
    );
}

// --- DoD 1/3/4/5/6: positive witness ---------------------------------------------

#[test]
fn qualified_sbb_manufactures_with_provenance_and_receipt() {
    let g = graph();
    let ad = admit(&g, &req(&g, "sbb:ingest-0000")).unwrap();
    let m = manufacture(&ad, &gen()).unwrap();
    assert_eq!(m.artifacts.len(), 3);
    for a in &m.artifacts {
        let header = a.bytes.lines().next().unwrap();
        assert!(
            header.contains(&format!("graph={}", g.digest())),
            "{header}"
        );
        assert!(header.contains("abb=abb:event-ingest"));
        assert!(header.contains("sbb=sbb:ingest-0000"));
        assert!(header.contains(&format!("sbb_digest={}", ad.sbb_digest())));
        assert!(a
            .bytes
            .contains("realized by sbb:ingest-0000 for cap:event-ingest"));
        assert!(!a.bytes.contains("{{"));
    }
    let r = &m.receipt;
    assert_eq!(r.schema, RECEIPT_SCHEMA);
    assert_eq!(r.graph_digest, g.digest());
    assert_eq!(r.abb, "abb:event-ingest");
    assert_eq!(r.sbb, "sbb:ingest-0000");
    assert_eq!(r.pack, "pack:ingest-0000");
    assert_eq!(r.outputs.len(), 3);
    assert_eq!(r.authority, Authority::None);
    assert_eq!(r.ceiling, Authority::Construct);
    assert!(verify_receipt(r).is_ok());
    let json = serde_json::to_string(r).unwrap();
    let back: ManufactureReceipt = serde_json::from_str(&json).unwrap();
    assert_eq!(&back, r);
}

#[test]
fn second_run_is_byte_identical() {
    let g = graph();
    let a = manufacture(&admit(&g, &req(&g, "sbb:ingest-0001")).unwrap(), &gen()).unwrap();
    let b = manufacture(&admit(&g, &req(&g, "sbb:ingest-0001")).unwrap(), &gen()).unwrap();
    assert_eq!(a, b);
    assert_eq!(replay(&a.receipt, &g, &gen()).unwrap(), a);
}

// --- reordering / duplicate delivery ----------------------------------------------

#[test]
fn element_reordering_does_not_change_digest_or_receipt() {
    let g = graph();
    let mut r = g.clone();
    r.candidate_sbbs.reverse();
    r.qualifications.reverse();
    r.packs.reverse();
    for s in &mut r.candidate_sbbs {
        s.artifacts.reverse();
        s.digest = Some(sbb_content_digest(s));
    }
    assert_eq!(g.digest(), r.digest());
    let a = manufacture(&admit(&g, &req(&g, "sbb:ingest-0000")).unwrap(), &gen()).unwrap();
    let b = manufacture(&admit(&r, &req(&r, "sbb:ingest-0000")).unwrap(), &gen()).unwrap();
    assert_eq!(a.receipt, b.receipt);
}

#[test]
fn duplicate_element_delivery_is_refused() {
    let mut g = graph();
    let dup = g.candidate_sbbs[0].clone();
    g.candidate_sbbs.push(dup);
    let err = admit(
        &g,
        &Request {
            expected_graph_digest: None,
            ..req(&graph(), "sbb:ingest-0000")
        },
    )
    .unwrap_err();
    assert_eq!(
        err,
        Refusal::DuplicateElement {
            id: "sbb:ingest-0000".into()
        }
    );
}

#[test]
fn duplicate_manufacture_is_idempotent_not_additive() {
    let g = graph();
    let ad = admit(&g, &req(&g, "sbb:ingest-0000")).unwrap();
    let r1 = manufacture(&ad, &gen()).unwrap().receipt;
    let r2 = manufacture(&ad, &gen()).unwrap().receipt;
    assert_eq!(r1.receipt_digest, r2.receipt_digest);
}

// --- DoD 2: refusal of UNKNOWN / mutable / unqualified / ceilings ------------------

#[test]
fn unknown_sbb_is_refused() {
    let g = graph();
    assert_eq!(
        admit(&g, &req(&g, "sbb:ghost")).unwrap_err(),
        Refusal::UnknownSbb {
            id: "sbb:ghost".into()
        }
    );
}

#[test]
fn unknown_abb_is_refused() {
    let g = graph();
    let mut r = req(&g, "sbb:ingest-0000");
    r.abb = "abb:ghost".into();
    assert_eq!(
        admit(&g, &r).unwrap_err(),
        Refusal::UnknownAbb {
            id: "abb:ghost".into()
        }
    );
}

#[test]
fn sbb_with_unknown_identity_is_refused() {
    let mut g = graph();
    g.candidate_sbbs[0].digest = None;
    assert_eq!(
        admit(&g, &req(&g, "sbb:ingest-0000")).unwrap_err(),
        Refusal::SbbIdentityUnknown {
            sbb: "sbb:ingest-0000".into()
        }
    );
}

#[test]
fn mutable_sbb_is_refused() {
    let mut g = graph();
    g.candidate_sbbs[0].mutable = true;
    assert_eq!(
        admit(&g, &req(&g, "sbb:ingest-0000")).unwrap_err(),
        Refusal::SbbMutable {
            sbb: "sbb:ingest-0000".into()
        }
    );
}

#[test]
fn wrong_sbb_digest_is_refused() {
    let mut g = graph();
    g.candidate_sbbs[0].digest = Some("sha256:0000".into());
    assert!(matches!(
        admit(&g, &req(&g, "sbb:ingest-0000")).unwrap_err(),
        Refusal::SbbDigestMismatch { .. }
    ));
}

#[test]
fn unqualified_sbb_is_refused() {
    let mut g = graph();
    g.qualifications.retain(|q| q.sbb != "sbb:ingest-0000");
    assert_eq!(
        admit(&g, &req(&g, "sbb:ingest-0000")).unwrap_err(),
        Refusal::SbbUnqualified {
            sbb: "sbb:ingest-0000".into()
        }
    );
}

#[test]
fn refuted_qualification_dominates_a_positive_one() {
    let mut g = graph();
    let mut refuted = g.qualifications[0].clone();
    refuted.id = "qual:zz-refuted".into();
    refuted.verdict = Verdict::Refuted;
    g.qualifications.push(refuted);
    assert_eq!(
        admit(&g, &req(&g, "sbb:ingest-0000")).unwrap_err(),
        Refusal::SbbUnqualified {
            sbb: "sbb:ingest-0000".into()
        }
    );
}

#[test]
fn sbb_exceeding_artifact_ceiling_is_refused() {
    let mut g = graph();
    g.contracts[0].max_artifacts = 2;
    // Re-qualify against the changed contract so only the ceiling can refuse.
    let k = contract_digest(&g.contracts[0]);
    for q in &mut g.qualifications {
        q.contract_digest = k.clone();
    }
    assert!(matches!(
        admit(&g, &req(&g, "sbb:ingest-0000")).unwrap_err(),
        Refusal::ExceedsContract { .. }
    ));
}

#[test]
fn sbb_missing_required_port_is_refused() {
    let mut g = graph();
    g.candidate_sbbs[0]
        .provides_ports
        .remove("port:receipts-out");
    resign(&mut g.candidate_sbbs[0]);
    let d = g.candidate_sbbs[0].digest.clone().unwrap();
    g.qualifications[0].sbb_digest = d;
    assert!(matches!(
        admit(&g, &req(&g, "sbb:ingest-0000")).unwrap_err(),
        Refusal::ExceedsContract { .. }
    ));
}

#[test]
fn sbb_authority_above_contract_ceiling_is_refused() {
    let mut g = graph();
    g.candidate_sbbs[0].authority = Authority::Do;
    resign(&mut g.candidate_sbbs[0]);
    let d = g.candidate_sbbs[0].digest.clone().unwrap();
    g.qualifications[0].sbb_digest = d;
    assert!(matches!(
        admit(&g, &req(&g, "sbb:ingest-0000")).unwrap_err(),
        Refusal::ExceedsAuthorityCeiling {
            authority: Authority::Do,
            ..
        }
    ));
}

#[test]
fn contract_ceiling_of_do_is_clamped_to_construct() {
    // Court mutant M10: dropping `.min(Authority::Construct)` survived. A contract that
    // declares a DO ceiling must still refuse a DO-authority SBB at CONSTRUCT.
    let mut g = graph();
    g.contracts[0].authority_ceiling = Authority::Do;
    let k = contract_digest(&g.contracts[0]);
    g.candidate_sbbs[0].authority = Authority::Do;
    resign(&mut g.candidate_sbbs[0]);
    let d = g.candidate_sbbs[0].digest.clone().unwrap();
    for q in &mut g.qualifications {
        q.contract_digest = k.clone();
    }
    g.qualifications[0].sbb_digest = d;
    assert_eq!(
        admit(&g, &req(&g, "sbb:ingest-0000")).unwrap_err(),
        Refusal::ExceedsAuthorityCeiling {
            sbb: "sbb:ingest-0000".into(),
            authority: Authority::Do,
            ceiling: Authority::Construct,
        }
    );
    // Positive control: the CONSTRUCT-authority sibling under the same contract admits.
    assert!(admit(&g, &req(&g, "sbb:ingest-0001")).is_ok());
}

#[test]
fn admission_below_construct_is_refused() {
    // Court A5: an Admitted value licenses manufacture at CONSTRUCT, so admission at a
    // lower requested authority is refused; selection at SELECT goes through plan().
    let g = graph();
    for a in [Authority::None, Authority::Select] {
        let mut r = req(&g, "sbb:ingest-0000");
        r.requested_authority = a;
        assert_eq!(
            admit(&g, &r).unwrap_err(),
            Refusal::InsufficientAuthority { admitted: a }
        );
        assert!(matches!(
            plan(&g, "abb:event-ingest", a).unwrap(),
            Decision::Select {
                authority: Authority::Select,
                ..
            }
        ));
    }
    let ad = admit(&g, &req(&g, "sbb:ingest-0000")).unwrap();
    assert!(manufacture(&ad, &gen()).is_ok());
}

#[test]
fn unauthorized_do_request_is_refused() {
    let g = graph();
    let mut r = req(&g, "sbb:ingest-0000");
    r.requested_authority = Authority::Do;
    assert_eq!(
        admit(&g, &r).unwrap_err(),
        Refusal::AuthorityExceeded {
            requested: Authority::Do
        }
    );
    assert_eq!(
        plan(&g, "abb:event-ingest", Authority::Do).unwrap_err(),
        Refusal::AuthorityExceeded {
            requested: Authority::Do
        }
    );
}

// --- DoD 10: stale qualification and changed contract ------------------------------

#[test]
fn stale_qualification_is_refused_after_sbb_changes() {
    let mut g = graph();
    g.candidate_sbbs[0].artifacts[0]
        .template
        .push_str("// drift\n");
    resign(&mut g.candidate_sbbs[0]);
    assert!(matches!(
        admit(&g, &req(&g, "sbb:ingest-0000")).unwrap_err(),
        Refusal::StaleQualification { .. }
    ));
}

#[test]
fn changed_architecture_contract_invalidates_qualification() {
    let mut g = graph();
    g.contracts[0].version = "1.1.0".into();
    assert!(matches!(
        admit(&g, &req(&g, "sbb:ingest-0000")).unwrap_err(),
        Refusal::ContractChanged { .. }
    ));
}

// --- stale subject / wrong digest / malformed input --------------------------------

#[test]
fn stale_graph_subject_is_refused() {
    let g = graph();
    let r = req(&g, "sbb:ingest-0000");
    let mut moved = g.clone();
    moved.strategy.title.push_str(" (revised)");
    assert!(matches!(
        admit(&moved, &r).unwrap_err(),
        Refusal::GraphDigestMismatch { .. }
    ));
}

#[test]
fn malformed_inputs_are_refused_with_typed_refusals() {
    for bad in ["", "{", "[]", "{\"schema\":\"ggen.ea.graph.v1\"}"] {
        assert!(
            matches!(
                parse_graph(bad).unwrap_err(),
                Refusal::MalformedGraph { .. }
            ),
            "{bad:?}"
        );
    }
    let wrong_schema = FIXTURE.replace("ggen.ea.graph.v1", "ggen.ea.graph.v0");
    assert!(matches!(
        parse_graph(&wrong_schema).unwrap_err(),
        Refusal::MalformedGraph { .. }
    ));
    let smuggled = FIXTURE.replacen('{', "{\"authority\":\"DO\",", 1);
    assert!(matches!(
        parse_graph(&smuggled).unwrap_err(),
        Refusal::MalformedGraph { .. }
    ));
}

#[test]
fn dangling_references_are_refused() {
    let mut g = graph();
    g.abbs[0].capability = "cap:ghost".into();
    assert!(matches!(
        admit(
            &g,
            &Request {
                expected_graph_digest: None,
                ..req(&graph(), "sbb:ingest-0000")
            }
        )
        .unwrap_err(),
        Refusal::DanglingReference { .. }
    ));
}

/// Adds a second, fully wired ABB (`abb:other`) with its own contract.
fn with_second_abb(mut g: EaGraph) -> EaGraph {
    g.abbs.push(Abb {
        id: "abb:other".into(),
        capability: "cap:event-ingest".into(),
        contract: "contract:other".into(),
    });
    let mut k = g.contracts[0].clone();
    k.id = "contract:other".into();
    k.abb = "abb:other".into();
    g.contracts.push(k);
    g
}

#[test]
fn sbb_bound_to_another_abb_is_refused() {
    let mut g = with_second_abb(graph());
    g.candidate_sbbs[0].abb = "abb:other".into();
    resign(&mut g.candidate_sbbs[0]);
    assert_eq!(
        admit(&g, &req(&g, "sbb:ingest-0000")).unwrap_err(),
        Refusal::SbbAbbMismatch {
            sbb: "sbb:ingest-0000".into(),
            expected: "abb:event-ingest".into(),
            actual: "abb:other".into(),
        }
    );
}

#[test]
fn sbb_realizing_a_nonexistent_abb_is_a_dangling_reference() {
    // Court A4: plan() used to skip such an SBB silently instead of refusing the graph.
    let mut g = graph();
    g.candidate_sbbs[1].abb = "abb:ghost".into();
    resign(&mut g.candidate_sbbs[1]);
    let want = Refusal::DanglingReference {
        from: "sbb:ingest-0001".into(),
        to: "abb:ghost".into(),
    };
    assert_eq!(
        plan(&g, "abb:event-ingest", Authority::Construct).unwrap_err(),
        want
    );
    assert_eq!(admit(&g, &req(&g, "sbb:ingest-0000")).unwrap_err(), want);
}

// --- DoD 9: Pack is never ABB nor SBB ----------------------------------------------

#[test]
fn pack_named_as_abb_or_sbb_is_refused() {
    let mut g = graph();
    g.packs.push(Pack {
        id: "abb:event-ingest".into(),
    });
    assert!(matches!(
        admit(
            &g,
            &Request {
                expected_graph_digest: None,
                ..req(&graph(), "sbb:ingest-0000")
            }
        )
        .unwrap_err(),
        Refusal::PackConflation { .. }
    ));
    let mut g = graph();
    g.candidate_sbbs[0].pack = "sbb:ingest-0000".into();
    assert!(matches!(
        admit(
            &g,
            &Request {
                expected_graph_digest: None,
                ..req(&graph(), "sbb:ingest-0000")
            }
        )
        .unwrap_err(),
        Refusal::PackConflation { .. }
    ));
}

#[test]
fn pack_colliding_with_any_element_id_is_refused_without_a_pack_list() {
    // Court A1: with `packs` empty, a pack named after another SBB (or any other
    // element) was admitted and recorded in the receipt as pack=<element id>.
    for collide in [
        "sbb:ingest-0001",
        "sbb:ingest-0000",
        "abb:event-ingest",
        "contract:ingest",
        "cap:event-ingest",
        "qual:ingest-0001",
        "strategy:real-time-ops",
        "",
        "  ",
    ] {
        let mut g = graph();
        g.packs.clear();
        g.candidate_sbbs[0].pack = collide.into();
        resign(&mut g.candidate_sbbs[0]);
        let d = g.candidate_sbbs[0].digest.clone().unwrap();
        g.qualifications[0].sbb_digest = d;
        assert_eq!(
            admit(&g, &req(&g, "sbb:ingest-0000")).unwrap_err(),
            Refusal::PackConflation { id: collide.into() },
            "{collide:?}"
        );
        assert_eq!(
            plan(&g, "abb:event-ingest", Authority::Construct).unwrap_err(),
            Refusal::PackConflation { id: collide.into() },
            "{collide:?}"
        );
    }
    // Positive control: the same edit with a fresh pack id admits, so the refusal above
    // is caused by the collision and not by clearing the pack list or re-signing.
    let mut g = graph();
    g.packs.clear();
    g.candidate_sbbs[0].pack = "pack:fresh".into();
    resign(&mut g.candidate_sbbs[0]);
    let d = g.candidate_sbbs[0].digest.clone().unwrap();
    g.qualifications[0].sbb_digest = d;
    let m = manufacture(&admit(&g, &req(&g, "sbb:ingest-0000")).unwrap(), &gen()).unwrap();
    assert_eq!(m.receipt.pack, "pack:fresh");
}

// --- DoD 7: SELECT || MANUFACTURE, separate from DO --------------------------------

#[test]
fn plan_selects_lowest_admissible_candidate() {
    let g = graph();
    assert_eq!(
        plan(&g, "abb:event-ingest", Authority::Construct).unwrap(),
        Decision::Select {
            abb: "abb:event-ingest".into(),
            sbb: "sbb:ingest-0000".into(),
            authority: Authority::Select
        }
    );
    let mut g2 = g.clone();
    g2.candidate_sbbs[0].mutable = true;
    assert!(matches!(
        plan(&g2, "abb:event-ingest", Authority::Construct).unwrap(),
        Decision::Select { ref sbb, .. } if sbb == "sbb:ingest-0001"
    ));
}

#[test]
fn plan_falls_back_to_manufacture_with_every_refusal_recorded() {
    let mut g = graph();
    for s in &mut g.candidate_sbbs {
        s.mutable = true;
    }
    match plan(&g, "abb:event-ingest", Authority::Construct).unwrap() {
        Decision::Manufacture {
            refusals,
            authority,
            missing_ports,
            required_ports,
            ..
        } => {
            assert_eq!(refusals.len(), 2);
            assert!(refusals
                .iter()
                .all(|r| matches!(r, Refusal::SbbMutable { .. })));
            assert_eq!(authority, Authority::Construct);
            // Both SBBs provide both ports; they are refused for mutability only.
            assert!(missing_ports.is_empty(), "{missing_ports:?}");
            assert_eq!(required_ports.len(), 2);
        }
        other => panic!("expected MANUFACTURE, got {other:?}"),
    }
}

#[test]
fn manufacture_decision_reports_only_ports_no_candidate_provides() {
    // Stale qualifications everywhere: every candidate still provides every port.
    let mut g = graph();
    for q in &mut g.qualifications {
        q.sbb_digest = "sha256:stale".into();
    }
    match plan(&g, "abb:event-ingest", Authority::Construct).unwrap() {
        Decision::Manufacture {
            missing_ports,
            required_ports,
            refusals,
            ..
        } => {
            assert!(missing_ports.is_empty(), "{missing_ports:?}");
            assert_eq!(required_ports.len(), 2);
            assert!(refusals
                .iter()
                .all(|r| matches!(r, Refusal::StaleQualification { .. })));
        }
        other => panic!("expected MANUFACTURE, got {other:?}"),
    }
    // No candidate provides port:receipts-out: exactly that port is missing.
    let mut g = graph();
    for i in 0..g.candidate_sbbs.len() {
        g.candidate_sbbs[i]
            .provides_ports
            .remove("port:receipts-out");
        resign(&mut g.candidate_sbbs[i]);
        let d = g.candidate_sbbs[i].digest.clone().unwrap();
        g.qualifications[i].sbb_digest = d;
    }
    match plan(&g, "abb:event-ingest", Authority::Construct).unwrap() {
        Decision::Manufacture { missing_ports, .. } => assert_eq!(
            missing_ports,
            std::collections::BTreeSet::from(["port:receipts-out".to_string()])
        ),
        other => panic!("expected MANUFACTURE, got {other:?}"),
    }
}

// --- artifact-path and template adversaries ----------------------------------------

fn with_artifact(path: &str, template: &str) -> EaGraph {
    let mut g = graph();
    g.candidate_sbbs[0].artifacts[0] = ArtifactSpec {
        path: path.into(),
        template: template.into(),
    };
    resign(&mut g.candidate_sbbs[0]);
    let d = g.candidate_sbbs[0].digest.clone().unwrap();
    g.qualifications[0].sbb_digest = d;
    g
}

/// SELECT implies manufacturable: sbb:ingest-0000 is refused, so plan must pick 0001,
/// and the SBB it picks must manufacture.
fn assert_not_selected(g: &EaGraph) {
    match plan(g, "abb:event-ingest", Authority::Construct).unwrap() {
        Decision::Select { sbb, .. } => {
            assert_eq!(sbb, "sbb:ingest-0001");
            let ad = admit(g, &req(g, &sbb)).unwrap();
            assert!(manufacture(&ad, &gen()).is_ok());
        }
        other => panic!("expected SELECT of sbb:ingest-0001, got {other:?}"),
    }
}

#[test]
fn every_template_placeholder_the_generator_binds_is_accepted() {
    let t = "{{strategy}}|{{capability}}|{{abb}}|{{sbb}}|{{contract}}|{{contract_version}}|{{ graph_digest }}";
    let g = with_artifact("gen/all.rs", t);
    let m = manufacture(&admit(&g, &req(&g, "sbb:ingest-0000")).unwrap(), &gen()).unwrap();
    let a = m.artifacts.iter().find(|a| a.path == "gen/all.rs").unwrap();
    let body = a.bytes.lines().nth(1).unwrap();
    assert_eq!(
        body,
        format!(
            "strategy:real-time-ops|cap:event-ingest|abb:event-ingest|sbb:ingest-0000|contract:ingest|1.0.0|{}",
            g.digest()
        )
    );
}

#[test]
fn path_escape_is_refused() {
    for p in [
        "/etc/passwd",
        "../escape.rs",
        "gen/../../x",
        "gen//x",
        "gen/./x",
        "",
        "a\\b",
    ] {
        let g = with_artifact(p, "x");
        // Refused at admission (court A3), so plan never SELECTs it either.
        assert_eq!(
            admit(&g, &req(&g, "sbb:ingest-0000")).unwrap_err(),
            Refusal::UnsafeArtifactPath { path: p.into() },
            "{p:?}"
        );
        assert_not_selected(&g);
    }
}

#[test]
fn duplicate_artifact_path_is_refused() {
    let mut g = graph();
    let first = g.candidate_sbbs[0].artifacts[0].clone();
    g.candidate_sbbs[0].artifacts[1].path = first.path.clone();
    resign(&mut g.candidate_sbbs[0]);
    let d = g.candidate_sbbs[0].digest.clone().unwrap();
    g.qualifications[0].sbb_digest = d;
    assert_eq!(
        admit(&g, &req(&g, "sbb:ingest-0000")).unwrap_err(),
        Refusal::DuplicateArtifactPath { path: first.path }
    );
    assert_not_selected(&g);
}

#[test]
fn unbound_or_unterminated_placeholder_is_refused() {
    for t in ["{{secret}}", "{{abb", "ok {{ graph_digest }} then {{nope}}"] {
        let g = with_artifact("gen/x.rs", t);
        assert!(
            matches!(
                admit(&g, &req(&g, "sbb:ingest-0000")).unwrap_err(),
                Refusal::UnboundPlaceholder { .. }
            ),
            "{t:?}"
        );
        assert_not_selected(&g);
    }
}

// --- replay mismatch / tampering -----------------------------------------------------

#[test]
fn tampered_receipt_is_refused() {
    let g = graph();
    let mut r = manufacture(&admit(&g, &req(&g, "sbb:ingest-0000")).unwrap(), &gen())
        .unwrap()
        .receipt;
    r.outputs[0].digest = "sha256:forged".into();
    assert!(matches!(
        verify_receipt(&r).unwrap_err(),
        Refusal::ReceiptTampered { .. }
    ));
    assert!(matches!(
        replay(&r, &g, &gen()).unwrap_err(),
        Refusal::ReceiptTampered { .. }
    ));
}

#[test]
fn receipt_claiming_do_authority_is_refused_even_if_self_consistent() {
    let g = graph();
    let mut r = manufacture(&admit(&g, &req(&g, "sbb:ingest-0000")).unwrap(), &gen())
        .unwrap()
        .receipt;
    r.ceiling = Authority::Do;
    let fake = {
        let mut c = r.clone();
        c.receipt_digest = String::new();
        canonical_digest(&c)
    };
    r.receipt_digest = fake;
    assert!(matches!(
        verify_receipt(&r).unwrap_err(),
        Refusal::AuthorityExceeded { .. }
    ));
}

#[test]
fn replay_against_moved_graph_or_generator_is_refused() {
    let g = graph();
    let m = manufacture(&admit(&g, &req(&g, "sbb:ingest-0000")).unwrap(), &gen()).unwrap();
    let mut moved = g.clone();
    moved.strategy.title = "moved".into();
    assert_eq!(
        replay(&m.receipt, &moved, &gen()).unwrap_err(),
        Refusal::ReplayMismatch {
            field: "graph_digest".into()
        }
    );
    let other = Generator {
        id: "ggen-abb-sbb".into(),
        version: "26.9.27".into(),
    };
    assert_eq!(
        replay(&m.receipt, &g, &other).unwrap_err(),
        Refusal::ReplayMismatch {
            field: "generator".into()
        }
    );
}

// --- anti-vacuity: the positive witness depends on every guarded field -------------

#[test]
fn every_single_field_mutation_of_the_fixture_changes_the_graph_digest() {
    let base = graph().digest();
    let mutants: [fn(&mut EaGraph); 8] = [
        |g| g.strategy.id.push('x'),
        |g| g.capabilities[0].id.push('x'),
        |g| g.abbs[0].contract.push('x'),
        |g| g.contracts[0].max_artifacts += 1,
        |g| g.contracts[0].authority_ceiling = Authority::Select,
        |g| g.candidate_sbbs[1].pack.push('x'),
        |g| g.qualifications[1].verdict = Verdict::Refuted,
        |g| {
            g.packs.push(Pack {
                id: "pack:extra".into(),
            })
        },
    ];
    for (i, m) in mutants.iter().enumerate() {
        let mut g = graph();
        m(&mut g);
        assert_ne!(g.digest(), base, "mutant {i} left the digest unchanged");
    }
}
