//! CDT revocation local-severance test vector (spec: ggen
//! `docs/specs/cdt-revocation/README.md`). Real chain, real BLAKE3, no
//! mocks (Chicago). Grounded at affidavit `ee57f9d`.
//!
//! Run (in a scratch archive of affidavit):
//! `cargo test --features crypto-trust --test sj_record \
//!    cdt_revocation_local_severance_detection`

#[test]
fn cdt_revocation_local_severance_detection() {
    use affidavit::chain::recompute_chain;
    use affidavit::sj_record::{
        Authority, AuthorityCeiling, CommitRecord, ReplayCommand, SjCampaign, SjCampaignDraft,
        SjRecord, StandingValue,
    };

    // -- Local check walk (spec section 2.2): resolve ancestors to their
    //    CURRENT canonical versions, fold the path log, compare against the
    //    admission-time root table. Sub-ms, offline.
    fn path_log<'a>(
        current: &[&'a SjRecord],
        leaf: &SjRecord,
    ) -> Vec<affidavit::types::OperationEvent> {
        let by_id: std::collections::BTreeMap<&str, &SjRecord> = current
            .iter()
            .map(|r| (r.document.work_order_id.as_str(), *r))
            .collect();
        let mut order: Vec<&SjRecord> = vec![];
        let mut cur = leaf;
        loop {
            order.push(cur);
            match cur.document.replay_binding.predecessor_work_order_ids.as_slice() {
                [] => break,
                [one] => {
                    cur = by_id[one.as_str()];
                }
                _ => panic!("vector uses single-parent chains"),
            }
        }
        order.reverse();
        order
            .iter()
            .flat_map(|r| r.base.events.clone())
            .collect()
    }

    fn derivation_digest(events: &[affidavit::types::OperationEvent]) -> String {
        recompute_chain(events)
            .expect("canonical events always fold")
            .as_hex()
            .to_string()
    }

    // -- 5-record chain WO1 <- WO2 <- WO3 <- WO4 <- WO5.
    fn record_for(n: usize, tag: &str, preds: Vec<String>) -> SjRecord {
        let draft = SjCampaignDraft {
            work_order_id: format!("WO{n}"),
            origin_ceiling: Some(AuthorityCeiling::Construct),
            origin_grant: "lease-cdt".into(),
            origin_actor: "lane:cdt-revocation".into(),
            provider_name: "affidavit.cli".into(),
            provider_execution_id: format!("cdt-exec-{tag}{n}"),
            subject: "cdt revocation vector".into(),
            repo: "affidavit".into(),
            subject_sha: format!("{n:040}"),
            base_sha: "b".repeat(40),
            commits: vec![CommitRecord {
                sha: format!("{tag}{n}{}", "c".repeat(38)),
                summary: format!("record {n}"),
                court_results: vec!["verify:ACCEPT".into()],
            }],
            residue_declaration: "test vector residue".into(),
            files_changed: vec!["docs/specs/cdt-revocation/".into()],
            remote_effects: vec!["push:branch".into()],
            replay_commands: vec![ReplayCommand {
                cmd: format!("check WO{n}"),
                exit: 0,
                cwd: "/tmp/cdt-scratch".into(),
                summary: None,
                output_sha256: None,
            }],
            durable_location: None,
            standing: StandingValue::Alive,
            derived_from: "cargo test --features crypto-trust --test sj_record".into(),
            broken_term: None,
            predecessor_work_order_ids: preds,
            authority: Authority {
                ceiling: AuthorityCeiling::Do,
                grant: "lease-cdt".into(),
                actor: "lane:cdt-revocation".into(),
            },
        };
        SjCampaign::new(draft)
            .expect("draft admits")
            .finalize()
            .expect("record finalizes")
    }

    let r1 = record_for(1, "a", vec![]);
    let r2 = record_for(2, "a", vec!["WO1".into()]);
    let r3 = record_for(3, "a", vec!["WO2".into()]);
    let r4 = record_for(4, "a", vec!["WO3".into()]);
    let r5 = record_for(5, "a", vec!["WO4".into()]);
    let admission = vec![&r1, &r2, &r3, &r4, &r5];

    // -- Admission roots X1..X5 (hand-verified math, spec section 4.2).
    let root_table: Vec<String> = admission
        .iter()
        .map(|r| derivation_digest(&path_log(&admission, r)))
        .collect();

    // Pre-revocation: every record's local check passes.
    for (i, r) in admission.iter().enumerate() {
        assert_eq!(
            derivation_digest(&path_log(&admission, r)),
            root_table[i],
            "WO{} must pass pre-revocation",
            i + 1
        );
    }

    // -- Revoke node 2: supersede WO2 with a distinct-commit WO2'. The
    //    canonical version of WO2 swaps to WO2'; ledger current versions
    //    become [R1, R2', R3, R4, R5].
    let r2_prime = record_for(2, "f", vec!["WO1".into()]);
    // Distinctness precondition (spec section 2.3): E2' != E2.
    assert_ne!(
        r2.base.events, r2_prime.base.events,
        "revocation must change bytes"
    );

    let current = vec![&r1, &r2_prime, &r3, &r4, &r5];
    let new_root_table: Vec<String> = current
        .iter()
        .map(|r| derivation_digest(&path_log(&current, r)))
        .collect();

    // -- Post-revocation local checks against the ADMISSION-TIME root table.
    //    WO1 passes (sibling of the severed subtree stays valid); WO3, WO4,
    //    WO5 SEVERED. Edge identified at depth 2 (spec section 4.2).
    assert_eq!(
        derivation_digest(&path_log(&current, &r1)),
        root_table[0],
        "WO1 unaffected"
    );
    for i in [2usize, 3, 4] {
        assert_ne!(
            derivation_digest(&path_log(&current, current[i])),
            root_table[i],
            "WO{} must detect severance locally",
            i + 1
        );
    }
    // The refreshed canonical lineage is internally consistent.
    for i in 0..5 {
        assert_eq!(
            derivation_digest(&path_log(&current, current[i])),
            new_root_table[i],
            "current canonical lineage must be internally consistent"
        );
    }
    // Edge identification: first diverging prefix depth is 2 (WO2 -> WO3).
    let severed_at = (1..=5).find(|&d| {
        derivation_digest(&path_log(&current, current[d - 1])) != root_table[d - 1]
    });
    assert_eq!(severed_at, Some(2), "severed edge is WO2 -> WO3");
}
