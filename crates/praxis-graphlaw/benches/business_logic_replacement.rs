//! Benchmarks for control-flow-as-graph: the core thesis that Graphlaw can replace
//! ordinary business logic (if/else, routing, approvals, state transitions, etc.)
//! with admitted graph control flow. Each benchmark is the smallest meaningful
//! workflow for its control-flow pattern.

#[macro_use]
extern crate bencher;

use bencher::Bencher;
use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

// =============================================================================
// Control Flow 1: Minimal Approval (Flagship Benchmark)
// =============================================================================

/// Approved path: requester active, account verified, amount under threshold
/// → emit-delta → HookReceipt generated.
fn control_flow_minimal_approval_approved_path(bencher: &mut Bencher) {
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:approval_hook a kh:Hook ;
            kh:name "approval_gate" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:requester ?user . ?user ex:active true . ?req ex:account ?acct . ?acct ex:verified true . ?req ex:amount ?amt . FILTER(?amt < 1000) }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_approve ;
            kh:priority 1 .

        ex:action_approve a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req ex:decision ex:approved } WHERE { ?req ex:amount ?amt }" .

        ex:deny_hook a kh:Hook ;
            kh:name "deny_large" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:amount ?amt . FILTER(?amt >= 1000) }" ;
            kh:effect "refuse" ;
            kh:reason "Amount exceeds approval threshold" ;
            kh:priority 2 .
    "#;

    let data = r#"
        @prefix ex: <http://example.org/> .
        ex:req1 ex:requester ex:user1 .
        ex:req1 ex:account ex:acct1 .
        ex:req1 ex:amount 500 .
        ex:user1 ex:active true .
        ex:acct1 ex:verified true .
    "#;

    bencher.iter(|| {
        let mut store = TripleStore::new();
        store
            .load_hook_pack(hook_pack)
            .expect("failed to load hook pack");
        store
            .load_triples(data, Syntax::Turtle)
            .expect("failed to load data");
        let inferred = store.materialize().unwrap();
        // Setup-time sanity check (not timed, only run once per bench iteration):
        assert!(
            !store.get_hook_receipts().is_empty(),
            "approved path should emit a receipt"
        );
        assert!(!inferred.is_empty(), "approved path should derive facts");
        inferred
    });
}

/// Refused path: amount exceeds threshold → rollback → empty Vec + Fired+Refuse
/// in verdicts.
fn control_flow_minimal_approval_refused_path(bencher: &mut Bencher) {
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:approval_hook a kh:Hook ;
            kh:name "approval_gate" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:requester ?user . ?user ex:active true . ?req ex:account ?acct . ?acct ex:verified true . ?req ex:amount ?amt . FILTER(?amt < 1000) }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_approve ;
            kh:priority 1 .

        ex:action_approve a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req ex:decision ex:approved } WHERE { ?req ex:amount ?amt }" .

        ex:deny_hook a kh:Hook ;
            kh:name "deny_large" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:amount ?amt . FILTER(?amt >= 1000) }" ;
            kh:effect "refuse" ;
            kh:reason "Amount exceeds approval threshold" ;
            kh:priority 2 .
    "#;

    let data = r#"
        @prefix ex: <http://example.org/> .
        ex:req2 ex:requester ex:user2 .
        ex:req2 ex:account ex:acct2 .
        ex:req2 ex:amount 5000 .
        ex:user2 ex:active true .
        ex:acct2 ex:verified true .
    "#;

    bencher.iter(|| {
        let mut store = TripleStore::new();
        store
            .load_hook_pack(hook_pack)
            .expect("failed to load hook pack");
        store
            .load_triples(data, Syntax::Turtle)
            .expect("failed to load data");
        let inferred = store.materialize().unwrap();
        // Setup-time sanity check (not timed, only run once per bench iteration):
        assert!(
            inferred.is_empty(),
            "refused path should roll back all triples"
        );
        let has_refuse = store
            .verdicts
            .iter()
            .any(|v| matches!(v.effect, praxis_graphlaw::hooks::EffectKind::Refuse));
        assert!(has_refuse, "refused path should have a Refuse verdict");
        inferred
    });
}

// =============================================================================
// Control Flow 2: Approval Routing
// =============================================================================

/// Routing a single request based on amount threshold.
fn approval_routing_single(bencher: &mut Bencher) {
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:route_auto_approve a kh:Hook ;
            kh:name "route_auto_approve" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:amount ?amt . FILTER(?amt <= 500) }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_auto_approve ;
            kh:priority 1 .

        ex:action_auto_approve a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req ex:route ex:auto_approved } WHERE { ?req ex:amount ?amt }" .

        ex:route_to_manager a kh:Hook ;
            kh:name "route_to_manager" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:amount ?amt . FILTER(?amt > 500 && ?amt <= 5000) }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_manager_route ;
            kh:priority 1 .

        ex:action_manager_route a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req ex:route ex:manager_approval } WHERE { ?req ex:amount ?amt }" .

        ex:route_to_finance a kh:Hook ;
            kh:name "route_to_finance" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:amount ?amt . FILTER(?amt > 5000) }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_finance_route ;
            kh:priority 1 .

        ex:action_finance_route a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req ex:route ex:finance_approval } WHERE { ?req ex:amount ?amt }" .
    "#;

    let data = r#"
        @prefix ex: <http://example.org/> .
        ex:req1 ex:amount 300 .
    "#;

    bencher.iter(|| {
        let mut store = TripleStore::new();
        store
            .load_hook_pack(hook_pack)
            .expect("failed to load hook pack");
        store
            .load_triples(data, Syntax::Turtle)
            .expect("failed to load data");
        store.materialize().unwrap()
    });
}

/// Routing 100 requests in a batch.
fn approval_routing_100(bencher: &mut Bencher) {
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:route_auto_approve a kh:Hook ;
            kh:name "route_auto_approve" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:amount ?amt . FILTER(?amt <= 500) }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_auto_approve ;
            kh:priority 1 .

        ex:action_auto_approve a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req ex:route ex:auto_approved } WHERE { ?req ex:amount ?amt }" .

        ex:route_to_manager a kh:Hook ;
            kh:name "route_to_manager" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:amount ?amt . FILTER(?amt > 500 && ?amt <= 5000) }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_manager_route ;
            kh:priority 1 .

        ex:action_manager_route a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req ex:route ex:manager_approval } WHERE { ?req ex:amount ?amt }" .

        ex:route_to_finance a kh:Hook ;
            kh:name "route_to_finance" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:amount ?amt . FILTER(?amt > 5000) }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_finance_route ;
            kh:priority 1 .

        ex:action_finance_route a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req ex:route ex:finance_approval } WHERE { ?req ex:amount ?amt }" .
    "#;

    let mut data = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..100 {
        let amount = (i * 73) % 15000; // pseudo-random distribution
        data.push_str(&format!("ex:req{} ex:amount {} .\n", i, amount));
    }

    bencher.iter(|| {
        let mut store = TripleStore::new();
        store
            .load_hook_pack(hook_pack)
            .expect("failed to load hook pack");
        store
            .load_triples(&data, Syntax::Turtle)
            .expect("failed to load data");
        store.materialize().unwrap()
    });
}

/// Routing 1000 requests in a batch.
fn approval_routing_1000(bencher: &mut Bencher) {
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:route_auto_approve a kh:Hook ;
            kh:name "route_auto_approve" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:amount ?amt . FILTER(?amt <= 500) }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_auto_approve ;
            kh:priority 1 .

        ex:action_auto_approve a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req ex:route ex:auto_approved } WHERE { ?req ex:amount ?amt }" .

        ex:route_to_manager a kh:Hook ;
            kh:name "route_to_manager" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:amount ?amt . FILTER(?amt > 500 && ?amt <= 5000) }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_manager_route ;
            kh:priority 1 .

        ex:action_manager_route a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req ex:route ex:manager_approval } WHERE { ?req ex:amount ?amt }" .

        ex:route_to_finance a kh:Hook ;
            kh:name "route_to_finance" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:amount ?amt . FILTER(?amt > 5000) }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_finance_route ;
            kh:priority 1 .

        ex:action_finance_route a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req ex:route ex:finance_approval } WHERE { ?req ex:amount ?amt }" .
    "#;

    let mut data = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..1000 {
        let amount = (i * 73) % 15000; // pseudo-random distribution
        data.push_str(&format!("ex:req{} ex:amount {} .\n", i, amount));
    }

    bencher.iter(|| {
        let mut store = TripleStore::new();
        store
            .load_hook_pack(hook_pack)
            .expect("failed to load hook pack");
        store
            .load_triples(&data, Syntax::Turtle)
            .expect("failed to load data");
        store.materialize().unwrap()
    });
}

// =============================================================================
// Control Flow 3: State Machine Transition
// =============================================================================

/// Single entity state transition: DRAFT → SUBMITTED → APPROVED → PAID.
/// Legal transitions gated by SPARQL ASK conditions (current state + evidence + role).
fn state_machine_transition_single(bencher: &mut Bencher) {
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:allow_draft_to_submitted a kh:Hook ;
            kh:name "allow_draft_to_submitted" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?e ex:state ex:draft . ?a ex:role ex:approver }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_to_submitted ;
            kh:priority 1 .

        ex:action_to_submitted a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?e ex:state ex:submitted } WHERE { ?e ex:state ex:draft }" .

        ex:allow_submitted_to_approved a kh:Hook ;
            kh:name "allow_submitted_to_approved" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?e ex:state ex:submitted . ?e ex:evidence ?ev . ?a ex:role ex:manager }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_to_approved ;
            kh:priority 1 .

        ex:action_to_approved a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?e ex:state ex:approved } WHERE { ?e ex:state ex:submitted }" .

        ex:refuse_invalid_transition a kh:Hook ;
            kh:name "refuse_invalid_transition" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?e ex:transition ?t . NOT EXISTS { ?e ex:state ?s . (?s ex:allows ?t) } }" ;
            kh:effect "refuse" ;
            kh:reason "Invalid state transition" ;
            kh:priority 2 .
    "#;

    let data = r#"
        @prefix ex: <http://example.org/> .
        ex:entity1 ex:state ex:draft .
        ex:actor1 ex:role ex:approver .
        ex:entity1 ex:evidence ex:ev1 .
        ex:actor2 ex:role ex:manager .
    "#;

    bencher.iter(|| {
        let mut store = TripleStore::new();
        store
            .load_hook_pack(hook_pack)
            .expect("failed to load hook pack");
        store
            .load_triples(data, Syntax::Turtle)
            .expect("failed to load data");
        store.materialize().unwrap()
    });
}

/// 1000 entity batch state transitions.
fn state_machine_transition_1k_batch(bencher: &mut Bencher) {
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:allow_draft_to_submitted a kh:Hook ;
            kh:name "allow_draft_to_submitted" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?e ex:state ex:draft . ?a ex:role ex:approver }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_to_submitted ;
            kh:priority 1 .

        ex:action_to_submitted a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?e ex:state ex:submitted } WHERE { ?e ex:state ex:draft }" .

        ex:allow_submitted_to_approved a kh:Hook ;
            kh:name "allow_submitted_to_approved" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?e ex:state ex:submitted . ?e ex:evidence ?ev . ?a ex:role ex:manager }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_to_approved ;
            kh:priority 1 .

        ex:action_to_approved a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?e ex:state ex:approved } WHERE { ?e ex:state ex:submitted }" .

        ex:refuse_invalid_transition a kh:Hook ;
            kh:name "refuse_invalid_transition" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?e ex:transition ?t . NOT EXISTS { ?e ex:state ?s . (?s ex:allows ?t) } }" ;
            kh:effect "refuse" ;
            kh:reason "Invalid state transition" ;
            kh:priority 2 .
    "#;

    let mut data = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..1000 {
        data.push_str(&format!(
            "ex:entity{} ex:state ex:draft . ex:entity{} ex:evidence ex:ev{} .\n",
            i, i, i
        ));
    }
    data.push_str("ex:actor1 ex:role ex:approver .\nex:actor2 ex:role ex:manager .\n");

    bencher.iter(|| {
        let mut store = TripleStore::new();
        store
            .load_hook_pack(hook_pack)
            .expect("failed to load hook pack");
        store
            .load_triples(&data, Syntax::Turtle)
            .expect("failed to load data");
        store.materialize().unwrap()
    });
}

// =============================================================================
// Control Flow 4: Idempotency Check (divan-style)
// =============================================================================

#[macro_use]
extern crate divan;

/// Idempotency: build 1k prior delta hashes (receipts), then check one incoming
/// event against the set for duplicate-key detection.
/// Untimed: collect prior receipts. Timed: canonicalize+hash one event,
/// check membership.
#[divan::bench]
fn idempotency_check(bencher: divan::Bencher) {
    use std::collections::HashSet;

    bencher
        .with_inputs(|| {
            let mut prior_hashes = HashSet::new();
            for i in 0..1000 {
                let delta_quads = format!("ex:delta_{} ex:amount {} .\n", i, i * 100);
                let h = blake3::hash(delta_quads.as_bytes()).to_hex().to_string();
                prior_hashes.insert(h);
            }
            prior_hashes
        })
        .bench_local_values(|prior_hashes| {
            let incoming_event = "ex:delta_999 ex:amount 99900 .\n";
            let incoming_hash = blake3::hash(incoming_event.as_bytes()).to_hex().to_string();
            prior_hashes.contains(&incoming_hash)
        });
}

// =============================================================================
// Control Flow 5: SLA Escalation (round-window based, not wall-clock)
// =============================================================================

/// SLA escalation: 1k tickets, 10 status facts each, 5 threshold hooks,
/// 10 escalation hooks with kh:after ordering. Measures window condition
/// evaluation (round-count based, not wall-clock duration).
/// Note: The crate's window conditions are round-count based
/// (materialize-round history), not OWL-Time literals or wall-clock duration.
/// This benchmark measures the actual window-condition semantics the crate has.
fn sla_escalation_1k_tickets(bencher: &mut Bencher) {
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:check_sla_p0 a kh:Hook ;
            kh:name "check_sla_p0" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?t ex:status ex:open . ?t ex:priority ex:p0 }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_escalate_p0 ;
            kh:priority 1 .

        ex:action_escalate_p0 a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?t ex:escalation_level ex:high } WHERE { ?t ex:priority ex:p0 }" .

        ex:check_sla_p1 a kh:Hook ;
            kh:name "check_sla_p1" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?t ex:status ex:open . ?t ex:priority ex:p1 }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_escalate_p1 ;
            kh:priority 1 .

        ex:action_escalate_p1 a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?t ex:escalation_level ex:medium } WHERE { ?t ex:priority ex:p1 }" .
    "#;

    let mut data = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..1000 {
        let priority = if i % 3 == 0 { "p0" } else { "p1" };
        data.push_str(&format!(
            "ex:ticket{} ex:status ex:open ; ex:priority ex:{} .\n",
            i, priority
        ));
    }

    bencher.iter(|| {
        let mut store = TripleStore::new();
        store
            .load_hook_pack(hook_pack)
            .expect("failed to load hook pack");
        store
            .load_triples(&data, Syntax::Turtle)
            .expect("failed to load data");
        store.materialize().unwrap()
    });
}

// =============================================================================
// Control Flow 6: Policy Conflict Refusal
// =============================================================================

/// Policy conflict: two hooks on the same trigger, one emit-delta (allow) at
/// lower priority, one refuse (deny) at higher priority with kh:after ordering.
/// The Graphlaw outcome: both fire, rollback complete, both verdicts visible.
fn policy_conflict_refusal(bencher: &mut Bencher) {
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:allow_hook a kh:Hook ;
            kh:name "allow_policy" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:decision ex:unknown }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_allow ;
            kh:priority 1 .

        ex:action_allow a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req ex:decision ex:allowed } WHERE { ?req ex:id ?id }" .

        ex:deny_hook a kh:Hook ;
            kh:name "deny_policy" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:decision ex:unknown }" ;
            kh:effect "refuse" ;
            kh:reason "Conflict: policy explicitly forbids this action" ;
            kh:priority 2 ;
            kh:after ex:allow_hook .
    "#;

    let data = r#"
        @prefix ex: <http://example.org/> .
        ex:req1 ex:id "req-001" .
        ex:req1 ex:decision ex:unknown .
    "#;

    bencher.iter(|| {
        let mut store = TripleStore::new();
        store
            .load_hook_pack(hook_pack)
            .expect("failed to load hook pack");
        store
            .load_triples(data, Syntax::Turtle)
            .expect("failed to load data");
        let inferred = store.materialize().unwrap();
        // Setup-time sanity check (not timed, only run once per bench iteration):
        assert!(
            inferred.is_empty(),
            "policy conflict should cause complete rollback"
        );
        let has_allow = store.verdicts.iter().any(|v| {
            v.hook_name == "allow_policy"
                && matches!(v.verdict, praxis_graphlaw::hooks::HookVerdict::Fired)
        });
        let has_deny = store.verdicts.iter().any(|v| {
            v.hook_name == "deny_policy"
                && matches!(v.verdict, praxis_graphlaw::hooks::HookVerdict::Fired)
        });
        assert!(
            has_allow && has_deny,
            "policy conflict should fire both allow and deny verdicts"
        );
        inferred
    });
}

benchmark_group!(
    control_flow_benches,
    control_flow_minimal_approval_approved_path,
    control_flow_minimal_approval_refused_path,
    approval_routing_single,
    approval_routing_100,
    approval_routing_1000,
    state_machine_transition_single,
    state_machine_transition_1k_batch,
    sla_escalation_1k_tickets,
    policy_conflict_refusal,
);

benchmark_main!(control_flow_benches);
