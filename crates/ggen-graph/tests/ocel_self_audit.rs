#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
//! Integration test for projecting, querying, and verifying the self-audit log in `DeterministicGraph`.

use ggen_graph::ocel::{
    extract_self_audit, generate_self_audit_log, project_self_audit, query_relationship,
};
use ggen_graph::DeterministicGraph;

#[test]
fn test_integration_self_audit_graph_projection_and_sparql(
) -> Result<(), Box<dyn std::error::Error>> {
    // 1. Generate the self-audit log
    let log = generate_self_audit_log();
    assert!(!log.objects.is_empty(), "Log objects should not be empty");
    assert!(!log.events.is_empty(), "Log events should not be empty");

    // 2. Project into DeterministicGraph
    let graph = DeterministicGraph::new()?;
    project_self_audit(&graph, &log)?;

    // 3. Extract back and verify equivalence
    let extracted = extract_self_audit(&graph)?;
    assert_eq!(
        extracted.objects.len(),
        log.objects.len(),
        "Object count mismatch"
    );
    assert_eq!(
        extracted.events.len(),
        log.events.len(),
        "Event count mismatch"
    );

    // 4. Query relationships using safe qualifiers
    let checks_relations = query_relationship(&graph, "--checks-->")?;
    assert!(
        !checks_relations.is_empty(),
        "Should find --checks--> relationships"
    );
    assert!(
        checks_relations.contains(&(
            "ev_checkpoint_evaluated".to_string(),
            "obj_cp_verify".to_string()
        )),
        "Should find ev_checkpoint_evaluated checking obj_cp_verify"
    );

    let satisfied_relations = query_relationship(&graph, "--satisfied_by-->")?;
    assert!(
        !satisfied_relations.is_empty(),
        "Should find --satisfied_by--> relationships"
    );
    assert!(
        satisfied_relations.contains(&(
            "ev_req_declared".to_string(),
            "obj_prd_compliance".to_string()
        )),
        "Should find ev_req_declared satisfied by obj_prd_compliance"
    );

    // 5. SPARQL query to verify chronological event ordering (CheckpointEvaluated precedes CheckpointPromoted)
    let sparql_query = r#"
        SELECT ?eval ?promo ?eval_ts ?promo_ts
        WHERE {
            ?eval <http://www.ocel-standard.org/ns#activity> "CheckpointEvaluated" .
            ?eval <http://www.ocel-standard.org/ns#timestamp> ?eval_ts .
            ?promo <http://www.ocel-standard.org/ns#activity> "CheckpointPromoted" .
            ?promo <http://www.ocel-standard.org/ns#timestamp> ?promo_ts .
        }
    "#;

    let mut found_ordered = false;
    if let oxigraph::sparql::QueryResults::Solutions(solutions) = graph.query(sparql_query)? {
        for sol_res in solutions {
            let sol = sol_res?;
            if let (
                Some(oxigraph::model::Term::NamedNode(_eval_node)),
                Some(oxigraph::model::Term::NamedNode(_promo_node)),
                Some(oxigraph::model::Term::Literal(eval_ts_lit)),
                Some(oxigraph::model::Term::Literal(promo_ts_lit)),
            ) = (
                sol.get("eval"),
                sol.get("promo"),
                sol.get("eval_ts"),
                sol.get("promo_ts"),
            ) {
                let eval_ts = eval_ts_lit.value();
                let promo_ts = promo_ts_lit.value();

                // Assert lexicographical chronological order (RFC3339 strings are sortable)
                assert!(
                    eval_ts < promo_ts,
                    "CheckpointEvaluated ({}) must occur before CheckpointPromoted ({})",
                    eval_ts,
                    promo_ts
                );
                found_ordered = true;
            }
        }
    }
    assert!(
        found_ordered,
        "Should find matching evaluation and promotion events to verify ordering"
    );

    Ok(())
}
