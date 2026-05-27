use crate::ocel::prov_types::ProvDerivation;
use crate::ocel::{OcelEvent, OcelLog, OcelObject, OcelObjectRef};
use crate::ocel::{ProvActivity, ProvAgent, ProvDocument, ProvEntity, ProvGeneration, ProvUsage};
use crate::DeterministicGraph;
use crate::GraphError;
use oxigraph::model::{GraphName, Literal, NamedNode, NamedOrBlankNode, Quad, Term};
use std::collections::HashMap;

/// Projector for converting between OCEL/PROV structured data and RDF graphs.
pub struct EvidenceProjector;

impl EvidenceProjector {
    /// Projects an `OcelLog` into the given `DeterministicGraph`.
    pub fn project_ocel(graph: &DeterministicGraph, log: &OcelLog) -> Result<(), GraphError> {
        let type_pred = NamedNode::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
            .map_err(|e| GraphError::Other(e.to_string()))?;
        let object_class = NamedNode::new("http://www.ocel-standard.org/ns#Object")
            .map_err(|e| GraphError::Other(e.to_string()))?;
        let event_class = NamedNode::new("http://www.ocel-standard.org/ns#Event")
            .map_err(|e| GraphError::Other(e.to_string()))?;
        let activity_class = NamedNode::new("http://www.w3.org/ns/prov#Activity")
            .map_err(|e| GraphError::Other(e.to_string()))?;

        let object_type_pred = NamedNode::new("http://www.ocel-standard.org/ns#objectType")
            .map_err(|e| GraphError::Other(e.to_string()))?;
        let activity_pred = NamedNode::new("http://www.ocel-standard.org/ns#activity")
            .map_err(|e| GraphError::Other(e.to_string()))?;
        let timestamp_pred = NamedNode::new("http://www.ocel-standard.org/ns#timestamp")
            .map_err(|e| GraphError::Other(e.to_string()))?;
        let started_at_pred = NamedNode::new("http://www.w3.org/ns/prov#startedAtTime")
            .map_err(|e| GraphError::Other(e.to_string()))?;
        let has_object_ref_pred = NamedNode::new("http://www.ocel-standard.org/ns#has_object_ref")
            .map_err(|e| GraphError::Other(e.to_string()))?;

        // 1. Project Objects
        for obj in &log.objects {
            let obj_uri = format!("http://ggen.dev/ocel/object/{}", obj.id);
            let obj_node =
                NamedNode::new(&obj_uri).map_err(|e| GraphError::Other(e.to_string()))?;
            let subject = NamedOrBlankNode::NamedNode(obj_node.clone());

            // obj a ocel:Object
            graph.insert_quad(&Quad::new(
                subject.clone(),
                type_pred.clone(),
                Term::NamedNode(object_class.clone()),
                GraphName::DefaultGraph,
            ))?;

            // obj ocel:objectType "type"
            graph.insert_quad(&Quad::new(
                subject.clone(),
                object_type_pred.clone(),
                Term::Literal(Literal::new_simple_literal(&obj.r#type)),
                GraphName::DefaultGraph,
            ))?;

            // obj attributes
            for (key, val) in &obj.attributes {
                let attr_pred =
                    NamedNode::new(format!("http://www.ocel-standard.org/ns#attribute/{}", key))
                        .map_err(|e| GraphError::Other(e.to_string()))?;
                graph.insert_quad(&Quad::new(
                    subject.clone(),
                    attr_pred,
                    Term::Literal(Literal::new_simple_literal(val)),
                    GraphName::DefaultGraph,
                ))?;
            }
        }

        // 2. Project Events
        for ev in &log.events {
            let ev_uri = format!("http://ggen.dev/ocel/event/{}", ev.id);
            let ev_node = NamedNode::new(&ev_uri).map_err(|e| GraphError::Other(e.to_string()))?;
            let subject = NamedOrBlankNode::NamedNode(ev_node.clone());

            // ev a ocel:Event
            graph.insert_quad(&Quad::new(
                subject.clone(),
                type_pred.clone(),
                Term::NamedNode(event_class.clone()),
                GraphName::DefaultGraph,
            ))?;

            // ev a prov:Activity
            graph.insert_quad(&Quad::new(
                subject.clone(),
                type_pred.clone(),
                Term::NamedNode(activity_class.clone()),
                GraphName::DefaultGraph,
            ))?;

            // ev ocel:activity "activity"
            graph.insert_quad(&Quad::new(
                subject.clone(),
                activity_pred.clone(),
                Term::Literal(Literal::new_simple_literal(&ev.activity)),
                GraphName::DefaultGraph,
            ))?;

            // ev ocel:timestamp "timestamp"
            graph.insert_quad(&Quad::new(
                subject.clone(),
                timestamp_pred.clone(),
                Term::Literal(Literal::new_simple_literal(ev.timestamp.to_rfc3339())),
                GraphName::DefaultGraph,
            ))?;

            // ev prov:startedAtTime "timestamp"
            graph.insert_quad(&Quad::new(
                subject.clone(),
                started_at_pred.clone(),
                Term::Literal(Literal::new_simple_literal(ev.timestamp.to_rfc3339())),
                GraphName::DefaultGraph,
            ))?;

            // ev attributes
            for (key, val) in &ev.attributes {
                let attr_pred =
                    NamedNode::new(format!("http://www.ocel-standard.org/ns#attribute/{}", key))
                        .map_err(|e| GraphError::Other(e.to_string()))?;
                graph.insert_quad(&Quad::new(
                    subject.clone(),
                    attr_pred,
                    Term::Literal(Literal::new_simple_literal(val)),
                    GraphName::DefaultGraph,
                ))?;
            }

            // ev object references
            for obj_ref in &ev.objects {
                let obj_uri = format!("http://ggen.dev/ocel/object/{}", obj_ref.id);
                let obj_node =
                    NamedNode::new(&obj_uri).map_err(|e| GraphError::Other(e.to_string()))?;

                // ev ocel:has_object_ref obj
                graph.insert_quad(&Quad::new(
                    subject.clone(),
                    has_object_ref_pred.clone(),
                    Term::NamedNode(obj_node.clone()),
                    GraphName::DefaultGraph,
                ))?;

                // ev ocel:qualifier_qual obj
                if let Some(ref qual) = obj_ref.qualifier {
                    let encoded_qual = qual.replace('<', "%3C").replace('>', "%3E");
                    let qual_iri =
                        format!("http://www.ocel-standard.org/ns#qualifier_{}", encoded_qual);
                    let qual_pred = NamedNode::new(&qual_iri).map_err(|e| {
                        GraphError::Other(format!(
                            "Failed to create NamedNode for '{}': {}",
                            qual_iri, e
                        ))
                    })?;
                    graph.insert_quad(&Quad::new(
                        subject.clone(),
                        qual_pred,
                        Term::NamedNode(obj_node),
                        GraphName::DefaultGraph,
                    ))?;
                }
            }
        }

        Ok(())
    }

    /// Extracts an `OcelLog` from the given `DeterministicGraph`.
    pub fn extract_ocel(graph: &DeterministicGraph) -> Result<OcelLog, GraphError> {
        let mut log = OcelLog::new();

        // Let's query all objects
        let query_objects = r#"
            PREFIX ocel: <http://www.ocel-standard.org/ns#>
            SELECT ?obj ?type
            WHERE {
                ?obj a ocel:Object .
                ?obj ocel:objectType ?type .
            }
        "#;

        let solutions = graph.query(query_objects)?;
        let mut objects_map = HashMap::new();

        if let oxigraph::sparql::QueryResults::Solutions(solutions_iter) = solutions {
            for sol_res in solutions_iter {
                let sol = sol_res?;
                if let (Some(Term::NamedNode(obj_node)), Some(Term::Literal(type_lit))) =
                    (sol.get("obj"), sol.get("type"))
                {
                    let obj_uri = obj_node.as_str();
                    let id = obj_uri
                        .split('/')
                        .next_back()
                        .unwrap_or(obj_uri)
                        .to_string();
                    let r#type = type_lit.value().to_string();

                    // Query attributes for this object
                    let query_attrs = format!(
                        r#"
                        SELECT ?key ?val
                        WHERE {{
                            <{}> ?pred ?val .
                            FILTER(STRSTARTS(STR(?pred), "http://www.ocel-standard.org/ns#attribute/"))
                            BIND(SUBSTR(STR(?pred), 40) AS ?key)
                        }}
                        "#,
                        obj_uri
                    );

                    let mut attributes = HashMap::new();
                    if let oxigraph::sparql::QueryResults::Solutions(attrs_iter) =
                        graph.query(&query_attrs)?
                    {
                        for attr_sol_res in attrs_iter {
                            let attr_sol = attr_sol_res?;
                            if let (Some(Term::Literal(key_lit)), Some(Term::Literal(val_lit))) =
                                (attr_sol.get("key"), attr_sol.get("val"))
                            {
                                attributes.insert(
                                    key_lit.value().to_string(),
                                    val_lit.value().to_string(),
                                );
                            }
                        }
                    }

                    objects_map.insert(id.clone(), r#type.clone());
                    log.objects.push(OcelObject {
                        id,
                        r#type,
                        attributes,
                    });
                }
            }
        }

        // Let's query all events
        let query_events = r#"
            PREFIX ocel: <http://www.ocel-standard.org/ns#>
            SELECT ?ev ?activity ?timestamp
            WHERE {
                ?ev a ocel:Event .
                ?ev ocel:activity ?activity .
                ?ev ocel:timestamp ?timestamp .
            }
        "#;

        let solutions_ev = graph.query(query_events)?;
        if let oxigraph::sparql::QueryResults::Solutions(solutions_ev_iter) = solutions_ev {
            for sol_res in solutions_ev_iter {
                let sol = sol_res?;
                if let (
                    Some(Term::NamedNode(ev_node)),
                    Some(Term::Literal(act_lit)),
                    Some(Term::Literal(ts_lit)),
                ) = (sol.get("ev"), sol.get("activity"), sol.get("timestamp"))
                {
                    let ev_uri = ev_node.as_str();
                    let id = ev_uri.split('/').next_back().unwrap_or(ev_uri).to_string();
                    let activity = act_lit.value().to_string();
                    let timestamp = chrono::DateTime::parse_from_rfc3339(ts_lit.value())
                        .map(|dt| dt.with_timezone(&chrono::Utc))
                        .unwrap_or_else(|_| chrono::Utc::now());

                    // Query attributes for this event
                    let query_attrs = format!(
                        r#"
                        SELECT ?key ?val
                        WHERE {{
                            <{}> ?pred ?val .
                            FILTER(STRSTARTS(STR(?pred), "http://www.ocel-standard.org/ns#attribute/"))
                            BIND(SUBSTR(STR(?pred), 40) AS ?key)
                        }}
                        "#,
                        ev_uri
                    );

                    let mut attributes = HashMap::new();
                    if let oxigraph::sparql::QueryResults::Solutions(attrs_iter) =
                        graph.query(&query_attrs)?
                    {
                        for attr_sol_res in attrs_iter {
                            let attr_sol = attr_sol_res?;
                            if let (Some(Term::Literal(key_lit)), Some(Term::Literal(val_lit))) =
                                (attr_sol.get("key"), attr_sol.get("val"))
                            {
                                attributes.insert(
                                    key_lit.value().to_string(),
                                    val_lit.value().to_string(),
                                );
                            }
                        }
                    }

                    // Query object references for this event
                    let query_refs = format!(
                        r#"
                        PREFIX ocel: <http://www.ocel-standard.org/ns#>
                        SELECT DISTINCT ?obj ?pred
                        WHERE {{
                            <{}> ocel:has_object_ref ?obj .
                            <{}> ?pred ?obj .
                            FILTER(STRSTARTS(STR(?pred), "http://www.ocel-standard.org/ns#qualifier_") || ?pred = ocel:has_object_ref)
                        }}
                        "#,
                        ev_uri, ev_uri
                    );

                    let mut obj_refs_map = HashMap::new();
                    if let oxigraph::sparql::QueryResults::Solutions(refs_iter) =
                        graph.query(&query_refs)?
                    {
                        for ref_sol_res in refs_iter {
                            let ref_sol = ref_sol_res?;
                            if let (
                                Some(Term::NamedNode(obj_node)),
                                Some(Term::NamedNode(pred_node)),
                            ) = (ref_sol.get("obj"), ref_sol.get("pred"))
                            {
                                let obj_uri = obj_node.as_str();
                                let obj_id = obj_uri
                                    .split('/')
                                    .next_back()
                                    .unwrap_or(obj_uri)
                                    .to_string();
                                let pred_str = pred_node.as_str();

                                if pred_str.contains("qualifier_") {
                                    let qualifier_raw =
                                        pred_str.split("qualifier_").last().unwrap_or(pred_str);

                                    let qualifier =
                                        qualifier_raw.replace("%3C", "<").replace("%3E", ">");
                                    obj_refs_map.insert(obj_id, Some(qualifier));
                                } else {
                                    obj_refs_map.entry(obj_id).or_insert(None);
                                }
                            }
                        }
                    }

                    let mut objects = Vec::new();
                    for (obj_id, qualifier) in obj_refs_map {
                        let r#type = objects_map
                            .get(&obj_id)
                            .cloned()
                            .unwrap_or_else(|| "Unknown".to_string());
                        objects.push(OcelObjectRef {
                            id: obj_id,
                            r#type,
                            qualifier,
                        });
                    }
                    objects.sort_by(|a, b| a.id.cmp(&b.id));

                    log.events.push(OcelEvent {
                        id,
                        activity,
                        timestamp,
                        objects,
                        attributes,
                    });
                }
            }
        }

        // Sort logs to preserve deterministic order
        log.objects.sort_by(|a, b| a.id.cmp(&b.id));
        log.events.sort_by(|a, b| a.id.cmp(&b.id));

        Ok(log)
    }

    /// Projects a `ProvDocument` into the given `DeterministicGraph`.
    pub fn project_prov(graph: &DeterministicGraph, doc: &ProvDocument) -> Result<(), GraphError> {
        let type_pred = NamedNode::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
            .map_err(|e| GraphError::Other(e.to_string()))?;
        let entity_class = NamedNode::new("http://www.w3.org/ns/prov#Entity")
            .map_err(|e| GraphError::Other(e.to_string()))?;
        let activity_class = NamedNode::new("http://www.w3.org/ns/prov#Activity")
            .map_err(|e| GraphError::Other(e.to_string()))?;
        let agent_class = NamedNode::new("http://www.w3.org/ns/prov#Agent")
            .map_err(|e| GraphError::Other(e.to_string()))?;

        let was_generated_by_pred = NamedNode::new("http://www.w3.org/ns/prov#wasGeneratedBy")
            .map_err(|e| GraphError::Other(e.to_string()))?;
        let used_pred = NamedNode::new("http://www.w3.org/ns/prov#used")
            .map_err(|e| GraphError::Other(e.to_string()))?;
        let started_at_pred = NamedNode::new("http://www.w3.org/ns/prov#startedAtTime")
            .map_err(|e| GraphError::Other(e.to_string()))?;
        let ended_at_pred = NamedNode::new("http://www.w3.org/ns/prov#endedAtTime")
            .map_err(|e| GraphError::Other(e.to_string()))?;
        let was_derived_from_pred = NamedNode::new("http://www.w3.org/ns/prov#wasDerivedFrom")
            .map_err(|e| GraphError::Other(e.to_string()))?;

        // Entities
        for ent in &doc.entities {
            let ent_uri = format!("http://ggen.dev/prov/entity/{}", ent.id);
            let ent_node =
                NamedNode::new(&ent_uri).map_err(|e| GraphError::Other(e.to_string()))?;
            let subject = NamedOrBlankNode::NamedNode(ent_node);

            graph.insert_quad(&Quad::new(
                subject.clone(),
                type_pred.clone(),
                Term::NamedNode(entity_class.clone()),
                GraphName::DefaultGraph,
            ))?;

            for (key, val) in &ent.attributes {
                let attr_pred =
                    NamedNode::new(format!("http://www.w3.org/ns/prov#attribute/{}", key))
                        .map_err(|e| GraphError::Other(e.to_string()))?;
                graph.insert_quad(&Quad::new(
                    subject.clone(),
                    attr_pred,
                    Term::Literal(Literal::new_simple_literal(val)),
                    GraphName::DefaultGraph,
                ))?;
            }
        }

        // Activities
        for act in &doc.activities {
            let act_uri = format!("http://ggen.dev/prov/activity/{}", act.id);
            let act_node =
                NamedNode::new(&act_uri).map_err(|e| GraphError::Other(e.to_string()))?;
            let subject = NamedOrBlankNode::NamedNode(act_node);

            graph.insert_quad(&Quad::new(
                subject.clone(),
                type_pred.clone(),
                Term::NamedNode(activity_class.clone()),
                GraphName::DefaultGraph,
            ))?;

            if let Some(ref start) = act.start_time {
                graph.insert_quad(&Quad::new(
                    subject.clone(),
                    started_at_pred.clone(),
                    Term::Literal(Literal::new_simple_literal(start.to_rfc3339())),
                    GraphName::DefaultGraph,
                ))?;
            }

            if let Some(ref end) = act.end_time {
                graph.insert_quad(&Quad::new(
                    subject.clone(),
                    ended_at_pred.clone(),
                    Term::Literal(Literal::new_simple_literal(end.to_rfc3339())),
                    GraphName::DefaultGraph,
                ))?;
            }

            for (key, val) in &act.attributes {
                let attr_pred =
                    NamedNode::new(format!("http://www.w3.org/ns/prov#attribute/{}", key))
                        .map_err(|e| GraphError::Other(e.to_string()))?;
                graph.insert_quad(&Quad::new(
                    subject.clone(),
                    attr_pred,
                    Term::Literal(Literal::new_simple_literal(val)),
                    GraphName::DefaultGraph,
                ))?;
            }
        }

        // Agents
        for agt in &doc.agents {
            let agt_uri = format!("http://ggen.dev/prov/agent/{}", agt.id);
            let agt_node =
                NamedNode::new(&agt_uri).map_err(|e| GraphError::Other(e.to_string()))?;
            let subject = NamedOrBlankNode::NamedNode(agt_node);

            graph.insert_quad(&Quad::new(
                subject.clone(),
                type_pred.clone(),
                Term::NamedNode(agent_class.clone()),
                GraphName::DefaultGraph,
            ))?;

            for (key, val) in &agt.attributes {
                let attr_pred =
                    NamedNode::new(format!("http://www.w3.org/ns/prov#attribute/{}", key))
                        .map_err(|e| GraphError::Other(e.to_string()))?;
                graph.insert_quad(&Quad::new(
                    subject.clone(),
                    attr_pred,
                    Term::Literal(Literal::new_simple_literal(val)),
                    GraphName::DefaultGraph,
                ))?;
            }
        }

        // Generations
        for gen in &doc.generations {
            let ent_uri = format!("http://ggen.dev/prov/entity/{}", gen.entity);
            let ent_node =
                NamedNode::new(&ent_uri).map_err(|e| GraphError::Other(e.to_string()))?;
            let subject = NamedOrBlankNode::NamedNode(ent_node);

            let act_uri = format!("http://ggen.dev/prov/activity/{}", gen.activity);
            let act_node =
                NamedNode::new(&act_uri).map_err(|e| GraphError::Other(e.to_string()))?;

            graph.insert_quad(&Quad::new(
                subject,
                was_generated_by_pred.clone(),
                Term::NamedNode(act_node),
                GraphName::DefaultGraph,
            ))?;
        }

        // Usages
        for usage in &doc.usages {
            let act_uri = format!("http://ggen.dev/prov/activity/{}", usage.activity);
            let act_node =
                NamedNode::new(&act_uri).map_err(|e| GraphError::Other(e.to_string()))?;
            let subject = NamedOrBlankNode::NamedNode(act_node);

            let ent_uri = format!("http://ggen.dev/prov/entity/{}", usage.entity);
            let ent_node =
                NamedNode::new(&ent_uri).map_err(|e| GraphError::Other(e.to_string()))?;

            graph.insert_quad(&Quad::new(
                subject,
                used_pred.clone(),
                Term::NamedNode(ent_node),
                GraphName::DefaultGraph,
            ))?;
        }

        // Derivations
        for derivation in &doc.derivations {
            let gen_ent_uri = format!(
                "http://ggen.dev/prov/entity/{}",
                derivation.generated_entity
            );
            let gen_ent_node =
                NamedNode::new(&gen_ent_uri).map_err(|e| GraphError::Other(e.to_string()))?;
            let subject = NamedOrBlankNode::NamedNode(gen_ent_node);

            let used_ent_uri = format!("http://ggen.dev/prov/entity/{}", derivation.used_entity);
            let used_ent_node =
                NamedNode::new(&used_ent_uri).map_err(|e| GraphError::Other(e.to_string()))?;

            graph.insert_quad(&Quad::new(
                subject,
                was_derived_from_pred.clone(),
                Term::NamedNode(used_ent_node),
                GraphName::DefaultGraph,
            ))?;
        }

        Ok(())
    }

    /// Extracts a `ProvDocument` from the given `DeterministicGraph`.
    pub fn extract_prov(graph: &DeterministicGraph) -> Result<ProvDocument, GraphError> {
        let mut doc = ProvDocument::new();

        // 1. Entities
        let query_entities = r#"
            PREFIX prov: <http://www.w3.org/ns/prov#>
            SELECT ?ent WHERE { ?ent a prov:Entity . }
        "#;
        if let oxigraph::sparql::QueryResults::Solutions(solutions) = graph.query(query_entities)? {
            for sol_res in solutions {
                let sol = sol_res?;
                if let Some(Term::NamedNode(ent_node)) = sol.get("ent") {
                    let ent_uri = ent_node.as_str();
                    let id = ent_uri
                        .split('/')
                        .next_back()
                        .unwrap_or(ent_uri)
                        .to_string();

                    // Query attributes
                    let query_attrs = format!(
                        r#"
                        SELECT ?key ?val
                        WHERE {{
                            <{}> ?pred ?val .
                            FILTER(STRSTARTS(STR(?pred), "http://www.w3.org/ns/prov#attribute/"))
                            BIND(SUBSTR(STR(?pred), 36) AS ?key)
                        }}
                        "#,
                        ent_uri
                    );
                    let mut attributes = HashMap::new();
                    if let oxigraph::sparql::QueryResults::Solutions(attrs_iter) =
                        graph.query(&query_attrs)?
                    {
                        for attr_sol_res in attrs_iter {
                            let attr_sol = attr_sol_res?;
                            if let (Some(Term::Literal(key_lit)), Some(Term::Literal(val_lit))) =
                                (attr_sol.get("key"), attr_sol.get("val"))
                            {
                                attributes.insert(
                                    key_lit.value().to_string(),
                                    val_lit.value().to_string(),
                                );
                            }
                        }
                    }
                    doc.entities.push(ProvEntity { id, attributes });
                }
            }
        }

        // 2. Activities
        let query_activities = r#"
            PREFIX prov: <http://www.w3.org/ns/prov#>
            SELECT ?act ?start ?end
            WHERE {
                ?act a prov:Activity .
                OPTIONAL { ?act prov:startedAtTime ?start . }
                OPTIONAL { ?act prov:endedAtTime ?end . }
            }
        "#;
        if let oxigraph::sparql::QueryResults::Solutions(solutions) =
            graph.query(query_activities)?
        {
            for sol_res in solutions {
                let sol = sol_res?;
                if let Some(Term::NamedNode(act_node)) = sol.get("act") {
                    let act_uri = act_node.as_str();
                    let id = act_uri
                        .split('/')
                        .next_back()
                        .unwrap_or(act_uri)
                        .to_string();

                    let start_time = sol.get("start").and_then(|t| match t {
                        Term::Literal(lit) => chrono::DateTime::parse_from_rfc3339(lit.value())
                            .map(|dt| dt.with_timezone(&chrono::Utc))
                            .ok(),
                        _ => None,
                    });

                    let end_time = sol.get("end").and_then(|t| match t {
                        Term::Literal(lit) => chrono::DateTime::parse_from_rfc3339(lit.value())
                            .map(|dt| dt.with_timezone(&chrono::Utc))
                            .ok(),
                        _ => None,
                    });

                    // Query attributes
                    let query_attrs = format!(
                        r#"
                        SELECT ?key ?val
                        WHERE {{
                            <{}> ?pred ?val .
                            FILTER(STRSTARTS(STR(?pred), "http://www.w3.org/ns/prov#attribute/"))
                            BIND(SUBSTR(STR(?pred), 36) AS ?key)
                        }}
                        "#,
                        act_uri
                    );
                    let mut attributes = HashMap::new();
                    if let oxigraph::sparql::QueryResults::Solutions(attrs_iter) =
                        graph.query(&query_attrs)?
                    {
                        for attr_sol_res in attrs_iter {
                            let attr_sol = attr_sol_res?;
                            if let (Some(Term::Literal(key_lit)), Some(Term::Literal(val_lit))) =
                                (attr_sol.get("key"), attr_sol.get("val"))
                            {
                                attributes.insert(
                                    key_lit.value().to_string(),
                                    val_lit.value().to_string(),
                                );
                            }
                        }
                    }
                    doc.activities.push(ProvActivity {
                        id,
                        start_time,
                        end_time,
                        attributes,
                    });
                }
            }
        }

        // 3. Agents
        let query_agents = r#"
            PREFIX prov: <http://www.w3.org/ns/prov#>
            SELECT ?agt WHERE { ?agt a prov:Agent . }
        "#;
        if let oxigraph::sparql::QueryResults::Solutions(solutions) = graph.query(query_agents)? {
            for sol_res in solutions {
                let sol = sol_res?;
                if let Some(Term::NamedNode(agt_node)) = sol.get("agt") {
                    let agt_uri = agt_node.as_str();
                    let id = agt_uri
                        .split('/')
                        .next_back()
                        .unwrap_or(agt_uri)
                        .to_string();

                    // Query attributes
                    let query_attrs = format!(
                        r#"
                        SELECT ?key ?val
                        WHERE {{
                            <{}> ?pred ?val .
                            FILTER(STRSTARTS(STR(?pred), "http://www.w3.org/ns/prov#attribute/"))
                            BIND(SUBSTR(STR(?pred), 36) AS ?key)
                        }}
                        "#,
                        agt_uri
                    );
                    let mut attributes = HashMap::new();
                    if let oxigraph::sparql::QueryResults::Solutions(attrs_iter) =
                        graph.query(&query_attrs)?
                    {
                        for attr_sol_res in attrs_iter {
                            let attr_sol = attr_sol_res?;
                            if let (Some(Term::Literal(key_lit)), Some(Term::Literal(val_lit))) =
                                (attr_sol.get("key"), attr_sol.get("val"))
                            {
                                attributes.insert(
                                    key_lit.value().to_string(),
                                    val_lit.value().to_string(),
                                );
                            }
                        }
                    }
                    doc.agents.push(ProvAgent { id, attributes });
                }
            }
        }

        // 4. Generations
        let query_generations = r#"
            PREFIX prov: <http://www.w3.org/ns/prov#>
            SELECT ?ent ?act WHERE { ?ent prov:wasGeneratedBy ?act . }
        "#;
        if let oxigraph::sparql::QueryResults::Solutions(solutions) =
            graph.query(query_generations)?
        {
            for sol_res in solutions {
                let sol = sol_res?;
                if let (Some(Term::NamedNode(ent_node)), Some(Term::NamedNode(act_node))) =
                    (sol.get("ent"), sol.get("act"))
                {
                    let ent_uri = ent_node.as_str();
                    let entity = ent_uri
                        .split('/')
                        .next_back()
                        .unwrap_or(ent_uri)
                        .to_string();

                    let act_uri = act_node.as_str();
                    let activity = act_uri
                        .split('/')
                        .next_back()
                        .unwrap_or(act_uri)
                        .to_string();

                    doc.generations.push(ProvGeneration { entity, activity });
                }
            }
        }

        // 5. Usages
        let query_usages = r#"
            PREFIX prov: <http://www.w3.org/ns/prov#>
            SELECT ?act ?ent WHERE { ?act prov:used ?ent . }
        "#;
        if let oxigraph::sparql::QueryResults::Solutions(solutions) = graph.query(query_usages)? {
            for sol_res in solutions {
                let sol = sol_res?;
                if let (Some(Term::NamedNode(act_node)), Some(Term::NamedNode(ent_node))) =
                    (sol.get("act"), sol.get("ent"))
                {
                    let act_uri = act_node.as_str();
                    let activity = act_uri
                        .split('/')
                        .next_back()
                        .unwrap_or(act_uri)
                        .to_string();

                    let ent_uri = ent_node.as_str();
                    let entity = ent_uri
                        .split('/')
                        .next_back()
                        .unwrap_or(ent_uri)
                        .to_string();

                    doc.usages.push(ProvUsage { activity, entity });
                }
            }
        }

        // 6. Derivations
        let query_derivations = r#"
            PREFIX prov: <http://www.w3.org/ns/prov#>
            SELECT ?gen ?used WHERE { ?gen prov:wasDerivedFrom ?used . }
        "#;
        if let oxigraph::sparql::QueryResults::Solutions(solutions) =
            graph.query(query_derivations)?
        {
            for sol_res in solutions {
                let sol = sol_res?;
                if let (Some(Term::NamedNode(gen_node)), Some(Term::NamedNode(used_node))) =
                    (sol.get("gen"), sol.get("used"))
                {
                    let gen_uri = gen_node.as_str();
                    let generated_entity = gen_uri
                        .split('/')
                        .next_back()
                        .unwrap_or(gen_uri)
                        .to_string();

                    let used_uri = used_node.as_str();
                    let used_entity = used_uri
                        .split('/')
                        .next_back()
                        .unwrap_or(used_uri)
                        .to_string();

                    doc.derivations.push(ProvDerivation {
                        generated_entity,
                        used_entity,
                    });
                }
            }
        }

        // Sort collections to preserve deterministic order
        doc.entities.sort_by(|a, b| a.id.cmp(&b.id));
        doc.activities.sort_by(|a, b| a.id.cmp(&b.id));
        doc.agents.sort_by(|a, b| a.id.cmp(&b.id));
        doc.generations.sort_by(|a, b| a.entity.cmp(&b.entity));
        doc.usages.sort_by(|a, b| a.activity.cmp(&b.activity));
        doc.derivations.sort_by(|a, b| {
            a.generated_entity
                .cmp(&b.generated_entity)
                .then(a.used_entity.cmp(&b.used_entity))
        });

        Ok(doc)
    }
}
