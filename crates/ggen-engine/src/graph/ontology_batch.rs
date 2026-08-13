//! Atomic, document-scoped RDF ontology batch admission.
//!
//! Parser state is scoped to one document: prefixes, base IRIs, blank-node
//! labels, and syntax selection must not leak across ontology imports. This
//! module parses every document independently, renames document-local blank
//! nodes, normalizes dataset serializations into the engine's current union-
//! graph semantics, and only then commits the complete admitted quad set
//! through one Oxigraph transaction.
//!
//! `TurtleDocument` keeps its historical name for API compatibility, but it
//! is now an RDF-source document rather than a Turtle-only document. Existing
//! callers continue to use `TurtleDocument::new(label, content)`; the format is
//! inferred deterministically from a fail-closed set of canonical RDF file
//! extensions. Plain `.json` is deliberately excluded even though Oxigraph aliases
//! it to JSON-LD: arbitrary JSON must cross an explicit projector boundary.
//! `with_format` exists for sources whose filename cannot carry a trustworthy
//! format signal.

use std::path::Path;

use oxigraph::{
    io::{RdfFormat, RdfParser},
    model::{GraphName, Quad},
};

use super::{AppError, DeterministicGraph, Result};

/// One RDF document admitted as part of an ontology batch.
///
/// Historical name retained to avoid a flag-day API break. A document may be
/// Turtle, RDF/XML, JSON-LD, N-Triples, N-Quads, TriG, or N3 when the
/// label uses an admitted RDF extension. Dataset formats are parsed
/// faithfully and then flattened into the engine's existing union/default-
/// graph model; source-level named-graph provenance is a separate admission
/// concern and must not be faked here.
#[derive(Debug, Clone, Copy)]
pub struct TurtleDocument<'a> {
    label: &'a str,
    content: &'a str,
    format: Option<RdfFormat>,
}

impl<'a> TurtleDocument<'a> {
    /// Construct a labelled RDF document whose syntax is inferred from the
    /// label's extension at admission time.
    ///
    /// `label` is carried into typed parse refusals so a batch failure names
    /// the exact ontology import that could not be admitted. Labels without
    /// an extension preserve the pre-existing Turtle behavior.
    #[must_use]
    pub const fn new(label: &'a str, content: &'a str) -> Self {
        Self {
            label,
            content,
            format: None,
        }
    }

    /// Construct a labelled RDF document with an explicit syntax.
    ///
    /// This is the authority-preserving escape hatch for content-addressed or
    /// otherwise extensionless vendored sources. Explicit syntax outranks
    /// filename inference; no content sniffing is performed.
    #[must_use]
    pub const fn with_format(label: &'a str, content: &'a str, format: RdfFormat) -> Self {
        Self {
            label,
            content,
            format: Some(format),
        }
    }

    fn resolved_format(self) -> Result<RdfFormat> {
        if let Some(format) = self.format {
            return Ok(format);
        }

        let Some(extension) = Path::new(self.label).extension().and_then(|ext| ext.to_str()) else {
            // Backward compatibility: before this admission boundary became
            // syntax-aware every document was unconditionally parsed as
            // Turtle, including tests/callers that used extensionless labels.
            return Ok(RdfFormat::Turtle);
        };

        // Do not delegate extension admission wholesale to
        // `RdfFormat::from_extension`: Oxigraph deliberately treats plain
        // `.json` as a JSON-LD alias. In ggen, `.json` is an ambiguous
        // external-schema carrier (OpenAPI, Kubernetes discovery, Terraform
        // provider schemas, STIX bundles, etc.) and therefore MUST cross an
        // explicit projector boundary before it acquires RDF authority.
        // Canonical `.jsonld` remains admitted. `.xml` is retained as the
        // standard RDF/XML alias.
        let format = match extension.to_ascii_lowercase().as_str() {
            "ttl" => Some(RdfFormat::Turtle),
            "rdf" | "xml" => Some(RdfFormat::RdfXml),
            "jsonld" => RdfFormat::from_extension("jsonld"),
            "nt" => Some(RdfFormat::NTriples),
            "nq" => Some(RdfFormat::NQuads),
            "trig" => Some(RdfFormat::TriG),
            "n3" => Some(RdfFormat::N3),
            _ => None,
        };

        format.ok_or_else(|| {
            AppError::fm_graph(
                10,
                format!(
                    "RDF document `{}` has unsupported or ambiguous extension `.{extension}`. \
                     Remediation: vendor RDF authorities with a canonical RDF extension \
                     (.ttl, .rdf/.xml, .jsonld, .nt, .nq, .trig, .n3). Arbitrary \
                     .json must be normalized by an explicit projector before RDF admission, \
                     or construct a content-addressed RDF document with an explicit RdfFormat.",
                    self.label
                ),
            )
        })
    }
}

/// Receipt for one atomic ontology-batch admission.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[must_use]
pub struct OntologyBatchReceipt {
    /// Number of independently parsed RDF documents.
    pub documents: usize,
    /// Number of quads produced by all document parsers before deduplication.
    pub parsed_quads: usize,
    /// Number of quads newly inserted into the destination graph.
    pub inserted_quads: usize,
}

/// Parse every document before mutating the graph, then atomically extend the
/// store once.
///
/// `RdfParser::rename_blank_nodes` is deliberately applied per document:
/// identical `_:` labels in separate ontology files remain distinct, while
/// prefixes and base directives retain their document-local scope. Dataset
/// formats (N-Quads/TriG) are normalized to `DefaultGraph` after parsing so
/// this change preserves the engine's long-standing "all imported ontology
/// facts form one union graph" contract rather than accidentally making some
/// imports invisible to ordinary `WHERE { ?s ?p ?o }` queries.
pub(super) fn insert_documents(
    graph: &DeterministicGraph,
    documents: &[TurtleDocument<'_>],
) -> Result<OntologyBatchReceipt> {
    if documents.is_empty() {
        return Ok(OntologyBatchReceipt {
            documents: 0,
            parsed_quads: 0,
            inserted_quads: 0,
        });
    }

    let mut quads: Vec<Quad> = Vec::new();
    for document in documents {
        let format = document.resolved_format()?;
        let parser = RdfParser::from_format(format).rename_blank_nodes();
        for parsed in parser.for_slice(document.content.as_bytes()) {
            let parsed = parsed.map_err(|error| {
                AppError::fm_graph(
                    2,
                    format!(
                        "RDF document `{}` ({}) parse failed: {error}",
                        document.label,
                        format.name()
                    ),
                )
            })?;

            // The engine has historically exposed imported ontologies as one
            // default/union graph. Preserve that observable contract even
            // when the source serialization itself is dataset-capable. The
            // provenance slice will replace this flattening with explicit
            // source named graphs *together with* union-default query
            // semantics, never as an isolated behavior change.
            quads.push(Quad::new(
                parsed.subject,
                parsed.predicate,
                parsed.object,
                GraphName::DefaultGraph,
            ));
        }
    }

    let parsed_quads = quads.len();
    let before = graph.store.len().map_err(|error| {
        AppError::fm_graph(
            2,
            format!("ontology batch store length unavailable before commit: {error}"),
        )
    })?;
    graph.store.extend(quads).map_err(|error| {
        AppError::fm_graph(2, format!("ontology batch atomic commit failed: {error}"))
    })?;
    let after = graph.store.len().map_err(|error| {
        AppError::fm_graph(
            2,
            format!("ontology batch store length unavailable after commit: {error}"),
        )
    })?;

    // Same blank-node canonicalization every other mutation path runs (see
    // `DeterministicGraph::canonicalize_blank_nodes`'s doc comment) — this
    // path bypasses `DeterministicGraph::insert_turtle` by writing straight
    // to `graph.store`, so it must call this itself rather than inherit it.
    graph.canonicalize_blank_nodes()?;

    Ok(OntologyBatchReceipt {
        documents: documents.len(),
        parsed_quads,
        inserted_quads: after.saturating_sub(before),
    })
}

#[cfg(test)]
mod tests {
    use std::{collections::HashSet, time::Instant};

    use super::*;

    fn ontology_documents(count: usize) -> Vec<(String, String)> {
        (0..count)
            .map(|index| {
                (
                    format!("import-{index}.ttl"),
                    format!(
                        "<urn:ggen:subject:{index}> <urn:ggen:predicate> <urn:ggen:object:{index}> ."
                    ),
                )
            })
            .collect()
    }

    fn borrowed_documents(owned: &[(String, String)]) -> Vec<TurtleDocument<'_>> {
        owned
            .iter()
            .map(|(label, content)| TurtleDocument::new(label, content))
            .collect()
    }

    #[test]
    fn ontology_batch_loads_128_documents_through_one_admission() -> Result<()> {
        let owned = ontology_documents(128);
        let documents = borrowed_documents(&owned);
        let graph = DeterministicGraph::new()?;

        let receipt = insert_documents(&graph, &documents)?;

        assert_eq!(receipt.documents, 128);
        assert_eq!(receipt.parsed_quads, 128);
        assert_eq!(receipt.inserted_quads, 128);
        assert_eq!(graph.all_quads()?.len(), 128);
        Ok(())
    }

    #[test]
    fn ontology_batch_preserves_document_scoped_blank_nodes() -> Result<()> {
        let owned = vec![
            (
                "left.ttl".to_string(),
                "_:shared <urn:ggen:predicate> <urn:ggen:left> .".to_string(),
            ),
            (
                "right.ttl".to_string(),
                "_:shared <urn:ggen:predicate> <urn:ggen:right> .".to_string(),
            ),
        ];
        let documents = borrowed_documents(&owned);
        let graph = DeterministicGraph::new()?;

        let receipt = insert_documents(&graph, &documents)?;
        let subjects: HashSet<String> = graph
            .all_quads()?
            .into_iter()
            .map(|quad| quad.subject.to_string())
            .collect();

        assert_eq!(receipt.inserted_quads, 2);
        assert_eq!(subjects.len(), 2);
        Ok(())
    }

    #[test]
    fn ontology_batch_parse_refusal_is_attributed_and_atomic() -> Result<()> {
        let graph = DeterministicGraph::new()?;
        graph.insert_turtle("<urn:ggen:sentinel> <urn:ggen:p> <urn:ggen:o> .")?;
        let before = graph.state_hash()?;
        let owned = vec![
            (
                "valid.ttl".to_string(),
                "<urn:ggen:valid> <urn:ggen:p> <urn:ggen:o> .".to_string(),
            ),
            (
                "broken.ttl".to_string(),
                "<urn:ggen:broken> <urn:ggen:p>".to_string(),
            ),
        ];
        let documents = borrowed_documents(&owned);

        let error = match insert_documents(&graph, &documents) {
            Ok(receipt) => {
                return Err(AppError::fm_graph(
                    2,
                    format!("invalid ontology batch was admitted: {receipt:?}"),
                ));
            }
            Err(error) => error,
        };

        assert!(error.to_string().contains("broken.ttl"));
        assert_eq!(graph.state_hash()?, before);
        assert_eq!(graph.all_quads()?.len(), 1);
        Ok(())
    }

    #[test]
    fn ontology_batch_state_matches_sequential_named_node_admission() -> Result<()> {
        let owned = ontology_documents(64);
        let documents = borrowed_documents(&owned);
        let sequential = DeterministicGraph::new()?;
        for document in &documents {
            sequential.insert_turtle(document.content)?;
        }
        let batched = DeterministicGraph::new()?;
        let receipt = insert_documents(&batched, &documents)?;

        assert_eq!(receipt.documents, documents.len());
        assert_eq!(batched.state_hash()?, sequential.state_hash()?);
        Ok(())
    }

    #[test]
    fn ontology_batch_admits_rdfxml_by_source_extension() -> Result<()> {
        let rdfxml = r#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:ex="http://example.com/">
  <rdf:Description rdf:about="urn:ggen:rdfxml-subject">
    <ex:predicate rdf:resource="urn:ggen:rdfxml-object"/>
  </rdf:Description>
</rdf:RDF>"#;
        let graph = DeterministicGraph::new()?;
        let documents = [TurtleDocument::new("vendor/skos.rdf", rdfxml)];

        let receipt = insert_documents(&graph, &documents)?;

        assert_eq!(receipt.documents, 1);
        assert_eq!(receipt.parsed_quads, 1);
        assert_eq!(receipt.inserted_quads, 1);
        assert_eq!(graph.all_quads()?.len(), 1);
        Ok(())
    }

    #[test]
    fn ontology_batch_admits_jsonld_by_source_extension() -> Result<()> {
        let jsonld = r#"{
  "@context": { "ex": "http://example.com/" },
  "@id": "urn:ggen:jsonld-subject",
  "ex:predicate": { "@id": "urn:ggen:jsonld-object" }
}"#;
        let graph = DeterministicGraph::new()?;
        let documents = [TurtleDocument::new("vendor/authority.jsonld", jsonld)];

        let receipt = insert_documents(&graph, &documents)?;

        assert_eq!(receipt.documents, 1);
        assert_eq!(receipt.parsed_quads, 1);
        assert_eq!(receipt.inserted_quads, 1);
        assert_eq!(graph.all_quads()?.len(), 1);
        Ok(())
    }

    #[test]
    fn ontology_batch_admits_mixed_rdf_serializations_atomically() -> Result<()> {
        let turtle = "<urn:ggen:ttl-s> <urn:ggen:p> <urn:ggen:ttl-o> .";
        let rdfxml = r#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:ex="http://example.com/">
  <rdf:Description rdf:about="urn:ggen:xml-s">
    <ex:p rdf:resource="urn:ggen:xml-o"/>
  </rdf:Description>
</rdf:RDF>"#;
        let jsonld = r#"{
  "@context": { "ex": "http://example.com/" },
  "@id": "urn:ggen:json-s",
  "ex:p": { "@id": "urn:ggen:json-o" }
}"#;
        let graph = DeterministicGraph::new()?;
        let documents = [
            TurtleDocument::new("authority.ttl", turtle),
            TurtleDocument::new("authority.rdf", rdfxml),
            TurtleDocument::new("authority.jsonld", jsonld),
        ];

        let receipt = insert_documents(&graph, &documents)?;

        assert_eq!(receipt.documents, 3);
        assert_eq!(receipt.parsed_quads, 3);
        assert_eq!(receipt.inserted_quads, 3);
        assert_eq!(graph.all_quads()?.len(), 3);
        Ok(())
    }

    #[test]
    fn dataset_serialization_is_flattened_into_existing_union_graph_contract() -> Result<()> {
        let nquads = "<urn:ggen:nq-s> <urn:ggen:p> <urn:ggen:nq-o> <urn:ggen:source-graph> .";
        let graph = DeterministicGraph::new()?;
        let documents = [TurtleDocument::new("authority.nq", nquads)];

        let receipt = insert_documents(&graph, &documents)?;
        let quads = graph.all_quads()?;

        assert_eq!(receipt.parsed_quads, 1);
        assert_eq!(quads.len(), 1);
        assert!(matches!(quads[0].graph_name, GraphName::DefaultGraph));
        Ok(())
    }

    #[test]
    fn explicit_format_overrides_nonstandard_extension() -> Result<()> {
        let rdfxml = r#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:ex="http://example.com/">
  <rdf:Description rdf:about="urn:ggen:explicit-s">
    <ex:p rdf:resource="urn:ggen:explicit-o"/>
  </rdf:Description>
</rdf:RDF>"#;
        let graph = DeterministicGraph::new()?;
        let documents = [TurtleDocument::with_format(
            "content-addressed.authority",
            rdfxml,
            RdfFormat::RdfXml,
        )];

        let receipt = insert_documents(&graph, &documents)?;

        assert_eq!(receipt.inserted_quads, 1);
        Ok(())
    }

    #[test]
    fn unsupported_extension_refuses_before_any_store_mutation() -> Result<()> {
        let graph = DeterministicGraph::new()?;
        graph.insert_turtle("<urn:ggen:sentinel> <urn:ggen:p> <urn:ggen:o> .")?;
        let before = graph.state_hash()?;
        let documents = [
            TurtleDocument::new(
                "valid.ttl",
                "<urn:ggen:valid> <urn:ggen:p> <urn:ggen:o> .",
            ),
            TurtleDocument::new("authority.openapi.json", "{}"),
        ];

        let error = insert_documents(&graph, &documents).expect_err("must refuse non-RDF syntax");

        assert!(error.to_string().contains("FM-GRAPH-010"), "{error}");
        assert!(error.to_string().contains("authority.openapi.json"), "{error}");
        assert_eq!(graph.state_hash()?, before);
        assert_eq!(graph.all_quads()?.len(), 1);
        Ok(())
    }

    #[test]
    #[ignore = "real-boundary startup benchmark receipt"]
    fn ontology_batch_startup_benchmark_receipt() -> Result<()> {
        for count in [8_usize, 36, 64, 128] {
            let owned = ontology_documents(count);
            let documents = borrowed_documents(&owned);

            let sequential = DeterministicGraph::new()?;
            let sequential_start = Instant::now();
            for document in &documents {
                sequential.insert_turtle(document.content)?;
            }
            let sequential_ns = sequential_start.elapsed().as_nanos();

            let batched = DeterministicGraph::new()?;
            let batch_start = Instant::now();
            let receipt = insert_documents(&batched, &documents)?;
            let batch_ns = batch_start.elapsed().as_nanos();

            assert_eq!(batched.state_hash()?, sequential.state_hash()?);
            eprintln!(
                "{{\"documents\":{count},\"sequential_ns\":{sequential_ns},\"batch_ns\":{batch_ns},\"parsed_quads\":{},\"inserted_quads\":{}}}",
                receipt.parsed_quads, receipt.inserted_quads
            );
        }
        Ok(())
    }
}
