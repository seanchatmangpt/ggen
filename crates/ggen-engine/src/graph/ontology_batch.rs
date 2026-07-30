//! Atomic, document-scoped ontology batch admission.
//!
//! Turtle parser state is scoped to one document: prefixes, base IRIs, and
//! blank-node labels must not leak across ontology imports. This module parses
//! every document independently, renames document-local blank nodes, and only
//! then commits the complete admitted quad set through one Oxigraph
//! transaction. The result removes the nonlinear repeated-store-scan path
//! without changing RDF document semantics.

use oxigraph::{
    io::{RdfFormat, RdfParser},
    model::Quad,
};

use super::{AppError, DeterministicGraph, Result};

/// One Turtle document admitted as part of an ontology batch.
#[derive(Debug, Clone, Copy)]
pub struct TurtleDocument<'a> {
    label: &'a str,
    content: &'a str,
}

impl<'a> TurtleDocument<'a> {
    /// Construct a labelled Turtle document.
    ///
    /// `label` is carried into typed parse refusals so a batch failure names
    /// the exact ontology import that could not be admitted.
    #[must_use]
    pub const fn new(label: &'a str, content: &'a str) -> Self {
        Self { label, content }
    }
}

/// Receipt for one atomic ontology-batch admission.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[must_use]
pub struct OntologyBatchReceipt {
    /// Number of independently parsed Turtle documents.
    pub documents: usize,
    /// Number of quads produced by all document parsers before deduplication.
    pub parsed_quads: usize,
    /// Number of quads newly inserted into the destination graph.
    pub inserted_quads: usize,
}

/// Parse every document before mutating the graph, then atomically extend the
/// store once. `RdfParser::rename_blank_nodes` is deliberately applied per
/// document: identical `_:` labels in separate ontology files remain distinct,
/// while prefixes and `@base` directives retain their document-local scope.
pub(super) fn insert_documents(
    graph: &DeterministicGraph, documents: &[TurtleDocument<'_>],
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
        let parser = RdfParser::from_format(RdfFormat::Turtle).rename_blank_nodes();
        for parsed in parser.for_slice(document.content.as_bytes()) {
            let quad = parsed.map_err(|error| {
                AppError::fm_graph(
                    2,
                    format!("turtle document `{}` parse failed: {error}", document.label),
                )
            })?;
            quads.push(quad);
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
        insert_documents(&batched, &documents)?;

        assert_eq!(batched.state_hash()?, sequential.state_hash()?);
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
