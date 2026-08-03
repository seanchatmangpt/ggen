//! Load a project's graph without running the full sync pipeline.
//!
//! `sync()` and `generation_rules::run()` each load the project graph as
//! step one of a much larger pipeline (template discovery, rendering,
//! writing, receipt chaining). A caller that only wants "the graph, as the
//! CLI would build it" — e.g. an ad-hoc query tool — has no way to get that
//! without either duplicating the loading logic (the exact drift this
//! module exists to prevent) or paying for the rest of the pipeline it
//! doesn't need.
//!
//! This module is the shared entry point for that narrower need. It
//! performs the same schema dispatch, ontology+imports+packs resolution,
//! and graph admission `sync`/`generation_rules` do — reusing
//! [`crate::sync::new_graph_engine`] and [`crate::sync::read_ontology_file`]
//! — but stops once the graph is built, before any template/render/write
//! stage.

use std::path::Path;
use std::sync::Arc;

use crate::error::Result;
use crate::graph::{GraphEngine, TurtleDocument};
use crate::schema_dispatch::{self, ParsedGgenToml};
use crate::sync::{new_graph_engine, read_ontology_file, EngineKind};

/// Load `root`'s project graph (ontology + imports + packs, whichever apply
/// to its `ggen.toml` schema) using the default [`EngineKind`], without
/// running template discovery, rendering, or writes.
///
/// # Errors
/// Propagates `ggen.toml` classification/parse failures, ontology
/// read/parse failures, and (for the frontmatter schema) pack resolution
/// failures — the same failure modes `sync run` surfaces at its own
/// Resolve stage, just without the template stages after it.
pub fn load_for_query(root: &Path) -> Result<Arc<dyn GraphEngine>> {
    load_for_query_with_engine(root, EngineKind::default())
}

/// As [`load_for_query`], with an explicit engine choice.
///
/// # Errors
/// See [`load_for_query`].
pub fn load_for_query_with_engine(root: &Path, engine: EngineKind) -> Result<Arc<dyn GraphEngine>> {
    let graph: Arc<dyn GraphEngine> = new_graph_engine(engine)?;
    let sources = match schema_dispatch::load(root)? {
        ParsedGgenToml::DeclarativeRules(manifest) => {
            let mut sources = Vec::with_capacity(1 + manifest.ontology.imports.len());
            let ontology_path = root.join(&manifest.ontology.source);
            sources.push(read_ontology_file(root, &ontology_path)?);
            for import in &manifest.ontology.imports {
                let import_path = root.join(import);
                sources.push(read_ontology_file(root, &import_path)?);
            }
            sources
        }
        ParsedGgenToml::Frontmatter(config) => {
            let packs = crate::pack::resolve(&config, root)?;
            let ontology_path = root.join(&config.ontology.source);
            let mut sources = Vec::with_capacity(
                1 + packs
                    .iter()
                    .map(|p| 1 + p.extra_ontology_paths.len())
                    .sum::<usize>(),
            );
            sources.push(read_ontology_file(root, &ontology_path)?);
            for pack in &packs {
                sources.push(read_ontology_file(root, &pack.ontology_path)?);
                for (declared, extra_path) in &pack.extra_ontology_paths {
                    let (_, content) = read_ontology_file(root, extra_path)?;
                    sources.push((declared.clone(), content));
                }
            }
            sources
        }
    };
    let documents: Vec<TurtleDocument<'_>> = sources
        .iter()
        .map(|(label, content)| TurtleDocument::new(label, content))
        .collect();
    let _receipt = graph.insert_turtle_documents(&documents)?;
    Ok(graph)
}
