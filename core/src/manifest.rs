use crate::frontmatter::{Frontmatter, TemplateSpec};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use utils::error::Result;

#[derive(Debug)]
pub struct ManifestInputs {
    pub canonical_graph: String,
    pub canonical_shapes: String,
    pub canonical_frontmatter: String,
    pub canonical_template: String,
    pub seed: String,
    pub canonical_rows: String,
}

impl ManifestInputs {
    pub fn from_all(
        fm: &Frontmatter, spec: &TemplateSpec, data_store: &crate::rdf::SimpleStore,
        shapes_store: &crate::rdf::SimpleStore, _bound: &BTreeMap<String, String>,
        matrix_rows: &[BTreeMap<String, String>],
    ) -> Result<Self> {
        // H_G: hash of sorted quads (merged data graph)
        let canonical_graph = crate::rdf::canonical_nquads(data_store)?;

        // H_S: hash of shapes graph
        let canonical_shapes = if shapes_store.len() > 0 {
            crate::rdf::canonical_nquads(shapes_store)?
        } else {
            String::new()
        };

        // H_T: hash of frontmatter-rendered + body bytes
        let rendered_frontmatter = serde_yaml::to_string(fm)?;
        let canonical_template = format!("{}\n---\n{}", rendered_frontmatter, spec.content);

        // H_R: hash of ordered matrix rows (canonical CSV-like format)
        let canonical_rows = canonicalize_rows(matrix_rows)?;

        let seed = fm.seed();

        Ok(Self {
            canonical_graph,
            canonical_shapes,
            canonical_frontmatter: rendered_frontmatter,
            canonical_template,
            seed,
            canonical_rows,
        })
    }
}

pub fn compute_manifest_key(inputs: &ManifestInputs) -> String {
    let mut hasher = Sha256::new();
    hasher.update(inputs.canonical_graph.as_bytes());
    hasher.update(inputs.canonical_shapes.as_bytes());
    hasher.update(inputs.canonical_frontmatter.as_bytes());
    hasher.update(inputs.canonical_template.as_bytes());
    hasher.update(inputs.seed.as_bytes());
    hasher.update(inputs.canonical_rows.as_bytes());
    format!("{:x}", hasher.finalize())
}

fn canonicalize_rows(rows: &[BTreeMap<String, String>]) -> Result<String> {
    if rows.is_empty() {
        return Ok(String::new());
    }

    // Get all keys across all rows (deterministic order)
    let mut all_keys: Vec<String> = rows
        .iter()
        .flat_map(|row| row.keys().cloned())
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .collect();

    all_keys.sort();

    let mut output = Vec::new();
    output.push(all_keys.join(","));

    for row in rows {
        let mut values = Vec::new();
        for key in &all_keys {
            let value = row.get(key).cloned().unwrap_or_default();
            values.push(value);
        }
        output.push(values.join(","));
    }

    Ok(output.join("\n"))
}
