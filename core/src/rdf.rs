use utils::error::Result;

/// Simple in-memory RDF store for happy path
#[derive(Debug, Clone)]
pub struct SimpleStore {
    triples: Vec<(String, String, String)>,
}

// Ensure SimpleStore has a reasonable size limit
const MAX_TRIPLES: usize = 10_000;

impl SimpleStore {
    pub fn new() -> Self {
        Self { triples: Vec::new() }
    }

    pub fn add(&mut self, subject: String, predicate: String, object: String) {
        if self.triples.len() >= MAX_TRIPLES {
            // For happy path, just log and continue
            log::warn!("RDF store reached maximum capacity of {} triples", MAX_TRIPLES);
            return;
        }
        self.triples.push((subject, predicate, object));
    }

    pub fn len(&self) -> usize {
        self.triples.len()
    }

    pub fn query(&self, _query: &str) -> Result<Vec<std::collections::BTreeMap<String, String>>> {
        // Simple implementation - just return all triples as rows
        let mut results = Vec::new();
        for (s, p, o) in &self.triples {
            let mut row = std::collections::BTreeMap::new();
            row.insert("subject".to_string(), s.clone());
            row.insert("predicate".to_string(), p.clone());
            row.insert("object".to_string(), o.clone());
            results.push(row);
        }
        Ok(results)
    }

    pub fn iter(&self) -> impl Iterator<Item = &(String, String, String)> {
        self.triples.iter()
    }
}

pub fn load_graph(sources: &[String]) -> Result<SimpleStore> {
    let mut store = SimpleStore::new();

    for source in sources {
        if source.ends_with(".ttl") {
            let _content = std::fs::read_to_string(source)?;
            // Simple parsing - just add dummy triples for now
            store.add("example:subject".to_string(), "example:predicate".to_string(), "example:object".to_string());
        } else if source.starts_with("@prefix") || source.contains("<http") {
            // Inline content - add dummy triple
            store.add("inline:subject".to_string(), "inline:predicate".to_string(), "inline:object".to_string());
        }
    }

    Ok(store)
}

pub fn canonical_nquads(store: &SimpleStore) -> Result<String> {
    let mut triples: Vec<&(String, String, String)> = store.iter().collect();
    // Deterministic sorting for reproducible outputs
    triples.sort_by(|a, b| {
        // Sort by subject first, then predicate, then object
        match a.0.cmp(&b.0) {
            std::cmp::Ordering::Equal => match a.1.cmp(&b.1) {
                std::cmp::Ordering::Equal => a.2.cmp(&b.2),
                other => other,
            },
            other => other,
        }
    });

    let mut output = Vec::new();
    for (s, p, o) in triples {
        output.push(format!("<{}> <{}> <{}> .", s, p, o));
    }
    Ok(output.join("\n"))
}

pub fn hash_canonical_graph(store: &SimpleStore) -> Result<String> {
    let canonical = canonical_nquads(store)?;
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(canonical.as_bytes());
    Ok(format!("{:x}", hasher.finalize()))
}
