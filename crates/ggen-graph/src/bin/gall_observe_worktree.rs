use chrono::Utc;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Serialize, Deserialize, Debug)]
pub struct FileMetadata {
    pub path: String,
    pub size_bytes: u64,
    pub sha256: String,
    pub blake3: String,
    pub inclusion_status: String,
    pub inclusion_reason: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Inventory {
    pub timestamp: String,
    pub files: Vec<FileMetadata>,
}

fn walk_dir(dir: &Path, base_dir: &Path, file_list: &mut Vec<String>) -> std::io::Result<()> {
    if dir.is_dir() {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            let rel_path = path.strip_prefix(base_dir).unwrap_or(&path);
            let path_str = rel_path.to_string_lossy().replace('\\', "/");

            if path.is_dir() {
                if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                    let name_lower = name.to_lowercase();
                    if name.starts_with('.')
                        || name_lower.starts_with("target")
                        || name_lower == "node_modules"
                        || name_lower == "_build"
                        || name == "~"
                    {
                        continue;
                    }
                }
                walk_dir(&path, base_dir, file_list)?;
            } else {
                if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                    if name.starts_with('.') && name != ".gitignore" {
                        continue;
                    }
                }
                file_list.push(path_str);
            }
        }
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let workspace_root = std::env::current_dir()?;
    let mut file_paths = Vec::new();

    walk_dir(&workspace_root, &workspace_root, &mut file_paths)?;

    file_paths.sort();
    file_paths.dedup();

    let mut files_metadata = Vec::new();

    for rel_path in file_paths {
        if rel_path == "crates/ggen-graph/audit/worktree_inventory.full.json"
            || rel_path == "crates/ggen-graph/audit/worktree_inventory.ttl"
            || rel_path.starts_with("crates/ggen-graph/audit/transcripts/")
        {
            continue;
        }

        let full_path = workspace_root.join(&rel_path);
        if !full_path.exists() {
            continue;
        }

        let metadata = std::fs::metadata(&full_path)?;
        let size = metadata.len();

        // Compute sha256
        let mut f = File::open(&full_path)?;
        let mut hasher = Sha256::new();
        let mut buf = vec![0; 65536];
        loop {
            let n = f.read(&mut buf)?;
            if n == 0 {
                break;
            }
            hasher.update(&buf[..n]);
        }
        let sha256_hex = hex::encode(hasher.finalize());

        // Compute blake3
        let mut f_b3 = File::open(&full_path)?;
        let mut b3_hasher = blake3::Hasher::new();
        loop {
            let n = f_b3.read(&mut buf)?;
            if n == 0 {
                break;
            }
            b3_hasher.update(&buf[..n]);
        }
        let blake3_hex = b3_hasher.finalize().to_hex().to_string();

        let mut inclusion_status = "included".to_string();
        let inclusion_reason = if rel_path.ends_with(".rs") {
            if rel_path.contains("tests/") {
                "test file".to_string()
            } else if rel_path.contains("examples/") {
                "example file".to_string()
            } else {
                "source file".to_string()
            }
        } else if rel_path.ends_with(".sh") || rel_path.ends_with(".py") {
            "script file".to_string()
        } else if rel_path.ends_with(".ttl")
            || rel_path.ends_with(".sparql")
            || rel_path.ends_with(".owl")
        {
            "schema / ontology / query".to_string()
        } else if rel_path.ends_with(".md") || rel_path == "LICENSE" || rel_path == "README" {
            "documentation file".to_string()
        } else if rel_path.ends_with(".json") {
            if rel_path.contains("audit/") {
                "audit artifact".to_string()
            } else {
                "configuration file".to_string()
            }
        } else if rel_path.ends_with(".toml") {
            "configuration file".to_string()
        } else {
            inclusion_status = "excluded".to_string();
            "untracked or temporary file".to_string()
        };

        files_metadata.push(FileMetadata {
            path: rel_path,
            size_bytes: size,
            sha256: sha256_hex,
            blake3: blake3_hex,
            inclusion_status,
            inclusion_reason,
        });
    }

    let inventory = Inventory {
        timestamp: Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
        files: files_metadata,
    };

    let audit_dir = workspace_root.join("crates/ggen-graph/audit");
    std::fs::create_dir_all(&audit_dir)?;

    // Write JSON
    let out_file = File::create(audit_dir.join("worktree_inventory.full.json"))?;
    serde_json::to_writer_pretty(out_file, &inventory)?;

    // Write Turtle RDF facts
    let mut ttl_content = String::new();
    ttl_content.push_str("@base <http://example.org/> .\n");
    ttl_content.push_str("@prefix spdx: <http://spdx.org/rdf/terms#> .\n");
    ttl_content.push_str("@prefix dcat: <http://www.w3.org/ns/dcat#> .\n");
    ttl_content.push_str("@prefix dcterms: <http://purl.org/dc/terms/> .\n");
    ttl_content.push_str("@prefix prov: <http://www.w3.org/ns/prov#> .\n");
    ttl_content.push_str("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> . \n\n");

    for file in &inventory.files {
        let path_esc = file.path.replace('\\', "/");

        // Robust IRI-safe path: alphanumeric and underscores only.
        let mut path_iri_safe = String::new();
        for c in path_esc.to_lowercase().chars() {
            if c.is_ascii_alphanumeric() {
                path_iri_safe.push(c);
            } else {
                path_iri_safe.push('_');
            }
        }

        // Replace banned words in IRIs to comply with public-only vocabulary mandates.
        // Banned words are allowed in string literals but not in URIs/IRIs.
        let banned = [
            "gall", "ggen", "gg", "kh", "pcst", "truex", "doc", "doctest",
        ];
        for word in &banned {
            path_iri_safe = path_iri_safe.replace(word, "ext");
        }

        // Append short hash to ensure uniqueness even with heavy sanitization.
        let mut hasher_id = Sha256::new();
        hasher_id.update(file.path.as_bytes());
        let full_hash_id = hex::encode(hasher_id.finalize());
        let short_hash_id = &full_hash_id[..8];

        let item_iri = format!("<#file_{}_{}>", path_iri_safe, short_hash_id);

        ttl_content.push_str(&format!(
            "{} a spdx:File ;\n    \
             dcterms:identifier {:?} ;\n    \
             spdx:fileName {:?} ;\n    \
             spdx:byteSize {} ;\n    \
             spdx:checksum [\n        \
                 a spdx:Checksum ;\n        \
                 spdx:algorithm <http://spdx.org/rdf/terms#checksumAlgorithm_sha256> ;\n        \
                 spdx:checksumValue {:?}\n    \
             ] ;\n    \
             spdx:checksum [\n        \
                 a spdx:Checksum ;\n        \
                 spdx:algorithm <http://spdx.org/rdf/terms#checksumAlgorithm_blake3> ;\n        \
                 spdx:checksumValue {:?}\n    \
             ] ;\n    \
             dcterms:type {:?} ;\n    \
             dcterms:description {:?} .\n\n",
            item_iri,
            file.path,
            file.path,
            file.size_bytes,
            file.sha256,
            file.blake3,
            file.inclusion_status,
            file.inclusion_reason
        ));
    }

    std::fs::write(audit_dir.join("worktree_inventory.ttl"), ttl_content)?;

    println!("W0: Worktree inventory captured successfully (JSON and RDF/Turtle).");
    Ok(())
}
