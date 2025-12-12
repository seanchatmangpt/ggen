use std::collections::HashSet;
use std::env;
use std::error::Error;
use std::fs;

/// Lightweight BibTeX sanity checker: detects missing cite keys and duplicates.
fn main() -> Result<(), Box<dyn Error>> {
    let path = env::args().nth(1).ok_or("usage: bibtex-parser <file>")?;
    let content = fs::read_to_string(&path)?;

    let mut seen_keys = HashSet::new();
    let mut errors: Vec<String> = Vec::new();

    for (idx, raw) in content.split('@').enumerate() {
        // Skip preamble before the first entry
        if idx == 0 {
            continue;
        }

        // Expect pattern: TYPE{key,
        let header_end = raw.find('{');
        let Some(header_end) = header_end else {
            errors.push(format!("entry {} missing '{{'", idx));
            continue;
        };
        let rest = &raw[header_end + 1..];
        let key_end = rest.find(',');
        let Some(key_end) = key_end else {
            errors.push(format!("entry {} missing cite key separator ','", idx));
            continue;
        };
        let cite_key = rest[..key_end].trim();
        if cite_key.is_empty() {
            errors.push(format!("entry {} has empty cite key", idx));
            continue;
        }
        if !seen_keys.insert(cite_key.to_owned()) {
            errors.push(format!("duplicate cite key: {}", cite_key));
        }

        // Basic required fields
        if !rest.contains("title") {
            errors.push(format!("{} missing title", cite_key));
        }
        if !rest.contains("author") {
            errors.push(format!("{} missing author", cite_key));
        }
    }

    if errors.is_empty() {
        println!("ok: {} entries validated", seen_keys.len());
        return Ok(());
    }

    for err in &errors {
        eprintln!("error: {}", err);
    }
    Err("bibtex validation failed".into())
}
