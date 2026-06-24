use std::path::Path;
use oxigraph::{
    model::*,
    sparql::{QueryResults, Variable},
    store::Store,
    io::RdfFormat,
};
use serde::{Deserialize, Serialize};
use crate::error::Result;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RepoCatalogEntry {
    pub name: String,
    pub github_url: String,
    pub short_desc: String,
    pub primary_language: Option<String>,
}

const CATALOG_QUERY: &str = r#"
PREFIX doap: <http://usefulinc.com/ns/doap#>
PREFIX ggen: <https://ggen.dev/ontology/core#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?name ?homepage ?shortdesc ?lang
WHERE {
  ?project a doap:Project ;
           doap:name ?name ;
           doap:homepage ?homepage .
  OPTIONAL { ?project doap:shortdesc ?shortdesc . }
  OPTIONAL { ?project ggen:primaryLanguage ?langNode .
             ?langNode rdfs:label ?lang . }
}
ORDER BY ?name
"#;

pub fn load_catalog(ttl_path: &Path) -> Result<Vec<RepoCatalogEntry>> {
    let store = Store::new()?;
    let content = std::fs::read_to_string(ttl_path)?;
    store.load_from_reader(RdfFormat::Turtle, content.as_bytes())
        .map_err(|e| crate::error::DaemonError::Scheduler(e.to_string()))?;

    #[allow(deprecated)] // migrate to SparqlEvaluator when oxigraph stabilises the API
    let results = store.query(CATALOG_QUERY)?;
    let QueryResults::Solutions(solutions) = results else {
        return Ok(vec![]);
    };

    let v_name = Variable::new_unchecked("name");
    let v_home = Variable::new_unchecked("homepage");
    let v_desc = Variable::new_unchecked("shortdesc");
    let v_lang = Variable::new_unchecked("lang");

    let mut entries = Vec::new();
    for row in solutions {
        let row = row?;
        let name = match row.get(&v_name) {
            Some(Term::Literal(l)) => l.value().to_owned(),
            _ => continue,
        };
        let github_url = match row.get(&v_home) {
            Some(Term::NamedNode(n)) => n.as_str().to_owned(),
            _ => format!("https://github.com/seanchatmangpt/{}", name),
        };
        let short_desc = match row.get(&v_desc) {
            Some(Term::Literal(l)) => l.value().to_owned(),
            _ => String::new(),
        };
        let primary_language = match row.get(&v_lang) {
            Some(Term::Literal(l)) => Some(l.value().to_owned()),
            _ => None,
        };
        entries.push(RepoCatalogEntry { name, github_url, short_desc, primary_language });
    }
    Ok(entries)
}

/// Append a new DOAP repo stanza to an existing catalog TTL file.
///
/// Idempotent at the load level only — it does NOT check for duplicates before
/// writing.  Callers should filter against `load_catalog` before calling this.
///
/// The stanza follows the DOAP vocabulary used by the existing catalog.
pub fn append_entry(ttl_path: &Path, entry: &RepoCatalogEntry) -> Result<()> {
    let lang_stanza = if let Some(ref lang) = entry.primary_language {
        format!(
            "    <https://ggen.dev/ontology/core#primaryLanguage> [ <http://www.w3.org/2000/01/rdf-schema#label> {:?} ] ;\n",
            lang
        )
    } else {
        String::new()
    };

    let desc_stanza = if entry.short_desc.is_empty() {
        String::new()
    } else {
        format!("    <http://usefulinc.com/ns/doap#shortdesc> {:?} ;\n", entry.short_desc)
    };

    let stanza = format!(
        "\n<{}> a <http://usefulinc.com/ns/doap#Project> ;\n    <http://usefulinc.com/ns/doap#name> {:?} ;\n    <http://usefulinc.com/ns/doap#homepage> <{}> ;\n{}{}..\n",
        entry.github_url,
        entry.name,
        entry.github_url,
        desc_stanza,
        lang_stanza,
    );

    let mut file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(ttl_path)?;
    use std::io::Write;
    write!(file, "{}", stanza)?;
    Ok(())
}

/// Filter catalog entries by primary language (case-insensitive).
pub fn filter_by_language<'a>(
    entries: &'a [RepoCatalogEntry],
    language: &str,
) -> Vec<&'a RepoCatalogEntry> {
    entries
        .iter()
        .filter(|e| {
            e.primary_language
                .as_deref()
                .map(|l| l.eq_ignore_ascii_case(language))
                .unwrap_or(false)
        })
        .collect()
}
