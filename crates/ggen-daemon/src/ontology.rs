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
pub struct JobDef {
    pub dispatch_iri: String,
    pub cron_expr: String,
    pub spec_manifest: String,
    pub language: Option<String>,
    pub estimated_commits: Option<i64>,
}

const JOBS_QUERY: &str = r#"
PREFIX cron: <https://ggen.dev/ontology/cron#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?dispatch ?cronExpr ?specManifest ?lang ?estCommits
WHERE {
  ?batch cron:dispatchBundle ?dispatch .
  ?dispatch cron:cronExpression ?cronExpr ;
            cron:specManifest ?specManifest .
  OPTIONAL { ?dispatch cron:appliesTo ?langNode .
             ?langNode rdfs:label ?lang . }
  OPTIONAL { ?dispatch cron:estimatedCommits ?estCommits . }
}
ORDER BY ?dispatch
"#;

pub fn load_jobs(ttl_path: &Path) -> Result<Vec<JobDef>> {
    let store = Store::new()?;
    let content = std::fs::read_to_string(ttl_path)?;
    store.load_from_reader(RdfFormat::Turtle, content.as_bytes())
        .map_err(|e| crate::error::DaemonError::Scheduler(e.to_string()))?;

    let results = store.query(JOBS_QUERY)?;
    let QueryResults::Solutions(solutions) = results else {
        return Ok(vec![]);
    };

    let mut jobs = Vec::new();
    let v_dispatch = Variable::new_unchecked("dispatch");
    let v_cron = Variable::new_unchecked("cronExpr");
    let v_manifest = Variable::new_unchecked("specManifest");
    let v_lang = Variable::new_unchecked("lang");
    let v_est = Variable::new_unchecked("estCommits");

    for row in solutions {
        let row = row?;
        let dispatch_iri = match row.get(&v_dispatch) {
            Some(Term::NamedNode(n)) => n.as_str().to_owned(),
            _ => continue,
        };
        let cron_expr = match row.get(&v_cron) {
            Some(Term::Literal(l)) => l.value().to_owned(),
            _ => continue,
        };
        let spec_manifest = match row.get(&v_manifest) {
            Some(Term::Literal(l)) => l.value().to_owned(),
            _ => continue,
        };
        let language = match row.get(&v_lang) {
            Some(Term::Literal(l)) => Some(l.value().to_owned()),
            _ => None,
        };
        let estimated_commits = match row.get(&v_est) {
            Some(Term::Literal(l)) => l.value().parse::<i64>().ok(),
            _ => None,
        };
        jobs.push(JobDef { dispatch_iri, cron_expr, spec_manifest, language, estimated_commits });
    }
    Ok(jobs)
}
