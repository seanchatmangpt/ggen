use std::io::Write;
use tempfile::NamedTempFile;
use ggen_daemon::load_jobs;

#[test]
fn load_jobs_parses_cron_schedule_ttl() {
    let ttl = r#"
@prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl:   <http://www.w3.org/2002/07/owl#> .
@prefix cron:  <https://ggen.dev/ontology/cron#> .
@prefix ggen:  <https://ggen.dev/ontology/core#> .

cron:TestBatch
    a cron:DailyBatch ;
    cron:dispatchBundle cron:TestDispatch .

cron:TestDispatch
    a cron:BundleDispatch ;
    cron:cronExpression "0 9 * * *" ;
    cron:specManifest ".specify/specs/community-health/ggen.toml" ;
    cron:estimatedCommits 177 .
"#;

    let mut f = NamedTempFile::new().expect("tempfile");
    f.write_all(ttl.as_bytes()).expect("write");

    let jobs = load_jobs(f.path()).expect("load_jobs should parse valid TTL");
    assert!(!jobs.is_empty(), "expected at least one job from TTL");

    let job = jobs.iter().find(|j| j.cron_expr == "0 9 * * *").expect("job not found");
    assert_eq!(job.spec_manifest, ".specify/specs/community-health/ggen.toml");
    assert_eq!(job.estimated_commits, Some(177));
}

#[test]
fn load_jobs_returns_empty_for_empty_ttl() {
    let ttl = r#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> ."#;
    let mut f = NamedTempFile::new().expect("tempfile");
    f.write_all(ttl.as_bytes()).expect("write");
    let jobs = load_jobs(f.path()).expect("should not error on empty TTL");
    assert!(jobs.is_empty());
}
