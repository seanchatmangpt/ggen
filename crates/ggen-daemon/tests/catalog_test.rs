use std::path::PathBuf;

fn ggen_root() -> PathBuf {
    let cwd = std::env::current_dir().unwrap();
    cwd.ancestors()
        .find(|p| p.join(".specify").exists())
        .map(|p| p.to_path_buf())
        .unwrap_or(cwd)
}

#[test]
fn load_catalog_returns_all_repos() {
    let root = ggen_root();
    let catalog_path = root.join(".specify/specs/repos-catalog.ttl");
    if !catalog_path.exists() {
        eprintln!("skipping: catalog not found");
        return;
    }

    let repos = ggen_daemon::catalog::load_catalog(&catalog_path).unwrap();
    assert!(!repos.is_empty(), "catalog should have repos");
    assert!(repos.len() >= 10, "expected at least 10 repos, got {}", repos.len());

    // Every entry must have a non-empty name
    for r in &repos {
        assert!(!r.name.is_empty(), "repo name must not be empty");
    }

    println!("Loaded {} repos. First: {}", repos.len(), repos[0].name);
}

#[test]
fn load_jobs_returns_all_cron_entries() {
    let root = ggen_root();
    let cron_path = root.join(".specify/specs/cron/cron-schedule.ttl");
    if !cron_path.exists() {
        eprintln!("skipping: cron TTL not found");
        return;
    }

    let jobs = ggen_daemon::ontology::load_jobs(&cron_path).unwrap();
    assert!(!jobs.is_empty(), "cron schedule should have jobs");

    for j in &jobs {
        assert!(!j.dispatch_iri.is_empty());
        assert!(!j.spec_manifest.is_empty());
        assert!(!j.cron_expr.is_empty());
    }

    println!("Loaded {} jobs. First manifest: {}", jobs.len(), jobs[0].spec_manifest);
}
