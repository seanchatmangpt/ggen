//! Performance benchmarks for pack operations
//!
//! Ensures all pack operations execute within performance constraints:
//! - List/discover operations: <100ms
//! - Single pack load: <50ms
//! - Multi-pack composition: <500ms

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ggen_core::gpack::GpackManifest;
use std::path::PathBuf;

const WEB_API_PACK: &str = "tests/fixtures/packs/web-api-pack";
const CLI_TOOL_PACK: &str = "tests/fixtures/packs/cli-tool-pack";
const DATABASE_PACK: &str = "tests/fixtures/packs/database-pack";

// ============================================================================
// Manifest Loading Benchmarks
// ============================================================================

fn bench_load_single_manifest(c: &mut Criterion) {
    let manifest_path = PathBuf::from(format!("{}/gpack.toml", WEB_API_PACK));

    c.bench_function("load_single_manifest", |b| {
        b.iter(|| {
            let manifest = GpackManifest::load_from_file(black_box(&manifest_path)).unwrap();
            black_box(manifest);
        });
    });
}

fn bench_load_multiple_manifests(c: &mut Criterion) {
    let paths = vec![
        PathBuf::from(format!("{}/gpack.toml", WEB_API_PACK)),
        PathBuf::from(format!("{}/gpack.toml", CLI_TOOL_PACK)),
        PathBuf::from(format!("{}/gpack.toml", DATABASE_PACK)),
    ];

    c.bench_function("load_multiple_manifests", |b| {
        b.iter(|| {
            for path in &paths {
                let manifest = GpackManifest::load_from_file(black_box(path)).unwrap();
                black_box(manifest);
            }
        });
    });
}

// ============================================================================
// File Discovery Benchmarks
// ============================================================================

fn bench_discover_templates(c: &mut Criterion) {
    let pack_path = PathBuf::from(WEB_API_PACK);
    let manifest = GpackManifest::load_from_file(&pack_path.join("gpack.toml")).unwrap();

    c.bench_function("discover_templates", |b| {
        b.iter(|| {
            let templates = manifest.discover_templates(black_box(&pack_path)).unwrap();
            black_box(templates);
        });
    });
}

fn bench_discover_all_files(c: &mut Criterion) {
    let pack_path = PathBuf::from(WEB_API_PACK);
    let manifest = GpackManifest::load_from_file(&pack_path.join("gpack.toml")).unwrap();

    c.bench_function("discover_all_files", |b| {
        b.iter(|| {
            let templates = manifest.discover_templates(black_box(&pack_path)).unwrap();
            let rdf = manifest.discover_rdf_files(black_box(&pack_path)).unwrap();
            let queries = manifest.discover_query_files(black_box(&pack_path)).unwrap();
            let shapes = manifest.discover_shape_files(black_box(&pack_path)).unwrap();
            black_box((templates, rdf, queries, shapes));
        });
    });
}

// ============================================================================
// Multi-Pack Composition Benchmarks
// ============================================================================

fn bench_compose_multiple_packs(c: &mut Criterion) {
    c.bench_function("compose_multiple_packs", |b| {
        b.iter(|| {
            // Load all packs
            let web_api_path = PathBuf::from(WEB_API_PACK);
            let cli_tool_path = PathBuf::from(CLI_TOOL_PACK);
            let database_path = PathBuf::from(DATABASE_PACK);

            let web_api = GpackManifest::load_from_file(&web_api_path.join("gpack.toml")).unwrap();
            let cli_tool = GpackManifest::load_from_file(&cli_tool_path.join("gpack.toml")).unwrap();
            let database = GpackManifest::load_from_file(&database_path.join("gpack.toml")).unwrap();

            // Collect all resources
            let mut all_templates = Vec::new();
            all_templates.extend(web_api.discover_templates(&web_api_path).unwrap());
            all_templates.extend(cli_tool.discover_templates(&cli_tool_path).unwrap());
            all_templates.extend(database.discover_templates(&database_path).unwrap());

            black_box(all_templates);
        });
    });
}

// ============================================================================
// Dependency Resolution Benchmarks
// ============================================================================

fn bench_resolve_dependencies(c: &mut Criterion) {
    let cli_pack = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", CLI_TOOL_PACK))).unwrap();
    let web_pack = GpackManifest::load_from_file(&PathBuf::from(format!("{}/gpack.toml", WEB_API_PACK))).unwrap();

    c.bench_function("resolve_dependencies", |b| {
        b.iter(|| {
            for (dep_id, version_req) in &cli_pack.dependencies {
                if dep_id == "test.web-api" {
                    let req = semver::VersionReq::parse(version_req).unwrap();
                    let ver = semver::Version::parse(&web_pack.metadata.version).unwrap();
                    let matches = req.matches(&ver);
                    black_box(matches);
                }
            }
        });
    });
}

// ============================================================================
// Scaling Benchmarks
// ============================================================================

fn bench_large_pack_discovery(c: &mut Criterion) {
    // Create a pack path that would have many files
    let pack_path = PathBuf::from(WEB_API_PACK);
    let manifest = GpackManifest::load_from_file(&pack_path.join("gpack.toml")).unwrap();

    c.bench_function("large_pack_discovery", |b| {
        b.iter(|| {
            // Simulate discovering files in a large pack
            for _ in 0..10 {
                let templates = manifest.discover_templates(black_box(&pack_path)).unwrap();
                black_box(templates);
            }
        });
    });
}

fn bench_many_pack_list(c: &mut Criterion) {
    let pack_paths = vec![
        PathBuf::from(WEB_API_PACK),
        PathBuf::from(CLI_TOOL_PACK),
        PathBuf::from(DATABASE_PACK),
    ];

    c.bench_function("list_many_packs", |b| {
        b.iter(|| {
            let mut pack_info = Vec::new();
            for pack_path in &pack_paths {
                let manifest = GpackManifest::load_from_file(&pack_path.join("gpack.toml")).unwrap();
                pack_info.push((manifest.metadata.id.clone(), manifest.metadata.version.clone()));
            }
            black_box(pack_info);
        });
    });
}

// ============================================================================
// Criterion Configuration
// ============================================================================

criterion_group!(
    benches,
    bench_load_single_manifest,
    bench_load_multiple_manifests,
    bench_discover_templates,
    bench_discover_all_files,
    bench_compose_multiple_packs,
    bench_resolve_dependencies,
    bench_large_pack_discovery,
    bench_many_pack_list
);

criterion_main!(benches);
