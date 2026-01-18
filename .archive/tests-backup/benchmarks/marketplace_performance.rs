// Performance Benchmarking Suite for Marketplace Commands
// Comprehensive benchmarks for search, maturity, export, comparison, and recommendation

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId, Throughput};
use ggen_marketplace::*;
use serde_json::json;
use std::time::Duration;
use std::collections::HashMap;

// ============================================================================
// Synthetic Data Generation
// ============================================================================

struct BenchmarkDataGenerator;

impl BenchmarkDataGenerator {
    /// Generate synthetic marketplace packages for benchmarking
    fn generate_packages(count: usize) -> Vec<MarketplacePackage> {
        (0..count)
            .map(|i| {
                let category = match i % 5 {
                    0 => "cli-framework",
                    1 => "web-api",
                    2 => "data-processing",
                    3 => "machine-learning",
                    _ => "developer-tools",
                };

                MarketplacePackage {
                    name: format!("benchmark-package-{}", i),
                    version: format!("1.{}.0", i % 100),
                    description: format!(
                        "Benchmark package {} for performance testing with comprehensive features",
                        i
                    ),
                    category: category.to_string(),
                    tags: vec![
                        format!("tag-{}", i % 10),
                        format!("benchmark"),
                        format!("category-{}", category),
                    ],
                    author: format!("author-{}", i % 20),
                    downloads: (i * 1000) as u64,
                    stars: (i * 10) as u32,
                    forks: (i * 2) as u32,
                    issues: (i % 50) as u32,
                    pull_requests: (i % 30) as u32,
                    last_updated: chrono::Utc::now(),
                    license: "MIT".to_string(),
                    repository: format!("https://github.com/benchmark/package-{}", i),
                    homepage: Some(format!("https://benchmark-{}.dev", i)),
                    dependencies: (0..(i % 10)).map(|d| format!("dep-{}", d)).collect(),
                    maturity_score: None,
                }
            })
            .collect()
    }

    /// Generate search queries with varying complexity
    fn generate_search_queries() -> Vec<SearchQuery> {
        vec![
            SearchQuery {
                text: "cli framework".to_string(),
                category: None,
                tags: vec![],
                min_stars: None,
                max_results: Some(10),
            },
            SearchQuery {
                text: "web api data".to_string(),
                category: Some("web-api".to_string()),
                tags: vec!["rest".to_string(), "json".to_string()],
                min_stars: Some(100),
                max_results: Some(20),
            },
            SearchQuery {
                text: "machine learning".to_string(),
                category: Some("machine-learning".to_string()),
                tags: vec!["ml".to_string(), "ai".to_string(), "neural".to_string()],
                min_stars: Some(500),
                max_results: Some(50),
            },
        ]
    }
}

// ============================================================================
// Search Performance Benchmarks
// ============================================================================

fn benchmark_search_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("marketplace_search");
    group.measurement_time(Duration::from_secs(10));
    group.sample_size(50);

    // Benchmark across different dataset sizes
    for size in [10, 100, 1000].iter() {
        let packages = BenchmarkDataGenerator::generate_packages(*size);
        let queries = BenchmarkDataGenerator::generate_search_queries();

        group.throughput(Throughput::Elements(*size as u64));

        // Simple text search
        group.bench_with_input(
            BenchmarkId::new("text_search", size),
            &packages,
            |b, pkgs| {
                b.iter(|| {
                    for query in &queries {
                        black_box(search_packages(pkgs, &query.text));
                    }
                });
            },
        );

        // Category filtered search
        group.bench_with_input(
            BenchmarkId::new("category_search", size),
            &packages,
            |b, pkgs| {
                b.iter(|| {
                    black_box(search_by_category(pkgs, "cli-framework"));
                });
            },
        );

        // Tag-based search
        group.bench_with_input(
            BenchmarkId::new("tag_search", size),
            &packages,
            |b, pkgs| {
                b.iter(|| {
                    black_box(search_by_tags(pkgs, &["benchmark", "cli-framework"]));
                });
            },
        );

        // Complex multi-criteria search
        group.bench_with_input(
            BenchmarkId::new("complex_search", size),
            &packages,
            |b, pkgs| {
                b.iter(|| {
                    for query in &queries {
                        black_box(advanced_search(
                            pkgs,
                            &query.text,
                            query.category.as_deref(),
                            &query.tags,
                            query.min_stars,
                            query.max_results.unwrap_or(10),
                        ));
                    }
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// Maturity Assessment Benchmarks
// ============================================================================

fn benchmark_maturity_assessment(c: &mut Criterion) {
    let mut group = c.benchmark_group("maturity_assessment");
    group.measurement_time(Duration::from_secs(15));
    group.sample_size(30);

    // Single package assessment
    let package = BenchmarkDataGenerator::generate_packages(1)[0].clone();

    group.bench_function("single_package", |b| {
        b.iter(|| {
            black_box(assess_maturity(&package));
        });
    });

    // Batch assessments
    for batch_size in [10, 100].iter() {
        let packages = BenchmarkDataGenerator::generate_packages(*batch_size);

        group.throughput(Throughput::Elements(*batch_size as u64));

        group.bench_with_input(
            BenchmarkId::new("batch_assessment", batch_size),
            &packages,
            |b, pkgs| {
                b.iter(|| {
                    black_box(batch_assess_maturity(pkgs));
                });
            },
        );

        // Parallel assessment
        group.bench_with_input(
            BenchmarkId::new("parallel_assessment", batch_size),
            &packages,
            |b, pkgs| {
                b.iter(|| {
                    black_box(parallel_assess_maturity(pkgs));
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// Export Performance Benchmarks
// ============================================================================

fn benchmark_export_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("export_performance");
    group.measurement_time(Duration::from_secs(10));
    group.sample_size(30);

    // CSV Export
    for size in [10, 100].iter() {
        let packages = BenchmarkDataGenerator::generate_packages(*size);

        group.throughput(Throughput::Elements(*size as u64));

        group.bench_with_input(
            BenchmarkId::new("csv_export", size),
            &packages,
            |b, pkgs| {
                b.iter(|| {
                    black_box(export_to_csv(pkgs).unwrap());
                });
            },
        );

        // JSON Export
        group.bench_with_input(
            BenchmarkId::new("json_export", size),
            &packages,
            |b, pkgs| {
                b.iter(|| {
                    black_box(export_to_json(pkgs).unwrap());
                });
            },
        );

        // HTML Export with styling
        group.bench_with_input(
            BenchmarkId::new("html_export", size),
            &packages,
            |b, pkgs| {
                b.iter(|| {
                    black_box(export_to_html(pkgs, true).unwrap());
                });
            },
        );

        // Markdown Export
        group.bench_with_input(
            BenchmarkId::new("markdown_export", size),
            &packages,
            |b, pkgs| {
                b.iter(|| {
                    black_box(export_to_markdown(pkgs).unwrap());
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// Comparison Benchmarks
// ============================================================================

fn benchmark_comparison_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("comparison");
    group.measurement_time(Duration::from_secs(10));
    group.sample_size(50);

    let packages = BenchmarkDataGenerator::generate_packages(100);

    // 2-package comparison (typical use case)
    group.bench_function("two_package_comparison", |b| {
        b.iter(|| {
            black_box(compare_packages(&packages[0], &packages[1]));
        });
    });

    // Multiple comparisons in sequence
    group.bench_function("sequential_comparisons", |b| {
        b.iter(|| {
            for i in 0..10 {
                black_box(compare_packages(&packages[i * 2], &packages[i * 2 + 1]));
            }
        });
    });

    // Detailed comparison with all metrics
    group.bench_function("detailed_comparison", |b| {
        b.iter(|| {
            black_box(detailed_compare_packages(
                &packages[0],
                &packages[1],
                true, // include maturity
                true, // include dependencies
                true, // include community metrics
            ));
        });
    });

    group.finish();
}

// ============================================================================
// Recommendation Engine Benchmarks
// ============================================================================

fn benchmark_recommendation_engine(c: &mut Criterion) {
    let mut group = c.benchmark_group("recommendation_engine");
    group.measurement_time(Duration::from_secs(15));
    group.sample_size(30);

    // Recommendation generation across different dataset sizes
    for size in [10, 100].iter() {
        let packages = BenchmarkDataGenerator::generate_packages(*size);
        let user_preferences = UserPreferences {
            categories: vec!["cli-framework".to_string(), "web-api".to_string()],
            min_stars: 100,
            min_maturity_score: Some(7.0),
            max_results: 10,
        };

        group.throughput(Throughput::Elements(*size as u64));

        // Basic recommendations
        group.bench_with_input(
            BenchmarkId::new("basic_recommendations", size),
            &packages,
            |b, pkgs| {
                b.iter(|| {
                    black_box(generate_recommendations(pkgs, &user_preferences));
                });
            },
        );

        // ML-based recommendations
        group.bench_with_input(
            BenchmarkId::new("ml_recommendations", size),
            &packages,
            |b, pkgs| {
                b.iter(|| {
                    black_box(ml_generate_recommendations(pkgs, &user_preferences));
                });
            },
        );

        // Ranking accuracy test
        group.bench_with_input(
            BenchmarkId::new("ranking_accuracy", size),
            &packages,
            |b, pkgs| {
                b.iter(|| {
                    black_box(rank_packages(pkgs, &user_preferences));
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// Memory Usage Benchmarks
// ============================================================================

fn benchmark_memory_usage(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_usage");
    group.measurement_time(Duration::from_secs(10));
    group.sample_size(20);

    // Memory usage for large dataset operations
    for size in [100, 1000].iter() {
        group.throughput(Throughput::Elements(*size as u64));

        group.bench_with_input(
            BenchmarkId::new("package_loading", size),
            size,
            |b, &s| {
                b.iter(|| {
                    let packages = black_box(BenchmarkDataGenerator::generate_packages(s));
                    drop(packages);
                });
            },
        );

        group.bench_with_input(
            BenchmarkId::new("search_and_filter", size),
            size,
            |b, &s| {
                b.iter(|| {
                    let packages = BenchmarkDataGenerator::generate_packages(s);
                    let _results = black_box(search_packages(&packages, "cli framework"));
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// End-to-End Workflow Benchmarks
// ============================================================================

fn benchmark_e2e_workflows(c: &mut Criterion) {
    let mut group = c.benchmark_group("e2e_workflows");
    group.measurement_time(Duration::from_secs(20));
    group.sample_size(20);

    let packages = BenchmarkDataGenerator::generate_packages(100);

    // Full search -> assess -> export workflow
    group.bench_function("search_assess_export", |b| {
        b.iter(|| {
            let results = black_box(search_packages(&packages, "cli framework"));
            let assessed = black_box(batch_assess_maturity(&results));
            let _exported = black_box(export_to_json(&assessed).unwrap());
        });
    });

    // Full recommendation -> compare -> export workflow
    group.bench_function("recommend_compare_export", |b| {
        b.iter(|| {
            let user_prefs = UserPreferences {
                categories: vec!["cli-framework".to_string()],
                min_stars: 100,
                min_maturity_score: Some(7.0),
                max_results: 10,
            };
            let recommendations = black_box(generate_recommendations(&packages, &user_prefs));
            if recommendations.len() >= 2 {
                let _comparison = black_box(compare_packages(&recommendations[0], &recommendations[1]));
            }
            let _exported = black_box(export_to_csv(&recommendations).unwrap());
        });
    });

    group.finish();
}

// ============================================================================
// Mock Implementations (for benchmarking structure)
// ============================================================================

#[derive(Clone, Debug)]
struct MarketplacePackage {
    name: String,
    version: String,
    description: String,
    category: String,
    tags: Vec<String>,
    author: String,
    downloads: u64,
    stars: u32,
    forks: u32,
    issues: u32,
    pull_requests: u32,
    last_updated: chrono::DateTime<chrono::Utc>,
    license: String,
    repository: String,
    homepage: Option<String>,
    dependencies: Vec<String>,
    maturity_score: Option<f64>,
}

struct SearchQuery {
    text: String,
    category: Option<String>,
    tags: Vec<String>,
    min_stars: Option<u32>,
    max_results: Option<usize>,
}

struct UserPreferences {
    categories: Vec<String>,
    min_stars: u32,
    min_maturity_score: Option<f64>,
    max_results: usize,
}

// Mock search functions
fn search_packages(packages: &[MarketplacePackage], query: &str) -> Vec<MarketplacePackage> {
    packages
        .iter()
        .filter(|p| p.name.contains(query) || p.description.contains(query))
        .cloned()
        .collect()
}

fn search_by_category(packages: &[MarketplacePackage], category: &str) -> Vec<MarketplacePackage> {
    packages
        .iter()
        .filter(|p| p.category == category)
        .cloned()
        .collect()
}

fn search_by_tags(packages: &[MarketplacePackage], tags: &[&str]) -> Vec<MarketplacePackage> {
    packages
        .iter()
        .filter(|p| tags.iter().any(|t| p.tags.contains(&t.to_string())))
        .cloned()
        .collect()
}

fn advanced_search(
    packages: &[MarketplacePackage],
    text: &str,
    category: Option<&str>,
    tags: &[String],
    min_stars: Option<u32>,
    max_results: usize,
) -> Vec<MarketplacePackage> {
    let mut results: Vec<_> = packages
        .iter()
        .filter(|p| {
            let text_match = p.name.contains(text) || p.description.contains(text);
            let category_match = category.map_or(true, |c| p.category == c);
            let tags_match = tags.is_empty() || tags.iter().any(|t| p.tags.contains(t));
            let stars_match = min_stars.map_or(true, |s| p.stars >= s);

            text_match && category_match && tags_match && stars_match
        })
        .cloned()
        .collect();

    results.truncate(max_results);
    results
}

// Mock maturity assessment
fn assess_maturity(package: &MarketplacePackage) -> f64 {
    let score = (package.stars as f64 * 0.4)
        + (package.downloads as f64 / 1000.0 * 0.3)
        + (package.forks as f64 * 0.2)
        + ((50 - package.issues) as f64 * 0.1);
    score / 100.0
}

fn batch_assess_maturity(packages: &[MarketplacePackage]) -> Vec<MarketplacePackage> {
    packages
        .iter()
        .map(|p| {
            let mut p = p.clone();
            p.maturity_score = Some(assess_maturity(&p));
            p
        })
        .collect()
}

fn parallel_assess_maturity(packages: &[MarketplacePackage]) -> Vec<MarketplacePackage> {
    use rayon::prelude::*;
    packages
        .par_iter()
        .map(|p| {
            let mut p = p.clone();
            p.maturity_score = Some(assess_maturity(&p));
            p
        })
        .collect()
}

// Mock export functions
fn export_to_csv(packages: &[MarketplacePackage]) -> Result<String, Box<dyn std::error::Error>> {
    let mut csv = String::from("name,version,category,stars,downloads,maturity\n");
    for pkg in packages {
        csv.push_str(&format!(
            "{},{},{},{},{},{}\n",
            pkg.name,
            pkg.version,
            pkg.category,
            pkg.stars,
            pkg.downloads,
            pkg.maturity_score.unwrap_or(0.0)
        ));
    }
    Ok(csv)
}

fn export_to_json(packages: &[MarketplacePackage]) -> Result<String, Box<dyn std::error::Error>> {
    Ok(serde_json::to_string_pretty(packages)?)
}

fn export_to_html(packages: &[MarketplacePackage], styled: bool) -> Result<String, Box<dyn std::error::Error>> {
    let mut html = String::from("<html><head>");
    if styled {
        html.push_str("<style>table { border-collapse: collapse; } td, th { border: 1px solid #ddd; padding: 8px; }</style>");
    }
    html.push_str("</head><body><table><tr><th>Name</th><th>Version</th><th>Stars</th></tr>");
    for pkg in packages {
        html.push_str(&format!(
            "<tr><td>{}</td><td>{}</td><td>{}</td></tr>",
            pkg.name, pkg.version, pkg.stars
        ));
    }
    html.push_str("</table></body></html>");
    Ok(html)
}

fn export_to_markdown(packages: &[MarketplacePackage]) -> Result<String, Box<dyn std::error::Error>> {
    let mut md = String::from("| Name | Version | Stars | Downloads |\n|------|---------|-------|----------|\n");
    for pkg in packages {
        md.push_str(&format!(
            "| {} | {} | {} | {} |\n",
            pkg.name, pkg.version, pkg.stars, pkg.downloads
        ));
    }
    Ok(md)
}

// Mock comparison functions
fn compare_packages(pkg1: &MarketplacePackage, pkg2: &MarketplacePackage) -> HashMap<String, String> {
    let mut comparison = HashMap::new();
    comparison.insert("name_comparison".to_string(), format!("{} vs {}", pkg1.name, pkg2.name));
    comparison.insert("stars_diff".to_string(), format!("{}", pkg1.stars as i64 - pkg2.stars as i64));
    comparison
}

fn detailed_compare_packages(
    pkg1: &MarketplacePackage,
    pkg2: &MarketplacePackage,
    _include_maturity: bool,
    _include_deps: bool,
    _include_community: bool,
) -> HashMap<String, String> {
    compare_packages(pkg1, pkg2)
}

// Mock recommendation functions
fn generate_recommendations(
    packages: &[MarketplacePackage],
    prefs: &UserPreferences,
) -> Vec<MarketplacePackage> {
    let mut results: Vec<_> = packages
        .iter()
        .filter(|p| {
            let category_match = prefs.categories.is_empty() || prefs.categories.contains(&p.category);
            let stars_match = p.stars >= prefs.min_stars;
            let maturity_match = prefs.min_maturity_score.map_or(true, |s| {
                p.maturity_score.unwrap_or(0.0) >= s
            });

            category_match && stars_match && maturity_match
        })
        .cloned()
        .collect();

    results.sort_by(|a, b| b.stars.cmp(&a.stars));
    results.truncate(prefs.max_results);
    results
}

fn ml_generate_recommendations(
    packages: &[MarketplacePackage],
    prefs: &UserPreferences,
) -> Vec<MarketplacePackage> {
    // Simulate ML-based ranking
    let mut results = generate_recommendations(packages, prefs);
    results.sort_by(|a, b| {
        let score_a = (a.stars as f64 * 0.5) + (a.downloads as f64 / 1000.0 * 0.3) + (a.forks as f64 * 0.2);
        let score_b = (b.stars as f64 * 0.5) + (b.downloads as f64 / 1000.0 * 0.3) + (b.forks as f64 * 0.2);
        score_b.partial_cmp(&score_a).unwrap_or(std::cmp::Ordering::Equal)
    });
    results
}

fn rank_packages(packages: &[MarketplacePackage], prefs: &UserPreferences) -> Vec<MarketplacePackage> {
    ml_generate_recommendations(packages, prefs)
}

// ============================================================================
// Criterion Configuration
// ============================================================================

criterion_group!(
    benches,
    benchmark_search_performance,
    benchmark_maturity_assessment,
    benchmark_export_performance,
    benchmark_comparison_performance,
    benchmark_recommendation_engine,
    benchmark_memory_usage,
    benchmark_e2e_workflows
);

criterion_main!(benches);
