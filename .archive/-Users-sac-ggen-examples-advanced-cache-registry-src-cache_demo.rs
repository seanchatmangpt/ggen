//! Demonstrates CacheManager usage patterns with production-ready error handling

use anyhow::{Context, Result};
use colored::*;
use ggen_core::cache::CacheManager;
use sha2::{Digest, Sha256};
use std::path::Path;
use std::time::Instant;

/// Demonstrates basic cache operations
pub async fn demonstrate_basic_caching(cache: &CacheManager) -> Result<()> {
    println!("\n{}", "=== Basic Cache Operations ===".bright_blue().bold());

    let key = "template:hello-world:v1.0.0";
    let content = b"Hello, World! This is cached content.";

    // Write to cache
    println!("\n{}", "Writing to cache...".yellow());
    let cache_path = cache.get_path(key);
    cache.write(key, content)
        .context("Failed to write to cache")?;
    println!("✓ Cached at: {}", cache_path.display().to_string().green());

    // Read from cache
    println!("\n{}", "Reading from cache...".yellow());
    let cached_content = cache.read(key)
        .context("Failed to read from cache")?;

    if cached_content == content {
        println!("✓ {}", "Content matches!".green());
    } else {
        println!("✗ {}", "Content mismatch!".red());
    }

    // Check if cached
    println!("\n{}", "Checking cache status...".yellow());
    if cache.is_cached(key) {
        println!("✓ Key '{}' is cached", key.green());
    } else {
        println!("✗ Key '{}' is not cached", key.red());
    }

    // Get cache statistics
    let cache_dir = cache.cache_dir();
    println!("\n{}", "Cache directory:".cyan());
    println!("  {}", cache_dir.display());

    Ok(())
}

/// Demonstrates deterministic caching with SHA256 hashing
pub async fn demonstrate_deterministic_caching(cache: &CacheManager) -> Result<()> {
    println!("\n{}", "=== Deterministic Caching with SHA256 ===".bright_blue().bold());

    let template_content = r#"
    name: {{ name }}
    description: {{ description }}
    version: 1.0.0
    "#;

    // Generate deterministic cache key using SHA256
    let mut hasher = Sha256::new();
    hasher.update(template_content.as_bytes());
    let hash = hasher.finalize();
    let cache_key = format!("template:sha256:{}", hex::encode(hash));

    println!("\n{}", "Generated cache key:".yellow());
    println!("  {}", cache_key.bright_white());

    // Cache the content
    cache.write(&cache_key, template_content.as_bytes())
        .context("Failed to write template to cache")?;
    println!("\n✓ {}", "Template cached with SHA256 key".green());

    // Verify cache hit with same content
    let retrieved = cache.read(&cache_key)
        .context("Failed to read from cache")?;

    if retrieved == template_content.as_bytes() {
        println!("✓ {}", "Cache hit verified!".green());
    }

    // Demonstrate that different content produces different key
    let modified_content = template_content.replace("1.0.0", "2.0.0");
    let mut hasher2 = Sha256::new();
    hasher2.update(modified_content.as_bytes());
    let hash2 = hasher2.finalize();
    let cache_key2 = format!("template:sha256:{}", hex::encode(hash2));

    println!("\n{}", "Modified content produces different key:".yellow());
    println!("  {}", cache_key2.bright_white());
    println!("  Keys differ: {}", (cache_key != cache_key2).to_string().green());

    Ok(())
}

/// Demonstrates cache performance benefits
pub async fn demonstrate_cache_performance(cache: &CacheManager) -> Result<()> {
    println!("\n{}", "=== Cache Performance Demonstration ===".bright_blue().bold());

    let key = "perf:large-template";
    let large_content: Vec<u8> = (0..1_000_000)
        .map(|i| (i % 256) as u8)
        .collect();

    // First write (cache miss)
    println!("\n{}", "First write (cache miss):".yellow());
    let start = Instant::now();
    cache.write(key, &large_content)
        .context("Failed to write to cache")?;
    let write_duration = start.elapsed();
    println!("  Time: {:.2}ms", write_duration.as_secs_f64() * 1000.0);

    // First read (cache hit)
    println!("\n{}", "First read (cache hit):".yellow());
    let start = Instant::now();
    let _cached = cache.read(key)
        .context("Failed to read from cache")?;
    let read_duration = start.elapsed();
    println!("  Time: {:.2}ms", read_duration.as_secs_f64() * 1000.0);

    // Multiple subsequent reads (demonstrating cache benefits)
    println!("\n{}", "10 subsequent reads:".yellow());
    let start = Instant::now();
    for _ in 0..10 {
        let _cached = cache.read(key)
            .context("Failed to read from cache")?;
    }
    let batch_duration = start.elapsed();
    println!("  Total time: {:.2}ms", batch_duration.as_secs_f64() * 1000.0);
    println!("  Average per read: {:.2}ms", (batch_duration.as_secs_f64() * 1000.0) / 10.0);

    println!("\n{}", "Performance Summary:".cyan().bold());
    println!("  Initial write: {:.2}ms", write_duration.as_secs_f64() * 1000.0);
    println!("  Cache read: {:.2}ms", read_duration.as_secs_f64() * 1000.0);
    println!("  Data size: {} bytes", large_content.len().to_string().bright_white());

    Ok(())
}

/// Demonstrates cache invalidation strategies
pub async fn demonstrate_cache_invalidation(cache: &CacheManager) -> Result<()> {
    println!("\n{}", "=== Cache Invalidation Strategies ===".bright_blue().bold());

    // Strategy 1: Version-based invalidation
    println!("\n{}", "Strategy 1: Version-based keys".yellow());
    let base_key = "template:my-app";

    for version in ["1.0.0", "1.1.0", "2.0.0"] {
        let versioned_key = format!("{}:{}", base_key, version);
        let content = format!("Template version {}", version);
        cache.write(&versioned_key, content.as_bytes())
            .context("Failed to write versioned cache")?;
        println!("  ✓ Cached: {}", versioned_key.green());
    }

    println!("\n{}", "  Each version is independently cached".cyan());
    println!("  {}", "Old versions can be explicitly cleared".cyan());

    // Strategy 2: Time-based invalidation (metadata approach)
    println!("\n{}", "Strategy 2: Time-based keys".yellow());
    let timestamp = chrono::Utc::now().timestamp();
    let time_key = format!("template:timed:{}:{}", base_key, timestamp);
    cache.write(&time_key, b"Time-sensitive content")
        .context("Failed to write time-based cache")?;
    println!("  ✓ Cached: {}", time_key.green());
    println!("  {}", "Key includes timestamp for automatic expiry".cyan());

    // Strategy 3: Content-hash based (immutable caching)
    println!("\n{}", "Strategy 3: Content-hash based (immutable)".yellow());
    let content = b"Immutable content";
    let mut hasher = Sha256::new();
    hasher.update(content);
    let hash = hex::encode(hasher.finalize());
    let hash_key = format!("template:immutable:{}", &hash[..16]);
    cache.write(&hash_key, content)
        .context("Failed to write hash-based cache")?;
    println!("  ✓ Cached: {}", hash_key.green());
    println!("  {}", "Content-addressed, never needs invalidation".cyan());

    // Strategy 4: Clear specific cache entry
    println!("\n{}", "Strategy 4: Explicit clearing".yellow());
    let temp_key = "template:temporary";
    cache.write(temp_key, b"Temporary content")
        .context("Failed to write temporary cache")?;
    println!("  ✓ Cached: {}", temp_key.green());

    cache.clear(temp_key)
        .context("Failed to clear cache")?;
    println!("  ✓ Cleared: {}", temp_key.red());

    if !cache.is_cached(temp_key) {
        println!("  ✓ {}", "Verified: key no longer cached".green());
    }

    Ok(())
}

/// Demonstrates cache namespace organization
pub async fn demonstrate_cache_namespaces(cache: &CacheManager) -> Result<()> {
    println!("\n{}", "=== Cache Namespace Organization ===".bright_blue().bold());

    // Organize by type
    let namespaces = [
        ("templates", vec!["react-app", "vue-app", "angular-app"]),
        ("dependencies", vec!["lodash", "axios", "react"]),
        ("generated", vec!["config", "schema", "types"]),
        ("build", vec!["artifacts", "manifests", "lockfiles"]),
    ];

    println!("\n{}", "Organizing cache by namespace:".yellow());

    for (namespace, items) in &namespaces {
        println!("\n  {}:", namespace.bright_cyan());
        for item in items {
            let key = format!("{}:{}", namespace, item);
            let content = format!("Content for {}", item);
            cache.write(&key, content.as_bytes())
                .context("Failed to write namespaced cache")?;
            println!("    ✓ {}", key.green());
        }
    }

    println!("\n{}", "Benefits of namespace organization:".cyan().bold());
    println!("  • Easy bulk operations (clear all templates)");
    println!("  • Logical organization and discovery");
    println!("  • Different retention policies per namespace");
    println!("  • Reduced key collisions");

    Ok(())
}

/// Demonstrates cache warming strategies
pub async fn demonstrate_cache_warming(cache: &CacheManager) -> Result<()> {
    println!("\n{}", "=== Cache Warming Strategies ===".bright_blue().bold());

    println!("\n{}", "Pre-populating frequently used templates...".yellow());

    let popular_templates = vec![
        ("react-component", "export default function Component() { }"),
        ("vue-component", "<template><div></div></template>"),
        ("typescript-config", r#"{"compilerOptions": {}}"#),
        ("package-json", r#"{"name": "app", "version": "1.0.0"}"#),
        ("dockerfile", "FROM node:18\nWORKDIR /app"),
    ];

    let start = Instant::now();

    for (name, content) in &popular_templates {
        let key = format!("template:popular:{}", name);
        cache.write(&key, content.as_bytes())
            .context("Failed to warm cache")?;
        println!("  ✓ Warmed: {}", key.green());
    }

    let duration = start.elapsed();
    println!("\n{}", "Cache warming complete:".cyan().bold());
    println!("  {} templates", popular_templates.len());
    println!("  {:.2}ms total", duration.as_secs_f64() * 1000.0);
    println!("  {:.2}ms per template", (duration.as_secs_f64() * 1000.0) / popular_templates.len() as f64);

    Ok(())
}

/// Runs all cache demonstrations
pub async fn run_all_demos(cache_dir: Option<&Path>) -> Result<()> {
    let cache = if let Some(dir) = cache_dir {
        CacheManager::new(dir.to_path_buf())
    } else {
        CacheManager::default()
    };

    demonstrate_basic_caching(&cache).await?;
    demonstrate_deterministic_caching(&cache).await?;
    demonstrate_cache_performance(&cache).await?;
    demonstrate_cache_invalidation(&cache).await?;
    demonstrate_cache_namespaces(&cache).await?;
    demonstrate_cache_warming(&cache).await?;

    println!("\n{}", "=== All Cache Demos Complete ===".bright_green().bold());

    Ok(())
}
