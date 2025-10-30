//! Content Routing and Provider Management

use anyhow::{Result, anyhow};
use std::collections::{HashMap, HashSet};
use std::time::{Duration, SystemTime};

/// Content routing for package discovery
#[derive(Debug)]
pub struct ContentRouter {
    providers: HashMap<String, Vec<ContentProvider>>,
    local_content: HashSet<String>,
    cache: ContentCache,
}

/// Content provider information
#[derive(Debug, Clone)]
pub struct ContentProvider {
    pub peer_id: String,
    pub addresses: Vec<String>,
    pub provided_at: SystemTime,
    pub last_seen: SystemTime,
    pub reputation: f64,
}

/// Content cache for faster lookups
#[derive(Debug)]
struct ContentCache {
    entries: HashMap<String, CacheEntry>,
    max_size: usize,
}

#[derive(Debug, Clone)]
struct CacheEntry {
    content_id: String,
    providers: Vec<String>,
    cached_at: SystemTime,
    ttl: Duration,
}

impl ContentRouter {
    pub fn new(cache_size: usize) -> Self {
        Self {
            providers: HashMap::new(),
            local_content: HashSet::new(),
            cache: ContentCache {
                entries: HashMap::new(),
                max_size: cache_size,
            },
        }
    }

    /// Register as a provider for content
    pub fn provide(&mut self, content_id: String) -> Result<()> {
        self.local_content.insert(content_id);
        Ok(())
    }

    /// Stop providing content
    pub fn stop_providing(&mut self, content_id: &str) -> Result<()> {
        self.local_content.remove(content_id);
        Ok(())
    }

    /// Register a remote provider for content
    pub fn add_provider(&mut self, content_id: String, provider: ContentProvider) {
        self.providers
            .entry(content_id.clone())
            .or_insert_with(Vec::new)
            .push(provider.clone());

        // Update cache
        self.cache.entries
            .entry(content_id.clone())
            .or_insert_with(|| CacheEntry {
                content_id,
                providers: Vec::new(),
                cached_at: SystemTime::now(),
                ttl: Duration::from_secs(3600),
            })
            .providers
            .push(provider.peer_id);
    }

    /// Find providers for content
    pub fn find_providers(&self, content_id: &str) -> Vec<ContentProvider> {
        // Check cache first
        if let Some(entry) = self.cache.entries.get(content_id) {
            if !self.is_cache_expired(entry) {
                return self.providers
                    .get(content_id)
                    .cloned()
                    .unwrap_or_default();
            }
        }

        // Return providers from main storage
        self.providers.get(content_id).cloned().unwrap_or_default()
    }

    /// Get best provider based on reputation
    pub fn get_best_provider(&self, content_id: &str) -> Option<ContentProvider> {
        let providers = self.find_providers(content_id);

        providers
            .into_iter()
            .max_by(|a, b| a.reputation.partial_cmp(&b.reputation).unwrap_or(std::cmp::Ordering::Equal))
    }

    /// Check if we provide this content locally
    pub fn is_local_provider(&self, content_id: &str) -> bool {
        self.local_content.contains(content_id)
    }

    /// Update provider reputation
    pub fn update_provider_reputation(&mut self, content_id: &str, peer_id: &str, delta: f64) {
        if let Some(providers) = self.providers.get_mut(content_id) {
            for provider in providers {
                if provider.peer_id == peer_id {
                    provider.reputation = (provider.reputation + delta).clamp(0.0, 100.0);
                    provider.last_seen = SystemTime::now();
                    break;
                }
            }
        }
    }

    /// Remove stale providers
    pub fn cleanup_stale_providers(&mut self, max_age: Duration) {
        let now = SystemTime::now();

        for providers in self.providers.values_mut() {
            providers.retain(|provider| {
                now.duration_since(provider.last_seen)
                    .unwrap_or(Duration::from_secs(0))
                    < max_age
            });
        }

        // Remove empty entries
        self.providers.retain(|_, providers| !providers.is_empty());

        // Cleanup cache
        self.cache.entries.retain(|_, entry| !self.is_cache_expired(entry));
    }

    /// Check if cache entry is expired
    fn is_cache_expired(&self, entry: &CacheEntry) -> bool {
        SystemTime::now()
            .duration_since(entry.cached_at)
            .unwrap_or(Duration::from_secs(0))
            > entry.ttl
    }

    /// Get routing statistics
    pub fn get_stats(&self) -> ContentRoutingStats {
        ContentRoutingStats {
            total_content: self.providers.len(),
            local_content: self.local_content.len(),
            total_providers: self.providers.values().map(|p| p.len()).sum(),
            cache_size: self.cache.entries.len(),
            cache_capacity: self.cache.max_size,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ContentRoutingStats {
    pub total_content: usize,
    pub local_content: usize,
    pub total_providers: usize,
    pub cache_size: usize,
    pub cache_capacity: usize,
}

impl Default for ContentProvider {
    fn default() -> Self {
        Self {
            peer_id: String::new(),
            addresses: Vec::new(),
            provided_at: SystemTime::now(),
            last_seen: SystemTime::now(),
            reputation: 50.0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_provide_content() {
        let mut router = ContentRouter::new(100);
        let result = router.provide("package-1".to_string());

        assert!(result.is_ok());
        assert!(router.is_local_provider("package-1"));
    }

    #[test]
    fn test_add_and_find_providers() {
        let mut router = ContentRouter::new(100);

        let provider = ContentProvider {
            peer_id: "peer1".to_string(),
            addresses: vec!["/ip4/127.0.0.1/tcp/4001".to_string()],
            ..Default::default()
        };

        router.add_provider("package-1".to_string(), provider);

        let providers = router.find_providers("package-1");
        assert_eq!(providers.len(), 1);
    }

    #[test]
    fn test_provider_reputation() {
        let mut router = ContentRouter::new(100);

        let provider = ContentProvider {
            peer_id: "peer1".to_string(),
            reputation: 50.0,
            ..Default::default()
        };

        router.add_provider("package-1".to_string(), provider);
        router.update_provider_reputation("package-1", "peer1", 10.0);

        let providers = router.find_providers("package-1");
        assert_eq!(providers[0].reputation, 60.0);
    }

    #[test]
    fn test_get_best_provider() {
        let mut router = ContentRouter::new(100);

        let provider1 = ContentProvider {
            peer_id: "peer1".to_string(),
            reputation: 50.0,
            ..Default::default()
        };

        let provider2 = ContentProvider {
            peer_id: "peer2".to_string(),
            reputation: 75.0,
            ..Default::default()
        };

        router.add_provider("package-1".to_string(), provider1);
        router.add_provider("package-1".to_string(), provider2);

        let best = router.get_best_provider("package-1");
        assert!(best.is_some());
        assert_eq!(best.unwrap().peer_id, "peer2");
    }
}
