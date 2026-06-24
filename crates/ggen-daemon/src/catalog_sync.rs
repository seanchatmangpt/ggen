//! GitHub repository discovery for catalog auto-expansion (Lever 7).
//!
//! Calls the GitHub REST API to list repos for a given owner, returning
//! non-archived, non-fork repositories.  Results feed into
//! `catalog::append_entry` to grow `repos-catalog.ttl` automatically.

use reqwest::blocking::Client;
use serde::Deserialize;
use crate::error::{DaemonError, Result};

#[derive(Debug, Deserialize)]
struct GithubRepoRaw {
    name: String,
    html_url: String,
    description: Option<String>,
    language: Option<String>,
    fork: bool,
    archived: bool,
    #[serde(default)]
    private: bool,
}

/// A discovered GitHub repository, ready to add to the catalog TTL.
#[derive(Debug, Clone)]
pub struct GithubRepo {
    pub name: String,
    pub html_url: String,
    pub description: String,
    pub language: Option<String>,
}

/// Fetch all non-archived, non-fork, public repositories for `owner`.
///
/// Pass `token = Some(pat)` for an authenticated request (5 000 req/hr rate
/// limit); `None` uses the public unauthenticated rate limit (60 req/hr).
///
/// This is a **blocking** function — call it from `tokio::task::spawn_blocking`
/// or a non-async context.
pub fn discover_repos(owner: &str, token: Option<&str>) -> Result<Vec<GithubRepo>> {
    let client = Client::builder()
        .user_agent("ggen-daemon/0.1 catalog-discovery")
        .timeout(std::time::Duration::from_secs(30))
        .build()
        .map_err(|e| DaemonError::Http(e.to_string()))?;

    let mut all: Vec<GithubRepo> = Vec::new();
    let mut page = 1u32;

    loop {
        let url = format!(
            "https://api.github.com/users/{}/repos?per_page=100&page={}&type=owner",
            owner, page
        );

        let mut req = client.get(&url);
        if let Some(t) = token {
            req = req.bearer_auth(t);
        }

        let resp = req.send().map_err(|e| DaemonError::Http(e.to_string()))?;

        if !resp.status().is_success() {
            let status = resp.status();
            let body = resp.text().unwrap_or_default();
            return Err(DaemonError::Http(format!("GitHub API {}: {}", status, body)));
        }

        let repos: Vec<GithubRepoRaw> =
            resp.json().map_err(|e| DaemonError::Http(e.to_string()))?;

        if repos.is_empty() {
            break;
        }

        for r in repos {
            if r.fork || r.archived || r.private {
                continue;
            }
            all.push(GithubRepo {
                name: r.name,
                html_url: r.html_url,
                description: r.description.unwrap_or_default(),
                language: r.language,
            });
        }

        page += 1;
    }

    Ok(all)
}
