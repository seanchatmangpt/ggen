//! GitHub API client for Pages and workflow operations
//!
//! This module provides a client for interacting with the GitHub API, specifically
//! for managing GitHub Pages configurations and GitHub Actions workflow runs.
//!
//! ## Features
//!
//! - **GitHub Pages**: Query and manage Pages configuration
//! - **Workflow Runs**: List, query, and trigger GitHub Actions workflows
//! - **Authentication**: Support for GitHub tokens via environment variables
//! - **Error Handling**: Comprehensive error messages for API failures
//!
//! ## Authentication
//!
//! The client automatically detects GitHub tokens from environment variables:
//! - `GITHUB_TOKEN` (primary)
//! - `GH_TOKEN` (fallback)
//!
//! ## Examples
//!
//! ### Creating a Client
//!
//! ```rust,no_run
//! use ggen_core::github::{GitHubClient, RepoInfo};
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let repo = RepoInfo::parse("owner/repo")?;
//! let client = GitHubClient::new(repo)?;
//!
//! if client.is_authenticated() {
//!     println!("Authenticated with GitHub");
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ### Getting Pages Configuration
//!
//! ```rust,no_run
//! use ggen_core::github::{GitHubClient, RepoInfo};
//!
//! # async fn example() -> ggen_utils::error::Result<()> {
//! let repo = RepoInfo::parse("owner/repo")?;
//! let client = GitHubClient::new(repo.clone())?;
//!
//! let pages_config = client.get_pages_config(&repo).await?;
//! println!("Pages URL: {:?}", pages_config.url);
//! # Ok(())
//! # }
//! ```
//!
//! ### Listing Workflow Runs
//!
//! ```rust,no_run
//! use ggen_core::github::{GitHubClient, RepoInfo};
//!
//! # async fn example() -> ggen_utils::error::Result<()> {
//! let repo = RepoInfo::parse("owner/repo")?;
//! let client = GitHubClient::new(repo.clone())?;
//!
//! let runs = client.get_workflow_runs(&repo, "deploy.yml", 10).await?;
//! println!("Found {} workflow runs", runs.total_count);
//! # Ok(())
//! # }
//! ```

//! GitHub API client for Pages and workflow operations
//!
//! This module provides a client for interacting with the GitHub API, specifically
//! for managing GitHub Pages configurations and GitHub Actions workflow runs.
//! It supports authentication via GitHub tokens and provides convenient methods
//! for common operations.
//!
//! ## Features
//!
//! - **GitHub Pages**: Get Pages configuration and check site status
//! - **Workflow Management**: List workflow runs, get run details, trigger workflows
//! - **Authentication**: Automatic token detection from environment variables
//! - **Error Handling**: Comprehensive error handling with context
//! - **Repository Parsing**: Parse "owner/repo" format into structured types
//!
//! ## Authentication
//!
//! The client automatically detects GitHub tokens from environment variables:
//! - `GITHUB_TOKEN` (preferred)
//! - `GH_TOKEN` (fallback)
//!
//! Operations that require authentication will fail with a clear error message
//! if no token is available.
//!
//! ## Examples
//!
//! ### Getting Pages Configuration
//!
//! ```rust,no_run
//! use ggen_core::github::{GitHubClient, RepoInfo};
//!
//! # async fn example() -> ggen_utils::error::Result<()> {
//! let repo = RepoInfo::parse("seanchatmangpt/ggen")?;
//! let client = GitHubClient::new(repo.clone())?;
//!
//! let pages_config = client.get_pages_config(&repo).await?;
//! println!("Pages URL: {:?}", pages_config.url);
//! # Ok(())
//! # }
//! ```
//!
//! ### Listing Workflow Runs
//!
//! ```rust,no_run
//! use ggen_core::github::{GitHubClient, RepoInfo};
//!
//! # async fn example() -> ggen_utils::error::Result<()> {
//! let repo = RepoInfo::parse("seanchatmangpt/ggen")?;
//! let client = GitHubClient::new(repo.clone())?;
//!
//! let runs = client.get_workflow_runs(&repo, "ci.yml", 10).await?;
//! for run in runs.workflow_runs {
//!     println!("Run #{}: {} - {}", run.run_number, run.name, run.status);
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ### Triggering a Workflow
//!
//! ```rust,no_run
//! use ggen_core::github::{GitHubClient, RepoInfo};
//!
//! # async fn example() -> ggen_utils::error::Result<()> {
//! let repo = RepoInfo::parse("seanchatmangpt/ggen")?;
//! let client = GitHubClient::new(repo.clone())?;
//!
//! // Requires GITHUB_TOKEN or GH_TOKEN environment variable
//! client.trigger_workflow(&repo, "deploy.yml", "main").await?;
//! # Ok(())
//! # }
//! ```

//! GitHub API client for Pages and Actions integration
//!
//! This module provides a client for interacting with the GitHub API to manage
//! GitHub Pages deployments and GitHub Actions workflow runs. It supports
//! authentication via environment variables and provides typed interfaces for
//! common GitHub operations.
//!
//! ## Features
//!
//! - **GitHub Pages**: Query Pages configuration and deployment status
//! - **Workflow Management**: List and trigger GitHub Actions workflows
//! - **Authentication**: Support for GITHUB_TOKEN and GH_TOKEN environment variables
//! - **Repository Info**: Parse and work with "owner/repo" format
//! - **Site Status**: Check accessibility of GitHub Pages sites
//!
//! ## Authentication
//!
//! The client automatically detects GitHub tokens from environment variables:
//! - `GITHUB_TOKEN` (primary)
//! - `GH_TOKEN` (fallback)
//!
//! Some operations (like triggering workflows) require authentication.
//!
//! ## Examples
//!
//! ### Creating a Client
//!
//! ```rust,no_run
//! use ggen_core::github::{GitHubClient, RepoInfo};
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let repo = RepoInfo::parse("seanchatmangpt/ggen")?;
//! let client = GitHubClient::new(repo)?;
//!
//! if client.is_authenticated() {
//!     println!("Authenticated with GitHub");
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ### Querying GitHub Pages
//!
//! ```rust,no_run
//! use ggen_core::github::{GitHubClient, RepoInfo};
//!
//! # async fn example() -> ggen_utils::error::Result<()> {
//! let repo = RepoInfo::parse("seanchatmangpt/ggen")?;
//! let client = GitHubClient::new(repo.clone())?;
//!
//! let pages_config = client.get_pages_config(&repo).await?;
//! if let Some(url) = pages_config.url {
//!     println!("Pages URL: {}", url);
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ### Working with Workflows
//!
//! ```rust,no_run
//! use ggen_core::github::{GitHubClient, RepoInfo};
//!
//! # async fn example() -> ggen_utils::error::Result<()> {
//! let repo = RepoInfo::parse("seanchatmangpt/ggen")?;
//! let client = GitHubClient::new(repo.clone())?;
//!
//! // List workflow runs
//! let runs = client.get_workflow_runs(&repo, "deploy.yml", 10).await?;
//! for run in runs.workflow_runs {
//!     println!("Run #{}: {} ({})", run.run_number, run.name, run.status);
//! }
//!
//! // Trigger a workflow (requires authentication)
//! client.trigger_workflow(&repo, "deploy.yml", "main").await?;
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::{Error, Result};
use reqwest;
use serde::{Deserialize, Serialize};
use std::env;
use url::Url;

/// GitHub API client for Pages and workflow operations
#[derive(Debug, Clone)]
pub struct GitHubClient {
    base_url: Url,
    client: reqwest::Client,
    token: Option<String>,
}

/// GitHub Pages configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PagesConfig {
    pub url: Option<String>,
    pub status: Option<String>,
    #[serde(default)]
    pub cname: Option<String>,
    pub source: Option<PagesSource>,
    pub https_enforced: Option<bool>,
    pub html_url: Option<String>,
}

/// GitHub Pages source configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PagesSource {
    pub branch: Option<String>,
    pub path: Option<String>,
}

/// GitHub Actions workflow run
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowRun {
    pub id: u64,
    pub name: String,
    pub head_branch: String,
    pub status: String,
    pub conclusion: Option<String>,
    pub created_at: String,
    pub updated_at: String,
    pub html_url: String,
    pub run_number: u32,
    pub event: String,
}

/// Workflow runs response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowRunsResponse {
    pub total_count: u32,
    pub workflow_runs: Vec<WorkflowRun>,
}

/// Pages deployment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PagesDeployment {
    pub id: Option<u64>,
    pub status_url: Option<String>,
    pub environment: Option<String>,
    pub created_at: Option<String>,
    pub updated_at: Option<String>,
}

/// Repository information
#[derive(Debug, Clone)]
pub struct RepoInfo {
    pub owner: String,
    pub name: String,
}

impl RepoInfo {
    /// Parse from "owner/repo" format
    pub fn parse(repo_str: &str) -> Result<Self> {
        let parts: Vec<&str> = repo_str.split('/').collect();
        if parts.len() != 2 {
            return Err(Error::new(&format!(
                "Invalid repository format. Expected 'owner/repo', got '{}'",
                repo_str
            )));
        }
        Ok(Self {
            owner: parts[0].to_string(),
            name: parts[1].to_string(),
        })
    }

    /// Get repository string
    pub fn as_str(&self) -> String {
        format!("{}/{}", self.owner, self.name)
    }
}

impl GitHubClient {
    /// Create a new GitHub API client
    pub fn new(_repo: RepoInfo) -> Result<Self> {
        let base_url = Url::parse("https://api.github.com").map_err(|e| {
            Error::with_context("Failed to parse GitHub API base URL", &e.to_string())
        })?;

        // Check for GitHub token in environment
        let token = env::var("GITHUB_TOKEN")
            .ok()
            .or_else(|| env::var("GH_TOKEN").ok());

        let mut client_builder = reqwest::Client::builder()
            .timeout(std::time::Duration::from_secs(30))
            .user_agent("ggen-cli");

        // Add authorization header if token is available
        if let Some(ref token) = token {
            let mut headers = reqwest::header::HeaderMap::new();
            headers.insert(
                reqwest::header::AUTHORIZATION,
                reqwest::header::HeaderValue::from_str(&format!("Bearer {}", token))
                    .map_err(|e| Error::with_context("Invalid GitHub token", &e.to_string()))?,
            );
            client_builder = client_builder.default_headers(headers);
        }

        let client = client_builder
            .build()
            .map_err(|e| Error::with_context("Failed to create HTTP client", &e.to_string()))?;

        Ok(Self {
            base_url,
            client,
            token,
        })
    }

    /// Check if client is authenticated
    pub fn is_authenticated(&self) -> bool {
        self.token.is_some()
    }

    /// Get Pages configuration for a repository
    pub async fn get_pages_config(&self, repo: &RepoInfo) -> Result<PagesConfig> {
        let url = self
            .base_url
            .join(&format!("repos/{}/pages", repo.as_str()))
            .map_err(|e| {
                Error::with_context("Failed to construct Pages API URL", &e.to_string())
            })?;

        let response = self.client.get(url.clone()).send().await.map_err(|e| {
            Error::with_context(
                &format!("Failed to fetch Pages config from {}", url),
                &e.to_string(),
            )
        })?;

        if response.status() == 404 {
            return Err(Error::new(&format!(
                "GitHub Pages not configured for repository {}",
                repo.as_str()
            )));
        }

        if !response.status().is_success() {
            return Err(Error::new(&format!(
                "GitHub API returned status: {} for URL: {}",
                response.status(),
                url
            )));
        }

        let config: PagesConfig = response.json().await.map_err(|e| {
            Error::with_context("Failed to parse Pages configuration", &e.to_string())
        })?;

        Ok(config)
    }

    /// Get workflow runs for a specific workflow file
    pub async fn get_workflow_runs(
        &self, repo: &RepoInfo, workflow_file: &str, per_page: u32,
    ) -> Result<WorkflowRunsResponse> {
        let url = self
            .base_url
            .join(&format!(
                "repos/{}/actions/workflows/{}/runs?per_page={}",
                repo.as_str(),
                workflow_file,
                per_page
            ))
            .map_err(|e| {
                Error::with_context("Failed to construct workflow runs API URL", &e.to_string())
            })?;

        let response = self.client.get(url.clone()).send().await.map_err(|e| {
            Error::with_context(
                &format!("Failed to fetch workflow runs from {}", url),
                &e.to_string(),
            )
        })?;

        if !response.status().is_success() {
            return Err(Error::new(&format!(
                "GitHub API returned status: {} for URL: {}",
                response.status(),
                url
            )));
        }

        let runs: WorkflowRunsResponse = response
            .json()
            .await
            .map_err(|e| Error::with_context("Failed to parse workflow runs", &e.to_string()))?;

        Ok(runs)
    }

    /// Get a specific workflow run by ID
    pub async fn get_workflow_run(&self, repo: &RepoInfo, run_id: u64) -> Result<WorkflowRun> {
        let url = self
            .base_url
            .join(&format!("repos/{}/actions/runs/{}", repo.as_str(), run_id))
            .map_err(|e| {
                Error::with_context("Failed to construct workflow run API URL", &e.to_string())
            })?;

        let response = self.client.get(url.clone()).send().await.map_err(|e| {
            Error::with_context(
                &format!("Failed to fetch workflow run from {}", url),
                &e.to_string(),
            )
        })?;

        if !response.status().is_success() {
            return Err(Error::new(&format!(
                "GitHub API returned status: {} for URL: {}",
                response.status(),
                url
            )));
        }

        let run: WorkflowRun = response
            .json()
            .await
            .map_err(|e| Error::with_context("Failed to parse workflow run", &e.to_string()))?;

        Ok(run)
    }

    /// Trigger a workflow dispatch event
    pub async fn trigger_workflow(
        &self,
        repo: &RepoInfo,
        workflow_file: &str,
        ref_name: &str, // branch, tag, or SHA
    ) -> Result<()> {
        if !self.is_authenticated() {
            return Err(Error::new("GitHub token required to trigger workflows. Set GITHUB_TOKEN or GH_TOKEN environment variable."));
        }

        let url = self
            .base_url
            .join(&format!(
                "repos/{}/actions/workflows/{}/dispatches",
                repo.as_str(),
                workflow_file
            ))
            .map_err(|e| {
                Error::with_context(
                    "Failed to construct workflow dispatch API URL",
                    &e.to_string(),
                )
            })?;

        #[derive(Serialize)]
        struct DispatchRequest<'a> {
            #[serde(rename = "ref")]
            ref_name: &'a str,
        }

        let body = DispatchRequest { ref_name };

        let response = self
            .client
            .post(url.clone())
            .json(&body)
            .send()
            .await
            .map_err(|e| {
                Error::with_context(
                    &format!("Failed to trigger workflow at {}", url),
                    &e.to_string(),
                )
            })?;

        if !response.status().is_success() {
            return Err(Error::new(&format!(
                "Failed to trigger workflow. Status: {}",
                response.status()
            )));
        }

        Ok(())
    }

    /// Check if a GitHub Pages site is accessible
    pub async fn check_site_status(&self, pages_url: &str) -> Result<u16> {
        let response = self.client.get(pages_url).send().await.map_err(|e| {
            Error::with_context(
                &format!("Failed to check site status at {}", pages_url),
                &e.to_string(),
            )
        })?;

        Ok(response.status().as_u16())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::{async_test, test};

    test!(test_repo_info_parse, {
        let repo = RepoInfo::parse("seanchatmangpt/ggen").unwrap();
        assert_eq!(repo.owner, "seanchatmangpt");
        assert_eq!(repo.name, "ggen");
        assert_eq!(repo.as_str(), "seanchatmangpt/ggen");
    });

    test!(test_repo_info_parse_invalid, {
        assert!(RepoInfo::parse("invalid").is_err());
        assert!(RepoInfo::parse("too/many/parts").is_err());
    });

    #[ignore] // Requires network access
    async_test!(test_github_client_creation, {
        let repo = RepoInfo::parse("seanchatmangpt/ggen").unwrap();
        let client = GitHubClient::new(repo);
        assert!(client.is_ok());
    });
}
