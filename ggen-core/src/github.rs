use anyhow::{Context, Result};
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
            anyhow::bail!(
                "Invalid repository format. Expected 'owner/repo', got '{}'",
                repo_str
            );
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
        let base_url =
            Url::parse("https://api.github.com").context("Failed to parse GitHub API base URL")?;

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
                    .context("Invalid GitHub token")?,
            );
            client_builder = client_builder.default_headers(headers);
        }

        let client = client_builder
            .build()
            .context("Failed to create HTTP client")?;

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
            .context("Failed to construct Pages API URL")?;

        let response = self
            .client
            .get(url.clone())
            .send()
            .await
            .context(format!("Failed to fetch Pages config from {}", url))?;

        if response.status() == 404 {
            anyhow::bail!(
                "GitHub Pages not configured for repository {}",
                repo.as_str()
            );
        }

        if !response.status().is_success() {
            anyhow::bail!(
                "GitHub API returned status: {} for URL: {}",
                response.status(),
                url
            );
        }

        let config: PagesConfig = response
            .json()
            .await
            .context("Failed to parse Pages configuration")?;

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
            .context("Failed to construct workflow runs API URL")?;

        let response = self
            .client
            .get(url.clone())
            .send()
            .await
            .context(format!("Failed to fetch workflow runs from {}", url))?;

        if !response.status().is_success() {
            anyhow::bail!(
                "GitHub API returned status: {} for URL: {}",
                response.status(),
                url
            );
        }

        let runs: WorkflowRunsResponse = response
            .json()
            .await
            .context("Failed to parse workflow runs")?;

        Ok(runs)
    }

    /// Get a specific workflow run by ID
    pub async fn get_workflow_run(&self, repo: &RepoInfo, run_id: u64) -> Result<WorkflowRun> {
        let url = self
            .base_url
            .join(&format!("repos/{}/actions/runs/{}", repo.as_str(), run_id))
            .context("Failed to construct workflow run API URL")?;

        let response = self
            .client
            .get(url.clone())
            .send()
            .await
            .context(format!("Failed to fetch workflow run from {}", url))?;

        if !response.status().is_success() {
            anyhow::bail!(
                "GitHub API returned status: {} for URL: {}",
                response.status(),
                url
            );
        }

        let run: WorkflowRun = response
            .json()
            .await
            .context("Failed to parse workflow run")?;

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
            anyhow::bail!("GitHub token required to trigger workflows. Set GITHUB_TOKEN or GH_TOKEN environment variable.");
        }

        let url = self
            .base_url
            .join(&format!(
                "repos/{}/actions/workflows/{}/dispatches",
                repo.as_str(),
                workflow_file
            ))
            .context("Failed to construct workflow dispatch API URL")?;

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
            .context(format!("Failed to trigger workflow at {}", url))?;

        if !response.status().is_success() {
            anyhow::bail!("Failed to trigger workflow. Status: {}", response.status());
        }

        Ok(())
    }

    /// Check if a GitHub Pages site is accessible
    pub async fn check_site_status(&self, pages_url: &str) -> Result<u16> {
        let response = self
            .client
            .get(pages_url)
            .send()
            .await
            .context(format!("Failed to check site status at {}", pages_url))?;

        Ok(response.status().as_u16())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_repo_info_parse() {
        let repo = RepoInfo::parse("seanchatmangpt/ggen").unwrap();
        assert_eq!(repo.owner, "seanchatmangpt");
        assert_eq!(repo.name, "ggen");
        assert_eq!(repo.as_str(), "seanchatmangpt/ggen");
    }

    #[test]
    fn test_repo_info_parse_invalid() {
        assert!(RepoInfo::parse("invalid").is_err());
        assert!(RepoInfo::parse("too/many/parts").is_err());
    }

    #[tokio::test]
    #[ignore] // Requires network access
    async fn test_github_client_creation() {
        let repo = RepoInfo::parse("seanchatmangpt/ggen").unwrap();
        let client = GitHubClient::new(repo);
        assert!(client.is_ok());
    }
}
