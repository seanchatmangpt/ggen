pub struct GitHubStewardshipMembrane;

impl GitHubStewardshipMembrane {
    pub fn map_obligation_to_issue(obligation_id: &str) -> String {
        format!("GITHUB_ISSUE_{}", obligation_id)
    }
}
