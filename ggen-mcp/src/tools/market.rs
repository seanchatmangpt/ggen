use crate::cli_helper::call_ggen_cli;
use crate::error::{
    get_bool_param, get_optional_string_param, get_optional_u64_param, get_string_param,
    success_response, Result,
};
///! Market tools - delegates to ggen CLI commands
use serde_json::Value;

/// List marketplace templates - delegates to `ggen market list`
pub async fn list(params: Value) -> Result<Value> {
    let category = get_optional_string_param(&params, "category");
    let tag = get_optional_string_param(&params, "tag");

    tracing::info!("Delegating to ggen market list");

    let mut args = vec!["market", "list"];
    let mut owned_args: Vec<String> = Vec::new();

    if let Some(ref c) = category {
        owned_args.push(c.clone());
    }
    if let Some(ref t) = tag {
        owned_args.push(t.clone());
    }

    let owned_refs: Vec<&str> = owned_args.iter().map(|s| s.as_str()).collect();
    let mut ref_idx = 0;

    if category.is_some() {
        args.push("--category");
        args.push(owned_refs[ref_idx]);
        ref_idx += 1;
    }
    if tag.is_some() {
        args.push("--tag");
        args.push(owned_refs[ref_idx]);
    }

    let result = call_ggen_cli(&args).await?;
    Ok(success_response(result))
}

/// Search marketplace templates - delegates to `ggen market search`
pub async fn search(params: Value) -> Result<Value> {
    let query = get_string_param(&params, "query")?;
    let category = get_optional_string_param(&params, "category");
    let limit = get_optional_u64_param(&params, "limit");

    tracing::info!("Delegating to ggen market search: {}", query);

    let mut args = vec!["market", "search", &query];
    let mut owned_args: Vec<String> = Vec::new();

    if let Some(ref c) = category {
        owned_args.push(c.clone());
    }
    if let Some(l) = limit {
        owned_args.push(l.to_string());
    }

    let owned_refs: Vec<&str> = owned_args.iter().map(|s| s.as_str()).collect();
    let mut ref_idx = 0;

    if category.is_some() {
        args.push("--category");
        args.push(owned_refs[ref_idx]);
        ref_idx += 1;
    }
    if limit.is_some() {
        args.push("--limit");
        args.push(owned_refs[ref_idx]);
    }

    let result = call_ggen_cli(&args).await?;
    Ok(success_response(result))
}

/// Install marketplace template - delegates to `ggen market add`
pub async fn install(params: Value) -> Result<Value> {
    let package = get_string_param(&params, "package")?;
    let version = get_optional_string_param(&params, "version");

    tracing::info!("Delegating to ggen market add: {}", package);

    let mut args = vec!["market", "add", &package];
    let mut owned_args: Vec<String> = Vec::new();

    if let Some(ref v) = version {
        owned_args.push(v.clone());
    }

    let owned_refs: Vec<&str> = owned_args.iter().map(|s| s.as_str()).collect();

    if version.is_some() {
        args.push("--version");
        args.push(owned_refs[0]);
    }

    let result = call_ggen_cli(&args).await?;
    Ok(success_response(result))
}

/// Get package info - delegates to `ggen market info`
pub async fn info(params: Value) -> Result<Value> {
    let package_id = get_string_param(&params, "package_id")?;

    tracing::info!("Delegating to ggen market info: {}", package_id);

    let result = call_ggen_cli(&["market", "info", &package_id]).await?;
    Ok(success_response(result))
}

/// Get package recommendations - delegates to `ggen market recommend`
pub async fn recommend(params: Value) -> Result<Value> {
    let based_on = get_optional_string_param(&params, "based_on");
    let category = get_optional_string_param(&params, "category");
    let limit = get_optional_u64_param(&params, "limit");

    tracing::info!("Delegating to ggen market recommend");

    let mut args = vec!["market", "recommend"];
    let mut owned_args: Vec<String> = Vec::new();

    if let Some(ref b) = based_on {
        owned_args.push(b.clone());
    }
    if let Some(ref c) = category {
        owned_args.push(c.clone());
    }
    if let Some(l) = limit {
        owned_args.push(l.to_string());
    }

    let owned_refs: Vec<&str> = owned_args.iter().map(|s| s.as_str()).collect();
    let mut ref_idx = 0;

    if based_on.is_some() {
        args.push("--based-on");
        args.push(owned_refs[ref_idx]);
        ref_idx += 1;
    }
    if category.is_some() {
        args.push("--category");
        args.push(owned_refs[ref_idx]);
        ref_idx += 1;
    }
    if limit.is_some() {
        args.push("--limit");
        args.push(owned_refs[ref_idx]);
    }

    let result = call_ggen_cli(&args).await?;
    Ok(success_response(result))
}

/// Search offline cache - delegates to `ggen market offline`
pub async fn offline_search(params: Value) -> Result<Value> {
    let query = get_string_param(&params, "query")?;
    let category = get_optional_string_param(&params, "category");
    let limit = get_optional_u64_param(&params, "limit");

    tracing::info!("Delegating to ggen market offline: {}", query);

    let mut args = vec!["market", "offline", &query];
    let mut owned_args: Vec<String> = Vec::new();

    if let Some(ref c) = category {
        owned_args.push(c.clone());
    }
    if let Some(l) = limit {
        owned_args.push(l.to_string());
    }

    let owned_refs: Vec<&str> = owned_args.iter().map(|s| s.as_str()).collect();
    let mut ref_idx = 0;

    if category.is_some() {
        args.push("--category");
        args.push(owned_refs[ref_idx]);
        ref_idx += 1;
    }
    if limit.is_some() {
        args.push("--limit");
        args.push(owned_refs[ref_idx]);
    }

    let result = call_ggen_cli(&args).await?;
    Ok(success_response(result))
}

/// Get cache status - delegates to `ggen market cache`
pub async fn cache_status(_params: Value) -> Result<Value> {
    tracing::info!("Delegating to ggen market cache");

    let result = call_ggen_cli(&["market", "cache"]).await?;
    Ok(success_response(result))
}

/// Sync marketplace cache - delegates to `ggen market sync`
pub async fn sync(params: Value) -> Result<Value> {
    let category = get_optional_string_param(&params, "category");
    let force = get_bool_param(&params, "force", false);

    tracing::info!("Delegating to ggen market sync");

    let mut args = vec!["market", "sync"];
    let mut owned_args: Vec<String> = Vec::new();

    if let Some(ref c) = category {
        owned_args.push(c.clone());
    }

    let owned_refs: Vec<&str> = owned_args.iter().map(|s| s.as_str()).collect();

    if category.is_some() {
        args.push("--category");
        args.push(owned_refs[0]);
    }
    if force {
        args.push("--force");
    }

    let result = call_ggen_cli(&args).await?;
    Ok(success_response(result))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[tokio::test]
    async fn test_search_requires_query() {
        let params = json!({});
        let result = search(params).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_install_requires_package() {
        let params = json!({});
        let result = install(params).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_info_requires_package_id() {
        let params = json!({});
        let result = info(params).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_offline_search_requires_query() {
        let params = json!({});
        let result = offline_search(params).await;
        assert!(result.is_err());
    }
}
