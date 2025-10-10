///! Market tools - delegates to ggen CLI commands

use serde_json::{json, Value};
use crate::cli_helper::call_ggen_cli;
use crate::error::{Result, get_string_param, get_optional_string_param, get_optional_u64_param, get_bool_param, success_response};

/// List marketplace templates - delegates to `ggen market list`
pub async fn list(params: Value) -> Result<Value> {
    let category = get_optional_string_param(&params, "category");
    let tag = get_optional_string_param(&params, "tag");

    tracing::info!("Delegating to ggen market list");

    let mut args = vec!["market", "list"];
    let category_str;
    let tag_str;

    if let Some(c) = category {
        args.push("--category");
        category_str = c;
        args.push(&category_str);
    }
    if let Some(t) = tag {
        args.push("--tag");
        tag_str = t;
        args.push(&tag_str);
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
    let category_str;
    let limit_str;

    if let Some(c) = category {
        args.push("--category");
        category_str = c;
        args.push(&category_str);
    }
    if let Some(l) = limit {
        args.push("--limit");
        limit_str = l.to_string();
        args.push(&limit_str);
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
    let version_str;

    if let Some(v) = version {
        args.push("--version");
        version_str = v;
        args.push(&version_str);
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
    let based_on_str;
    let category_str;
    let limit_str;

    if let Some(b) = based_on {
        args.push("--based-on");
        based_on_str = b;
        args.push(&based_on_str);
    }
    if let Some(c) = category {
        args.push("--category");
        category_str = c;
        args.push(&category_str);
    }
    if let Some(l) = limit {
        args.push("--limit");
        limit_str = l.to_string();
        args.push(&limit_str);
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
    let category_str;
    let limit_str;

    if let Some(c) = category {
        args.push("--category");
        category_str = c;
        args.push(&category_str);
    }
    if let Some(l) = limit {
        args.push("--limit");
        limit_str = l.to_string();
        args.push(&limit_str);
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
    let category_str;

    if let Some(c) = category {
        args.push("--category");
        category_str = c;
        args.push(&category_str);
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
