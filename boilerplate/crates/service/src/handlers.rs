use crate::ApiError;
use axum::{extract::Path, Json};
use domain::entities::{Item, ItemId};
use serde::Deserialize;

#[derive(Deserialize)]
pub struct CreateItemRequest {
    pub name: String,
    pub description: Option<String>,
}

pub async fn health() -> Json<serde_json::Value> {
    Json(serde_json::json!({ "status": "ok", "version": env!("CARGO_PKG_VERSION") }))
}

pub async fn create_item(Json(req): Json<CreateItemRequest>) -> Result<Json<Item>, ApiError> {
    let item = Item::new(req.name);
    Ok(Json(item))
}

pub async fn get_item(Path(_id): Path<ItemId>) -> Result<Json<Item>, ApiError> {
    Err(ApiError::from(bp_core::CoreError::not_found(
        "Item", "stub",
    )))
}
