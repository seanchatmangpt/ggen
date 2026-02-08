//! Receipt NIF bindings
//!
//! This module provides Erlang NIF bindings for cryptographic receipt operations.

use crate::error::WorkflowResult;
use crate::patterns::WorkflowContext;
use crate::receipts::{ReceiptGenerator, WorkflowReceipt};

/// Generate a receipt from context data
#[rustler::nif]
fn generate_receipt(context_json: String) -> String {
    let result: WorkflowResult<String> = (|| {
        let generator = ReceiptGenerator::new();
        let context: WorkflowContext = serde_json::from_str(&context_json)?;
        let receipt = generator.generate_receipt(&context)?;
        Ok(serde_json::to_string(&receipt)?)
    })();

    match result {
        Ok(s) => s,
        Err(e) => format!("{{\"error\": \"{}\"}}", e),
    }
}

/// Create a test receipt
#[rustler::nif]
fn create_test_receipt() -> String {
    let generator = ReceiptGenerator::new();

    let mut context = WorkflowContext::default();
    context
        .input
        .insert("test".to_string(), serde_json::json!("value"));
    context
        .output
        .insert("result".to_string(), serde_json::json!({"success": true}));

    let receipt = generator.generate_receipt(&context).unwrap();
    serde_json::to_string(&receipt)
        .unwrap_or_else(|_| "{\"error\": \"generation failed\"}".to_string())
}

/// Verify a receipt
#[rustler::nif]
fn verify_receipt(receipt_json: String) -> String {
    let result: WorkflowResult<bool> = (|| {
        let generator = ReceiptGenerator::new();
        let receipt: WorkflowReceipt = serde_json::from_str(&receipt_json)?;
        let is_valid = generator.verify_receipt(&receipt)?;
        Ok(is_valid)
    })();

    match result {
        Ok(b) => b.to_string(),
        Err(e) => format!("{{\"error\": \"{}\"}}", e),
    }
}

/// Get receipt metadata
#[rustler::nif]
fn receipt_metadata(receipt_json: String) -> String {
    if let Ok(receipt) = serde_json::from_str::<WorkflowReceipt>(&receipt_json) {
        serde_json::to_string(&receipt.metadata)
            .unwrap_or_else(|_| "{\"error\": \"serialization failed\"}".to_string())
    } else {
        "{\"error\": \"invalid receipt\"}".to_string()
    }
}

/// Get receipt ID
#[rustler::nif]
fn receipt_id(receipt_json: String) -> String {
    if let Ok(receipt) = serde_json::from_str::<WorkflowReceipt>(&receipt_json) {
        receipt.receipt_id
    } else {
        "error: invalid receipt".to_string()
    }
}

/// Get receipt timestamp
#[rustler::nif]
fn receipt_timestamp(receipt_json: String) -> String {
    if let Ok(receipt) = serde_json::from_str::<WorkflowReceipt>(&receipt_json) {
        receipt.timestamp.to_rfc3339()
    } else {
        "error: invalid receipt".to_string()
    }
}

/// Wrapper functions for internal module use
pub fn receipt_generate(
    generator: &ReceiptGenerator, context: &WorkflowContext,
) -> WorkflowResult<String> {
    let receipt = generator.generate_receipt(context)?;
    Ok(serde_json::to_string(&receipt)?)
}

pub fn receipt_verify_internal(
    generator: &ReceiptGenerator, receipt: &WorkflowReceipt,
) -> WorkflowResult<bool> {
    generator.verify_receipt(receipt)
}

pub fn receipt_info(receipt: &WorkflowReceipt) -> WorkflowResult<String> {
    Ok(serde_json::to_string(&receipt.metadata)?)
}
