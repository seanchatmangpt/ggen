// Traditional Approach: Manual validation (separate from types)
// File: validation.rs (89 LOC)

use super::domain::*;

#[derive(Debug, Clone, PartialEq)]
pub enum ValidationError {
    EmptyName,
    InvalidName(String),
    InvalidDescription(String),
    NegativePrice,
    InvalidCurrency,
    NegativeQuantity,
    InvalidSKU(String),
    InvalidCategoryPath,
    CircularCategoryReference,
    InvalidVariantAttributes,
    PriceModifierTooLarge,
}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValidationError::EmptyName => write!(f, "Name cannot be empty"),
            ValidationError::InvalidName(msg) => write!(f, "Invalid name: {}", msg),
            ValidationError::InvalidDescription(msg) => write!(f, "Invalid description: {}", msg),
            ValidationError::NegativePrice => write!(f, "Price cannot be negative"),
            ValidationError::InvalidCurrency => write!(f, "Invalid currency"),
            ValidationError::NegativeQuantity => write!(f, "Quantity cannot be negative"),
            ValidationError::InvalidSKU(msg) => write!(f, "Invalid SKU: {}", msg),
            ValidationError::InvalidCategoryPath => write!(f, "Invalid category path"),
            ValidationError::CircularCategoryReference => write!(f, "Circular category reference detected"),
            ValidationError::InvalidVariantAttributes => write!(f, "Invalid variant attributes"),
            ValidationError::PriceModifierTooLarge => write!(f, "Price modifier too large"),
        }
    }
}

impl std::error::Error for ValidationError {}

/// Validate a product
/// PROBLEM: This is separate from Product type - easy to forget to call
pub fn validate_product(product: &Product) -> Result<(), ValidationError> {
    // Validate name
    if product.name.is_empty() {
        return Err(ValidationError::EmptyName);
    }
    if product.name.len() > 100 {
        return Err(ValidationError::InvalidName("Name too long (max 100 chars)".to_string()));
    }

    // Validate description
    if product.description.len() > 1000 {
        return Err(ValidationError::InvalidDescription("Description too long (max 1000 chars)".to_string()));
    }

    // Validate price
    if product.base_price.amount < 0.0 {
        return Err(ValidationError::NegativePrice);
    }

    // Validate SKU format (manual regex)
    if product.sku.len() < 5 || !product.sku.chars().all(|c| c.is_alphanumeric() || c == '-') {
        return Err(ValidationError::InvalidSKU("SKU must be alphanumeric with dashes".to_string()));
    }

    // Validate inventory
    validate_inventory(&product.inventory)?;

    // Validate variants
    for variant in &product.variants {
        validate_variant(variant)?;
    }

    Ok(())
}

/// Validate inventory
pub fn validate_inventory(inventory: &Inventory) -> Result<(), ValidationError> {
    if inventory.quantity < 0 {
        return Err(ValidationError::NegativeQuantity);
    }
    if inventory.reserved < 0 {
        return Err(ValidationError::NegativeQuantity);
    }
    if inventory.reserved > inventory.quantity {
        return Err(ValidationError::InvalidName("Reserved cannot exceed quantity".to_string()));
    }
    if inventory.reorder_point < 0 {
        return Err(ValidationError::NegativeQuantity);
    }
    if inventory.reorder_quantity <= 0 {
        return Err(ValidationError::NegativeQuantity);
    }

    Ok(())
}

/// Validate product variant
pub fn validate_variant(variant: &ProductVariant) -> Result<(), ValidationError> {
    if variant.name.is_empty() {
        return Err(ValidationError::EmptyName);
    }
    if variant.attributes.is_empty() {
        return Err(ValidationError::InvalidVariantAttributes);
    }
    // Price modifier should not be larger than 1000% of base price
    if variant.price_modifier.abs() > 10000.0 {
        return Err(ValidationError::PriceModifierTooLarge);
    }

    Ok(())
}

/// Validate category
pub fn validate_category(category: &Category, all_categories: &std::collections::HashMap<String, Category>) -> Result<(), ValidationError> {
    if category.name.is_empty() {
        return Err(ValidationError::EmptyName);
    }

    // Check for circular references
    if let Some(parent_id) = &category.parent_id {
        let mut visited = std::collections::HashSet::new();
        let mut current_id = parent_id.clone();

        loop {
            if visited.contains(&current_id) {
                return Err(ValidationError::CircularCategoryReference);
            }
            visited.insert(current_id.clone());

            if let Some(parent) = all_categories.get(&current_id) {
                if let Some(next_parent_id) = &parent.parent_id {
                    current_id = next_parent_id.clone();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    Ok(())
}

// PROBLEM: All validation is manual
// PROBLEM: Easy to forget to call these functions
// PROBLEM: Can construct invalid Product/Category/Inventory
// PROBLEM: Validation rules duplicated between code and documentation
// PROBLEM: No compile-time guarantees
