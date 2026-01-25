// Traditional Approach: Hand-written domain model
// File: domain.rs (287 LOC)

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Product in the catalog
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Product {
    pub id: String,
    pub name: String,
    pub description: String,
    pub sku: String,
    pub category_id: String,
    pub base_price: Price,
    pub variants: Vec<ProductVariant>,
    pub inventory: Inventory,
    pub created_at: i64,
    pub updated_at: i64,
}

/// Product variant (e.g., different sizes/colors)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ProductVariant {
    pub id: String,
    pub name: String,
    pub attributes: HashMap<String, String>, // e.g., {"size": "L", "color": "blue"}
    pub price_modifier: f64,
    pub sku_suffix: String,
}

/// Product category with hierarchical support
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Category {
    pub id: String,
    pub name: String,
    pub parent_id: Option<String>,
    pub path: Vec<String>, // Ancestry path
    pub attributes: Vec<String>, // Available attributes for products in this category
}

/// Price with currency support
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Price {
    pub amount: f64,
    pub currency: Currency,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Currency {
    USD,
    EUR,
    GBP,
    JPY,
}

/// Inventory tracking
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Inventory {
    pub quantity: i32,
    pub reserved: i32,
    pub warehouse_location: String,
    pub reorder_point: i32,
    pub reorder_quantity: i32,
}

impl Product {
    /// Create a new product (no validation yet - that's separate)
    pub fn new(
        id: String,
        name: String,
        description: String,
        category_id: String,
        base_price: Price,
    ) -> Self {
        let now = chrono::Utc::now().timestamp();
        let sku = Self::generate_sku(&category_id, &name);

        Self {
            id,
            name,
            description,
            sku,
            category_id,
            base_price,
            variants: Vec::new(),
            inventory: Inventory::default(),
            created_at: now,
            updated_at: now,
        }
    }

    /// Generate SKU from category and product name
    /// Manual implementation - easy to have bugs
    pub fn generate_sku(category_id: &str, name: &str) -> String {
        let category_prefix = category_id
            .chars()
            .filter(|c| c.is_alphanumeric())
            .take(3)
            .collect::<String>()
            .to_uppercase();

        let product_suffix = name
            .chars()
            .filter(|c| c.is_alphanumeric())
            .take(5)
            .collect::<String>()
            .to_uppercase();

        format!("{}-{}-{}", category_prefix, product_suffix, chrono::Utc::now().timestamp())
    }

    /// Add a variant
    /// No validation - can add invalid variants
    pub fn add_variant(&mut self, variant: ProductVariant) {
        self.variants.push(variant);
        self.updated_at = chrono::Utc::now().timestamp();
    }

    /// Calculate price for a specific variant
    pub fn variant_price(&self, variant_id: &str) -> Option<Price> {
        self.variants.iter()
            .find(|v| v.id == variant_id)
            .map(|v| Price {
                amount: self.base_price.amount + v.price_modifier,
                currency: self.base_price.currency.clone(),
            })
    }

    /// Get available quantity (total - reserved)
    pub fn available_quantity(&self) -> i32 {
        self.inventory.quantity - self.inventory.reserved
    }

    /// Check if reorder is needed
    pub fn needs_reorder(&self) -> bool {
        self.available_quantity() < self.inventory.reorder_point
    }
}

impl Category {
    /// Create a new category
    pub fn new(id: String, name: String, parent_id: Option<String>) -> Self {
        Self {
            id,
            name,
            parent_id,
            path: Vec::new(), // Must be calculated separately
            attributes: Vec::new(),
        }
    }

    /// Build full path from root to this category
    /// Manual implementation - must maintain consistency
    pub fn build_path(&mut self, categories: &HashMap<String, Category>) {
        let mut path = Vec::new();
        let mut current_id = self.id.clone();

        // Traverse up to root
        loop {
            let category = categories.get(&current_id).unwrap();
            path.insert(0, category.name.clone());

            if let Some(parent_id) = &category.parent_id {
                current_id = parent_id.clone();
            } else {
                break;
            }
        }

        self.path = path;
    }

    /// Check if this is a root category
    pub fn is_root(&self) -> bool {
        self.parent_id.is_none()
    }

    /// Add an attribute that products in this category can have
    pub fn add_attribute(&mut self, attribute: String) {
        if !self.attributes.contains(&attribute) {
            self.attributes.push(attribute);
        }
    }
}

impl Price {
    /// Create a new price
    pub fn new(amount: f64, currency: Currency) -> Self {
        Self { amount, currency }
    }

    /// Convert to another currency (simplified - would use real exchange rates)
    pub fn convert_to(&self, target: Currency) -> Option<Self> {
        // Simplified exchange rates
        let usd_rate = match self.currency {
            Currency::USD => 1.0,
            Currency::EUR => 1.1,
            Currency::GBP => 1.25,
            Currency::JPY => 0.0067,
        };

        let target_rate = match target {
            Currency::USD => 1.0,
            Currency::EUR => 1.1,
            Currency::GBP => 1.25,
            Currency::JPY => 0.0067,
        };

        Some(Self {
            amount: self.amount * usd_rate / target_rate,
            currency: target,
        })
    }
}

impl Inventory {
    /// Reserve quantity for an order
    /// No validation - can over-reserve
    pub fn reserve(&mut self, quantity: i32) -> bool {
        if self.quantity - self.reserved >= quantity {
            self.reserved += quantity;
            true
        } else {
            false
        }
    }

    /// Release reserved quantity
    pub fn release(&mut self, quantity: i32) {
        self.reserved = (self.reserved - quantity).max(0);
    }

    /// Fulfill order (reduce quantity and reserved)
    pub fn fulfill(&mut self, quantity: i32) -> bool {
        if self.reserved >= quantity && self.quantity >= quantity {
            self.quantity -= quantity;
            self.reserved -= quantity;
            true
        } else {
            false
        }
    }

    /// Restock inventory
    pub fn restock(&mut self, quantity: i32) {
        self.quantity += quantity;
    }
}

impl Default for Inventory {
    fn default() -> Self {
        Self {
            quantity: 0,
            reserved: 0,
            warehouse_location: "DEFAULT".to_string(),
            reorder_point: 10,
            reorder_quantity: 50,
        }
    }
}

// NO VALIDATION IN THIS FILE
// Validation is separate (validation.rs)
// Easy to forget to call validation
// Easy to construct invalid states
