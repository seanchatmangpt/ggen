// Supply Chain Optimization Engine - Rust
use serde::{Deserialize, Serialize};
use rust_decimal::Decimal;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InventoryItem {
    pub item_code: String,
    pub quantity_on_hand: i32,
    pub safety_stock: i32,
    pub reorder_point: i32,
    pub economic_order_quantity: i32,
    pub lead_time_days: i32,
}

impl InventoryItem {
    pub fn needs_reorder(&self) -> bool {
        self.quantity_on_hand <= self.reorder_point
    }

    pub fn recommended_order_quantity(&self) -> i32 {
        if self.needs_reorder() {
            self.economic_order_quantity
        } else {
            0
        }
    }
}

#[derive(Debug, Clone)]
pub struct SupplyChainOptimizer {
    items: Vec<InventoryItem>,
}

impl SupplyChainOptimizer {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn add_item(&mut self, item: InventoryItem) {
        self.items.push(item);
    }

    pub fn calculate_eoq(&self, annual_demand: i32, ordering_cost: Decimal, holding_cost: Decimal) -> i32 {
        let numerator = Decimal::from(2 * annual_demand) * ordering_cost;
        let eoq_squared = numerator / holding_cost;
        let eoq = eoq_squared.sqrt().unwrap_or(Decimal::ZERO);
        eoq.round_dp(0).to_i32().unwrap_or(0)
    }

    pub fn generate_purchase_recommendations(&self) -> Vec<PurchaseRecommendation> {
        self.items
            .iter()
            .filter(|item| item.needs_reorder())
            .map(|item| PurchaseRecommendation {
                item_code: item.item_code.clone(),
                recommended_quantity: item.recommended_order_quantity(),
                current_stock: item.quantity_on_hand,
                reorder_point: item.reorder_point,
            })
            .collect()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PurchaseRecommendation {
    pub item_code: String,
    pub recommended_quantity: i32,
    pub current_stock: i32,
    pub reorder_point: i32,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reorder_detection() {
        let item = InventoryItem {
            item_code: "ITEM001".to_string(),
            quantity_on_hand: 50,
            safety_stock: 20,
            reorder_point: 100,
            economic_order_quantity: 500,
            lead_time_days: 14,
        };

        assert!(item.needs_reorder());
        assert_eq!(item.recommended_order_quantity(), 500);
    }

    #[test]
    fn test_eoq_calculation() {
        let optimizer = SupplyChainOptimizer::new();
        let eoq = optimizer.calculate_eoq(
            12000,              // annual demand
            Decimal::from(50),  // ordering cost
            Decimal::from(2),   // holding cost per unit
        );

        // EOQ = sqrt((2 * 12000 * 50) / 2) = sqrt(600000) ≈ 775
        assert!((eoq - 775).abs() < 10);
    }

    #[test]
    fn test_purchase_recommendations() {
        let mut optimizer = SupplyChainOptimizer::new();

        optimizer.add_item(InventoryItem {
            item_code: "A".to_string(),
            quantity_on_hand: 50,
            safety_stock: 20,
            reorder_point: 100,
            economic_order_quantity: 500,
            lead_time_days: 14,
        });

        optimizer.add_item(InventoryItem {
            item_code: "B".to_string(),
            quantity_on_hand: 200,
            safety_stock: 50,
            reorder_point: 100,
            economic_order_quantity: 300,
            lead_time_days: 7,
        });

        let recommendations = optimizer.generate_purchase_recommendations();
        assert_eq!(recommendations.len(), 1);
        assert_eq!(recommendations[0].item_code, "A");
    }
}
