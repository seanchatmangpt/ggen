// Inventory Management - Stock Tracking Engine (Rust)
// High-performance inventory tracking with FIFO/LIFO support

use std::collections::{HashMap, VecDeque};
use chrono::{DateTime, Utc};

#[derive(Debug, Clone, PartialEq)]
pub enum ValuationMethod {
    FIFO,
    LIFO,
    WeightedAverage,
}

#[derive(Debug, Clone)]
pub struct StockMovement {
    pub movement_id: String,
    pub product_id: String,
    pub quantity: i32,  // Positive=increase, negative=decrease
    pub unit_cost: f64,
    pub location_id: String,
    pub timestamp: DateTime<Utc>,
    pub movement_type: MovementType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MovementType {
    Receive,
    Transfer,
    Sell,
    Adjust,
    Return,
    Scrap,
}

#[derive(Debug)]
pub struct StockLevel {
    pub product_id: String,
    pub location_id: String,
    pub quantity_on_hand: i32,
    pub quantity_reserved: i32,
    pub valuation_method: ValuationMethod,
    fifo_layers: VecDeque<FIFOLayer>,
}

#[derive(Debug, Clone)]
struct FIFOLayer {
    quantity: i32,
    unit_cost: f64,
    received_at: DateTime<Utc>,
}

impl StockLevel {
    pub fn new(product_id: String, location_id: String, valuation_method: ValuationMethod) -> Self {
        Self {
            product_id,
            location_id,
            quantity_on_hand: 0,
            quantity_reserved: 0,
            valuation_method,
            fifo_layers: VecDeque::new(),
        }
    }

    pub fn quantity_available(&self) -> i32 {
        self.quantity_on_hand - self.quantity_reserved
    }

    pub fn receive(&mut self, quantity: i32, unit_cost: f64) {
        self.quantity_on_hand += quantity;

        // Add FIFO layer
        if self.valuation_method == ValuationMethod::FIFO {
            self.fifo_layers.push_back(FIFOLayer {
                quantity,
                unit_cost,
                received_at: Utc::now(),
            });
        }
    }

    pub fn sell(&mut self, quantity: i32) -> Result<f64, String> {
        if quantity > self.quantity_available() {
            return Err(format!(
                "Insufficient stock: requested {}, available {}",
                quantity,
                self.quantity_available()
            ));
        }

        self.quantity_on_hand -= quantity;

        // Calculate COGS (Cost of Goods Sold) using FIFO
        if self.valuation_method == ValuationMethod::FIFO {
            Ok(self.calculate_fifo_cogs(quantity))
        } else {
            Ok(0.0)  // TODO: Implement LIFO and WeightedAverage
        }
    }

    fn calculate_fifo_cogs(&mut self, mut qty_to_sell: i32) -> f64 {
        let mut total_cost = 0.0;

        while qty_to_sell > 0 && !self.fifo_layers.is_empty() {
            if let Some(mut layer) = self.fifo_layers.pop_front() {
                let qty_from_layer = qty_to_sell.min(layer.quantity);

                total_cost += qty_from_layer as f64 * layer.unit_cost;
                qty_to_sell -= qty_from_layer;
                layer.quantity -= qty_from_layer;

                // Put remaining back if not fully consumed
                if layer.quantity > 0 {
                    self.fifo_layers.push_front(layer);
                }
            }
        }

        total_cost
    }

    pub fn current_value(&self) -> f64 {
        if self.valuation_method == ValuationMethod::FIFO {
            self.fifo_layers.iter()
                .map(|layer| layer.quantity as f64 * layer.unit_cost)
                .sum()
        } else {
            0.0
        }
    }
}

pub struct InventoryEngine {
    stock_levels: HashMap<(String, String), StockLevel>,  // (product_id, location_id)
    movements: Vec<StockMovement>,
}

impl InventoryEngine {
    pub fn new() -> Self {
        Self {
            stock_levels: HashMap::new(),
            movements: Vec::new(),
        }
    }

    pub fn get_or_create_stock_level(
        &mut self,
        product_id: &str,
        location_id: &str,
        valuation_method: ValuationMethod,
    ) -> &mut StockLevel {
        self.stock_levels
            .entry((product_id.to_string(), location_id.to_string()))
            .or_insert_with(|| StockLevel::new(
                product_id.to_string(),
                location_id.to_string(),
                valuation_method,
            ))
    }

    pub fn receive_inventory(
        &mut self,
        product_id: &str,
        location_id: &str,
        quantity: i32,
        unit_cost: f64,
    ) {
        let stock = self.get_or_create_stock_level(product_id, location_id, ValuationMethod::FIFO);
        stock.receive(quantity, unit_cost);

        self.movements.push(StockMovement {
            movement_id: format!("MOV-{}", self.movements.len() + 1),
            product_id: product_id.to_string(),
            quantity,
            unit_cost,
            location_id: location_id.to_string(),
            timestamp: Utc::now(),
            movement_type: MovementType::Receive,
        });
    }

    pub fn sell_inventory(
        &mut self,
        product_id: &str,
        location_id: &str,
        quantity: i32,
    ) -> Result<f64, String> {
        let stock = self.stock_levels
            .get_mut(&(product_id.to_string(), location_id.to_string()))
            .ok_or_else(|| "Stock not found".to_string())?;

        let cogs = stock.sell(quantity)?;

        self.movements.push(StockMovement {
            movement_id: format!("MOV-{}", self.movements.len() + 1),
            product_id: product_id.to_string(),
            quantity: -quantity,
            unit_cost: 0.0,  // Calculated via FIFO
            location_id: location_id.to_string(),
            timestamp: Utc::now(),
            movement_type: MovementType::Sell,
        });

        Ok(cogs)
    }

    pub fn check_reorder_needed(
        &self,
        product_id: &str,
        location_id: &str,
        reorder_point: i32,
    ) -> bool {
        if let Some(stock) = self.stock_levels.get(&(product_id.to_string(), location_id.to_string())) {
            stock.quantity_available() <= reorder_point
        } else {
            true  // No stock = needs reorder
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fifo_cogs_calculation() {
        let mut engine = InventoryEngine::new();

        // Receive 100 units @ $10
        engine.receive_inventory("PRODUCT-A", "WAREHOUSE-1", 100, 10.0);

        // Receive 50 units @ $12
        engine.receive_inventory("PRODUCT-A", "WAREHOUSE-1", 50, 12.0);

        // Sell 120 units (should consume 100@$10 + 20@$12)
        let cogs = engine.sell_inventory("PRODUCT-A", "WAREHOUSE-1", 120).unwrap();

        assert_eq!(cogs, 100.0 * 10.0 + 20.0 * 12.0);

        // Remaining stock: 30 units @ $12
        let stock = engine.stock_levels.get(&("PRODUCT-A".to_string(), "WAREHOUSE-1".to_string())).unwrap();
        assert_eq!(stock.quantity_on_hand, 30);
        assert_eq!(stock.current_value(), 30.0 * 12.0);
    }

    #[test]
    fn test_insufficient_stock_error() {
        let mut engine = InventoryEngine::new();
        engine.receive_inventory("PRODUCT-B", "WAREHOUSE-1", 50, 10.0);

        let result = engine.sell_inventory("PRODUCT-B", "WAREHOUSE-1", 100);
        assert!(result.is_err());
    }

    #[test]
    fn test_reorder_point_check() {
        let mut engine = InventoryEngine::new();
        engine.receive_inventory("PRODUCT-C", "WAREHOUSE-1", 100, 10.0);

        assert!(!engine.check_reorder_needed("PRODUCT-C", "WAREHOUSE-1", 20));

        engine.sell_inventory("PRODUCT-C", "WAREHOUSE-1", 85).unwrap();
        assert!(engine.check_reorder_needed("PRODUCT-C", "WAREHOUSE-1", 20));
    }
}
