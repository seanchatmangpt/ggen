// ============================================================================
// Order Processing Engine - Rust State Machine
// ============================================================================
// High-performance order state machine with transition validation
// ============================================================================

use std::collections::HashMap;
use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};

// ============================================================================
// Order State Machine
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum OrderStatus {
    Cart,
    Pending,
    Processing,
    Shipped,
    Delivered,
    Cancelled,
    Refunded,
    OnHold,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Order {
    pub order_number: String,
    pub customer_id: String,
    pub status: OrderStatus,
    pub items: Vec<OrderItem>,
    pub subtotal: f64,
    pub tax: f64,
    pub shipping_cost: f64,
    pub discount: f64,
    pub total: f64,
    pub currency: String,
    pub order_date: DateTime<Utc>,
    pub payment: Option<Payment>,
    pub shipment: Option<Shipment>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrderItem {
    pub product_id: String,
    pub sku: String,
    pub quantity: u32,
    pub unit_price: f64,
    pub line_total: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Payment {
    pub transaction_id: String,
    pub method: String,
    pub amount: f64,
    pub status: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Shipment {
    pub carrier: String,
    pub tracking_number: String,
    pub estimated_delivery: DateTime<Utc>,
    pub actual_delivery: Option<DateTime<Utc>>,
}

// ============================================================================
// State Transition Logic
// ============================================================================

pub struct OrderProcessor {
    valid_transitions: HashMap<OrderStatus, Vec<OrderStatus>>,
}

impl OrderProcessor {
    pub fn new() -> Self {
        let mut valid_transitions = HashMap::new();

        // Define valid state transitions
        valid_transitions.insert(OrderStatus::Cart, vec![
            OrderStatus::Pending,
            OrderStatus::Cancelled,
        ]);

        valid_transitions.insert(OrderStatus::Pending, vec![
            OrderStatus::Processing,
            OrderStatus::Cancelled,
            OrderStatus::OnHold,
        ]);

        valid_transitions.insert(OrderStatus::Processing, vec![
            OrderStatus::Shipped,
            OrderStatus::Cancelled,
            OrderStatus::OnHold,
        ]);

        valid_transitions.insert(OrderStatus::Shipped, vec![
            OrderStatus::Delivered,
            OrderStatus::OnHold,
        ]);

        valid_transitions.insert(OrderStatus::Delivered, vec![
            OrderStatus::Refunded,
        ]);

        valid_transitions.insert(OrderStatus::OnHold, vec![
            OrderStatus::Processing,
            OrderStatus::Cancelled,
        ]);

        Self { valid_transitions }
    }

    /// Validate if transition is allowed
    pub fn can_transition(&self, from: OrderStatus, to: OrderStatus) -> bool {
        self.valid_transitions
            .get(&from)
            .map(|allowed| allowed.contains(&to))
            .unwrap_or(false)
    }

    /// Transition order to new status
    pub fn transition(
        &self,
        order: &mut Order,
        new_status: OrderStatus,
    ) -> Result<(), String> {
        if !self.can_transition(order.status, new_status) {
            return Err(format!(
                "Invalid transition from {:?} to {:?}",
                order.status, new_status
            ));
        }

        order.status = new_status;
        Ok(())
    }

    /// Calculate order total
    pub fn calculate_total(order: &mut Order) {
        order.total = order.subtotal + order.tax + order.shipping_cost - order.discount;
    }

    /// Process payment
    pub fn process_payment(
        &self,
        order: &mut Order,
        payment: Payment,
    ) -> Result<(), String> {
        if order.status != OrderStatus::Pending {
            return Err("Order must be in Pending status to process payment".to_string());
        }

        order.payment = Some(payment);
        self.transition(order, OrderStatus::Processing)?;
        Ok(())
    }

    /// Ship order
    pub fn ship_order(
        &self,
        order: &mut Order,
        shipment: Shipment,
    ) -> Result<(), String> {
        if order.status != OrderStatus::Processing {
            return Err("Order must be in Processing status to ship".to_string());
        }

        order.shipment = Some(shipment);
        self.transition(order, OrderStatus::Shipped)?;
        Ok(())
    }

    /// Mark as delivered
    pub fn mark_delivered(&self, order: &mut Order) -> Result<(), String> {
        if order.status != OrderStatus::Shipped {
            return Err("Order must be in Shipped status to mark delivered".to_string());
        }

        if let Some(ref mut shipment) = order.shipment {
            shipment.actual_delivery = Some(Utc::now());
        }

        self.transition(order, OrderStatus::Delivered)?;
        Ok(())
    }

    /// Cancel order
    pub fn cancel_order(&self, order: &mut Order) -> Result<(), String> {
        match order.status {
            OrderStatus::Delivered | OrderStatus::Refunded | OrderStatus::Cancelled => {
                return Err(format!("Cannot cancel order in {:?} status", order.status));
            }
            _ => {}
        }

        self.transition(order, OrderStatus::Cancelled)?;
        Ok(())
    }
}

// ============================================================================
// Fulfillment Engine
// ============================================================================

pub struct FulfillmentEngine;

impl FulfillmentEngine {
    /// Generate pick list for warehouse
    pub fn generate_pick_list(order: &Order) -> Vec<(String, u32)> {
        order.items
            .iter()
            .map(|item| (item.sku.clone(), item.quantity))
            .collect()
    }

    /// Check inventory availability
    pub fn check_inventory(
        order: &Order,
        inventory: &HashMap<String, u32>,
    ) -> Result<(), Vec<String>> {
        let mut out_of_stock = Vec::new();

        for item in &order.items {
            let available = inventory.get(&item.sku).copied().unwrap_or(0);
            if available < item.quantity {
                out_of_stock.push(item.sku.clone());
            }
        }

        if out_of_stock.is_empty() {
            Ok(())
        } else {
            Err(out_of_stock)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_state_transitions() {
        let processor = OrderProcessor::new();

        assert!(processor.can_transition(OrderStatus::Cart, OrderStatus::Pending));
        assert!(processor.can_transition(OrderStatus::Pending, OrderStatus::Processing));
        assert!(!processor.can_transition(OrderStatus::Cart, OrderStatus::Shipped));
    }

    #[test]
    fn test_calculate_total() {
        let mut order = create_test_order();
        OrderProcessor::calculate_total(&mut order);

        // 100 + 10 + 5 - 15 = 100
        assert_eq!(order.total, 100.0);
    }

    fn create_test_order() -> Order {
        Order {
            order_number: "ORD-001".to_string(),
            customer_id: "CUST-001".to_string(),
            status: OrderStatus::Cart,
            items: vec![],
            subtotal: 100.0,
            tax: 10.0,
            shipping_cost: 5.0,
            discount: 15.0,
            total: 0.0,
            currency: "USD".to_string(),
            order_date: Utc::now(),
            payment: None,
            shipment: None,
        }
    }
}
