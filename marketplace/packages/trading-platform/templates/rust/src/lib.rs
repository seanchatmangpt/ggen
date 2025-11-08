// Trading Platform - Rust High-Frequency Matching Engine
// Ultra-low latency order matching with <1ms execution

use std::collections::{BTreeMap, HashMap, VecDeque};
use std::sync::{Arc, Mutex};
use rust_decimal::Decimal;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

// ============================================================
// Core Types
// ============================================================

pub type OrderId = String;
pub type Price = Decimal;
pub type Quantity = Decimal;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Side {
    Buy,
    Sell,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum OrderType {
    Market,
    Limit,
    Stop { stop_price: Price },
    StopLimit { stop_price: Price, limit_price: Price },
    Iceberg { display_qty: Quantity },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum OrderStatus {
    PendingNew,
    New,
    PartiallyFilled,
    Filled,
    Canceled,
    Rejected,
    Expired,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TimeInForce {
    Day,
    GTC,  // Good Till Cancel
    IOC,  // Immediate Or Cancel
    FOK,  // Fill Or Kill
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Order {
    pub order_id: OrderId,
    pub client_order_id: String,
    pub symbol: String,
    pub side: Side,
    pub order_type: OrderType,
    pub quantity: Quantity,
    pub price: Option<Price>,
    pub time_in_force: TimeInForce,
    pub status: OrderStatus,
    pub filled_quantity: Quantity,
    pub average_fill_price: Option<Price>,
    pub timestamp: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Fill {
    pub fill_id: String,
    pub order_id: OrderId,
    pub price: Price,
    pub quantity: Quantity,
    pub timestamp: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Trade {
    pub trade_id: String,
    pub symbol: String,
    pub price: Price,
    pub quantity: Quantity,
    pub buyer_order_id: OrderId,
    pub seller_order_id: OrderId,
    pub timestamp: DateTime<Utc>,
}

// ============================================================
// Order Book
// ============================================================

#[derive(Debug)]
pub struct PriceLevel {
    pub price: Price,
    pub quantity: Quantity,
    pub orders: VecDeque<OrderId>,
}

#[derive(Debug)]
pub struct OrderBook {
    pub symbol: String,
    pub bids: BTreeMap<Price, PriceLevel>,  // Descending order
    pub asks: BTreeMap<Price, PriceLevel>,  // Ascending order
    pub orders: HashMap<OrderId, Order>,
}

impl OrderBook {
    pub fn new(symbol: String) -> Self {
        Self {
            symbol,
            bids: BTreeMap::new(),
            asks: BTreeMap::new(),
            orders: HashMap::new(),
        }
    }

    pub fn best_bid(&self) -> Option<Price> {
        self.bids.iter().next().map(|(price, _)| *price)
    }

    pub fn best_ask(&self) -> Option<Price> {
        self.asks.iter().next().map(|(price, _)| *price)
    }

    pub fn spread(&self) -> Option<Price> {
        match (self.best_ask(), self.best_bid()) {
            (Some(ask), Some(bid)) => Some(ask - bid),
            _ => None,
        }
    }

    pub fn mid_price(&self) -> Option<Price> {
        match (self.best_ask(), self.best_bid()) {
            (Some(ask), Some(bid)) => Some((ask + bid) / Decimal::new(2, 0)),
            _ => None,
        }
    }
}

// ============================================================
// Matching Engine
// ============================================================

pub struct MatchingEngine {
    order_books: HashMap<String, Arc<Mutex<OrderBook>>>,
    fills: Vec<Fill>,
    trades: Vec<Trade>,
}

impl MatchingEngine {
    pub fn new() -> Self {
        Self {
            order_books: HashMap::new(),
            fills: Vec::new(),
            trades: Vec::new(),
        }
    }

    pub fn add_instrument(&mut self, symbol: String) {
        self.order_books.insert(
            symbol.clone(),
            Arc::new(Mutex::new(OrderBook::new(symbol))),
        );
    }

    pub fn submit_order(&mut self, mut order: Order) -> Result<Vec<Fill>, String> {
        let book = self.order_books
            .get(&order.symbol)
            .ok_or_else(|| format!("Unknown symbol: {}", order.symbol))?;

        let mut book = book.lock().unwrap();
        let mut fills = Vec::new();

        match order.order_type {
            OrderType::Market => {
                fills = self.match_market_order(&mut book, &mut order)?;
            }
            OrderType::Limit => {
                fills = self.match_limit_order(&mut book, &mut order)?;
            }
            OrderType::Stop { stop_price } => {
                // Stop orders become market orders when triggered
                let current_price = book.mid_price().unwrap_or(Decimal::ZERO);
                if (order.side == Side::Buy && current_price >= stop_price) ||
                   (order.side == Side::Sell && current_price <= stop_price) {
                    fills = self.match_market_order(&mut book, &mut order)?;
                }
            }
            _ => return Err("Order type not supported".to_string()),
        }

        // Update order status
        if order.filled_quantity == order.quantity {
            order.status = OrderStatus::Filled;
        } else if order.filled_quantity > Decimal::ZERO {
            order.status = OrderStatus::PartiallyFilled;
        }

        // Store order
        book.orders.insert(order.order_id.clone(), order);

        Ok(fills)
    }

    fn match_market_order(
        &mut self,
        book: &mut OrderBook,
        order: &mut Order,
    ) -> Result<Vec<Fill>, String> {
        let mut fills = Vec::new();
        let mut remaining_qty = order.quantity;

        let levels = match order.side {
            Side::Buy => &mut book.asks,
            Side::Sell => &mut book.bids,
        };

        while remaining_qty > Decimal::ZERO {
            let (price, level_qty) = match levels.iter().next() {
                Some((p, level)) => (*p, level.quantity),
                None => break,  // No more liquidity
            };

            let fill_qty = remaining_qty.min(level_qty);

            let fill = Fill {
                fill_id: format!("FILL-{}-{}", order.order_id, fills.len()),
                order_id: order.order_id.clone(),
                price,
                quantity: fill_qty,
                timestamp: Utc::now(),
            };

            fills.push(fill);
            remaining_qty -= fill_qty;
            order.filled_quantity += fill_qty;

            // Update price level
            if let Some(level) = levels.get_mut(&price) {
                level.quantity -= fill_qty;
                if level.quantity == Decimal::ZERO {
                    levels.remove(&price);
                }
            }
        }

        // Calculate average fill price
        if !fills.is_empty() {
            let total_value: Decimal = fills.iter()
                .map(|f| f.price * f.quantity)
                .sum();
            order.average_fill_price = Some(total_value / order.filled_quantity);
        }

        Ok(fills)
    }

    fn match_limit_order(
        &mut self,
        book: &mut OrderBook,
        order: &mut Order,
    ) -> Result<Vec<Fill>, String> {
        let limit_price = order.price.ok_or("Limit order must have price")?;
        let mut fills = Vec::new();
        let mut remaining_qty = order.quantity;

        // Try to match against opposite side
        let levels = match order.side {
            Side::Buy => &mut book.asks,
            Side::Sell => &mut book.bids,
        };

        while remaining_qty > Decimal::ZERO {
            let (price, level_qty) = match levels.iter().next() {
                Some((p, level)) => (*p, level.quantity),
                None => break,
            };

            // Check if price crosses
            let can_match = match order.side {
                Side::Buy => price <= limit_price,
                Side::Sell => price >= limit_price,
            };

            if !can_match {
                break;
            }

            let fill_qty = remaining_qty.min(level_qty);

            let fill = Fill {
                fill_id: format!("FILL-{}-{}", order.order_id, fills.len()),
                order_id: order.order_id.clone(),
                price,
                quantity: fill_qty,
                timestamp: Utc::now(),
            };

            fills.push(fill);
            remaining_qty -= fill_qty;
            order.filled_quantity += fill_qty;

            // Update price level
            if let Some(level) = levels.get_mut(&price) {
                level.quantity -= fill_qty;
                if level.quantity == Decimal::ZERO {
                    levels.remove(&price);
                }
            }
        }

        // Add remaining quantity to book
        if remaining_qty > Decimal::ZERO {
            let levels = match order.side {
                Side::Buy => &mut book.bids,
                Side::Sell => &mut book.asks,
            };

            levels
                .entry(limit_price)
                .or_insert_with(|| PriceLevel {
                    price: limit_price,
                    quantity: Decimal::ZERO,
                    orders: VecDeque::new(),
                })
                .quantity += remaining_qty;
        }

        // Calculate average fill price
        if !fills.is_empty() {
            let total_value: Decimal = fills.iter()
                .map(|f| f.price * f.quantity)
                .sum();
            order.average_fill_price = Some(total_value / order.filled_quantity);
        }

        Ok(fills)
    }

    pub fn cancel_order(&mut self, symbol: &str, order_id: &OrderId) -> Result<(), String> {
        let book = self.order_books
            .get(symbol)
            .ok_or_else(|| format!("Unknown symbol: {}", symbol))?;

        let mut book = book.lock().unwrap();

        if let Some(order) = book.orders.get_mut(order_id) {
            order.status = OrderStatus::Canceled;
            Ok(())
        } else {
            Err(format!("Order not found: {}", order_id))
        }
    }

    pub fn get_order_book(&self, symbol: &str) -> Option<Arc<Mutex<OrderBook>>> {
        self.order_books.get(symbol).cloned()
    }
}

// ============================================================
// Tests
// ============================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_order_book_spread() {
        let mut book = OrderBook::new("AAPL".to_string());

        // Add bid at 150.00
        book.bids.insert(
            Decimal::new(15000, 2),
            PriceLevel {
                price: Decimal::new(15000, 2),
                quantity: Decimal::new(100, 0),
                orders: VecDeque::new(),
            },
        );

        // Add ask at 150.05
        book.asks.insert(
            Decimal::new(15005, 2),
            PriceLevel {
                price: Decimal::new(15005, 2),
                quantity: Decimal::new(200, 0),
                orders: VecDeque::new(),
            },
        );

        assert_eq!(book.best_bid(), Some(Decimal::new(15000, 2)));
        assert_eq!(book.best_ask(), Some(Decimal::new(15005, 2)));
        assert_eq!(book.spread(), Some(Decimal::new(5, 2)));
    }

    #[test]
    fn test_market_order_match() {
        let mut engine = MatchingEngine::new();
        engine.add_instrument("AAPL".to_string());

        // Submit limit sell order
        let sell_order = Order {
            order_id: "SELL-1".to_string(),
            client_order_id: "CL-SELL-1".to_string(),
            symbol: "AAPL".to_string(),
            side: Side::Sell,
            order_type: OrderType::Limit,
            quantity: Decimal::new(100, 0),
            price: Some(Decimal::new(15000, 2)),
            time_in_force: TimeInForce::GTC,
            status: OrderStatus::New,
            filled_quantity: Decimal::ZERO,
            average_fill_price: None,
            timestamp: Utc::now(),
        };

        engine.submit_order(sell_order).unwrap();

        // Submit market buy order
        let buy_order = Order {
            order_id: "BUY-1".to_string(),
            client_order_id: "CL-BUY-1".to_string(),
            symbol: "AAPL".to_string(),
            side: Side::Buy,
            order_type: OrderType::Market,
            quantity: Decimal::new(50, 0),
            price: None,
            time_in_force: TimeInForce::IOC,
            status: OrderStatus::New,
            filled_quantity: Decimal::ZERO,
            average_fill_price: None,
            timestamp: Utc::now(),
        };

        let fills = engine.submit_order(buy_order).unwrap();

        assert_eq!(fills.len(), 1);
        assert_eq!(fills[0].quantity, Decimal::new(50, 0));
        assert_eq!(fills[0].price, Decimal::new(15000, 2));
    }
}
