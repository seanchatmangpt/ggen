/// Cryptocurrency Exchange Core Implementation
/// Provides wallet management, order matching, and DeFi protocol integration

use std::collections::HashMap;
use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Wallet {
    pub address: String,
    pub wallet_type: WalletType,
    pub balances: HashMap<String, f64>,
    pub created_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum WalletType {
    HotWallet,
    ColdWallet,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Order {
    pub id: String,
    pub user_id: String,
    pub order_type: OrderType,
    pub side: OrderSide,
    pub pair: String,
    pub price: f64,
    pub quantity: f64,
    pub status: OrderStatus,
    pub created_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OrderType {
    Market,
    Limit,
    StopLoss,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OrderSide {
    Buy,
    Sell,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OrderStatus {
    Open,
    PartiallyFilled,
    Filled,
    Cancelled,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Trade {
    pub id: String,
    pub buy_order_id: String,
    pub sell_order_id: String,
    pub price: f64,
    pub quantity: f64,
    pub fee: f64,
    pub executed_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LiquidityPool {
    pub id: String,
    pub token_a: String,
    pub token_b: String,
    pub reserve_a: f64,
    pub reserve_b: f64,
    pub total_liquidity: f64,
    pub apy: f64,
}

pub struct OrderBook {
    pub bids: Vec<Order>,
    pub asks: Vec<Order>,
}

impl OrderBook {
    pub fn new() -> Self {
        Self {
            bids: Vec::new(),
            asks: Vec::new(),
        }
    }

    pub fn add_order(&mut self, order: Order) {
        match order.side {
            OrderSide::Buy => {
                self.bids.push(order);
                self.bids.sort_by(|a, b| b.price.partial_cmp(&a.price).unwrap());
            }
            OrderSide::Sell => {
                self.asks.push(order);
                self.asks.sort_by(|a, b| a.price.partial_cmp(&b.price).unwrap());
            }
        }
    }

    pub fn match_orders(&mut self) -> Vec<Trade> {
        let mut trades = Vec::new();

        while !self.bids.is_empty() && !self.asks.is_empty() {
            let bid = &self.bids[0];
            let ask = &self.asks[0];

            if bid.price >= ask.price {
                let quantity = bid.quantity.min(ask.quantity);
                let price = (bid.price + ask.price) / 2.0;
                let fee = quantity * price * 0.001; // 0.1% fee

                let trade = Trade {
                    id: uuid::Uuid::new_v4().to_string(),
                    buy_order_id: bid.id.clone(),
                    sell_order_id: ask.id.clone(),
                    price,
                    quantity,
                    fee,
                    executed_at: Utc::now(),
                };

                trades.push(trade);

                // Update order quantities
                if bid.quantity <= ask.quantity {
                    self.bids.remove(0);
                }
                if ask.quantity <= bid.quantity {
                    self.asks.remove(0);
                }
            } else {
                break;
            }
        }

        trades
    }
}

impl LiquidityPool {
    pub fn new(token_a: String, token_b: String, reserve_a: f64, reserve_b: f64) -> Self {
        let total_liquidity = (reserve_a * reserve_b).sqrt();

        Self {
            id: uuid::Uuid::new_v4().to_string(),
            token_a,
            token_b,
            reserve_a,
            reserve_b,
            total_liquidity,
            apy: 0.0,
        }
    }

    pub fn get_price(&self) -> f64 {
        self.reserve_b / self.reserve_a
    }

    pub fn swap(&mut self, token_in: &str, amount_in: f64) -> Result<f64, String> {
        let fee = amount_in * 0.003; // 0.3% fee
        let amount_in_with_fee = amount_in - fee;

        let (reserve_in, reserve_out) = if token_in == self.token_a {
            (self.reserve_a, self.reserve_b)
        } else if token_in == self.token_b {
            (self.reserve_b, self.reserve_a)
        } else {
            return Err("Invalid token".to_string());
        };

        let amount_out = (reserve_out * amount_in_with_fee) / (reserve_in + amount_in_with_fee);

        // Update reserves
        if token_in == self.token_a {
            self.reserve_a += amount_in;
            self.reserve_b -= amount_out;
        } else {
            self.reserve_b += amount_in;
            self.reserve_a -= amount_out;
        }

        self.total_liquidity = (self.reserve_a * self.reserve_b).sqrt();

        Ok(amount_out)
    }

    pub fn add_liquidity(&mut self, amount_a: f64, amount_b: f64) -> f64 {
        let liquidity_minted = ((amount_a / self.reserve_a) * self.total_liquidity)
            .min((amount_b / self.reserve_b) * self.total_liquidity);

        self.reserve_a += amount_a;
        self.reserve_b += amount_b;
        self.total_liquidity += liquidity_minted;

        liquidity_minted
    }
}

pub struct WalletManager {
    wallets: HashMap<String, Wallet>,
}

impl WalletManager {
    pub fn new() -> Self {
        Self {
            wallets: HashMap::new(),
        }
    }

    pub fn create_wallet(&mut self, wallet_type: WalletType) -> Wallet {
        let wallet = Wallet {
            address: format!("0x{}", uuid::Uuid::new_v4().to_string().replace("-", "")),
            wallet_type,
            balances: HashMap::new(),
            created_at: Utc::now(),
        };

        self.wallets.insert(wallet.address.clone(), wallet.clone());
        wallet
    }

    pub fn deposit(&mut self, address: &str, token: &str, amount: f64) -> Result<(), String> {
        let wallet = self.wallets.get_mut(address)
            .ok_or_else(|| "Wallet not found".to_string())?;

        *wallet.balances.entry(token.to_string()).or_insert(0.0) += amount;
        Ok(())
    }

    pub fn withdraw(&mut self, address: &str, token: &str, amount: f64) -> Result<(), String> {
        let wallet = self.wallets.get_mut(address)
            .ok_or_else(|| "Wallet not found".to_string())?;

        let balance = wallet.balances.get(token).unwrap_or(&0.0);
        if *balance < amount {
            return Err("Insufficient balance".to_string());
        }

        *wallet.balances.get_mut(token).unwrap() -= amount;
        Ok(())
    }

    pub fn get_balance(&self, address: &str, token: &str) -> f64 {
        self.wallets.get(address)
            .and_then(|w| w.balances.get(token))
            .copied()
            .unwrap_or(0.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wallet_creation() {
        let mut manager = WalletManager::new();
        let wallet = manager.create_wallet(WalletType::HotWallet);
        assert!(wallet.address.starts_with("0x"));
    }

    #[test]
    fn test_deposit_withdraw() {
        let mut manager = WalletManager::new();
        let wallet = manager.create_wallet(WalletType::HotWallet);

        manager.deposit(&wallet.address, "BTC", 1.0).unwrap();
        assert_eq!(manager.get_balance(&wallet.address, "BTC"), 1.0);

        manager.withdraw(&wallet.address, "BTC", 0.5).unwrap();
        assert_eq!(manager.get_balance(&wallet.address, "BTC"), 0.5);
    }

    #[test]
    fn test_order_matching() {
        let mut book = OrderBook::new();

        let buy_order = Order {
            id: "1".to_string(),
            user_id: "user1".to_string(),
            order_type: OrderType::Limit,
            side: OrderSide::Buy,
            pair: "BTC-USD".to_string(),
            price: 50000.0,
            quantity: 1.0,
            status: OrderStatus::Open,
            created_at: Utc::now(),
        };

        let sell_order = Order {
            id: "2".to_string(),
            user_id: "user2".to_string(),
            order_type: OrderType::Limit,
            side: OrderSide::Sell,
            pair: "BTC-USD".to_string(),
            price: 49900.0,
            quantity: 1.0,
            status: OrderStatus::Open,
            created_at: Utc::now(),
        };

        book.add_order(buy_order);
        book.add_order(sell_order);

        let trades = book.match_orders();
        assert_eq!(trades.len(), 1);
        assert_eq!(trades[0].quantity, 1.0);
    }

    #[test]
    fn test_liquidity_pool_swap() {
        let mut pool = LiquidityPool::new("ETH".to_string(), "USDC".to_string(), 100.0, 200000.0);

        let amount_out = pool.swap("ETH", 1.0).unwrap();
        assert!(amount_out > 0.0);
        assert!(pool.reserve_a > 100.0);
        assert!(pool.reserve_b < 200000.0);
    }
}
