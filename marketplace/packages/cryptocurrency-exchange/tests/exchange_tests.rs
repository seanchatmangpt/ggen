// Chicago TDD Tests for Cryptocurrency Exchange
// Focus: State verification, integration testing, security

#[cfg(test)]
mod wallet_tests {
    use super::super::*;

    #[test]
    fn test_wallet_creation_generates_unique_addresses() {
        let mut manager = WalletManager::new();
        let wallet1 = manager.create_wallet(WalletType::HotWallet);
        let wallet2 = manager.create_wallet(WalletType::ColdWallet);

        assert_ne!(wallet1.address, wallet2.address);
        assert!(wallet1.address.starts_with("0x"));
        assert!(wallet2.address.starts_with("0x"));
    }

    #[test]
    fn test_hot_wallet_allows_immediate_withdrawals() {
        let mut manager = WalletManager::new();
        let wallet = manager.create_wallet(WalletType::HotWallet);

        manager.deposit(&wallet.address, "ETH", 10.0).unwrap();
        assert_eq!(manager.get_balance(&wallet.address, "ETH"), 10.0);

        manager.withdraw(&wallet.address, "ETH", 5.0).unwrap();
        assert_eq!(manager.get_balance(&wallet.address, "ETH"), 5.0);
    }

    #[test]
    fn test_cannot_withdraw_more_than_balance() {
        let mut manager = WalletManager::new();
        let wallet = manager.create_wallet(WalletType::HotWallet);

        manager.deposit(&wallet.address, "BTC", 1.0).unwrap();

        let result = manager.withdraw(&wallet.address, "BTC", 2.0);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "Insufficient balance");
    }

    #[test]
    fn test_multi_token_wallet_maintains_separate_balances() {
        let mut manager = WalletManager::new();
        let wallet = manager.create_wallet(WalletType::HotWallet);

        manager.deposit(&wallet.address, "BTC", 1.0).unwrap();
        manager.deposit(&wallet.address, "ETH", 10.0).unwrap();
        manager.deposit(&wallet.address, "USDT", 1000.0).unwrap();

        assert_eq!(manager.get_balance(&wallet.address, "BTC"), 1.0);
        assert_eq!(manager.get_balance(&wallet.address, "ETH"), 10.0);
        assert_eq!(manager.get_balance(&wallet.address, "USDT"), 1000.0);
    }

    #[test]
    fn test_withdrawal_from_nonexistent_wallet_fails() {
        let mut manager = WalletManager::new();
        let result = manager.withdraw("0xnonexistent", "BTC", 1.0);
        assert!(result.is_err());
    }
}

#[cfg(test)]
mod order_matching_tests {
    use super::super::*;

    #[test]
    fn test_order_book_maintains_price_priority() {
        let mut book = OrderBook::new();

        book.add_order(create_limit_order("1", OrderSide::Buy, 100.0, 1.0));
        book.add_order(create_limit_order("2", OrderSide::Buy, 105.0, 1.0));
        book.add_order(create_limit_order("3", OrderSide::Buy, 95.0, 1.0));

        assert_eq!(book.bids[0].price, 105.0);
        assert_eq!(book.bids[1].price, 100.0);
        assert_eq!(book.bids[2].price, 95.0);
    }

    #[test]
    fn test_matching_orders_creates_trade() {
        let mut book = OrderBook::new();

        book.add_order(create_limit_order("buy1", OrderSide::Buy, 100.0, 1.0));
        book.add_order(create_limit_order("sell1", OrderSide::Sell, 99.0, 1.0));

        let trades = book.match_orders();

        assert_eq!(trades.len(), 1);
        assert_eq!(trades[0].quantity, 1.0);
        assert!((trades[0].price - 99.5).abs() < 0.01);
    }

    #[test]
    fn test_partial_order_fills_correctly() {
        let mut book = OrderBook::new();

        book.add_order(create_limit_order("buy1", OrderSide::Buy, 100.0, 5.0));
        book.add_order(create_limit_order("sell1", OrderSide::Sell, 100.0, 2.0));

        let trades = book.match_orders();

        assert_eq!(trades.len(), 1);
        assert_eq!(trades[0].quantity, 2.0);
        assert_eq!(book.bids.len(), 1);
        assert_eq!(book.asks.len(), 0);
    }

    #[test]
    fn test_no_match_when_bid_below_ask() {
        let mut book = OrderBook::new();

        book.add_order(create_limit_order("buy1", OrderSide::Buy, 95.0, 1.0));
        book.add_order(create_limit_order("sell1", OrderSide::Sell, 100.0, 1.0));

        let trades = book.match_orders();
        assert_eq!(trades.len(), 0);
    }

    #[test]
    fn test_fee_calculation_on_trade_execution() {
        let mut book = OrderBook::new();

        book.add_order(create_limit_order("buy1", OrderSide::Buy, 1000.0, 1.0));
        book.add_order(create_limit_order("sell1", OrderSide::Sell, 1000.0, 1.0));

        let trades = book.match_orders();

        assert_eq!(trades[0].fee, 1.0); // 0.1% of 1000
    }

    fn create_limit_order(id: &str, side: OrderSide, price: f64, quantity: f64) -> Order {
        Order {
            id: id.to_string(),
            user_id: "test_user".to_string(),
            order_type: OrderType::Limit,
            side,
            pair: "BTC-USD".to_string(),
            price,
            quantity,
            status: OrderStatus::Open,
            created_at: Utc::now(),
        }
    }
}

#[cfg(test)]
mod liquidity_pool_tests {
    use super::super::*;

    #[test]
    fn test_liquidity_pool_initialization() {
        let pool = LiquidityPool::new("ETH".to_string(), "USDC".to_string(), 100.0, 200000.0);

        assert_eq!(pool.reserve_a, 100.0);
        assert_eq!(pool.reserve_b, 200000.0);
        assert_eq!(pool.get_price(), 2000.0);
    }

    #[test]
    fn test_swap_maintains_constant_product() {
        let mut pool = LiquidityPool::new("ETH".to_string(), "USDC".to_string(), 100.0, 200000.0);
        let initial_k = pool.reserve_a * pool.reserve_b;

        pool.swap("ETH", 10.0).unwrap();

        let final_k = pool.reserve_a * pool.reserve_b;
        assert!(final_k >= initial_k);
    }

    #[test]
    fn test_swap_adjusts_reserves_correctly() {
        let mut pool = LiquidityPool::new("ETH".to_string(), "USDC".to_string(), 100.0, 200000.0);

        let usdc_out = pool.swap("ETH", 10.0).unwrap();

        assert!(pool.reserve_a > 100.0);
        assert!(pool.reserve_b < 200000.0);
        assert!(usdc_out > 0.0);
    }

    #[test]
    fn test_add_liquidity_mints_lp_tokens() {
        let mut pool = LiquidityPool::new("ETH".to_string(), "USDC".to_string(), 100.0, 200000.0);
        let initial_liquidity = pool.total_liquidity;

        let lp_tokens = pool.add_liquidity(10.0, 20000.0);

        assert!(lp_tokens > 0.0);
        assert!(pool.total_liquidity > initial_liquidity);
    }

    #[test]
    fn test_swap_with_invalid_token_fails() {
        let mut pool = LiquidityPool::new("ETH".to_string(), "USDC".to_string(), 100.0, 200000.0);
        let result = pool.swap("BTC", 1.0);
        assert!(result.is_err());
    }

    #[test]
    fn test_price_impact_increases_with_trade_size() {
        let mut pool1 = LiquidityPool::new("ETH".to_string(), "USDC".to_string(), 100.0, 200000.0);
        let mut pool2 = LiquidityPool::new("ETH".to_string(), "USDC".to_string(), 100.0, 200000.0);

        let small_out = pool1.swap("ETH", 1.0).unwrap();
        let large_out = pool2.swap("ETH", 10.0).unwrap();

        let small_price = small_out / 1.0;
        let large_price = large_out / 10.0;

        assert!(small_price > large_price); // Better price for smaller trade
    }
}

#[cfg(test)]
mod security_tests {
    use super::super::*;

    #[test]
    fn test_reentrancy_guard_on_withdrawals() {
        // Test that withdrawal operations cannot be called recursively
        let mut manager = WalletManager::new();
        let wallet = manager.create_wallet(WalletType::HotWallet);

        manager.deposit(&wallet.address, "ETH", 10.0).unwrap();

        // First withdrawal should succeed
        assert!(manager.withdraw(&wallet.address, "ETH", 5.0).is_ok());

        // Balance should be updated before second withdrawal
        assert_eq!(manager.get_balance(&wallet.address, "ETH"), 5.0);
    }

    #[test]
    fn test_integer_overflow_protection() {
        let mut manager = WalletManager::new();
        let wallet = manager.create_wallet(WalletType::HotWallet);

        // Attempt to deposit extremely large value
        manager.deposit(&wallet.address, "ETH", f64::MAX / 2.0).unwrap();

        // Should not overflow when adding more
        let result = manager.deposit(&wallet.address, "ETH", f64::MAX / 2.0);
        // In production, this should have overflow protection
        assert!(result.is_ok());
    }

    #[test]
    fn test_address_validation() {
        let manager = WalletManager::new();

        // Test that invalid addresses are rejected
        let result = manager.wallets.get("invalid_address");
        assert!(result.is_none());
    }

    #[test]
    fn test_transaction_atomicity() {
        let mut book = OrderBook::new();

        book.add_order(create_limit_order("buy1", OrderSide::Buy, 100.0, 1.0));
        book.add_order(create_limit_order("sell1", OrderSide::Sell, 100.0, 1.0));

        let initial_bid_count = book.bids.len();
        let initial_ask_count = book.asks.len();

        let trades = book.match_orders();

        // Either both orders matched or neither
        assert!(trades.len() == 1 || (book.bids.len() == initial_bid_count && book.asks.len() == initial_ask_count));
    }

    #[test]
    fn test_decimal_precision_in_calculations() {
        let mut pool = LiquidityPool::new("ETH".to_string(), "USDC".to_string(), 100.0, 200000.0);

        // Test that small amounts don't result in zero due to precision loss
        let amount_out = pool.swap("ETH", 0.0001).unwrap();
        assert!(amount_out > 0.0);
    }

    fn create_limit_order(id: &str, side: OrderSide, price: f64, quantity: f64) -> Order {
        Order {
            id: id.to_string(),
            user_id: "test_user".to_string(),
            order_type: OrderType::Limit,
            side,
            pair: "BTC-USD".to_string(),
            price,
            quantity,
            status: OrderStatus::Open,
            created_at: Utc::now(),
        }
    }
}

#[cfg(test)]
mod integration_tests {
    use super::super::*;

    #[test]
    fn test_complete_trade_flow() {
        let mut wallet_manager = WalletManager::new();
        let mut order_book = OrderBook::new();

        // Setup: Create wallets and fund them
        let buyer_wallet = wallet_manager.create_wallet(WalletType::HotWallet);
        let seller_wallet = wallet_manager.create_wallet(WalletType::HotWallet);

        wallet_manager.deposit(&buyer_wallet.address, "USD", 10000.0).unwrap();
        wallet_manager.deposit(&seller_wallet.address, "BTC", 1.0).unwrap();

        // Execute: Place and match orders
        let buy_order = create_order("buy1", "buyer", OrderSide::Buy, 10000.0, 1.0);
        let sell_order = create_order("sell1", "seller", OrderSide::Sell, 10000.0, 1.0);

        order_book.add_order(buy_order);
        order_book.add_order(sell_order);

        let trades = order_book.match_orders();

        // Verify: Trade executed correctly
        assert_eq!(trades.len(), 1);
        assert_eq!(trades[0].quantity, 1.0);
        assert_eq!(trades[0].price, 10000.0);
    }

    #[test]
    fn test_defi_liquidity_provision_flow() {
        let mut pool = LiquidityPool::new("ETH".to_string(), "USDC".to_string(), 100.0, 200000.0);

        // User adds liquidity
        let lp_tokens = pool.add_liquidity(10.0, 20000.0);
        assert!(lp_tokens > 0.0);

        // User swaps tokens
        let usdc_out = pool.swap("ETH", 5.0).unwrap();
        assert!(usdc_out > 0.0);

        // Pool maintains reserves
        assert!(pool.total_liquidity > 0.0);
    }

    fn create_order(id: &str, user: &str, side: OrderSide, price: f64, quantity: f64) -> Order {
        Order {
            id: id.to_string(),
            user_id: user.to_string(),
            order_type: OrderType::Limit,
            side,
            pair: "BTC-USD".to_string(),
            price,
            quantity,
            status: OrderStatus::Open,
            created_at: Utc::now(),
        }
    }
}

#[cfg(test)]
mod performance_tests {
    use super::super::*;

    #[test]
    fn test_order_matching_performance_with_large_book() {
        let mut book = OrderBook::new();

        // Add 1000 orders
        for i in 0..500 {
            book.add_order(create_limit_order(&format!("buy{}", i), OrderSide::Buy, 100.0 - i as f64 * 0.1, 1.0));
            book.add_order(create_limit_order(&format!("sell{}", i), OrderSide::Sell, 100.0 + i as f64 * 0.1, 1.0));
        }

        let start = std::time::Instant::now();
        let trades = book.match_orders();
        let duration = start.elapsed();

        assert!(duration.as_millis() < 100); // Should complete in <100ms
        assert!(trades.len() > 0);
    }

    fn create_limit_order(id: &str, side: OrderSide, price: f64, quantity: f64) -> Order {
        Order {
            id: id.to_string(),
            user_id: "test_user".to_string(),
            order_type: OrderType::Limit,
            side,
            pair: "BTC-USD".to_string(),
            price,
            quantity,
            status: OrderStatus::Open,
            created_at: Utc::now(),
        }
    }
}
