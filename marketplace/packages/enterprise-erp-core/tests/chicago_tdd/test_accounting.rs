// ============================================================================
// Enterprise ERP Core - Chicago TDD Tests
// Test-Driven Development with London School (Chicago style)
// ============================================================================

#[cfg(test)]
mod accounting_tests {
    use super::*;
    use chrono::NaiveDate;
    use rust_decimal::Decimal;

    // Mock implementations (in real code, these would be in separate files)
    #[path = "../../templates/rust/accounting_engine.rs"]
    mod accounting_engine;
    use accounting_engine::*;

    // ========================================================================
    // Double-Entry Validation Tests
    // ========================================================================

    #[test]
    fn test_balanced_entry_accepted() {
        let mut engine = AccountingEngine::new();
        setup_basic_accounts(&mut engine);

        let entry = create_balanced_entry("JE001", 10000);
        assert!(engine.post_entry(entry).is_ok());
    }

    #[test]
    fn test_unbalanced_entry_rejected() {
        let mut engine = AccountingEngine::new();
        setup_basic_accounts(&mut engine);

        let entry = JournalEntry {
            entry_number: "JE002".to_string(),
            entry_date: NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
            description: "Unbalanced entry".to_string(),
            lines: vec![
                JournalLine {
                    account: "1000".to_string(),
                    debit: Decimal::from(5000),
                    credit: Decimal::ZERO,
                    department: None,
                    memo: None,
                },
                JournalLine {
                    account: "3000".to_string(),
                    debit: Decimal::ZERO,
                    credit: Decimal::from(3000), // Not balanced!
                    department: None,
                    memo: None,
                },
            ],
            is_reversing: false,
            created_by: "admin".to_string(),
            created_at: chrono::Utc::now(),
        };

        assert!(engine.post_entry(entry).is_err());
    }

    #[test]
    fn test_department_required_validation() {
        let mut engine = AccountingEngine::new();

        // Create account requiring department
        engine.add_account(Account {
            number: "5000".to_string(),
            name: "Departmental Expense".to_string(),
            account_type: AccountType::Expense,
            parent: None,
            is_active: true,
            requires_department: true,
        }).unwrap();

        engine.add_account(Account {
            number: "1000".to_string(),
            name: "Cash".to_string(),
            account_type: AccountType::Asset,
            parent: None,
            is_active: true,
            requires_department: false,
        }).unwrap();

        // Entry without department should fail
        let entry = JournalEntry {
            entry_number: "JE003".to_string(),
            entry_date: NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
            description: "Missing department".to_string(),
            lines: vec![
                JournalLine {
                    account: "5000".to_string(),
                    debit: Decimal::from(1000),
                    credit: Decimal::ZERO,
                    department: None, // Missing!
                    memo: None,
                },
                JournalLine {
                    account: "1000".to_string(),
                    debit: Decimal::ZERO,
                    credit: Decimal::from(1000),
                    department: None,
                    memo: None,
                },
            ],
            is_reversing: false,
            created_by: "admin".to_string(),
            created_at: chrono::Utc::now(),
        };

        assert!(engine.post_entry(entry).is_err());
    }

    #[test]
    fn test_multiple_line_entry_balancing() {
        let mut engine = AccountingEngine::new();
        setup_comprehensive_accounts(&mut engine);

        let entry = JournalEntry {
            entry_number: "JE004".to_string(),
            entry_date: NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
            description: "Multi-line entry".to_string(),
            lines: vec![
                JournalLine {
                    account: "1000".to_string(), // Cash
                    debit: Decimal::from(5000),
                    credit: Decimal::ZERO,
                    department: None,
                    memo: None,
                },
                JournalLine {
                    account: "1200".to_string(), // A/R
                    debit: Decimal::from(3000),
                    credit: Decimal::ZERO,
                    department: None,
                    memo: None,
                },
                JournalLine {
                    account: "4000".to_string(), // Revenue
                    debit: Decimal::ZERO,
                    credit: Decimal::from(8000),
                    department: None,
                    memo: None,
                },
            ],
            is_reversing: false,
            created_by: "admin".to_string(),
            created_at: chrono::Utc::now(),
        };

        assert!(engine.post_entry(entry).is_ok());
    }

    // ========================================================================
    // Trial Balance Tests
    // ========================================================================

    #[test]
    fn test_trial_balance_generation() {
        let mut engine = AccountingEngine::new();
        setup_basic_accounts(&mut engine);

        // Post several entries
        engine.post_entry(create_balanced_entry("JE001", 10000)).unwrap();
        engine.post_entry(create_balanced_entry("JE002", 5000)).unwrap();
        engine.post_entry(create_balanced_entry("JE003", 3000)).unwrap();

        let tb = engine.generate_trial_balance(
            NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
            NaiveDate::from_ymd_opt(2025, 1, 31).unwrap(),
        );

        assert_eq!(tb.accounts.len(), 2); // Cash and Common Stock
        assert!(tb.accounts.iter().any(|a| a.account_number == "1000"));
        assert!(tb.accounts.iter().any(|a| a.account_number == "3000"));
    }

    #[test]
    fn test_trial_balance_balances() {
        let mut engine = AccountingEngine::new();
        setup_basic_accounts(&mut engine);

        engine.post_entry(create_balanced_entry("JE001", 10000)).unwrap();

        let tb = engine.generate_trial_balance(
            NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
            NaiveDate::from_ymd_opt(2025, 1, 31).unwrap(),
        );

        let total_debits: Decimal = tb.accounts.iter().map(|a| a.debit_total).sum();
        let total_credits: Decimal = tb.accounts.iter().map(|a| a.credit_total).sum();

        assert_eq!(total_debits, total_credits);
    }

    #[test]
    fn test_trial_balance_date_filtering() {
        let mut engine = AccountingEngine::new();
        setup_basic_accounts(&mut engine);

        // Entry in January
        let jan_entry = JournalEntry {
            entry_number: "JE001".to_string(),
            entry_date: NaiveDate::from_ymd_opt(2025, 1, 15).unwrap(),
            description: "January entry".to_string(),
            lines: vec![
                JournalLine {
                    account: "1000".to_string(),
                    debit: Decimal::from(5000),
                    credit: Decimal::ZERO,
                    department: None,
                    memo: None,
                },
                JournalLine {
                    account: "3000".to_string(),
                    debit: Decimal::ZERO,
                    credit: Decimal::from(5000),
                    department: None,
                    memo: None,
                },
            ],
            is_reversing: false,
            created_by: "admin".to_string(),
            created_at: chrono::Utc::now(),
        };

        // Entry in February
        let feb_entry = JournalEntry {
            entry_number: "JE002".to_string(),
            entry_date: NaiveDate::from_ymd_opt(2025, 2, 15).unwrap(),
            description: "February entry".to_string(),
            lines: vec![
                JournalLine {
                    account: "1000".to_string(),
                    debit: Decimal::from(3000),
                    credit: Decimal::ZERO,
                    department: None,
                    memo: None,
                },
                JournalLine {
                    account: "3000".to_string(),
                    debit: Decimal::ZERO,
                    credit: Decimal::from(3000),
                    department: None,
                    memo: None,
                },
            ],
            is_reversing: false,
            created_by: "admin".to_string(),
            created_at: chrono::Utc::now(),
        };

        engine.post_entry(jan_entry).unwrap();
        engine.post_entry(feb_entry).unwrap();

        // Trial balance for January only
        let tb_jan = engine.generate_trial_balance(
            NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
            NaiveDate::from_ymd_opt(2025, 1, 31).unwrap(),
        );

        let cash_balance = tb_jan.accounts.iter()
            .find(|a| a.account_number == "1000")
            .unwrap()
            .balance;

        assert_eq!(cash_balance, Decimal::from(5000));
    }

    // ========================================================================
    // Account Balance Tests
    // ========================================================================

    #[test]
    fn test_account_balance_calculation() {
        let mut engine = AccountingEngine::new();
        setup_basic_accounts(&mut engine);

        engine.post_entry(create_balanced_entry("JE001", 10000)).unwrap();
        engine.post_entry(create_balanced_entry("JE002", 5000)).unwrap();

        let balance = engine.get_account_balance(
            "1000",
            NaiveDate::from_ymd_opt(2025, 1, 31).unwrap(),
        );

        assert_eq!(balance, Some(Decimal::from(15000)));
    }

    #[test]
    fn test_account_balance_as_of_date() {
        let mut engine = AccountingEngine::new();
        setup_basic_accounts(&mut engine);

        // Entry on Jan 15
        let entry1 = JournalEntry {
            entry_number: "JE001".to_string(),
            entry_date: NaiveDate::from_ymd_opt(2025, 1, 15).unwrap(),
            description: "First entry".to_string(),
            lines: vec![
                JournalLine {
                    account: "1000".to_string(),
                    debit: Decimal::from(10000),
                    credit: Decimal::ZERO,
                    department: None,
                    memo: None,
                },
                JournalLine {
                    account: "3000".to_string(),
                    debit: Decimal::ZERO,
                    credit: Decimal::from(10000),
                    department: None,
                    memo: None,
                },
            ],
            is_reversing: false,
            created_by: "admin".to_string(),
            created_at: chrono::Utc::now(),
        };

        // Entry on Jan 25
        let entry2 = JournalEntry {
            entry_number: "JE002".to_string(),
            entry_date: NaiveDate::from_ymd_opt(2025, 1, 25).unwrap(),
            description: "Second entry".to_string(),
            lines: vec![
                JournalLine {
                    account: "1000".to_string(),
                    debit: Decimal::from(5000),
                    credit: Decimal::ZERO,
                    department: None,
                    memo: None,
                },
                JournalLine {
                    account: "3000".to_string(),
                    debit: Decimal::ZERO,
                    credit: Decimal::from(5000),
                    department: None,
                    memo: None,
                },
            ],
            is_reversing: false,
            created_by: "admin".to_string(),
            created_at: chrono::Utc::now(),
        };

        engine.post_entry(entry1).unwrap();
        engine.post_entry(entry2).unwrap();

        // Balance as of Jan 20 (only first entry)
        let balance_jan20 = engine.get_account_balance(
            "1000",
            NaiveDate::from_ymd_opt(2025, 1, 20).unwrap(),
        );
        assert_eq!(balance_jan20, Some(Decimal::from(10000)));

        // Balance as of Jan 31 (both entries)
        let balance_jan31 = engine.get_account_balance(
            "1000",
            NaiveDate::from_ymd_opt(2025, 1, 31).unwrap(),
        );
        assert_eq!(balance_jan31, Some(Decimal::from(15000)));
    }

    // ========================================================================
    // Multi-Currency Tests
    // ========================================================================

    #[test]
    fn test_multi_currency_entry() {
        // Test posting entries in different currencies
        // (Implementation would depend on Currency support in engine)
    }

    // ========================================================================
    // Period Close Tests
    // ========================================================================

    #[test]
    fn test_period_close_validation() {
        // Test that period close prevents posting to closed periods
        // (Implementation would depend on Period support in engine)
    }

    // ========================================================================
    // Audit Trail Tests
    // ========================================================================

    #[test]
    fn test_audit_trail_creation() {
        let mut engine = AccountingEngine::new();
        setup_basic_accounts(&mut engine);

        let entry = create_balanced_entry("JE001", 10000);
        engine.post_entry(entry.clone()).unwrap();

        // Verify entry is recorded
        assert_eq!(engine.entries.len(), 1);
        assert_eq!(engine.entries[0].entry_number, "JE001");
        assert_eq!(engine.entries[0].created_by, "admin");
    }

    // ========================================================================
    // Helper Functions
    // ========================================================================

    fn setup_basic_accounts(engine: &mut AccountingEngine) {
        engine.add_account(Account {
            number: "1000".to_string(),
            name: "Cash".to_string(),
            account_type: AccountType::Asset,
            parent: None,
            is_active: true,
            requires_department: false,
        }).unwrap();

        engine.add_account(Account {
            number: "3000".to_string(),
            name: "Common Stock".to_string(),
            account_type: AccountType::Equity,
            parent: None,
            is_active: true,
            requires_department: false,
        }).unwrap();
    }

    fn setup_comprehensive_accounts(engine: &mut AccountingEngine) {
        setup_basic_accounts(engine);

        engine.add_account(Account {
            number: "1200".to_string(),
            name: "Accounts Receivable".to_string(),
            account_type: AccountType::Asset,
            parent: None,
            is_active: true,
            requires_department: false,
        }).unwrap();

        engine.add_account(Account {
            number: "4000".to_string(),
            name: "Sales Revenue".to_string(),
            account_type: AccountType::Revenue,
            parent: None,
            is_active: true,
            requires_department: false,
        }).unwrap();
    }

    fn create_balanced_entry(entry_number: &str, amount: i64) -> JournalEntry {
        JournalEntry {
            entry_number: entry_number.to_string(),
            entry_date: NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
            description: format!("Test entry {}", entry_number),
            lines: vec![
                JournalLine {
                    account: "1000".to_string(),
                    debit: Decimal::from(amount),
                    credit: Decimal::ZERO,
                    department: None,
                    memo: None,
                },
                JournalLine {
                    account: "3000".to_string(),
                    debit: Decimal::ZERO,
                    credit: Decimal::from(amount),
                    department: None,
                    memo: None,
                },
            ],
            is_reversing: false,
            created_by: "admin".to_string(),
            created_at: chrono::Utc::now(),
        }
    }
}
