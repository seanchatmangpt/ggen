// ============================================================================
// Enterprise ERP Core - Accounting Engine (Rust)
// Double-entry accounting with validation and audit trail
// ============================================================================

use std::collections::HashMap;
use chrono::{DateTime, Utc, NaiveDate};
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

// ----------------------------------------------------------------------------
// Core Data Structures
// ----------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum AccountType {
    Asset,
    Liability,
    Equity,
    Revenue,
    Expense,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Account {
    pub number: String,
    pub name: String,
    pub account_type: AccountType,
    pub parent: Option<String>,
    pub is_active: bool,
    pub requires_department: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JournalLine {
    pub account: String,
    pub debit: Decimal,
    pub credit: Decimal,
    pub department: Option<String>,
    pub memo: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JournalEntry {
    pub entry_number: String,
    pub entry_date: NaiveDate,
    pub description: String,
    pub lines: Vec<JournalLine>,
    pub is_reversing: bool,
    pub created_by: String,
    pub created_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrialBalance {
    pub as_of_date: NaiveDate,
    pub accounts: Vec<TrialBalanceAccount>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrialBalanceAccount {
    pub account_number: String,
    pub account_name: String,
    pub debit_total: Decimal,
    pub credit_total: Decimal,
    pub balance: Decimal,
}

// ----------------------------------------------------------------------------
// Accounting Engine
// ----------------------------------------------------------------------------

pub struct AccountingEngine {
    accounts: HashMap<String, Account>,
    entries: Vec<JournalEntry>,
}

impl AccountingEngine {
    pub fn new() -> Self {
        Self {
            accounts: HashMap::new(),
            entries: Vec::new(),
        }
    }

    /// Add an account to the chart of accounts
    pub fn add_account(&mut self, account: Account) -> Result<(), String> {
        if account.number.is_empty() {
            return Err("Account number cannot be empty".to_string());
        }
        if account.name.is_empty() {
            return Err("Account name cannot be empty".to_string());
        }

        self.accounts.insert(account.number.clone(), account);
        Ok(())
    }

    /// Post a journal entry with double-entry validation
    pub fn post_entry(&mut self, entry: JournalEntry) -> Result<(), String> {
        // Validate all accounts exist
        for line in &entry.lines {
            if !self.accounts.contains_key(&line.account) {
                return Err(format!("Account {} does not exist", line.account));
            }
        }

        // Validate double-entry balancing
        let total_debits: Decimal = entry.lines.iter().map(|l| l.debit).sum();
        let total_credits: Decimal = entry.lines.iter().map(|l| l.credit).sum();

        if total_debits != total_credits {
            return Err(format!(
                "Journal entry does not balance: debits={}, credits={}",
                total_debits, total_credits
            ));
        }

        // Validate department requirements
        for line in &entry.lines {
            if let Some(account) = self.accounts.get(&line.account) {
                if account.requires_department && line.department.is_none() {
                    return Err(format!(
                        "Account {} requires a department",
                        line.account
                    ));
                }
            }
        }

        self.entries.push(entry);
        Ok(())
    }

    /// Generate trial balance for a date range
    pub fn generate_trial_balance(
        &self,
        start_date: NaiveDate,
        end_date: NaiveDate,
    ) -> TrialBalance {
        let mut balances: HashMap<String, (Decimal, Decimal)> = HashMap::new();

        // Sum debits and credits for each account
        for entry in &self.entries {
            if entry.entry_date >= start_date && entry.entry_date <= end_date {
                for line in &entry.lines {
                    let (debit_total, credit_total) = balances
                        .entry(line.account.clone())
                        .or_insert((Decimal::ZERO, Decimal::ZERO));
                    *debit_total += line.debit;
                    *credit_total += line.credit;
                }
            }
        }

        // Build trial balance accounts
        let mut accounts: Vec<TrialBalanceAccount> = balances
            .into_iter()
            .filter_map(|(account_number, (debit_total, credit_total))| {
                self.accounts.get(&account_number).map(|account| {
                    TrialBalanceAccount {
                        account_number: account_number.clone(),
                        account_name: account.name.clone(),
                        debit_total,
                        credit_total,
                        balance: debit_total - credit_total,
                    }
                })
            })
            .collect();

        accounts.sort_by(|a, b| a.account_number.cmp(&b.account_number));

        TrialBalance {
            as_of_date: end_date,
            accounts,
        }
    }

    /// Validate double-entry for all journal entries
    pub fn validate_all_entries(&self) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        for entry in &self.entries {
            let total_debits: Decimal = entry.lines.iter().map(|l| l.debit).sum();
            let total_credits: Decimal = entry.lines.iter().map(|l| l.credit).sum();

            if total_debits != total_credits {
                errors.push(format!(
                    "Entry {} does not balance: debits={}, credits={}",
                    entry.entry_number, total_debits, total_credits
                ));
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Get account balance at a specific date
    pub fn get_account_balance(
        &self,
        account_number: &str,
        as_of_date: NaiveDate,
    ) -> Option<Decimal> {
        self.accounts.get(account_number)?;

        let mut balance = Decimal::ZERO;

        for entry in &self.entries {
            if entry.entry_date <= as_of_date {
                for line in &entry.lines {
                    if line.account == account_number {
                        balance += line.debit - line.credit;
                    }
                }
            }
        }

        Some(balance)
    }
}

// ----------------------------------------------------------------------------
// Tests
// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_engine() -> AccountingEngine {
        let mut engine = AccountingEngine::new();

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

        engine
    }

    #[test]
    fn test_double_entry_validation() {
        let mut engine = create_test_engine();

        let entry = JournalEntry {
            entry_number: "JE001".to_string(),
            entry_date: NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
            description: "Initial investment".to_string(),
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
            created_at: Utc::now(),
        };

        assert!(engine.post_entry(entry).is_ok());
    }

    #[test]
    fn test_unbalanced_entry_rejected() {
        let mut engine = create_test_engine();

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
                    credit: Decimal::from(3000),
                    department: None,
                    memo: None,
                },
            ],
            is_reversing: false,
            created_by: "admin".to_string(),
            created_at: Utc::now(),
        };

        assert!(engine.post_entry(entry).is_err());
    }

    #[test]
    fn test_trial_balance() {
        let mut engine = create_test_engine();

        engine.post_entry(JournalEntry {
            entry_number: "JE001".to_string(),
            entry_date: NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
            description: "Test entry".to_string(),
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
            created_at: Utc::now(),
        }).unwrap();

        let tb = engine.generate_trial_balance(
            NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
            NaiveDate::from_ymd_opt(2025, 1, 31).unwrap(),
        );

        assert_eq!(tb.accounts.len(), 2);
    }
}
