/// Banking Core Implementation
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Account {
    pub account_number: String,
    pub account_type: AccountType,
    pub balance: f64,
    pub available_balance: f64,
    pub status: AccountStatus,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AccountType {
    Checking,
    Savings,
    Loan,
    CreditCard,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AccountStatus {
    Active,
    Frozen,
    Closed,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Transaction {
    pub id: String,
    pub account_number: String,
    pub transaction_type: TransactionType,
    pub amount: f64,
    pub description: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TransactionType {
    Deposit,
    Withdrawal,
    Transfer,
    ACH,
    Wire,
}

pub struct InterestCalculator;

impl InterestCalculator {
    pub fn simple_interest(principal: f64, rate: f64, time: f64) -> f64 {
        principal * rate * time
    }

    pub fn compound_interest(principal: f64, rate: f64, time: f64, frequency: u32) -> f64 {
        principal * (1.0 + rate / frequency as f64).powf(frequency as f64 * time)
    }

    pub fn apy(rate: f64, frequency: u32) -> f64 {
        (1.0 + rate / frequency as f64).powf(frequency as f64) - 1.0
    }
}

pub struct Ledger {
    entries: Vec<LedgerEntry>,
}

#[derive(Debug, Clone)]
pub struct LedgerEntry {
    pub debit_account: String,
    pub credit_account: String,
    pub amount: f64,
}

impl Ledger {
    pub fn new() -> Self {
        Self { entries: Vec::new() }
    }

    pub fn post(&mut self, entry: LedgerEntry) {
        self.entries.push(entry);
    }
}
