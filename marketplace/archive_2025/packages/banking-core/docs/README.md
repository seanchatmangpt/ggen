# Banking Core Package

## Overview

Core banking system with account management, transaction processing, interest calculation, ledger system, and comprehensive regulatory reporting.

## Features

### Account Management
- **Account Types**: Checking, Savings, Loan, Credit Card, Investment
- **Multi-Currency**: Support for 50+ currencies
- **Joint Accounts**: Multiple account holders
- **Minor Accounts**: UTMA/UGMA custodial accounts
- **Account Linking**: Transfer between linked accounts

### Transaction Processing
- **ACH Transfers**: NACHA-compliant automated clearing
- **Wire Transfers**: Domestic and international
- **Check Processing**: Check 21 image processing
- **Card Payments**: Debit and credit card transactions
- **P2P Transfers**: Instant peer-to-peer payments

### Interest Calculation
- **Simple Interest**: I = P × r × t
- **Compound Interest**: A = P(1 + r/n)^(nt)
- **APY Calculation**: Annual percentage yield
- **Tiered Rates**: Balance-based interest tiers
- **Daily Accrual**: Precise interest calculation

### Ledger System
- **Double-Entry Bookkeeping**: Every debit has credit
- **General Ledger**: Chart of accounts
- **Sub-Ledgers**: Detailed transaction records
- **Real-Time Posting**: Immediate ledger updates
- **Reconciliation**: Daily account reconciliation

## Regulatory Compliance

### Federal Regulations
- **FDIC Insurance**: Deposit insurance compliance
- **Regulation D**: Savings withdrawal limits
- **Regulation E**: Electronic fund transfer rules
- **Regulation Z**: Truth in lending
- **BSA/AML**: Anti-money laundering program
- **OFAC**: Sanctions screening
- **ECOA**: Equal Credit Opportunity Act

### Reporting Requirements
- **Call Reports**: Quarterly FFIEC reporting
- **CTR Filing**: Currency transactions >$10,000
- **SAR Filing**: Suspicious activity reports
- **CIP/KYC**: Customer identification program
- **Audit Trail**: Complete transaction history

### Standards Compliance
- **ISO 20022**: Financial messaging standard
- **NACHA**: ACH network rules
- **SWIFT**: International payment messaging
- **PCI-DSS**: Payment card security

## API Reference

### Account Management

```typescript
POST /api/v1/accounts
{
  "customerId": "string",
  "accountType": "checking" | "savings" | "loan",
  "currency": "USD",
  "initialDeposit": 1000.00
}

GET /api/v1/accounts/{accountNumber}
Response: {
  "accountNumber": "1234567890",
  "accountType": "checking",
  "balance": 5432.10,
  "availableBalance": 5432.10,
  "status": "active"
}
```

### Transaction Processing

```typescript
POST /api/v1/transactions/ach
{
  "fromAccount": "1234567890",
  "toAccount": "0987654321",
  "routingNumber": "123456789",
  "amount": 500.00,
  "description": "Transfer"
}

GET /api/v1/accounts/{accountNumber}/transactions
Query: {
  "startDate": "2025-01-01",
  "endDate": "2025-01-31",
  "type": "all" | "debit" | "credit"
}
```

### Interest Calculation

```typescript
POST /api/v1/accounts/{accountNumber}/interest/calculate
{
  "calculationDate": "2025-01-31"
}

Response: {
  "balance": 10000.00,
  "interestRate": 2.5,
  "apyRate": 2.528,
  "accruedInterest": 20.83,
  "compoundingFrequency": "daily"
}
```

## SPARQL Queries

15 pre-built templates for:
- Account summaries
- Transaction history
- ACH transfer processing
- Interest accrual
- Limit monitoring
- Regulatory reporting (Call Report, CTR, SAR)
- OFAC screening
- Audit trail

## Interest Calculation Formulas

### Simple Interest
```
I = P × r × t
where:
  P = Principal
  r = Annual interest rate
  t = Time in years
```

### Compound Interest
```
A = P(1 + r/n)^(nt)
where:
  n = Compounding frequency
  t = Time in years
```

### APY (Annual Percentage Yield)
```
APY = (1 + r/n)^n - 1
```

### Daily Accrual
```
Daily Interest = Balance × (APR / 365)
```

## Implementation Examples

### Rust
```rust
use banking_core::*;

let mut account = Account::new(
    "1234567890",
    AccountType::Savings,
    1000.00
)?;

account.deposit(500.00)?;
account.earn_interest(InterestRate::new(2.5, Daily))?;
```

### TypeScript
```typescript
import { Account, Transaction } from '@ggen/banking-core';

const account = new Account('1234567890', 'checking');
await account.deposit(1000);
const balance = await account.getBalance();
```

### Python
```python
from banking_core import Account, InterestCalculator

account = Account('1234567890', 'savings', 10000)
calculator = InterestCalculator(account, rate=2.5, compounding='daily')
interest = calculator.calculate_accrued()
```

## Transaction Lifecycle

1. **Initiation**: User submits transaction
2. **Validation**: Verify funds, limits, compliance
3. **Authorization**: Approve transaction
4. **Posting**: Update account balances
5. **Ledger Entry**: Record double-entry
6. **Reconciliation**: Verify balance accuracy
7. **Reporting**: Regulatory reporting if required

## Security Measures

### Transaction Security
- **Fraud Detection**: ML-based anomaly detection
- **Velocity Checks**: Transaction frequency limits
- **Geolocation**: Location-based verification
- **Device Fingerprinting**: Known device tracking

### Access Controls
- **Multi-Factor Authentication**: SMS, email, authenticator
- **Role-Based Access**: Granular permissions
- **Session Management**: Timeout and encryption
- **Audit Logging**: All access logged

### Data Protection
- **Encryption at Rest**: AES-256
- **Encryption in Transit**: TLS 1.3
- **PII Masking**: Sensitive data protection
- **Secure Deletion**: NIST 800-88 guidelines

## Testing

Chicago TDD test suite:
- Unit tests (accounts, transactions)
- Integration tests (end-to-end flows)
- Security tests (fraud, limits)
- Compliance tests (regulatory rules)
- Performance tests (high volume)

## Regulatory Reporting

### Call Report (FFIEC 031/041)
- **Schedule RC**: Balance sheet
- **Schedule RI**: Income statement
- **Schedule RC-R**: Regulatory capital
- **Due Date**: 30 days after quarter-end

### CTR (Currency Transaction Report)
- **Trigger**: Cash transactions >$10,000
- **Filing**: Within 15 days
- **Form**: FinCEN Form 112

### SAR (Suspicious Activity Report)
- **Trigger**: Suspicious patterns detected
- **Filing**: Within 30 days of detection
- **Form**: FinCEN Form 111

## Best Practices

1. **Daily Reconciliation**: Verify all account balances
2. **Real-Time Monitoring**: Fraud and compliance alerts
3. **Backup and Recovery**: 15-minute RPO/RTO
4. **Change Management**: Strict deployment controls
5. **Vendor Management**: Third-party risk assessment

## License

MIT License
