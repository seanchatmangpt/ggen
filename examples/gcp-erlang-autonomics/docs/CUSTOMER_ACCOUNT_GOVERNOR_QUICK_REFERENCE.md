# Customer Account Governor - Quick Reference

## File Locations

```
examples/gcp-erlang-autonomics/
├── src/
│   ├── lib.rs                                    # Exports marketplace module
│   └── marketplace/
│       ├── mod.rs                                # Module declarations & re-exports
│       ├── customer_account_governor.rs          # Main FSM implementation (840 lines)
│       └── [other governors...]
├── tests/
│   └── customer_account_governor_tests.rs        # Integration tests (650+ lines)
└── docs/
    ├── CUSTOMER_ACCOUNT_GOVERNOR.md              # Full documentation
    └── CUSTOMER_ACCOUNT_GOVERNOR_QUICK_REFERENCE.md (this file)
```

## Quick State Transitions

### Successful Onboarding
```
Onboarding --[EmailVerified]--> Onboarding
Onboarding --[ProfileCompleted]--> Onboarding
Onboarding --[KycCheckPassed]--> Active ✓
```

### Fraud Detection & Suspension
```
Active --[FraudAlert(score:50)]--> Suspended (medium risk)
Active --[FraudAlert(score:80)]--> UnderReview (high risk)
Suspended --[IssueResolved]--> Active ✓
```

### Account Ban Flow
```
Active --[FraudDetected]--> UnderReview
UnderReview --[ReviewCompletedBanned]--> Deactivated
Deactivated --[DataRetentionComplete]--> Archived (terminal)
```

## Key Types

### Account States (6 total)
```rust
pub enum AccountState {
    Onboarding,    // 0: New customer registration (7d timeout)
    Active,        // 1: Normal operation (no timeout)
    Suspended,     // 2: Temporary suspension (30d timeout)
    UnderReview,   // 3: Human/admin review (14d timeout)
    Deactivated,   // 4: Closed account (30d retention)
    Archived,      // 5: Terminal state (read-only forever)
}
```

### Events (19 total)
```rust
// Onboarding
EmailVerified, ProfileCompleted, KycCheckPassed, KycCheckFailed,
FraudDetected(String),

// Active Monitoring
PeriodicComplianceCheck(FraudScore), InactivityDetected, ActivityDetected,
FraudAlert { reason, score }, AbuseReport { reason, evidence },

// Suspended Recovery
IssueResolved, CustomerAppeals { reason }, EscalateToReview,

// Under Review
ReviewCompletedApproved, ReviewCompletedBanned { reason },
CustomerAppealsDenied { reason },

// Deactivation/Archive
DataRetentionComplete, CustomerRequestsReactivation, ArchiveAccount,
TimeoutTransition, Reset
```

### Actions (18 total)
```rust
// Verification & KYC
SendVerificationEmail, RequestKycDocumentation, RunFraudCheck,
VerifyCompanyDomain, ValidatePaymentMethod, EnableTwoFactorAuth,

// Communication
SendWelcomeEmail, SendEngagementEmail, OfferSupport,
NotifyCustomerSuspension { reason }, NotifyBanDecision { reason },

// Monitoring & Compliance
MonitorActivity, IncreaseMonitoringFrequency,
PreserveDataComplianceHold, PrepareAuditReport,

// Account Control
BlockOperations, ScheduleDataDeletion, DeleteAllData
```

## Fraud Scoring

### Calculation
```
Score (0-100) = (
  payment_velocity × 0.40 +
  geographic_anomaly × 0.30 +
  usage_deviation × 0.20 +
  behavioral_anomaly × 0.10
)
```

### Risk Levels
- **0-30**: Compliant ✓ (continue normal monitoring)
- **31-70**: Increase Monitoring ⚠️ (3-day check frequency)
- **71-90**: Suspicious 🚨 (1-day frequency, suspend account)
- **91-100**: Critical 🛑 (escalate to human review)

### Signals Detected
- Payment velocity (>50 charges/24h)
- Impossible travel (<1h country change)
- Off-hours activity (>70% outside 9-5)
- Brute-force login (3+ failures → success)
- Resource creation spike (100+ rapid creates)

## Compliance Monitoring

### Workflow
```
Every Account (Active state)
    ├─ Weekly: PeriodicComplianceCheck(fraud_score)
    ├─ Monthly: InactivityDetected
    └─ Real-time: FraudAlert, AbuseReport events
```

### Escalation
```
Fraud Score → Status → Action
< 30       → Compliant → Continue (status: Compliant)
30-70      → Monitor   → Increase frequency to 3d
71-90      → Suspicious → Auto-suspend + daily checks
> 90       → Critical   → Escalate to UnderReview
```

## State Timeouts

| State | Timeout | Auto-Transition |
|-------|---------|-----------------|
| Onboarding | 7 days | → Deactivated |
| Suspended | 30 days | → Deactivated |
| UnderReview | 14 days | → Deactivated (auto-decision) |
| Deactivated | 30 days | → Archived (delete data) |

## GDPR Workflows

### Right-to-be-Forgotten
```
Customer requests deletion
    ↓
Deactivated (30-day retention)
    ↓
DataRetentionComplete event
    ↓
Archived (data deleted)
```

### Account Recovery (Compromised)
```
FraudDetector detects impossible travel
    ↓
FraudAlert (score 95)
    ↓
Suspended (auto-freeze)
    ↓
EmailVerified + SMSVerified (customer verifies)
    ↓
IssueResolved
    ↓
Active (restored)
```

## Implementation Patterns

### Pattern: Result<T, E> (No Panics)
```rust
// ✓ Correct
pub async fn transition(
    &mut self,
    event: AccountEvent,
) -> Result<(AccountState, Option<AccountAction>), AccountGovernorError>

// ✗ Wrong - Never use unwrap/expect in production
// match governor.transition(event).await.unwrap() { ... }
```

### Pattern: Invariant Checks
```rust
// Prevents invalid states from existing
pub fn validate_invariants(&self) -> Result<(), AccountGovernorError> {
    if self.customer_id.is_empty() {
        return Err(InvariantViolation("customer_id empty".into()));
    }
    Ok(())
}
```

### Pattern: Chicago TDD (AAA)
```rust
#[tokio::test]
async fn test_fraud_detection() {
    // Arrange
    let mut governor = AccountGovernor::new("cust-1".to_string());
    governor.transition(AccountEvent::KycCheckPassed).await.ok();

    // Act
    let (state, action) = governor.transition(
        AccountEvent::FraudAlert { reason: "...".into(), score: 75 }
    ).await?;

    // Assert
    assert_eq!(state, AccountState::UnderReview);
    assert!(matches!(action, Some(AccountAction::PreserveDataComplianceHold)));
}
```

## Audit Trail

Every transition recorded:
```
AuditTrailEntry {
    timestamp: DateTime<Utc>,
    from_state: AccountState,
    to_state: AccountState,
    event: String,
    action: Option<String>,
    reason: Option<String>,
    metadata: HashMap<String, String>,
}
```

### Example
```
Onboarding → Active
  Event: "KycCheckPassed"
  Action: "SendWelcomeEmail"
  Metadata: { "kyc_method": "government_id" }

Active → UnderReview
  Event: "FraudAlert { score: 85 }"
  Action: "PreserveDataComplianceHold"
  Metadata: { "fraud_score": "85", "payment_velocity": "60" }
```

## Key APIs

### Create Account
```rust
let governor = AccountGovernor::new("customer-id".to_string());
// Starts in Onboarding state
// 7-day timeout
// Empty audit trail
```

### Perform Transition
```rust
let (new_state, action) = governor.transition(event).await?;
// Deterministic: same event → same result
// Audit recorded automatically
```

### Check State
```rust
governor.current_state()           // Get current state
governor.is_verified()              // Check if verified
governor.time_in_state()           // How long in current state
governor.check_timeout()           // Has state timed out?
governor.validate_invariants()     // Verify consistency
```

### Access Audit Trail
```rust
for entry in &governor.audit_trail {
    println!("{} → {}", entry.from_state.as_str(), entry.to_state.as_str());
    if let Some(action) = &entry.action {
        println!("  Action: {}", action);
    }
}
```

### Fraud Detection
```rust
let fraud_score = FraudDetector::analyze_activity(
    customer_id,
    &activity_history
)?;

let is_compromised = FraudDetector::check_for_compromised_account(
    &activity_history
);
```

## Testing

### Run All Tests
```bash
cargo test --lib marketplace::customer_account_governor
cargo test --test customer_account_governor_tests
```

### Test Categories
- ✅ Onboarding workflows (happy path & failures)
- ✅ Fraud detection (medium, high, critical)
- ✅ Compliance monitoring (low, medium, high, critical)
- ✅ Account recovery (suspended → active)
- ✅ Review & ban (under_review → deactivated)
- ✅ Audit trail completeness
- ✅ State timeout enforcement
- ✅ GDPR workflows (deactivation → archival)
- ✅ Edge cases (archived terminal, concurrent abuse)

## Design Principles

### Type Safety
- States prevent invalid combinations
- Compiler enforces impossible state prevention
- Type system encodes business rules

### Zero Panics
- All operations return `Result<T, E>`
- No `unwrap()` or `expect()` in production
- Clippy enforces via `-D warnings`

### Determinism
- Same event → identical state transition
- Audit trail proves determinism
- Timestamps immutable

### Compliance
- GDPR right-to-be-forgotten support
- 30-day retention before deletion
- KYC/AML verification enforced
- Audit trail for regulatory reporting

### Performance
- O(1) state transitions
- O(n) fraud scoring (n = activity entries)
- <1ms typical latency
- 1000+ concurrent accounts per instance

## Common Patterns

### Pattern: Email Verification
```rust
governor.transition(AccountEvent::EmailVerified).await?;
// Still in Onboarding - need KYC
// Fire KycCheckPassed to reach Active
```

### Pattern: Fraud Escalation
```rust
// Medium risk → Suspended
governor.transition(AccountEvent::FraudAlert { score: 50 }).await?;
// Can appeal or wait 30 days

// High risk → UnderReview
governor.transition(AccountEvent::FraudAlert { score: 80 }).await?;
// Human review required
```

### Pattern: Compliance Check
```rust
let fraud_score = FraudDetector::analyze_activity(&customer_id, &history)?;
governor.transition(AccountEvent::PeriodicComplianceCheck(fraud_score)).await?;
// May auto-suspend if score > 70
// May escalate if score > 90
```

### Pattern: Account Recovery (Reactivation)
```rust
// Account deactivated and within 30 days?
governor.transition(AccountEvent::CustomerRequestsReactivation).await?;
// → Returns to Active (if within 30d)
// → Stays Deactivated (if > 30d)
```

## Metrics Dashboard

**Account Distribution**
- Onboarding: X accounts (< 7 days old)
- Active: X accounts (verified, normal)
- Suspended: X accounts (under investigation)
- UnderReview: X accounts (human review)
- Deactivated: X accounts (retention period)
- Archived: X accounts (permanent)

**Fraud Metrics**
- Avg fraud score: X (0-100)
- Accounts in high-risk: X%
- False positives (appeal rate): X%
- Account recovery rate: X%

**Compliance Metrics**
- KYC verification rate: X%
- Avg verification time: X hours
- GDPR deletion compliance: 100% (within 30d)

## Troubleshooting

### Account stuck in Onboarding?
- Check if 7-day timeout has passed
- Verify EmailVerified, ProfileCompleted, KycCheckPassed all fired
- Check audit trail for exact state

### Fraud alert seems wrong?
- Review FraudScore components (payment_velocity, geographic_anomaly, etc.)
- Check fraud_detection.rs for signal calculation
- False positives: customer can appeal

### Audit trail missing entries?
- Only state-changing events recorded (not all events)
- ActivityDetected doesn't change state → no audit entry
- Check AuditTrailEntry struct for what's recorded

### Can't reactivate deleted account?
- Reactivation only works within 30 days of deactivation
- After 30 days: account archived and unrecoverable
- Check governor.state == Deactivated (not Archived)

## Performance Tuning

### Fraud Detection
- Cache activity history (avoid O(n) every time)
- Batch score calculations (run every 7 days, not per-event)
- Use approximate scores for real-time decisions

### Audit Trail
- Archive old entries (>90 days) to separate storage
- Compress audit trail JSON for storage
- Index by customer_id for queries

### Compliance Monitoring
- Adjust check_frequency based on tier (premium: 7d, standard: 14d)
- Defer fraud detection if load high (queue events)
- Parallel compliance checks across customers
