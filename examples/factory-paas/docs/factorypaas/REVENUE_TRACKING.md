# FactoryPaaS Revenue Tracking Guide

**Version**: 1.0.0
**Last Updated**: 2026-01-24
**Audience**: Publishers, Operators, Financial Teams

---

## 💰 Revenue Model Overview

FactoryPaaS uses a **70/30 revenue share model** where publishers (affiliates) earn 70% of conversion revenue and the platform retains 30% as a service fee.

### Key Principles

1. **Transparent Attribution**: Every click tracked with cryptographic receipts
2. **Last-Click Model**: Most recent click within attribution window receives credit
3. **30-Day Window**: Clicks older than 30 days are not attributed
4. **Minimum Payout**: $10 threshold for payouts
5. **Weekly Settlements**: Payouts processed every Monday

---

## 📊 Revenue Tracking Examples

### Example 1: Basic Click-to-Conversion Flow

**Scenario**: Publisher promotes VPN service with $50 payout per sale.

```
Day 1 (Monday, Jan 1):
  User clicks affiliate link
    Publisher: pub-456
    Offer: VPN-789 ($50 payout)
    Receipt: ClickRecorded {
      "click_id": "click-123",
      "publisher_id": "pub-456",
      "offer_id": "VPN-789",
      "timestamp": "2026-01-01T10:30:00Z",
      "ip_hash": "sha256:abc123...",
      "signature": "ed25519:def456..."
    }

Day 3 (Wednesday, Jan 3):
  User purchases VPN subscription
    Advertiser sends webhook with conversion
    Receipt: AttributionComputed {
      "click_id": "click-123",
      "publisher_id": "pub-456",
      "amount": "$50.00",
      "computed_at": "2026-01-03T14:22:15Z",
      "signature": "ed25519:ghi789..."
    }

Day 8 (Monday, Jan 8):
  Weekly payout calculation
    Total attributed revenue: $50.00
    Platform fee (30%): $15.00
    Publisher payout (70%): $35.00
    
    Receipt: PayoutCalculated {
      "publisher_id": "pub-456",
      "gross_revenue": "$50.00",
      "platform_fee": "$15.00",
      "net_payout": "$35.00",
      "period_start": "2026-01-01T00:00:00Z",
      "period_end": "2026-01-07T23:59:59Z",
      "signature": "ed25519:jkl012..."
    }

    Stripe transfer initiated: $35.00
    Email notification sent to publisher
```

**Revenue Breakdown**:
```
Advertiser pays:      $50.00
FactoryPaaS retains:  $15.00 (30%)
Publisher receives:   $35.00 (70%)
```

---

### Example 2: Multiple Conversions, Same Publisher

**Scenario**: Publisher has 5 conversions in a week, different offers.

```
Week of Jan 1-7, 2026:
  Monday:    VPN offer ($50 payout) → attributed → $35.00 publisher payout
  Tuesday:   Antivirus ($30 payout) → attributed → $21.00 publisher payout
  Wednesday: Password Manager ($20 payout) → attributed → $14.00 publisher payout
  Friday:    VPN offer ($50 payout) → attributed → $35.00 publisher payout
  Sunday:    VPN offer ($50 payout) → attributed → $35.00 publisher payout

Total gross revenue:    $200.00
Total platform fee:     $60.00 (30%)
Total publisher payout: $140.00 (70%)
```

**Payout Receipt**:
```json
{
  "publisher_id": "pub-456",
  "period": "2026-W01",
  "conversions": [
    {"offer": "VPN-789", "amount": "$50.00", "payout": "$35.00"},
    {"offer": "AV-234", "amount": "$30.00", "payout": "$21.00"},
    {"offer": "PM-567", "amount": "$20.00", "payout": "$14.00"},
    {"offer": "VPN-789", "amount": "$50.00", "payout": "$35.00"},
    {"offer": "VPN-789", "amount": "$50.00", "payout": "$35.00"}
  ],
  "gross_revenue": "$200.00",
  "platform_fee": "$60.00",
  "net_payout": "$140.00",
  "stripe_transfer_id": "tr_1234567890",
  "signature": "ed25519:mno345..."
}
```

---

### Example 3: Attribution Window Edge Case

**Scenario**: Click happens 29 days before conversion (within window) vs 31 days (outside window).

```
Case A: Within Window (✓ Attributed)
  Click:      Jan 1, 10:00 AM
  Conversion: Jan 30, 2:00 PM  (29 days, 4 hours later)
  Result:     ✓ Attributed (within 30-day window)
  Publisher receives: $35.00 (70% of $50)

Case B: Outside Window (✗ Not Attributed)
  Click:      Jan 1, 10:00 AM
  Conversion: Feb 1, 2:00 PM  (31 days, 4 hours later)
  Result:     ✗ NOT attributed (exceeds 30-day window)
  Publisher receives: $0.00
  Platform retains: $0.00 (conversion not tracked)
```

**Receipt for Case B**:
```json
{
  "type": "AttributionRejected",
  "click_id": "click-456",
  "reason": "attribution_window_exceeded",
  "window_days": 30,
  "days_since_click": 31.17,
  "computed_at": "2026-02-01T14:00:00Z",
  "signature": "ed25519:pqr678..."
}
```

---

### Example 4: Minimum Payout Threshold

**Scenario**: Publisher earns less than $10 minimum in a week.

```
Week of Jan 1-7:
  Wednesday: Password Manager ($20 payout) → $14.00 publisher payout
  (No other conversions)

Total gross revenue:    $20.00
Total platform fee:     $6.00
Total publisher payout: $14.00

Result: ✓ Payout processed (>= $10 minimum)
```

**Scenario 2**: Publisher earns less than $10 minimum.

```
Week of Jan 8-14:
  Friday: Small offer ($10 payout) → $7.00 publisher payout
  (No other conversions)

Total gross revenue:    $10.00
Total platform fee:     $3.00
Total publisher payout: $7.00

Result: ✗ Payout deferred (< $10 minimum)
Balance carries over to next week
```

**Receipt**:
```json
{
  "type": "PayoutDeferred",
  "publisher_id": "pub-456",
  "period": "2026-W02",
  "amount": "$7.00",
  "minimum_threshold": "$10.00",
  "carryover_balance": "$7.00",
  "next_payout_date": "2026-01-22",
  "signature": "ed25519:stu901..."
}
```

---

## 📈 Revenue Reporting

### Real-Time Dashboard

Publishers can view real-time revenue via API or web dashboard:

```bash
# Get current month revenue
curl -X GET "https://api.factorypaas.example.com/api/v1/publishers/$PUB_ID/revenue?month=2026-01" \
  -H "Authorization: Bearer $API_KEY"
```

**Response**:
```json
{
  "publisher_id": "pub-456",
  "period": "2026-01",
  "clicks": 12500,
  "attributed_clicks": 487,
  "conversions": 43,
  "revenue": {
    "gross": "$2,150.00",
    "platform_fee": "$645.00",
    "net": "$1,505.00"
  },
  "payouts": {
    "processed": "$1,505.00",
    "pending": "$0.00",
    "deferred": "$0.00"
  },
  "top_offers": [
    {"offer_id": "VPN-789", "conversions": 18, "revenue": "$900.00"},
    {"offer_id": "AV-234", "conversions": 12, "revenue": "$360.00"},
    {"offer_id": "PM-567", "conversions": 8, "revenue": "$160.00"}
  ],
  "receipts": {
    "total": 12543,
    "click_recorded": 12500,
    "attribution_computed": 43,
    "payout_calculated": 4
  }
}
```

### Historical Reports

```bash
# Get last 12 months revenue
curl -X GET "https://api.factorypaas.example.com/api/v1/publishers/$PUB_ID/revenue/history?months=12" \
  -H "Authorization: Bearer $API_KEY"
```

**Response**:
```json
{
  "publisher_id": "pub-456",
  "periods": [
    {"month": "2026-01", "clicks": 12500, "conversions": 43, "net_revenue": "$1,505.00"},
    {"month": "2025-12", "clicks": 11200, "conversions": 38, "net_revenue": "$1,330.00"},
    {"month": "2025-11", "clicks": 10800, "conversions": 35, "net_revenue": "$1,225.00"}
  ],
  "total_clicks": 147500,
  "total_conversions": 521,
  "total_revenue": "$18,255.00",
  "average_cpm": "$12.38",
  "average_conversion_rate": "0.35%"
}
```

---

## 🔍 Receipt Verification

### Verifying Individual Receipts

Publishers can independently verify receipts:

```bash
# Download receipt
curl -X GET "https://api.factorypaas.example.com/api/v1/receipts/$RECEIPT_ID" \
  -H "Authorization: Bearer $API_KEY" \
  > receipt.json

# Verify signature (using FactoryPaaS public key)
cat receipt.json | jq -r '.signature' | openssl dgst -sha256 -verify factorypaas_public_key.pem -signature -
```

**Expected output**:
```
Verified OK
```

### Verifying Receipt Chain

```bash
# Verify entire receipt chain for a period
curl -X POST "https://api.factorypaas.example.com/api/v1/receipts/verify-chain" \
  -H "Authorization: Bearer $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "publisher_id": "pub-456",
    "period_start": "2026-01-01T00:00:00Z",
    "period_end": "2026-01-31T23:59:59Z"
  }'
```

**Response**:
```json
{
  "verified": true,
  "receipts_checked": 12543,
  "signature_failures": 0,
  "hash_mismatches": 0,
  "merkle_root": "sha256:abc123def456...",
  "verification_timestamp": "2026-02-01T10:00:00Z",
  "signature": "ed25519:vwx234..."
}
```

---

## 💳 Payout Methods

### Supported Payment Methods

| Method | Fee | Processing Time | Minimum | Maximum |
|--------|-----|-----------------|---------|---------|
| **Stripe (ACH)** | $0 | 3-5 business days | $10 | $100,000/week |
| **PayPal** | 2.9% + $0.30 | 1-2 business days | $10 | $10,000/week |
| **Wire Transfer** | $25 | 1-3 business days | $1,000 | Unlimited |
| **Cryptocurrency (USDC)** | Network fees only | <1 hour | $10 | Unlimited |

### Setting Up Payout Method

```bash
# Configure Stripe payout (recommended)
curl -X POST "https://api.factorypaas.example.com/api/v1/publishers/$PUB_ID/payout-method" \
  -H "Authorization: Bearer $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "method": "stripe",
    "stripe_account_id": "acct_1234567890"
  }'
```

---

## 📊 Revenue Optimization

### Best Practices for Publishers

1. **Diversify Offers**: Promote multiple offers to reduce dependency on single advertiser
2. **Track Performance**: Monitor which offers have highest conversion rates
3. **Optimize Content**: Use AI-generated SEO content for better organic traffic
4. **A/B Testing**: Test different article formats and CTAs
5. **Seasonal Trends**: Focus on seasonal offers (VPNs in privacy month, antivirus in tax season)

### Performance Metrics

```
Key Metrics to Monitor:
  - Click-Through Rate (CTR): Clicks / Impressions
  - Conversion Rate: Conversions / Clicks
  - Earnings Per Click (EPC): Revenue / Clicks
  - Average Order Value (AOV): Revenue / Conversions
```

**Example Performance Report**:
```json
{
  "publisher_id": "pub-456",
  "period": "2026-01",
  "metrics": {
    "impressions": 125000,
    "clicks": 12500,
    "conversions": 43,
    "revenue": "$1,505.00",
    "ctr": "10.00%",
    "conversion_rate": "0.34%",
    "epc": "$0.12",
    "aov": "$50.00"
  },
  "top_performing_content": [
    {
      "url": "https://content.factorypaas.com/best-vpn-2026",
      "clicks": 3500,
      "conversions": 18,
      "revenue": "$630.00",
      "epc": "$0.18"
    },
    {
      "url": "https://content.factorypaas.com/antivirus-comparison",
      "clicks": 2800,
      "conversions": 12,
      "revenue": "$252.00",
      "epc": "$0.09"
    }
  ]
}
```

---

## 🧮 Financial Reporting

### Tax Reporting (US Publishers)

FactoryPaaS automatically generates 1099-MISC forms for US publishers earning $600+ annually.

```bash
# Download 1099-MISC
curl -X GET "https://api.factorypaas.example.com/api/v1/publishers/$PUB_ID/tax-docs/1099?year=2026" \
  -H "Authorization: Bearer $API_KEY" \
  > 1099-MISC-2026.pdf
```

**Tax Form Details**:
- **Box 3** (Other Income): Total annual revenue
- **Payer**: FactoryPaaS Inc., EIN: 12-3456789
- **Recipient**: Publisher information on file

### Invoice Generation

```bash
# Generate invoice for payout
curl -X GET "https://api.factorypaas.example.com/api/v1/payouts/$PAYOUT_ID/invoice" \
  -H "Authorization: Bearer $API_KEY" \
  > invoice.pdf
```

**Invoice includes**:
- Period covered
- List of all conversions with receipts
- Gross revenue, platform fee, net payout
- Payment method and transaction ID
- Cryptographic signature for verification

---

## 🚨 Dispute Resolution

### Disputing a Payout

If a publisher believes a payout is incorrect:

```bash
# Create dispute
curl -X POST "https://api.factorypaas.example.com/api/v1/payouts/$PAYOUT_ID/dispute" \
  -H "Authorization: Bearer $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "reason": "Missing conversions",
    "expected_amount": "$42.00",
    "actual_amount": "$35.00",
    "evidence": {
      "click_ids": ["click-123", "click-456"],
      "conversion_dates": ["2026-01-15", "2026-01-18"]
    }
  }'
```

**Dispute Resolution Process**:
1. System retrieves all receipts for disputed period
2. Recalculates payout from receipts
3. Compares against original payout
4. If discrepancy found, issues adjustment payout
5. All steps cryptographically signed in receipts

---

## 📞 Support

For revenue tracking questions:
- **Documentation**: https://docs.factorypaas.example.com/revenue
- **Support**: revenue@factorypaas.example.com
- **Billing**: billing@factorypaas.example.com

---

**Version**: 1.0.0
**Last Updated**: 2026-01-24
**Maintained by**: FactoryPaaS Revenue Team
