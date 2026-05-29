# KYC/AML Compliance

**Complete KYC/AML compliance solution with identity verification, risk scoring, and sanctions screening**

## Features

- **RDF Ontology (280+ lines)**: Compliance domain model
- **SPARQL Templates (15+ queries)**: Risk assessment, sanctions matching, SAR generation
- **Rust Sanctions Screening**: High-performance fuzzy matching
- **TypeScript Onboarding UI**: Customer verification workflow
- **Python Transaction Monitoring**: Real-time alert generation

## Quick Start

### Rust - Sanctions Screening

```rust
let mut engine = SanctionsScreeningEngine::new();
engine.load_sanctions_list(SanctionsListType::OFAC, entries);

let customer = Customer {
    customer_id: "CUST-001".to_string(),
    full_name: "John Doe".to_string(),
    country_of_residence: "US".to_string(),
    // ...
};

let matches = engine.screen_customer(&customer);
```

### TypeScript - KYC Client

```typescript
const client = new KYCClient(apiUrl, apiKey);

await client.submitIdentityDocument(customerId, {
  documentType: 'PASSPORT',
  documentNumber: 'P123456',
  issuer: 'US',
  expiryDate: new Date('2030-01-01'),
});

const risk = await client.getRiskAssessment(customerId);
```

### Python - Transaction Monitoring

```python
monitor = TransactionMonitor()
monitor.add_rule(MonitoringRule('RULE-1', 'THRESHOLD', Decimal('10000'), 24))

tx = Transaction('TX-001', 'CUST-001', Decimal('15000'), 'USD', datetime.now())
alerts = monitor.check_transaction(tx)
```

## Compliance

- FATF recommendations
- FinCEN requirements
- MiFID II
- AMLD (Anti-Money Laundering Directive)

## License

MIT
