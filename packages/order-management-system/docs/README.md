# Order Management System

Production-ready order processing and fulfillment system with comprehensive RDF ontology.

## Overview

This package provides a complete order management solution including:

- **Order Lifecycle Management**: State machine for cart → pending → processing → shipped → delivered
- **Payment Processing**: Payment validation and refund handling
- **Fulfillment**: Multi-warehouse fulfillment with pick lists and backorders
- **Shipment Tracking**: Integration with carriers and tracking numbers
- **Returns & Exchanges**: Complete return/refund workflow

## Components

### 1. RDF Ontology (290+ lines)

**File**: `ontology/order-management.ttl`

Defines comprehensive order semantics:

- Order states and lifecycle
- Order items and line-level details
- Payment methods and transactions
- Shipping and delivery information
- Fulfillment and warehouse operations
- Returns, refunds, and exchanges

### 2. SPARQL Templates (12+ queries)

**File**: `sparql/queries.rq`

Production queries for:

- Order status filtering
- Total calculation with tax
- Fulfillment instructions
- Shipment tracking
- Return processing
- Analytics and reporting

### 3. Multi-Language Code

**Rust**: `templates/rust/order_processor.rs`
- High-performance state machine
- Order validation and transitions
- Total calculation engine

**TypeScript**: `templates/typescript/order-management-ui.ts`
- React UI components
- Order list and detail views
- Status management interface

**Python**: `templates/python/order_analytics.py`
- Revenue analytics
- Fulfillment time analysis
- Customer value metrics

### 4. Chicago TDD Tests (600+ lines)

**File**: `tests/chicago_tdd/order_processor_tests.rs`

Test coverage:
- Order lifecycle state machine (9 tests)
- Order calculations (6 tests)
- Payment processing (4 tests)
- Fulfillment workflows (4 tests)
- Returns and refunds (4 tests)
- Analytics (3 tests)

## Order Workflow

### Standard Order Flow

```
Cart → Pending → Processing → Shipped → Delivered
```

### Alternative Flows

**Cancellation**:
```
Cart/Pending/Processing → Cancelled
```

**Hold**:
```
Pending/Processing → OnHold → Processing
```

**Refund**:
```
Delivered → Refunded
```

## Usage Examples

### Extract Orders by Status

```sparql
SELECT ?order ?orderNumber ?customer ?total ?orderDate
WHERE {
    ?order a om:Order ;
           om:hasOrderNumber ?orderNumber ;
           om:hasOrderStatus om:ProcessingStatus ;
           om:hasCustomer ?customer ;
           om:hasTotal ?total ;
           om:hasOrderDate ?orderDate .
}
ORDER BY DESC(?orderDate)
```

### Calculate Order Total

```rust
use order_processor::OrderProcessor;

let mut order = Order { /* ... */ };
OrderProcessor::calculate_total(&mut order);
// order.total = subtotal + tax + shipping - discount
```

### Generate Fulfillment Instructions

```sparql
SELECT ?order ?orderNumber ?warehouse ?item ?sku ?quantity
WHERE {
    ?order a om:Order ;
           om:hasOrderStatus om:ProcessingStatus ;
           om:hasFulfillment ?fulfillment ;
           om:hasOrderItem ?item .

    ?fulfillment om:hasWarehouse ?warehouse .
    ?item om:hasSKU ?sku ;
          om:hasQuantity ?quantity .
}
ORDER BY ?warehouse ?sku
```

## Integration

### Payment Providers

Integrate with payment gateways:

```typescript
const service = new OrderManagementService();
await service.processPayment(orderNumber, {
  transactionId: 'txn_123',
  method: 'credit_card',
  amount: 115.00,
  status: 'completed'
});
```

### Shipping Carriers

Track shipments:

```typescript
await service.shipOrder(orderNumber, {
  carrier: 'FedEx',
  trackingNumber: '1234567890',
  estimatedDelivery: new Date('2025-01-15')
});
```

## Performance

- **State transitions**: O(1) lookup via HashMap
- **Total calculation**: O(n) where n = number of items
- **SPARQL queries**: Optimized with indexes on status and date

## Testing

Run Chicago TDD tests:

```bash
cargo test --test order_processor_tests
```

All tests use Chicago-style testing with clear ARRANGE-ACT-ASSERT structure.

## License

MIT License - See LICENSE file for details
