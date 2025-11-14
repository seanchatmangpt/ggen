# Order State Machine

## Overview

The order management system uses a finite state machine to enforce valid order lifecycle transitions.

## States

### 1. Cart
- **Description**: Items in shopping cart, not yet ordered
- **Entry**: User adds items to cart
- **Exit**: User places order or abandons cart
- **Valid Transitions**: Pending, Cancelled

### 2. Pending
- **Description**: Order placed, awaiting payment confirmation
- **Entry**: User submits order
- **Exit**: Payment confirmed or order cancelled
- **Valid Transitions**: Processing, Cancelled, OnHold

### 3. Processing
- **Description**: Payment confirmed, order being prepared
- **Entry**: Payment successfully processed
- **Exit**: Order shipped or put on hold
- **Valid Transitions**: Shipped, Cancelled, OnHold

### 4. Shipped
- **Description**: Order shipped to customer
- **Entry**: Fulfillment complete, carrier has package
- **Exit**: Package delivered
- **Valid Transitions**: Delivered, OnHold

### 5. Delivered
- **Description**: Order delivered to customer
- **Entry**: Carrier confirms delivery
- **Exit**: Return/refund initiated
- **Valid Transitions**: Refunded

### 6. Cancelled
- **Description**: Order cancelled
- **Entry**: User or system cancels order
- **Exit**: Terminal state
- **Valid Transitions**: None (terminal)

### 7. Refunded
- **Description**: Order refunded
- **Entry**: Refund processed
- **Exit**: Terminal state
- **Valid Transitions**: None (terminal)

### 8. OnHold
- **Description**: Order on hold, awaiting resolution
- **Entry**: Issue detected (payment, inventory, fraud)
- **Exit**: Issue resolved
- **Valid Transitions**: Processing, Cancelled

## Transition Matrix

| From        | To          | Valid | Reason                          |
|-------------|-------------|-------|---------------------------------|
| Cart        | Pending     | ✅    | Order submitted                 |
| Cart        | Cancelled   | ✅    | User abandons cart              |
| Pending     | Processing  | ✅    | Payment confirmed               |
| Pending     | Cancelled   | ✅    | Payment failed or user cancels  |
| Pending     | OnHold      | ✅    | Fraud detection                 |
| Processing  | Shipped     | ✅    | Fulfillment complete            |
| Processing  | Cancelled   | ✅    | Inventory unavailable           |
| Processing  | OnHold      | ✅    | Issue detected                  |
| Shipped     | Delivered   | ✅    | Carrier confirms delivery       |
| Shipped     | OnHold      | ✅    | Delivery exception              |
| Delivered   | Refunded    | ✅    | Return processed                |
| OnHold      | Processing  | ✅    | Issue resolved                  |
| OnHold      | Cancelled   | ✅    | Issue cannot be resolved        |
| Cancelled   | *any*       | ❌    | Terminal state                  |
| Refunded    | *any*       | ❌    | Terminal state                  |

## Implementation

### Rust

```rust
pub fn can_transition(&self, from: OrderStatus, to: OrderStatus) -> bool {
    self.valid_transitions
        .get(&from)
        .map(|allowed| allowed.contains(&to))
        .unwrap_or(false)
}

pub fn transition(&self, order: &mut Order, new_status: OrderStatus) -> Result<(), String> {
    if !self.can_transition(order.status, new_status) {
        return Err(format!(
            "Invalid transition from {:?} to {:?}",
            order.status, new_status
        ));
    }
    order.status = new_status;
    Ok(())
}
```

### TypeScript

```typescript
async updateOrderStatus(orderNumber: string, newStatus: OrderStatus): Promise<Order> {
  const response = await fetch(`${this.baseUrl}/${orderNumber}/status`, {
    method: 'PUT',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ status: newStatus }),
  });
  return response.json();
}
```

## Business Rules

### Payment Processing
- Only Pending orders can be processed
- Payment must match order total exactly
- Transaction ID must be unique

### Fulfillment
- Only Processing orders can be shipped
- All items must be in stock or on backorder
- Warehouse must be assigned

### Cancellation
- Cannot cancel Delivered, Refunded, or Cancelled orders
- Cancellation from Shipped requires carrier coordination

### Returns
- Only Delivered orders can be refunded
- Return window: 30 days from delivery
- Return reason required

## Error Handling

### Invalid Transition
```
Error: "Invalid transition from Delivered to Processing"
```

### Missing Prerequisites
```
Error: "Order must be in Pending status to process payment"
```

### Business Rule Violation
```
Error: "Cannot cancel order in Delivered status"
```
