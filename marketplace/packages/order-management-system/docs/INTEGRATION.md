# Order Management Integration Guide

## Overview

This guide covers integration points for payment providers, shipping carriers, and warehouse systems.

## Payment Integration

### Supported Payment Providers

- Stripe
- PayPal
- Square
- Braintree
- Authorize.net

### Payment Flow

```typescript
// 1. Create payment intent
const paymentIntent = await paymentProvider.createIntent({
  amount: order.total,
  currency: order.currency,
  orderId: order.orderNumber
});

// 2. Process payment
const result = await service.processPayment(order.orderNumber, {
  transactionId: paymentIntent.id,
  method: 'credit_card',
  amount: order.total,
  status: 'completed'
});

// 3. Order transitions to Processing
// order.status = OrderStatus.Processing
```

### Webhook Handling

```typescript
// Handle payment webhook
app.post('/webhooks/payment', async (req, res) => {
  const event = req.body;

  if (event.type === 'payment_intent.succeeded') {
    await service.processPayment(event.orderNumber, {
      transactionId: event.id,
      method: event.paymentMethod,
      amount: event.amount,
      status: 'completed'
    });
  }

  res.status(200).send();
});
```

### Refund Processing

```typescript
// Process full refund
const refund = await paymentProvider.createRefund({
  transactionId: payment.transactionId,
  amount: order.total,
  reason: 'customer_request'
});

// Update order
await service.refundOrder(order.orderNumber, {
  refundAmount: refund.amount,
  refundReason: refund.reason
});
```

## Shipping Integration

### Supported Carriers

- FedEx
- UPS
- USPS
- DHL
- Canada Post

### Shipping Flow

```typescript
// 1. Calculate shipping rates
const rates = await shippingProvider.getRates({
  origin: warehouse.address,
  destination: order.deliveryAddress,
  packages: order.items
});

// 2. Create shipment
const shipment = await shippingProvider.createShipment({
  orderId: order.orderNumber,
  carrier: 'FedEx',
  service: 'GROUND',
  packages: order.items,
  destination: order.deliveryAddress
});

// 3. Update order with tracking
await service.shipOrder(order.orderNumber, {
  carrier: shipment.carrier,
  trackingNumber: shipment.trackingNumber,
  estimatedDelivery: shipment.estimatedDelivery
});
```

### Tracking Updates

```typescript
// Subscribe to tracking updates
app.post('/webhooks/tracking', async (req, res) => {
  const event = req.body;

  if (event.status === 'delivered') {
    await service.markDelivered(event.orderNumber);
  }

  res.status(200).send();
});
```

### Label Generation

```typescript
// Generate shipping label
const label = await shippingProvider.generateLabel({
  shipmentId: shipment.id,
  format: 'pdf'
});

// Save label URL
shipment.labelUrl = label.url;
```

## Warehouse Integration

### Multi-Warehouse Fulfillment

```sparql
# Split order across warehouses
SELECT ?warehouse (COUNT(?item) AS ?itemCount)
WHERE {
    ?order om:hasOrderNumber "ORD-001" ;
           om:hasOrderItem ?item .

    ?item om:hasSKU ?sku .

    ?warehouse wms:hasInventory ?inventory .
    ?inventory wms:hasSKU ?sku .
}
GROUP BY ?warehouse
```

### Inventory Sync

```typescript
// Check inventory before fulfillment
const inventory = await warehouseSystem.getInventory({
  skus: order.items.map(item => item.sku)
});

const allAvailable = order.items.every(item => {
  const available = inventory[item.sku] || 0;
  return available >= item.quantity;
});

if (!allAvailable) {
  // Create backorder
  await service.createBackorder(order.orderNumber, insufficientItems);
}
```

### Pick List Generation

```typescript
// Generate pick list for warehouse
const pickList = await service.generatePickList(order.orderNumber);

// pickList = [
//   { sku: 'SKU-001', quantity: 2, location: 'A-12-03' },
//   { sku: 'SKU-002', quantity: 1, location: 'B-05-18' }
// ]

// Send to warehouse management system
await warehouseSystem.createPickJob({
  orderId: order.orderNumber,
  items: pickList
});
```

## ERP Integration

### Order Export

```typescript
// Export order to ERP system
const erpOrder = {
  orderNumber: order.orderNumber,
  customerId: order.customerId,
  items: order.items.map(item => ({
    sku: item.sku,
    quantity: item.quantity,
    price: item.unitPrice
  })),
  totals: {
    subtotal: order.subtotal,
    tax: order.tax,
    shipping: order.shippingCost,
    discount: order.discount,
    total: order.total
  }
};

await erpSystem.createOrder(erpOrder);
```

### Accounting Sync

```typescript
// Sync to accounting system
await accountingSystem.createInvoice({
  orderId: order.orderNumber,
  customerId: order.customerId,
  lineItems: order.items,
  total: order.total,
  tax: order.tax
});
```

## Notification Integration

### Email Notifications

```typescript
// Order confirmation
await emailService.send({
  to: customer.email,
  template: 'order-confirmation',
  data: {
    orderNumber: order.orderNumber,
    items: order.items,
    total: order.total,
    estimatedDelivery: order.estimatedDelivery
  }
});

// Shipment notification
await emailService.send({
  to: customer.email,
  template: 'order-shipped',
  data: {
    orderNumber: order.orderNumber,
    trackingNumber: shipment.trackingNumber,
    carrier: shipment.carrier
  }
});
```

### SMS Notifications

```typescript
// Delivery notification
await smsService.send({
  to: customer.phone,
  message: `Your order ${order.orderNumber} has been delivered!`
});
```

## Analytics Integration

### Export to Data Warehouse

```python
# Export orders to analytics platform
from order_analytics import OrderAnalytics

analytics = OrderAnalytics(orders)
metrics = analytics.calculate_order_metrics()

# Push to data warehouse
warehouse.push('order_metrics', {
    'date': datetime.now(),
    'total_orders': metrics.total_orders,
    'total_revenue': metrics.total_revenue,
    'average_order_value': metrics.average_order_value
})
```

## Error Handling

### Payment Failures

```typescript
try {
  await service.processPayment(orderNumber, payment);
} catch (error) {
  if (error.code === 'insufficient_funds') {
    // Put order on hold
    await service.updateOrderStatus(orderNumber, OrderStatus.OnHold);
    await notifyCustomer('payment_failed');
  }
}
```

### Inventory Shortages

```typescript
try {
  await service.fulfillOrder(orderNumber);
} catch (error) {
  if (error.code === 'insufficient_inventory') {
    // Create backorder
    await service.createBackorder(orderNumber, error.items);
    await notifyCustomer('items_backordered');
  }
}
```

### Shipping Exceptions

```typescript
// Handle delivery exception
if (trackingUpdate.status === 'exception') {
  await service.updateOrderStatus(orderNumber, OrderStatus.OnHold);
  await notifyCustomer('delivery_exception', {
    reason: trackingUpdate.exception
  });
}
```

## Security

### PCI Compliance

- Never store credit card numbers
- Use tokenization for payment processing
- Encrypt sensitive data at rest
- Use HTTPS for all API calls

### Data Protection

```typescript
// Sanitize order data before logging
const sanitizedOrder = {
  ...order,
  payment: {
    method: order.payment.method,
    last4: order.payment.last4
    // Remove: transactionId, full card details
  }
};
```

## Performance Optimization

### Caching

```typescript
// Cache shipping rates
const cacheKey = `rates:${origin}:${destination}`;
let rates = await cache.get(cacheKey);

if (!rates) {
  rates = await shippingProvider.getRates({ origin, destination });
  await cache.set(cacheKey, rates, { ttl: 3600 }); // 1 hour
}
```

### Batch Processing

```typescript
// Batch fulfillment
const processingOrders = await service.getOrders(OrderStatus.Processing);

const fulfillmentJobs = processingOrders.map(order =>
  service.fulfillOrder(order.orderNumber)
);

await Promise.all(fulfillmentJobs);
```
