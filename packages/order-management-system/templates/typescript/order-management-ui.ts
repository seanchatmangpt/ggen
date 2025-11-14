// ============================================================================
// Order Management UI - TypeScript React Components
// ============================================================================
// Production-ready order management interface
// ============================================================================

import React, { useState, useEffect } from 'react';

// ============================================================================
// Types
// ============================================================================

export enum OrderStatus {
  Cart = 'Cart',
  Pending = 'Pending',
  Processing = 'Processing',
  Shipped = 'Shipped',
  Delivered = 'Delivered',
  Cancelled = 'Cancelled',
  Refunded = 'Refunded',
  OnHold = 'OnHold',
}

export interface Order {
  orderNumber: string;
  customerId: string;
  status: OrderStatus;
  items: OrderItem[];
  subtotal: number;
  tax: number;
  shippingCost: number;
  discount: number;
  total: number;
  orderDate: Date;
  payment?: Payment;
  shipment?: Shipment;
}

export interface OrderItem {
  productId: string;
  sku: string;
  productName: string;
  quantity: number;
  unitPrice: number;
  lineTotal: number;
}

export interface Payment {
  transactionId: string;
  method: string;
  amount: number;
  status: string;
}

export interface Shipment {
  carrier: string;
  trackingNumber: string;
  estimatedDelivery: Date;
  actualDelivery?: Date;
}

// ============================================================================
// Order Management Service
// ============================================================================

export class OrderManagementService {
  private baseUrl: string;

  constructor(baseUrl: string = '/api/orders') {
    this.baseUrl = baseUrl;
  }

  async getOrders(status?: OrderStatus): Promise<Order[]> {
    const params = status ? `?status=${status}` : '';
    const response = await fetch(`${this.baseUrl}${params}`);
    return response.json();
  }

  async getOrder(orderNumber: string): Promise<Order> {
    const response = await fetch(`${this.baseUrl}/${orderNumber}`);
    return response.json();
  }

  async updateOrderStatus(
    orderNumber: string,
    newStatus: OrderStatus
  ): Promise<Order> {
    const response = await fetch(`${this.baseUrl}/${orderNumber}/status`, {
      method: 'PUT',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ status: newStatus }),
    });
    return response.json();
  }

  async processPayment(
    orderNumber: string,
    payment: Payment
  ): Promise<Order> {
    const response = await fetch(`${this.baseUrl}/${orderNumber}/payment`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(payment),
    });
    return response.json();
  }

  async shipOrder(
    orderNumber: string,
    shipment: Shipment
  ): Promise<Order> {
    const response = await fetch(`${this.baseUrl}/${orderNumber}/ship`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(shipment),
    });
    return response.json();
  }

  async cancelOrder(orderNumber: string): Promise<Order> {
    const response = await fetch(`${this.baseUrl}/${orderNumber}/cancel`, {
      method: 'POST',
    });
    return response.json();
  }
}

// ============================================================================
// Order List Component
// ============================================================================

export const OrderList: React.FC = () => {
  const [orders, setOrders] = useState<Order[]>([]);
  const [statusFilter, setStatusFilter] = useState<OrderStatus | undefined>();
  const service = new OrderManagementService();

  useEffect(() => {
    loadOrders();
  }, [statusFilter]);

  const loadOrders = async () => {
    const data = await service.getOrders(statusFilter);
    setOrders(data);
  };

  return (
    <div className="order-list">
      <h2>Order Management</h2>

      <div className="filters">
        <label>Status Filter:</label>
        <select
          value={statusFilter || ''}
          onChange={(e) => setStatusFilter(e.target.value as OrderStatus)}
        >
          <option value="">All Orders</option>
          {Object.values(OrderStatus).map((status) => (
            <option key={status} value={status}>
              {status}
            </option>
          ))}
        </select>
      </div>

      <table className="order-table">
        <thead>
          <tr>
            <th>Order #</th>
            <th>Customer</th>
            <th>Date</th>
            <th>Total</th>
            <th>Status</th>
            <th>Actions</th>
          </tr>
        </thead>
        <tbody>
          {orders.map((order) => (
            <OrderRow key={order.orderNumber} order={order} />
          ))}
        </tbody>
      </table>
    </div>
  );
};

// ============================================================================
// Order Row Component
// ============================================================================

const OrderRow: React.FC<{ order: Order }> = ({ order }) => {
  return (
    <tr>
      <td>{order.orderNumber}</td>
      <td>{order.customerId}</td>
      <td>{new Date(order.orderDate).toLocaleDateString()}</td>
      <td>${order.total.toFixed(2)}</td>
      <td>
        <span className={`status-badge status-${order.status.toLowerCase()}`}>
          {order.status}
        </span>
      </td>
      <td>
        <button onClick={() => viewOrder(order.orderNumber)}>View</button>
      </td>
    </tr>
  );
};

// ============================================================================
// Order Detail Component
// ============================================================================

export const OrderDetail: React.FC<{ orderNumber: string }> = ({
  orderNumber,
}) => {
  const [order, setOrder] = useState<Order | null>(null);
  const service = new OrderManagementService();

  useEffect(() => {
    loadOrder();
  }, [orderNumber]);

  const loadOrder = async () => {
    const data = await service.getOrder(orderNumber);
    setOrder(data);
  };

  if (!order) {
    return <div>Loading...</div>;
  }

  return (
    <div className="order-detail">
      <h2>Order {order.orderNumber}</h2>

      <div className="order-summary">
        <p>
          <strong>Status:</strong> {order.status}
        </p>
        <p>
          <strong>Customer:</strong> {order.customerId}
        </p>
        <p>
          <strong>Order Date:</strong>{' '}
          {new Date(order.orderDate).toLocaleString()}
        </p>
      </div>

      <div className="order-items">
        <h3>Items</h3>
        <table>
          <thead>
            <tr>
              <th>Product</th>
              <th>SKU</th>
              <th>Quantity</th>
              <th>Unit Price</th>
              <th>Total</th>
            </tr>
          </thead>
          <tbody>
            {order.items.map((item, index) => (
              <tr key={index}>
                <td>{item.productName}</td>
                <td>{item.sku}</td>
                <td>{item.quantity}</td>
                <td>${item.unitPrice.toFixed(2)}</td>
                <td>${item.lineTotal.toFixed(2)}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>

      <div className="order-totals">
        <p>
          <strong>Subtotal:</strong> ${order.subtotal.toFixed(2)}
        </p>
        <p>
          <strong>Tax:</strong> ${order.tax.toFixed(2)}
        </p>
        <p>
          <strong>Shipping:</strong> ${order.shippingCost.toFixed(2)}
        </p>
        <p>
          <strong>Discount:</strong> -${order.discount.toFixed(2)}
        </p>
        <p className="total">
          <strong>Total:</strong> ${order.total.toFixed(2)}
        </p>
      </div>

      {order.shipment && (
        <div className="shipment-info">
          <h3>Shipment Details</h3>
          <p>
            <strong>Carrier:</strong> {order.shipment.carrier}
          </p>
          <p>
            <strong>Tracking Number:</strong> {order.shipment.trackingNumber}
          </p>
          <p>
            <strong>Estimated Delivery:</strong>{' '}
            {new Date(order.shipment.estimatedDelivery).toLocaleDateString()}
          </p>
        </div>
      )}
    </div>
  );
};

function viewOrder(orderNumber: string) {
  // Navigate to order detail page
  console.log('Viewing order:', orderNumber);
}
