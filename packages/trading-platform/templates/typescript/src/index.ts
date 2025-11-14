// Trading Platform - TypeScript WebSocket Client
// Real-time order management and market data streaming

import WebSocket from 'ws';
import { EventEmitter } from 'events';

// ============================================================
// Type Definitions
// ============================================================

export type OrderSide = 'BUY' | 'SELL';
export type OrderType = 'MARKET' | 'LIMIT' | 'STOP' | 'STOP_LIMIT';
export type OrderStatus = 'PENDING_NEW' | 'NEW' | 'PARTIALLY_FILLED' | 'FILLED' | 'CANCELED' | 'REJECTED';
export type TimeInForce = 'DAY' | 'GTC' | 'IOC' | 'FOK';

export interface Order {
  orderId: string;
  clientOrderId: string;
  symbol: string;
  side: OrderSide;
  orderType: OrderType;
  quantity: number;
  price?: number;
  stopPrice?: number;
  timeInForce: TimeInForce;
  status: OrderStatus;
  filledQuantity: number;
  averageFillPrice?: number;
  timestamp: Date;
}

export interface Fill {
  fillId: string;
  orderId: string;
  price: number;
  quantity: number;
  timestamp: Date;
}

export interface Quote {
  symbol: string;
  bidPrice: number;
  bidSize: number;
  askPrice: number;
  askSize: number;
  timestamp: Date;
}

export interface Trade {
  tradeId: string;
  symbol: string;
  price: number;
  quantity: number;
  timestamp: Date;
}

export interface OrderBookLevel {
  price: number;
  quantity: number;
  orderCount: number;
}

export interface OrderBookSnapshot {
  symbol: string;
  bids: OrderBookLevel[];
  asks: OrderBookLevel[];
  timestamp: Date;
}

// ============================================================
// Trading Client
// ============================================================

export class TradingClient extends EventEmitter {
  private ws: WebSocket | null = null;
  private url: string;
  private apiKey: string;
  private reconnectInterval: number = 5000;
  private heartbeatInterval: NodeJS.Timeout | null = null;
  private subscriptions: Set<string> = new Set();

  constructor(url: string, apiKey: string) {
    super();
    this.url = url;
    this.apiKey = apiKey;
  }

  /**
   * Connect to trading platform
   */
  async connect(): Promise<void> {
    return new Promise((resolve, reject) => {
      this.ws = new WebSocket(this.url, {
        headers: {
          'Authorization': `Bearer ${this.apiKey}`,
        },
      });

      this.ws.on('open', () => {
        console.log('Connected to trading platform');
        this.startHeartbeat();
        this.emit('connected');
        resolve();
      });

      this.ws.on('message', (data: string) => {
        try {
          const message = JSON.parse(data.toString());
          this.handleMessage(message);
        } catch (error) {
          console.error('Failed to parse message:', error);
        }
      });

      this.ws.on('error', (error) => {
        console.error('WebSocket error:', error);
        this.emit('error', error);
        reject(error);
      });

      this.ws.on('close', () => {
        console.log('Disconnected from trading platform');
        this.stopHeartbeat();
        this.emit('disconnected');
        this.scheduleReconnect();
      });
    });
  }

  /**
   * Disconnect from trading platform
   */
  disconnect(): void {
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
    this.stopHeartbeat();
  }

  /**
   * Submit new order
   */
  async submitOrder(order: Omit<Order, 'orderId' | 'status' | 'filledQuantity' | 'timestamp'>): Promise<string> {
    const message = {
      type: 'NEW_ORDER',
      data: {
        ...order,
        timestamp: new Date().toISOString(),
      },
    };

    this.send(message);

    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        reject(new Error('Order submission timeout'));
      }, 5000);

      this.once('order_ack', (ack: any) => {
        clearTimeout(timeout);
        if (ack.status === 'REJECTED') {
          reject(new Error(ack.rejectReason || 'Order rejected'));
        } else {
          resolve(ack.orderId);
        }
      });
    });
  }

  /**
   * Cancel existing order
   */
  async cancelOrder(orderId: string): Promise<void> {
    const message = {
      type: 'CANCEL_ORDER',
      data: { orderId },
    };

    this.send(message);

    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        reject(new Error('Cancel timeout'));
      }, 5000);

      this.once('cancel_ack', (ack: any) => {
        clearTimeout(timeout);
        if (ack.orderId === orderId) {
          resolve();
        } else {
          reject(new Error('Cancel failed'));
        }
      });
    });
  }

  /**
   * Subscribe to market data
   */
  subscribeMarketData(symbol: string, dataType: 'QUOTE' | 'TRADE' | 'DEPTH'): void {
    const subscription = `${dataType}:${symbol}`;

    if (!this.subscriptions.has(subscription)) {
      const message = {
        type: 'SUBSCRIBE',
        data: {
          symbol,
          dataType,
        },
      };

      this.send(message);
      this.subscriptions.add(subscription);
    }
  }

  /**
   * Unsubscribe from market data
   */
  unsubscribeMarketData(symbol: string, dataType: 'QUOTE' | 'TRADE' | 'DEPTH'): void {
    const subscription = `${dataType}:${symbol}`;

    if (this.subscriptions.has(subscription)) {
      const message = {
        type: 'UNSUBSCRIBE',
        data: {
          symbol,
          dataType,
        },
      };

      this.send(message);
      this.subscriptions.delete(subscription);
    }
  }

  /**
   * Handle incoming messages
   */
  private handleMessage(message: any): void {
    switch (message.type) {
      case 'ORDER_ACK':
        this.emit('order_ack', message.data);
        break;

      case 'CANCEL_ACK':
        this.emit('cancel_ack', message.data);
        break;

      case 'EXECUTION_REPORT':
        const order: Order = {
          ...message.data,
          timestamp: new Date(message.data.timestamp),
        };
        this.emit('execution_report', order);
        break;

      case 'FILL':
        const fill: Fill = {
          ...message.data,
          timestamp: new Date(message.data.timestamp),
        };
        this.emit('fill', fill);
        break;

      case 'QUOTE':
        const quote: Quote = {
          ...message.data,
          timestamp: new Date(message.data.timestamp),
        };
        this.emit('quote', quote);
        break;

      case 'TRADE':
        const trade: Trade = {
          ...message.data,
          timestamp: new Date(message.data.timestamp),
        };
        this.emit('trade', trade);
        break;

      case 'DEPTH':
        const depth: OrderBookSnapshot = {
          ...message.data,
          timestamp: new Date(message.data.timestamp),
        };
        this.emit('depth', depth);
        break;

      case 'HEARTBEAT':
        // Acknowledge heartbeat
        break;

      default:
        console.warn('Unknown message type:', message.type);
    }
  }

  /**
   * Send message to server
   */
  private send(message: any): void {
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify(message));
    } else {
      throw new Error('Not connected to trading platform');
    }
  }

  /**
   * Start heartbeat to keep connection alive
   */
  private startHeartbeat(): void {
    this.heartbeatInterval = setInterval(() => {
      try {
        this.send({ type: 'HEARTBEAT' });
      } catch (error) {
        console.error('Heartbeat failed:', error);
      }
    }, 30000); // Every 30 seconds
  }

  /**
   * Stop heartbeat
   */
  private stopHeartbeat(): void {
    if (this.heartbeatInterval) {
      clearInterval(this.heartbeatInterval);
      this.heartbeatInterval = null;
    }
  }

  /**
   * Schedule reconnection attempt
   */
  private scheduleReconnect(): void {
    setTimeout(() => {
      console.log('Attempting to reconnect...');
      this.connect().catch((error) => {
        console.error('Reconnection failed:', error);
      });
    }, this.reconnectInterval);
  }
}

// ============================================================
// Utility Functions
// ============================================================

export function calculatePnL(
  side: OrderSide,
  entryPrice: number,
  currentPrice: number,
  quantity: number
): number {
  if (side === 'BUY') {
    return (currentPrice - entryPrice) * quantity;
  } else {
    return (entryPrice - currentPrice) * quantity;
  }
}

export function calculateSpread(bidPrice: number, askPrice: number): number {
  return askPrice - bidPrice;
}

export function calculateSpreadBps(bidPrice: number, askPrice: number): number {
  const spread = askPrice - bidPrice;
  const midPrice = (bidPrice + askPrice) / 2;
  return (spread / midPrice) * 10000; // Basis points
}

export default TradingClient;
