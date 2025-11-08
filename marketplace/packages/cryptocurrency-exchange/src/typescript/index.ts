// Cryptocurrency Exchange TypeScript Implementation
export interface Wallet {
  address: string;
  type: 'hot' | 'cold';
  balances: Map<string, number>;
}

export interface Order {
  id: string;
  userId: string;
  type: 'market' | 'limit' | 'stop_loss';
  side: 'buy' | 'sell';
  pair: string;
  price: number;
  quantity: number;
  status: 'open' | 'filled' | 'cancelled';
}

export class WalletManager {
  private wallets: Map<string, Wallet> = new Map();

  createWallet(type: 'hot' | 'cold'): Wallet {
    const address = '0x' + Math.random().toString(16).substring(2, 42);
    const wallet: Wallet = { address, type, balances: new Map() };
    this.wallets.set(address, wallet);
    return wallet;
  }

  deposit(address: string, token: string, amount: number): void {
    const wallet = this.wallets.get(address);
    if (!wallet) throw new Error('Wallet not found');
    wallet.balances.set(token, (wallet.balances.get(token) || 0) + amount);
  }

  getBalance(address: string, token: string): number {
    return this.wallets.get(address)?.balances.get(token) || 0;
  }
}

export class OrderBook {
  private bids: Order[] = [];
  private asks: Order[] = [];

  addOrder(order: Order): void {
    if (order.side === 'buy') {
      this.bids.push(order);
      this.bids.sort((a, b) => b.price - a.price);
    } else {
      this.asks.push(order);
      this.asks.sort((a, b) => a.price - b.price);
    }
  }

  matchOrders(): Array<{ buyId: string; sellId: string; price: number; quantity: number }> {
    const trades = [];
    while (this.bids.length > 0 && this.asks.length > 0) {
      const bid = this.bids[0];
      const ask = this.asks[0];
      if (bid.price >= ask.price) {
        const quantity = Math.min(bid.quantity, ask.quantity);
        trades.push({
          buyId: bid.id,
          sellId: ask.id,
          price: (bid.price + ask.price) / 2,
          quantity,
        });
        if (bid.quantity <= ask.quantity) this.bids.shift();
        if (ask.quantity <= bid.quantity) this.asks.shift();
      } else break;
    }
    return trades;
  }
}
