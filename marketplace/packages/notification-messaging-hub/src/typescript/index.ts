// Notification & Messaging Hub - TypeScript Implementation
// Enterprise multi-channel notification system

export enum MessagePriority {
  Low = 'low',
  Medium = 'medium',
  High = 'high',
  Critical = 'critical',
}

export enum DeliveryStatus {
  Pending = 'pending',
  Queued = 'queued',
  Sending = 'sending',
  Delivered = 'delivered',
  Failed = 'failed',
  Bounced = 'bounced',
  Expired = 'expired',
}

export enum Channel {
  Email = 'email',
  SMS = 'sms',
  Push = 'push',
  Webhook = 'webhook',
  InApp = 'in_app',
  Slack = 'slack',
  Teams = 'teams',
}

export interface Message {
  id: string;
  subject?: string;
  body: string;
  htmlBody?: string;
  priority: MessagePriority;
  channel: Channel;
  recipientIds: string[];
  templateId?: string;
  templateVars?: Record<string, string>;
  createdAt: Date;
  scheduledAt?: Date;
  expiresAt?: Date;
  metadata?: Record<string, string>;
}

export interface DeliveryAttempt {
  messageId: string;
  attemptNumber: number;
  status: DeliveryStatus;
  provider: string;
  attemptedAt: Date;
  completedAt?: Date;
  failureReason?: string;
  responseTimeMs?: number;
}

export interface RateLimit {
  channel: Channel;
  maxPerMinute: number;
  maxPerHour: number;
  maxPerDay: number;
  burstSize: number;
}

export interface MessageProvider {
  send(message: Message): Promise<DeliveryAttempt>;
  getStatus(messageId: string): Promise<DeliveryStatus>;
  supportsChannel(channel: Channel): boolean;
  providerName(): string;
}

export interface MessageQueue {
  enqueue(message: Message): Promise<void>;
  dequeue(count: number): Promise<Message[]>;
  peek(count: number): Promise<Message[]>;
  depth(): Promise<number>;
  acknowledge(messageId: string): Promise<void>;
}

export class MessageBuilder {
  private message: Partial<Message>;

  constructor(id: string, body: string, channel: Channel) {
    this.message = {
      id,
      body,
      channel,
      priority: MessagePriority.Medium,
      recipientIds: [],
      createdAt: new Date(),
      metadata: {},
    };
  }

  withSubject(subject: string): this {
    this.message.subject = subject;
    return this;
  }

  withHtmlBody(htmlBody: string): this {
    this.message.htmlBody = htmlBody;
    return this;
  }

  withPriority(priority: MessagePriority): this {
    this.message.priority = priority;
    return this;
  }

  withRecipients(recipientIds: string[]): this {
    this.message.recipientIds = recipientIds;
    return this;
  }

  withTemplate(templateId: string, vars: Record<string, string>): this {
    this.message.templateId = templateId;
    this.message.templateVars = vars;
    return this;
  }

  schedule(at: Date): this {
    this.message.scheduledAt = at;
    return this;
  }

  expiresAt(at: Date): this {
    this.message.expiresAt = at;
    return this;
  }

  withMetadata(metadata: Record<string, string>): this {
    this.message.metadata = { ...this.message.metadata, ...metadata };
    return this;
  }

  build(): Message {
    if (!this.message.recipientIds || this.message.recipientIds.length === 0) {
      throw new Error('Message must have at least one recipient');
    }
    return this.message as Message;
  }
}

export class RateLimitManager {
  private limits: Map<Channel, RateLimit>;
  private counters: Map<string, number>;
  private windows: Map<string, Date>;

  constructor() {
    this.limits = new Map();
    this.counters = new Map();
    this.windows = new Map();

    // Initialize default limits
    this.setDefaultLimits();
  }

  private setDefaultLimits(): void {
    this.limits.set(Channel.Email, {
      channel: Channel.Email,
      maxPerMinute: 100,
      maxPerHour: 5000,
      maxPerDay: 50000,
      burstSize: 200,
    });

    this.limits.set(Channel.SMS, {
      channel: Channel.SMS,
      maxPerMinute: 50,
      maxPerHour: 1000,
      maxPerDay: 10000,
      burstSize: 100,
    });

    this.limits.set(Channel.Push, {
      channel: Channel.Push,
      maxPerMinute: 1000,
      maxPerHour: 50000,
      maxPerDay: 500000,
      burstSize: 2000,
    });
  }

  setLimit(channel: Channel, limit: RateLimit): void {
    this.limits.set(channel, limit);
  }

  async checkLimit(channel: Channel, window: 'minute' | 'hour' | 'day'): Promise<boolean> {
    const limit = this.limits.get(channel);
    if (!limit) return true;

    const key = `${channel}:${window}`;
    const now = new Date();
    const windowStart = this.windows.get(key);

    // Reset window if expired
    if (windowStart && this.isWindowExpired(windowStart, window)) {
      this.counters.set(key, 0);
      this.windows.set(key, now);
    }

    const count = this.counters.get(key) || 0;
    const maxLimit = this.getMaxForWindow(limit, window);

    return count < maxLimit;
  }

  async increment(channel: Channel, window: 'minute' | 'hour' | 'day'): Promise<void> {
    const key = `${channel}:${window}`;
    const count = this.counters.get(key) || 0;
    this.counters.set(key, count + 1);

    if (!this.windows.has(key)) {
      this.windows.set(key, new Date());
    }
  }

  private isWindowExpired(start: Date, window: 'minute' | 'hour' | 'day'): boolean {
    const now = new Date();
    const diff = now.getTime() - start.getTime();

    switch (window) {
      case 'minute':
        return diff > 60000;
      case 'hour':
        return diff > 3600000;
      case 'day':
        return diff > 86400000;
      default:
        return false;
    }
  }

  private getMaxForWindow(limit: RateLimit, window: 'minute' | 'hour' | 'day'): number {
    switch (window) {
      case 'minute':
        return limit.maxPerMinute;
      case 'hour':
        return limit.maxPerHour;
      case 'day':
        return limit.maxPerDay;
    }
  }
}

export class NotificationHub {
  private providers: Map<Channel, MessageProvider[]>;
  private queues: Map<Channel, MessageQueue>;
  private rateLimitManager: RateLimitManager;

  constructor() {
    this.providers = new Map();
    this.queues = new Map();
    this.rateLimitManager = new RateLimitManager();
  }

  registerProvider(channel: Channel, provider: MessageProvider): void {
    const providers = this.providers.get(channel) || [];
    providers.push(provider);
    this.providers.set(channel, providers);
  }

  registerQueue(channel: Channel, queue: MessageQueue): void {
    this.queues.set(channel, queue);
  }

  async sendMessage(message: Message): Promise<DeliveryAttempt> {
    // Check expiration
    if (message.expiresAt && message.expiresAt < new Date()) {
      throw new Error('Message expired');
    }

    // Check rate limits
    const canSend = await this.rateLimitManager.checkLimit(message.channel, 'minute');
    if (!canSend) {
      // Queue for later
      const queue = this.queues.get(message.channel);
      if (queue) {
        await queue.enqueue(message);
        return {
          messageId: message.id,
          attemptNumber: 0,
          status: DeliveryStatus.Queued,
          provider: 'queue',
          attemptedAt: new Date(),
        };
      }
      throw new Error('Rate limit exceeded and no queue available');
    }

    // Check if scheduled
    if (message.scheduledAt && message.scheduledAt > new Date()) {
      const queue = this.queues.get(message.channel);
      if (queue) {
        await queue.enqueue(message);
        return {
          messageId: message.id,
          attemptNumber: 0,
          status: DeliveryStatus.Queued,
          provider: 'queue',
          attemptedAt: new Date(),
        };
      }
    }

    // Get providers for channel
    const providers = this.providers.get(message.channel);
    if (!providers || providers.length === 0) {
      throw new Error(`No provider available for channel: ${message.channel}`);
    }

    // Try providers in order
    let lastError: Error | null = null;
    for (const provider of providers) {
      try {
        const result = await provider.send(message);
        await this.rateLimitManager.increment(message.channel, 'minute');
        return result;
      } catch (error) {
        lastError = error as Error;
        console.error(`Provider ${provider.providerName()} failed:`, error);
        continue;
      }
    }

    throw lastError || new Error('All providers failed');
  }

  async processQueue(channel: Channel, batchSize: number): Promise<DeliveryAttempt[]> {
    const queue = this.queues.get(channel);
    if (!queue) {
      throw new Error(`No queue for channel: ${channel}`);
    }

    const messages = await queue.dequeue(batchSize);
    const results: DeliveryAttempt[] = [];

    for (const message of messages) {
      try {
        const result = await this.sendMessage(message);
        await queue.acknowledge(message.id);
        results.push(result);
      } catch (error) {
        console.error(`Failed to send message ${message.id}:`, error);
      }
    }

    return results;
  }

  async getQueueDepth(channel: Channel): Promise<number> {
    const queue = this.queues.get(channel);
    return queue ? queue.depth() : 0;
  }
}

// Example usage and testing helpers
export function createTestMessage(): Message {
  return new MessageBuilder('test-123', 'Test message', Channel.Email)
    .withSubject('Test Subject')
    .withRecipients(['user-1', 'user-2'])
    .withPriority(MessagePriority.High)
    .build();
}

export function isMessageExpired(message: Message): boolean {
  return message.expiresAt ? message.expiresAt < new Date() : false;
}

export function isMessageScheduled(message: Message): boolean {
  return message.scheduledAt ? message.scheduledAt > new Date() : false;
}
