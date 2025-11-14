"""
Notification & Messaging Hub - Python Implementation
Enterprise multi-channel notification system
"""

from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Dict, List, Optional, Protocol
from abc import ABC, abstractmethod
import asyncio


class MessagePriority(Enum):
    """Message priority levels"""
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


class DeliveryStatus(Enum):
    """Delivery status enumeration"""
    PENDING = "pending"
    QUEUED = "queued"
    SENDING = "sending"
    DELIVERED = "delivered"
    FAILED = "failed"
    BOUNCED = "bounced"
    EXPIRED = "expired"


class Channel(Enum):
    """Message channel types"""
    EMAIL = "email"
    SMS = "sms"
    PUSH = "push"
    WEBHOOK = "webhook"
    IN_APP = "in_app"
    SLACK = "slack"
    TEAMS = "teams"


@dataclass
class Message:
    """Core message structure"""
    id: str
    body: str
    channel: Channel
    recipient_ids: List[str]
    priority: MessagePriority = MessagePriority.MEDIUM
    subject: Optional[str] = None
    html_body: Optional[str] = None
    template_id: Optional[str] = None
    template_vars: Dict[str, str] = field(default_factory=dict)
    created_at: datetime = field(default_factory=datetime.utcnow)
    scheduled_at: Optional[datetime] = None
    expires_at: Optional[datetime] = None
    metadata: Dict[str, str] = field(default_factory=dict)

    def is_expired(self) -> bool:
        """Check if message has expired"""
        if self.expires_at:
            return self.expires_at < datetime.utcnow()
        return False

    def is_scheduled(self) -> bool:
        """Check if message is scheduled for future"""
        if self.scheduled_at:
            return self.scheduled_at > datetime.utcnow()
        return False


@dataclass
class DeliveryAttempt:
    """Delivery attempt tracking"""
    message_id: str
    attempt_number: int
    status: DeliveryStatus
    provider: str
    attempted_at: datetime
    completed_at: Optional[datetime] = None
    failure_reason: Optional[str] = None
    response_time_ms: Optional[int] = None


@dataclass
class RateLimit:
    """Rate limiting configuration"""
    channel: Channel
    max_per_minute: int
    max_per_hour: int
    max_per_day: int
    burst_size: int

    @staticmethod
    def for_channel(channel: Channel) -> 'RateLimit':
        """Get default rate limit for channel"""
        limits = {
            Channel.EMAIL: RateLimit(
                channel=Channel.EMAIL,
                max_per_minute=100,
                max_per_hour=5000,
                max_per_day=50000,
                burst_size=200,
            ),
            Channel.SMS: RateLimit(
                channel=Channel.SMS,
                max_per_minute=50,
                max_per_hour=1000,
                max_per_day=10000,
                burst_size=100,
            ),
            Channel.PUSH: RateLimit(
                channel=Channel.PUSH,
                max_per_minute=1000,
                max_per_hour=50000,
                max_per_day=500000,
                burst_size=2000,
            ),
        }
        return limits.get(channel, RateLimit(
            channel=channel,
            max_per_minute=100,
            max_per_hour=5000,
            max_per_day=50000,
            burst_size=200,
        ))

    def check_limit(self, current_count: int, window: str) -> bool:
        """Check if current count is within limit"""
        if window == "minute":
            return current_count < self.max_per_minute
        elif window == "hour":
            return current_count < self.max_per_hour
        elif window == "day":
            return current_count < self.max_per_day
        return False


class MessageProvider(Protocol):
    """Message provider interface"""

    async def send(self, message: Message) -> DeliveryAttempt:
        """Send a message"""
        ...

    async def get_status(self, message_id: str) -> DeliveryStatus:
        """Get delivery status"""
        ...

    def supports_channel(self, channel: Channel) -> bool:
        """Check if provider supports channel"""
        ...

    def provider_name(self) -> str:
        """Get provider name"""
        ...


class MessageQueue(Protocol):
    """Message queue interface"""

    async def enqueue(self, message: Message) -> None:
        """Add message to queue"""
        ...

    async def dequeue(self, count: int) -> List[Message]:
        """Remove messages from queue"""
        ...

    async def peek(self, count: int) -> List[Message]:
        """View messages without removing"""
        ...

    async def depth(self) -> int:
        """Get queue depth"""
        ...

    async def acknowledge(self, message_id: str) -> None:
        """Acknowledge message processing"""
        ...


class RateLimitManager:
    """Rate limit management"""

    def __init__(self):
        self.limits: Dict[Channel, RateLimit] = {}
        self.counters: Dict[str, int] = {}
        self.windows: Dict[str, datetime] = {}
        self._initialize_limits()

    def _initialize_limits(self):
        """Initialize default rate limits"""
        for channel in [Channel.EMAIL, Channel.SMS, Channel.PUSH]:
            self.limits[channel] = RateLimit.for_channel(channel)

    def set_limit(self, channel: Channel, limit: RateLimit):
        """Set rate limit for channel"""
        self.limits[channel] = limit

    async def check_limit(self, channel: Channel, window: str) -> bool:
        """Check if within rate limit"""
        limit = self.limits.get(channel)
        if not limit:
            return True

        key = f"{channel.value}:{window}"
        now = datetime.utcnow()
        window_start = self.windows.get(key)

        # Reset window if expired
        if window_start and self._is_window_expired(window_start, window):
            self.counters[key] = 0
            self.windows[key] = now

        count = self.counters.get(key, 0)
        return limit.check_limit(count, window)

    async def increment(self, channel: Channel, window: str):
        """Increment rate limit counter"""
        key = f"{channel.value}:{window}"
        self.counters[key] = self.counters.get(key, 0) + 1

        if key not in self.windows:
            self.windows[key] = datetime.utcnow()

    def _is_window_expired(self, start: datetime, window: str) -> bool:
        """Check if time window has expired"""
        now = datetime.utcnow()
        diff = (now - start).total_seconds()

        if window == "minute":
            return diff > 60
        elif window == "hour":
            return diff > 3600
        elif window == "day":
            return diff > 86400
        return False


class NotificationHub:
    """Main notification hub coordinator"""

    def __init__(self):
        self.providers: Dict[Channel, List[MessageProvider]] = {}
        self.queues: Dict[Channel, MessageQueue] = {}
        self.rate_limit_manager = RateLimitManager()

    def register_provider(self, channel: Channel, provider: MessageProvider):
        """Register a message provider"""
        if channel not in self.providers:
            self.providers[channel] = []
        self.providers[channel].append(provider)

    def register_queue(self, channel: Channel, queue: MessageQueue):
        """Register a message queue"""
        self.queues[channel] = queue

    async def send_message(self, message: Message) -> DeliveryAttempt:
        """Send a message through appropriate channel"""
        # Check expiration
        if message.is_expired():
            raise ValueError("Message expired")

        # Check rate limits
        can_send = await self.rate_limit_manager.check_limit(
            message.channel, "minute"
        )
        if not can_send:
            # Queue for later
            queue = self.queues.get(message.channel)
            if queue:
                await queue.enqueue(message)
                return DeliveryAttempt(
                    message_id=message.id,
                    attempt_number=0,
                    status=DeliveryStatus.QUEUED,
                    provider="queue",
                    attempted_at=datetime.utcnow(),
                )
            raise ValueError("Rate limit exceeded and no queue available")

        # Check if scheduled
        if message.is_scheduled():
            queue = self.queues.get(message.channel)
            if queue:
                await queue.enqueue(message)
                return DeliveryAttempt(
                    message_id=message.id,
                    attempt_number=0,
                    status=DeliveryStatus.QUEUED,
                    provider="queue",
                    attempted_at=datetime.utcnow(),
                )

        # Get providers for channel
        providers = self.providers.get(message.channel)
        if not providers:
            raise ValueError(f"No provider for channel: {message.channel}")

        # Try providers in order
        last_error = None
        for provider in providers:
            try:
                result = await provider.send(message)
                await self.rate_limit_manager.increment(message.channel, "minute")
                return result
            except Exception as e:
                last_error = e
                print(f"Provider {provider.provider_name()} failed: {e}")
                continue

        raise last_error or ValueError("All providers failed")

    async def process_queue(
        self, channel: Channel, batch_size: int
    ) -> List[DeliveryAttempt]:
        """Process messages from queue"""
        queue = self.queues.get(channel)
        if not queue:
            raise ValueError(f"No queue for channel: {channel}")

        messages = await queue.dequeue(batch_size)
        results = []

        for message in messages:
            try:
                result = await self.send_message(message)
                await queue.acknowledge(message.id)
                results.append(result)
            except Exception as e:
                print(f"Failed to send message {message.id}: {e}")

        return results

    async def get_queue_depth(self, channel: Channel) -> int:
        """Get queue depth for channel"""
        queue = self.queues.get(channel)
        return await queue.depth() if queue else 0


class MessageBuilder:
    """Fluent builder for messages"""

    def __init__(self, id: str, body: str, channel: Channel):
        self._message = Message(
            id=id,
            body=body,
            channel=channel,
            recipient_ids=[],
        )

    def with_subject(self, subject: str) -> 'MessageBuilder':
        """Set message subject"""
        self._message.subject = subject
        return self

    def with_html_body(self, html_body: str) -> 'MessageBuilder':
        """Set HTML body"""
        self._message.html_body = html_body
        return self

    def with_priority(self, priority: MessagePriority) -> 'MessageBuilder':
        """Set message priority"""
        self._message.priority = priority
        return self

    def with_recipients(self, recipient_ids: List[str]) -> 'MessageBuilder':
        """Set recipients"""
        self._message.recipient_ids = recipient_ids
        return self

    def with_template(
        self, template_id: str, vars: Dict[str, str]
    ) -> 'MessageBuilder':
        """Set template"""
        self._message.template_id = template_id
        self._message.template_vars = vars
        return self

    def schedule(self, at: datetime) -> 'MessageBuilder':
        """Schedule message"""
        self._message.scheduled_at = at
        return self

    def expires_at(self, at: datetime) -> 'MessageBuilder':
        """Set expiration"""
        self._message.expires_at = at
        return self

    def with_metadata(self, metadata: Dict[str, str]) -> 'MessageBuilder':
        """Add metadata"""
        self._message.metadata.update(metadata)
        return self

    def build(self) -> Message:
        """Build the message"""
        if not self._message.recipient_ids:
            raise ValueError("Message must have at least one recipient")
        return self._message
