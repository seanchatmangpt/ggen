"""
Multi-Tenant SaaS - Billing and Analytics Engine (Python)
Handles usage metering, billing calculation, and tenant analytics
"""

from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import List, Dict, Optional
from decimal import Decimal


@dataclass
class UsageMetric:
    """Usage metric for a tenant"""
    tenant_id: str
    metric_type: str  # 'api_calls', 'storage', 'compute'
    value: Decimal
    timestamp: datetime


@dataclass
class SubscriptionTier:
    """Subscription tier pricing"""
    name: str
    base_price: Decimal
    api_calls_included: int
    storage_gb_included: int
    overage_api_per_1k: Decimal
    overage_storage_per_gb: Decimal


@dataclass
class BillingRecord:
    """Billing invoice for a tenant"""
    tenant_id: str
    billing_period_start: datetime
    billing_period_end: datetime
    base_charge: Decimal
    overage_charges: Decimal
    total_amount: Decimal
    currency: str = "USD"


class TenantBillingEngine:
    """
    Billing engine for calculating tenant charges based on usage and tier
    """

    TIERS = {
        'Free': SubscriptionTier(
            name='Free',
            base_price=Decimal('0.00'),
            api_calls_included=1000,
            storage_gb_included=1,
            overage_api_per_1k=Decimal('0.00'),  # No overage allowed
            overage_storage_per_gb=Decimal('0.00'),
        ),
        'Basic': SubscriptionTier(
            name='Basic',
            base_price=Decimal('29.00'),
            api_calls_included=50000,
            storage_gb_included=10,
            overage_api_per_1k=Decimal('0.50'),
            overage_storage_per_gb=Decimal('2.00'),
        ),
        'Pro': SubscriptionTier(
            name='Pro',
            base_price=Decimal('99.00'),
            api_calls_included=500000,
            storage_gb_included=100,
            overage_api_per_1k=Decimal('0.30'),
            overage_storage_per_gb=Decimal('1.00'),
        ),
        'Enterprise': SubscriptionTier(
            name='Enterprise',
            base_price=Decimal('499.00'),
            api_calls_included=5000000,
            storage_gb_included=1000,
            overage_api_per_1k=Decimal('0.10'),
            overage_storage_per_gb=Decimal('0.50'),
        ),
    }

    def __init__(self):
        self.usage_cache: Dict[str, List[UsageMetric]] = {}

    def record_usage(self, metric: UsageMetric):
        """Record a usage metric for billing calculation"""
        if metric.tenant_id not in self.usage_cache:
            self.usage_cache[metric.tenant_id] = []
        self.usage_cache[metric.tenant_id].append(metric)

    def calculate_billing(
        self,
        tenant_id: str,
        tier_name: str,
        period_start: datetime,
        period_end: datetime,
    ) -> BillingRecord:
        """
        Calculate billing for a tenant for the given period
        """
        tier = self.TIERS.get(tier_name)
        if not tier:
            raise ValueError(f"Invalid subscription tier: {tier_name}")

        # Aggregate usage metrics for the period
        usage = self._aggregate_usage(tenant_id, period_start, period_end)

        # Calculate base charge
        base_charge = tier.base_price

        # Calculate overage charges
        overage_api = max(0, usage['api_calls'] - tier.api_calls_included)
        overage_storage = max(0, usage['storage_gb'] - tier.storage_gb_included)

        overage_charges = (
            (overage_api / 1000) * tier.overage_api_per_1k +
            overage_storage * tier.overage_storage_per_gb
        )

        total_amount = base_charge + overage_charges

        return BillingRecord(
            tenant_id=tenant_id,
            billing_period_start=period_start,
            billing_period_end=period_end,
            base_charge=base_charge,
            overage_charges=overage_charges,
            total_amount=total_amount,
        )

    def _aggregate_usage(
        self,
        tenant_id: str,
        period_start: datetime,
        period_end: datetime,
    ) -> Dict[str, Decimal]:
        """
        Aggregate usage metrics for a tenant in the given period
        """
        metrics = self.usage_cache.get(tenant_id, [])

        # Filter metrics within the period
        period_metrics = [
            m for m in metrics
            if period_start <= m.timestamp <= period_end
        ]

        # Aggregate by metric type
        usage = {
            'api_calls': Decimal(0),
            'storage_gb': Decimal(0),
            'compute_hours': Decimal(0),
        }

        for metric in period_metrics:
            if metric.metric_type == 'api_calls':
                usage['api_calls'] += metric.value
            elif metric.metric_type == 'storage':
                # Assume storage is in GB
                usage['storage_gb'] = max(usage['storage_gb'], metric.value)
            elif metric.metric_type == 'compute':
                usage['compute_hours'] += metric.value

        return usage

    def generate_invoice(self, billing_record: BillingRecord) -> str:
        """
        Generate invoice text for a billing record
        """
        invoice = f"""
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                    MULTI-TENANT SAAS INVOICE
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Tenant ID: {billing_record.tenant_id}
Billing Period: {billing_record.billing_period_start.date()} to {billing_record.billing_period_end.date()}

CHARGES:
  Base Subscription:     {billing_record.currency} {billing_record.base_charge:.2f}
  Overage Charges:       {billing_record.currency} {billing_record.overage_charges:.2f}
  ─────────────────────────────────────────────────────────
  TOTAL DUE:             {billing_record.currency} {billing_record.total_amount:.2f}

Payment due: {(billing_record.billing_period_end + timedelta(days=7)).date()}
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
        """
        return invoice.strip()


class TenantAnalytics:
    """
    Analytics engine for tenant insights and monitoring
    """

    def __init__(self, billing_engine: TenantBillingEngine):
        self.billing_engine = billing_engine

    def calculate_mrr(self, active_tenants: List[Dict]) -> Decimal:
        """
        Calculate Monthly Recurring Revenue (MRR)
        """
        mrr = Decimal(0)
        for tenant in active_tenants:
            tier = TenantBillingEngine.TIERS.get(tenant['tier'])
            if tier:
                mrr += tier.base_price
        return mrr

    def identify_churn_risk(self, tenant_id: str, recent_usage: List[UsageMetric]) -> bool:
        """
        Identify tenants at risk of churn based on declining usage
        """
        if len(recent_usage) < 30:
            return False

        # Compare recent 7 days vs previous 7 days
        now = datetime.now()
        recent_week = [m for m in recent_usage if m.timestamp >= now - timedelta(days=7)]
        previous_week = [m for m in recent_usage if now - timedelta(days=14) <= m.timestamp < now - timedelta(days=7)]

        recent_api_calls = sum(m.value for m in recent_week if m.metric_type == 'api_calls')
        previous_api_calls = sum(m.value for m in previous_week if m.metric_type == 'api_calls')

        # Churn risk if usage dropped by 50%+
        if previous_api_calls > 0 and recent_api_calls / previous_api_calls < 0.5:
            return True

        return False

    def recommend_tier_upgrade(self, tenant_id: str, current_tier: str, usage: Dict) -> Optional[str]:
        """
        Recommend tier upgrade if tenant is consistently exceeding limits
        """
        tier = TenantBillingEngine.TIERS.get(current_tier)
        if not tier:
            return None

        # Check if overage is significant
        api_overage = max(0, usage.get('api_calls', 0) - tier.api_calls_included)
        storage_overage = max(0, usage.get('storage_gb', 0) - tier.storage_gb_included)

        if api_overage > tier.api_calls_included * 0.3:  # 30% overage
            # Recommend next tier
            tier_order = ['Free', 'Basic', 'Pro', 'Enterprise']
            current_idx = tier_order.index(current_tier)
            if current_idx < len(tier_order) - 1:
                return tier_order[current_idx + 1]

        return None


# Example usage
if __name__ == '__main__':
    billing_engine = TenantBillingEngine()
    analytics = TenantAnalytics(billing_engine)

    # Record usage for a tenant
    tenant_id = "tenant_acme"
    billing_engine.record_usage(UsageMetric(
        tenant_id=tenant_id,
        metric_type='api_calls',
        value=Decimal('75000'),
        timestamp=datetime.now(),
    ))
    billing_engine.record_usage(UsageMetric(
        tenant_id=tenant_id,
        metric_type='storage',
        value=Decimal('15'),  # GB
        timestamp=datetime.now(),
    ))

    # Calculate billing
    period_start = datetime.now().replace(day=1, hour=0, minute=0, second=0, microsecond=0)
    period_end = period_start + timedelta(days=30)

    billing_record = billing_engine.calculate_billing(
        tenant_id=tenant_id,
        tier_name='Basic',
        period_start=period_start,
        period_end=period_end,
    )

    # Generate invoice
    invoice = billing_engine.generate_invoice(billing_record)
    print(invoice)

    # Check tier recommendation
    usage = {'api_calls': 75000, 'storage_gb': 15}
    recommendation = analytics.recommend_tier_upgrade(tenant_id, 'Basic', usage)
    if recommendation:
        print(f"\nRecommendation: Upgrade to {recommendation} tier")
