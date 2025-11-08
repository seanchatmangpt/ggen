"""
============================================================================
Order Analytics - Python Reporting Engine
============================================================================
Production-ready analytics for order processing insights
============================================================================
"""

from typing import List, Dict, Any, Optional
from datetime import datetime, timedelta
from dataclasses import dataclass
from enum import Enum
import pandas as pd
import numpy as np


class OrderStatus(Enum):
    """Order status enumeration"""
    CART = "Cart"
    PENDING = "Pending"
    PROCESSING = "Processing"
    SHIPPED = "Shipped"
    DELIVERED = "Delivered"
    CANCELLED = "Cancelled"
    REFUNDED = "Refunded"
    ON_HOLD = "OnHold"


@dataclass
class OrderMetrics:
    """Order analytics metrics"""
    total_orders: int
    total_revenue: float
    average_order_value: float
    conversion_rate: float
    cancellation_rate: float
    return_rate: float


class OrderAnalytics:
    """
    Order analytics and reporting engine

    Provides insights into order patterns, revenue, and customer behavior
    """

    def __init__(self, orders: List[Dict[str, Any]]):
        """Initialize analytics with order data"""
        self.df = pd.DataFrame(orders)
        self.df['orderDate'] = pd.to_datetime(self.df['orderDate'])

    def calculate_revenue_by_period(
        self,
        period: str = 'D',
        start_date: Optional[datetime] = None,
        end_date: Optional[datetime] = None
    ) -> pd.DataFrame:
        """
        Calculate revenue by time period

        Args:
            period: Time period ('D' for day, 'W' for week, 'M' for month)
            start_date: Optional start date filter
            end_date: Optional end date filter

        Returns:
            DataFrame with period and revenue
        """
        df = self.df.copy()

        # Filter by date range
        if start_date:
            df = df[df['orderDate'] >= start_date]
        if end_date:
            df = df[df['orderDate'] <= end_date]

        # Exclude cancelled and refunded orders
        df = df[~df['status'].isin([
            OrderStatus.CANCELLED.value,
            OrderStatus.REFUNDED.value
        ])]

        # Group by period and sum revenue
        revenue = df.groupby(df['orderDate'].dt.to_period(period)).agg({
            'total': 'sum',
            'orderNumber': 'count'
        }).rename(columns={'total': 'revenue', 'orderNumber': 'orderCount'})

        return revenue

    def calculate_order_metrics(self) -> OrderMetrics:
        """
        Calculate comprehensive order metrics

        Returns:
            OrderMetrics with key performance indicators
        """
        total_orders = len(self.df)

        # Total revenue (excluding cancelled/refunded)
        valid_orders = self.df[~self.df['status'].isin([
            OrderStatus.CANCELLED.value,
            OrderStatus.REFUNDED.value
        ])]
        total_revenue = valid_orders['total'].sum()

        # Average order value
        average_order_value = valid_orders['total'].mean() if len(valid_orders) > 0 else 0

        # Conversion rate (cart to order)
        cart_count = len(self.df[self.df['status'] == OrderStatus.CART.value])
        order_count = len(self.df[self.df['status'] != OrderStatus.CART.value])
        conversion_rate = order_count / (cart_count + order_count) if (cart_count + order_count) > 0 else 0

        # Cancellation rate
        cancelled_count = len(self.df[self.df['status'] == OrderStatus.CANCELLED.value])
        cancellation_rate = cancelled_count / total_orders if total_orders > 0 else 0

        # Return rate
        refunded_count = len(self.df[self.df['status'] == OrderStatus.REFUNDED.value])
        return_rate = refunded_count / total_orders if total_orders > 0 else 0

        return OrderMetrics(
            total_orders=total_orders,
            total_revenue=total_revenue,
            average_order_value=average_order_value,
            conversion_rate=conversion_rate,
            cancellation_rate=cancellation_rate,
            return_rate=return_rate
        )

    def analyze_fulfillment_time(self) -> pd.DataFrame:
        """
        Analyze average time from order to delivery

        Returns:
            DataFrame with fulfillment time statistics
        """
        # Filter delivered orders with shipment data
        delivered = self.df[self.df['status'] == OrderStatus.DELIVERED.value].copy()

        if len(delivered) == 0:
            return pd.DataFrame()

        # Calculate fulfillment time
        delivered['fulfillmentTime'] = (
            pd.to_datetime(delivered['shipment'].apply(lambda x: x.get('actualDelivery'))) -
            delivered['orderDate']
        ).dt.total_seconds() / 3600  # Convert to hours

        # Group by carrier
        stats = delivered.groupby('shipment.carrier')['fulfillmentTime'].agg([
            'count', 'mean', 'median', 'std', 'min', 'max'
        ]).round(2)

        return stats

    def identify_high_value_customers(self, top_n: int = 10) -> pd.DataFrame:
        """
        Identify top customers by total order value

        Args:
            top_n: Number of top customers to return

        Returns:
            DataFrame with customer analytics
        """
        # Exclude cancelled/refunded orders
        valid_orders = self.df[~self.df['status'].isin([
            OrderStatus.CANCELLED.value,
            OrderStatus.REFUNDED.value
        ])]

        # Group by customer
        customer_stats = valid_orders.groupby('customerId').agg({
            'total': ['sum', 'mean', 'count']
        }).round(2)

        customer_stats.columns = ['totalRevenue', 'avgOrderValue', 'orderCount']
        customer_stats = customer_stats.sort_values('totalRevenue', ascending=False)

        return customer_stats.head(top_n)

    def analyze_product_performance(self) -> pd.DataFrame:
        """
        Analyze product sales performance

        Returns:
            DataFrame with product sales metrics
        """
        # Extract all order items
        items = []
        for _, order in self.df.iterrows():
            if order['status'] not in [OrderStatus.CANCELLED.value, OrderStatus.REFUNDED.value]:
                for item in order['items']:
                    items.append({
                        'sku': item['sku'],
                        'productId': item['productId'],
                        'quantity': item['quantity'],
                        'revenue': item['lineTotal']
                    })

        items_df = pd.DataFrame(items)

        # Group by SKU
        product_stats = items_df.groupby('sku').agg({
            'quantity': 'sum',
            'revenue': 'sum',
            'productId': 'count'
        }).rename(columns={'productId': 'orderCount'}).round(2)

        product_stats = product_stats.sort_values('revenue', ascending=False)

        return product_stats

    def generate_daily_report(self, date: datetime) -> Dict[str, Any]:
        """
        Generate comprehensive daily report

        Args:
            date: Date for report

        Returns:
            Dictionary with daily metrics
        """
        daily_orders = self.df[self.df['orderDate'].dt.date == date.date()]

        valid_orders = daily_orders[~daily_orders['status'].isin([
            OrderStatus.CANCELLED.value,
            OrderStatus.REFUNDED.value
        ])]

        return {
            'date': date.strftime('%Y-%m-%d'),
            'total_orders': len(daily_orders),
            'revenue': valid_orders['total'].sum(),
            'avg_order_value': valid_orders['total'].mean() if len(valid_orders) > 0 else 0,
            'orders_by_status': daily_orders['status'].value_counts().to_dict(),
            'top_products': self._get_top_products(daily_orders, top_n=5)
        }

    def _get_top_products(self, orders: pd.DataFrame, top_n: int = 5) -> List[Dict[str, Any]]:
        """Extract top selling products from orders"""
        items = []
        for _, order in orders.iterrows():
            for item in order['items']:
                items.append({
                    'sku': item['sku'],
                    'quantity': item['quantity']
                })

        if not items:
            return []

        items_df = pd.DataFrame(items)
        top = items_df.groupby('sku')['quantity'].sum().sort_values(ascending=False).head(top_n)

        return [{'sku': sku, 'quantity': int(qty)} for sku, qty in top.items()]


# ============================================================================
# Example Usage
# ============================================================================

if __name__ == '__main__':
    # Sample order data
    sample_orders = [
        {
            'orderNumber': 'ORD-001',
            'customerId': 'CUST-001',
            'status': 'Delivered',
            'total': 150.00,
            'orderDate': '2025-01-01T10:00:00Z',
            'items': [
                {'sku': 'SKU-001', 'productId': 'PROD-001', 'quantity': 2, 'lineTotal': 100.00},
                {'sku': 'SKU-002', 'productId': 'PROD-002', 'quantity': 1, 'lineTotal': 50.00}
            ],
            'shipment': {'carrier': 'FedEx', 'actualDelivery': '2025-01-05T14:00:00Z'}
        }
    ]

    # Initialize analytics
    analytics = OrderAnalytics(sample_orders)

    # Calculate metrics
    metrics = analytics.calculate_order_metrics()
    print(f"Total Orders: {metrics.total_orders}")
    print(f"Total Revenue: ${metrics.total_revenue:.2f}")
    print(f"Average Order Value: ${metrics.average_order_value:.2f}")
