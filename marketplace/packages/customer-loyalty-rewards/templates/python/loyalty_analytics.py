"""
============================================================================
Loyalty Analytics - Python Segmentation Engine
============================================================================
Production-ready analytics for loyalty program insights
============================================================================
"""

from typing import List, Dict, Any, Optional
from datetime import datetime, timedelta
from dataclasses import dataclass
from enum import Enum
import pandas as pd
import numpy as np


class Tier(Enum):
    """Loyalty tier enumeration"""
    BRONZE = "Bronze"
    SILVER = "Silver"
    GOLD = "Gold"
    PLATINUM = "Platinum"


@dataclass
class LoyaltyMetrics:
    """Loyalty program metrics"""
    total_members: int
    active_members: int
    total_points_issued: int
    total_points_redeemed: int
    redemption_rate: float
    average_points_per_member: float
    tier_distribution: Dict[str, int]


class LoyaltyAnalytics:
    """
    Loyalty program analytics and segmentation engine

    Provides insights into member behavior, engagement, and program ROI
    """

    def __init__(self, members: List[Dict[str, Any]]):
        """Initialize analytics with member data"""
        self.df = pd.DataFrame(members)
        if 'enrollmentDate' in self.df.columns:
            self.df['enrollmentDate'] = pd.to_datetime(self.df['enrollmentDate'])

    def calculate_program_metrics(self) -> LoyaltyMetrics:
        """
        Calculate comprehensive loyalty program metrics

        Returns:
            LoyaltyMetrics with key performance indicators
        """
        total_members = len(self.df)

        # Active members (activity in last 90 days)
        ninety_days_ago = datetime.now() - timedelta(days=90)
        active_members = len(self.df[
            self.df.get('lastActivityDate', pd.NaT) >= ninety_days_ago
        ])

        # Points metrics
        total_points_issued = self.df['lifetimePoints'].sum()
        total_points_redeemed = (
            self.df['lifetimePoints'] - self.df['pointsBalance']
        ).sum()

        # Redemption rate
        redemption_rate = (
            total_points_redeemed / total_points_issued
            if total_points_issued > 0 else 0
        )

        # Average points per member
        avg_points = self.df['pointsBalance'].mean()

        # Tier distribution
        tier_dist = self.df['tier'].value_counts().to_dict()

        return LoyaltyMetrics(
            total_members=total_members,
            active_members=active_members,
            total_points_issued=int(total_points_issued),
            total_points_redeemed=int(total_points_redeemed),
            redemption_rate=redemption_rate,
            average_points_per_member=avg_points,
            tier_distribution=tier_dist
        )

    def segment_members_by_engagement(self) -> pd.DataFrame:
        """
        Segment members by engagement level

        Returns:
            DataFrame with member segments
        """
        # Calculate engagement score
        self.df['engagementScore'] = (
            self.df['lifetimePoints'] / 100 +
            self.df.get('transactionCount', 0) * 10 +
            self.df.get('referralCount', 0) * 50
        )

        # Define segments
        def assign_segment(score):
            if score >= 1000:
                return 'Champions'
            elif score >= 500:
                return 'Loyal Customers'
            elif score >= 200:
                return 'Potential Loyalists'
            elif score >= 50:
                return 'New Customers'
            else:
                return 'At Risk'

        self.df['segment'] = self.df['engagementScore'].apply(assign_segment)

        # Group by segment
        segments = self.df.groupby('segment').agg({
            'memberId': 'count',
            'pointsBalance': 'mean',
            'lifetimePoints': 'mean',
            'engagementScore': 'mean'
        }).round(2)

        segments.columns = ['memberCount', 'avgPoints', 'avgLifetimePoints', 'avgEngagement']

        return segments

    def analyze_tier_progression(self) -> pd.DataFrame:
        """
        Analyze tier upgrade/downgrade patterns

        Returns:
            DataFrame with tier progression statistics
        """
        tier_stats = self.df.groupby('tier').agg({
            'memberId': 'count',
            'pointsBalance': ['mean', 'median'],
            'lifetimePoints': ['mean', 'median'],
        }).round(2)

        tier_stats.columns = ['memberCount', 'avgPoints', 'medianPoints',
                               'avgLifetimePoints', 'medianLifetimePoints']

        return tier_stats

    def identify_churn_risk(self, inactive_days: int = 90) -> pd.DataFrame:
        """
        Identify members at risk of churning

        Args:
            inactive_days: Days of inactivity threshold

        Returns:
            DataFrame with at-risk members
        """
        if 'lastActivityDate' not in self.df.columns:
            return pd.DataFrame()

        threshold_date = datetime.now() - timedelta(days=inactive_days)

        at_risk = self.df[
            self.df['lastActivityDate'] < threshold_date
        ].copy()

        at_risk['daysSinceActivity'] = (
            datetime.now() - at_risk['lastActivityDate']
        ).dt.days

        return at_risk[['memberId', 'tier', 'pointsBalance', 'daysSinceActivity']].sort_values(
            'daysSinceActivity', ascending=False
        )

    def calculate_redemption_patterns(
        self,
        transactions: List[Dict[str, Any]]
    ) -> pd.DataFrame:
        """
        Analyze redemption patterns and preferences

        Args:
            transactions: List of points transactions

        Returns:
            DataFrame with redemption analytics
        """
        txns_df = pd.DataFrame(transactions)

        if txns_df.empty:
            return pd.DataFrame()

        # Filter redemptions
        redemptions = txns_df[txns_df['transactionType'] == 'Redeemed'].copy()

        # Group by reward type
        patterns = redemptions.groupby('rewardType').agg({
            'points': ['count', 'sum', 'mean']
        }).round(2)

        patterns.columns = ['redemptionCount', 'totalPointsRedeemed', 'avgPointsPerRedemption']

        return patterns

    def identify_high_value_members(self, top_n: int = 10) -> pd.DataFrame:
        """
        Identify top members by lifetime value

        Args:
            top_n: Number of top members to return

        Returns:
            DataFrame with high-value member analytics
        """
        high_value = self.df.nlargest(top_n, 'lifetimePoints').copy()

        high_value['pointsRedeemed'] = (
            high_value['lifetimePoints'] - high_value['pointsBalance']
        )

        high_value['redemptionRate'] = (
            high_value['pointsRedeemed'] / high_value['lifetimePoints']
        ).round(2)

        return high_value[[
            'memberId', 'tier', 'lifetimePoints', 'pointsBalance',
            'pointsRedeemed', 'redemptionRate'
        ]]

    def analyze_referral_effectiveness(
        self,
        referrals: List[Dict[str, Any]]
    ) -> pd.DataFrame:
        """
        Analyze referral program effectiveness

        Args:
            referrals: List of referral records

        Returns:
            DataFrame with referral analytics
        """
        referrals_df = pd.DataFrame(referrals)

        if referrals_df.empty:
            return pd.DataFrame()

        # Group by referrer
        referral_stats = referrals_df.groupby('referrerId').agg({
            'refereeId': 'count',
            'pointsAwarded': 'sum'
        }).rename(columns={
            'refereeId': 'successfulReferrals',
            'pointsAwarded': 'totalPointsEarned'
        })

        referral_stats = referral_stats.sort_values(
            'successfulReferrals', ascending=False
        )

        return referral_stats

    def calculate_tier_roi(
        self,
        revenue_data: pd.DataFrame
    ) -> pd.DataFrame:
        """
        Calculate ROI by tier level

        Args:
            revenue_data: DataFrame with member revenue data

        Returns:
            DataFrame with tier-level ROI
        """
        # Merge member data with revenue
        roi_df = self.df.merge(revenue_data, on='memberId', how='left')

        # Calculate metrics by tier
        tier_roi = roi_df.groupby('tier').agg({
            'revenue': ['sum', 'mean', 'count'],
            'pointsBalance': 'sum',
            'lifetimePoints': 'sum'
        }).round(2)

        tier_roi.columns = [
            'totalRevenue', 'avgRevenue', 'memberCount',
            'totalPointsBalance', 'totalLifetimePoints'
        ]

        # Calculate points cost (assume $0.01 per point)
        tier_roi['pointsCost'] = tier_roi['totalLifetimePoints'] * 0.01

        # Calculate ROI
        tier_roi['roi'] = (
            (tier_roi['totalRevenue'] - tier_roi['pointsCost']) /
            tier_roi['pointsCost']
        ).round(2)

        return tier_roi

    def generate_monthly_report(self, year: int, month: int) -> Dict[str, Any]:
        """
        Generate comprehensive monthly report

        Args:
            year: Report year
            month: Report month

        Returns:
            Dictionary with monthly metrics
        """
        # Filter members enrolled in month
        monthly_df = self.df[
            (self.df['enrollmentDate'].dt.year == year) &
            (self.df['enrollmentDate'].dt.month == month)
        ]

        return {
            'period': f'{year}-{month:02d}',
            'new_members': len(monthly_df),
            'total_members': len(self.df),
            'tier_distribution': monthly_df['tier'].value_counts().to_dict(),
            'avg_points_per_new_member': monthly_df['pointsBalance'].mean(),
            'total_points_issued': monthly_df['lifetimePoints'].sum()
        }


# ============================================================================
# Example Usage
# ============================================================================

if __name__ == '__main__':
    # Sample member data
    sample_members = [
        {
            'memberId': 'MEM-001',
            'tier': 'Gold',
            'pointsBalance': 5000,
            'lifetimePoints': 8000,
            'enrollmentDate': '2024-01-15T10:00:00Z',
            'lastActivityDate': '2025-01-05T14:00:00Z'
        },
        {
            'memberId': 'MEM-002',
            'tier': 'Silver',
            'pointsBalance': 1500,
            'lifetimePoints': 2000,
            'enrollmentDate': '2024-06-20T10:00:00Z',
            'lastActivityDate': '2025-01-07T14:00:00Z'
        }
    ]

    # Initialize analytics
    analytics = LoyaltyAnalytics(sample_members)

    # Calculate metrics
    metrics = analytics.calculate_program_metrics()
    print(f"Total Members: {metrics.total_members}")
    print(f"Active Members: {metrics.active_members}")
    print(f"Redemption Rate: {metrics.redemption_rate:.2%}")

    # Segment members
    segments = analytics.segment_members_by_engagement()
    print("\nMember Segments:")
    print(segments)
