// ============================================================================
// Customer Loyalty UI - TypeScript React Components
// ============================================================================
// Production-ready loyalty program interface
// ============================================================================

import React, { useState, useEffect } from 'react';

// ============================================================================
// Types
// ============================================================================

export enum Tier {
  Bronze = 'Bronze',
  Silver = 'Silver',
  Gold = 'Gold',
  Platinum = 'Platinum',
}

export interface Member {
  memberId: string;
  tier: Tier;
  pointsBalance: number;
  lifetimePoints: number;
  enrollmentDate: Date;
}

export interface Reward {
  rewardId: string;
  name: string;
  pointsCost: number;
  rewardType: RewardType;
}

export type RewardType =
  | { type: 'discount'; amount: number }
  | { type: 'percentage'; percentage: number }
  | { type: 'freeShipping' }
  | { type: 'product'; productId: string };

export interface Badge {
  badgeId: string;
  name: string;
  description: string;
  icon: string;
}

export interface PointsTransaction {
  transactionId: string;
  points: number;
  transactionType: 'Earned' | 'Redeemed' | 'Expired';
  timestamp: Date;
  description: string;
}

// ============================================================================
// Loyalty Service
// ============================================================================

export class LoyaltyService {
  private baseUrl: string;

  constructor(baseUrl: string = '/api/loyalty') {
    this.baseUrl = baseUrl;
  }

  async getMember(memberId: string): Promise<Member> {
    const response = await fetch(`${this.baseUrl}/members/${memberId}`);
    return response.json();
  }

  async getAvailableRewards(memberId: string): Promise<Reward[]> {
    const response = await fetch(
      `${this.baseUrl}/members/${memberId}/rewards`
    );
    return response.json();
  }

  async redeemReward(
    memberId: string,
    rewardId: string
  ): Promise<{ success: boolean; voucher?: string }> {
    const response = await fetch(
      `${this.baseUrl}/members/${memberId}/redeem`,
      {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ rewardId }),
      }
    );
    return response.json();
  }

  async getPointsHistory(memberId: string): Promise<PointsTransaction[]> {
    const response = await fetch(
      `${this.baseUrl}/members/${memberId}/history`
    );
    return response.json();
  }

  async getBadges(memberId: string): Promise<Badge[]> {
    const response = await fetch(`${this.baseUrl}/members/${memberId}/badges`);
    return response.json();
  }
}

// ============================================================================
// Member Dashboard Component
// ============================================================================

export const MemberDashboard: React.FC<{ memberId: string }> = ({
  memberId,
}) => {
  const [member, setMember] = useState<Member | null>(null);
  const [rewards, setRewards] = useState<Reward[]>([]);
  const service = new LoyaltyService();

  useEffect(() => {
    loadMemberData();
  }, [memberId]);

  const loadMemberData = async () => {
    const [memberData, rewardsData] = await Promise.all([
      service.getMember(memberId),
      service.getAvailableRewards(memberId),
    ]);
    setMember(memberData);
    setRewards(rewardsData);
  };

  if (!member) {
    return <div>Loading...</div>;
  }

  return (
    <div className="loyalty-dashboard">
      <MemberStatus member={member} />
      <RewardsCatalog member={member} rewards={rewards} />
      <TierProgress member={member} />
    </div>
  );
};

// ============================================================================
// Member Status Component
// ============================================================================

const MemberStatus: React.FC<{ member: Member }> = ({ member }) => {
  const tierColor = {
    Bronze: '#CD7F32',
    Silver: '#C0C0C0',
    Gold: '#FFD700',
    Platinum: '#E5E4E2',
  };

  return (
    <div className="member-status">
      <h2>Your Loyalty Status</h2>
      <div
        className="tier-badge"
        style={{ backgroundColor: tierColor[member.tier] }}
      >
        <span className="tier-name">{member.tier}</span>
      </div>
      <div className="points-display">
        <div className="points-balance">
          <span className="points-value">{member.pointsBalance}</span>
          <span className="points-label">Available Points</span>
        </div>
        <div className="lifetime-points">
          <span className="lifetime-value">{member.lifetimePoints}</span>
          <span className="lifetime-label">Lifetime Points</span>
        </div>
      </div>
    </div>
  );
};

// ============================================================================
// Rewards Catalog Component
// ============================================================================

const RewardsCatalog: React.FC<{ member: Member; rewards: Reward[] }> = ({
  member,
  rewards,
}) => {
  const [selectedReward, setSelectedReward] = useState<Reward | null>(null);
  const service = new LoyaltyService();

  const handleRedeem = async (reward: Reward) => {
    const result = await service.redeemReward(member.memberId, reward.rewardId);
    if (result.success) {
      alert(`Reward redeemed! Voucher: ${result.voucher}`);
    }
  };

  return (
    <div className="rewards-catalog">
      <h3>Available Rewards</h3>
      <div className="rewards-grid">
        {rewards.map((reward) => (
          <RewardCard
            key={reward.rewardId}
            reward={reward}
            onRedeem={() => handleRedeem(reward)}
            canAfford={member.pointsBalance >= reward.pointsCost}
          />
        ))}
      </div>
    </div>
  );
};

// ============================================================================
// Reward Card Component
// ============================================================================

const RewardCard: React.FC<{
  reward: Reward;
  onRedeem: () => void;
  canAfford: boolean;
}> = ({ reward, onRedeem, canAfford }) => {
  const getRewardDescription = (type: RewardType): string => {
    if (type.type === 'discount') {
      return `$${type.amount} off your next order`;
    } else if (type.type === 'percentage') {
      return `${type.percentage}% off your next order`;
    } else if (type.type === 'freeShipping') {
      return 'Free shipping on your next order';
    } else {
      return 'Free product reward';
    }
  };

  return (
    <div className={`reward-card ${canAfford ? '' : 'disabled'}`}>
      <h4>{reward.name}</h4>
      <p>{getRewardDescription(reward.rewardType)}</p>
      <div className="reward-cost">
        <span className="points-cost">{reward.pointsCost} points</span>
      </div>
      <button onClick={onRedeem} disabled={!canAfford}>
        {canAfford ? 'Redeem' : 'Not Enough Points'}
      </button>
    </div>
  );
};

// ============================================================================
// Tier Progress Component
// ============================================================================

const TierProgress: React.FC<{ member: Member }> = ({ member }) => {
  const tierLevels = [
    { name: Tier.Bronze, minPoints: 0 },
    { name: Tier.Silver, minPoints: 1000 },
    { name: Tier.Gold, minPoints: 5000 },
    { name: Tier.Platinum, minPoints: 10000 },
  ];

  const currentTierIndex = tierLevels.findIndex(
    (t) => t.name === member.tier
  );
  const nextTier = tierLevels[currentTierIndex + 1];

  if (!nextTier) {
    return (
      <div className="tier-progress">
        <p>You've reached the highest tier!</p>
      </div>
    );
  }

  const pointsNeeded = nextTier.minPoints - member.lifetimePoints;
  const progress =
    ((member.lifetimePoints - tierLevels[currentTierIndex].minPoints) /
      (nextTier.minPoints - tierLevels[currentTierIndex].minPoints)) *
    100;

  return (
    <div className="tier-progress">
      <h3>Progress to {nextTier.name}</h3>
      <div className="progress-bar">
        <div className="progress-fill" style={{ width: `${progress}%` }} />
      </div>
      <p>
        {pointsNeeded} points until {nextTier.name} tier
      </p>
    </div>
  );
};

// ============================================================================
// Points History Component
// ============================================================================

export const PointsHistory: React.FC<{ memberId: string }> = ({ memberId }) => {
  const [transactions, setTransactions] = useState<PointsTransaction[]>([]);
  const service = new LoyaltyService();

  useEffect(() => {
    loadHistory();
  }, [memberId]);

  const loadHistory = async () => {
    const data = await service.getPointsHistory(memberId);
    setTransactions(data);
  };

  return (
    <div className="points-history">
      <h3>Points History</h3>
      <table>
        <thead>
          <tr>
            <th>Date</th>
            <th>Description</th>
            <th>Type</th>
            <th>Points</th>
          </tr>
        </thead>
        <tbody>
          {transactions.map((txn) => (
            <tr key={txn.transactionId}>
              <td>{new Date(txn.timestamp).toLocaleDateString()}</td>
              <td>{txn.description}</td>
              <td>
                <span className={`txn-type ${txn.transactionType.toLowerCase()}`}>
                  {txn.transactionType}
                </span>
              </td>
              <td
                className={
                  txn.transactionType === 'Earned' ? 'positive' : 'negative'
                }
              >
                {txn.transactionType === 'Earned' ? '+' : '-'}
                {Math.abs(txn.points)}
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};
