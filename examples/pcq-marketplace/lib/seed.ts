import type { Account, Listing, MarketNode } from './types';

export const MARKET_CONFIG = Object.freeze({
  displayName: 'PCQ Atlas Marketplace',
  currencyName: 'Proof-Carrying Quanta',
  currencyCode: 'PCQ' as const,
  decimals: 6 as const,
  initialSupplyMicros: '5000000000000',
  treasuryAccountId: 'treasury',
  feeBps: 25,
  maxOrderQuantity: 25,
  streamIntervalMs: 500,
});

export const INITIAL_ACCOUNTS: Account[] = [
  { id: 'buyer-atlas', label: 'Atlas Procurement', balanceMicros: '10000000000' },
  { id: 'buyer-orbit', label: 'Orbit Operations', balanceMicros: '750000000000' },
  { id: 'seller-delta', label: 'Delta Runtime Works', balanceMicros: '200000000000' },
  { id: 'seller-europa', label: 'Europa Process Labs', balanceMicros: '250000000000' },
  { id: 'seller-harbor', label: 'Harbor Systems', balanceMicros: '300000000000' },
  { id: 'seller-northstar', label: 'Northstar Foundry', balanceMicros: '300000000000' },
  { id: 'seller-pacific', label: 'Pacific Policy Mesh', balanceMicros: '150000000000' },
  { id: 'seller-sierra', label: 'Sierra Evidence', balanceMicros: '250000000000' },
  { id: 'treasury', label: 'PCQ Treasury', balanceMicros: '2790000000000' },
];

export const MARKET_NODES: MarketNode[] = [
  { id: 'berlin', label: 'Sierra Evidence', kind: 'supplier', position: [13.405, 52.52], liquidityMicros: '250000000000' },
  { id: 'chicago', label: 'Chicago Demand', kind: 'buyer', position: [-87.6298, 41.8781], liquidityMicros: '750000000000' },
  { id: 'london', label: 'Europa Process Labs', kind: 'supplier', position: [-0.1276, 51.5072], liquidityMicros: '250000000000' },
  { id: 'los-angeles', label: 'Los Angeles Treasury', kind: 'treasury', position: [-118.2437, 34.0522], liquidityMicros: '2790000000000' },
  { id: 'new-york', label: 'New York Demand', kind: 'buyer', position: [-74.006, 40.7128], liquidityMicros: '10000000000' },
  { id: 'san-francisco', label: 'Northstar Foundry', kind: 'supplier', position: [-122.4194, 37.7749], liquidityMicros: '300000000000' },
  { id: 'seattle', label: 'Harbor Systems', kind: 'supplier', position: [-122.3321, 47.6062], liquidityMicros: '300000000000' },
  { id: 'singapore', label: 'Delta Runtime Works', kind: 'supplier', position: [103.8198, 1.3521], liquidityMicros: '200000000000' },
  { id: 'tokyo', label: 'Pacific Policy Mesh', kind: 'supplier', position: [139.6917, 35.6895], liquidityMicros: '150000000000' },
];

export const INITIAL_LISTINGS: Listing[] = [
  { id: 'atomvm-swarm', title: 'AtomVM Swarm Runtime', category: 'runtime', sellerAccountId: 'seller-delta', nodeId: 'singapore', priceMicros: '720000000', inventory: 96, proofScore: 97 },
  { id: 'legacy-equivalence', title: 'Legacy Equivalence Verifier', category: 'validator', sellerAccountId: 'seller-sierra', nodeId: 'berlin', priceMicros: '640000000', inventory: 180, proofScore: 98 },
  { id: 'next-deck-pack', title: 'Next.js + deck.gl Marketplace Pack', category: 'projection', sellerAccountId: 'seller-northstar', nodeId: 'san-francisco', priceMicros: '880000000', inventory: 120, proofScore: 99 },
  { id: 'ocel-mining', title: 'OCEL Process Mining Rail', category: 'evidence', sellerAccountId: 'seller-europa', nodeId: 'london', priceMicros: '510000000', inventory: 220, proofScore: 96 },
  { id: 'replay-appliance', title: 'Offline Replay Evidence Appliance', category: 'receipt', sellerAccountId: 'seller-harbor', nodeId: 'seattle', priceMicros: '760000000', inventory: 140, proofScore: 99 },
  { id: 'shacl-policy', title: 'SHACL Policy Gate Mesh', category: 'policy', sellerAccountId: 'seller-pacific', nodeId: 'tokyo', priceMicros: '390000000', inventory: 300, proofScore: 95 },
];
