import { MarketDashboard } from '@/components/market-dashboard';
import { getMarketEngine } from '@/lib/market';

export const dynamic = 'force-dynamic';

export default function Home() {
  return <MarketDashboard initialSnapshot={getMarketEngine().snapshot()} />;
}
