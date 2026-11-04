import { getMarketEngine } from '@/lib/market';

export const dynamic = 'force-dynamic';
export const runtime = 'nodejs';

export async function GET() {
  return Response.json(getMarketEngine().snapshot(), {
    headers: { 'cache-control': 'no-store' },
  });
}
