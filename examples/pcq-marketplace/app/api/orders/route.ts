import { getMarketEngine } from '@/lib/market';
import { z } from 'zod';

export const runtime = 'nodejs';

const OrderSchema = z.object({
  listingId: z.string().min(1),
  buyerAccountId: z.enum(['buyer-atlas', 'buyer-orbit']),
  quantity: z.number().int().positive().max(25),
});

export async function POST(request: Request) {
  try {
    const order = OrderSchema.parse(await request.json());
    return Response.json(getMarketEngine().settle(order), { status: 201 });
  } catch (error) {
    const message = error instanceof Error ? error.message : 'ORDER_REFUSED';
    return Response.json({ error: message }, { status: 422 });
  }
}
