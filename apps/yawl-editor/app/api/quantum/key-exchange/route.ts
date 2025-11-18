/**
 * Quantum-Safe Key Exchange API
 * POST /api/quantum/key-exchange
 *
 * Handles post-quantum key agreement and distribution
 */

import { NextRequest, NextResponse } from 'next/server'
import { z } from 'zod'
import { getQuantumCryptoService } from '@/lib/quantum-crypto'

// Request validation
const KeyExchangeRequestSchema = z.object({
  userId: z.string().min(1),
  userName: z.string().min(1),
  publicKey: z.string(),
  deviceId: z.string().optional(),
  timestamp: z.string().datetime().optional(),
})

type KeyExchangeRequest = z.infer<typeof KeyExchangeRequestSchema>

/**
 * Storage for user public keys (in production: use secure database)
 */
const publicKeyRegistry = new Map<string, { key: string; timestamp: Date; deviceId?: string }>()

/**
 * POST /api/quantum/key-exchange
 * Register public key for quantum-safe communication
 */
export async function POST(request: NextRequest) {
  try {
    const body = await request.json()

    // Validate request
    const validatedData = KeyExchangeRequestSchema.parse(body)

    // Store public key
    publicKeyRegistry.set(validatedData.userId, {
      key: validatedData.publicKey,
      timestamp: new Date(validatedData.timestamp || Date.now()),
      deviceId: validatedData.deviceId,
    })

    // Generate response with server's public key
    const cryptoService = getQuantumCryptoService()
    const serverKeyPair = cryptoService.generateKeyPair({
      purpose: 'communication',
      expiryDays: 90,
    })

    return NextResponse.json(
      {
        success: true,
        message: 'Key exchange successful',
        serverPublicKey: serverKeyPair.publicKey,
        keyId: serverKeyPair.keyId,
        algorithm: 'kyber-pq-hybrid',
        expiresAt: serverKeyPair.expiresAt,
        timestamp: new Date().toISOString(),
      },
      { status: 200 }
    )
  } catch (error) {
    console.error('Key exchange error:', error)

    if (error instanceof z.ZodError) {
      return NextResponse.json(
        {
          success: false,
          error: 'Invalid request format',
          details: error.errors,
        },
        { status: 400 }
      )
    }

    return NextResponse.json(
      {
        success: false,
        error: 'Key exchange failed',
      },
      { status: 500 }
    )
  }
}

/**
 * GET /api/quantum/key-exchange?userId={userId}
 * Retrieve user's public key for communication
 */
export async function GET(request: NextRequest) {
  try {
    const userId = request.nextUrl.searchParams.get('userId')

    if (!userId) {
      return NextResponse.json(
        { success: false, error: 'userId parameter required' },
        { status: 400 }
      )
    }

    const keyData = publicKeyRegistry.get(userId)

    if (!keyData) {
      return NextResponse.json(
        { success: false, error: 'User public key not found' },
        { status: 404 }
      )
    }

    return NextResponse.json(
      {
        success: true,
        userId,
        publicKey: keyData.key,
        deviceId: keyData.deviceId,
        registeredAt: keyData.timestamp.toISOString(),
      },
      { status: 200 }
    )
  } catch (error) {
    console.error('Key retrieval error:', error)

    return NextResponse.json(
      {
        success: false,
        error: 'Failed to retrieve key',
      },
      { status: 500 }
    )
  }
}
