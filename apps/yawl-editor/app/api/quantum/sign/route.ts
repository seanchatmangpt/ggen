/**
 * Quantum-Safe Digital Signature API
 * POST /api/quantum/sign
 *
 * Handles post-quantum digital signatures for message authentication
 */

import { NextRequest, NextResponse } from 'next/server'
import { z } from 'zod'
import { getQuantumCryptoService, QuantumSignature } from '@/lib/quantum-crypto'

// Sign request schema
const SignRequestSchema = z.object({
  message: z.string().min(1),
  privateKeyPem: z.string().min(1),
  keyId: z.string().min(1),
})

// Verify request schema
const VerifyRequestSchema = z.object({
  message: z.string().min(1),
  signature: z.object({
    signature: z.string(),
    keyId: z.string(),
    algorithm: z.string(),
    timestamp: z.string(),
  }),
  publicKeyPem: z.string().min(1),
})

/**
 * POST /api/quantum/sign
 * Create quantum-safe digital signature
 * Query param: ?action=verify for signature verification
 */
export async function POST(request: NextRequest) {
  try {
    const url = new URL(request.url)
    const action = url.searchParams.get('action') || 'sign'

    if (action === 'verify') {
      return handleVerify(request)
    }

    return handleSign(request)
  } catch (error) {
    console.error('Quantum signature error:', error)
    return NextResponse.json(
      { success: false, error: 'Signature operation failed' },
      { status: 500 }
    )
  }
}

/**
 * Handle signature creation
 */
async function handleSign(request: NextRequest) {
  try {
    const body = await request.json()
    const validatedData = SignRequestSchema.parse(body)

    const cryptoService = getQuantumCryptoService()

    // Create quantum-safe signature
    const signature = cryptoService.sign(
      validatedData.message,
      validatedData.privateKeyPem,
      validatedData.keyId
    )

    return NextResponse.json(
      {
        success: true,
        message: 'Message signed successfully',
        signature: {
          signature: signature.signature,
          keyId: signature.keyId,
          algorithm: signature.algorithm,
          timestamp: signature.timestamp.toISOString(),
        },
      },
      { status: 200 }
    )
  } catch (error) {
    console.error('Signature creation error:', error)

    if (error instanceof z.ZodError) {
      return NextResponse.json(
        {
          success: false,
          error: 'Invalid signature request',
          details: error.errors,
        },
        { status: 400 }
      )
    }

    return NextResponse.json(
      {
        success: false,
        error: error instanceof Error ? error.message : 'Signature creation failed',
      },
      { status: 500 }
    )
  }
}

/**
 * Handle signature verification
 */
async function handleVerify(request: NextRequest) {
  try {
    const body = await request.json()
    const validatedData = VerifyRequestSchema.parse(body)

    const cryptoService = getQuantumCryptoService()

    // Create signature object with proper typing
    const signature: QuantumSignature = {
      signature: validatedData.signature.signature,
      keyId: validatedData.signature.keyId,
      algorithm: validatedData.signature.algorithm as any,
      timestamp: new Date(validatedData.signature.timestamp),
      verified: false,
    }

    // Verify signature
    const isValid = cryptoService.verify(
      validatedData.message,
      signature,
      validatedData.publicKeyPem
    )

    return NextResponse.json(
      {
        success: true,
        message: isValid ? 'Signature verified successfully' : 'Signature verification failed',
        isValid,
        keyId: signature.keyId,
        algorithm: signature.algorithm,
        timestamp: signature.timestamp.toISOString(),
      },
      { status: 200 }
    )
  } catch (error) {
    console.error('Signature verification error:', error)

    if (error instanceof z.ZodError) {
      return NextResponse.json(
        {
          success: false,
          error: 'Invalid verification request',
          details: error.errors,
        },
        { status: 400 }
      )
    }

    return NextResponse.json(
      {
        success: false,
        error: error instanceof Error ? error.message : 'Signature verification failed',
      },
      { status: 500 }
    )
  }
}
