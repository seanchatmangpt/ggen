/**
 * Quantum-Safe Encryption API
 * POST /api/quantum/encrypt
 * POST /api/quantum/decrypt
 *
 * Handles quantum-safe encryption and decryption of sensitive data
 */

import { NextRequest, NextResponse } from 'next/server'
import { z } from 'zod'
import {
  getQuantumCryptoService,
  QuantumEncryptedPayload,
} from '@/lib/quantum-crypto'

// Encryption request schema
const EncryptRequestSchema = z.object({
  data: z.string().min(1),
  recipientKeyId: z.string().min(1),
  recipientPublicKey: z.string().min(1),
  contentType: z.string().optional(),
})

// Decryption request schema
const DecryptRequestSchema = z.object({
  encryptedPayload: z.object({
    ciphertext: z.string(),
    ephemeralPublicKey: z.string(),
    nonce: z.string(),
    keyId: z.string(),
    algorithm: z.string(),
  }),
  privateKeyPem: z.string(),
})

/**
 * POST /api/quantum/encrypt
 * Encrypt data using recipient's quantum-safe public key
 */
export async function POST(request: NextRequest) {
  try {
    const url = new URL(request.url)
    const action = url.searchParams.get('action') || 'encrypt'

    if (action === 'decrypt') {
      return handleDecrypt(request)
    }

    return handleEncrypt(request)
  } catch (error) {
    console.error('Quantum encryption error:', error)
    return NextResponse.json(
      { success: false, error: 'Encryption operation failed' },
      { status: 500 }
    )
  }
}

/**
 * Handle encryption request
 */
async function handleEncrypt(request: NextRequest) {
  try {
    const body = await request.json()
    const validatedData = EncryptRequestSchema.parse(body)

    const cryptoService = getQuantumCryptoService()

    // Encrypt data
    const encryptedPayload = cryptoService.encrypt(
      validatedData.data,
      validatedData.recipientPublicKey,
      validatedData.recipientKeyId
    )

    // Update metadata
    if (validatedData.contentType) {
      encryptedPayload.metadata = {
        ...encryptedPayload.metadata,
        contentType: validatedData.contentType,
      }
    }

    return NextResponse.json(
      {
        success: true,
        message: 'Data encrypted successfully',
        encryptedPayload,
        timestamp: new Date().toISOString(),
      },
      { status: 200 }
    )
  } catch (error) {
    console.error('Encryption error:', error)

    if (error instanceof z.ZodError) {
      return NextResponse.json(
        {
          success: false,
          error: 'Invalid encryption request',
          details: error.errors,
        },
        { status: 400 }
      )
    }

    return NextResponse.json(
      {
        success: false,
        error: error instanceof Error ? error.message : 'Encryption failed',
      },
      { status: 500 }
    )
  }
}

/**
 * Handle decryption request
 */
async function handleDecrypt(request: NextRequest) {
  try {
    const body = await request.json()
    const validatedData = DecryptRequestSchema.parse(body)

    const cryptoService = getQuantumCryptoService()

    // Decrypt payload
    const decryptedBuffer = cryptoService.decrypt(
      validatedData.encryptedPayload as QuantumEncryptedPayload,
      validatedData.privateKeyPem
    )

    const decryptedData = decryptedBuffer.toString('utf-8')

    // Try to parse as JSON for better response
    let parsedData: any
    try {
      parsedData = JSON.parse(decryptedData)
    } catch {
      parsedData = decryptedData
    }

    return NextResponse.json(
      {
        success: true,
        message: 'Data decrypted successfully',
        data: parsedData,
        timestamp: new Date().toISOString(),
      },
      { status: 200 }
    )
  } catch (error) {
    console.error('Decryption error:', error)

    if (error instanceof z.ZodError) {
      return NextResponse.json(
        {
          success: false,
          error: 'Invalid decryption request',
          details: error.errors,
        },
        { status: 400 }
      )
    }

    return NextResponse.json(
      {
        success: false,
        error: error instanceof Error ? error.message : 'Decryption failed',
      },
      { status: 500 }
    )
  }
}
