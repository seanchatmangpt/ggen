/**
 * Cryptographic Receipt Service
 * Implements receipt generation, signing, and validation for measurement transparency
 */

import crypto from 'crypto';
import { Pool } from 'pg';
import * as Types from '../../../shared/types';

interface ReceiptData {
  id: string;
  customerId: string;
  type: Types.Receipt['type'];
  data: Record<string, any>;
  previousHash?: string;
}

export class ReceiptService {
  private pool: Pool;
  private privateKeyPem: string;
  private publicKeyPem: string;

  constructor(pool: Pool, privateKeyPath?: string, publicKeyPath?: string) {
    this.pool = pool;
    // In production, load from secure key management (e.g., AWS KMS, HashiCorp Vault)
    this.privateKeyPem = privateKeyPath || this.generatePrivateKey();
    this.publicKeyPem = publicKeyPath || this.generatePublicKey(this.privateKeyPem);
  }

  /**
   * Generate a receipt hash using SHA-256
   */
  private generateHash(data: string): string {
    return crypto.createHash('sha256').update(data).digest('hex');
  }

  /**
   * Sign receipt data with private key
   */
  private sign(data: string): string {
    const signer = crypto.createSign('RSA-SHA256');
    signer.update(data);
    return signer.sign(this.privateKeyPem, 'hex');
  }

  /**
   * Verify receipt signature
   */
  private verify(data: string, signature: string): boolean {
    const verifier = crypto.createVerify('RSA-SHA256');
    verifier.update(data);
    return verifier.verify(this.publicKeyPem, signature, 'hex');
  }

  /**
   * Generate RSA key pair
   */
  private generatePrivateKey(): string {
    const { privateKey } = crypto.generateKeyPairSync('rsa', {
      modulusLength: 2048,
      publicKeyEncoding: { type: 'spki', format: 'pem' },
      privateKeyEncoding: { type: 'pkcs8', format: 'pem' },
    });
    return privateKey as unknown as string;
  }

  /**
   * Derive public key from private key
   */
  private generatePublicKey(privateKeyPem: string): string {
    const privateKey = crypto.createPrivateKey(privateKeyPem);
    return crypto
      .createPublicKey(privateKey)
      .export({ type: 'spki', format: 'pem' }) as unknown as string;
  }

  /**
   * Create and store a receipt
   */
  async createReceipt(receipt: ReceiptData): Promise<Types.Receipt> {
    try {
      // Get previous hash for chain linking
      let previousHash: string | undefined;
      if (receipt.type !== 'SETUP') {
        const lastReceiptResult = await this.pool.query(
          `SELECT hash FROM receipts WHERE customer_id = $1 ORDER BY timestamp DESC LIMIT 1`,
          [receipt.customerId]
        );
        previousHash = lastReceiptResult.rows[0]?.hash;
      }

      // Create receipt data
      const receiptData = {
        id: receipt.id,
        type: receipt.type,
        data: receipt.data,
        timestamp: new Date().toISOString(),
        previousHash,
      };

      // Generate hash
      const dataString = JSON.stringify(receiptData);
      const hash = this.generateHash(dataString);

      // Sign
      const signature = this.sign(dataString);

      // Store in database
      const result = await this.pool.query(
        `INSERT INTO receipts (id, customer_id, type, data, hash, signature, previous_hash, timestamp)
         VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
         RETURNING *`,
        [
          receipt.id,
          receipt.customerId,
          receipt.type,
          JSON.stringify(receipt.data),
          hash,
          signature,
          previousHash,
          new Date(),
        ]
      );

      return result.rows[0];
    } catch (err) {
      console.error('Failed to create receipt', err);
      throw err;
    }
  }

  /**
   * Retrieve and validate a receipt
   */
  async getReceipt(receiptId: string): Promise<Types.Receipt | null> {
    try {
      const result = await this.pool.query(
        'SELECT * FROM receipts WHERE id = $1',
        [receiptId]
      );

      if (result.rows.length === 0) return null;

      const receipt = result.rows[0];

      // Verify signature
      const receiptData = {
        id: receipt.id,
        type: receipt.type,
        data: receipt.data,
        timestamp: receipt.timestamp.toISOString(),
        previousHash: receipt.previous_hash,
      };

      const dataString = JSON.stringify(receiptData);
      const isValid = this.verify(dataString, receipt.signature);

      if (!isValid) {
        console.warn(`Invalid signature for receipt ${receiptId}`);
      }

      return receipt;
    } catch (err) {
      console.error('Failed to get receipt', err);
      throw err;
    }
  }

  /**
   * Validate entire receipt chain for a customer
   */
  async validateChain(customerId: string): Promise<Types.ReceiptValidation> {
    try {
      const result = await this.pool.query(
        `SELECT * FROM receipts WHERE customer_id = $1 ORDER BY timestamp ASC`,
        [customerId]
      );

      const receipts = result.rows;
      let isChainValid = true;
      const errors: string[] = [];

      // Validate each receipt and its chain linkage
      for (let i = 0; i < receipts.length; i++) {
        const receipt = receipts[i];

        // Verify signature
        const receiptData = {
          id: receipt.id,
          type: receipt.type,
          data: receipt.data,
          timestamp: receipt.timestamp.toISOString(),
          previousHash: receipt.previous_hash,
        };

        const dataString = JSON.stringify(receiptData);
        const signatureValid = this.verify(dataString, receipt.signature);

        if (!signatureValid) {
          isChainValid = false;
          errors.push(`Receipt ${receipt.id}: Invalid signature`);
        }

        // Verify chain linkage
        if (i > 0) {
          const previousReceipt = receipts[i - 1];
          if (receipt.previous_hash !== previousReceipt.hash) {
            isChainValid = false;
            errors.push(
              `Receipt ${receipt.id}: Chain linkage broken. Expected previous hash ${previousReceipt.hash}, got ${receipt.previous_hash}`
            );
          }
        }

        // Verify hash immutability
        const expectedHash = this.generateHash(dataString);
        if (receipt.hash !== expectedHash) {
          isChainValid = false;
          errors.push(
            `Receipt ${receipt.id}: Hash mismatch. Expected ${expectedHash}, got ${receipt.hash}`
          );
        }
      }

      return {
        receiptId: receipts[receipts.length - 1]?.id || '',
        isValid: isChainValid && errors.length === 0,
        chainValid: isChainValid,
        signatureValid: errors.filter(e => e.includes('signature')).length === 0,
        errors: errors.length > 0 ? errors : undefined,
        verifiedAt: new Date(),
      };
    } catch (err) {
      console.error('Failed to validate chain', err);
      throw err;
    }
  }

  /**
   * Get all receipts for a customer
   */
  async getCustomerReceipts(
    customerId: string,
    type?: Types.Receipt['type'],
    limit: number = 50,
    offset: number = 0
  ): Promise<Types.Receipt[]> {
    try {
      let query = 'SELECT * FROM receipts WHERE customer_id = $1';
      const params: any[] = [customerId];

      if (type) {
        query += ' AND type = $2';
        params.push(type);
      }

      query += ' ORDER BY timestamp DESC LIMIT $' + (params.length + 1) + ' OFFSET $' + (params.length + 2);
      params.push(limit, offset);

      const result = await this.pool.query(query, params);
      return result.rows;
    } catch (err) {
      console.error('Failed to get customer receipts', err);
      throw err;
    }
  }

  /**
   * Create value calculation receipt
   */
  async createValueCalculationReceipt(
    customerId: string,
    valueCalculation: Types.ValueCalculation
  ): Promise<Types.Receipt> {
    return this.createReceipt({
      id: valueCalculation.id,
      customerId,
      type: 'VALUE_CALCULATION',
      data: {
        valueDefinitionId: valueCalculation.valueDefinitionId,
        calculatedValue: valueCalculation.calculatedValue,
        previousValue: valueCalculation.previousValue,
        changePercentage: valueCalculation.changePercentage,
        metricValues: valueCalculation.metricValues,
        period: valueCalculation.period,
        timestamp: valueCalculation.timestamp.toISOString(),
      },
    });
  }

  /**
   * Create setup completion receipt
   */
  async createSetupReceipt(
    customerId: string,
    setupStep: number,
    data: Record<string, any>
  ): Promise<Types.Receipt> {
    return this.createReceipt({
      id: crypto.randomUUID(),
      customerId,
      type: 'SETUP',
      data: {
        setupStep,
        ...data,
        completedAt: new Date().toISOString(),
      },
    });
  }

  /**
   * Create approval receipt
   */
  async createApprovalReceipt(
    customerId: string,
    approvalId: string,
    status: 'APPROVED' | 'REJECTED',
    approverEmail: string,
    comments?: string
  ): Promise<Types.Receipt> {
    return this.createReceipt({
      id: crypto.randomUUID(),
      customerId,
      type: 'APPROVAL',
      data: {
        approvalId,
        status,
        approverEmail,
        comments,
        approvedAt: new Date().toISOString(),
      },
    });
  }

  /**
   * Create go-live receipt
   */
  async createGoLiveReceipt(customerId: string, goLiveDate: Date): Promise<Types.Receipt> {
    return this.createReceipt({
      id: crypto.randomUUID(),
      customerId,
      type: 'GO_LIVE',
      data: {
        goLiveDate: goLiveDate.toISOString(),
        initiatedAt: new Date().toISOString(),
      },
    });
  }

  /**
   * Batch verify receipts
   */
  async batchVerify(receiptIds: string[]): Promise<Map<string, boolean>> {
    const results = new Map<string, boolean>();

    for (const receiptId of receiptIds) {
      try {
        const receipt = await this.getReceipt(receiptId);
        if (!receipt) {
          results.set(receiptId, false);
          continue;
        }

        const receiptData = {
          id: receipt.id,
          type: receipt.type,
          data: receipt.data,
          timestamp: receipt.timestamp.toISOString(),
          previousHash: receipt.previous_hash,
        };

        const dataString = JSON.stringify(receiptData);
        const isValid = this.verify(dataString, receipt.signature);
        results.set(receiptId, isValid);
      } catch (err) {
        results.set(receiptId, false);
      }
    }

    return results;
  }

  /**
   * Export receipts for audit
   */
  async exportAuditTrail(customerId: string): Promise<string> {
    try {
      const receipts = await this.getCustomerReceipts(customerId, undefined, 10000, 0);

      const auditData = {
        customerId,
        exportedAt: new Date().toISOString(),
        receiptCount: receipts.length,
        receipts: receipts.map(r => ({
          id: r.id,
          type: r.type,
          timestamp: r.timestamp,
          hash: r.hash,
          data: r.data,
        })),
      };

      return JSON.stringify(auditData, null, 2);
    } catch (err) {
      console.error('Failed to export audit trail', err);
      throw err;
    }
  }
}
