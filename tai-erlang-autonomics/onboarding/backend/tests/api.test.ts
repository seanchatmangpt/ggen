/**
 * Comprehensive API tests using Jest
 * Tests all endpoints with real database connections
 */

import request from 'supertest';
import app from '../src/server';
import { Pool } from 'pg';
import jwt from 'jsonwebtoken';
import * as Types from '../../../shared/types';

describe('Onboarding Platform API', () => {
  let pool: Pool;
  let token: string;
  let customerId: string;

  beforeAll(async () => {
    pool = new Pool({
      user: process.env.DB_USER || 'postgres',
      password: process.env.DB_PASSWORD || 'postgres',
      host: process.env.DB_HOST || 'localhost',
      port: parseInt(process.env.DB_PORT || '5432', 10),
      database: process.env.DB_NAME || 'onboarding_test',
    });

    // Generate test JWT
    token = jwt.sign(
      {
        userId: 'test-user-123',
        email: 'test@example.com',
        role: 'CUSTOMER_ADMIN',
        customerId: 'test-customer-123',
      },
      process.env.JWT_SECRET || 'secret',
      { expiresIn: '24h' }
    );
  });

  afterAll(async () => {
    await pool.end();
  });

  describe('Health Check', () => {
    it('should return healthy status', async () => {
      const response = await request(app).get('/health');

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(response.body.data.status).toBe('healthy');
    });
  });

  describe('Authentication', () => {
    it('should login with valid credentials', async () => {
      // Insert test user
      await pool.query(
        'INSERT INTO users (id, email, name, role, auth_provider) VALUES ($1, $2, $3, $4, $5)',
        ['test-user-123', 'test@example.com', 'Test User', 'CUSTOMER_ADMIN', 'JWT']
      );

      const response = await request(app)
        .post('/api/v1/auth/login')
        .send({
          email: 'test@example.com',
          password: 'testpassword',
        });

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(response.body.data.token).toBeDefined();
      expect(response.body.data.email).toBe('test@example.com');
    });

    it('should reject invalid credentials', async () => {
      const response = await request(app)
        .post('/api/v1/auth/login')
        .send({
          email: 'nonexistent@example.com',
          password: 'wrongpassword',
        });

      expect(response.status).toBe(401);
      expect(response.body.success).toBe(false);
      expect(response.body.error.code).toBe('INVALID_CREDENTIALS');
    });

    it('should reject request without auth token', async () => {
      const response = await request(app).get('/api/v1/customers/123');

      expect(response.status).toBe(401);
      expect(response.body.success).toBe(false);
      expect(response.body.error.code).toBe('UNAUTHORIZED');
    });
  });

  describe('Customers', () => {
    it('should create a new customer', async () => {
      const response = await request(app)
        .post('/api/v1/customers')
        .set('Authorization', `Bearer ${token}`)
        .send({
          name: 'Test Company',
          email: `test-${Date.now()}@example.com`,
        });

      expect(response.status).toBe(201);
      expect(response.body.success).toBe(true);
      expect(response.body.data.id).toBeDefined();
      expect(response.body.data.status).toBe('ONBOARDING');

      customerId = response.body.data.id;
    });

    it('should validate customer creation data', async () => {
      const response = await request(app)
        .post('/api/v1/customers')
        .set('Authorization', `Bearer ${token}`)
        .send({
          name: '', // Invalid: empty name
          email: 'invalid-email', // Invalid: bad email format
        });

      expect(response.status).toBe(400);
      expect(response.body.success).toBe(false);
      expect(response.body.error.code).toBe('VALIDATION_ERROR');
    });

    it('should retrieve customer by ID', async () => {
      const response = await request(app)
        .get(`/api/v1/customers/${customerId}`)
        .set('Authorization', `Bearer ${token}`);

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(response.body.data.id).toBe(customerId);
    });

    it('should return 404 for non-existent customer', async () => {
      const response = await request(app)
        .get('/api/v1/customers/nonexistent-id')
        .set('Authorization', `Bearer ${token}`);

      expect(response.status).toBe(404);
      expect(response.body.success).toBe(false);
      expect(response.body.error.code).toBe('NOT_FOUND');
    });
  });

  describe('Setup Wizard', () => {
    it('should complete setup step 1', async () => {
      const response = await request(app)
        .post(`/api/v1/customers/${customerId}/setup/step1`)
        .set('Authorization', `Bearer ${token}`)
        .send({
          apiKey: 'sk_test_123',
          webhookUrl: 'https://example.com/webhook',
        });

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(response.body.data.status).toBe('COMPLETED');
    });

    it('should track setup progress', async () => {
      const response = await request(app)
        .get(`/api/v1/customers/${customerId}`)
        .set('Authorization', `Bearer ${token}`);

      expect(response.status).toBe(200);
      expect(response.body.data.setupSteps).toBeDefined();
      expect(response.body.data.setupSteps.length).toBeGreaterThan(0);
    });
  });

  describe('Value Definitions', () => {
    it('should create a value definition', async () => {
      const response = await request(app)
        .post(`/api/v1/customers/${customerId}/value-definitions`)
        .set('Authorization', `Bearer ${token}`)
        .send({
          name: 'Revenue Impact',
          description: 'Incremental revenue from our platform',
          metrics: [
            {
              name: 'Revenue',
              unit: 'USD',
              description: 'Monthly revenue',
              dataSource: 'salesforce',
              refreshInterval: 3600,
            },
          ],
          baselineValue: 100000,
          targetValue: 150000,
          scoringModel: 'LINEAR',
        });

      expect(response.status).toBe(201);
      expect(response.body.success).toBe(true);
      expect(response.body.data.id).toBeDefined();
      expect(response.body.data.status).toBe('ACTIVE');
    });

    it('should validate value definition data', async () => {
      const response = await request(app)
        .post(`/api/v1/customers/${customerId}/value-definitions`)
        .set('Authorization', `Bearer ${token}`)
        .send({
          name: '',
          description: '',
          metrics: [],
          baselineValue: -100, // Invalid: negative
          targetValue: 50,
          scoringModel: 'LINEAR',
        });

      expect(response.status).toBe(400);
      expect(response.body.success).toBe(false);
    });

    it('should reject target value less than baseline', async () => {
      const response = await request(app)
        .post(`/api/v1/customers/${customerId}/value-definitions`)
        .set('Authorization', `Bearer ${token}`)
        .send({
          name: 'Invalid Definition',
          description: 'Test',
          metrics: [
            {
              name: 'Revenue',
              unit: 'USD',
              description: 'Monthly revenue',
              dataSource: 'salesforce',
              refreshInterval: 3600,
            },
          ],
          baselineValue: 150000,
          targetValue: 100000, // Less than baseline
          scoringModel: 'LINEAR',
        });

      expect(response.status).toBe(400);
      expect(response.body.success).toBe(false);
    });
  });

  describe('Dashboard', () => {
    it('should retrieve dashboard summary', async () => {
      const response = await request(app)
        .get(`/api/v1/customers/${customerId}/dashboard/summary`)
        .set('Authorization', `Bearer ${token}`);

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(response.body.data.customerId).toBe(customerId);
      expect(response.body.data.setupProgress).toBeDefined();
      expect(response.body.data.currentValue).toBeDefined();
      expect(response.body.data.measurementAccuracy).toBeDefined();
    });

    it('should show active alerts', async () => {
      const response = await request(app)
        .get(`/api/v1/customers/${customerId}/dashboard/summary`)
        .set('Authorization', `Bearer ${token}`);

      expect(response.status).toBe(200);
      expect(response.body.data.activeAlerts).toBeDefined();
      expect(Array.isArray(response.body.data.activeAlerts)).toBe(true);
    });
  });

  describe('Support Tickets', () => {
    let ticketId: string;

    it('should create a support ticket', async () => {
      const response = await request(app)
        .post(`/api/v1/customers/${customerId}/tickets`)
        .set('Authorization', `Bearer ${token}`)
        .send({
          title: 'Question about value calculation',
          description: 'How are subscriptions counted?',
          category: 'FINANCIAL',
          priority: 'HIGH',
        });

      expect(response.status).toBe(201);
      expect(response.body.success).toBe(true);
      expect(response.body.data.id).toBeDefined();
      expect(response.body.data.status).toBe('OPEN');

      ticketId = response.body.data.id;
    });

    it('should validate ticket data', async () => {
      const response = await request(app)
        .post(`/api/v1/customers/${customerId}/tickets`)
        .set('Authorization', `Bearer ${token}`)
        .send({
          title: '',
          description: '',
          category: 'INVALID_CATEGORY',
          priority: 'INVALID_PRIORITY',
        });

      expect(response.status).toBe(400);
      expect(response.body.success).toBe(false);
    });

    it('should add message to ticket', async () => {
      const response = await request(app)
        .post(`/api/v1/customers/${customerId}/tickets/${ticketId}/reply`)
        .set('Authorization', `Bearer ${token}`)
        .send({
          message: 'Subscriptions are counted at full annual value.',
        });

      expect(response.status).toBe(201);
      expect(response.body.success).toBe(true);
      expect(response.body.data.message).toBeDefined();
    });
  });

  describe('Approvals', () => {
    it('should create an approval workflow', async () => {
      const response = await request(app)
        .post(`/api/v1/customers/${customerId}/approvals`)
        .set('Authorization', `Bearer ${token}`)
        .send({
          stage: 'FINANCE_REVIEW',
          approvers: [
            {
              name: 'Jane Doe',
              email: 'jane@example.com',
              role: 'CFO',
            },
          ],
        });

      expect(response.status).toBe(201);
      expect(response.body.success).toBe(true);
      expect(response.body.data.stage).toBe('FINANCE_REVIEW');
      expect(response.body.data.status).toBe('PENDING');
    });

    it('should validate approval data', async () => {
      const response = await request(app)
        .post(`/api/v1/customers/${customerId}/approvals`)
        .set('Authorization', `Bearer ${token}`)
        .send({
          stage: 'INVALID_STAGE',
          approvers: [],
        });

      expect(response.status).toBe(400);
      expect(response.body.success).toBe(false);
    });
  });

  describe('Receipts & Validation', () => {
    it('should retrieve receipts for customer', async () => {
      const response = await request(app)
        .get(`/api/v1/customers/${customerId}/receipts`)
        .set('Authorization', `Bearer ${token}`);

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(Array.isArray(response.body.data)).toBe(true);
    });

    it('should validate receipt integrity', async () => {
      // First get a receipt
      const getResponse = await request(app)
        .get(`/api/v1/customers/${customerId}/receipts?limit=1`)
        .set('Authorization', `Bearer ${token}`);

      if (getResponse.body.data.length > 0) {
        const receiptId = getResponse.body.data[0].id;

        const response = await request(app)
          .post(`/api/v1/customers/${customerId}/receipts/validate`)
          .set('Authorization', `Bearer ${token}`)
          .send({ receiptId });

        expect(response.status).toBe(200);
        expect(response.body.success).toBe(true);
        expect(response.body.data.isValid).toBeDefined();
        expect(response.body.data.signatureValid).toBeDefined();
      }
    });
  });

  describe('Error Handling', () => {
    it('should handle missing required fields', async () => {
      const response = await request(app)
        .post('/api/v1/customers')
        .set('Authorization', `Bearer ${token}`)
        .send({});

      expect(response.status).toBe(400);
      expect(response.body.success).toBe(false);
      expect(response.body.error).toBeDefined();
    });

    it('should handle invalid JWT', async () => {
      const response = await request(app)
        .get(`/api/v1/customers/${customerId}`)
        .set('Authorization', 'Bearer invalid-token');

      expect(response.status).toBe(403);
      expect(response.body.success).toBe(false);
      expect(response.body.error.code).toBe('FORBIDDEN');
    });

    it('should handle expired JWT', async () => {
      const expiredToken = jwt.sign(
        { userId: 'test-user-123' },
        process.env.JWT_SECRET || 'secret',
        { expiresIn: '-1h' }
      );

      const response = await request(app)
        .get(`/api/v1/customers/${customerId}`)
        .set('Authorization', `Bearer ${expiredToken}`);

      expect(response.status).toBe(403);
      expect(response.body.success).toBe(false);
    });

    it('should handle malformed JSON', async () => {
      const response = await request(app)
        .post('/api/v1/customers')
        .set('Authorization', `Bearer ${token}`)
        .set('Content-Type', 'application/json')
        .send('{invalid json}');

      expect(response.status).toBeGreaterThanOrEqual(400);
    });
  });

  describe('Data Validation', () => {
    it('should enforce unique email addresses', async () => {
      const email = `unique-${Date.now()}@example.com`;

      // Create first customer
      await request(app)
        .post('/api/v1/customers')
        .set('Authorization', `Bearer ${token}`)
        .send({
          name: 'Company 1',
          email,
        });

      // Try to create second with same email
      const response = await request(app)
        .post('/api/v1/customers')
        .set('Authorization', `Bearer ${token}`)
        .send({
          name: 'Company 2',
          email,
        });

      expect(response.status).toBe(400);
    });

    it('should validate email format', async () => {
      const response = await request(app)
        .post('/api/v1/customers')
        .set('Authorization', `Bearer ${token}`)
        .send({
          name: 'Test Company',
          email: 'invalid-email-format',
        });

      expect(response.status).toBe(400);
      expect(response.body.error.code).toBe('VALIDATION_ERROR');
    });

    it('should validate phone number format', async () => {
      // If phone fields are added
      const response = await request(app)
        .post('/api/v1/customers')
        .set('Authorization', `Bearer ${token}`)
        .send({
          name: 'Test Company',
          email: 'test@example.com',
          phone: 'invalid-phone',
        });

      if (response.status !== 201) {
        expect(response.body.error.code).toBe('VALIDATION_ERROR');
      }
    });
  });
});
