/**
 * Main Express server configuration
 * Handles setup wizard, value definitions, receipts, approvals, go-live, dashboard, alerts, and support
 */

import express, { Express, Request, Response, NextFunction } from 'express';
import cors from 'cors';
import helmet from 'helmet';
import morgan from 'morgan';
import jwt from 'jsonwebtoken';
import { createLogger, format, transports } from 'winston';
import { v4 as uuidv4 } from 'uuid';
import { Pool } from 'pg';
import redis from 'redis';
import * as OT from '@opentelemetry/api';
import * as Types from '../../shared/types';
import * as Schemas from '../../shared/schemas';

// Initialize logging
const logger = createLogger({
  format: format.combine(
    format.timestamp(),
    format.errors({ stack: true }),
    format.json(),
  ),
  transports: [
    new transports.Console(),
    new transports.File({ filename: 'logs/error.log', level: 'error' }),
    new transports.File({ filename: 'logs/combined.log' }),
  ],
});

// Database connection pool
const pool = new Pool({
  user: process.env.DB_USER || 'postgres',
  password: process.env.DB_PASSWORD || 'postgres',
  host: process.env.DB_HOST || 'localhost',
  port: parseInt(process.env.DB_PORT || '5432', 10),
  database: process.env.DB_NAME || 'onboarding',
});

// Redis client
const redisClient = redis.createClient({
  host: process.env.REDIS_HOST || 'localhost',
  port: parseInt(process.env.REDIS_PORT || '6379', 10),
});

// Request context middleware
interface AuthenticatedRequest extends Request {
  userId?: string;
  customerId?: string;
  role?: string;
  span?: OT.Span;
}

// Error handling
interface ApiError extends Error {
  statusCode?: number;
  code?: string;
  details?: Record<string, any>;
}

// Middleware for request tracing
const tracingMiddleware = (req: AuthenticatedRequest, res: Response, next: NextFunction) => {
  const tracer = OT.trace.getTracer('onboarding');
  const span = tracer.startSpan(`${req.method} ${req.path}`);
  req.span = span;

  span.setAttributes({
    'http.method': req.method,
    'http.url': req.url,
    'http.client_ip': req.ip,
  });

  res.on('finish', () => {
    span.setAttributes({
      'http.status_code': res.statusCode,
    });
    span.end();
  });

  next();
};

// Authentication middleware
const authenticateJWT = (req: AuthenticatedRequest, res: Response, next: NextFunction) => {
  const authHeader = req.headers.authorization;
  const token = authHeader?.split(' ')[1];

  if (!token) {
    return res.status(401).json({
      success: false,
      error: {
        code: 'UNAUTHORIZED',
        message: 'Missing authentication token',
      },
      timestamp: new Date(),
    } as Types.ApiResponse<null>);
  }

  try {
    const decoded = jwt.verify(token, process.env.JWT_SECRET || 'secret') as any;
    req.userId = decoded.userId;
    req.customerId = decoded.customerId;
    req.role = decoded.role;
    next();
  } catch (err) {
    logger.error('JWT verification failed', { error: err });
    return res.status(403).json({
      success: false,
      error: {
        code: 'FORBIDDEN',
        message: 'Invalid token',
      },
      timestamp: new Date(),
    } as Types.ApiResponse<null>);
  }
};

// Setup Express app
const app: Express = express();

// Global middleware
app.use(helmet());
app.use(cors({
  origin: process.env.CORS_ORIGIN || 'http://localhost:3000',
  credentials: true,
}));
app.use(morgan('combined', { stream: { write: msg => logger.info(msg) } }));
app.use(express.json({ limit: '10mb' }));
app.use(tracingMiddleware);

// Health check endpoint
app.get('/health', async (req: AuthenticatedRequest, res: Response) => {
  try {
    const client = await pool.connect();
    await client.query('SELECT NOW()');
    client.release();

    res.status(200).json({
      success: true,
      data: { status: 'healthy', timestamp: new Date() },
      timestamp: new Date(),
    } as Types.ApiResponse<any>);
  } catch (err) {
    logger.error('Health check failed', { error: err });
    res.status(503).json({
      success: false,
      error: {
        code: 'SERVICE_UNAVAILABLE',
        message: 'Database connection failed',
      },
      timestamp: new Date(),
    } as Types.ApiResponse<null>);
  }
});

// Auth endpoints
app.post('/api/v1/auth/login', async (req: AuthenticatedRequest, res: Response) => {
  try {
    const { email, password } = req.body;

    if (!email || !password) {
      return res.status(400).json({
        success: false,
        error: {
          code: 'INVALID_INPUT',
          message: 'Email and password are required',
        },
        timestamp: new Date(),
      } as Types.ApiResponse<null>);
    }

    const result = await pool.query(
      'SELECT id, email, role, customer_id FROM users WHERE email = $1',
      [email]
    );

    if (result.rows.length === 0) {
      return res.status(401).json({
        success: false,
        error: {
          code: 'INVALID_CREDENTIALS',
          message: 'Invalid email or password',
        },
        timestamp: new Date(),
      } as Types.ApiResponse<null>);
    }

    const user = result.rows[0];
    const token = jwt.sign(
      {
        userId: user.id,
        email: user.email,
        role: user.role,
        customerId: user.customer_id,
      },
      process.env.JWT_SECRET || 'secret',
      { expiresIn: '24h' }
    );

    res.json({
      success: true,
      data: { token, userId: user.id, email: user.email, role: user.role },
      timestamp: new Date(),
    } as Types.ApiResponse<any>);
  } catch (err) {
    logger.error('Login failed', { error: err });
    res.status(500).json({
      success: false,
      error: {
        code: 'INTERNAL_ERROR',
        message: 'Login failed',
      },
      timestamp: new Date(),
    } as Types.ApiResponse<null>);
  }
});

// Customer endpoints
app.post('/api/v1/customers', authenticateJWT, async (req: AuthenticatedRequest, res: Response) => {
  try {
    const validation = Schemas.createCustomerSchema.safeParse(req.body);
    if (!validation.success) {
      return res.status(400).json({
        success: false,
        error: {
          code: 'VALIDATION_ERROR',
          message: 'Invalid customer data',
          details: validation.error.errors,
        },
        timestamp: new Date(),
      } as Types.ApiResponse<null>);
    }

    const customerId = uuidv4();
    const now = new Date();

    const result = await pool.query(
      `INSERT INTO customers (id, name, email, status, created_at, updated_at)
       VALUES ($1, $2, $3, $4, $5, $6)
       RETURNING *`,
      [customerId, validation.data.name, validation.data.email, Types.CustomerStatus.ONBOARDING, now, now]
    );

    res.status(201).json({
      success: true,
      data: result.rows[0],
      timestamp: new Date(),
    } as Types.ApiResponse<any>);
  } catch (err) {
    logger.error('Create customer failed', { error: err });
    res.status(500).json({
      success: false,
      error: {
        code: 'INTERNAL_ERROR',
        message: 'Failed to create customer',
      },
      timestamp: new Date(),
    } as Types.ApiResponse<null>);
  }
});

app.get('/api/v1/customers/:id', authenticateJWT, async (req: AuthenticatedRequest, res: Response) => {
  try {
    const { id } = req.params;

    const result = await pool.query(
      'SELECT * FROM customers WHERE id = $1',
      [id]
    );

    if (result.rows.length === 0) {
      return res.status(404).json({
        success: false,
        error: {
          code: 'NOT_FOUND',
          message: 'Customer not found',
        },
        timestamp: new Date(),
      } as Types.ApiResponse<null>);
    }

    res.json({
      success: true,
      data: result.rows[0],
      timestamp: new Date(),
    } as Types.ApiResponse<any>);
  } catch (err) {
    logger.error('Get customer failed', { error: err });
    res.status(500).json({
      success: false,
      error: {
        code: 'INTERNAL_ERROR',
        message: 'Failed to fetch customer',
      },
      timestamp: new Date(),
    } as Types.ApiResponse<null>);
  }
});

// Setup wizard endpoints
app.post('/api/v1/customers/:id/setup/step1', authenticateJWT, async (req: AuthenticatedRequest, res: Response) => {
  try {
    const { id } = req.params;
    const { apiKey, webhookUrl } = req.body;

    // Validate setup step 1: API Integration
    const validation = Schemas.updateSetupStepSchema.safeParse({
      status: Types.SetupStepStatus.IN_PROGRESS,
    });

    if (!validation.success) {
      return res.status(400).json({
        success: false,
        error: {
          code: 'VALIDATION_ERROR',
          message: 'Invalid setup step data',
        },
        timestamp: new Date(),
      } as Types.ApiResponse<null>);
    }

    const now = new Date();
    const result = await pool.query(
      `UPDATE setup_steps SET status = $1, updated_at = $2
       WHERE customer_id = $3 AND step = 1
       RETURNING *`,
      [Types.SetupStepStatus.COMPLETED, now, id]
    );

    res.json({
      success: true,
      data: result.rows[0],
      timestamp: new Date(),
    } as Types.ApiResponse<any>);
  } catch (err) {
    logger.error('Setup step 1 failed', { error: err });
    res.status(500).json({
      success: false,
      error: {
        code: 'INTERNAL_ERROR',
        message: 'Failed to complete setup step 1',
      },
      timestamp: new Date(),
    } as Types.ApiResponse<null>);
  }
});

// Value definition endpoints
app.post('/api/v1/customers/:id/value-definitions', authenticateJWT, async (req: AuthenticatedRequest, res: Response) => {
  try {
    const { id } = req.params;
    const validation = Schemas.createValueDefinitionSchema.safeParse(req.body);

    if (!validation.success) {
      return res.status(400).json({
        success: false,
        error: {
          code: 'VALIDATION_ERROR',
          message: 'Invalid value definition data',
          details: validation.error.errors,
        },
        timestamp: new Date(),
      } as Types.ApiResponse<null>);
    }

    const definitionId = uuidv4();
    const now = new Date();

    const result = await pool.query(
      `INSERT INTO value_definitions
       (id, customer_id, name, description, metrics, baseline_value, target_value,
        scoring_model, custom_formula, status, created_at, updated_at)
       VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
       RETURNING *`,
      [
        definitionId,
        id,
        validation.data.name,
        validation.data.description,
        JSON.stringify(validation.data.metrics),
        validation.data.baselineValue,
        validation.data.targetValue,
        validation.data.scoringModel,
        validation.data.customFormula,
        'ACTIVE',
        now,
        now,
      ]
    );

    res.status(201).json({
      success: true,
      data: result.rows[0],
      timestamp: new Date(),
    } as Types.ApiResponse<any>);
  } catch (err) {
    logger.error('Create value definition failed', { error: err });
    res.status(500).json({
      success: false,
      error: {
        code: 'INTERNAL_ERROR',
        message: 'Failed to create value definition',
      },
      timestamp: new Date(),
    } as Types.ApiResponse<null>);
  }
});

// Dashboard endpoint
app.get('/api/v1/customers/:id/dashboard/summary', authenticateJWT, async (req: AuthenticatedRequest, res: Response) => {
  try {
    const { id } = req.params;

    const customerResult = await pool.query(
      'SELECT * FROM customers WHERE id = $1',
      [id]
    );

    if (customerResult.rows.length === 0) {
      return res.status(404).json({
        success: false,
        error: {
          code: 'NOT_FOUND',
          message: 'Customer not found',
        },
        timestamp: new Date(),
      } as Types.ApiResponse<null>);
    }

    const customer = customerResult.rows[0];
    const setupStepsResult = await pool.query(
      'SELECT * FROM setup_steps WHERE customer_id = $1 ORDER BY step',
      [id]
    );

    const alertsResult = await pool.query(
      'SELECT * FROM alerts WHERE customer_id = $1 AND is_resolved = false',
      [id]
    );

    const ticketsResult = await pool.query(
      'SELECT COUNT(*) FROM support_tickets WHERE customer_id = $1 AND status != $2',
      [id, 'CLOSED']
    );

    const valueCalcResult = await pool.query(
      'SELECT * FROM value_calculations WHERE customer_id = $1 ORDER BY timestamp DESC LIMIT 1',
      [id]
    );

    const setupProgress = (setupStepsResult.rows.filter((s: any) => s.status === 'COMPLETED').length / 5) * 100;

    const dashboard: Types.DashboardSummary = {
      customerId: customer.id,
      status: customer.status,
      setupProgress,
      setupSteps: setupStepsResult.rows,
      currentValue: valueCalcResult.rows[0]?.calculated_value || 0,
      targetValue: 100,
      valueChangePercentage: valueCalcResult.rows[0]?.change_percentage || 0,
      measurementAccuracy: 85, // Example value
      approvalsStatus: {},
      lastMeasurementAt: valueCalcResult.rows[0]?.timestamp || new Date(),
      nextMeasurementAt: new Date(Date.now() + 24 * 60 * 60 * 1000),
      activeAlerts: alertsResult.rows,
      openTickets: parseInt(ticketsResult.rows[0].count, 10),
    };

    res.json({
      success: true,
      data: dashboard,
      timestamp: new Date(),
    } as Types.ApiResponse<Types.DashboardSummary>);
  } catch (err) {
    logger.error('Get dashboard failed', { error: err });
    res.status(500).json({
      success: false,
      error: {
        code: 'INTERNAL_ERROR',
        message: 'Failed to fetch dashboard',
      },
      timestamp: new Date(),
    } as Types.ApiResponse<null>);
  }
});

// Support ticket endpoints
app.post('/api/v1/customers/:id/tickets', authenticateJWT, async (req: AuthenticatedRequest, res: Response) => {
  try {
    const { id } = req.params;
    const validation = Schemas.createSupportTicketSchema.safeParse(req.body);

    if (!validation.success) {
      return res.status(400).json({
        success: false,
        error: {
          code: 'VALIDATION_ERROR',
          message: 'Invalid ticket data',
          details: validation.error.errors,
        },
        timestamp: new Date(),
      } as Types.ApiResponse<null>);
    }

    const ticketId = uuidv4();
    const now = new Date();

    const result = await pool.query(
      `INSERT INTO support_tickets
       (id, customer_id, title, description, status, priority, category, created_at, updated_at)
       VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
       RETURNING *`,
      [
        ticketId,
        id,
        validation.data.title,
        validation.data.description,
        'OPEN',
        validation.data.priority,
        validation.data.category,
        now,
        now,
      ]
    );

    res.status(201).json({
      success: true,
      data: result.rows[0],
      timestamp: new Date(),
    } as Types.ApiResponse<any>);
  } catch (err) {
    logger.error('Create support ticket failed', { error: err });
    res.status(500).json({
      success: false,
      error: {
        code: 'INTERNAL_ERROR',
        message: 'Failed to create support ticket',
      },
      timestamp: new Date(),
    } as Types.ApiResponse<null>);
  }
});

// Error handling middleware
app.use((err: ApiError, req: Request, res: Response, next: NextFunction) => {
  logger.error('Unhandled error', { error: err });
  res.status(err.statusCode || 500).json({
    success: false,
    error: {
      code: err.code || 'INTERNAL_ERROR',
      message: err.message || 'Internal server error',
      details: err.details,
    },
    timestamp: new Date(),
  } as Types.ApiResponse<null>);
});

// Start server
const PORT = parseInt(process.env.PORT || '3001', 10);
const server = app.listen(PORT, () => {
  logger.info(`Onboarding API server running on port ${PORT}`);
});

export default app;
