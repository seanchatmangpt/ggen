// Generated REST API using Express.js with TypeScript
// This demonstrates RDF-driven code generation for TypeScript

import express, { Request, Response, NextFunction } from 'express';
import { body, param, query, validationResult } from 'express-validator';
import cors from 'cors';
import helmet from 'helmet';
import rateLimit from 'express-rate-limit';
import { v4 as uuidv4 } from 'uuid';

// ============================================================================
// Type Definitions (Generated from RDF RequestSchema/ResponseSchema)
// ============================================================================

interface User {
  id: string;
  username: string;
  email: string;
  createdAt: Date;
  updatedAt: Date;
}

interface CreateUserRequest {
  username: string;
  email: string;
}

interface UpdateUserRequest {
  username?: string;
  email?: string;
}

interface ApiResponse<T> {
  success: boolean;
  data?: T;
  error?: string;
  meta?: {
    page?: number;
    limit?: number;
    total?: number;
  };
}

interface QueryParams {
  page?: string;
  limit?: string;
}

// ============================================================================
// Database Layer (In-Memory Store for Template)
// ============================================================================

class DatabaseService {
  private users: Map<string, User> = new Map();

  async getUsers(page: number = 1, limit: number = 10): Promise<{ users: User[]; total: number }> {
    const allUsers = Array.from(this.users.values());
    const start = (page - 1) * limit;
    const end = start + limit;
    return {
      users: allUsers.slice(start, end),
      total: allUsers.length,
    };
  }

  async getUserById(id: string): Promise<User | null> {
    return this.users.get(id) || null;
  }

  async createUser(data: CreateUserRequest): Promise<User> {
    const user: User = {
      id: uuidv4(),
      username: data.username,
      email: data.email,
      createdAt: new Date(),
      updatedAt: new Date(),
    };
    this.users.set(user.id, user);
    return user;
  }

  async updateUser(id: string, data: UpdateUserRequest): Promise<User | null> {
    const user = this.users.get(id);
    if (!user) return null;

    const updated: User = {
      ...user,
      username: data.username ?? user.username,
      email: data.email ?? user.email,
      updatedAt: new Date(),
    };
    this.users.set(id, updated);
    return updated;
  }

  async deleteUser(id: string): Promise<boolean> {
    return this.users.delete(id);
  }
}

// ============================================================================
// Middleware (Generated from rest:Middleware)
// ============================================================================

// Authentication Middleware (JWT validation)
const authMiddleware = (req: Request, res: Response, next: NextFunction) => {
  const authHeader = req.headers.authorization;

  if (!authHeader || !authHeader.startsWith('Bearer ')) {
    return res.status(401).json({
      success: false,
      error: 'Unauthorized - Missing or invalid token',
    } as ApiResponse<never>);
  }

  // In production, validate JWT token here
  const token = authHeader.substring(7);
  // For template: accept any token
  next();
};

// Error Handler Middleware
const errorHandler = (
  err: Error,
  req: Request,
  res: Response,
  next: NextFunction
) => {
  console.error(err.stack);

  const statusCode = res.statusCode !== 200 ? res.statusCode : 500;

  res.status(statusCode).json({
    success: false,
    error: err.message,
  } as ApiResponse<never>);
};

// Validation Error Handler
const validationErrorHandler = (req: Request, res: Response, next: NextFunction) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return res.status(422).json({
      success: false,
      error: 'Validation failed',
      data: errors.array(),
    });
  }
  next();
};

// ============================================================================
// Rate Limiting (Generated from rest:RateLimiter)
// ============================================================================

const limiter = rateLimit({
  windowMs: 1 * 60 * 1000, // 1 minute
  max: 100, // 100 requests per minute
  message: {
    success: false,
    error: 'Too many requests, please try again later',
  } as ApiResponse<never>,
});

// ============================================================================
// Validation Rules (Generated from rest:ValidationRule)
// ============================================================================

const createUserValidation = [
  body('username')
    .isString()
    .trim()
    .isLength({ min: 3, max: 30 })
    .withMessage('Username must be 3-30 characters'),
  body('email')
    .isEmail()
    .normalizeEmail()
    .withMessage('Must be a valid email address'),
];

const updateUserValidation = [
  body('username')
    .optional()
    .isString()
    .trim()
    .isLength({ min: 3, max: 30 })
    .withMessage('Username must be 3-30 characters'),
  body('email')
    .optional()
    .isEmail()
    .normalizeEmail()
    .withMessage('Must be a valid email address'),
];

const idParamValidation = [
  param('id').isUUID().withMessage('Invalid user ID format'),
];

const queryValidation = [
  query('page').optional().isInt({ min: 1 }).toInt(),
  query('limit').optional().isInt({ min: 1, max: 100 }).toInt(),
];

// ============================================================================
// Route Handlers (Generated from SPARQL Query 2)
// ============================================================================

class UserController {
  constructor(private db: DatabaseService) {}

  // GET /api/users - List all users
  listUsers = async (req: Request<{}, {}, {}, QueryParams>, res: Response) => {
    try {
      const page = parseInt(req.query.page || '1', 10);
      const limit = parseInt(req.query.limit || '10', 10);

      const { users, total } = await this.db.getUsers(page, limit);

      res.status(200).json({
        success: true,
        data: users,
        meta: { page, limit, total },
      } as ApiResponse<User[]>);
    } catch (error) {
      res.status(500).json({
        success: false,
        error: 'Failed to fetch users',
      } as ApiResponse<never>);
    }
  };

  // GET /api/users/:id - Get single user
  getUser = async (req: Request<{ id: string }>, res: Response) => {
    try {
      const user = await this.db.getUserById(req.params.id);

      if (!user) {
        return res.status(404).json({
          success: false,
          error: 'User not found',
        } as ApiResponse<never>);
      }

      res.status(200).json({
        success: true,
        data: user,
      } as ApiResponse<User>);
    } catch (error) {
      res.status(500).json({
        success: false,
        error: 'Failed to fetch user',
      } as ApiResponse<never>);
    }
  };

  // POST /api/users - Create new user
  createUser = async (req: Request<{}, {}, CreateUserRequest>, res: Response) => {
    try {
      const user = await this.db.createUser(req.body);

      res.status(201).json({
        success: true,
        data: user,
      } as ApiResponse<User>);
    } catch (error) {
      res.status(500).json({
        success: false,
        error: 'Failed to create user',
      } as ApiResponse<never>);
    }
  };

  // PUT /api/users/:id - Update user
  updateUser = async (req: Request<{ id: string }, {}, UpdateUserRequest>, res: Response) => {
    try {
      const user = await this.db.updateUser(req.params.id, req.body);

      if (!user) {
        return res.status(404).json({
          success: false,
          error: 'User not found',
        } as ApiResponse<never>);
      }

      res.status(200).json({
        success: true,
        data: user,
      } as ApiResponse<User>);
    } catch (error) {
      res.status(500).json({
        success: false,
        error: 'Failed to update user',
      } as ApiResponse<never>);
    }
  };

  // DELETE /api/users/:id - Delete user
  deleteUser = async (req: Request<{ id: string }>, res: Response) => {
    try {
      const deleted = await this.db.deleteUser(req.params.id);

      if (!deleted) {
        return res.status(404).json({
          success: false,
          error: 'User not found',
        } as ApiResponse<never>);
      }

      res.status(204).send();
    } catch (error) {
      res.status(500).json({
        success: false,
        error: 'Failed to delete user',
      } as ApiResponse<never>);
    }
  };
}

// ============================================================================
// Router Configuration (Generated from SPARQL Query 13)
// ============================================================================

function createRouter(db: DatabaseService): express.Router {
  const router = express.Router();
  const userController = new UserController(db);

  // Health check
  router.get('/health', (req, res) => {
    res.status(200).json({
      status: 'healthy',
      timestamp: new Date().toISOString(),
    });
  });

  // User routes
  router.get(
    '/api/users',
    queryValidation,
    validationErrorHandler,
    userController.listUsers
  );

  router.get(
    '/api/users/:id',
    idParamValidation,
    validationErrorHandler,
    userController.getUser
  );

  router.post(
    '/api/users',
    createUserValidation,
    validationErrorHandler,
    userController.createUser
  );

  router.put(
    '/api/users/:id',
    idParamValidation,
    updateUserValidation,
    validationErrorHandler,
    userController.updateUser
  );

  router.delete(
    '/api/users/:id',
    idParamValidation,
    validationErrorHandler,
    userController.deleteUser
  );

  return router;
}

// ============================================================================
// Application Setup
// ============================================================================

function createApp(db: DatabaseService): express.Application {
  const app = express();

  // Security middleware
  app.use(helmet());
  app.use(cors());
  app.use(limiter);

  // Body parsing
  app.use(express.json());
  app.use(express.urlencoded({ extended: true }));

  // Routes
  app.use(createRouter(db));

  // Error handling
  app.use(errorHandler);

  return app;
}

// ============================================================================
// Server Entry Point
// ============================================================================

if (require.main === module) {
  const db = new DatabaseService();
  const app = createApp(db);
  const PORT = process.env.PORT || 3000;

  app.listen(PORT, () => {
    console.log(`🚀 Server running on http://localhost:${PORT}`);
  });
}

export { createApp, DatabaseService, UserController };
