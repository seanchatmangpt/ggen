// API Gateway & Service Mesh - TypeScript Implementation
// Production-grade Express-based API gateway

import express, { Request, Response, NextFunction } from 'express';
import axios, { AxiosRequestConfig } from 'axios';
import CircuitBreaker from 'opossum';
import rateLimit from 'express-rate-limit';
import { createProxyMiddleware } from 'http-proxy-middleware';

export interface RouteConfig {
  path: string;
  method: string;
  backendService: string;
  rateLimitRps?: number;
  circuitBreaker?: CircuitBreakerConfig;
  retryPolicy?: RetryConfig;
  timeoutSeconds: number;
}

export interface CircuitBreakerConfig {
  failureThreshold: number;
  timeoutSeconds: number;
  halfOpenRequests: number;
}

export interface RetryConfig {
  maxAttempts: number;
  backoffMultiplier: number;
  initialDelayMs: number;
}

export interface ServiceMeshConfig {
  meshType: 'istio' | 'linkerd' | 'consul';
  mtlsMode: 'STRICT' | 'PERMISSIVE' | 'DISABLE';
  sidecarPort: number;
  controlPlaneEndpoint: string;
}

export interface MetricsCollector {
  requestCount: number;
  errorCount: number;
  latencyHistogram: number[];
}

export class ApiGateway {
  private app: express.Application;
  private routes: Map<string, RouteConfig>;
  private circuitBreakers: Map<string, CircuitBreaker>;
  private meshConfig: ServiceMeshConfig;
  private metrics: MetricsCollector;

  constructor(meshConfig: ServiceMeshConfig) {
    this.app = express();
    this.routes = new Map();
    this.circuitBreakers = new Map();
    this.meshConfig = meshConfig;
    this.metrics = {
      requestCount: 0,
      errorCount: 0,
      latencyHistogram: []
    };

    this.setupMiddleware();
  }

  private setupMiddleware(): void {
    // Body parsing
    this.app.use(express.json());
    this.app.use(express.urlencoded({ extended: true }));

    // Request logging
    this.app.use((req, res, next) => {
      const start = Date.now();
      res.on('finish', () => {
        const duration = Date.now() - start;
        this.metrics.latencyHistogram.push(duration);
        this.metrics.requestCount++;

        console.log(`${req.method} ${req.path} - ${res.statusCode} - ${duration}ms`);
      });
      next();
    });

    // Error handling
    this.app.use((err: Error, req: Request, res: Response, next: NextFunction) => {
      this.metrics.errorCount++;
      console.error('Error:', err);
      res.status(500).json({ error: 'Internal server error' });
    });
  }

  public addRoute(config: RouteConfig): void {
    const routeKey = `${config.method}:${config.path}`;
    this.routes.set(routeKey, config);

    // Create rate limiter if configured
    const middleware: express.RequestHandler[] = [];

    if (config.rateLimitRps) {
      middleware.push(rateLimit({
        windowMs: 1000,
        max: config.rateLimitRps,
        message: 'Rate limit exceeded'
      }));
    }

    // Create circuit breaker for backend
    if (config.circuitBreaker) {
      const breaker = new CircuitBreaker(
        async (url: string, options: AxiosRequestConfig) => {
          return await axios(url, options);
        },
        {
          timeout: config.circuitBreaker.timeoutSeconds * 1000,
          errorThresholdPercentage: 50,
          resetTimeout: config.circuitBreaker.timeoutSeconds * 1000,
          volumeThreshold: config.circuitBreaker.failureThreshold
        }
      );

      breaker.on('open', () => {
        console.log(`Circuit breaker OPEN for ${config.backendService}`);
      });

      breaker.on('halfOpen', () => {
        console.log(`Circuit breaker HALF_OPEN for ${config.backendService}`);
      });

      breaker.on('close', () => {
        console.log(`Circuit breaker CLOSED for ${config.backendService}`);
      });

      this.circuitBreakers.set(config.backendService, breaker);
    }

    // Setup route handler
    const handler = async (req: Request, res: Response, next: NextFunction) => {
      try {
        const result = await this.proxyRequest(req, config);
        res.status(result.status).json(result.data);
      } catch (error) {
        next(error);
      }
    };

    // Register route
    const method = config.method.toLowerCase() as keyof express.Application;
    (this.app[method] as Function)(config.path, ...middleware, handler);
  }

  private async proxyRequest(req: Request, config: RouteConfig): Promise<any> {
    const backendUrl = `http://${config.backendService}${req.path}`;
    const breaker = this.circuitBreakers.get(config.backendService);

    const requestConfig: AxiosRequestConfig = {
      method: req.method as any,
      url: backendUrl,
      data: req.body,
      headers: this.buildHeaders(req),
      timeout: config.timeoutSeconds * 1000
    };

    // Execute with circuit breaker if configured
    if (breaker) {
      return await breaker.fire(backendUrl, requestConfig);
    }

    // Execute with retry if configured
    if (config.retryPolicy) {
      return await this.executeWithRetry(requestConfig, config.retryPolicy);
    }

    // Direct execution
    return await axios(requestConfig);
  }

  private buildHeaders(req: Request): Record<string, string> {
    const headers: Record<string, string> = {};

    // Copy original headers
    Object.entries(req.headers).forEach(([key, value]) => {
      if (typeof value === 'string') {
        headers[key] = value;
      }
    });

    // Add distributed tracing headers (W3C Trace Context)
    if (!headers['traceparent']) {
      const traceId = this.generateTraceId();
      const spanId = this.generateSpanId();
      headers['traceparent'] = `00-${traceId}-${spanId}-01`;
    }

    // Add service mesh headers
    if (this.meshConfig.meshType === 'istio') {
      headers['x-envoy-internal'] = 'true';
    }

    return headers;
  }

  private async executeWithRetry(
    config: AxiosRequestConfig,
    retryPolicy: RetryConfig
  ): Promise<any> {
    let lastError: Error | null = null;
    let delay = retryPolicy.initialDelayMs;

    for (let attempt = 1; attempt <= retryPolicy.maxAttempts; attempt++) {
      try {
        return await axios(config);
      } catch (error) {
        lastError = error as Error;

        if (attempt < retryPolicy.maxAttempts) {
          await this.sleep(delay);
          delay *= retryPolicy.backoffMultiplier;
        }
      }
    }

    throw lastError;
  }

  private sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }

  private generateTraceId(): string {
    return Array.from({ length: 32 }, () =>
      Math.floor(Math.random() * 16).toString(16)
    ).join('');
  }

  private generateSpanId(): string {
    return Array.from({ length: 16 }, () =>
      Math.floor(Math.random() * 16).toString(16)
    ).join('');
  }

  public setupHealthCheck(): void {
    this.app.get('/health', (req, res) => {
      const errorRate = this.metrics.requestCount > 0
        ? (this.metrics.errorCount / this.metrics.requestCount) * 100
        : 0;

      res.json({
        status: 'healthy',
        meshType: this.meshConfig.meshType,
        mtlsMode: this.meshConfig.mtlsMode,
        metrics: {
          requests: this.metrics.requestCount,
          errors: this.metrics.errorCount,
          errorRate: errorRate.toFixed(2) + '%'
        }
      });
    });
  }

  public listen(port: number, callback?: () => void): void {
    this.setupHealthCheck();
    this.app.listen(port, callback || (() => {
      console.log(`API Gateway listening on port ${port}`);
      console.log(`Service Mesh: ${this.meshConfig.meshType} (${this.meshConfig.mtlsMode})`);
    }));
  }
}

// Example usage
if (require.main === module) {
  const meshConfig: ServiceMeshConfig = {
    meshType: 'istio',
    mtlsMode: 'STRICT',
    sidecarPort: 15001,
    controlPlaneEndpoint: 'istiod.istio-system:15012'
  };

  const gateway = new ApiGateway(meshConfig);

  // Add routes
  gateway.addRoute({
    path: '/api/v1/users',
    method: 'GET',
    backendService: 'users-service:8080',
    rateLimitRps: 100,
    circuitBreaker: {
      failureThreshold: 5,
      timeoutSeconds: 30,
      halfOpenRequests: 3
    },
    retryPolicy: {
      maxAttempts: 3,
      backoffMultiplier: 2.0,
      initialDelayMs: 100
    },
    timeoutSeconds: 5
  });

  gateway.listen(8080);
}
