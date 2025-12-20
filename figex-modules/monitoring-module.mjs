import { register, Counter, Histogram, Summary } from 'prom-client';

const metrics = {
  requestCounter: new Counter({
    name: 'http_requests_total',
    help: 'Total number of HTTP requests',
    labelNames: ['method', 'route', 'status'],
  }),
  errorCounter: new Counter({
    name: 'http_errors_total',
    help: 'Total number of HTTP errors',
    labelNames: ['type'],
  }),
  responseTimeHistogram: new Histogram({
    name: 'http_request_duration_seconds',
    help: 'Duration of HTTP requests in seconds',
    labelNames: ['method', 'route'],
    // Use exponential buckets for better distribution
    buckets: [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 30, 60],
  }),
};

export function initMetrics() {
  register.registerMetric(metrics.requestCounter);
  register.registerMetric(metrics.errorCounter);
  register.registerMetric(metrics.responseTimeHistogram);
}

export function recordRequest(duration, status, method, route) {
  try {
    metrics.requestCounter.inc({ method, route, status });
    metrics.responseTimeHistogram.observe({ method, route }, duration);
  } catch (err) {
    console.error('Failed to record request metrics:', err);
  }
}

export function recordError(type) {
  try {
    metrics.errorCounter.inc({ type });
  } catch (err) {
    console.error('Failed to record error metrics:', err);
  }
}

export function getMetrics() {
  try {
    return register.metrics();
  } catch (err) {
    console.error('Failed to get metrics:', err);
    return [];
  }
}