import { describe, it, expect } from 'vitest';
import { initMetrics, recordRequest, recordError, getMetrics } from './monitoring';

describe('Monitoring Module', () => {
  it('should initialize metrics with default values', () => {
    initMetrics();
    expect(getMetrics()).toEqual({ requests: 0, errors: 0 });
  });

  it('should record a request and update metrics', () => {
    initMetrics();
    recordRequest();
    expect(getMetrics()).toEqual({ requests: 1, errors: 0 });
  });

  it('should record an error and update metrics', () => {
    initMetrics();
    recordError();
    expect(getMetrics()).toEqual({ requests: 0, errors: 1 });
  });

  it('should record multiple requests and errors', () => {
    initMetrics();
    recordRequest();
    recordRequest();
    recordError();
    expect(getMetrics()).toEqual({ requests: 2, errors: 1 });
  });

  it('should return correct metrics after multiple operations', () => {
    initMetrics();
    recordRequest();
    recordError();
    recordRequest();
    expect(getMetrics()).toEqual({ requests: 2, errors: 1 });
  });
});