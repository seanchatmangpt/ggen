import { describe, it, expect } from 'vitest';
import { Cache } from './cache.mjs';

describe('Cache Module', () => {
  it('should set and get a value', () => {
    const cache = new Cache();
    cache.set('key1', 'value1');
    expect(cache.get('key1')).toBe('value1');
  });

  it('should return undefined for non-existent key', () => {
    const cache = new Cache();
    expect(cache.get('nonexistent')).toBeUndefined();
  });

  it('should delete a key', () => {
    const cache = new Cache();
    cache.set('key2', 'value2');
    cache.delete('key2');
    expect(cache.get('key2')).toBeUndefined();
  });

  it('should clear all entries', () => {
    const cache = new Cache();
    cache.set('key3', 'value3');
    cache.set('key4', 'value4');
    cache.clear();
    expect(cache.get('key3')).toBeUndefined();
    expect(cache.get('key4')).toBeUndefined();
  });

  it('should expire entries after TTL', (done) => {
    const cache = new Cache({ ttl: 1000 });
    cache.set('key5', 'value5');
    setTimeout(() => {
      expect(cache.get('key5')).toBeUndefined();
      done();
    }, 1000);
  });

  it('should evict LRU entries when size exceeds limit', () => {
    const cache = new Cache({ maxSize: 2 });
    cache.set('key6', 'value6');
    cache.set('key7', 'value7');
    cache.set('key8', 'value8');
    expect(cache.get('key6')).toBeUndefined();
    expect(cache.get('key7')).toBe('value7');
    expect(cache.get('key8')).toBe('value8');
  });
});