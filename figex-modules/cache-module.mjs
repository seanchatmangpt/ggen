// cache.mjs
import { Map } from 'immutable';

/**
 * In-memory cache with TTL, LRU eviction, and size limits.
 */
export class Cache {
  constructor(options = {}) {
    this.options = {
      maxEntries: 1000,
      ...options
    };
    this.cache = new Map();
    this.expiryMap = new Map();
    this.stats = {
      hits: 0,
      misses: 0,
      evictions: 0,
      totalEntries: 0
    };
  }

  /**
   * Set a key-value pair with an optional TTL.
   * @param {string} key - The key to store.
   * @param {any} value - The value to store.
   * @param {number} [ttl=0] - Time to live in milliseconds.
   */
  set(key, value, ttl = 0) {
    if (typeof key !== 'string') {
      throw new TypeError('Key must be a string');
    }

    const now = Date.now();
    const expiry = ttl > 0 ? now + ttl : null;

    if (this.cache.has(key)) {
      this.cache = this.cache.set(key, value);
      this.expiryMap = this.expiryMap.set(key, expiry);
      return;
    }

    if (this.cache.size >= this.options.maxEntries) {
      this._evict();
    }

    this.cache = this.cache.set(key, value);
    this.expiryMap = this.expiryMap.set(key, expiry);
    this.stats.totalEntries++;
  }

  /**
   * Get a value by key.
   * @param {string} key - The key to retrieve.
   * @returns {any} The value if found, undefined otherwise.
   */
  get(key) {
    if (typeof key !== 'string') {
      throw new TypeError('Key must be a string');
    }

    const now = Date.now();
    const expiry = this.expiryMap.get(key);

    if (!expiry || now < expiry) {
      const value = this.cache.get(key);
      if (value !== undefined) {
        this.stats.hits++;
      } else {
        this.stats.misses++;
      }
      return value;
    }

    this.stats.misses++;
    this._evict();
    return undefined;
  }

  /**
   * Delete a key from the cache.
   * @param {string} key - The key to delete.
   */
  delete(key) {
    if (typeof key !== 'string') {
      throw new TypeError('Key must be a string');
    }

    if (this.cache.has(key)) {
      this.cache = this.cache.delete(key);
      this.expiryMap = this.expiryMap.delete(key);
      this.stats.totalEntries--;
    }
  }

  /**
   * Clear all entries from the cache.
   */
  clear() {
    this.cache = new Map();
    this.expiryMap = new Map();
    this.stats = {
      hits: 0,
      misses: 0,
      evictions: 0,
      totalEntries: 0
    };
  }

  /**
   * Get cache statistics.
   * @returns {Object} Cache statistics.
   */
  stats() {
    return { ...this.stats };
  }

  /**
   * Evict the least recently used entry.
   * @private
   */
  _evict() {
    if (this.cache.size < this.options.maxEntries) return;

    const now = Date.now();
    const expiredKeys = Array.from(this.expiryMap.entries())
      .filter(([key, expiry]) => expiry && now >= expiry)
      .map(([key]) => key);

    if (expiredKeys.length > 0) {
      this.cache = this.cache.delete(expiredKeys);
      this.expiryMap = this.expiryMap.delete(expiredKeys);
      this.stats.evictions += expiredKeys.length;
      this.stats.totalEntries -= expiredKeys.length;
    }

    if (this.cache.size >= this.options.maxEntries) {
      const lruKey = Array.from(this.cache.keys()).sort((a, b) => {
        const aExpiry = this.expiryMap.get(a);
        const bExpiry = this.expiryMap.get(b);
        return aExpiry - bExpiry;
      })[0];

      if (lruKey) {
        this.cache = this.cache.delete(lruKey);
        this.expiryMap = this.expiryMap.delete(lruKey);
        this.stats.evictions++;
        this.stats.totalEntries--;
      }
    }
  }
}