/**
 * LRU Cache class that stores key-value pairs and evicts the least recently used item when the cache is full.
 * @param {number} maxSize - Maximum number of items the cache can hold. Defaults to 100.
 * @throws {Error} If maxSize is less than 1.
 * @example
 * const cache = new LRUCache(2)
 * cache.set('key1', 'value1')
 * cache.set('key2', 'value2')
 * console.log(cache.get('key1')) // 'value1'
 * cache.set('key3', 'value3') // 'key2' is evicted
 * @example
 * try { new LRUCache(0) } catch (e) { console.error(e.message) } // 'maxSize must be at least 1'
 */
export class LRUCache {
  /**
   * Creates an instance of LRUCache.
   * @param {number} [maxSize=100] - Maximum number of items the cache can hold.
   */
  constructor(maxSize = 100) {
    if (maxSize < 1) {
      throw new Error('maxSize must be at least 1')
    }
    this.maxSize = maxSize
    this.cache = new Map()
    this.order = []
  }

  /**
   * Sets a key-value pair in the cache. If the cache is full, the least recently used item is evicted.
   * @param {string} key - The key to store.
   * @param {any} value - The value to store.
   * @returns {void}
   * @throws {Error} If key is not a string.
   * @example
   * const cache = new LRUCache(2)
   * cache.set('key1', 'value1')
   * cache.set('key2', 'value2')
   * cache.set('key3', 'value3') // 'key2' is evicted
   */
  set(key, value) {
    if (typeof key !== 'string') {
      throw new Error('key must be a string')
    }

    if (this.cache.has(key)) {
      this.cache.set(key, value)
      this.moveToEnd(key)
      return
    }

    if (this.cache.size >= this.maxSize) {
      const lruKey = this.order.shift()
      this.cache.delete(lruKey)
    }

    this.cache.set(key, value)
    this.order.push(key)
  }

  /**
   * Gets the value associated with a key. If the key is found, it is marked as recently used.
   * @param {string} key - The key to retrieve.
   * @returns {any | undefined} The value associated with the key, or undefined if the key is not found.
   * @throws {Error} If key is not a string.
   * @example
   * const cache = new LRUCache(2)
   * cache.set('key1', 'value1')
   * console.log(cache.get('key1')) // 'value1'
   */
  get(key) {
    if (typeof key !== 'string') {
      throw new Error('key must be a string')
    }

    if (!this.cache.has(key)) {
      return undefined
    }

    const value = this.cache.get(key)
    this.moveToEnd(key)
    return value
  }

  /**
   * Deletes a key from the cache.
   * @param {string} key - The key to delete.
   * @returns {void}
   * @throws {Error} If key is not a string.
   * @example
   * const cache = new LRUCache(2)
   * cache.set('key1', 'value1')
   * cache.delete('key1') // cache is now empty
   */
  delete(key) {
    if (typeof key !== 'string') {
      throw new Error('key must be a string')
    }

    if (this.cache.has(key)) {
      this.cache.delete(key)
      this.order = this.order.filter(k => k !== key)
    }
  }

  /**
   * Moves a key to the end of the order list to mark it as recently used.
   * @private
   * @param {string} key - The key to move.
   * @returns {void}
   */
  moveToEnd(key) {
    const index = this.order.indexOf(key)
    if (index > -1) {
      this.order.splice(index, 1)
      this.order.push(key)
    }
  }
}