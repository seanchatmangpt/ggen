/**
 * Doctest - Validates JSDoc @example blocks
 * Tests the code examples from JSDoc comments to ensure they work
 */

import { describe, it, expect } from 'vitest'
import EventBus from './event-bus.mjs'
import { chunk } from './chunk-array.mjs'
import { debounce, throttle } from './debounce.mjs'

describe('EventBus - JSDoc Examples', () => {
  it('should register and emit events', () => {
    const bus = new EventBus()
    let receivedData = null

    bus.on('click', (data) => {
      receivedData = data
    })
    bus.emit('click', 'button1')

    expect(receivedData).toBe('button1')
  })

  it('should remove event listeners', () => {
    const bus = new EventBus()
    let callCount = 0

    const handler = () => callCount++
    bus.on('click', handler)
    bus.emit('click')
    expect(callCount).toBe(1)

    bus.off('click', handler)
    bus.emit('click')
    expect(callCount).toBe(1) // Should not increase
  })
})

describe('chunk - JSDoc Examples', () => {
  it('should split array into chunks', () => {
    const result = chunk([1, 2, 3, 4, 5], 2)
    expect(result).toEqual([[1, 2], [3, 4], [5]])
  })

  it('should throw TypeError for non-array', () => {
    expect(() => chunk('not-array', 2)).toThrow(TypeError)
  })

  it('should throw RangeError for invalid size', () => {
    expect(() => chunk([1, 2, 3], 0)).toThrow(RangeError)
    expect(() => chunk([1, 2, 3], -1)).toThrow(RangeError)
  })
})

describe('debounce - JSDoc Examples', () => {
  it('should delay function execution', async () => {
    let callCount = 0
    const debouncedFn = debounce(() => callCount++, 100)

    // Call multiple times rapidly
    debouncedFn()
    debouncedFn()
    debouncedFn()

    // Should not have executed yet
    expect(callCount).toBe(0)

    // Wait for debounce delay
    await new Promise(resolve => setTimeout(resolve, 150))

    // Should have executed once
    expect(callCount).toBe(1)
  })
})

describe('throttle - JSDoc Examples', () => {
  it('should limit execution rate', async () => {
    let callCount = 0
    const throttledFn = throttle(() => callCount++, 100)

    // Call multiple times
    throttledFn() // Should execute (1)
    expect(callCount).toBe(1)

    throttledFn() // Should be throttled
    expect(callCount).toBe(1)

    // Wait for throttle window
    await new Promise(resolve => setTimeout(resolve, 150))

    throttledFn() // Should execute (2)
    expect(callCount).toBe(2)
  })
})
