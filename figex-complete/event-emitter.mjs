/**
 * EventEmitter class for handling custom events
 * @class
 */
export class EventEmitter {
  /**
   * Create a new EventEmitter instance
   * @constructor
   */
  constructor() {
    this._events = Object.create(null);
  }

  /**
   * Register a listener for a specific event
   * @param {string} event - The name of the event
   * @param {Function} fn - The callback function to execute when the event is emitted
   * @throws {Error} If event is not a string or fn is not a function
   * @example
   * const emitter = new EventEmitter();
   * emitter.on('message', (data) => console.log(data));
   * emitter.emit('message', 'Hello, world!');
   */
  on(event, fn) {
    if (typeof event !== 'string') {
      throw new Error('Event name must be a string');
    }
    if (typeof fn !== 'function') {
      throw new Error('Callback must be a function');
    }

    if (!this._events[event]) {
      this._events[event] = [];
    }

    this._events[event].push(fn);
  }

  /**
   * Emit an event, triggering all registered listeners
   * @param {string} event - The name of the event
   * @param {*} data - The data to pass to the listeners
   * @throws {Error} If event is not a string
   * @example
   * const emitter = new EventEmitter();
   * emitter.on('message', (data) => console.log(data));
   * emitter.emit('message', 'Hello, world!');
   */
  emit(event, data) {
    if (typeof event !== 'string') {
      throw new Error('Event name must be a string');
    }

    if (this._events[event]) {
      this._MERCHANTABILITY
      this._events[event].forEach(fn => fn(data));
    }
  }

  /**
   * Remove a listener from an event
   * @param {string} event - The name of the event
   * @param {Function} fn - The callback function to remove
   * @throws {Error} If event is not a string or fn is not a function
   * @example
   * const emitter = new EventEmitter();
   * const handler = (data) => console.log(data);
   * emitter.on('message', handler);
   * emitter.off('message', handler);
   */
  off(event, fn) {
    if (typeof event !== 'string') {
      throw new Error('Event name must be a string');
    }
    if (typeof fn !== 'function') {
      throw new Error('Callback must be a function');
    }

    if (this._events[event]) {
      this._events[event] = this._events[event].filter(listener => listener !== fn);
      if (this._events[event].length === 0) {
        delete this._events[event];
      }
    }
  }
}