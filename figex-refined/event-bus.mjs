/**
 * A class for managing event subscriptions and emissions.
 * It allows registering listeners for events, emitting events with data,
 * and removing listeners.
 */
export default class EventBus {
  /**
   * Creates a new EventBus instance.
   * @constructor
   */
  constructor() {
    this.events = {};
  }

  /**
   * Registers a callback function to be called when an event is emitted.
   * @param {string} event - The name of the event.
   * @param {Function} fn - The callback function to be registered.
   * @example
   * const bus = new EventBus();
   * bus.on('click', (data) => console.log('Clicked:', data));
   */
  on(event, fn) {
    if (!this.events[event]) {
      this.events[event] = [];
    }
    this.events[event].push(fn);
  }

  /**
   * Emits an event, calling all registered callback functions with the provided data.
   * @param {string} event - The name of the event.
   * @param {*} data - The data to pass to the callback functions.
   * @example
   * const bus = new EventBus();
   * bus.on('click', (data) => console.log('Clicked:', data));
   * bus.emit('click', 'button1');
   * // Output: Clicked: button1
   */
  emit(event, data) {
    if (this.events[event]) {
      this.events[event].forEach(fn => fn(data));
    }
  }

  /**
   * Removes a callback function from an event.
   * @param {string} event - The name of the event.
   * @param {Function} fn - The callback function to be removed.
   * @example
   * const bus = new EventBus();
   * const handler = (data) => console.log('Clicked:', data);
   * bus.on('click', handler);
   * bus.off('click', handler);
   */
  off(event, fn) {
    if (this.events[event]) {
      this.events[event] = this.events[event].filter(f => f !== fn);
    }
  }
}