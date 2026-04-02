export default class EventBus {
  constructor() {
    this.events = {};
  }

  on(event, fn) {
    if (!this.events[event]) {
      this.events[event] = [];
    }
    this.events[event].push(fn);
  }

  emit(event, data) {
    if (this.events[event]) {
      this.events[event].forEach(fn => fn(data));
    }
  }

  off(event, fn) {
    if (this.events[event]) {
      this.events[event] = this.events[event].filter(f => f !== fn);
    }
  }
}