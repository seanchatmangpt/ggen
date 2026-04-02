/**
 * Base class for all CLI commands
 */

import { Logger } from './utils/logger.js';

// Simple color utility (no external deps needed)
const chalk = {
  bold: (str) => `\x1b[1m${str}\x1b[0m`,
  dim: (str) => `\x1b[2m${str}\x1b[0m`,
  red: (str) => `\x1b[31m${str}\x1b[0m`,
  green: (str) => `\x1b[32m${str}\x1b[0m`,
};

export class CommandBase {
  constructor(config) {
    this.name = config.name;
    this.aliases = config.aliases || [];
    this.category = config.category;
    this.description = config.description;
    this.examples = config.examples;
    this.slo = config.slo;
    this.logger = new Logger({ name: this.name });
    this.startTime = Date.now();
  }

  /**
   * Define command schema
   */
  defineSchema() {
    return { positional: [], options: {} };
  }

  /**
   * Execute command
   */
  async execute(args, options) {
    throw new Error('execute() must be implemented by subclass');
  }

  /**
   * Return success result
   */
  success(data, format = 'text') {
    const duration = this.getDuration();
    const result = {
      exitCode: 0,
      success: true,
      duration,
      data,
    };

    if (format === 'json') {
      console.log(JSON.stringify(result, null, 2));
    } else if (format === 'yaml') {
      console.log(this._toYAML(result));
    } else {
      console.log(chalk.green(`✓ Success (${duration}ms)`));
      if (typeof data === 'string') {
        console.log(data);
      }
    }

    return result;
  }

  /**
   * Return failure result
   */
  fail(code, message, details = {}) {
    const duration = this.getDuration();
    const result = {
      exitCode: 1,
      success: false,
      code,
      message,
      details,
      duration,
    };

    console.error(chalk.red(`✗ ${code}: ${message}`));
    if (details && Object.keys(details).length > 0) {
      console.error(chalk.dim('Details:'));
      console.error(chalk.dim(JSON.stringify(details, null, 2)));
    }

    return result;
  }

  /**
   * Get elapsed duration
   */
  getDuration() {
    return Date.now() - this.startTime;
  }

  /**
   * Convert to YAML string
   * @private
   */
  _toYAML(obj, indent = 0) {
    const spaces = ' '.repeat(indent);
    const lines = [];

    for (const [key, value] of Object.entries(obj)) {
      if (value === null || value === undefined) {
        lines.push(`${spaces}${key}:`);
      } else if (typeof value === 'object' && !Array.isArray(value)) {
        lines.push(`${spaces}${key}:`);
        lines.push(this._toYAML(value, indent + 2));
      } else if (Array.isArray(value)) {
        lines.push(`${spaces}${key}:`);
        for (const item of value) {
          lines.push(`${spaces}  - ${item}`);
        }
      } else if (typeof value === 'string' && value.includes('\n')) {
        lines.push(`${spaces}${key}: |-`);
        for (const line of value.split('\n')) {
          lines.push(`${spaces}  ${line}`);
        }
      } else {
        lines.push(`${spaces}${key}: ${value}`);
      }
    }

    return lines.join('\n');
  }
}
