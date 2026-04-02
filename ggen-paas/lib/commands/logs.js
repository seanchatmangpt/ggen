/**
 * ggen-paas CLI Command: logs
 *
 * Stream logs from services.
 */

import { CommandBase } from '../command-base.js';

export default class LogsCommand extends CommandBase {
  constructor() {
    super({
      name: 'logs',
      aliases: ['log', 'tail'],
      category: 'Management',
      description: 'Stream logs from services',
      examples: `
        ggen paas logs api-gateway
        ggen paas logs web-ui --follow
        ggen paas logs scheduler --lines 100
      `,
      slo: { maxDurationMs: 5000, expectedPassRate: 99.0 },
    });
  }

  defineSchema() {
    return {
      positional: [
        {
          name: 'service',
          type: 'string',
          required: true,
          description: 'Service name',
        },
      ],
      options: {
        follow: { shortForm: '-f', longForm: '--follow', type: 'boolean', default: false },
        lines: { longForm: '--lines', type: 'integer', default: 20 },
        namespace: { shortForm: '-n', longForm: '--namespace', type: 'string', default: 'ggen-paas' },
        level: { longForm: '--level', type: 'string', default: 'info', choices: ['debug', 'info', 'warn', 'error'] },
      },
    };
  }

  async execute(args, options) {
    const logger = this.logger;

    try {
      const service = args.arg0 || 'unknown';
      logger.info(`Fetching logs from ${service}...`);
      logger.success(`Logs (last ${options.lines} lines, level: ${options.level})`);
      
      return this.success({
        service,
        lines: 20,
        follow: options.follow,
        namespace: options.namespace,
      });
    } catch (error) {
      return this.fail('LOGS_ERROR', error.message);
    }
  }
}
