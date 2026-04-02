/**
 * ggen-paas CLI Command: describe
 *
 * Describe a resource in detail.
 */

import { CommandBase } from '../command-base.js';

export default class DescribeCommand extends CommandBase {
  constructor() {
    super({
      name: 'describe',
      aliases: ['desc', 'show'],
      category: 'Management',
      description: 'Describe a resource in detail',
      examples: `
        ggen paas describe api-gateway
        ggen paas describe postgres --detail full
        ggen paas describe redis --output yaml
      `,
      slo: { maxDurationMs: 5000, expectedPassRate: 99.0 },
    });
  }

  defineSchema() {
    return {
      positional: [
        {
          name: 'resource',
          type: 'string',
          required: true,
          description: 'Resource name',
        },
      ],
      options: {
        output: { shortForm: '-o', longForm: '--output', type: 'string', default: 'text', choices: ['text', 'json', 'yaml'] },
        detail: { longForm: '--detail', type: 'string', default: 'summary', choices: ['summary', 'full', 'extended'] },
      },
    };
  }

  async execute(args, options) {
    const logger = this.logger;

    try {
      const resource = args.arg0 || 'unknown';
      logger.info(`Describing ${resource}...`);
      
      const description = {
        name: resource,
        type: 'service',
        status: 'healthy',
        detail: options.detail,
      };

      logger.success(`Resource description retrieved`);
      return this.success(description, options.output);
    } catch (error) {
      return this.fail('DESCRIBE_ERROR', error.message);
    }
  }
}
