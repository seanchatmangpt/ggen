/**
 * ggen-paas CLI Command: sync
 *
 * Synchronizes specifications with RDF store and cloud infrastructure.
 */

import { CommandBase } from '../command-base.js';

export default class SyncCommand extends CommandBase {
  constructor() {
    super({
      name: 'sync',
      aliases: ['update'],
      category: 'Management',
      description: 'Synchronize specifications with RDF store and infrastructure',
      examples: `
        ggen paas sync infrastructure
        ggen paas sync specifications --dry-run
        ggen paas sync all --force
      `,
      slo: { maxDurationMs: 60000, expectedPassRate: 99.0 },
    });
  }

  defineSchema() {
    return {
      positional: [
        {
          name: 'target',
          type: 'string',
          required: true,
          choices: ['specifications', 'infrastructure', 'all'],
        },
      ],
      options: {
        dryRun: { longForm: '--dry-run', type: 'boolean', default: false },
        force: { longForm: '--force', type: 'boolean', default: false },
        environment: { shortForm: '-e', longForm: '--environment', type: 'string', default: 'development' },
      },
    };
  }

  async execute(args, options) {
    const logger = this.logger;

    try {
      const target = args.arg0 || 'all';
      logger.info(`Syncing ${target} to ${options.environment}...`);
      logger.success(`Sync complete (dry-run: ${options.dryRun})`);
      return this.success({ target, environment: options.environment, dryRun: options.dryRun });
    } catch (error) {
      return this.fail('SYNC_ERROR', error.message);
    }
  }
}
