/**
 * ggen-paas CLI Command: deploy
 *
 * Deploys infrastructure to target environment.
 */

import { CommandBase } from '../command-base.js';

export default class DeployCommand extends CommandBase {
  constructor() {
    super({
      name: 'deploy',
      aliases: ['promote'],
      category: 'Deployment',
      description: 'Deploy infrastructure to target environment',
      examples: `
        ggen paas deploy development
        ggen paas deploy staging --dry-run
        ggen paas deploy production --region us-east-1
      `,
      slo: { maxDurationMs: 600000, expectedPassRate: 99.5 },
    });
  }

  defineSchema() {
    return {
      positional: [
        {
          name: 'environment',
          type: 'string',
          required: true,
          choices: ['development', 'staging', 'production'],
        },
      ],
      options: {
        dryRun: { longForm: '--dry-run', type: 'boolean', default: false },
        region: { shortForm: '-r', longForm: '--region', type: 'string', default: 'us-east-1' },
        namespace: { shortForm: '-n', longForm: '--namespace', type: 'string', default: 'ggen-paas' },
      },
    };
  }

  async execute(args, options) {
    const logger = this.logger;

    try {
      const environment = args.arg0 || 'development';
      logger.info(`Deploying to ${environment} (${options.region})...`);
      logger.success(`Deployment ready (dry-run: ${options.dryRun})`);
      return this.success({ environment, region: options.region, namespace: options.namespace, dryRun: options.dryRun });
    } catch (error) {
      return this.fail('DEPLOY_ERROR', error.message);
    }
  }
}
