/**
 * ggen-paas CLI Command: explain
 *
 * Explain RDF concepts and relationships.
 */

import { CommandBase } from '../command-base.js';

export default class ExplainCommand extends CommandBase {
  constructor() {
    super({
      name: 'explain',
      aliases: ['help-rdf', 'query'],
      category: 'Help',
      description: 'Explain RDF concepts and relationships',
      examples: `
        ggen paas explain Container
        ggen paas explain DataStore --detail full
        ggen paas explain SLA --output yaml
      `,
      slo: { maxDurationMs: 2000, expectedPassRate: 99.5 },
    });
  }

  defineSchema() {
    return {
      positional: [
        {
          name: 'concept',
          type: 'string',
          required: true,
          description: 'RDF concept to explain',
        },
      ],
      options: {
        detail: { longForm: '--detail', type: 'string', default: 'summary', choices: ['summary', 'full', 'extended'] },
        output: { shortForm: '-o', longForm: '--output', type: 'string', default: 'text', choices: ['text', 'json', 'yaml'] },
      },
    };
  }

  async execute(args, options) {
    const logger = this.logger;

    try {
      const concept = args.arg0 || 'unknown';
      logger.info(`Explaining RDF concept: ${concept}...`);
      
      const explanation = {
        concept,
        namespace: 'http://ggen.org/paas#',
        description: `RDF concept: ${concept}`,
        detail: options.detail,
      };

      logger.success(`Concept explanation retrieved`);
      return this.success(explanation, options.output);
    } catch (error) {
      return this.fail('EXPLAIN_ERROR', error.message);
    }
  }
}
