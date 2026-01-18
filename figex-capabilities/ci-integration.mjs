/**
 * Generates a GitHub Action workflow file based on provided configuration.
 * @param {Object} config - Configuration object
 * @param {string} config.name - Name of the workflow
 * * @param {string} config.on - Events that trigger the workflow (e.g., 'push', 'pull_request')
 * @param {string[]} config.jobs - List of jobs to run in the workflow
 * @param {string} config.jobs[].name - Name of the job
 * @param {string[]} config.jobs[].steps - Steps to run in the job
 * @param {string} config.jobs[].steps[].run - Command to run in the step
 * @param {string} [config.basePath] - Base path for the workflow file (default: '.github/workflows')
 * @returns {string} Generated workflow content
 * @throws {Error} If config is invalid or missing required fields
 * @example
 * const config = {
 *   name: 'Design System Validation',
 *   on: 'pull_request',
 *   jobs: [
 *     {
 *       name: 'Run Lint',
 *       steps: [
 *         { run: 'npx lint-staged' },
 *         { run: 'npx prettier --check' }
 *       ]
 *     },
 *     {
 *       name: 'Run Tests',
 *       steps: [
 *         { run: 'npm test' }
 *       ]
 *     }
 *   ]
 * };
 * const workflow = generateGitHubAction(config);
 * // workflow contains the generated GitHub Actions YAML
 */
export function generateGitHubAction(config) {
  if (!config || !config.name || !config.on || !config.jobs) {
    throw new Error('Invalid configuration: name, on, and jobs are required');
  }

  const basePath = config.basePath || '.github/workflows';

  let workflowContent = `name: ${config.name}\n\non:\n  ${config.on}\n\njobs:\n`;

  config.jobs.forEach(job => {
    if (!job.name || !job.steps) {
      throw new Error('Invalid job configuration: name and steps are required');
    }

    workflowContent += `  ${job.name}:\n`;
    workflowContent += `    runs-on: ubuntu-latest\n`;
    workflowContent += `    steps:\n`;

    job.steps.forEach(step => {
      if (!step.run) {
        throw new Error('Invalid step configuration: run is required');
      }

      workflowContent += `      - name: ${step.run}\n`;
      workflowContent += `        run: ${step.run}\n`;
    });

    workflowContent += '\n';
  });

  return workflowContent;
}

/**
 * Creates a pre-commit hook configuration based on provided rules.
 * @param {Object} rules - Rules for the pre-commit hook
 * @param {string[]} rules.fix - List of fix commands
 * @param {string[]} rules.lint - List of lint commands
 * @param {string} [rules.message] - Message to display when hook runs
 * @param {string} [rules.basePath] - Base path for the hook file (default: '.git/hooks')
 * @returns {string} Generated hook content
 * @throws {Error} If rules are invalid or missing required fields
 * @example
 * const rules = {
 *   fix: ['npx eslint --fix', 'npx prettier --write'],
 *   lint: ['npx eslint', 'npx prettier --check'],
 *   message: 'Running pre-commit hooks...',
 *   basePath: '.git/hooks'
 * };
 * const hook = createPreCommitHook(rules);
 * // hook contains the generated pre-commit hook script
 */
export function createPreCommitHook(rules) {
  if (!rules || !rules.fix || !rules.lint) {
    throw new Error('Invalid rules: fix and lint are required');
  }

  const basePath = rules.basePath || '.git/hooks';

  let hookContent = `#!/bin/sh\n`;
  hookContent += `# ${rules.message || 'Running pre-commit hooks...'}\n\n`;

  hookContent += `# Run fix commands\n`;
  rules.fix.forEach(command => {
    hookContent += `echo "Fixing: ${command}"\n`;
    hookContent += `if ${command}; then\n`;
    hookContent += `  echo "Fix completed for: ${command}"\n`;
    hookContent += `else\n`;
    hookContent += `  echo "Fix failed for: ${command}"\n`;
    hookContent += `  exit 1\n`;
    hookContent += `fi\n`;
  });

  hookContent += `\n# Run lint commands\n`;
  rules.lint.forEach(command => {
    hookContent += `echo "Linting: ${command}"\n`;
    hookContent += `if ${command}; then\n`;
    hook
Content += `  echo "Lint passed for: ${command}"\n`;
    hookContent += `else\n`;
    hookContent += `  echo "Lint failed for: ${command}"\n`;
    hookContent += `  exit 1\n`;
    hookContent += `fi\n`;
  });

  hookContent += `\nexit 0`;

  return hookContent;
}

/**
 * Validates a pull request based on provided data.
 * @param {Object} prData - Pull request data
 * @param {string} prData.title - Title of the pull request
 * @param {string} prData.body - Body of the pull request
 * @param {string} prData.baseBranch - Base branch of the pull request
 * @param {string} prData.headBranch - Head branch of the pull request
 * @param {string[]} prData.labels - Labels associated with the pull request
 * @param {string} [prData.author] - Author of the pull request
 * @returns {boolean} True if the pull request is valid, false otherwise
 * @throws {Error} If prData is invalid or missing required fields
 * @example
 * const prData = {
 *   title: 'Add new design system component',
 *   body: 'This PR adds a new button component to the design system.',
 *   baseBranch: 'main',
 *   headBranch: 'feature/new-button',
 *   labels: ['design-system', 'needs-review'],
 *   author: 'john.doe'
 * };
 * const isValid = validatePullRequest(prData);
 * // isValid === true
 */
export function validatePullRequest(prData) {
  if (!prData || !prData.title || !prData.body || !prData.baseBranch || !prData.headBranch || !prData.labels) {
    throw new Error('Invalid pull request data: title, body, baseBranch, headBranch, and labels are required');
  }

  if (prData.labels.length === 0) {
    throw new Error('Pull request must have at least one label');
  }

  if (prData.baseBranch === prData.headBranch) {
    throw new Error('Base branch and head branch cannot be the same');
  }

  if (!prData.title.trim()) {
    throw new Error('Pull request title cannot be empty');
  }

  if (!prData.body.trim()) {
    throw new Error('Pull request body cannot be empty');
  }

  if (!prData.labels.every(label => label.trim())) {
    throw new Error('All labels must be non-empty strings');
  }

  return true;
}