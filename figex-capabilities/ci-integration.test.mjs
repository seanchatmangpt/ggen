import { describe, it, expect } from 'vitest'
import { generateGitHubAction, createPreCommitHook, validatePullRequest } from './ci-integration.mjs'

describe('ci-integration - JSDoc Examples', () => {
  it('example 1', async () => {
    const config = {
      name: 'Design System Validation',
      on: 'pull_request',
      jobs: [
        {
          name: 'Run Lint',
          steps: [
            { run: 'npx lint-staged' },
            { run: 'npx prettier --check' }
          ]
        },
        {
          name: 'Run Tests',
          steps: [
            { run: 'npm test' }
          ]
        }
      ]
    };
    const workflow = generateGitHubAction(config);
    // workflow contains the generated GitHub Actions YAML
  })

  it('example 2', async () => {
    const rules = {
      fix: ['npx eslint --fix', 'npx prettier --write'],
      lint: ['npx eslint', 'npx prettier --check'],
      message: 'Running pre-commit hooks...',
      basePath: '.git/hooks'
    };
    const hook = createPreCommitHook(rules);
    // hook contains the generated pre-commit hook script
  })

  it('example 3', async () => {
    const prData = {
      title: 'Add new design system component',
      body: 'This PR adds a new button component to the design system.',
      baseBranch: 'main',
      headBranch: 'feature/new-button',
      labels: ['design-system', 'needs-review'],
      author: 'john.doe'
    };
    const isValid = validatePullRequest(prData);
    // isValid === true
  })

})
