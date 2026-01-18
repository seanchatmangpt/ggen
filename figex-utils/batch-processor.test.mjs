import { describe, it, expect } from 'vitest'
import { processFiles, runParallel } from './batch-processor.mjs'

describe('batch-processor - JSDoc Examples', () => {
  it('example 1', async () => {
    const filePaths = ['file1.txt', 'file2.txt', 'file3.txt'];
    const handler = (filePath) => {
      return new Promise((resolve) => {
        console.log(`Processing ${filePath}`);
        resolve();
      });
    };
    await processFiles(filePaths, handler);
  })

  it('example 2', async () => {
    const task1 = () => console.log('Task 1');
    const task2 = () => console.log('Task 2');
    const task3 = () => console.log('Task 3');
    const tasks = [task1, task2, task3];
    await runParallel(tasks, 2);
  })

})
