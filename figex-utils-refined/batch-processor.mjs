/**
 * Processes multiple files in parallel with progress tracking.
 * @param {string[]} filePaths - Array of file paths to process.
 * @param {Function} handler - Function to handle each file. Should accept a file path and return a Promise.
 * @returns {Promise<void>} A promise that resolves when all files are processed.
 * @throws {Error} If filePaths is not an array or if handler is not a function.
 * @example
 * const filePaths = ['file1.txt', 'file2.txt', 'file3.txt'];
 * const handler = (filePath) => {
 *   return new Promise((resolve) => {
 *     console.log(`Processing ${filePath}`);
 *     resolve();
 *   });
 * };
 * await processFiles(filePaths, handler);
 */
export async function processFiles(filePaths, handler) {
  if (!Array.isArray(filePaths)) {
    throw new Error('filePaths must be an array');
  }

  if (typeof handler !== 'function') {
    throw new Error('handler must be a function');
  }

  const totalFiles = filePaths.length;
  let processedFiles = 0;

  const promises = filePaths.map(filePath => {
    return new Promise((resolve, reject) => {
      handler(filePath)
        .then(() => {
          processedFiles++;
          console.log(`Processed ${processedFiles} of ${total, totalFiles} files.`);
          resolve();
        })
        .catch(reject);
    });
  });

  await Promise.all(promises);
  console.log(`All ${totalFiles} files processed.`);
}

/**
 * Runs an array of tasks in parallel with a specified maximum concurrency.
 * @param {Array<Function>} tasks - Array of functions to run.
 * @param {number} maxConcurrency - Maximum number of tasks to run in parallel.
 * @returns {Promise<void>} A promise that resolves when all tasks are completed.
 * @throws {Error} If tasks is not an array or if maxConcurrency is not a positive integer.
 * @example
 * const task1 = () => console.log('Task 1');
 * const task2 = () => console.log('Task 2');
 * const task3 = () => console.log('Task 3');
 * const tasks = [task1, task2, task3];
 * await runParallel(tasks, 2);
 */
export async function runParallel(tasks, maxConcurrency) {
  if (!Array.isArray(tasks)) {
    throw new Error('tasks must be an array');
  }

  if (typeof maxConcurrency !== 'number' || maxConcurrency <= 0) {
    throw new Error('maxConcurrency must be a positive integer');
  }

  const results = [];
  const promises = [];

  for (let i = 0; i < tasks.length; i++) {
    const task = tasks[i];
    const promise = new Promise((resolve, reject) => {
      try {
        const result = task();
        results.push(result);
        resolve(result);
      } catch (error) {
        reject(error);
      }
    });

    promises.push(promise);

    // If we've reached max concurrency, wait for one to complete
    if (promises.length >= maxConcurrency) {
      await Promise.race(promises);
    }
  }

  // Wait for any remaining promises
  await Promise.all(promises);
}