function retry(fn, maxRetries, delay) {
  return async function(...args) {
    let attempt = 0;
    while (attempt < maxRetries) {
      try {
        return await fn(...args);
      } catch (error) {
        attempt++;
        if (attempt >= maxRetries) throw error;
        await new Promise(resolve => setTimeout(resolve, delay * Math.pow(2, attempt - 1)));
      }
    }
  };
}