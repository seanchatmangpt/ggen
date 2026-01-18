function promisifyAll(obj) {
  return Object.keys(obj).reduce((acc, key) => {
    if (typeof obj[key] === 'function') {
      acc[key] = function(...args) {
        return new Promise((resolve, reject) => {
          obj[key].apply(this, [...args, (err, result) => {
            if (err) reject(err);
            else resolve(result);
          }]);
        });
      };
    } else {
      acc[key] = obj[key];
    }
    return acc;
  }, {});
}