function hash(str) {
  const crypto = require('crypto');
  return crypto.createHash('sha256').update(str).digest('hex');
}