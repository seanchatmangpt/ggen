// rate-limit.mjs
const Redis = require('ioredis');
const { promisify } = require('util');

const redis = new Redis();

const rateLimitMiddleware = (options) => {
  const { maxRequests, windowMs, keyGenerator } = options;

  const get = promisify(redis.get).bind(redis);
  const set = promisify(redis.set).bind(redis);
  const expire = promisify(redis.expire).bind(redis);

  return async (req, res, next) => {
    const key = await keyGenerator(req);
    const now = Date.now();

    try {
      const remaining = await get(key);
      if (remaining === null) {
        await set(key, maxRequests - 1, 'EX', Math.floor(windowMs / 1000));
        await expire(key, Math.floor(windowMs / 1000));
        return next();
      }

      const remainingInt = parseInt(remaining, 10);
      if (remainingInt > 0) {
        await set(key, remainingInt - 1, 'EX', Math.floor(windowMs / 1000));
        return next();
      }

      const lastReset = await get(`${key}:reset`);
      if (lastReset === null) {
        await set(key, maxRequests - 1, 'EX', Math.floor(windowMs / 1000));
        await expire(key, Math.floor(windowMs / 1000));
        return next();
      }

      const lastResetInt = parseInt(lastReset, 10);
      if (now >= lastResetInt) {
        await set(key, maxRequests - 1, 'EX', Math.floor(windowMs / 1000));
        await expire(key, Math.floor(windowMs / 1000));
        return next();
      }

      res.status(429).send('Too many requests, please try again later.');
    } catch (err) {
      res.status(500).send('Internal server error.');
    }
  };
};

module.exports = { rateLimitMiddleware };