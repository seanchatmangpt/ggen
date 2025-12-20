// logger.mjs
export const info = (msg, meta) => {
  log('INFO', msg, meta);
};

export const warn = (msg, meta) => {
  log('WARN', msg, meta);
};

export const error = (msg, meta) => {
  log('ERROR', msg, meta);
};

export const debug = (msg, meta) => {
  log('DEBUG', msg, meta);
};

function log(level, msg, meta) {
  const timestamp = new Date().toISOString();
  const logEntry = {
    timestamp,
    level,
    message: msg,
    meta: meta || {}
  };

  console.log(JSON.stringify(logEntry, null, 2));
}