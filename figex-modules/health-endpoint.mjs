// health.mjs
import { promisify } from 'util';
import { exec } from 'child_process';

const healthCheckHandler = async (req, res) => {
  const status = 'passing';
  const timestamp = new Date().toISOString();

  try {
    const memory = await getMemoryUsage();
    const uptime = await getUptime();
    const healthy = checkSystemHealth(memory, uptime);

    res.status(200).json({
      status,
      timestamp,
      checks: { memory, uptime, healthy }
    });
  } catch (error) {
    res.status(500).json({
      status: 'failed',
      timestamp,
      checks: { memory: null, uptime: null, healthy: false },
      error: error.message
    });
  }
};

async function getMemoryUsage() {
  const { stdout } = await promisify(exec)("free -m | awk '/Mem/{print $2}'");
  return parseInt(stdout.trim());
}

async function getUptime() {
  const { stdout } = await promisify(exec)("uptime | awk '{print $1}'");
  return parseInt(stdout.trim());
}

function checkSystemHealth(memory, uptime) {
  const memoryThreshold = 800; // in MB
  const uptimeThreshold = 72; // in hours

  const healthy = memory < memoryThreshold && uptime < uptimeThreshold;
  return {
    memory,
    uptime,
    healthy
  };
}

export { healthCheckHandler };