// db.mjs
import { Pool } from 'pg';

const pool = new Pool({
  max: 10,
  idleTimeoutMillis: 30000,
  connectionTimeoutMillis: 2000,
});

export async function connect(url) {
  try {
    await pool.end();
    pool.config = {
      connectionString: url,
    };
    await pool.connect();
    return pool;
  } catch (error) {
    console.error('Failed to connect to database:', error);
    throw error;
  }
}

export async function disconnect() {
  try {
    await pool.end();
    console.log('Database connection closed');
  } catch (error) {
    console.error('Failed to disconnect from database:', error);
    throw error;
  }
}

export async function query(sql, params = []) {
  let retries = 3;
  while (retries > 0) {
    try {
      const client = await pool.connect();
      const res = await client.query(sql, params);
      await client.release();
      return res;
    } catch (error) {
      retries--;
      if (retries === 0) {
        console.error('Query failed after retries:', error);
        throw error;
      }
      console.warn(`Query failed, retrying... ${retries} attempts left`);
      await new Promise(resolve => setTimeout(resolve, 1000));
    }
  }
  throw new Error('Query failed after all retries');
}

export async function transaction(fn) {
  const client = await pool.connect();
  try {
    await client.query('BEGIN');
    const result = await fn(client);
    await client.query('COMMIT');
    return result;
  } catch (error) {
    await client.query('ROLLBACK');
    console.error('Transaction failed:', error);
    throw error;
  } finally {
    await client.release();
  }
}