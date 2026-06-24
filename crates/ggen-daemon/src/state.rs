use chrono::Utc;
use rusqlite::{params, Connection};
use serde::{Deserialize, Serialize};
use std::{path::PathBuf, sync::{Arc, Mutex}};
use tracing::warn;
use crate::error::{DaemonError, Result};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobRun {
    pub id: i64,
    pub dispatch_iri: String,
    pub spec_manifest: String,
    /// RFC-3339 UTC timestamp when the run was recorded.
    pub started_at: String,
    /// RFC-3339 UTC timestamp when the run finished, if complete.
    pub finished_at: Option<String>,
    pub exit_code: Option<i64>,
    pub stdout_tail: Option<String>,
    pub stderr_tail: Option<String>,
}

/// Checkpoint written after each campaign day completes.
#[derive(Debug, Clone)]
pub struct CampaignCheckpoint {
    pub campaign_id: String,
    pub day: u8,
    pub bundles_dispatched: usize,
    pub repos_succeeded: usize,
    pub repos_failed: usize,
}

/// SQLite-backed job run store.
///
/// Replaces the previous Vec<JobRun> + JSON approach, giving O(1) per-write
/// instead of O(n) full-file rewrites and enabling crash-safe campaign resume.
///
/// The inner `Connection` is protected by a `Mutex`, allowing `DaemonState`
/// to be cheaply cloned (clones share the same connection via Arc).
pub struct DaemonState {
    pub cron_ttl_path: String,
    conn: Arc<Mutex<Connection>>,
}

impl Clone for DaemonState {
    fn clone(&self) -> Self {
        Self { cron_ttl_path: self.cron_ttl_path.clone(), conn: Arc::clone(&self.conn) }
    }
}

impl DaemonState {
    /// Open (or create) the state database.
    ///
    /// Pass `db_path = None` for an in-memory database (tests).
    /// Pass `db_path = Some(path)` for a file-backed database (production).
    /// Corrupt / invalid files are silently replaced with a fresh database.
    pub async fn new(db_path: Option<PathBuf>, cron_ttl_path: String) -> Result<Self> {
        let conn = open_with_recovery(db_path)?;
        Ok(Self { cron_ttl_path, conn: Arc::new(Mutex::new(conn)) })
    }

    pub async fn record_start(&self, dispatch_iri: &str, spec_manifest: &str) -> Result<i64> {
        let now = rfc3339_now();
        let conn = self.conn.lock().unwrap();
        conn.execute(
            "INSERT INTO job_runs (dispatch_iri, manifest, started_at) VALUES (?1, ?2, ?3)",
            params![dispatch_iri, spec_manifest, now],
        ).map_err(|e| DaemonError::Database(e.to_string()))?;
        Ok(conn.last_insert_rowid())
    }

    pub async fn record_finish(
        &self,
        run_id: i64,
        exit_code: i32,
        stdout_tail: &str,
        stderr_tail: &str,
    ) -> Result<()> {
        let now = rfc3339_now();
        let conn = self.conn.lock().unwrap();
        conn.execute(
            "UPDATE job_runs \
             SET finished_at = ?1, exit_code = ?2, stdout_tail = ?3, stderr_tail = ?4 \
             WHERE id = ?5",
            params![now, exit_code as i64, stdout_tail, stderr_tail, run_id],
        ).map_err(|e| DaemonError::Database(e.to_string()))?;
        Ok(())
    }

    pub async fn recent_runs(&self, limit: i64) -> Result<Vec<JobRun>> {
        let conn = self.conn.lock().unwrap();
        let mut stmt = conn.prepare(
            "SELECT id, dispatch_iri, manifest, started_at, finished_at, exit_code, \
             stdout_tail, stderr_tail \
             FROM job_runs ORDER BY id DESC LIMIT ?1",
        ).map_err(|e| DaemonError::Database(e.to_string()))?;

        let runs = stmt.query_map(params![limit], row_to_job_run)
            .map_err(|e| DaemonError::Database(e.to_string()))?
            .flatten()
            .collect();
        Ok(runs)
    }

    pub async fn all_runs(&self) -> Vec<JobRun> {
        let conn = self.conn.lock().unwrap();
        conn.prepare(
            "SELECT id, dispatch_iri, manifest, started_at, finished_at, exit_code, \
             stdout_tail, stderr_tail \
             FROM job_runs ORDER BY id",
        )
        .into_iter()
        .flat_map(|mut stmt| {
            stmt.query_map([], row_to_job_run)
                .into_iter()
                .flatten()
                .flatten()
                .collect::<Vec<_>>()
        })
        .collect()
    }

    /// Upsert a campaign checkpoint (idempotent: safe to call twice for the same day).
    pub async fn record_checkpoint(&self, c: &CampaignCheckpoint) -> Result<()> {
        let now = rfc3339_now();
        let conn = self.conn.lock().unwrap();

        // Ensure the parent campaign row exists before inserting the checkpoint.
        conn.execute(
            "INSERT OR IGNORE INTO campaigns (campaign_id, started_at, completed) VALUES (?1, ?2, 0)",
            params![c.campaign_id, now],
        ).map_err(|e| DaemonError::Database(e.to_string()))?;

        conn.execute(
            "INSERT OR REPLACE INTO campaign_checkpoints \
             (campaign_id, day, bundles_dispatched, repos_succeeded, repos_failed, completed_at) \
             VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
            params![
                c.campaign_id,
                c.day as i64,
                c.bundles_dispatched as i64,
                c.repos_succeeded as i64,
                c.repos_failed as i64,
                now,
            ],
        ).map_err(|e| DaemonError::Database(e.to_string()))?;

        Ok(())
    }

    /// Return the set of days that have a completed checkpoint for a campaign.
    pub async fn completed_days(&self, campaign_id: &str) -> Vec<u8> {
        let conn = self.conn.lock().unwrap();
        let Ok(mut stmt) = conn.prepare(
            "SELECT day FROM campaign_checkpoints WHERE campaign_id = ?1",
        ) else { return vec![] };

        stmt.query_map(params![campaign_id], |r| r.get::<_, i64>(0))
            .into_iter()
            .flatten()
            .flatten()
            .map(|d| d as u8)
            .collect()
    }

    pub async fn mark_campaign_complete(&self, campaign_id: &str) {
        let conn = self.conn.lock().unwrap();
        let _ = conn.execute(
            "UPDATE campaigns SET completed = 1 WHERE campaign_id = ?1",
            params![campaign_id],
        );
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Open and init the database; on schema failure against a corrupt file,
/// delete it and start fresh.  rusqlite opens corrupt files lazily and only
/// errors on the first SQL execution, so recovery must wrap `init_schema`.
fn open_with_recovery(db_path: Option<PathBuf>) -> Result<Connection> {
    let conn = raw_open(db_path.as_deref())?;
    match init_schema(&conn) {
        Ok(()) => Ok(conn),
        Err(e) => match db_path.as_ref() {
            Some(p) => {
                warn!("state DB schema failed ({}), resetting", e);
                drop(conn);
                let _ = std::fs::remove_file(p);
                let conn2 = raw_open(Some(p))?;
                init_schema(&conn2)?;
                Ok(conn2)
            }
            None => Err(e),
        },
    }
}

fn raw_open(db_path: Option<&std::path::Path>) -> Result<Connection> {
    match db_path {
        Some(p) => Connection::open(p).map_err(|e| DaemonError::Database(e.to_string())),
        None => Connection::open_in_memory().map_err(|e| DaemonError::Database(e.to_string())),
    }
}

fn init_schema(conn: &Connection) -> Result<()> {
    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS job_runs (
            id           INTEGER PRIMARY KEY AUTOINCREMENT,
            dispatch_iri TEXT NOT NULL,
            manifest     TEXT NOT NULL,
            started_at   TEXT NOT NULL,
            finished_at  TEXT,
            exit_code    INTEGER,
            stdout_tail  TEXT,
            stderr_tail  TEXT
        );
        CREATE TABLE IF NOT EXISTS campaigns (
            campaign_id TEXT PRIMARY KEY,
            started_at  TEXT NOT NULL,
            completed   INTEGER NOT NULL DEFAULT 0
        );
        CREATE TABLE IF NOT EXISTS campaign_checkpoints (
            campaign_id        TEXT NOT NULL,
            day                INTEGER NOT NULL,
            bundles_dispatched INTEGER NOT NULL,
            repos_succeeded    INTEGER NOT NULL,
            repos_failed       INTEGER NOT NULL,
            completed_at       TEXT NOT NULL,
            PRIMARY KEY (campaign_id, day)
        );",
    ).map_err(|e| DaemonError::Database(e.to_string()))
}

fn row_to_job_run(row: &rusqlite::Row<'_>) -> rusqlite::Result<JobRun> {
    Ok(JobRun {
        id: row.get("id")?,
        dispatch_iri: row.get("dispatch_iri")?,
        spec_manifest: row.get("manifest")?,
        started_at: row.get("started_at")?,
        finished_at: row.get("finished_at")?,
        exit_code: row.get("exit_code")?,
        stdout_tail: row.get("stdout_tail")?,
        stderr_tail: row.get("stderr_tail")?,
    })
}

fn rfc3339_now() -> String {
    Utc::now().to_rfc3339()
}
