use crate::models::{DetectedCapability, FileEntry, Symbol};
use rusqlite::{Connection, Result};
use std::path::Path;
pub fn get_db_conn(path: &Path) -> Result<Connection> {
    let conn = Connection::open(path)?;
    conn.execute("CREATE TABLE IF NOT EXISTS files (path TEXT PRIMARY KEY, hash TEXT NOT NULL, size_bytes INTEGER NOT NULL, language TEXT NOT NULL, git_root TEXT, is_test BOOLEAN NOT NULL, is_binary BOOLEAN NOT NULL)", [])?;
    conn.execute("CREATE TABLE IF NOT EXISTS symbols (id INTEGER PRIMARY KEY AUTOINCREMENT, file_path TEXT NOT NULL, name TEXT NOT NULL, kind TEXT NOT NULL, line INTEGER NOT NULL, FOREIGN KEY(file_path) REFERENCES files(path))", [])?;
    conn.execute("CREATE TABLE IF NOT EXISTS capabilities (id INTEGER PRIMARY KEY AUTOINCREMENT, file_path TEXT NOT NULL, capability TEXT NOT NULL, matched_term TEXT NOT NULL, confidence REAL NOT NULL, evidence_type TEXT NOT NULL, classification TEXT NOT NULL, line_number INTEGER, FOREIGN KEY(file_path) REFERENCES files(path))", [])?;
    Ok(conn)
}
pub fn insert_catalog(
    conn: &mut Connection, files: &[FileEntry], symbols: &[Symbol], caps: &[DetectedCapability],
) -> Result<()> {
    let tx = conn.transaction()?;
    {
        let mut stmt = tx.prepare("INSERT OR REPLACE INTO files (path, hash, size_bytes, language, git_root, is_test, is_binary) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)")?;
        for f in files {
            stmt.execute((
                &f.path,
                &f.hash,
                f.size_bytes as i64,
                format!("{:?}", f.language),
                &f.git_root,
                f.is_test,
                f.is_binary,
            ))?;
        }
    }
    {
        let mut stmt = tx
            .prepare("INSERT INTO symbols (file_path, name, kind, line) VALUES (?1, ?2, ?3, ?4)")?;
        for s in symbols {
            stmt.execute((&s.file_path, &s.name, &s.kind, s.line as i64))?;
        }
    }
    {
        let mut stmt = tx.prepare("INSERT INTO capabilities (file_path, capability, matched_term, confidence, evidence_type, classification, line_number) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)")?;
        for c in caps {
            stmt.execute((
                &c.file_path,
                &c.capability,
                &c.matched_term,
                c.confidence,
                &c.evidence_type,
                &c.classification,
                c.line_number,
            ))?;
        }
    }
    tx.commit()?;
    Ok(())
}
