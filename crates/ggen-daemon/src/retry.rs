use std::time::Duration;
use tracing::warn;
use crate::error::Result;

/// Retry an async operation with exponential backoff.
///
/// Delays: 1s, 2s, 4s, 8s, ... up to max_attempts.
pub async fn retry_with_backoff<F, Fut, T>(
    label: &str,
    max_attempts: u32,
    mut f: F,
) -> Result<T>
where
    F: FnMut() -> Fut,
    Fut: std::future::Future<Output = Result<T>>,
{
    let mut delay = Duration::from_secs(1);
    for attempt in 1..=max_attempts {
        match f().await {
            Ok(v) => return Ok(v),
            Err(e) if attempt < max_attempts => {
                warn!("{}: attempt {}/{} failed: {} — retrying in {:?}", label, attempt, max_attempts, e, delay);
                tokio::time::sleep(delay).await;
                delay = (delay * 2).min(Duration::from_secs(60));
            }
            Err(e) => return Err(e),
        }
    }
    unreachable!()
}
