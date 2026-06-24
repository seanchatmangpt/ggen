// Chicago TDD: real async state via AtomicU32, no mocks.
//
// NOTE on delays: retry_with_backoff starts at 1s and doubles between attempts.
// To keep tests fast we use max_attempts=1 (zero sleeps) and max_attempts=2
// (one 1s sleep only).  A test that exercises 3+ attempts would sleep 3s+;
// we avoid that here in favour of meaningful but quick assertions.
use std::sync::{
    atomic::{AtomicU32, Ordering},
    Arc,
};

use ggen_daemon::{
    error::{DaemonError, Result},
    retry::retry_with_backoff,
};

// ---------------------------------------------------------------------------
// 1. Closure that always fails → Err after max_attempts=1, no panic
// ---------------------------------------------------------------------------
#[tokio::test]
async fn always_failing_returns_err_without_panic() {
    let attempts = Arc::new(AtomicU32::new(0));
    let attempts_clone = Arc::clone(&attempts);

    // max_attempts=1 means we call the closure once, it fails, and we
    // return that error immediately — no sleep occurs at all.
    let result: Result<()> = retry_with_backoff("always-fail", 1, || {
        let c = Arc::clone(&attempts_clone);
        async move {
            c.fetch_add(1, Ordering::SeqCst);
            Err(DaemonError::Scheduler("deliberate failure".to_string()))
        }
    })
    .await;

    assert!(result.is_err(), "expected Err but got Ok");
    // The closure must have been called exactly once.
    assert_eq!(attempts.load(Ordering::SeqCst), 1, "expected exactly 1 call");
    // Confirm the error message is propagated, not swallowed.
    let msg = result.unwrap_err().to_string();
    assert!(
        msg.contains("deliberate failure"),
        "error message not propagated: {msg}"
    );
}

// ---------------------------------------------------------------------------
// 2. Closure fails on the first call, succeeds on the second → Ok on retry
//    (uses max_attempts=2, which means one 1s sleep between attempts)
// ---------------------------------------------------------------------------
#[tokio::test]
async fn fails_once_then_succeeds_returns_ok() {
    let calls = Arc::new(AtomicU32::new(0));
    let calls_clone = Arc::clone(&calls);

    // max_attempts=2: attempt 1 fails (sleeps 1s), attempt 2 succeeds.
    // This test takes ~1s by design — real async behaviour, not mocked time.
    let result: Result<u32> = retry_with_backoff("fail-once", 2, || {
        let c = Arc::clone(&calls_clone);
        async move {
            let n = c.fetch_add(1, Ordering::SeqCst);
            if n == 0 {
                Err(DaemonError::Scheduler("first attempt".to_string()))
            } else {
                Ok(42u32)
            }
        }
    })
    .await;

    assert!(result.is_ok(), "expected Ok but got: {:?}", result.err());
    assert_eq!(result.unwrap(), 42u32);
    assert_eq!(calls.load(Ordering::SeqCst), 2, "expected exactly 2 calls");
}

// ---------------------------------------------------------------------------
// 3. Closure always succeeds → Ok on the very first attempt (no sleep)
// ---------------------------------------------------------------------------
#[tokio::test]
async fn always_succeeds_returns_ok_on_first_attempt() {
    let calls = Arc::new(AtomicU32::new(0));
    let calls_clone = Arc::clone(&calls);

    let result: Result<&str> = retry_with_backoff("always-ok", 5, || {
        let c = Arc::clone(&calls_clone);
        async move {
            c.fetch_add(1, Ordering::SeqCst);
            Ok("success")
        }
    })
    .await;

    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "success");
    // Should have returned after the very first call, never sleeping.
    assert_eq!(calls.load(Ordering::SeqCst), 1, "expected exactly 1 call");
}

// ---------------------------------------------------------------------------
// 4. The `label` parameter is accepted and passed through without panicking
//    (uses a label with special characters to surface any formatting issues)
// ---------------------------------------------------------------------------
#[tokio::test]
async fn label_parameter_passes_through_without_panic() {
    let label = "ggen/daemon: repo-sync [attempt]";

    let result: Result<bool> =
        retry_with_backoff(label, 1, || async { Ok(true) }).await;

    assert!(
        result.is_ok(),
        "label '{label}' caused unexpected error: {:?}",
        result.err()
    );
    assert!(result.unwrap());
}

// ---------------------------------------------------------------------------
// 5. All attempts exhausted → call count equals max_attempts exactly
// ---------------------------------------------------------------------------
#[tokio::test]
async fn exhausted_attempts_call_count_equals_max_attempts() {
    // max_attempts=1: no sleep, confirms the loop boundary is exact.
    let calls = Arc::new(AtomicU32::new(0));
    let calls_clone = Arc::clone(&calls);

    let result: Result<()> = retry_with_backoff("exhausted", 1, || {
        let c = Arc::clone(&calls_clone);
        async move {
            c.fetch_add(1, Ordering::SeqCst);
            Err(DaemonError::Scheduler("still failing".to_string()))
        }
    })
    .await;

    assert!(result.is_err());
    assert_eq!(
        calls.load(Ordering::SeqCst),
        1,
        "call count must equal max_attempts"
    );
}
