//! OTEL Correlation ID propagation for the A2A message pipeline.
//!
//! Provides helpers that link A2A protocol fields (`correlation_id`,
//! `causation_chain`) to `tracing` spans so that distributed traces
//! crossing agent boundaries are automatically correlated.

use a2a_generated::converged::message::ConvergedMessage;
use tracing::Span;

/// Create a tracing span linked to the A2A message's correlation context.
///
/// The returned span records `a2a.message_id`, `a2a.correlation_id`,
/// `a2a.source`, and `a2a.target` as fields.  When a `causation_chain`
/// is present, each parent ID is recorded as `a2a.parent_id` so that
/// downstream consumers can reconstruct the causal graph.
///
/// # Usage
///
/// ```ignore
/// let span = correlation::span_from_a2a_context(&message);
/// let _guard = span.enter();
/// // ... process_message ...
/// ```
pub fn span_from_a2a_context(message: &ConvergedMessage) -> Span {
    let span = tracing::info_span!(
        "ggen.a2a.message",
        a2a.message_id = %message.message_id,
        a2a.correlation_id = ?message.envelope.correlation_id,
        a2a.source = %message.source,
        a2a.target = ?message.target,
    );

    // If there's a causation chain, add span links for trace correlation.
    if let Some(chain) = &message.envelope.causation_chain {
        for parent_id in chain {
            span.record("a2a.parent_id", parent_id.as_str());
        }
    }

    span
}

/// Extract the correlation ID from a `ConvergedMessage`, falling back to
/// the `message_id` when the envelope field is `None`.
///
/// This is useful when a batch processor needs a stable identifier for a
/// batch-level span that groups per-message child spans.
pub fn correlation_id_or_fallback(message: &ConvergedMessage) -> &str {
    message
        .envelope
        .correlation_id
        .as_deref()
        .unwrap_or(&message.message_id)
}

#[cfg(test)]
mod tests {
    use super::*;
    use a2a_generated::converged::message::ConvergedMessage;

    /// Build a minimal message for testing.
    fn test_message() -> ConvergedMessage {
        ConvergedMessage::text(
            "msg-test-1".to_string(),
            "test-agent".to_string(),
            "hello".to_string(),
        )
    }

    #[test]
    fn span_from_a2a_context_basic() {
        let msg = test_message();
        // Should not panic -- just exercises field construction.
        let _span = span_from_a2a_context(&msg);
    }

    #[test]
    fn span_from_a2a_context_with_correlation() {
        let mut msg = test_message();
        msg.envelope.correlation_id = Some("corr-42".to_string());
        msg.envelope.causation_chain = Some(vec!["parent-a".to_string(), "parent-b".to_string()]);
        let _span = span_from_a2a_context(&msg);
    }

    #[test]
    fn correlation_id_or_fallback_present() {
        let mut msg = test_message();
        msg.envelope.correlation_id = Some("corr-99".to_string());
        assert_eq!(correlation_id_or_fallback(&msg), "corr-99");
    }

    #[test]
    fn correlation_id_or_fallback_missing() {
        let msg = test_message();
        assert_eq!(correlation_id_or_fallback(&msg), "msg-test-1");
    }
}
