//! Telco Routing Commands
//!
//! This module provides commands for carrier-grade routing using the A2A UnifiedMessageRouter.

use crate::runtime;
use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use serde::Serialize;

#[derive(Serialize)]
pub struct RouteOutput {
    pub status: String,
    pub message: String,
    pub trace_id: Option<String>,
}

/// Execute a routing cycle using the MCPP Telco discipline
///
/// This command bridges the marketplace signal to the A2A routing layer.
/// In Vision 2030, this is the "Routing Discipline".
#[verb]
pub fn route(payload: Option<String>) -> VerbResult<RouteOutput> {
    use a2a_generated::converged::ConvergedMessage;
    use a2a_generated::handlers::HandlerFactory;
    use tracing::{info, info_span, Instrument};

    let payload_str = payload.unwrap_or_else(|| "{\"test\": \"v26.5.4\"}".to_string());

    runtime::block_on(async move {
        // 1. Boundary Crossing: Instantiate the REAL router from factory
        let mut router = HandlerFactory::create_router();

        // 2. Map external signal to ConvergedMessage
        let message = ConvergedMessage::text(
            "release-probe-1".to_string(),
            "telco-cli".to_string(),
            payload_str,
        );

        // 3. Execute real routing logic with instrumentation
        let span = info_span!(
            "telco_route",
            msg_id = %message.message_id,
            source = %message.source,
            target = ?message.target,
            otel.kind = "client"
        );

        let result = async {
            info!("Initiating A2A routing cycle");
            router.route(&message).await.map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!("Routing failed: {:?}", e))
            })
        }
        .instrument(span)
        .await?;

        // 4. Extract real trace_id and metrics
        // In Vision 2030, the trace_id must be tied to the causal message_id
        let trace_id = Some(format!("trace-{}", message.message_id));

        info!(
            status = ?result.status,
            ops = result.metrics.operations,
            "Routing cycle complete"
        );

        Ok(RouteOutput {
            status: format!("{:?}", result.status),
            message: format!("Routed via {} ops", result.metrics.operations),
            trace_id,
        })
    })
    .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
}
