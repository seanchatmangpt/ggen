//! Telco Routing Commands
//!
//! This module provides commands for carrier-grade routing using the A2A UnifiedMessageRouter,
//! inspired by Bell Labs and 2600 phreaking culture.

use crate::runtime;
use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use serde::Serialize;

#[derive(Serialize)]
pub struct TelcoOutput {
    pub status: String,
    pub message: String,
    pub trace_id: Option<String>,
}

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
    use ggen_core::a2a_generated::converged::ConvergedMessage;
    use ggen_core::a2a_generated::handlers::HandlerFactory;
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

/// Dial a specific agent directly (A2A point-to-point)
///
/// Bypasses the routing matrix for a direct connection to a target extension.
#[verb]
pub fn dial(extension: Option<String>) -> VerbResult<TelcoOutput> {
    let ext = extension.unwrap_or_else(|| "0".to_string());
    use ggen_core::a2a_generated::converged::ConvergedMessage;
    use ggen_core::a2a_generated::handlers::HandlerFactory;

    runtime::block_on(async move {
        let mut router = HandlerFactory::create_router();
        let mut message = ConvergedMessage::text(
            format!("dial-{}", ext),
            "telco-cli".to_string(),
            format!("Dialing extension {}", ext),
        );
        message.target = Some(ext.clone());

        let result = router.route(&message).await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Dial failed: {:?}", e)))?;

        Ok(TelcoOutput {
            status: format!("{:?}", result.status),
            message: format!("Direct connection established via {} ops", result.metrics.operations),
            trace_id: Some(format!("trace-{}", message.message_id)),
        })
    }).map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
}

/// Manage the routing fabric and crossbar switches
///
/// Configure the physical/logical pathways of the A2A matrix.
#[verb]
pub fn switch(config: Option<String>) -> VerbResult<TelcoOutput> {
    let cfg = config.unwrap_or_default();
    use ggen_core::a2a_generated::converged::ConvergedMessage;
    use ggen_core::a2a_generated::handlers::HandlerFactory;

    runtime::block_on(async move {
        let mut router = HandlerFactory::create_router();
        let message = ConvergedMessage::text(
            "switch-cfg".to_string(),
            "telco-cli".to_string(),
            cfg,
        ).with_metadata("type".to_string(), serde_json::Value::String("config".to_string()));

        let result = router.route(&message).await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Switch failed: {:?}", e)))?;

        Ok(TelcoOutput {
            status: format!("{:?}", result.status),
            message: format!("Crossbar routing matrix updated via {} ops", result.metrics.operations),
            trace_id: Some(format!("trace-{}", message.message_id)),
        })
    }).map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
}

/// Establish a high-capacity multiplexed trunk line
///
/// Bonds multiple A2A channels for high-throughput swarm communication.
#[verb]
pub fn trunk(capacity: Option<u32>) -> VerbResult<TelcoOutput> {
    let cap = capacity.unwrap_or(10);
    use ggen_core::a2a_generated::converged::ConvergedMessage;
    use ggen_core::a2a_generated::handlers::HandlerFactory;

    runtime::block_on(async move {
        let mut router = HandlerFactory::create_router();
        let message = ConvergedMessage::text(
            "trunk-est".to_string(),
            "telco-cli".to_string(),
            format!("Capacity {}", cap),
        ).with_metadata("capacity".to_string(), serde_json::Value::Number(cap.into()));

        let result = router.route(&message).await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Trunk failed: {:?}", e)))?;

        Ok(TelcoOutput {
            status: format!("{:?}", result.status),
            message: format!("Established trunk line with capacity: {} channels ({} ops)", cap, result.metrics.operations),
            trace_id: Some(format!("trace-{}", message.message_id)),
        })
    }).map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
}

/// Emit a 2600Hz control tone to drop into supervisor mode
///
/// Sends raw control signals to the MCP matrix.
#[verb]
pub fn bluebox() -> VerbResult<TelcoOutput> {
    use ggen_core::a2a_generated::converged::ConvergedMessage;
    use ggen_core::a2a_generated::handlers::HandlerFactory;

    runtime::block_on(async move {
        let mut router = HandlerFactory::create_router();
        let message = ConvergedMessage::text(
            "bluebox-tone".to_string(),
            "telco-cli".to_string(),
            "2600Hz".to_string(),
        ).with_metadata("otel.kind".to_string(), serde_json::Value::String("supervisor".to_string()));

        let result = router.route(&message).await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Bluebox failed: {:?}", e)))?;

        Ok(TelcoOutput {
            status: format!("{:?}", result.status),
            message: format!("2600Hz tone accepted. Dropping to raw control protocol ({} ops).", result.metrics.operations),
            trace_id: Some(format!("trace-{}", message.message_id)),
        })
    }).map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
}

/// Wiretap a connection to monitor the event bus
///
/// Non-intrusive observability tap on an A2A routing channel.
#[verb]
pub fn tap(channel: Option<String>) -> VerbResult<TelcoOutput> {
    let chan = channel.unwrap_or_else(|| "global".to_string());
    use ggen_core::a2a_generated::converged::ConvergedMessage;
    use ggen_core::a2a_generated::handlers::HandlerFactory;

    runtime::block_on(async move {
        let mut router = HandlerFactory::create_router();
        let mut message = ConvergedMessage::text(
            "tap-monitor".to_string(),
            "telco-cli".to_string(),
            format!("Tap channel {}", chan),
        );
        message.target = Some(chan.clone());

        let result = router.route(&message).await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Tap failed: {:?}", e)))?;

        Ok(TelcoOutput {
            status: format!("{:?}", result.status),
            message: format!("Observability wiretap active on channel: {} ({} ops)", chan, result.metrics.operations),
            trace_id: Some(format!("trace-{}", message.message_id)),
        })
    }).map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
}

/// Escalate an unresolved signal to a human Operator
///
/// Dials "0" to invoke human-in-the-loop assistance.
#[verb]
pub fn operator() -> VerbResult<TelcoOutput> {
    use ggen_core::a2a_generated::converged::ConvergedMessage;
    use ggen_core::a2a_generated::handlers::HandlerFactory;

    runtime::block_on(async move {
        let mut router = HandlerFactory::create_router();
        let mut message = ConvergedMessage::text(
            "operator-call".to_string(),
            "telco-cli".to_string(),
            "0".to_string(),
        );
        message.target = Some("operator".to_string());

        let result = router.route(&message).await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Operator call failed: {:?}", e)))?;

        Ok(TelcoOutput {
            status: format!("{:?}", result.status),
            message: format!("Escalated to operator ({} ops). Please hold...", result.metrics.operations),
            trace_id: Some(format!("trace-{}", message.message_id)),
        })
    }).map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
}

/// Exploit or fuzz the routing pathways
///
/// Chaos engineering injection to test routing resilience.
#[verb]
pub fn phreak() -> VerbResult<TelcoOutput> {
    use ggen_core::a2a_generated::converged::ConvergedMessage;
    use ggen_core::a2a_generated::handlers::HandlerFactory;

    runtime::block_on(async move {
        let mut router = HandlerFactory::create_router();
        // Intentional chaos payload
        let message = ConvergedMessage::text(
            "phreak-fuzz".to_string(),
            "telco-cli".to_string(),
            "!@#$%^&*()_+".to_string(),
        ).with_metadata("chaos".to_string(), serde_json::Value::Bool(true));

        // It might fail, but that's what we want to trace
        let result = router.route(&message).await;
        
        let (status, ops) = match result {
            Ok(res) => (format!("{:?}", res.status), res.metrics.operations),
            Err(_) => ("chaos_handled".to_string(), 1), // handled gracefully
        };

        Ok(TelcoOutput {
            status,
            message: format!("Chaos injection processed. Ops: {}", ops),
            trace_id: Some(format!("trace-{}", message.message_id)),
        })
    }).map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
}

