use crate::proof::receipt::StewardshipReceipt;
use anyhow::{anyhow, Result};
use blake3;

pub struct ReplayEngine;

impl ReplayEngine {
    pub fn verify_transition(
        receipt: &StewardshipReceipt, input: &[u8], output: &[u8],
    ) -> Result<()> {
        let actual_input_hash: [u8; 32] = blake3::hash(input).into();
        let actual_output_hash: [u8; 32] = blake3::hash(output).into();

        if actual_input_hash != receipt.input_state_hash {
            return Err(anyhow!("Replay Failure: Input hash mismatch."));
        }

        if actual_output_hash != receipt.output_state_hash {
            return Err(anyhow!("Replay Failure: Output hash mismatch. Transition did not reproduce expected state."));
        }

        Ok(())
    }
}
