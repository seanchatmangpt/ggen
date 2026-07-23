use super::common::{domain_digest, EvidenceError};

pub fn rendezvous_shard(identity: &str, shard_count: usize) -> Result<usize, EvidenceError> {
    if shard_count == 0 {
        return Err(EvidenceError::InvalidData {
            identity: identity.to_owned(),
            message: "zero shard count".to_owned(),
        });
    }

    let mut winner = 0_usize;
    let mut winner_score = [0_u8; 32];
    for shard in 0..shard_count {
        let shard_bytes = (shard as u64).to_le_bytes();
        let score = domain_digest("tcps/rendezvous/v1", &[identity.as_bytes(), &shard_bytes]);
        if shard == 0 || score.as_bytes() > &winner_score {
            winner = shard;
            winner_score = *score.as_bytes();
        }
    }
    Ok(winner)
}
