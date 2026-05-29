use blake3::Hash;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Receipt {
    pub hash: String,
    pub packet_count: usize,
    pub triple_count: usize,
}

impl Receipt {
    pub fn new(hash: Hash, packet_count: usize, triple_count: usize) -> Self {
        Self {
            hash: hash.to_hex().to_string(),
            packet_count,
            triple_count,
        }
    }
}
