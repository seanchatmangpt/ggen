//! Admission gates and predicates for lawful packet/receipt flow
//! Validates structural integrity and legal requirements at each hierarchy level

use crate::hierarchy::{AdmissionVerdict, SegmentBuilder};
use crate::models::Construct8Packet;

/// Trait for admission predicates that validate packets or receipts
pub trait AdmissionPredicate: Send + Sync {
    fn admit(&self, packet: &Construct8Packet) -> AdmissionVerdict;
}

/// Structural admission gate: validates basic packet invariants
/// - valid_mask must be non-zero (at least one triple)
/// - emit_mask must have bits set only where valid_mask is set
/// - law_ref must be non-zero (lawful attribution required)
#[derive(Debug, Clone, Copy)]
pub struct StructuralAdmissionGate;

impl StructuralAdmissionGate {
    pub fn new() -> Self {
        Self
    }

    /// Perform structural admission check
    pub fn check(&self, packet: &Construct8Packet) -> AdmissionVerdict {
        // valid_mask must be non-zero
        if packet.valid_mask == 0 {
            return AdmissionVerdict::Refuse(
                "Packet has no valid triples (valid_mask == 0)".to_string(),
            );
        }

        // emit_mask can only be set where valid_mask is set
        if (packet.emit_mask & !packet.valid_mask) != 0 {
            return AdmissionVerdict::Refuse(
                "emit_mask has bits set where valid_mask is not set (structural violation)"
                    .to_string(),
            );
        }

        // law_ref must be non-zero
        if packet.law_ref == 0 {
            return AdmissionVerdict::Refuse(
                "Packet must have non-zero law_ref (unlawful)".to_string(),
            );
        }

        AdmissionVerdict::Admit
    }
}

impl Default for StructuralAdmissionGate {
    fn default() -> Self {
        Self::new()
    }
}

impl AdmissionPredicate for StructuralAdmissionGate {
    fn admit(&self, packet: &Construct8Packet) -> AdmissionVerdict {
        self.check(packet)
    }
}

/// Law admission gate: ensures packet law_ref matches expected value
#[derive(Debug, Clone, Copy)]
pub struct LawAdmissionGate {
    pub expected_law_ref: u64,
}

impl LawAdmissionGate {
    pub fn new(expected_law_ref: u64) -> Self {
        Self { expected_law_ref }
    }

    /// Perform law admission check
    pub fn check(&self, packet: &Construct8Packet) -> AdmissionVerdict {
        if packet.law_ref != self.expected_law_ref {
            return AdmissionVerdict::Refuse(format!(
                "law_ref mismatch: expected {}, got {}",
                self.expected_law_ref, packet.law_ref
            ));
        }
        AdmissionVerdict::Admit
    }
}

impl AdmissionPredicate for LawAdmissionGate {
    fn admit(&self, packet: &Construct8Packet) -> AdmissionVerdict {
        self.check(packet)
    }
}

/// Gated segment builder: applies admission predicate to each packet
/// before adding to segment
pub struct GatedSegmentBuilder<G: AdmissionPredicate> {
    gate: G,
    builder: SegmentBuilder,
}

impl<G: AdmissionPredicate> GatedSegmentBuilder<G> {
    pub fn new(gate: G, builder: SegmentBuilder) -> Self {
        Self { gate, builder }
    }

    /// Feed a packet if it passes the admission gate
    pub fn feed(&mut self, packet: &Construct8Packet) -> Result<(), String> {
        match self.gate.admit(packet) {
            AdmissionVerdict::Admit => self.builder.feed(packet),
            AdmissionVerdict::Refuse(reason) => Err(reason),
        }
    }

    /// Finish the gated segment builder and produce receipt
    pub fn finish(self) -> crate::hierarchy::SegmentReceipt {
        self.builder.finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_structural_gate_valid() {
        let gate = StructuralAdmissionGate::new();
        let packet = Construct8Packet {
            epoch: 100,
            law_ref: 1,
            subjects: [1, 0, 0, 0, 0, 0, 0, 0],
            predicates: [2, 0, 0, 0, 0, 0, 0, 0],
            objects: [3, 0, 0, 0, 0, 0, 0, 0],
            kind_mask: 0,
            valid_mask: 1,
            emit_mask: 1,
            block_mask: 0,
            order: 0,
            receipt_seed: [0u8; 32],
        };

        assert_eq!(gate.admit(&packet), AdmissionVerdict::Admit);
    }

    #[test]
    fn test_structural_gate_no_valid_mask() {
        let gate = StructuralAdmissionGate::new();
        let packet = Construct8Packet {
            epoch: 100,
            law_ref: 1,
            subjects: [0; 8],
            predicates: [0; 8],
            objects: [0; 8],
            kind_mask: 0,
            valid_mask: 0, // No valid triples
            emit_mask: 0,
            block_mask: 0,
            order: 0,
            receipt_seed: [0u8; 32],
        };

        match gate.admit(&packet) {
            AdmissionVerdict::Refuse(reason) => {
                assert!(reason.contains("no valid triples"));
            }
            AdmissionVerdict::Admit => panic!("Should have refused"),
        }
    }

    #[test]
    fn test_structural_gate_emit_not_in_valid() {
        let gate = StructuralAdmissionGate::new();
        let packet = Construct8Packet {
            epoch: 100,
            law_ref: 1,
            subjects: [1, 0, 0, 0, 0, 0, 0, 0],
            predicates: [2, 0, 0, 0, 0, 0, 0, 0],
            objects: [3, 0, 0, 0, 0, 0, 0, 0],
            kind_mask: 0,
            valid_mask: 1, // Only bit 0 valid
            emit_mask: 3,  // Bits 0 and 1 emit (bit 1 not valid)
            block_mask: 0,
            order: 0,
            receipt_seed: [0u8; 32],
        };

        match gate.admit(&packet) {
            AdmissionVerdict::Refuse(reason) => {
                assert!(reason.contains("structural violation"));
            }
            AdmissionVerdict::Admit => panic!("Should have refused"),
        }
    }

    #[test]
    fn test_structural_gate_no_law_ref() {
        let gate = StructuralAdmissionGate::new();
        let packet = Construct8Packet {
            epoch: 100,
            law_ref: 0, // Unlawful
            subjects: [1, 0, 0, 0, 0, 0, 0, 0],
            predicates: [2, 0, 0, 0, 0, 0, 0, 0],
            objects: [3, 0, 0, 0, 0, 0, 0, 0],
            kind_mask: 0,
            valid_mask: 1,
            emit_mask: 1,
            block_mask: 0,
            order: 0,
            receipt_seed: [0u8; 32],
        };

        match gate.admit(&packet) {
            AdmissionVerdict::Refuse(reason) => {
                assert!(reason.contains("unlawful"));
            }
            AdmissionVerdict::Admit => panic!("Should have refused"),
        }
    }

    #[test]
    fn test_law_gate_match() {
        let gate = LawAdmissionGate::new(42);
        let packet = Construct8Packet {
            epoch: 100,
            law_ref: 42,
            subjects: [1, 0, 0, 0, 0, 0, 0, 0],
            predicates: [2, 0, 0, 0, 0, 0, 0, 0],
            objects: [3, 0, 0, 0, 0, 0, 0, 0],
            kind_mask: 0,
            valid_mask: 1,
            emit_mask: 1,
            block_mask: 0,
            order: 0,
            receipt_seed: [0u8; 32],
        };

        assert_eq!(gate.admit(&packet), AdmissionVerdict::Admit);
    }

    #[test]
    fn test_law_gate_mismatch() {
        let gate = LawAdmissionGate::new(42);
        let packet = Construct8Packet {
            epoch: 100,
            law_ref: 99,
            subjects: [1, 0, 0, 0, 0, 0, 0, 0],
            predicates: [2, 0, 0, 0, 0, 0, 0, 0],
            objects: [3, 0, 0, 0, 0, 0, 0, 0],
            kind_mask: 0,
            valid_mask: 1,
            emit_mask: 1,
            block_mask: 0,
            order: 0,
            receipt_seed: [0u8; 32],
        };

        match gate.admit(&packet) {
            AdmissionVerdict::Refuse(reason) => {
                assert!(reason.contains("law_ref mismatch"));
            }
            AdmissionVerdict::Admit => panic!("Should have refused"),
        }
    }

    #[test]
    fn test_gated_segment_builder() {
        let gate = StructuralAdmissionGate::new();
        let builder = crate::hierarchy::SegmentBuilder::new(1, 100, 200);
        let mut gated = GatedSegmentBuilder::new(gate, builder);

        let packet = Construct8Packet {
            epoch: 100,
            law_ref: 1,
            subjects: [1, 0, 0, 0, 0, 0, 0, 0],
            predicates: [2, 0, 0, 0, 0, 0, 0, 0],
            objects: [3, 0, 0, 0, 0, 0, 0, 0],
            kind_mask: 0,
            valid_mask: 1,
            emit_mask: 1,
            block_mask: 0,
            order: 0,
            receipt_seed: [0u8; 32],
        };

        assert!(gated.feed(&packet).is_ok());
        let receipt = gated.finish();
        assert_eq!(receipt.packet_count, 1);
    }
}
