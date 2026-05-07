//! Law and TableLaw: Traits for compiled operational semantics

/// A high-level law for a semantic machine.
pub trait Law {
    type Field: Copy;
    type Condition: Copy;
    type Admitted: Copy;
    type Receipt;
    type Exit;
    type Error;

    fn validate(field: Self::Field) -> std::result::Result<Self::Field, Self::Error>;
    fn select(field: Self::Field) -> Self::Condition;
    fn admit(field: Self::Field, condition: Self::Condition) -> Self::Admitted;
    fn receipt(admitted: Self::Admitted, sequence: u64) -> Self::Receipt;
    fn exit(receipt: Self::Receipt) -> Self::Exit;
}

/// A law that can be represented as a 256-state lookup table (2^8).
pub trait TableLaw: Law<Field = u8> {
    const VALID: [bool; 256];
    const SELECT: [Self::Condition; 256];
    #[allow(clippy::type_complexity)]
    const ADMIT: [fn(u8, Self::Condition) -> Self::Admitted; 256];
}
