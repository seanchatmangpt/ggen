use crate::semantic_bit::law::Law;
use crate::semantic_bit::phase::*;
use std::marker::PhantomData;

/// A machine that enforces operational law via typestate.
pub struct Machine<L: Law, P> {
    pub field: L::Field,
    _marker: PhantomData<(L, P)>,
}

impl<L: Law> Machine<L, Input> {
    pub fn new(field: L::Field) -> Self {
        Self {
            field,
            _marker: PhantomData,
        }
    }

    pub fn validate(self) -> std::result::Result<Machine<L, Validated>, L::Error> {
        let field = L::validate(self.field)?;
        Ok(Machine {
            field,
            _marker: PhantomData,
        })
    }
}

impl<L: Law> Machine<L, Validated> {
    pub fn select(self) -> (Machine<L, Selected<L::Condition>>, L::Condition) {
        let condition = L::select(self.field);
        (
            Machine {
                field: self.field,
                _marker: PhantomData,
            },
            condition,
        )
    }
}

impl<L: Law, C> Machine<L, Selected<C>> {
    pub fn admit(self, condition: L::Condition) -> Machine<L, Admitted<L::Condition>> {
        let _ = L::admit(self.field, condition);
        Machine {
            field: self.field,
            _marker: PhantomData,
        }
    }
}

impl<L: Law, C> Machine<L, Admitted<C>> {
    pub fn receipt(self, sequence: u64) -> (Machine<L, Receipted<L::Receipt>>, L::Receipt) {
        let condition = L::select(self.field);
        let admitted = L::admit(self.field, condition);
        let receipt = L::receipt(admitted, sequence);
        (
            Machine {
                field: self.field,
                _marker: PhantomData,
            },
            receipt,
        )
    }
}

impl<L: Law, R> Machine<L, Receipted<R>> {
    pub fn exit(self, receipt: L::Receipt) -> (Machine<L, Exited<L::Exit>>, L::Exit) {
        let exit = L::exit(receipt);
        (
            Machine {
                field: self.field,
                _marker: PhantomData,
            },
            exit,
        )
    }
}
