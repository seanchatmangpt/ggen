// Aggregate accumulator implementations for COUNT, SUM, MIN, MAX, AVG
use crate::utils::Utils;
use crate::Encoder;

// `Encoder::add`'s generic classifier (encoding.rs) only recognizes 3 lexical shapes as
// non-IRI: a leading `?` (variable), a leading `_:` (blank node), and a leading `"`
// (quoted literal, e.g. `"3"^^<...>`). A bare numeric string like `Encoder::add("3".to_string())`
// matches none of those and falls through to `add_iri` -- silently mis-encoding every
// aggregate result as an IRI rather than a typed literal. This is invisible to a caller
// that only ever decodes the RAW string back out for display (which is why this repo's
// own group_by_unbound_variable_groups_instead_of_panicking test, which only checks
// `count_binding.val == "3"`, passed before this fix), but any expression-level use of an
// aggregate's own output (HAVING, ORDER BY, arithmetic on an aggregate) needs a real typed
// literal to compare against -- `PlanExpression::Variable`'s evaluator (eval.rs) only
// produces an `EncodedTerm::IntegerLiteral`/similar when `Encoder::decode_to_term` returns
// `Term::Literal` with a matching xsd datatype; an IRI decodes to `Term::Iri` instead, so
// e.g. `HAVING (COUNT(*) > 1)` compares a NamedNode against an IntegerLiteral and silently
// evaluates false for every row. `Encoder::add_literal(value, datatype, lang)` bypasses the
// generic classifier and interns the correct `EncodedValue::Literal` shape directly.
const XSD_INTEGER: &str = "<http://www.w3.org/2001/XMLSchema#integer>";
const XSD_DECIMAL: &str = "<http://www.w3.org/2001/XMLSchema#decimal>";

pub trait Accumulator {
    fn add(&mut self, encoded_item: usize);
    fn get(&self) -> usize;
}

#[derive(Debug, Default)]
pub struct CountAccumulator {
    count: usize,
}

impl Accumulator for CountAccumulator {
    fn add(&mut self, _item: usize) {
        self.count += 1;
    }
    fn get(&self) -> usize {
        // COUNT is always a whole number -- xsd:integer per SPARQL 1.1 (18.5.1.1).
        Encoder::add_literal(self.count.to_string(), Some(XSD_INTEGER.to_string()), None)
    }
}

#[derive(Debug)]
pub struct SumAccumulator {
    sum: f64,
}

impl Accumulator for SumAccumulator {
    fn add(&mut self, item: usize) {
        if let Some(val) = Encoder::decode(&item) {
            let val = Utils::remove_literal_tags(&val);
            self.sum += val.parse::<f64>().unwrap_or(0.0);
        }
    }
    fn get(&self) -> usize {
        // This accumulator stores operands as f64 regardless of their original xsd type
        // (no integer/decimal/double type-promotion tracking) -- xsd:decimal is the
        // widest-safe SPARQL numeric tag for that representation, not a claim that the
        // original operand types are preserved. Disclosed, separate gap (not fixed here):
        // the query engine's own EncodedTerm (sparql/eval.rs) has no decimal/float
        // variant, only IntegerLiteral -- so a value tagged xsd:decimal still can't be
        // numerically compared once it flows through PlanExpression::Variable's
        // evaluator (it falls through to a generic StringLiteral there), meaning
        // HAVING/ORDER BY/comparisons on a SUM/MIN/MAX/AVG result still don't work
        // end-to-end even after this fix -- only the accumulated VALUE is now correct
        // and properly typed as a literal (previously mis-typed as an IRI). See the
        // matching doc note on sparql/eval.rs's PlanExpression::Variable arm.
        Encoder::add_literal(self.sum.to_string(), Some(XSD_DECIMAL.to_string()), None)
    }
}

impl Default for SumAccumulator {
    fn default() -> Self {
        SumAccumulator { sum: 0.0 }
    }
}

#[derive(Debug, Default)]
pub struct MinAccumulator {
    min: Option<f64>,
}

impl Accumulator for MinAccumulator {
    fn add(&mut self, item: usize) {
        if let Some(val) = Encoder::decode(&item) {
            let val = Utils::remove_literal_tags(&val);
            if let Ok(num) = val.parse::<f64>() {
                self.min = Some(self.min.map_or(num, |m| m.min(num)));
            }
        }
    }
    fn get(&self) -> usize {
        let val = self.min.map_or("0".to_string(), |m| m.to_string());
        Encoder::add_literal(val, Some(XSD_DECIMAL.to_string()), None)
    }
}

#[derive(Debug, Default)]
pub struct MaxAccumulator {
    max: Option<f64>,
}

impl Accumulator for MaxAccumulator {
    fn add(&mut self, item: usize) {
        if let Some(val) = Encoder::decode(&item) {
            let val = Utils::remove_literal_tags(&val);
            if let Ok(num) = val.parse::<f64>() {
                self.max = Some(self.max.map_or(num, |m| m.max(num)));
            }
        }
    }
    fn get(&self) -> usize {
        let val = self.max.map_or("0".to_string(), |m| m.to_string());
        Encoder::add_literal(val, Some(XSD_DECIMAL.to_string()), None)
    }
}

#[derive(Debug)]
pub struct AvgAccumulator {
    sum: f64,
    count: usize,
}

impl Accumulator for AvgAccumulator {
    fn add(&mut self, item: usize) {
        if let Some(val) = Encoder::decode(&item) {
            let val = Utils::remove_literal_tags(&val);
            if let Ok(num) = val.parse::<f64>() {
                self.sum += num;
                self.count += 1;
            }
        }
    }
    fn get(&self) -> usize {
        let avg = if self.count > 0 {
            self.sum / self.count as f64
        } else {
            0.0
        };
        Encoder::add_literal(avg.to_string(), Some(XSD_DECIMAL.to_string()), None)
    }
}

impl Default for AvgAccumulator {
    fn default() -> Self {
        AvgAccumulator { sum: 0.0, count: 0 }
    }
}

#[derive(Debug)]
pub enum AccumulatorImpl {
    Count(CountAccumulator),
    Sum(SumAccumulator),
    Min(MinAccumulator),
    Max(MaxAccumulator),
    Avg(AvgAccumulator),
}

impl Accumulator for AccumulatorImpl {
    fn add(&mut self, encoded_item: usize) {
        match self {
            Self::Count(acc) => acc.add(encoded_item),
            Self::Sum(acc) => acc.add(encoded_item),
            Self::Min(acc) => acc.add(encoded_item),
            Self::Max(acc) => acc.add(encoded_item),
            Self::Avg(acc) => acc.add(encoded_item),
        }
    }
    fn get(&self) -> usize {
        match self {
            Self::Count(acc) => acc.get(),
            Self::Sum(acc) => acc.get(),
            Self::Min(acc) => acc.get(),
            Self::Max(acc) => acc.get(),
            Self::Avg(acc) => acc.get(),
        }
    }
}
