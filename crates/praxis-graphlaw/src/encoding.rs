use crate::{fastmap::FxHashMap, BlankNodeImpl, LiteralImpl, Term, TermImpl};
use once_cell::sync::Lazy;
use std::sync::Mutex;

static GLOBAL_ENCODER: Lazy<Mutex<InternalEncoder>> =
    Lazy::new(|| Mutex::new(InternalEncoder::new()));

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum EncodedValue {
    Iri(String),
    LiteralLexical(String),
    BlankNodeLabel(String),
    Literal {
        value: usize,            // Index of the lexical value string
        datatype: Option<usize>, // Index of the datatype IRI
        lang: Option<usize>,     // Index of the language tag
    },
    Variable(String),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct InternalEncoder {
    encoded: FxHashMap<EncodedValue, usize>,
    decoded: FxHashMap<usize, EncodedValue>,
    counter: usize,
}

impl Default for InternalEncoder {
    fn default() -> Self {
        Self::new()
    }
}

impl InternalEncoder {
    pub fn new() -> InternalEncoder {
        InternalEncoder {
            encoded: FxHashMap::default(),
            decoded: FxHashMap::default(),
            counter: 0,
        }
    }

    pub fn add_iri(&mut self, iri: String) -> usize {
        let val = EncodedValue::Iri(iri);
        self.intern(val)
    }

    pub fn add_literal_lexical(&mut self, lexical: String) -> usize {
        let val = EncodedValue::LiteralLexical(lexical);
        self.intern(val)
    }

    pub fn add_blank_node_label(&mut self, label: String) -> usize {
        let val = EncodedValue::BlankNodeLabel(label);
        self.intern(val)
    }

    pub fn add_variable(&mut self, var: String) -> usize {
        let val = EncodedValue::Variable(var);
        self.intern(val)
    }

    pub fn add_literal(
        &mut self,
        value: String,
        datatype: Option<String>,
        lang: Option<String>,
    ) -> usize {
        let value_id = self.add_literal_lexical(value);
        let datatype_id = datatype.map(|dt| self.add_iri(dt));
        let lang_id = lang.map(|l| self.add_literal_lexical(l));
        let val = EncodedValue::Literal {
            value: value_id,
            datatype: datatype_id,
            lang: lang_id,
        };
        self.intern(val)
    }

    fn intern(&mut self, val: EncodedValue) -> usize {
        if let Some(id) = self.encoded.get(&val) {
            *id
        } else {
            let id = self.counter;
            self.encoded.insert(val.clone(), id);
            self.decoded.insert(id, val);
            self.counter += 1;
            id
        }
    }

    pub fn add(&mut self, s: String) -> usize {
        if s.starts_with('?') {
            self.add_variable(s)
        } else if let Some(label) = s.strip_prefix("_:") {
            self.add_blank_node_label(label.to_string())
        } else if s.starts_with('"') {
            let last_quote = s.rfind('"');
            if let Some(end_lex) = last_quote {
                if end_lex > 0 {
                    let lexical = s[1..end_lex].to_string();
                    let suffix = &s[end_lex + 1..];
                    let mut datatype = None;
                    let mut lang = None;
                    if let Some(tag) = suffix.strip_prefix('@') {
                        lang = Some(tag.to_string());
                    } else if let Some(dt) = suffix.strip_prefix("^^") {
                        datatype = Some(dt.to_string());
                    }
                    self.add_literal(lexical, datatype, lang)
                } else {
                    self.add_iri(s)
                }
            } else {
                self.add_iri(s)
            }
        } else {
            self.add_iri(s)
        }
    }

    pub fn get(&self, s: &str) -> Option<usize> {
        if s.starts_with('?') {
            self.encoded
                .get(&EncodedValue::Variable(s.to_string()))
                .copied()
        } else {
            if let Some(id) = self.encoded.get(&EncodedValue::Iri(s.to_string())).copied() {
                Some(id)
            } else if let Some(id) = self
                .encoded
                .get(&EncodedValue::Variable(format!("?{}", s)))
                .copied()
            {
                Some(id)
            } else if let Some(label) = s.strip_prefix("_:") {
                self.encoded
                    .get(&EncodedValue::BlankNodeLabel(label.to_string()))
                    .copied()
            } else if s.starts_with('"') {
                let last_quote = s.rfind('"');
                if let Some(end_lex) = last_quote {
                    if end_lex > 0 {
                        let lexical = s[1..end_lex].to_string();
                        let suffix = &s[end_lex + 1..];
                        let mut datatype = None;
                        let mut lang = None;
                        if let Some(tag) = suffix.strip_prefix('@') {
                            lang = Some(tag.to_string());
                        } else if let Some(dt) = suffix.strip_prefix("^^") {
                            datatype = Some(dt.to_string());
                        }
                        let val_id = self.encoded.get(&EncodedValue::LiteralLexical(lexical))?;
                        let datatype_id = if let Some(dt) = datatype {
                            Some(*self.encoded.get(&EncodedValue::Iri(dt))?)
                        } else {
                            None
                        };
                        let lang_id = if let Some(l) = lang {
                            Some(*self.encoded.get(&EncodedValue::LiteralLexical(l))?)
                        } else {
                            None
                        };
                        let lit_val = EncodedValue::Literal {
                            value: *val_id,
                            datatype: datatype_id,
                            lang: lang_id,
                        };
                        self.encoded.get(&lit_val).copied()
                    } else {
                        self.encoded.get(&EncodedValue::Iri(s.to_string())).copied()
                    }
                } else {
                    self.encoded.get(&EncodedValue::Iri(s.to_string())).copied()
                }
            } else {
                self.encoded.get(&EncodedValue::Iri(s.to_string())).copied()
            }
        }
    }

    pub fn decode(&self, encoded: &usize) -> Option<String> {
        match self.decoded.get(encoded)? {
            EncodedValue::Iri(s) => Some(s.clone()),
            EncodedValue::LiteralLexical(s) => Some(s.clone()),
            EncodedValue::BlankNodeLabel(label) => Some(format!("_:{}", label)),
            EncodedValue::Variable(s) => Some(s.clone()),
            EncodedValue::Literal {
                value,
                datatype,
                lang,
            } => {
                let val_str = match self.decoded.get(value)? {
                    EncodedValue::LiteralLexical(s) => s.clone(),
                    _ => return None,
                };
                let mut res = format!("\"{}\"", val_str);
                if let Some(lang_id) = lang {
                    if let EncodedValue::LiteralLexical(lang_str) = self.decoded.get(lang_id)? {
                        res.push_str(&format!("@{}", lang_str));
                    }
                } else if let Some(dt_id) = datatype {
                    if let EncodedValue::Iri(dt_str) = self.decoded.get(dt_id)? {
                        res.push_str(&format!("^^{}", dt_str));
                    }
                }
                Some(res)
            }
        }
    }

    pub fn decode_to_term(&self, id: usize) -> Option<Term> {
        match self.decoded.get(&id)? {
            EncodedValue::Iri(_) => Some(Term::Iri(TermImpl { iri: id })),
            EncodedValue::BlankNodeLabel(_) => Some(Term::BlankNode(BlankNodeImpl { id })),
            EncodedValue::Literal {
                value,
                datatype,
                lang,
            } => Some(Term::Literal(LiteralImpl {
                id,
                value: *value,
                datatype: *datatype,
                lang: *lang,
            })),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Encoder {}

// Swarm finding (`panics-unwraps-core`, encoding.rs:236, HIGH): `GLOBAL_ENCODER.lock().unwrap()`
// turned any single panic while a thread held this lock (from lib.rs:104-114's documented
// `RSPEngine::register_r2r` multi-thread mode, or any other panic mid-`Encoder::*` call) into a
// permanent process-wide crash -- every subsequent `Encoder::add/get/decode` call panics
// immediately on the poisoned lock, wedging all other callers for the rest of the process's
// life, not just the thread that panicked. Fixed with Rust's standard poison-recovery idiom
// (`unwrap_or_else(|poisoned| poisoned.into_inner())`) rather than propagating a typed refusal:
// `InternalEncoder`'s own mutations (`intern`'s two `HashMap::insert`s plus a counter increment)
// are simple, synchronous, and do not themselves panic under any realistic input, so a poisoning
// event necessarily originates from a caller-side bug *outside* this locked section, not from a
// half-completed mutation of the encoder's own `encoded`/`decoded` maps -- recovering the guard
// restores exactly the valid state that existed when the unrelated panic fired. This is strictly
// better than today's guaranteed-permanent-wedge, not a claim that every possible panic source is
// eliminated (that would require auditing and hardening every call site that can reach these
// functions, a much larger, separate effort).
fn recover(
    result: std::sync::LockResult<std::sync::MutexGuard<'_, InternalEncoder>>,
) -> std::sync::MutexGuard<'_, InternalEncoder> {
    result.unwrap_or_else(|poisoned| poisoned.into_inner())
}

impl Encoder {
    pub fn add(uri: String) -> usize {
        let mut encoder = recover(GLOBAL_ENCODER.lock());
        encoder.add(uri)
    }

    pub fn get(uri: &str) -> Option<usize> {
        let encoder = recover(GLOBAL_ENCODER.lock());
        encoder.get(uri)
    }

    pub fn decode(encoded: &usize) -> Option<String> {
        let encoder = recover(GLOBAL_ENCODER.lock());
        encoder.decode(encoded)
    }

    pub fn add_iri(iri: String) -> usize {
        let mut encoder = recover(GLOBAL_ENCODER.lock());
        encoder.add_iri(iri)
    }

    pub fn add_blank_node(label: String) -> usize {
        let mut encoder = recover(GLOBAL_ENCODER.lock());
        encoder.add_blank_node_label(label)
    }

    pub fn add_literal(value: String, datatype: Option<String>, lang: Option<String>) -> usize {
        let mut encoder = recover(GLOBAL_ENCODER.lock());
        encoder.add_literal(value, datatype, lang)
    }

    pub fn decode_to_term(id: usize) -> Option<Term> {
        let encoder = recover(GLOBAL_ENCODER.lock());
        encoder.decode_to_term(id)
    }
}

// Regression for swarm finding `panics-unwraps-core` (encoding.rs:236, HIGH): before the
// `recover()` fix above, poisoning `GLOBAL_ENCODER` (e.g. via any panic on a thread holding the
// lock, a real, documented, live code path per lib.rs:104-114's multi-thread `RSPEngine` mode, not
// just a hypothetical) would have permanently wedged every subsequent `Encoder::add/get/decode`
// call for the rest of the process's life. This test deliberately poisons the lock on a separate
// thread, then proves the crate's own public API continues to work correctly afterward -- not
// just "doesn't panic," but returns the actually-correct decoded value.
#[test]
fn encoder_recovers_from_lock_poisoning() {
    let handle = std::thread::spawn(|| {
        let _guard = GLOBAL_ENCODER.lock().unwrap();
        panic!("intentional test poisoning of GLOBAL_ENCODER");
    });
    // The spawned thread panics while holding the lock; `join` returns `Err` with the panic
    // payload, which we deliberately discard -- the poisoning itself is the test setup, not the
    // thing under test.
    assert!(
        handle.join().is_err(),
        "the spawned thread must have actually panicked, or this test proves nothing"
    );

    let id = Encoder::add("http://poison-recovery-test/example".to_string());
    let decoded =
        Encoder::decode(&id).expect("Encoder::decode must succeed after lock poisoning recovery");
    assert_eq!(
        decoded, "http://poison-recovery-test/example",
        "the recovered encoder must return the correct value, not just avoid panicking"
    );
}

#[test]
fn test_encoding() {
    let mut encoder = InternalEncoder::new();
    let _encoded1 = encoder.add("http://test/1".to_string());
    let encoded2 = encoder.add("http://test/2".to_string());
    let encoded3 = encoder.add("http://test/3".to_string());
    let decoded2 = encoder.decode(&encoded2);
    let decoded2_again = encoder.decode(&encoded2);
    assert_eq!("http://test/2", decoded2.unwrap());
    assert_eq!("http://test/2", decoded2_again.unwrap());
    assert_eq!(2, encoded3);
}

#[test]
fn test_encoder_literal_vs_iri_distinct() {
    // Add same lexical value string as an IRI and as a literal, verify IDs are distinct.
    let iri_str = "http://example.com/foo".to_string();
    let lit_str = "\"http://example.com/foo\"".to_string();

    let iri_id = Encoder::add(iri_str.clone());
    let lit_id = Encoder::add(lit_str.clone());

    assert_ne!(iri_id, lit_id, "IRI and Literal must have distinct IDs");

    let decoded_iri = Encoder::decode(&iri_id).unwrap();
    let decoded_lit = Encoder::decode(&lit_id).unwrap();

    assert_eq!(decoded_iri, "http://example.com/foo");
    assert_eq!(decoded_lit, "\"http://example.com/foo\"");
}

#[test]
fn test_literal_datatype_and_langtag_preserved() {
    let lit_with_dt = "\"10\"^^<http://www.w3.org/2001/XMLSchema#integer>".to_string();
    let lit_with_lang = "\"hello\"@en".to_string();

    let dt_id = Encoder::add(lit_with_dt.clone());
    let lang_id = Encoder::add(lit_with_lang.clone());

    assert_eq!(Encoder::decode(&dt_id).unwrap(), lit_with_dt);
    assert_eq!(Encoder::decode(&lang_id).unwrap(), lit_with_lang);

    // Verify decode_to_term works
    let term_dt = Encoder::decode_to_term(dt_id).unwrap();
    if let Term::Literal(lit) = term_dt {
        assert_eq!(Encoder::decode(&lit.id).unwrap(), lit_with_dt);
        assert_eq!(Encoder::decode(&lit.value).unwrap(), "10");
        assert_eq!(
            Encoder::decode(&lit.datatype.unwrap()).unwrap(),
            "<http://www.w3.org/2001/XMLSchema#integer>"
        );
        assert!(lit.lang.is_none());
    } else {
        panic!("Expected Term::Literal");
    }

    let term_lang = Encoder::decode_to_term(lang_id).unwrap();
    if let Term::Literal(lit) = term_lang {
        assert_eq!(Encoder::decode(&lit.id).unwrap(), lit_with_lang);
        assert_eq!(Encoder::decode(&lit.value).unwrap(), "hello");
        assert!(lit.datatype.is_none());
        assert_eq!(Encoder::decode(&lit.lang.unwrap()).unwrap(), "en");
    } else {
        panic!("Expected Term::Literal");
    }
}
