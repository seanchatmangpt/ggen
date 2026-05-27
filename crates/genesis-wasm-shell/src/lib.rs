use genesis_core::{Construct8, Pair2, Receipt, RelationPage, ReplayCursor};
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Clone, Copy, Debug)]
pub struct WasmPair2 {
    pub left: u8,
    pub right: u8,
}

#[wasm_bindgen]
impl WasmPair2 {
    #[wasm_bindgen(constructor)]
    pub fn new(left: u8, right: u8) -> Self {
        Self { left, right }
    }
}

impl From<Pair2> for WasmPair2 {
    fn from(p: Pair2) -> Self {
        Self {
            left: p.left,
            right: p.right,
        }
    }
}

impl From<WasmPair2> for Pair2 {
    fn from(p: WasmPair2) -> Self {
        Self {
            left: p.left,
            right: p.right,
        }
    }
}

#[wasm_bindgen]
pub struct WasmRelationPage {
    inner: RelationPage<256>,
}

#[wasm_bindgen]
impl WasmRelationPage {
    #[wasm_bindgen(constructor)]
    pub fn new(predicate: u32) -> Self {
        Self {
            inner: RelationPage::new(predicate),
        }
    }

    #[wasm_bindgen(getter)]
    pub fn predicate(&self) -> u32 {
        self.inner.predicate
    }

    #[wasm_bindgen(getter)]
    pub fn len(&self) -> usize {
        self.inner.len as usize
    }

    #[wasm_bindgen]
    pub fn contains(&self, left: u8, right: u8) -> bool {
        self.inner.contains(Pair2::new(left, right))
    }

    #[wasm_bindgen]
    pub fn insert(&mut self, left: u8, right: u8, epoch: u64) -> Result<(), JsValue> {
        let pair = Pair2::new(left, right);
        self.inner
            .insert(pair, epoch)
            .map_err(|e| JsValue::from_str(&format!("Refused insert: {:?}", e.reason)))
    }

    #[wasm_bindgen]
    pub fn pairs_flat(&self) -> Vec<u8> {
        let len = self.inner.len as usize;
        let mut flat = Vec::with_capacity(len * 2);
        for i in 0..len {
            flat.push(self.inner.pairs[i].left);
            flat.push(self.inner.pairs[i].right);
        }
        flat
    }

    #[wasm_bindgen]
    pub fn serialize(&self) -> Vec<u8> {
        let len = self.inner.len as usize;
        let mut bytes = Vec::with_capacity(4 + 8 + len * 2);
        bytes.extend_from_slice(&self.inner.predicate.to_le_bytes());
        bytes.extend_from_slice(&(self.inner.len as u64).to_le_bytes());
        for i in 0..len {
            bytes.push(self.inner.pairs[i].left);
            bytes.push(self.inner.pairs[i].right);
        }
        bytes
    }

    #[wasm_bindgen]
    pub fn deserialize(bytes: &[u8]) -> Result<WasmRelationPage, JsValue> {
        if bytes.len() < 12 {
            return Err(JsValue::from_str("Invalid byte buffer: too short"));
        }
        let predicate = u32::from_le_bytes(
            bytes[0..4]
                .try_into()
                .map_err(|_| JsValue::from_str("Invalid predicate bytes"))?,
        );
        let len = u64::from_le_bytes(
            bytes[4..12]
                .try_into()
                .map_err(|_| JsValue::from_str("Invalid len bytes"))?,
        ) as usize;
        if bytes.len() < 12 + len * 2 {
            return Err(JsValue::from_str(
                "Invalid byte buffer: pairs length mismatch",
            ));
        }
        let mut page = RelationPage::new(predicate);
        for i in 0..len {
            let left = bytes[12 + i * 2];
            let right = bytes[12 + i * 2 + 1];
            page.insert(Pair2::new(left, right), 0)
                .map_err(|e| JsValue::from_str(&format!("Refused insert: {:?}", e.reason)))?;
        }
        Ok(WasmRelationPage { inner: page })
    }
}

#[wasm_bindgen]
pub struct RelationPageStreamer {
    current_page: Option<RelationPage<256>>,
    epoch: u64,
}

#[wasm_bindgen]
impl RelationPageStreamer {
    #[wasm_bindgen(constructor)]
    pub fn new(epoch: u64) -> Self {
        Self {
            current_page: None,
            epoch,
        }
    }

    #[wasm_bindgen]
    pub fn push_pair(
        &mut self,
        predicate: u32,
        left: u8,
        right: u8,
    ) -> Result<Option<WasmRelationPage>, JsValue> {
        let pair = Pair2::new(left, right);

        if let Some(ref mut page) = self.current_page {
            if page.predicate != predicate || page.len >= 256 {
                let completed = self.current_page.take().expect("must exist");

                let mut new_page = RelationPage::new(predicate);
                new_page
                    .insert(pair, self.epoch)
                    .map_err(|e| JsValue::from_str(&format!("Refused insert: {:?}", e.reason)))?;

                self.current_page = Some(new_page);
                return Ok(Some(WasmRelationPage { inner: completed }));
            } else {
                page.insert(pair, self.epoch)
                    .map_err(|e| JsValue::from_str(&format!("Refused insert: {:?}", e.reason)))?;
                return Ok(None);
            }
        } else {
            let mut page = RelationPage::new(predicate);
            page.insert(pair, self.epoch)
                .map_err(|e| JsValue::from_str(&format!("Refused insert: {:?}", e.reason)))?;
            self.current_page = Some(page);
            return Ok(None);
        }
    }

    #[wasm_bindgen]
    pub fn flush(&mut self) -> Option<WasmRelationPage> {
        self.current_page
            .take()
            .map(|p| WasmRelationPage { inner: p })
    }
}

#[wasm_bindgen]
pub struct WasmConstruct8 {
    inner: Construct8,
}

#[wasm_bindgen]
impl WasmConstruct8 {
    #[wasm_bindgen(constructor)]
    pub fn new(epoch: u64, relation_id: u32) -> Self {
        Self {
            inner: Construct8::new(epoch, relation_id),
        }
    }

    #[wasm_bindgen(getter)]
    pub fn epoch(&self) -> u64 {
        self.inner.epoch
    }

    #[wasm_bindgen(getter)]
    pub fn relation_id(&self) -> u32 {
        self.inner.relation_id
    }

    #[wasm_bindgen(getter)]
    pub fn valid_mask(&self) -> u8 {
        self.inner.valid_mask
    }

    #[wasm_bindgen]
    pub fn push(&mut self, left: u8, right: u8) -> Result<usize, JsValue> {
        let pair = Pair2::new(left, right);
        self.inner
            .push(pair)
            .map(|idx| idx)
            .map_err(|e| JsValue::from_str(&format!("Refused push: {:?}", e.reason)))
    }

    #[wasm_bindgen]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[wasm_bindgen]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    #[wasm_bindgen]
    pub fn get(&self, lane: usize) -> Option<WasmPair2> {
        self.inner.get(lane).map(|p| p.into())
    }
}

#[wasm_bindgen]
pub struct WasmReceipt {
    inner: Receipt,
}

#[wasm_bindgen]
impl WasmReceipt {
    #[wasm_bindgen]
    pub fn generate(act: &WasmConstruct8, previous_receipt: &[u8]) -> Result<WasmReceipt, JsValue> {
        if previous_receipt.len() != 32 {
            return Err(JsValue::from_str(
                "previous_receipt must be exactly 32 bytes",
            ));
        }
        let mut prev = [0u8; 32];
        prev.copy_from_slice(previous_receipt);
        let receipt = Receipt::generate(&act.inner, &prev);
        Ok(Self { inner: receipt })
    }

    #[wasm_bindgen(getter)]
    pub fn signature(&self) -> Vec<u8> {
        self.inner.signature.to_vec()
    }
}

#[wasm_bindgen]
pub struct WasmReplayCursor {
    inner: ReplayCursor,
}

#[wasm_bindgen]
impl WasmReplayCursor {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            inner: ReplayCursor::new(),
        }
    }

    #[wasm_bindgen(getter)]
    pub fn expected_epoch(&self) -> u64 {
        self.inner.expected_epoch
    }

    #[wasm_bindgen(getter)]
    pub fn expected_relation_id(&self) -> u32 {
        self.inner.expected_relation_id
    }

    #[wasm_bindgen(getter)]
    pub fn processed_count(&self) -> u64 {
        self.inner.processed_count
    }

    #[wasm_bindgen(getter)]
    pub fn last_receipt(&self) -> Vec<u8> {
        self.inner.last_receipt.to_vec()
    }

    #[wasm_bindgen]
    pub fn advance(
        &mut self,
        act: &WasmConstruct8,
        expected_receipt_sig: &[u8],
    ) -> Result<(), JsValue> {
        if expected_receipt_sig.len() != 32 {
            return Err(JsValue::from_str(
                "expected_receipt signature must be exactly 32 bytes",
            ));
        }
        let mut sig = [0u8; 32];
        sig.copy_from_slice(expected_receipt_sig);
        let expected_receipt = Receipt { signature: sig };

        self.inner
            .advance(&act.inner, &expected_receipt)
            .map_err(|e| JsValue::from_str(&format!("Replay check refused: {:?}", e.reason)))
    }
}
