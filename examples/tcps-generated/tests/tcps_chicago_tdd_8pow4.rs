//! TCPS Auto Select 8^4 behavioral fabric.
//!
//! The previous 8^4 implementation assigned arbitrary files to synthetic cells and
//! treated file readability as proof of every interaction. This replacement executes
//! the real TCPS Auto Select and Blue River Dam APIs across four independent eight-
//! state input factors:
//!
//!     8 authority masks × 8 readiness masks × 8 time budgets × 8 request modes
//!     = 4,096 real selection judgments.
//!
//! Every result is checked against an independent public-contract oracle. Selected
//! proposals are then passed through the real authorization boundary. No arbitrary
//! witness assignment, fake OCEL, or existence-only discharge remains.

#[path = "support/common.rs"]
mod common;
#[path = "support/sharding.rs"]
mod sharding;

use std::collections::BTreeSet;
use std::sync::OnceLock;

use common::{domain_digest, merkle_root, serialize_json, EvidenceDigest, EvidenceError};
use serde::Serialize;
use serde_json::Value as JsonValue;
use sha2::{Digest as _, Sha256};
use sharding::rendezvous_shard;
use tcps_generated::受領証_impl::{受領種別, 受領証, 要約値};
use tcps_generated::自動選択::{
    候補, 作業領域, 拒否理由, 方策, 測度, 認知品種, 選択する, 選択提案, 選択結果,
    選択要求,
};
use tcps_generated::語彙::{成否, 有無};
use tcps_generated::青い川のダム::{仲介者, 能力札, 許可拒否};

include!("auto_select_8pow4/model.rs");
include!("auto_select_8pow4/geometry.rs");
include!("auto_select_8pow4/policy.rs");
include!("auto_select_8pow4/matrix.rs");
include!("auto_select_8pow4/tests.rs");
