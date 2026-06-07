use crate::descriptor::PackDescriptor;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, thiserror::Error)]
#[error("Cycle detected in dependencies involving {0}")]
pub struct DependencyCycleError(pub String);

#[derive(Debug, thiserror::Error)]
#[error("Dependency not found: {0}")]
pub struct DependencyNotFoundError(pub String);

#[derive(Debug, thiserror::Error)]
#[error("Incompatible version conflict for dependency {dep_id} of {pack_id}: constraint {constraint}, found version {found_version}")]
pub struct VersionConflictError {
    pub pack_id: String,
    pub dep_id: String,
    pub constraint: String,
    pub found_version: String,
}

pub fn is_compatible(version: &str, constraint: &str) -> bool {
    let constraint = constraint.trim();
    if constraint == "*" || constraint.is_empty() {
        return true;
    }

    // Parse version into major, minor, patch
    let parse_ver = |v: &str| -> Option<(u64, u64, u64)> {
        let parts: Vec<&str> = v.split('.').collect();
        let major = parts.first().cloned().unwrap_or("0").parse().ok()?;
        let minor = parts.get(1).cloned().unwrap_or("0").parse().ok()?;
        let patch = parts.get(2).cloned().unwrap_or("0").parse().ok()?;
        Some((major, minor, patch))
    };

    let Some((v_maj, v_min, v_pat)) = parse_ver(version) else {
        return false;
    };

    if let Some(c_ver) = constraint.strip_prefix('^') {
        let Some((c_maj, c_min, c_pat)) = parse_ver(c_ver) else {
            return false;
        };
        if v_maj != c_maj {
            return false;
        }
        if v_maj == 0 {
            if v_min != c_min {
                return false;
            }
            if v_min == 0 {
                return v_pat == c_pat;
            }
            return v_pat >= c_pat;
        }
        if v_min < c_min {
            return false;
        }
        if v_min == c_min && v_pat < c_pat {
            return false;
        }
        true
    } else if let Some(c_ver) = constraint.strip_prefix(">=") {
        let Some((c_maj, c_min, c_pat)) = parse_ver(c_ver.trim()) else {
            return false;
        };
        if v_maj > c_maj {
            return true;
        }
        if v_maj < c_maj {
            return false;
        }
        if v_min > c_min {
            return true;
        }
        if v_min < c_min {
            return false;
        }
        v_pat >= c_pat
    } else if let Some(c_ver) = constraint.strip_prefix("<=") {
        let Some((c_maj, c_min, c_pat)) = parse_ver(c_ver.trim()) else {
            return false;
        };
        if v_maj < c_maj {
            return true;
        }
        if v_maj > c_maj {
            return false;
        }
        if v_min < c_min {
            return true;
        }
        if v_min > c_min {
            return false;
        }
        v_pat <= c_pat
    } else {
        let clean_c = constraint.strip_prefix('=').unwrap_or(constraint).trim();
        let Some((c_maj, c_min, c_pat)) = parse_ver(clean_c) else {
            return false;
        };
        v_maj == c_maj && v_min == c_min && v_pat == c_pat
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PackPlan {
    pub target_profile: String,
    pub packs: Vec<PackDescriptor>,
    pub resolution_order: Vec<String>, // Topologically sorted pack IDs
    pub checksums: HashMap<String, String>, // Pack ID -> Blake3 hash
}

impl PackPlan {
    pub fn resolve(descriptors: &[PackDescriptor]) -> Result<Self, anyhow::Error> {
        let mut sorted = Vec::new();
        let mut visited = HashMap::new(); // id -> visited_state (0 = visiting, 1 = visited)

        let descriptor_map: HashMap<String, &PackDescriptor> =
            descriptors.iter().map(|d| (d.id.clone(), d)).collect();

        fn visit(
            id: &str, descriptor_map: &HashMap<String, &PackDescriptor>,
            visited: &mut HashMap<String, usize>, sorted: &mut Vec<String>,
        ) -> Result<(), anyhow::Error> {
            match visited.get(id) {
                Some(&0) => return Err(DependencyCycleError(id.to_string()).into()),
                Some(&1) => return Ok(()),
                _ => {}
            }

            visited.insert(id.to_string(), 0);

            if let Some(desc) = descriptor_map.get(id) {
                for (dep_id, constraint) in &desc.dependencies {
                    if let Some(dep_desc) = descriptor_map.get(dep_id) {
                        if !is_compatible(&dep_desc.version, constraint) {
                            return Err(VersionConflictError {
                                pack_id: id.to_string(),
                                dep_id: dep_id.clone(),
                                constraint: constraint.clone(),
                                found_version: dep_desc.version.clone(),
                            }
                            .into());
                        }
                    } else {
                        return Err(DependencyNotFoundError(dep_id.clone()).into());
                    }
                    visit(dep_id, descriptor_map, visited, sorted)?;
                }
            } else {
                return Err(DependencyNotFoundError(id.to_string()).into());
            }

            visited.insert(id.to_string(), 1);
            sorted.push(id.to_string());
            Ok(())
        }

        for desc in descriptors {
            visit(&desc.id, &descriptor_map, &mut visited, &mut sorted)?;
        }

        let mut checksums = HashMap::new();
        for desc in descriptors {
            let serialized = toml::to_string(desc)?;
            let hash = blake3::hash(serialized.as_bytes()).to_hex().to_string();
            checksums.insert(desc.id.clone(), hash);
        }

        let mut packs_in_order = Vec::new();
        for id in &sorted {
            if let Some(desc) = descriptor_map.get(id) {
                packs_in_order.push((*desc).clone());
            }
        }

        Ok(PackPlan {
            target_profile: "default".to_string(),
            packs: packs_in_order,
            resolution_order: sorted,
            checksums,
        })
    }
}
