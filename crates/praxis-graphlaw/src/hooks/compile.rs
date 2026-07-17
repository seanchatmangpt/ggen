// Hook scheduling and compilation (PROJ-403)

use crate::fastmap::FxHashMap;

use super::{CompiledHook, HookId, KnowledgeHook};

/// Schedules CompiledHooks using Kahn's algorithm on HookId edges.
///
/// # Algorithm
/// Topological sort using Kahn's algorithm with tie-breaking by (priority, HookId).
/// - Deterministic: same input order and priorities → same schedule every time
/// - Tie-break: (priority ASC, HookId ASC) ensures stable ordering
/// - No string comparisons; all edges are HookId indices (O(1) lookup)
///
/// # Complexity
/// O(|H| + |D|) where |H| = hooks, |D| = total dependencies
/// - In_degree computation: O(|D|)
/// - Kahn's loop: O(|H|) iterations, each with O(log |H|) sort
/// - Total: O(|H| log |H| + |D|)
///
/// # Errors
/// Returns `Err(String)` if a cycle is detected (scheduled.len() < hooks.len())
pub fn schedule_hooks(hooks: &[CompiledHook]) -> Result<Vec<CompiledHook>, String> {
    // Build hook_id → hook reference map
    let mut hook_map = FxHashMap::default();
    let mut in_degree: FxHashMap<HookId, usize> = FxHashMap::default();
    let mut adj: FxHashMap<HookId, Vec<HookId>> = FxHashMap::default();

    for hook in hooks {
        hook_map.insert(hook.id, hook.clone());
        in_degree.insert(hook.id, 0);
        adj.insert(hook.id, Vec::new());
    }

    // Build adjacency list and in-degree counters
    for hook in hooks {
        for &dep_id in &hook.after {
            if !hook_map.contains_key(&dep_id) {
                return Err(format!(
                    "hook '{}' has unknown after-dependency 'HookId({})'",
                    hook.iri, dep_id.0
                ));
            }
            adj.get_mut(&dep_id)
                .ok_or_else(|| format!("internal error: adj missing {}", dep_id.0))?
                .push(hook.id);
            *in_degree
                .get_mut(&hook.id)
                .ok_or_else(|| format!("internal error: in_degree missing {}", hook.id.0))? += 1;
        }
    }

    // Initialize queue with zero in-degree hooks
    let mut queue = Vec::new();
    for (&hook_id, &deg) in &in_degree {
        if deg == 0 {
            queue.push(
                hook_map
                    .get(&hook_id)
                    .ok_or_else(|| format!("internal error: hook missing {}", hook_id.0))?
                    .clone(),
            );
        }
    }

    // Kahn's algorithm with tie-breaking by (priority, HookId)
    let mut scheduled = Vec::new();
    while !queue.is_empty() {
        queue.sort_unstable_by(|a, b| (a.priority, a.id).cmp(&(b.priority, b.id)));
        let next = queue.remove(0);
        scheduled.push(next.clone());

        // Decrement in-degree for neighbors
        for &neighbor_id in adj
            .get(&next.id)
            .ok_or_else(|| format!("internal error: adj missing {}", next.id.0))?
        {
            let deg = in_degree
                .get_mut(&neighbor_id)
                .ok_or_else(|| format!("internal error: deg missing {}", neighbor_id.0))?;
            *deg -= 1;
            if *deg == 0 {
                queue.push(
                    hook_map
                        .get(&neighbor_id)
                        .ok_or_else(|| format!("internal error: hook missing {}", neighbor_id.0))?
                        .clone(),
                );
            }
        }
    }

    // Cycle detection: if not all hooks were scheduled, there's a cycle
    if scheduled.len() < hooks.len() {
        return Err("dependency cycle detected in hooks".to_string());
    }

    Ok(scheduled)
}

/// Compiles KnowledgeHooks to CompiledHooks with ID-based dependency tracking.
///
/// # Algorithm
/// 1. Assign HookId by position (deterministic: input order → HookId order)
/// 2. Track unique 'on' values; assign EventId:
///    - If all hooks share same 'on' value: single shared EventId
///    - Else: per-hook EventId in order of first appearance
/// 3. Resolve 'after' string IRIs to HookId indices; error on unknown IRI
/// 4. Return Vec<CompiledHook> in input order
///
/// # Complexity
/// O(|H| + |D|) where |H| = number of hooks, |D| = total dependencies
/// (dominated by dependency resolution loop)
///
/// # Errors
/// Returns `Err(String)` if any hook references unknown IRI in 'after' field
pub fn compile_hooks(hooks: Vec<KnowledgeHook>) -> Result<Vec<CompiledHook>, String> {
    // Build hook IRI → position mapping for dependency resolution
    let mut iri_to_id = FxHashMap::default();
    for (idx, hook) in hooks.iter().enumerate() {
        iri_to_id.insert(hook.iri.clone(), super::HookId(idx as u32));
    }

    // Determine EventId assignment strategy: all same 'on' value → shared EventId
    let all_same_on = !hooks.is_empty() && hooks.iter().all(|h| h.on == hooks[0].on);

    let mut on_to_event_id = FxHashMap::default();
    let mut next_event_id = 0u32;

    // Pre-assign all EventIds before moving hooks
    for hook in &hooks {
        if all_same_on {
            on_to_event_id.insert(hook.on.clone(), super::EventId(0));
        } else if !on_to_event_id.contains_key(&hook.on) {
            on_to_event_id.insert(hook.on.clone(), super::EventId(next_event_id));
            next_event_id += 1;
        }
    }

    // Compile each hook
    let mut compiled = Vec::with_capacity(hooks.len());
    for hook in hooks {
        // Resolve 'after' dependencies
        let mut after_ids = smallvec::SmallVec::new();
        for iri in &hook.after {
            match iri_to_id.get(iri) {
                Some(&id) => after_ids.push(id),
                None => {
                    return Err(format!(
                        "hook '{}' has unknown after-dependency '{}'",
                        hook.iri, iri
                    ));
                }
            }
        }

        let event_id = on_to_event_id
            .get(&hook.on)
            .copied()
            .ok_or_else(|| format!("missing EventId for on value '{}'", hook.on))?;

        let id = iri_to_id
            .get(&hook.iri)
            .copied()
            .ok_or_else(|| format!("missing HookId for IRI '{}'", hook.iri))?;

        compiled.push(CompiledHook {
            id,
            iri: hook.iri,
            name: hook.name,
            event: event_id,
            on: hook.on,
            condition: hook.condition,
            effect: hook.effect,
            action: hook.action,
            reason: hook.reason,
            priority: hook.priority,
            after: after_ids,
        });
    }

    Ok(compiled)
}
