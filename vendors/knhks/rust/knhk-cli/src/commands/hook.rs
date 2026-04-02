// rust/knhk-cli/src/commands/hook.rs
// Hook commands - Knowledge hook operations using knhk-hot FFI

use std::fs;
use std::path::PathBuf;
use serde::{Deserialize, Serialize};

/// Hook storage entry
#[derive(Debug, Serialize, Deserialize)]
struct HookEntry {
    id: String,
    name: String,
    op: String,
    pred: u64,
    off: u64,
    len: u64,
    s: Option<u64>,
    p: Option<u64>,
    o: Option<u64>,
    k: Option<u64>,
}

/// Hook storage
#[derive(Debug, Serialize, Deserialize)]
struct HookStorage {
    hooks: Vec<HookEntry>,
}

/// Create a hook
/// hook(#{name, op, run := #{pred, off, len}, args})
pub fn create(
    name: String,
    op: String,
    pred: u64,
    off: u64,
    len: u64,
    s: Option<u64>,
    p: Option<u64>,
    o: Option<u64>,
    k: Option<u64>,
) -> Result<(), String> {
    println!("Creating hook: {}", name);
    println!("  Operation: {}", op);
    println!("  Run: pred={}, off={}, len={}", pred, off, len);
    
    // Validate run length ≤ 8
    if len > 8 {
        return Err(format!("Run length {} exceeds max_run_len 8", len));
    }
    
    // Validate operation
    let valid_ops = [
        "ASK_SP", "ASK_SPO", "COUNT_SP_GE", "COUNT_SP_EQ", "COUNT_SP_LE",
        "COUNT_O_P_GE", "COUNT_O_P_EQ", "COUNT_O_P_LE", "SELECT_SP",
        "VALIDATE_SP", "COMPARE_O", "CONSTRUCT8"
    ];
    let op_upper = op.to_uppercase();
    if !valid_ops.iter().any(|&o| o == op_upper) {
        return Err(format!("Invalid operation: {}. Must be one of: {:?}", op, valid_ops));
    }
    
    // Load existing hooks
    let mut storage = load_hooks()?;
    
    // Check if hook with same name exists
    if storage.hooks.iter().any(|h| h.name == name) {
        return Err(format!("Hook with name '{}' already exists", name));
    }
    
    // Create hook entry
    let hook_id = format!("hook_{}", storage.hooks.len() + 1);
    storage.hooks.push(HookEntry {
        id: hook_id.clone(),
        name: name.clone(),
        op: op.clone(),
        pred,
        off,
        len,
        s,
        p,
        o,
        k,
    });
    
    // Save hooks
    save_hooks(&storage)?;
    
    println!("✓ Hook created (id: {})", hook_id);
    
    Ok(())
}

/// List hooks
pub fn list() -> Result<(), String> {
    let storage = load_hooks()?;
    
    if storage.hooks.is_empty() {
        println!("No hooks created");
        return Ok(());
    }
    
    println!("Created hooks:");
    for (i, hook) in storage.hooks.iter().enumerate() {
        println!("  {}. {} (id: {})", i + 1, hook.name, hook.id);
        println!("     Operation: {}", hook.op);
        println!("     Run: pred={}, off={}, len={}", hook.pred, hook.off, hook.len);
        if let Some(s) = hook.s {
            println!("     S: 0x{:016x}", s);
        }
        if let Some(p) = hook.p {
            println!("     P: 0x{:016x}", p);
        }
        if let Some(o) = hook.o {
            println!("     O: 0x{:016x}", o);
        }
        if let Some(k) = hook.k {
            println!("     K: {}", k);
        }
    }
    
    Ok(())
}

/// Evaluate a hook using knhk-hot FFI
pub fn eval(hook_name: String) -> Result<(), String> {
    println!("Evaluating hook: {}", hook_name);
    
    // Load hooks
    let storage = load_hooks()?;
    
    // Find hook
    let hook = storage.hooks.iter()
        .find(|h| h.name == hook_name)
        .ok_or_else(|| format!("Hook '{}' not found", hook_name))?;
    
    #[cfg(feature = "std")]
    {
        use knhk_hot::{Engine, Op, Ir, Receipt as HotReceipt, Run as HotRun};
        
        // Create dummy SoA arrays for evaluation
        // In production, these would come from loaded ontology O
        let mut s_array = [0u64; 8];
        let mut p_array = [0u64; 8];
        let mut o_array = [0u64; 8];
        
        // Initialize arrays with hook values
        if hook.len > 0 {
            s_array[hook.off as usize] = hook.s.unwrap_or(0);
            p_array[hook.off as usize] = hook.pred;
            o_array[hook.off as usize] = hook.o.unwrap_or(0);
        }
        
        // Initialize engine
        let mut engine = Engine::new(
            s_array.as_ptr(),
            p_array.as_ptr(),
            o_array.as_ptr(),
        );
        
        // Pin run
        let hot_run = HotRun {
            pred: hook.pred,
            off: hook.off,
            len: hook.len,
        };
        
        match engine.pin_run(hot_run) {
            Ok(_) => println!("  ✓ Run pinned"),
            Err(e) => return Err(format!("Failed to pin run: {}", e)),
        }
        
        // Parse operation
        let op = match hook.op.to_uppercase().as_str() {
            "ASK_SP" => Op::AskSp,
            "ASK_SPO" => Op::AskSpo,
            "COUNT_SP_GE" => Op::CountSpGe,
            "COUNT_SP_EQ" => Op::CountSpEq,
            "COUNT_SP_LE" => Op::CountSpLe,
            "ASK_OP" => Op::AskOp,
            "UNIQUE_SP" => Op::UniqueSp,
            "COUNT_OP_GE" => Op::CountOpGe,
            "COUNT_OP_LE" => Op::CountOpLe,
            "COUNT_OP_EQ" => Op::CountOpEq,
            "COMPARE_O_EQ" => Op::CompareOEQ,
            "COMPARE_O_GT" => Op::CompareOGT,
            "COMPARE_O_LT" => Op::CompareOLT,
            "COMPARE_O_GE" => Op::CompareOGE,
            "COMPARE_O_LE" => Op::CompareOLE,
            "CONSTRUCT8" => Op::Construct8,
            _ => return Err(format!("Unsupported operation: {}", hook.op)),
        };
        
        // Create IR
        let mut ir = Ir {
            op,
            s: hook.s.unwrap_or(0),
            p: hook.pred,
            o: hook.o.unwrap_or(0),
            k: hook.k.unwrap_or(0),
            out_S: std::ptr::null_mut(),
            out_P: std::ptr::null_mut(),
            out_O: std::ptr::null_mut(),
            out_mask: 0,
        };
        
        // Execute hook
        let mut receipt = HotReceipt::default();
        
        if op == Op::Construct8 {
            let lanes_written = engine.eval_construct8(&mut ir, &mut receipt);
            println!("  ✓ Hook executed (lanes written: {})", lanes_written);
        } else {
            let result = engine.eval_bool(&mut ir, &mut receipt);
            println!("  ✓ Hook executed (result: {})", result);
        }
        
        println!("  Receipt:");
        println!("    Ticks: {} (budget: ≤8)", receipt.ticks);
        if receipt.ticks > 8 {
            println!("    ⚠ Tick budget exceeded!");
        } else {
            println!("    ✓ Within budget");
        }
        println!("    Lanes: {}", receipt.lanes);
        println!("    Span ID: 0x{:016x}", receipt.span_id);
        println!("    A hash: 0x{:016x}", receipt.a_hash);
    }
    
    #[cfg(not(feature = "std"))]
    {
        return Err("Hook evaluation requires std feature".to_string());
    }
    
    println!("✓ Hook evaluated");
    
    Ok(())
}

/// Show hook details
pub fn show(hook_name: String) -> Result<(), String> {
    let storage = load_hooks()?;
    
    let hook = storage.hooks.iter()
        .find(|h| h.name == hook_name)
        .ok_or_else(|| format!("Hook '{}' not found", hook_name))?;
    
    println!("Hook: {}", hook.name);
    println!("  ID: {}", hook.id);
    println!("  Operation: {}", hook.op);
    println!("  Run:");
    println!("    Predicate: 0x{:016x}", hook.pred);
    println!("    Offset: {}", hook.off);
    println!("    Length: {} (max: 8)", hook.len);
    if let Some(s) = hook.s {
        println!("  Subject: 0x{:016x}", s);
    }
    if let Some(p) = hook.p {
        println!("  Predicate (IR): 0x{:016x}", p);
    }
    if let Some(o) = hook.o {
        println!("  Object: 0x{:016x}", o);
    }
    if let Some(k) = hook.k {
        println!("  K (threshold): {}", k);
    }
    
    Ok(())
}

fn get_config_dir() -> Result<PathBuf, String> {
    #[cfg(target_os = "windows")]
    {
        let mut path = PathBuf::from(std::env::var("APPDATA").map_err(|_| "APPDATA not set")?);
        path.push("knhk");
        Ok(path)
    }
    
    #[cfg(not(target_os = "windows"))]
    {
        let home = std::env::var("HOME").map_err(|_| "HOME not set")?;
        let mut path = PathBuf::from(home);
        path.push(".knhk");
        Ok(path)
    }
}

fn load_hooks() -> Result<HookStorage, String> {
    let config_dir = get_config_dir()?;
    let hooks_file = config_dir.join("hooks.json");
    
    if !hooks_file.exists() {
        return Ok(HookStorage {
            hooks: Vec::new(),
        });
    }
    
    let content = fs::read_to_string(&hooks_file)
        .map_err(|e| format!("Failed to read hooks file: {}", e))?;
    
    let storage: HookStorage = serde_json::from_str(&content)
        .map_err(|e| format!("Failed to parse hooks file: {}", e))?;
    
    Ok(storage)
}

fn save_hooks(storage: &HookStorage) -> Result<(), String> {
    let config_dir = get_config_dir()?;
    fs::create_dir_all(&config_dir)
        .map_err(|e| format!("Failed to create config directory: {}", e))?;
    
    let hooks_file = config_dir.join("hooks.json");
    let content = serde_json::to_string_pretty(storage)
        .map_err(|e| format!("Failed to serialize hooks: {}", e))?;
    
    fs::write(&hooks_file, content)
        .map_err(|e| format!("Failed to write hooks file: {}", e))?;
    
    Ok(())
}

