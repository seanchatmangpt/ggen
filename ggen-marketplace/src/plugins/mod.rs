// WebAssembly Plugin System using wasmtime
use anyhow::{Result, Context, anyhow};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;
use tokio::sync::RwLock;
use wasmtime::*;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PluginMetadata {
    pub name: String,
    pub version: String,
    pub author: String,
    pub description: String,
    pub capabilities: Vec<PluginCapability>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum PluginCapability {
    SearchRanking,
    PackageValidation,
    CustomProtocol,
    DataTransform,
    SecurityScan,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PluginConfig {
    pub enabled: bool,
    pub priority: i32,
    pub settings: HashMap<String, String>,
}

pub struct PluginManager {
    engine: Engine,
    plugins: Arc<RwLock<HashMap<String, LoadedPlugin>>>,
    configs: Arc<RwLock<HashMap<String, PluginConfig>>>,
}

struct LoadedPlugin {
    module: Module,
    metadata: PluginMetadata,
    store: Store<PluginState>,
    instance: Instance,
}

struct PluginState {
    memory: Option<Memory>,
    data: HashMap<String, Vec<u8>>,
}

impl PluginState {
    fn new() -> Self {
        Self {
            memory: None,
            data: HashMap::new(),
        }
    }
}

impl PluginManager {
    pub fn new() -> Result<Self> {
        let mut config = Config::new();
        config.wasm_simd(true);
        config.wasm_bulk_memory(true);
        config.cranelift_opt_level(OptLevel::Speed);

        let engine = Engine::new(&config)?;

        Ok(Self {
            engine,
            plugins: Arc::new(RwLock::new(HashMap::new())),
            configs: Arc::new(RwLock::new(HashMap::new())),
        })
    }

    /// Load a plugin from WebAssembly binary
    pub async fn load_plugin(&self, name: String, wasm_bytes: &[u8]) -> Result<()> {
        let module = Module::new(&self.engine, wasm_bytes)
            .context("Failed to compile WASM module")?;

        // Create plugin state
        let mut store = Store::new(&self.engine, PluginState::new());

        // Define host functions available to plugins
        let mut linker = Linker::new(&self.engine);
        self.define_host_functions(&mut linker)?;

        // Instantiate the module
        let instance = linker.instantiate(&mut store, &module)
            .context("Failed to instantiate WASM module")?;

        // Get plugin metadata
        let metadata = self.extract_metadata(&instance, &mut store).await?;

        let loaded_plugin = LoadedPlugin {
            module,
            metadata: metadata.clone(),
            store,
            instance,
        };

        let mut plugins = self.plugins.write().await;
        plugins.insert(name.clone(), loaded_plugin);

        // Set default config
        let mut configs = self.configs.write().await;
        configs.insert(name, PluginConfig {
            enabled: true,
            priority: 0,
            settings: HashMap::new(),
        });

        Ok(())
    }

    /// Define host functions that plugins can call
    fn define_host_functions(&self, linker: &mut Linker<PluginState>) -> Result<()> {
        // Log function
        linker.func_wrap("env", "log", |caller: Caller<'_, PluginState>, ptr: i32, len: i32| {
            if let Some(memory) = caller.data().memory {
                let data = memory.data(&caller);
                if let Ok(slice) = Self::read_memory(data, ptr as usize, len as usize) {
                    if let Ok(message) = std::str::from_utf8(slice) {
                        println!("[Plugin] {}", message);
                    }
                }
            }
        })?;

        // Store data function
        linker.func_wrap("env", "store_data",
            |mut caller: Caller<'_, PluginState>, key_ptr: i32, key_len: i32, val_ptr: i32, val_len: i32| -> i32 {
                if let Some(memory) = caller.data().memory {
                    let data = memory.data(&caller);

                    if let (Ok(key_slice), Ok(val_slice)) = (
                        Self::read_memory(data, key_ptr as usize, key_len as usize),
                        Self::read_memory(data, val_ptr as usize, val_len as usize),
                    ) {
                        if let Ok(key) = std::str::from_utf8(key_slice) {
                            caller.data_mut().data.insert(key.to_string(), val_slice.to_vec());
                            return 0; // Success
                        }
                    }
                }
                -1 // Error
            }
        )?;

        Ok(())
    }

    fn read_memory(memory: &[u8], offset: usize, len: usize) -> Result<&[u8]> {
        memory.get(offset..offset + len)
            .ok_or_else(|| anyhow!("Memory access out of bounds"))
    }

    /// Extract plugin metadata
    async fn extract_metadata(&self, instance: &Instance, store: &mut Store<PluginState>) -> Result<PluginMetadata> {
        // Try to call get_metadata function if it exists
        if let Ok(get_metadata) = instance.get_typed_func::<(), i32>(store, "get_metadata") {
            let ptr = get_metadata.call(store, ())?;

            // Read metadata from memory
            if let Some(memory) = instance.get_memory(store, "memory") {
                store.data_mut().memory = Some(memory);
                // In production, would read and parse JSON metadata from memory
            }
        }

        // Return default metadata for now
        Ok(PluginMetadata {
            name: "Unknown".to_string(),
            version: "0.1.0".to_string(),
            author: "Unknown".to_string(),
            description: "No description".to_string(),
            capabilities: vec![],
        })
    }

    /// Call a plugin function
    pub async fn call_plugin<P: Serialize, R: for<'de> Deserialize<'de>>(
        &self,
        plugin_name: &str,
        function_name: &str,
        params: P,
    ) -> Result<R> {
        let mut plugins = self.plugins.write().await;
        let plugin = plugins.get_mut(plugin_name)
            .ok_or_else(|| anyhow!("Plugin not found: {}", plugin_name))?;

        // Check if plugin is enabled
        let configs = self.configs.read().await;
        let config = configs.get(plugin_name)
            .ok_or_else(|| anyhow!("Plugin config not found"))?;

        if !config.enabled {
            return Err(anyhow!("Plugin is disabled"));
        }

        // Serialize parameters to JSON
        let params_json = serde_json::to_vec(&params)?;

        // Allocate memory in WASM for parameters
        let alloc_fn = plugin.instance
            .get_typed_func::<i32, i32>(&mut plugin.store, "alloc")
            .context("Plugin missing alloc function")?;

        let params_ptr = alloc_fn.call(&mut plugin.store, params_json.len() as i32)?;

        // Write parameters to WASM memory
        if let Some(memory) = plugin.instance.get_memory(&mut plugin.store, "memory") {
            memory.write(&mut plugin.store, params_ptr as usize, &params_json)?;
        }

        // Call the plugin function
        let plugin_fn = plugin.instance
            .get_typed_func::<(i32, i32), i32>(&mut plugin.store, function_name)
            .context(format!("Plugin function not found: {}", function_name))?;

        let result_ptr = plugin_fn.call(&mut plugin.store, (params_ptr, params_json.len() as i32))?;

        // Read result from WASM memory
        let memory = plugin.instance.get_memory(&mut plugin.store, "memory")
            .ok_or_else(|| anyhow!("Plugin memory not found"))?;

        let memory_data = memory.data(&plugin.store);

        // Read result length (assuming first 4 bytes contain length)
        let result_len = i32::from_le_bytes([
            memory_data[result_ptr as usize],
            memory_data[result_ptr as usize + 1],
            memory_data[result_ptr as usize + 2],
            memory_data[result_ptr as usize + 3],
        ]) as usize;

        let result_data = &memory_data[result_ptr as usize + 4..result_ptr as usize + 4 + result_len];

        // Deserialize result
        let result: R = serde_json::from_slice(result_data)?;

        Ok(result)
    }

    /// Enable or disable a plugin
    pub async fn set_plugin_enabled(&self, plugin_name: &str, enabled: bool) -> Result<()> {
        let mut configs = self.configs.write().await;
        let config = configs.get_mut(plugin_name)
            .ok_or_else(|| anyhow!("Plugin not found"))?;

        config.enabled = enabled;
        Ok(())
    }

    /// Set plugin priority (higher priority plugins run first)
    pub async fn set_plugin_priority(&self, plugin_name: &str, priority: i32) -> Result<()> {
        let mut configs = self.configs.write().await;
        let config = configs.get_mut(plugin_name)
            .ok_or_else(|| anyhow!("Plugin not found"))?;

        config.priority = priority;
        Ok(())
    }

    /// List all loaded plugins
    pub async fn list_plugins(&self) -> Vec<(String, PluginMetadata, PluginConfig)> {
        let plugins = self.plugins.read().await;
        let configs = self.configs.read().await;

        plugins.iter()
            .filter_map(|(name, plugin)| {
                configs.get(name).map(|config| {
                    (name.clone(), plugin.metadata.clone(), config.clone())
                })
            })
            .collect()
    }

    /// Unload a plugin
    pub async fn unload_plugin(&self, plugin_name: &str) -> Result<()> {
        let mut plugins = self.plugins.write().await;
        plugins.remove(plugin_name)
            .ok_or_else(|| anyhow!("Plugin not found"))?;

        let mut configs = self.configs.write().await;
        configs.remove(plugin_name);

        Ok(())
    }

    /// Call all plugins with a specific capability
    pub async fn call_plugins_by_capability<P: Serialize + Clone, R: for<'de> Deserialize<'de>>(
        &self,
        capability: PluginCapability,
        function_name: &str,
        params: P,
    ) -> Vec<(String, Result<R>)> {
        let plugin_list = self.list_plugins().await;
        let mut results = Vec::new();

        for (name, metadata, config) in plugin_list {
            if config.enabled && metadata.capabilities.contains(&capability) {
                let result = self.call_plugin(&name, function_name, params.clone()).await;
                results.push((name, result));
            }
        }

        // Sort by priority
        results.sort_by_key(|(name, _)| {
            let configs = futures::executor::block_on(self.configs.read());
            configs.get(name).map(|c| -c.priority).unwrap_or(0)
        });

        results
    }
}

impl Default for PluginManager {
    fn default() -> Self {
        Self::new().expect("Failed to create plugin manager")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_plugin_manager_creation() {
        let manager = PluginManager::new().unwrap();
        let plugins = manager.list_plugins().await;
        assert_eq!(plugins.len(), 0);
    }

    #[test]
    fn test_plugin_metadata_serialization() {
        let metadata = PluginMetadata {
            name: "test-plugin".to_string(),
            version: "1.0.0".to_string(),
            author: "Test Author".to_string(),
            description: "A test plugin".to_string(),
            capabilities: vec![PluginCapability::SearchRanking],
        };

        let json = serde_json::to_string(&metadata).unwrap();
        let deserialized: PluginMetadata = serde_json::from_str(&json).unwrap();

        assert_eq!(metadata.name, deserialized.name);
    }
}
