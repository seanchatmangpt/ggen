<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Chapter 17.3: Foreign Function Interface](#chapter-173-foreign-function-interface)
  - [Context](#context)
  - [Problem](#problem)
  - [Forces](#forces)
  - [Solution](#solution)
  - [Diagram](#diagram)
  - [Implementation](#implementation)
    - [FFI Type System](#ffi-type-system)
    - [Python Integration (PyO3)](#python-integration-pyo3)
    - [JavaScript Integration (QuickJS)](#javascript-integration-quickjs)
    - [C Library Integration (bindgen)](#c-library-integration-bindgen)
    - [Async FFI Support](#async-ffi-support)
  - [Result](#result)
    - [Benefits Achieved](#benefits-achieved)
    - [Related Patterns](#related-patterns)
    - [Example: Multi-Language Code Analysis Pipeline](#example-multi-language-code-analysis-pipeline)
  - [Verification](#verification)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Chapter 17.3: Foreign Function Interface

## Context

GGen needs to interact with external systems, tools, and libraries. Traditional approaches either tightly couple GGen to specific tools or create complex bridging code that breaks abstraction boundaries.

## Problem

**How can GGen safely and efficiently call functions from other programming languages and external tools?**

## Forces

- **Language Diversity**: Users need to integrate with tools written in Python, JavaScript, Go, etc.
- **Performance**: FFI calls should not significantly slow down generation
- **Safety**: External code must not compromise GGen's memory safety or determinism
- **Debuggability**: FFI issues should be easy to diagnose and fix
- **Portability**: FFI should work across different platforms and architectures
- **Maintainability**: FFI interfaces should be stable and well-documented

## Solution

**Implement a safe, efficient FFI layer with automatic memory management, type conversion, and error handling.**

Create an FFI system that:

1. **Provides type-safe bindings** with compile-time guarantees
2. **Manages memory automatically** to prevent leaks and corruption
3. **Converts data types seamlessly** between Rust and foreign languages
4. **Handles errors gracefully** with proper error propagation
5. **Supports async operations** for non-blocking external calls

## Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                    GGen Core Engine                         │
│  ┌─────────────────────────────────────────────────────┐   │
│  │                 FFI Layer                          │   │
│  │  ┌─────────────────────────────────────────────┐   │   │
│  │  │            Type Bridge                     │   │   │
│  │  │  ┌─────────────────────────────────────┐   │   │   │
│  │  │  │   Rust Types  ↔  Foreign Types     │   │   │   │
│  │  │  └─────────────────────────────────────┘   │   │   │
│  │  └─────────────────────────────────────────────┘   │   │
│  │  ┌─────────────────────────────────────────────┐   │   │
│  │  │           Memory Manager                   │   │   │
│  │  │  • Automatic cleanup                   │   │   │   │
│  │  │  • Reference counting                  │   │   │   │
│  │  │  • Leak detection                      │   │   │   │
│  │  └─────────────────────────────────────────────┘   │   │
│  └─────────────────────────────────────────────────────┘   │
│  ┌─────────────────────────────────────────────────────┐   │
│  │              External Libraries                     │   │
│  │  • Python modules (via PyO3)                       │   │
│  │  • JavaScript engines (via QuickJS)                │   │
│  │  • C libraries (via bindgen)                       │   │
│  │  • Go programs (via RPC)                           │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

## Implementation

### FFI Type System

```rust
use std::ffi::{CStr, CString};
use serde::{Serialize, Deserialize};

/// Safe wrapper for foreign function calls
pub struct FFISafe<T> {
    value: T,
    _phantom: std::marker::PhantomData<*const ()>,
}

impl<T> FFISafe<T> {
    pub fn new(value: T) -> Self {
        Self {
            value,
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn into_inner(self) -> T {
        self.value
    }
}

impl FFISafe<CString> {
    /// Convert Rust string to C-compatible string
    pub fn from_string(s: String) -> Result<Self, anyhow::Error> {
        CString::new(s)
            .map(|cs| Self::new(cs))
            .map_err(|e| anyhow::anyhow!("Invalid C string: {}", e))
    }

    /// Convert C string back to Rust string
    pub fn to_string(self) -> Result<String, anyhow::Error> {
        self.value
            .to_str()
            .map(|s| s.to_string())
            .map_err(|e| anyhow::anyhow!("Invalid UTF-8: {}", e))
    }
}

/// Trait for types that can be converted to/from FFI representation
pub trait FFIConvertible: Sized {
    type FFIType;

    fn to_ffi(self) -> Self::FFIType;
    fn from_ffi(ffi_value: Self::FFIType) -> Result<Self, anyhow::Error>;
}
```

### Python Integration (PyO3)

```rust
use pyo3::prelude::*;
use pyo3::types::PyDict;

/// Python FFI bridge for calling Python functions
pub struct PythonFFI {
    py: Python,
}

impl PythonFFI {
    pub fn new() -> Result<Self, anyhow::Error> {
        Python::with_gil(|py| {
            Ok(Self { py })
        })
    }

    pub fn call_python_function(
        &self,
        module_name: &str,
        function_name: &str,
        args: Vec<FFISafe<CString>>,
    ) -> Result<String, anyhow::Error> {
        Python::with_gil(|py| {
            // Import Python module
            let module = PyModule::import(py, module_name)?;

            // Convert arguments to Python objects
            let py_args: Vec<PyObject> = args
                .into_iter()
                .map(|arg| arg.to_string().unwrap_or_default().into_py(py))
                .collect();

            // Call Python function
            let result = module.call(function_name, py_args, None)?;

            // Convert result back to Rust string
            result.extract::<String>(py)
        })
    }
}

/// Example: Python code analysis
pub fn analyze_python_code(source_code: &str) -> Result<String, anyhow::Error> {
    let python_ffi = PythonFFI::new()?;

    let args = vec![
        FFISafe::from_string(source_code.to_string())?,
    ];

    python_ffi.call_python_function(
        "ast_analyzer",
        "analyze_code",
        args,
    )
}
```

### JavaScript Integration (QuickJS)

```rust
use quickjs::{Context, Runtime, Value};

/// JavaScript FFI bridge for calling JavaScript functions
pub struct JavaScriptFFI {
    runtime: Runtime,
    context: Context,
}

impl JavaScriptFFI {
    pub fn new() -> Result<Self, anyhow::Error> {
        let runtime = Runtime::new()?;
        let context = Context::full(&runtime)?;

        Ok(Self { runtime, context })
    }

    pub fn call_js_function(
        &self,
        code: &str,
        function_name: &str,
        args: Vec<String>,
    ) -> Result<String, anyhow::Error> {
        // Execute JavaScript code
        self.context.eval_global_str(code)?;

        // Get the function
        let function = self.context.global_object()?.get_property(function_name)?;

        // Convert arguments to JS values
        let js_args: Vec<Value> = args
            .iter()
            .map(|arg| self.context.value_from_str(arg).unwrap_or_else(|_| Value::new_string(&self.context, "")))
            .collect();

        // Call the function
        let result = function.call(&self.context, &js_args)?;

        // Convert result back to Rust string
        Ok(result.as_string().unwrap_or_default())
    }
}

/// Example: JavaScript template processing
pub fn process_template_js(template: &str, data: &str) -> Result<String, anyhow::Error> {
    let js_ffi = JavaScriptFFI::new()?;

    let js_code = r#"
        function processTemplate(template, data) {
            // Use a JS template engine like Handlebars.js
            const Handlebars = require('handlebars');
            const compiled = Handlebars.compile(template);
            return compiled(JSON.parse(data));
        }
    "#;

    js_ffi.call_js_function(
        js_code,
        "processTemplate",
        vec![template.to_string(), data.to_string()],
    )
}
```

### C Library Integration (bindgen)

```rust
use std::os::raw::{c_char, c_int};
use std::ffi::CStr;

// Generated bindings (via bindgen)
extern "C" {
    fn analyze_c_code(source: *const c_char) -> *mut c_char;
    fn free_c_string(ptr: *mut c_char);
}

/// C FFI bridge for calling C libraries
pub struct CFFI;

impl CFFI {
    pub fn call_c_function(source_code: &str) -> Result<String, anyhow::Error> {
        let c_string = FFISafe::from_string(source_code.to_string())?;

        unsafe {
            // Call C function
            let result_ptr = analyze_c_code(c_string.value.as_ptr());

            if result_ptr.is_null() {
                return Err(anyhow::anyhow!("C function returned null"));
            }

            // Convert C string to Rust string
            let c_str = CStr::from_ptr(result_ptr);
            let result = c_str.to_string_lossy().into_owned();

            // Free the C string
            free_c_string(result_ptr);

            Ok(result)
        }
    }
}
```

### Async FFI Support

```rust
use tokio::task;

/// Async wrapper for FFI calls
pub async fn call_ffi_async<F, T>(
    f: F,
) -> Result<T, anyhow::Error>
where
    F: FnOnce() -> Result<T, anyhow::Error> + Send + 'static,
    T: Send + 'static,
{
    task::spawn_blocking(f).await?
}

/// Example: Async Python analysis
pub async fn analyze_python_async(source_code: &str) -> Result<String, anyhow::Error> {
    call_ffi_async(move || {
        let python_ffi = PythonFFI::new()?;
        let args = vec![FFISafe::from_string(source_code.to_string())?];
        python_ffi.call_python_function("analyzer", "analyze", args)
    }).await
}
```

## Result

**A comprehensive FFI layer that enables GGen to safely interact with external tools and libraries.**

### Benefits Achieved

- **🔒 Safety**: Memory-safe FFI with automatic cleanup
- **🚀 Performance**: Minimal overhead for FFI operations
- **🔧 Flexibility**: Support for multiple programming languages
- **📦 Maintainability**: Clean abstractions hide FFI complexity
- **🛠️ Debuggability**: Clear error messages and stack traces

### Related Patterns

- **014_fan_out_projection.md** - FFI can extend fan-out capabilities
- **021_knowledge_hooks.md** - FFI hooks can call external knowledge sources
- **024_git_as_runtime.md** - FFI can integrate with Git tools

### Example: Multi-Language Code Analysis Pipeline

```rust
use ggen_ffi::{PythonFFI, JavaScriptFFI, CFFI};

pub struct CodeAnalyzer {
    python_ffi: PythonFFI,
    js_ffi: JavaScriptFFI,
    c_ffi: CFFI,
}

impl CodeAnalyzer {
    pub fn new() -> Result<Self, anyhow::Error> {
        Ok(Self {
            python_ffi: PythonFFI::new()?,
            js_ffi: JavaScriptFFI::new()?,
            c_ffi: CFFI,
        })
    }

    pub async fn analyze_codebase(&self, files: Vec<String>) -> Result<Vec<String>, anyhow::Error> {
        let mut results = Vec::new();

        for file in files {
            let analysis = match self.detect_language(&file) {
                "python" => self.analyze_python_async(&file).await?,
                "javascript" => self.analyze_javascript_async(&file).await?,
                "c" => self.analyze_c_async(&file)?,
                _ => continue,
            };

            results.push(analysis);
        }

        Ok(results)
    }

    fn detect_language(&self, filename: &str) -> &str {
        if filename.ends_with(".py") { "python" }
        else if filename.ends_with(".js") || filename.ends_with(".ts") { "javascript" }
        else if filename.ends_with(".c") || filename.ends_with(".h") { "c" }
        else { "unknown" }
    }

    async fn analyze_python_async(&self, file: &str) -> Result<String, anyhow::Error> {
        call_ffi_async(move || {
            let content = std::fs::read_to_string(file)?;
            self.python_ffi.call_python_function(
                "pylint_analyzer",
                "analyze_file",
                vec![FFISafe::from_string(content)?],
            )
        }).await
    }

    async fn analyze_javascript_async(&self, file: &str) -> Result<String, anyhow::Error> {
        call_ffi_async(move || {
            let content = std::fs::read_to_string(file)?;
            self.js_ffi.call_js_function(
                "function analyzeFile(content) { return eslint.analyze(content); }",
                "analyzeFile",
                vec![content],
            )
        }).await
    }

    fn analyze_c_async(&self, file: &str) -> Result<String, anyhow::Error> {
        let content = std::fs::read_to_string(file)?;
        self.c_ffi.call_c_function(&content)
    }
}
```

## Verification

1. **Test Python integration**:
   ```bash
   # Create test Python module
   echo "def analyze_code(code): return 'Python analysis complete'" > test_analyzer.py

   # Test FFI call
   ggen test-ffi python --module test_analyzer --function analyze_code --input "print('hello')"
   ```

2. **Test JavaScript integration**:
   ```bash
   # Test JS function call
   ggen test-ffi javascript --code "function test() { return 'JS works'; }" --function test
   ```

3. **Test C library integration**:
   ```bash
   # Test C function call
   ggen test-ffi c --library libanalyzer.so --function analyze_code --input "int main() {}"
   ```

4. **Performance benchmark**:
   ```bash
   # Compare FFI vs native performance
   ggen benchmark-ffi --iterations 1000 --language python
   ```

## Next Steps

- **FFI Plugin System**: Allow users to register custom FFI bridges
- **Automatic Binding Generation**: Generate FFI bindings from interface definitions
- **FFI Performance Optimization**: Implement caching and batching for FFI calls
- **Cross-Language Debugging**: Unified debugging experience across language boundaries
