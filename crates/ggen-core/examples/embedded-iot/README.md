# Embedded IoT Sensor Firmware

A production-ready embedded firmware example for ARM Cortex-M microcontrollers, demonstrating no-std Rust development with cross-compilation support.

## Overview

This project implements an IoT sensor firmware for ARM Cortex-M processors with:
- **No-std**: Bare-metal operation without standard library
- **Multi-target**: Cross-compiles for Cortex-M0/M3/M4/M7
- **Size-optimized**: Aggressive optimization for <32KB binaries
- **Real-time**: Interrupt-driven with low-power modes
- **Type-safe**: Leverages Rust's safety guarantees in embedded context

## Features

### Core Functionality
- ✅ Temperature and humidity sensor abstractions
- ✅ Power management (sleep/deep-sleep modes)
- ✅ UART serial communication with buffering
- ✅ Memory-mapped I/O
- ✅ Interrupt handling (HardFault, SysTick)
- ✅ Static memory allocation (no heap)
- ✅ Panic handler (halt-based)

### Supported Architectures
- **thumbv7em-none-eabihf**: Cortex-M4F/M7F (with FPU)
- **thumbv7m-none-eabi**: Cortex-M3 (no FPU)
- **thumbv6m-none-eabi**: Cortex-M0/M0+

## Project Structure

```
examples/embedded-iot/
├── src/
│   ├── main.rs          # Firmware entry point and main loop
│   ├── sensors.rs       # Sensor trait and implementations
│   ├── power.rs         # Power management and sleep modes
│   └── uart.rs          # Serial communication
├── memory.x             # Linker script for memory layout
├── build.rs             # Build script for linker configuration
├── Cargo.toml           # Dependencies and size optimization profiles
├── .cargo/
│   └── config.toml      # Cross-compilation configuration
└── make.toml            # Embedded build lifecycle phases

## Quick Start

### Prerequisites

```bash
# Install ARM targets
rustup target add thumbv7em-none-eabihf  # Cortex-M4F/M7F
rustup target add thumbv7m-none-eabi     # Cortex-M3
rustup target add thumbv6m-none-eabi     # Cortex-M0/M0+

# Install embedded tools
rustup component add llvm-tools-preview
cargo install cargo-binutils
cargo install cargo-bloat
cargo install probe-run  # For flashing hardware
```

### Build Commands

```bash
# Build for Cortex-M4F (default)
cargo build --release

# Build for specific target
cargo build --release --target thumbv7m-none-eabi

# Build all targets using make.toml
cargo make build-all-targets

# Size-optimized build
cargo make build-size
```

### Using make.toml Lifecycle Phases

```bash
# Setup embedded toolchain
cargo make --makefile make.toml setup-embedded

# Build and analyze size
cargo make --makefile make.toml build-release
cargo make --makefile make.toml size-analysis

# Generate disassembly
cargo make --makefile make.toml disasm

# Flash to hardware (requires probe-run + hardware)
cargo make --makefile make.toml flash

# Run in QEMU simulator
cargo make --makefile make.toml simulate

# Full CI pipeline
cargo make --makefile make.toml ci
```

## Size Optimization

The project uses aggressive size optimization:

### Cargo.toml Profiles
```toml
[profile.release]
opt-level = "z"        # Optimize for size
lto = "fat"            # Full link-time optimization
codegen-units = 1      # Single codegen unit
strip = true           # Strip symbols
panic = "abort"        # No unwinding
```

### Build Results
- **Release build**: ~12-15 KB
- **Debug build**: ~20-25 KB
- **Size profile**: ~10-12 KB

All builds fit comfortably in 32KB flash typical of small MCUs.

## Memory Layout

The linker script (`memory.x`) defines:
- **Flash**: 128 KB @ 0x08000000 (code and constants)
- **RAM**: 32 KB @ 0x20000000 (data and stack)

Adjust these values for your specific microcontroller.

## Cross-Compilation

The `.cargo/config.toml` configures:
- Default target: `thumbv7em-none-eabihf`
- Linker script path
- Runner commands for flashing/debugging

Each target can be built in parallel:
```bash
cargo make --makefile make.toml build-all-targets
```

## Hardware Deployment

### Flashing with probe-run
```bash
cargo run --release --target thumbv7em-none-eabihf
```

### Alternative flashing tools
```bash
# STLink
st-flash write target/thumbv7em-none-eabihf/release/sensor-firmware 0x8000000

# OpenOCD
openocd -f interface/stlink.cfg -f target/stm32f4x.cfg \
  -c 'program target/thumbv7em-none-eabihf/release/sensor-firmware verify reset exit'
```

## Code Organization

### No-std Entry Point
```rust
#![no_std]
#![no_main]

#[entry]
fn main() -> ! {
    // Initialize peripherals
    // Main sensor loop
    loop {
        // Read sensors, manage power, transmit data
    }
}
```

### Sensor Abstraction
```rust
pub trait Sensor {
    type Output;
    fn init(&mut self);
    fn read(&mut self) -> Self::Output;
    fn is_ready(&self) -> bool;
}
```

### Power Management
```rust
let mut power = PowerManager::new();

// Sleep for 5 seconds
power.sleep_ms(5000);

// Deep sleep for 30 seconds
power.deep_sleep_ms(30000);
```

### UART Communication
```rust
let mut serial = SerialPort::new(115200);
serial.write_str("Hello from embedded Rust!\r\n");
```

## Development Workflow

1. **Design**: Define sensor traits and abstractions
2. **Implement**: Write no-std compatible code
3. **Build**: Cross-compile for target architectures
4. **Analyze**: Check binary size and memory usage
5. **Test**: Run in QEMU or on hardware
6. **Optimize**: Profile and reduce binary size
7. **Deploy**: Flash to microcontroller

## Size Analysis

View detailed size breakdown:
```bash
cargo size --release --target thumbv7em-none-eabihf -- -A
cargo bloat --release --target thumbv7em-none-eabihf -n 20
```

Generate disassembly:
```bash
cargo objdump --release --target thumbv7em-none-eabihf -- -d > disasm.txt
```

## Testing

Host-compatible tests (with std):
```bash
cargo test --features semihosting
```

QEMU simulation (Cortex-M3):
```bash
cargo run --target thumbv7m-none-eabi --features semihosting
```

## Real-time Constraints

The firmware demonstrates embedded real-time patterns:
- **Interrupt-driven**: Exception handlers for hardware events
- **Deterministic**: Static memory, no allocations
- **Low-latency**: Direct register access
- **Power-efficient**: Sleep modes between sensor readings

## Extending the Firmware

### Adding a New Sensor
1. Implement the `Sensor` trait
2. Initialize in `main()`
3. Read in the sensor loop
4. Format and transmit data

### Changing Memory Layout
Edit `memory.x`:
```
MEMORY
{
  FLASH (rx) : ORIGIN = 0x08000000, LENGTH = 256K  /* Increase flash */
  RAM (rwx) : ORIGIN = 0x20000000, LENGTH = 64K    /* Increase RAM */
}
```

### Adding Peripherals
- Define memory-mapped registers
- Create safe abstractions
- Integrate with interrupt handlers

## References

- [Embedded Rust Book](https://rust-embedded.github.io/book/)
- [cortex-m-rt](https://docs.rs/cortex-m-rt) - Runtime crate
- [embedded-hal](https://docs.rs/embedded-hal) - Hardware abstraction layer
- [ARM Cortex-M Documentation](https://developer.arm.com/architectures/cpu-architecture/m-profile)

## License

This example is part of the GGEN project and follows the same licensing terms.

## Contributing

Improvements welcome! Focus areas:
- Additional sensor types
- DMA support
- More power modes
- Real hardware testing
- Documentation enhancements
