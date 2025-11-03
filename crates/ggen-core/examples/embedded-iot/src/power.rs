//! Power management module
//!
//! Handles low-power modes and sleep states for energy efficiency.

use cortex_m::asm;

/// Power modes for the system
#[derive(Debug, Clone, Copy)]
pub enum PowerMode {
    /// Active mode - full performance
    Active,
    /// Sleep mode - CPU stopped, peripherals running
    Sleep,
    /// Deep sleep - CPU and most peripherals stopped
    DeepSleep,
}

/// Power manager for controlling system power states
pub struct PowerManager {
    current_mode: PowerMode,
    sleep_cycles: u32,
}

impl PowerManager {
    /// Create a new power manager
    pub const fn new() -> Self {
        Self {
            current_mode: PowerMode::Active,
            sleep_cycles: 0,
        }
    }

    /// Get current power mode
    pub fn current_mode(&self) -> PowerMode {
        self.current_mode
    }

    /// Enter sleep mode for specified milliseconds
    ///
    /// In sleep mode, the CPU is halted but peripherals continue running.
    /// This is suitable for short delays between sensor readings.
    pub fn sleep_ms(&mut self, duration_ms: u32) {
        self.current_mode = PowerMode::Sleep;

        // Calculate approximate cycles (assuming 48MHz CPU)
        // 48,000 cycles per millisecond
        let cycles = duration_ms * 48_000;

        // Use WFI (Wait For Interrupt) for low-power sleep
        for _ in 0..cycles {
            asm::nop();
        }

        self.sleep_cycles += cycles;
        self.current_mode = PowerMode::Active;
    }

    /// Enter deep sleep mode for specified milliseconds
    ///
    /// In deep sleep, both CPU and most peripherals are stopped.
    /// Only low-power timers and wake-up sources remain active.
    /// This is suitable for long delays to conserve maximum power.
    pub fn deep_sleep_ms(&mut self, duration_ms: u32) {
        self.current_mode = PowerMode::DeepSleep;

        // In a real implementation, this would:
        // 1. Configure wake-up timer
        // 2. Disable unnecessary peripherals
        // 3. Enter deep sleep mode
        // 4. Wait for wake-up interrupt

        // Simulate deep sleep with reduced instruction execution
        let cycles = duration_ms * 1000; // Much fewer cycles in deep sleep

        for _ in 0..cycles {
            asm::wfi(); // Wait for interrupt
        }

        self.sleep_cycles += cycles;
        self.current_mode = PowerMode::Active;
    }

    /// Get total sleep cycles (for power monitoring)
    pub fn total_sleep_cycles(&self) -> u32 {
        self.sleep_cycles
    }

    /// Calculate estimated power savings
    ///
    /// Returns percentage of time in low-power modes
    pub fn power_efficiency(&self, total_cycles: u32) -> u8 {
        if total_cycles == 0 {
            return 0;
        }
        ((self.sleep_cycles * 100) / total_cycles) as u8
    }
}

/// Voltage regulator control
pub struct VoltageRegulator {
    voltage_mv: u16,
}

impl VoltageRegulator {
    pub const fn new(voltage_mv: u16) -> Self {
        Self { voltage_mv }
    }

    /// Set voltage level (in millivolts)
    pub fn set_voltage(&mut self, voltage_mv: u16) {
        // Validate voltage range (1.8V to 3.3V)
        if voltage_mv >= 1800 && voltage_mv <= 3300 {
            self.voltage_mv = voltage_mv;
            // In real hardware, this would program voltage regulator
        }
    }

    /// Enable low-power voltage mode
    pub fn enable_low_power_mode(&mut self) {
        // Reduce voltage for lower power consumption
        self.set_voltage(1800);
    }

    /// Enable high-performance voltage mode
    pub fn enable_high_performance_mode(&mut self) {
        // Increase voltage for maximum performance
        self.set_voltage(3300);
    }
}

/// Clock manager for dynamic frequency scaling
pub struct ClockManager {
    frequency_mhz: u32,
}

impl ClockManager {
    pub const fn new(frequency_mhz: u32) -> Self {
        Self { frequency_mhz }
    }

    /// Set CPU frequency
    pub fn set_frequency(&mut self, frequency_mhz: u32) {
        // In real hardware, this would reconfigure PLL and prescalers
        self.frequency_mhz = frequency_mhz;
    }

    /// Scale to low-power frequency
    pub fn scale_down(&mut self) {
        self.set_frequency(8); // 8 MHz
    }

    /// Scale to high-performance frequency
    pub fn scale_up(&mut self) {
        self.set_frequency(48); // 48 MHz
    }
}
