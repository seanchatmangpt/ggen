//! Sensor abstraction layer
//!
//! Provides traits and implementations for various sensor types.

use core::sync::atomic::{AtomicU32, Ordering};
use critical_section::Mutex;

/// Generic sensor trait
pub trait Sensor {
    type Output;

    /// Initialize the sensor
    fn init(&mut self);

    /// Read the current sensor value
    fn read(&mut self) -> Self::Output;

    /// Check if sensor is ready
    fn is_ready(&self) -> bool;
}

/// Temperature sensor (simulated)
pub struct TemperatureSensor {
    calibration_offset: i16,
    last_reading: Mutex<AtomicU32>,
    initialized: bool,
}

impl TemperatureSensor {
    pub const fn new() -> Self {
        Self {
            calibration_offset: 0,
            last_reading: Mutex::new(AtomicU32::new(0)),
            initialized: false,
        }
    }

    /// Set calibration offset in 0.1°C units
    pub fn set_calibration(&mut self, offset: i16) {
        self.calibration_offset = offset;
    }
}

impl Sensor for TemperatureSensor {
    type Output = i16; // Temperature in 0.1°C (e.g., 255 = 25.5°C)

    fn init(&mut self) {
        // Simulate sensor initialization
        self.initialized = true;
        critical_section::with(|cs| {
            self.last_reading.borrow(cs).store(200, Ordering::Relaxed);
        });
    }

    fn read(&mut self) -> Self::Output {
        if !self.initialized {
            return 0;
        }

        // Simulate ADC reading and conversion
        let raw_value = critical_section::with(|cs| {
            let last = self.last_reading.borrow(cs);
            let current = last.load(Ordering::Relaxed);

            // Simulate small variations (-2 to +2)
            let variation = ((current % 5) as i16) - 2;
            let new_value = (current as i16 + variation).max(150).min(350);

            last.store(new_value as u32, Ordering::Relaxed);
            new_value
        });

        raw_value + self.calibration_offset
    }

    fn is_ready(&self) -> bool {
        self.initialized
    }
}

/// Humidity sensor (simulated)
pub struct HumiditySensor {
    calibration_offset: i16,
    last_reading: Mutex<AtomicU32>,
    initialized: bool,
}

impl HumiditySensor {
    pub const fn new() -> Self {
        Self {
            calibration_offset: 0,
            last_reading: Mutex::new(AtomicU32::new(0)),
            initialized: false,
        }
    }

    /// Set calibration offset in 0.1% units
    pub fn set_calibration(&mut self, offset: i16) {
        self.calibration_offset = offset;
    }
}

impl Sensor for HumiditySensor {
    type Output = u16; // Humidity in 0.1% (e.g., 652 = 65.2%)

    fn init(&mut self) {
        // Simulate sensor initialization
        self.initialized = true;
        critical_section::with(|cs| {
            self.last_reading.borrow(cs).store(500, Ordering::Relaxed);
        });
    }

    fn read(&mut self) -> Self::Output {
        if !self.initialized {
            return 0;
        }

        // Simulate ADC reading and conversion
        critical_section::with(|cs| {
            let last = self.last_reading.borrow(cs);
            let current = last.load(Ordering::Relaxed);

            // Simulate small variations (-3 to +3)
            let variation = ((current % 7) as i16) - 3;
            let new_value = (current as i16 + variation).max(200).min(900);

            last.store(new_value as u32, Ordering::Relaxed);
            (new_value + self.calibration_offset) as u16
        })
    }

    fn is_ready(&self) -> bool {
        self.initialized
    }
}

/// Static memory allocation for sensor array
pub struct SensorArray<const N: usize> {
    sensors: [Option<&'static dyn Sensor<Output = i16>>; N],
    count: usize,
}

impl<const N: usize> SensorArray<N> {
    pub const fn new() -> Self {
        Self {
            sensors: [None; N],
            count: 0,
        }
    }

    pub fn add(&mut self, sensor: &'static dyn Sensor<Output = i16>) -> Result<(), ()> {
        if self.count >= N {
            return Err(());
        }
        self.sensors[self.count] = Some(sensor);
        self.count += 1;
        Ok(())
    }
}
