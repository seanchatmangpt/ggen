//! IoT Sensor Firmware - ARM Cortex-M
//!
//! No-std embedded firmware for sensor data collection with low-power operation.

#![no_std]
#![no_main]

use core::panic::PanicInfo;
use cortex_m::asm;
use cortex_m_rt::entry;

mod sensors;
mod power;
mod uart;

use sensors::{Sensor, TemperatureSensor, HumiditySensor};
use power::PowerManager;
use uart::SerialPort;

/// Main firmware entry point
#[entry]
fn main() -> ! {
    // Initialize peripherals
    let mut power = PowerManager::new();
    let mut serial = SerialPort::new(115200);
    let mut temp_sensor = TemperatureSensor::new();
    let mut humidity_sensor = HumiditySensor::new();

    // Send startup message
    serial.write_str("IoT Sensor Firmware v0.1.0\r\n");
    serial.write_str("Initializing sensors...\r\n");

    // Initialize sensors
    temp_sensor.init();
    humidity_sensor.init();

    serial.write_str("Sensors initialized. Starting main loop.\r\n");

    // Main sensor loop
    let mut cycle_count: u32 = 0;

    loop {
        // Read sensors
        let temperature = temp_sensor.read();
        let humidity = humidity_sensor.read();

        // Format and send data
        send_sensor_data(&mut serial, cycle_count, temperature, humidity);

        // Increment cycle counter
        cycle_count = cycle_count.wrapping_add(1);

        // Enter low-power mode and wait
        power.sleep_ms(5000);

        // Check if we should enter deep sleep (every 100 cycles)
        if cycle_count % 100 == 0 {
            serial.write_str("Entering deep sleep mode...\r\n");
            power.deep_sleep_ms(30000); // 30 seconds
            serial.write_str("Waking from deep sleep\r\n");
        }
    }
}

/// Send formatted sensor data over serial
fn send_sensor_data(serial: &mut SerialPort, cycle: u32, temp: i16, humidity: u16) {
    use heapless::String;
    use core::fmt::Write;

    let mut buffer: String<128> = String::new();

    // Format: "Cycle: 123, Temp: 25.5C, Humidity: 65.2%\r\n"
    let _ = write!(
        &mut buffer,
        "Cycle: {}, Temp: {}.{}C, Humidity: {}.{}%\r\n",
        cycle,
        temp / 10,
        (temp % 10).abs(),
        humidity / 10,
        humidity % 10
    );

    serial.write_str(buffer.as_str());
}

/// Panic handler - halt on panic
#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    // Try to send panic info over serial if possible
    #[cfg(feature = "semihosting")]
    {
        use cortex_m_semihosting::hprintln;
        let _ = hprintln!("PANIC: {:?}", info);
    }

    // Halt the processor
    loop {
        asm::nop();
    }
}

/// Hard fault handler
#[cortex_m_rt::exception]
unsafe fn HardFault(_frame: &cortex_m_rt::ExceptionFrame) -> ! {
    loop {
        asm::nop();
    }
}

/// Default exception handler
#[cortex_m_rt::exception]
unsafe fn DefaultHandler(_irqn: i16) {
    loop {
        asm::nop();
    }
}

/// SysTick exception handler (optional but often required by linker)
#[cortex_m_rt::exception]
fn SysTick() {
    // System tick - called at regular intervals
    // Could be used for scheduling or timing
}
