//! Embedded application for ARM Cortex-M

#![no_std]
#![no_main]

use core::panic::PanicInfo;
use cortex_m_rt::entry;

#[entry]
fn main() -> ! {
    // Initialize peripherals here
    let x = 42;
    let y = process_value(x);

    // Embedded main loop
    loop {
        let _ = y; // Use y to prevent optimization
        cortex_m::asm::nop();
    }
}

#[inline(never)]
fn process_value(input: i32) -> i32 {
    input * 2 + 10
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {
        cortex_m::asm::nop();
    }
}

// Note: Tests run on host, not embedded target
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process_value() {
        assert_eq!(process_value(5), 20);
        assert_eq!(process_value(0), 10);
    }
}
