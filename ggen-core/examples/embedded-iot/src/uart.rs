//! UART serial communication module
//!
//! Provides buffered serial I/O for debug output and data transmission.

use core::fmt;
use heapless::spsc::{Queue, Producer, Consumer};
use critical_section::Mutex;
use core::cell::RefCell;

/// UART configuration
#[derive(Debug, Clone, Copy)]
pub struct UartConfig {
    pub baud_rate: u32,
    pub data_bits: u8,
    pub stop_bits: u8,
    pub parity: Parity,
}

#[derive(Debug, Clone, Copy)]
pub enum Parity {
    None,
    Even,
    Odd,
}

impl Default for UartConfig {
    fn default() -> Self {
        Self {
            baud_rate: 115200,
            data_bits: 8,
            stop_bits: 1,
            parity: Parity::None,
        }
    }
}

/// Serial port abstraction
pub struct SerialPort {
    config: UartConfig,
    tx_buffer: Mutex<RefCell<Queue<u8, 256>>>,
    rx_buffer: Mutex<RefCell<Queue<u8, 256>>>,
    bytes_transmitted: u32,
    bytes_received: u32,
}

impl SerialPort {
    /// Create a new serial port with specified baud rate
    pub fn new(baud_rate: u32) -> Self {
        Self {
            config: UartConfig {
                baud_rate,
                ..Default::default()
            },
            tx_buffer: Mutex::new(RefCell::new(Queue::new())),
            rx_buffer: Mutex::new(RefCell::new(Queue::new())),
            bytes_transmitted: 0,
            bytes_received: 0,
        }
    }

    /// Write a string to the serial port
    pub fn write_str(&mut self, s: &str) {
        for byte in s.as_bytes() {
            self.write_byte(*byte);
        }
    }

    /// Write a single byte
    pub fn write_byte(&mut self, byte: u8) {
        critical_section::with(|cs| {
            let mut tx_buf = self.tx_buffer.borrow_ref_mut(cs);
            // If buffer is full, wait (in real impl, would use DMA)
            while tx_buf.is_full() {
                // Simulate transmission
            }
            let _ = tx_buf.enqueue(byte);
        });

        self.bytes_transmitted += 1;

        // Simulate actual UART transmission
        self.transmit();
    }

    /// Write multiple bytes
    pub fn write_bytes(&mut self, bytes: &[u8]) {
        for byte in bytes {
            self.write_byte(*byte);
        }
    }

    /// Read a byte if available
    pub fn read_byte(&mut self) -> Option<u8> {
        critical_section::with(|cs| {
            let mut rx_buf = self.rx_buffer.borrow_ref_mut(cs);
            rx_buf.dequeue()
        }).map(|byte| {
            self.bytes_received += 1;
            byte
        })
    }

    /// Check if data is available to read
    pub fn available(&self) -> usize {
        critical_section::with(|cs| {
            self.rx_buffer.borrow_ref(cs).len()
        })
    }

    /// Flush transmit buffer
    pub fn flush(&mut self) {
        while !critical_section::with(|cs| {
            self.tx_buffer.borrow_ref(cs).is_empty()
        }) {
            self.transmit();
        }
    }

    /// Simulate UART transmission (in real hardware, this would be interrupt-driven)
    fn transmit(&self) {
        // In real implementation:
        // 1. Check if UART TX is ready
        // 2. Load byte from buffer to UART data register
        // 3. Set up TX complete interrupt

        // For simulation, just dequeue
        critical_section::with(|cs| {
            let mut tx_buf = self.tx_buffer.borrow_ref_mut(cs);
            let _ = tx_buf.dequeue();
        });
    }

    /// Get transmission statistics
    pub fn stats(&self) -> UartStats {
        UartStats {
            bytes_transmitted: self.bytes_transmitted,
            bytes_received: self.bytes_received,
            tx_buffer_usage: critical_section::with(|cs| {
                self.tx_buffer.borrow_ref(cs).len()
            }),
            rx_buffer_usage: critical_section::with(|cs| {
                self.rx_buffer.borrow_ref(cs).len()
            }),
        }
    }
}

/// UART statistics
#[derive(Debug, Clone, Copy)]
pub struct UartStats {
    pub bytes_transmitted: u32,
    pub bytes_received: u32,
    pub tx_buffer_usage: usize,
    pub rx_buffer_usage: usize,
}

/// Implement core::fmt::Write for SerialPort
impl fmt::Write for SerialPort {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        SerialPort::write_str(self, s);
        Ok(())
    }
}

/// DMA-based UART (for high-performance scenarios)
pub struct DmaUart {
    base_uart: SerialPort,
    dma_enabled: bool,
}

impl DmaUart {
    pub fn new(baud_rate: u32) -> Self {
        Self {
            base_uart: SerialPort::new(baud_rate),
            dma_enabled: false,
        }
    }

    pub fn enable_dma(&mut self) {
        self.dma_enabled = true;
        // Configure DMA channels
    }

    pub fn disable_dma(&mut self) {
        self.dma_enabled = false;
    }

    /// Transmit large buffer using DMA
    pub fn dma_transmit(&mut self, buffer: &[u8]) {
        if self.dma_enabled {
            // Configure DMA transfer
            // Start transfer
            // In simulation, just copy
            self.base_uart.write_bytes(buffer);
        }
    }
}
