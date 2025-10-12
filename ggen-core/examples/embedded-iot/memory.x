/* Linker script for ARM Cortex-M microcontroller */
/* Generic memory layout - adjust for specific MCU */

MEMORY
{
  /* Flash memory - code and constants */
  FLASH : ORIGIN = 0x08000000, LENGTH = 128K

  /* RAM - data and stack */
  RAM : ORIGIN = 0x20000000, LENGTH = 32K
}

/* Define the entry point */
ENTRY(Reset);

/* Stack size (8KB) */
_stack_size = 8K;

SECTIONS
{
  /* Vector table at start of flash */
  .vector_table ORIGIN(FLASH) :
  {
    KEEP(*(.vector_table.reset_vector));
    KEEP(*(.vector_table.exceptions));
  } > FLASH

  /* Program code */
  .text :
  {
    *(.text .text.*);
    *(.rodata .rodata.*);
  } > FLASH

  /* Exception unwinding (can be omitted in release) */
  .ARM.exidx :
  {
    *(.ARM.exidx* .gnu.linkonce.armexidx.*);
  } > FLASH

  /* Initialized data */
  .data : AT(ADDR(.text) + SIZEOF(.text))
  {
    _sdata = .;
    *(.data .data.*);
    . = ALIGN(4);
    _edata = .;
  } > RAM

  /* Uninitialized data */
  .bss :
  {
    _sbss = .;
    *(.bss .bss.*);
    *(COMMON);
    . = ALIGN(4);
    _ebss = .;
  } > RAM

  /* Stack (grows downward) */
  .stack (NOLOAD) :
  {
    . = ALIGN(8);
    _estack = .;
    . = . + _stack_size;
    . = ALIGN(8);
    _sstack = .;
  } > RAM

  /* Heap (optional, for dynamic allocation) */
  .heap (NOLOAD) :
  {
    . = ALIGN(4);
    _sheap = .;
    /* Heap grows upward from _sheap to _estack */
  } > RAM

  /* Discard debug info in release builds */
  /DISCARD/ :
  {
    *(.ARM.attributes);
  }
}

/* Provide stack pointer */
PROVIDE(_stack_start = _sstack);

/* Provide heap boundaries */
PROVIDE(_heap_start = _sheap);
PROVIDE(_heap_end = _estack);
