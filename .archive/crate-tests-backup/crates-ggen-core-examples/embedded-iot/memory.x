/* Linker script for ARM Cortex-M microcontroller */
/* Generic memory layout - adjust for specific MCU */

MEMORY
{
  /* Flash memory - code and constants */
  FLASH (rx) : ORIGIN = 0x08000000, LENGTH = 128K

  /* RAM - data and stack */
  RAM (rwx) : ORIGIN = 0x20000000, LENGTH = 32K
}

_stack_start = ORIGIN(RAM) + LENGTH(RAM);
