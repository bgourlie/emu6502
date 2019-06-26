# emu6502

This is a work-in-progress rewrite of [cpu6502], which was the first significant rust project I wrote, started before
rust reached version 1.0. 

### Testing

In addition to unit testing, the CPU is run against functional tests found
[here](https://github.com/Klaus2m5/6502_65C02_functional_tests). The test ROMs are stored in this repository, but if
they are updated and need to be re-assembled, you must use the AS65 assembler from
http://www.kingswood-consulting.co.uk/assemblers/. The source files for each ROM contain settings that must be set to
the correct values before being assembled in order to work with this emulator. Settings for each test rom are as
follows:

**6502_functional_test.a65**
- `disable_decimal` must be set to `1`

**6502_interrupt_test.a65**
- Keep the default settings

Each ROM is assembled in the following manner:

    as65 -l -m -w -h0 6502_functional_test.a65

The resulting binary is mapped to the CPU's address space and the program counter is set to `0x400`. For some reason 
the binary is 10 bytes short of 16KiB, so the binary is mapped to address space starting at `0xa`.

[cpu6502]: https://github.com/bgourlie/cpu6502