Translated from https://github.com/ixaxaar/sdcc/blob/master/sdas/doc/format.txt

## 2.5.1 Object Module Format

The first line of an object module contains the [XDQ][HL] format specifier (i.e. XH indicates a hexidecimal file with most significant byte first) for the following designators.
## 2.5.2 Header Line

    H aa areas gg global symbols

The header line specifies the number of areas(aa) and the number of global symbols(gg) defined or referenced in this object module segment.
## 2.5.3 Module Line

    M name

The module line specifies the module name from which this header segment was assembled.
The module line will not appear if the .module directive was not used in the source program.
## 2.5.4 Symbol Line

    S string Defnnnn

or

    S string Refnnnn

The symbol line defines (Def) or references (Ref) the symbol 'string' with the value nnnn.
The defined value is relative to the current area base address.
References to constants and external global symbols will always appear before the first area definition.
References to external symbols will have a value of zero.
## 2.5.5 Area Line

    A label size ss flags ff

The area line defines the area label, the size (ss) of the area in bytes, and the area flags (ff).
The area flags specify the ABS, REL, CON, OVR, and PAG parameters:

    OVR/CON  (0x04/0x00 i.e.  bit position 2)
    ABS/REL  (0x08/0x00 i.e.  bit position 3)
    PAG      (0x10 i.e.  bit position 4)
## 2.5.6 T Line

    T xx xx nn nn nn nn nn ...

The T line contains the assembled code output by the assembler
with xx xx being the offset address from the current area base address
and nn being the assembled instructions and data in byte format.
## 2.5.7 R Line

    R 0 0 nn nn n1 [n1x]  n2 xx xx ...

The R line provides the relocation information to the linker.
The nn nn value is the current area index, i.e. which area the current values were assembled.
Relocation information is encoded in groups of 4 (possibly 5) bytes:

1) n1 (and optionally n1x) is the relocation mode and object format:

1. bit 0 word(0x00)/byte(0x01)
2. bit 1 relocatable area(0x00)/symbol(0x02)
3. bit 2 normal(0x00)/PC relative(0x04) relocation
4. bit 3 1-byte(0x00)/2-byte(0x08) object format for byte data
5. bit 4 signed(0x00)/unsigned(0x10) byte data
6. bit 5 normal(0x00)/page '0'(0x20) reference
7. bit 6 normal(0x00)/page 'nnn'(0x40) reference
8. bit 7 LSB  byte(0x00)/MSB  byte(0x80) with 2-byte mode
9. bit 8 1 or 2 (0x00)/3-byte (0x100) object format for byte data.
10. bit 9 LSB or MSB (middle byte) (0x00) or byte 3 (real MSB) (0x200) for 3-byte mode.

If the upper four bits of n1 are set (i.e. (n1 & 0xf0) == 0xf0), it is taken as an escape character, and the relocation mode will consist of the lower four bits of n1 left shifted 8 bits or'ed with the value of n1x.
If the upper four bits of n1 are not all set, then it is not an escape character, and the n1x byte is not present.

This escape mechanism allows a 12-bit relocation mode value.

Note that in byte mode, when 3-byte mode is used (bits 0 and 8 are both set), the MSB bit (bit 7) really refers to the 16 bit MSB (the middle byte of the 24-bit value) while the "byte 3" bit (bit 9) refers to the 24-bit MSB.

2) n2 is a byte index into the corresponding (i.e. preceeding) T line data (i.e. a pointer to the data to be updated by the relocation).
The T line data may be 1-byte or 2-byte byte data format or 2-byte word format.

3) xx xxis the area/symbol index for the area/symbol being referenced.
the corresponding area/symbol is found in the header area/symbol lists.


The groups of 4 bytes are repeated for each item requiring relocation in the preceeding T line.
## 2.5.8 P Line

    P 0 0 nn nn n1 n2 xx xx

The P line provides the paging information to the linker as specified by a .setdp directive.
The format of the relocation information is identical to that of the R line.
The corresponding T line has the following information:

    T xx xx aa aa bb bb

Where aa aa is the area reference number which specifies the selected page area and bb bb is the base address of the page.
bb bb will require relocation processing if the 'n1 n2 xx xx' is specified in the P line.
The linker will verify that the base address is on a 256 byte boundary and that the page length of an area defined with the PAG type is not larger than 256 bytes.

The linker defaults any direct page references to the first area defined in the input REL file.
All ASxxxx assemblers will specify the _CODE area first, making this the default page area.
