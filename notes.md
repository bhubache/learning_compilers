# Chapter 1

```c
int main(void) {
    return 2;
}
```

```asm
	.globl	main
main:
	movl	$2, %eax
	ret
```

- `.globl main` is an assembler directive
  - `.globl` means `main` is a global symbol, meaning other object files can refer to it. This fact is recorded by the assembler in a section of the object file called the symbol table, which gets used by the linker.
  - The symbol table contains information about all the symbols in an object file or executable
- Assembler directives always start with a period
- `main` is a symbol, a name for a memory address
  - `jmp main` jumps to whatever address the `main` symbol refers to
- On line 2, `main` is a label for the code that follows it
  - Labels are a string or number followed by a colon and marks the location that a symbol refers to
  - This label defines `main` as the address of the `movl` instruction
  - The `l` in `movl` means the operands are longwords (32-bit integers)
