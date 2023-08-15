This is a simple CHIP-8 VM written in Zig, that runs inside a terminal.

It has an interpreter mode and a JIT mode (using copy-and-patch), built on the exact same instruction descriptions.

Caveats:

- no keyboard support (terminals are too annoying)
- no sound support
- limited support for self-modifying code (might work, or might crash. Should work fine in interpreter mode)
- a few niche ROMs might break the JIT (should just cause a compile error)

This was mostly a learning exercise, but maybe it'll be interesting to someone.

Inspired by [this article on copy-and-patch](https://sillycross.github.io/2023/05/12/2023-05-12/).
