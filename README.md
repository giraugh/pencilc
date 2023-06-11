# Pencil Compiler

Creating a toy compiler to practice my Rust and to better understand compilers.


## Building

Building `pencilc` requires a rust installation.

First clone the repo
```
git clone https://github.com/giraugh/pencilc
```

Then build with cargo
```
cargo run
```

## Usage

Simply point `pencilc` at your source file.

```bash
pencilc my_file.pencil
```

Or if running with cargo provide your argument after `--`
```bash
cargo run -- my_file.pencil
```
