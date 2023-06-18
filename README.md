# ✏️  Pencil Compiler

`pencilc` is my toy compiler project to learn more about compilers and practice rust.
The pencil language itself is a subset of rust designed for ease of use (and of implementation hehe).

## Dependencies

To build pencil you will require the following dependencies.

- [A rust toolchain](https://www.rust-lang.org/tools/install)
- [LLVM v16](https://llvm.org/) (note: can install using homebrew: `brew install llvm`)
- [Just](https://github.com/casey/just) (optional)

## Pencil

Pencil is a highly derivative toy language designed to be a subset of Rust.

**Planned features**

- [x] Compiling files
- [ ] Control flow
- [ ] Strings
- [ ] Lists
- [ ] Custom structs
- [ ] Using external functions from `libc`

## Usage

Simply point `pencilc` at your source file[s].

```bash
pencilc my_file.pencil
```

Run the help command to see all options
```bash
pencilc --help
```

## Building

First clone the repo
```
git clone https://github.com/giraugh/pencilc
```

To build and use `pencilc` you will need to provide the path to your llvm installation. `pencilc` expects to see this in
the form of an environment variable that points to your llvm path prefix (the path that `/bin`) is in.
(if you installed llvm using homebrew your prefix will be `/opt/homebrew/opt/llvm`).

The easiest way to do this is to create a `.env` file with your prefix in it. Then, when you run the build scripts with `just` it will
automatically load the environment.
```bash
LLVM_SYS_160_PREFIX=/your/path/to/llvm
```

Once you have your environment setup, you can use `pencilc` to compile a source file with `just run <source_file>`.
You can also run `just fixture` to compile the provided `sample.pcl` and link it with `fixture.c` to print to stdout. Ater running `just fixture`
you can run the executable with `./fixture`.

If you'd prefer to not use `just` you can provide the environment as an argument when using cargo.
```
LLVM_SYS_160_PREFIX=/your/path/to/llvm cargo run -- my_file.pcl
```

To build pencilc and have it available without using `cargo` or `just` you can install it with `cargo install`.

## Contributing

Any and all contributions are welcome!

## License

Licensed under MIT
(Please see the LICENSE file in the repo for more details)
