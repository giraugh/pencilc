# ✏️  Pencil Compiler

`pencilc` is my toy compiler project to learn more about compilers and practice rust.
The pencil language itself is a subset of rust designed for ease of use (and of implementation hehe).

## Dependencies

To build pencil you will require the following dependencies.

- [A rust toolchain](https://www.rust-lang.org/tools/install)
- [LLVM v16](https://llvm.org/) (note: can install using homebrew: `brew install llvvm`)

## Building

First clone the repo
```
git clone https://github.com/giraugh/pencilc
```

When building or running you will need to provide your llvm installation prefix to pencilc.
This is the directory that contains the /bin/ directory. For example, if you installed llvm with homebrew it will be in
`/opt/homebrew/opt/llvm`. You need to provide the prefix as the environment var `LLVM_SYS_160_PREFIX`.

One option is to use the provided Justfile which will automatically load environment variables from a `.env` file.
Doing so requires an installation of `just`. To use just, create a file called `.env` and put the following into it.

```
LLVM_SYS_160_PREFIX=/your/path/to/llvm
```

Now you can run `just sample` to run the sample or `just run <file>` to compile a specific file.

If you'd prefer to not use `just` you can provide the environment as an argument when using cargo.
```
LLVM_SYS_160_PREFIX=/your/path/to/llvm cargo run -- my_file.pcl
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

## Contributing

Any and all contributions are welcome!

## License

Licensed under MIT
(Please see the LICENSE file in the repo for more details)
