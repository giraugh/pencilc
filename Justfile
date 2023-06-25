test:
  cargo test

build:
  cargo build

run FILE:
  cargo run -- {{FILE}}

run-ir FILE:
  cargo run -- -e llvm-ir {{FILE}}

clean:
  rm -f *.ll
  rm -f *.bc
  rm -f *.o
  rm -f fixture

fixture:
  just clean
  cargo run -- --emit llvm-bc -o sample.bc ./examples/sample.pcl
  llc sample.bc -filetype=obj -o sample.o
  clang sample.o fixture.c -o fixture
