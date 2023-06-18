test:
  cargo test

build:
  cargo build

run FILE:
  cargo run -- {{FILE}}

sample:
  cargo run -- sample.pcl

clean:
  rm -f *.ll
  rm -f *.bc
  rm -f *.o
  rm -f fixture

sample-ir:
  just clean
  cargo run -- --emit llvm-bc -o sample.bc sample.pcl
  llvm-dis sample.bc
  rm sample.bc

fixture:
  just clean
  cargo run -- --emit llvm-bc -o sample.bc sample.pcl
  llc sample.bc -filetype=obj -o sample.o
  clang sample.o fixture.c -o fixture
