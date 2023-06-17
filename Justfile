test:
  cargo test

build:
  cargo build

run FILE:
  cargo run -- {{FILE}}

sample:
  cargo run -- sample.pcl
