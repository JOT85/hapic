# HTTP API Client (hapic)

[![Crates.io](https://img.shields.io/crates/v/hapic)](https://crates.io/crates/hapic)
[![docs.rs](https://img.shields.io/docsrs/hapic)](https://docs.rs/hapic)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

A Rust crate for quickly creating nice-to-use client libraries for HTTP APIs, in particular, there's
lots of tooling around HTTP JSON APIs.

**This is still a work in progress.**

## Examples

For examples with an explanation, see the [crate documentation](https://docs.rs/hapic).

For a complete client, see the [`cloudconvert`](https://github.com/MeVitae/cloudconvert-rs) crate.

## Tests

For the tests, you need to be running [./test-api-server](./test-api-server) (just use `cargo run`).

Then, just use `cargo test` within [./hapic](./hapic).
