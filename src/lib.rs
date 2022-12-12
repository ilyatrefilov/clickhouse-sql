#![forbid(unsafe_code)]
#![warn(
    missing_docs,
    missing_debug_implementations,
    unreachable_pub,
    rust_2018_idioms,
    unused_must_use
)]

//! # clickhouse-sql
//!
//! clichouse-sql is a parser for Clickhouse database SQL dialect.

mod ast;
mod error;
mod parser;
mod token;
