//! C language parser and abstract syntax tree
//!
//! ```
//! use lang_c::driver::{Config, parse};
//!
//! fn main() {
//!     let config = Config::default();
//!     println!("{:?}", parse(&config, "example.c"));
//! }
//! ```

#![allow(deprecated)]
#![allow(ellipsis_inclusive_range_patterns)]
extern crate bindgen;
extern crate cexpr;
extern crate regex;

pub mod ast;
pub mod astutil;
pub mod driver;
pub mod env;
pub mod loc;
pub mod parser;
pub mod print;
pub mod span;
pub mod strings;
pub mod visit;

pub mod hack_bindgen;


#[cfg(test)]
mod tests;
