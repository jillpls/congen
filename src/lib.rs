#![warn(clippy::all, rust_2018_idioms)]

mod app;
mod generation;
mod raw_data;
mod rewrite;
mod sound_change;
pub mod sounds;
mod word;

pub use app::WrapperApp;
use std::fmt::Formatter;
use std::num::ParseIntError;

#[derive(Clone, Debug)]
pub enum ConGenError {
    GenericParseError(String),
    TodoExplained(String),
    Todo,
}

pub const APP_VERSION: &str = env!("CARGO_PKG_VERSION");

pub type ConGenResult<T> = Result<T, ConGenError>;

impl std::fmt::Display for ConGenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConGenError::GenericParseError(s) => write!(f, "Failed to parse ({})", s),
            ConGenError::TodoExplained(s) => write!(f, "{}", s),
            _ => {
                todo!()
            }
        }
    }
}

impl From<ParseIntError> for ConGenError {
    fn from(value: ParseIntError) -> Self {
        Self::GenericParseError(value.to_string())
    }
}
