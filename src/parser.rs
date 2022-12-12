use crate::token::{Token, Tokenizer};

pub fn tokenize_sql(query: &str) -> Result<Vec<Token>, &'static str> {
    Tokenizer::new(query).collect::<Result<Vec<_>, _>>()
}
