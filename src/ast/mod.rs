mod statements;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Identifier<'a> {
    pub name: &'a str,
}
