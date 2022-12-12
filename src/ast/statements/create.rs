use crate::ast::Identifier;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct CreateDatabaseStatement<'a> {
    pub if_not_exists: bool,
    pub database: Identifier<'a>,
    pub cluster: Option<Identifier<'a>>,
    pub engine: Option<DatabaseEngine>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum DatabaseEngine {}
