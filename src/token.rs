use logos::{Lexer, Logos, Span};

pub struct Token<'a> {
    pub source: &'a str,
    pub kind: TokenKind,
    pub span: Span,
}

impl<'a> Token<'a> {
    pub fn new_eoi_token(source: &'a str) -> Self {
        Self {
            source,
            kind: TokenKind::EOI,
            span: source.len()..source.len(),
        }
    }
}

pub struct Tokenizer<'a> {
    source: &'a str,
    lexer: Lexer<'a, TokenKind>,
    done: bool,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            lexer: TokenKind::lexer(source),
            done: false,
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token<'a>, &'static str>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.next() {
            Some(kind) if kind == TokenKind::Error => {
                let rest = Token {
                    source: self.source,
                    kind: TokenKind::Error,
                    span: self.lexer.span().start..self.source.len(),
                };
                Some(Err("syntax error"))
            }
            Some(kind) => Some(Ok(Token {
                source: self.source,
                kind,
                span: self.lexer.span(),
            })),
            None if !self.done => {
                self.done = true;
                Some(Ok(Token::new_eoi_token(self.source)))
            }

            None => None,
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Logos, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Misc
    #[error]
    Error,

    EOI,

    #[regex(r"/\*.?*\*/", logos::skip)]
    MultiLineComment,
    #[regex(r"--[^\t\n\f]*", logos::skip)]
    SingleLineComment,
    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    Whitespace,
    #[regex(r"[a-zA-Z_][0-9a-zA-Z_]*")]
    #[regex(r#""([^"\\]|\\.|""])*""#)]
    #[regex(r#"`([^`\\]|\\.|``])*`"#)]
    Identifier,
    #[regex(r#"'([^'\\]|\\.|''])*'"#)]
    QuotedString,

    #[regex(r"[0-9]+")]
    LiteralInteger,

    #[regex(r"[0-9]+[eE][+-]?[0-9]+")]
    #[regex(r"([0-9]*\.[0-9]+([eE][+-]?[0-9]+)?)|([0-9]+\.[0-9]*([eE][+-]?[0-9]+)?)")]
    LiteralFloat,

    // Tokens
    #[token("->")]
    Arrow,
    #[token("*")]
    Asterisk,
    #[token("`")]
    Backquote,
    #[token("\\")]
    Backslash,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("||")]
    Concat,
    #[token("-")]
    Dash,
    #[token(".")]
    Dot,
    #[token("==")]
    EqDouble,
    #[token("=")]
    EqSingle,
    #[token(">=")]
    GE,
    #[token(">")]
    GT,
    #[token("{")]
    LBrace,
    #[token("[")]
    LBracket,
    #[token("<=")]
    LE,
    #[token("(")]
    LParen,
    #[token("<")]
    LT,
    #[token("<>")]
    #[token("!=")]
    NotEq,
    #[token("%")]
    Percent,
    #[token("+")]
    Plus,
    #[token("?")]
    Query,
    #[token("}")]
    RBrace,
    #[token("]")]
    RBracket,
    #[token(")")]
    RParen,
    #[token(";")]
    SemiColon,
    #[token("_")]
    Underscore,

    // Keywords
    #[token("ADD", ignore(ascii_case))]
    ADD,
    #[token("AFTER", ignore(ascii_case))]
    AFTER,
    #[token("ALIAS", ignore(ascii_case))]
    ALIAS,
    #[token("ALL", ignore(ascii_case))]
    ALL,
    #[token("ALTER", ignore(ascii_case))]
    ALTER,
    #[token("AND", ignore(ascii_case))]
    AND,
    #[token("ANTI", ignore(ascii_case))]
    ANTI,
    #[token("ANY", ignore(ascii_case))]
    ANY,
    #[token("ARRAY", ignore(ascii_case))]
    ARRAY,
    #[token("AS", ignore(ascii_case))]
    AS,
    #[token("ASCENDING", ignore(ascii_case))]
    #[token("ASC", ignore(ascii_case))]
    ASCENDING,
    #[token("ASOF", ignore(ascii_case))]
    ASOF,
    #[token("AST", ignore(ascii_case))]
    AST,
    #[token("ASYNC", ignore(ascii_case))]
    ASYNC,
    #[token("ATTACH", ignore(ascii_case))]
    ATTACH,
    #[token("BETWEEN", ignore(ascii_case))]
    BETWEEN,
    #[token("BOTH", ignore(ascii_case))]
    BOTH,
    #[token("BY", ignore(ascii_case))]
    BY,
    #[token("CASE", ignore(ascii_case))]
    CASE,
    #[token("CAST", ignore(ascii_case))]
    CAST,
    #[token("CHECK", ignore(ascii_case))]
    CHECK,
    #[token("CLEAR", ignore(ascii_case))]
    CLEAR,
    #[token("CLUSTER", ignore(ascii_case))]
    CLUSTER,
    #[token("CODEC", ignore(ascii_case))]
    CODEC,
    #[token("COLLATE", ignore(ascii_case))]
    COLLATE,
    #[token("COLUMN", ignore(ascii_case))]
    COLUMN,
    #[token("COMMENT", ignore(ascii_case))]
    COMMENT,
    #[token("CONSTRAINT", ignore(ascii_case))]
    CONSTRAINT,
    #[token("CREATE", ignore(ascii_case))]
    CREATE,
    #[token("CROSS", ignore(ascii_case))]
    CROSS,
    #[token("CUBE", ignore(ascii_case))]
    CUBE,
    #[token("CURRENT", ignore(ascii_case))]
    CURRENT,
    #[token("DATABASE", ignore(ascii_case))]
    DATABASE,
    #[token("DATABASES", ignore(ascii_case))]
    DATABASES,
    #[token("DATE", ignore(ascii_case))]
    DATE,
    #[token("DAY", ignore(ascii_case))]
    DAY,
    #[token("DEDUPLICATE", ignore(ascii_case))]
    DEDUPLICATE,
    #[token("DEFAULT", ignore(ascii_case))]
    DEFAULT,
    #[token("DELAY", ignore(ascii_case))]
    DELAY,
    #[token("DELETE", ignore(ascii_case))]
    DELETE,
    #[token("DESCENDING", ignore(ascii_case))]
    #[token("DESC", ignore(ascii_case))]
    DESC,
    #[token("DESCRIBE", ignore(ascii_case))]
    DESCRIBE,
    #[token("DETACH", ignore(ascii_case))]
    DETACH,
    #[token("DICTIONARIES", ignore(ascii_case))]
    DICTIONARIES,
    #[token("DICTIONARY", ignore(ascii_case))]
    DICTIONARY,
    #[token("DISK", ignore(ascii_case))]
    DISK,
    #[token("DISTINCT", ignore(ascii_case))]
    DISTINCT,
    #[token("DISTRIBUTED", ignore(ascii_case))]
    DISTRIBUTED,
    #[token("DROP", ignore(ascii_case))]
    DROP,
    #[token("ELSE", ignore(ascii_case))]
    ELSE,
    #[token("END", ignore(ascii_case))]
    END,
    #[token("ENGINE", ignore(ascii_case))]
    ENGINE,
    #[token("EVENTS", ignore(ascii_case))]
    EVENTS,
    #[token("EXISTS", ignore(ascii_case))]
    EXISTS,
    #[token("EXPLAIN", ignore(ascii_case))]
    EXPLAIN,
    #[token("EXPRESSION", ignore(ascii_case))]
    EXPRESSION,
    #[token("EXTRACT", ignore(ascii_case))]
    EXTRACT,
    #[token("FETCHES", ignore(ascii_case))]
    FETCHES,
    #[token("FINAL", ignore(ascii_case))]
    FINAL,
    #[token("FIRST", ignore(ascii_case))]
    FIRST,
    #[token("FLUSH", ignore(ascii_case))]
    FLUSH,
    #[token("FOLLOWING", ignore(ascii_case))]
    FOLLOWING,
    #[token("FOR", ignore(ascii_case))]
    FOR,
    #[token("FORMAT", ignore(ascii_case))]
    FORMAT,
    #[token("FREEZE", ignore(ascii_case))]
    FREEZE,
    #[token("FROM", ignore(ascii_case))]
    FROM,
    #[token("FULL", ignore(ascii_case))]
    FULL,
    #[token("FUNCTION", ignore(ascii_case))]
    FUNCTION,
    #[token("GLOBAL", ignore(ascii_case))]
    GLOBAL,
    #[token("GRANULARITY", ignore(ascii_case))]
    GRANULARITY,
    #[token("GROUP", ignore(ascii_case))]
    GROUP,
    #[token("HAVING", ignore(ascii_case))]
    HAVING,
    #[token("HIERARCHICAL", ignore(ascii_case))]
    HIERARCHICAL,
    #[token("HOUR", ignore(ascii_case))]
    HOUR,
    #[token("ID", ignore(ascii_case))]
    ID,
    #[token("IF", ignore(ascii_case))]
    IF,
    #[token("ILIKE", ignore(ascii_case))]
    ILIKE,
    #[token("IN", ignore(ascii_case))]
    IN,
    #[token("INDEX", ignore(ascii_case))]
    INDEX,
    #[token("INFINITY", ignore(ascii_case))]
    #[token("INF", ignore(ascii_case))]
    INF,
    #[token("INJECTIVE", ignore(ascii_case))]
    INJECTIVE,
    #[token("INNER", ignore(ascii_case))]
    INNER,
    #[token("INSERT", ignore(ascii_case))]
    INSERT,
    #[token("INTERVAL", ignore(ascii_case))]
    INTERVAL,
    #[token("INTO", ignore(ascii_case))]
    INTO,
    #[token("IS", ignore(ascii_case))]
    IS,
    #[token("IS_OBJECT_ID", ignore(ascii_case))]
    IS_OBJECT_ID,
    #[token("JOIN", ignore(ascii_case))]
    JOIN,
    #[token("KEY", ignore(ascii_case))]
    KEY,
    #[token("KILL", ignore(ascii_case))]
    KILL,
    #[token("LAST", ignore(ascii_case))]
    LAST,
    #[token("LAYOUT", ignore(ascii_case))]
    LAYOUT,
    #[token("LEADING", ignore(ascii_case))]
    LEADING,
    #[token("LEFT", ignore(ascii_case))]
    LEFT,
    #[token("LIFETIME", ignore(ascii_case))]
    LIFETIME,
    #[token("LIKE", ignore(ascii_case))]
    LIKE,
    #[token("LIMIT", ignore(ascii_case))]
    LIMIT,
    #[token("LIVE", ignore(ascii_case))]
    LIVE,
    #[token("LOCAL", ignore(ascii_case))]
    LOCAL,
    #[token("LOGS", ignore(ascii_case))]
    LOGS,
    #[token("MATERIALIZE", ignore(ascii_case))]
    MATERIALIAZE,
    #[token("MATERIALIZED", ignore(ascii_case))]
    MATERIALIAZED,
    #[token("MAX", ignore(ascii_case))]
    MAX,
    #[token("MERGES", ignore(ascii_case))]
    MERGES,
    #[token("MIN", ignore(ascii_case))]
    MIN,
    #[token("MINUTE", ignore(ascii_case))]
    MINUTE,
    #[token("MODIFY", ignore(ascii_case))]
    MODIFY,
    #[token("MONTH", ignore(ascii_case))]
    MONTH,
    #[token("MOVE", ignore(ascii_case))]
    MOVE,
    #[token("MUTATION", ignore(ascii_case))]
    MUTATION,
    #[token("NAN", ignore(ascii_case))]
    NAN_SQL,
    #[token("NO", ignore(ascii_case))]
    NO,
    #[token("NOT", ignore(ascii_case))]
    NOT,
    #[token("NULL", ignore(ascii_case))]
    NULL_SQL,
    #[token("NULLS", ignore(ascii_case))]
    NULLS,
    #[token("OFFSET", ignore(ascii_case))]
    OFFSET,
    #[token("ON", ignore(ascii_case))]
    ON,
    #[token("OPTIMIZE", ignore(ascii_case))]
    OPTIMIZE,
    #[token("OR", ignore(ascii_case))]
    OR,
    #[token("ORDER", ignore(ascii_case))]
    ORDER,
    #[token("OUTER", ignore(ascii_case))]
    OUTER,
    #[token("OUTFILE", ignore(ascii_case))]
    OUTFILE,
    #[token("OVER", ignore(ascii_case))]
    OVER,
    #[token("PARTITION", ignore(ascii_case))]
    PARTITION,
    #[token("POPULATE", ignore(ascii_case))]
    POPULATE,
    #[token("PRECEDING", ignore(ascii_case))]
    PRECEDING,
    #[token("PREWHERE", ignore(ascii_case))]
    PREWHERE,
    #[token("PRIMARY", ignore(ascii_case))]
    PRIMARY,
    #[token("PROJECTION", ignore(ascii_case))]
    PROJECTION,
    #[token("QUARTER", ignore(ascii_case))]
    QUARTER,
    #[token("RANGE", ignore(ascii_case))]
    RANGE,
    #[token("RELOAD", ignore(ascii_case))]
    RELOAD,
    #[token("REMOVE", ignore(ascii_case))]
    REMOVE,
    #[token("RENAME", ignore(ascii_case))]
    RENAME,
    #[token("REPLACE", ignore(ascii_case))]
    REPLACE,
    #[token("REPLICA", ignore(ascii_case))]
    REPLICA,
    #[token("REPLICATED", ignore(ascii_case))]
    REPLICATED,
    #[token("RIGHT", ignore(ascii_case))]
    RIGHT,
    #[token("ROLLUP", ignore(ascii_case))]
    ROLLUP,
    #[token("ROW", ignore(ascii_case))]
    ROW,
    #[token("ROWS", ignore(ascii_case))]
    ROWS,
    #[token("SAMPLE", ignore(ascii_case))]
    SAMPLE,
    #[token("SECOND", ignore(ascii_case))]
    SECOND,
    #[token("SELECT", ignore(ascii_case))]
    SELECT,
    #[token("SEMI", ignore(ascii_case))]
    SEMI,
    #[token("SENDS", ignore(ascii_case))]
    SENDS,
    #[token("SET", ignore(ascii_case))]
    SET,
    #[token("SETTINGS", ignore(ascii_case))]
    SETTINGS,
    #[token("SHOW", ignore(ascii_case))]
    SHOW,
    #[token("SOURCE", ignore(ascii_case))]
    SOURCE,
    #[token("START", ignore(ascii_case))]
    START,
    #[token("STOP", ignore(ascii_case))]
    STOP,
    #[token("SUBSTRING", ignore(ascii_case))]
    SUBSTRING,
    #[token("SYNC", ignore(ascii_case))]
    SYNC,
    #[token("SYNTAX", ignore(ascii_case))]
    SYNTAX,
    #[token("SYSTEM", ignore(ascii_case))]
    SYSTEM,
    #[token("TABLE", ignore(ascii_case))]
    TABLE,
    #[token("TABLES", ignore(ascii_case))]
    TABLES,
    #[token("TEMPORARY", ignore(ascii_case))]
    TEMPORARY,
    #[token("TEST", ignore(ascii_case))]
    TEST,
    #[token("THEN", ignore(ascii_case))]
    THEN,
    #[token("TIES", ignore(ascii_case))]
    TIES,
    #[token("TIMEOUT", ignore(ascii_case))]
    TIMEOUT,
    #[token("TIMESTAMP", ignore(ascii_case))]
    TIMSTAMP,
    #[token("TO", ignore(ascii_case))]
    TO,
    #[token("TOP", ignore(ascii_case))]
    TOP,
    #[token("TOTALS", ignore(ascii_case))]
    TOTALS,
    #[token("TRAILING", ignore(ascii_case))]
    TRAILING,
    #[token("TRIM", ignore(ascii_case))]
    TRIM,
    #[token("TRUNCATE", ignore(ascii_case))]
    TRUNCATE,
    #[token("TTL", ignore(ascii_case))]
    TTL,
    #[token("TYPE", ignore(ascii_case))]
    TYPE,
    #[token("UNBOUNDED", ignore(ascii_case))]
    UNBOUNDED,
    #[token("UNION", ignore(ascii_case))]
    UNION,
    #[token("UPDATE", ignore(ascii_case))]
    UPDATE,
    #[token("USE", ignore(ascii_case))]
    USE,
    #[token("USING", ignore(ascii_case))]
    USING,
    #[token("UUID", ignore(ascii_case))]
    UUID,
    #[token("VALUES", ignore(ascii_case))]
    VALUES,
    #[token("VIEW", ignore(ascii_case))]
    VIEW,
    #[token("VOLUME", ignore(ascii_case))]
    VOLUME,
    #[token("WATCH", ignore(ascii_case))]
    WATCH,
    #[token("WEEK", ignore(ascii_case))]
    WEEK,
    #[token("WHEN", ignore(ascii_case))]
    WHEN,
    #[token("WHERE", ignore(ascii_case))]
    WHERE,
    #[token("WINDOW", ignore(ascii_case))]
    WINDOW,
    #[token("WITH", ignore(ascii_case))]
    WITH,
    #[token("YYYY", ignore(ascii_case))]
    #[token("YEAR", ignore(ascii_case))]
    YEAR,
}
