//! Token types for the `.mont` DSL.
//!
//! Tokens carry borrowed source slices (`&'src str`) for zero-copy lexing.
//! The parser stage maps these into owned AST nodes via `.to_string()`.

use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    // Keywords
    Type,      // `Type` (legacy `Type = A | B.`)
    TypeKw,    // `type` (new `type A.`)
    SortKw,    // `sort`
    Morph,     // `MORPH`
    Strips,    // `STRIPS`
    Namespace, // `namespace`
    Extend,    // `extend`
    Import,    // `import`
    As,        // `as`
    Forall,    // `forall`
    Exists,    // `exists`

    // Operators / punctuation
    Eq,         // `=`
    Pipe,       // `|`
    End,        // `.` (statement terminator)
    Subtype,    // `:<`
    ColonColon, // `::` (semantic term separator)
    Colon,      // `:`
    Slash,      // `/`
    Backslash,  // `\`
    RArrow,     // `->`
    Arrow,      // `-->`
    LParen,     // `(`
    RParen,     // `)`
    LBracket,   // `[`
    RBracket,   // `]`
    Comma,      // `,`
    Dot,        // `.` (namespace separator, when followed by ident-start)

    // Logic / semantic-term operators
    Lambda,  // `λ`
    And,     // `∧` or `/\`
    Or,      // `∨` or `\/`
    Not,     // `¬` or `~`
    Implies, // `→` or `=>`

    // Data tokens
    Ident(&'src str),
    DocString(&'src str),
    /// Double-quoted string for multi-word PROD entries like `"as well as"`.
    QuotedString(&'src str),
    /// `+s`, `+ing`, `+'s` — morpheme surface forms.
    PlusIdent(&'src str),
    /// Integer literal (semantic terms).
    IntLit(i64),
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Type => write!(f, "Type"),
            Token::TypeKw => write!(f, "type"),
            Token::SortKw => write!(f, "sort"),
            Token::Morph => write!(f, "MORPH"),
            Token::Strips => write!(f, "STRIPS"),
            Token::Namespace => write!(f, "namespace"),
            Token::Extend => write!(f, "extend"),
            Token::Import => write!(f, "import"),
            Token::As => write!(f, "as"),
            Token::Forall => write!(f, "forall"),
            Token::Exists => write!(f, "exists"),
            Token::Eq => write!(f, "="),
            Token::Pipe => write!(f, "|"),
            Token::End | Token::Dot => write!(f, "."),
            Token::Subtype => write!(f, ":<"),
            Token::ColonColon => write!(f, "::"),
            Token::Colon => write!(f, ":"),
            Token::Slash => write!(f, "/"),
            Token::Backslash => write!(f, "\\"),
            Token::RArrow => write!(f, "->"),
            Token::Arrow => write!(f, "-->"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Comma => write!(f, ","),
            Token::Lambda => write!(f, "λ"),
            Token::And => write!(f, "∧"),
            Token::Or => write!(f, "∨"),
            Token::Not => write!(f, "¬"),
            Token::Implies => write!(f, "→"),
            Token::Ident(s) => write!(f, "{s}"),
            Token::DocString(s) => write!(f, "-- | {s}"),
            Token::QuotedString(s) => write!(f, "\"{s}\""),
            Token::PlusIdent(s) => write!(f, "{s}"),
            Token::IntLit(n) => write!(f, "{n}"),
        }
    }
}
