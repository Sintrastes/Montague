//! Hand-rolled recursive-descent parser for the `.mont` DSL.
//!
//! The grammar is ~50 lines of BNF — a hand-written parser with explicit
//! span tracking is more maintainable here than a parser-combinator library
//! whose pre-1.0 API changes between versions.

use crate::ast::*;
use crate::error::MontParseError;

// ---------------------------------------------------------------------------
// Token types
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Type,
    ColonEq,
    Pipe,
    End,
    Subtype,
    Colon,
    Slash,
    Backslash,
    RArrow,
    Arrow,
    LParen,
    RParen,
    Comma,
    Import,
    As,
    Ident(String),
    DocString(String),
    /// Double-quoted string for multi-word PROD entries like `"as well as"`.
    QuotedString(String),
    /// `MORPH` keyword.
    Morph,
    /// `STRIPS` keyword.
    Strips,
    /// `.` used as a namespace separator (mid-line, followed by non-whitespace).
    Dot,
    /// `namespace` keyword.
    Namespace,
    /// `extend` keyword.
    Extend,
    /// Lowercase `type` keyword — for single-type declarations: `type A.`
    TypeKw,
}

struct TokenWithSpan {
    token: Token,
    span: Span,
}

// ---------------------------------------------------------------------------
// Scanner
// ---------------------------------------------------------------------------

struct Scanner<'a> {
    src: &'a str,
    pos: usize,
}

impl<'a> Scanner<'a> {
    fn new(src: &'a str) -> Self {
        Scanner { src, pos: 0 }
    }

    fn skip_ws(&mut self) {
        loop {
            while self.pos < self.src.len() {
                let c = self.src.as_bytes()[self.pos];
                if c == b' ' || c == b'\t' || c == b'\n' || c == b'\r' {
                    self.pos += 1;
                } else {
                    break;
                }
            }
            if self.pos + 1 < self.src.len() && &self.src[self.pos..self.pos + 2] == "--" {
                if self.pos + 2 < self.src.len() {
                    let third = self.src.as_bytes()[self.pos + 2];
                    if third == b'>' {
                        break;
                    } // -->
                    if third == b' '
                        && self.pos + 3 < self.src.len()
                        && self.src.as_bytes()[self.pos + 3] == b'|'
                    {
                        break;
                    } // -- |
                }
                while self.pos < self.src.len() && self.src.as_bytes()[self.pos] != b'\n' {
                    self.pos += 1;
                }
                continue;
            }
            break;
        }
    }

    fn next(&mut self) -> Option<TokenWithSpan> {
        self.skip_ws();
        if self.pos >= self.src.len() {
            return None;
        }
        let start = self.pos;

        // doc string
        if self.pos + 3 < self.src.len() && &self.src[self.pos..self.pos + 4] == "-- |" {
            self.pos += 4;
            let cs = self.pos;
            while self.pos < self.src.len() && self.src.as_bytes()[self.pos] != b'\n' {
                self.pos += 1;
            }
            return Some(TokenWithSpan {
                token: Token::DocString(self.src[cs..self.pos].trim().to_string()),
                span: Span::new(start, self.pos),
            });
        }

        // multi-char ops
        if self.pos + 1 < self.src.len() {
            let two = &self.src[self.pos..self.pos + 2];
            match two {
                ":<" => {
                    self.pos += 2;
                    return Some(TokenWithSpan {
                        token: Token::Subtype,
                        span: Span::new(start, self.pos),
                    });
                }
                "->" => {
                    self.pos += 2;
                    return Some(TokenWithSpan {
                        token: Token::RArrow,
                        span: Span::new(start, self.pos),
                    });
                }
                _ => {}
            }
        }
        if self.pos + 2 < self.src.len() && &self.src[self.pos..self.pos + 3] == "-->" {
            self.pos += 3;
            return Some(TokenWithSpan {
                token: Token::Arrow,
                span: Span::new(start, self.pos),
            });
        }

        // single char
        let c = self.src.as_bytes()[self.pos] as char;
        match c {
            '=' => {
                self.pos += 1;
                Some(TokenWithSpan {
                    token: Token::ColonEq,
                    span: Span::new(start, self.pos),
                })
            }
            '|' => {
                self.pos += 1;
                Some(TokenWithSpan {
                    token: Token::Pipe,
                    span: Span::new(start, self.pos),
                })
            }
            '.' => {
                self.pos += 1;
                // Disambiguate: if followed by whitespace-then-newline/EOF/comment
                // → End (statement terminator); otherwise → Dot (namespace sep).
                let mut peek = self.pos;
                while peek < self.src.len()
                    && (self.src.as_bytes()[peek] == b' ' || self.src.as_bytes()[peek] == b'\t')
                {
                    peek += 1;
                }
                let is_end = peek >= self.src.len()
                    || self.src.as_bytes()[peek] == b'\n'
                    || self.src.as_bytes()[peek] == b'\r'
                    || (peek + 1 < self.src.len()
                        && self.src.as_bytes()[peek] == b'-'
                        && self.src.as_bytes()[peek + 1] == b'-');
                Some(TokenWithSpan {
                    token: if is_end { Token::End } else { Token::Dot },
                    span: Span::new(start, self.pos),
                })
            }
            ':' => {
                self.pos += 1;
                Some(TokenWithSpan {
                    token: Token::Colon,
                    span: Span::new(start, self.pos),
                })
            }
            '/' => {
                self.pos += 1;
                Some(TokenWithSpan {
                    token: Token::Slash,
                    span: Span::new(start, self.pos),
                })
            }
            '\\' => {
                self.pos += 1;
                Some(TokenWithSpan {
                    token: Token::Backslash,
                    span: Span::new(start, self.pos),
                })
            }
            '(' => {
                self.pos += 1;
                Some(TokenWithSpan {
                    token: Token::LParen,
                    span: Span::new(start, self.pos),
                })
            }
            ')' => {
                self.pos += 1;
                Some(TokenWithSpan {
                    token: Token::RParen,
                    span: Span::new(start, self.pos),
                })
            }
            ',' => {
                self.pos += 1;
                Some(TokenWithSpan {
                    token: Token::Comma,
                    span: Span::new(start, self.pos),
                })
            }
            '"' => {
                self.pos += 1; // consume opening "
                let cs = self.pos;
                while self.pos < self.src.len() {
                    if self.src.as_bytes()[self.pos] == b'"' {
                        let content = self.src[cs..self.pos].to_string();
                        self.pos += 1; // consume closing "
                        return Some(TokenWithSpan {
                            token: Token::QuotedString(content),
                            span: Span::new(start, self.pos),
                        });
                    }
                    self.pos += 1;
                }
                // Unterminated quote — treat rest as content
                let content = self.src[cs..].to_string();
                Some(TokenWithSpan {
                    token: Token::QuotedString(content),
                    span: Span::new(start, self.pos),
                })
            }
            '+' => {
                self.pos += 1; // consume +
                while self.pos < self.src.len() {
                    let ch = self.src.as_bytes()[self.pos] as char;
                    if ch.is_alphanumeric() || ch == '\'' || ch == '_' {
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                let s = self.src[start..self.pos].to_string();
                Some(TokenWithSpan {
                    token: Token::Ident(s),
                    span: Span::new(start, self.pos),
                })
            }
            _c if _c.is_alphabetic() => {
                while self.pos < self.src.len() {
                    let ch = self.src.as_bytes()[self.pos] as char;
                    if ch.is_alphanumeric() || ch == '_' {
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                let s = self.src[start..self.pos].to_string();
                let tok = match s.as_str() {
                    "Type" => Token::Type,
                    "import" => Token::Import,
                    "as" => Token::As,
                    "MORPH" => Token::Morph,
                    "STRIPS" => Token::Strips,
                    "namespace" => Token::Namespace,
                    "extend" => Token::Extend,
                    "type" => Token::TypeKw,
                    _ => Token::Ident(s),
                };
                Some(TokenWithSpan {
                    token: tok,
                    span: Span::new(start, self.pos),
                })
            }
            _ => {
                self.pos += 1;
                self.next()
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

struct Parser<'a> {
    tokens: &'a [TokenWithSpan],
    pos: usize,
    errors: Vec<MontParseError>,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos).map(|t| &t.token)
    }

    fn eat(&mut self, expected: Token) -> bool {
        match self.peek() {
            Some(t) if *t == expected => {
                self.pos += 1;
                true
            }
            _ => false,
        }
    }

    // identifiers
    fn ident(&mut self) -> Option<Spanned<String>> {
        match self.tokens.get(self.pos) {
            Some(TokenWithSpan {
                token: Token::Ident(s),
                span,
            }) => {
                let r = Spanned::new(s.clone(), *span);
                self.pos += 1;
                Some(r)
            }
            _ => None,
        }
    }

    fn type_ident(&mut self) -> Option<Spanned<String>> {
        let t = self.ident()?;
        if t.item.starts_with(|c: char| c.is_uppercase()) {
            Some(t)
        } else {
            self.errors.push(MontParseError::InvalidTypeName {
                found: t.item.clone(),
                span: t.span,
            });
            Some(t)
        }
    }

    fn entity_ident(&mut self) -> Option<Spanned<String>> {
        let t = self.ident()?;
        if t.item.starts_with(|c: char| c.is_lowercase()) {
            Some(t)
        } else {
            self.errors.push(MontParseError::InvalidEntityName {
                found: t.item.clone(),
                span: t.span,
            });
            Some(t)
        }
    }

    // type expressions
    fn type_atom(&mut self) -> Option<Spanned<TypeExpr>> {
        let id = self.type_ident()?;
        let start = id.span.start;
        if self.eat(Token::LParen) {
            let mut args = Vec::new();
            loop {
                if matches!(self.peek(), Some(Token::RParen)) {
                    break;
                }
                if let Some(a) = self.type_expr() {
                    args.push(a);
                } else {
                    break;
                }
                if !self.eat(Token::Comma) {
                    break;
                }
            }
            let end = self.peek_span().unwrap_or(id.span.end);
            self.eat(Token::RParen);
            Some(Spanned::new(
                TypeExpr::CustomApp {
                    path: vec![id.item],
                    args,
                },
                Span::new(start, end),
            ))
        } else {
            Some(Spanned::new(TypeExpr::TypeIdent(id.item), id.span))
        }
    }

    fn peek_span(&self) -> Option<usize> {
        self.tokens.get(self.pos).map(|t| t.span.end)
    }

    fn type_expr(&mut self) -> Option<Spanned<TypeExpr>> {
        // Parse primary (atom or parenthesized expr)
        let mut left = if self.eat(Token::LParen) {
            let inner = self.type_expr()?;
            self.eat(Token::RParen);
            inner
        } else {
            self.type_atom()?
        };

        // Now check for operators (left-associative chain)
        loop {
            let op = match self.peek() {
                Some(Token::Slash) => Some("slash"),
                Some(Token::Backslash) | Some(Token::RArrow) => Some("backslash"),
                Some(Token::Pipe) => Some("pipe"),
                _ => None,
            };
            match op {
                Some(op_name) => {
                    self.pos += 1;
                    let right = self.type_expr()?;
                    let span = Span::new(left.span.start, right.span.end);
                    left = Spanned::new(
                        match op_name {
                            "slash" => TypeExpr::LeftArrow(Box::new(left), Box::new(right)),
                            "backslash" => TypeExpr::RightArrow(Box::new(left), Box::new(right)),
                            "pipe" => TypeExpr::Union(Box::new(left), Box::new(right)),
                            _ => unreachable!(),
                        },
                        span,
                    );
                }
                None => break,
            }
        }
        Some(left)
    }

    // declarations
    fn type_decl(&mut self) -> Option<Spanned<Declaration>> {
        let start = self.tokens.get(self.pos)?.span.start;
        self.eat(Token::Type);
        self.eat(Token::ColonEq);
        let mut types = Vec::new();
        while let Some(t) = self.type_ident() {
            types.push(t.item);
            if !self.eat(Token::Pipe) {
                break;
            }
        }
        let end = self.peek_span().unwrap_or(start);
        self.eat(Token::End);
        Some(Spanned::new(
            Declaration::TypeDecl(types),
            Span::new(start, end),
        ))
    }

    fn subtype_decl(&mut self) -> Option<Spanned<Declaration>> {
        let sub = self.type_ident()?;
        let start = sub.span.start;
        self.eat(Token::Subtype);
        let sup = self.type_ident()?;
        let end = sup.span.end;
        self.eat(Token::End);
        Some(Spanned::new(
            Declaration::SubtypeDecl {
                sub: sub.item,
                sup: sup.item,
            },
            Span::new(start, end),
        ))
    }

    fn atom_decl(&mut self) -> Option<Spanned<Declaration>> {
        let start = self.peek_span().unwrap_or(0);
        let doc = match self.tokens.get(self.pos) {
            Some(TokenWithSpan {
                token: Token::DocString(d),
                ..
            }) => {
                let d = d.clone();
                self.pos += 1;
                Some(d)
            }
            _ => None,
        };
        let entity = self.entity_ident()?;
        self.eat(Token::Colon);
        let ty = self.type_expr()?;
        let end = ty.span.end;
        self.eat(Token::End);
        Some(Spanned::new(
            Declaration::AtomDecl {
                doc,
                entity: entity.item,
                ty,
            },
            Span::new(start, end),
        ))
    }

    /// Parse a surface word in a production LHS — either a single identifier
    /// or a double-quoted multi-word phrase. Multi-word phrases have internal
    /// spaces replaced with underscores for canonical single-token matching.
    fn prod_word(&mut self) -> Option<Spanned<String>> {
        match self.tokens.get(self.pos) {
            Some(TokenWithSpan {
                token: Token::QuotedString(s),
                span,
            }) => {
                let word = s.replace(' ', "_");
                let r = Spanned::new(word, *span);
                self.pos += 1;
                Some(r)
            }
            _ => self.entity_ident(),
        }
    }

    fn production_decl(&mut self) -> Option<Spanned<Declaration>> {
        let mut words = Vec::new();
        let first = self.prod_word()?;
        let start = first.span.start;
        words.push(first.item);
        while self.eat(Token::Comma) {
            if let Some(w) = self.prod_word() {
                words.push(w.item);
            } else {
                break;
            }
        }
        self.eat(Token::Arrow);
        let entity = self.ident()?;
        let end = entity.span.end;
        self.eat(Token::End);
        Some(Spanned::new(
            Declaration::ProductionDecl {
                words,
                entity: entity.item,
            },
            Span::new(start, end),
        ))
    }

    fn declaration(&mut self) -> Option<Spanned<Declaration>> {
        match self.peek() {
            Some(Token::TypeKw) => self.single_type_decl(),
            Some(Token::Morph) => self.morpheme_decl(),
            Some(Token::Type) => self.type_decl(),
            Some(Token::Namespace) => self.namespace_decl(),
            Some(Token::Ident(_)) => {
                if self.pos + 1 < self.tokens.len() {
                    match &self.tokens[self.pos + 1].token {
                        Token::Subtype => self.subtype_decl(),
                        Token::Colon => self.atom_decl(),
                        Token::Comma | Token::Arrow => self.production_decl(),
                        Token::Dot => self.namespace_or_atom_decl(),
                        _ => self.atom_decl(),
                    }
                } else {
                    None
                }
            }
            Some(Token::DocString(_)) => self.atom_decl(),
            Some(Token::QuotedString(_)) => self.production_decl(),
            _ => None,
        }
    }

    /// `type <TypeIdent>.`  Backtracks on failure so `type` can be an entity name.
    fn single_type_decl(&mut self) -> Option<Spanned<Declaration>> {
        let saved = self.pos;
        let start = self.peek_span().unwrap_or(0);
        self.eat(Token::TypeKw);
        let t = match self.type_ident() {
            Some(t) => t,
            None => {
                self.pos = saved;
                return None;
            }
        };
        let end = t.span.end;
        self.eat(Token::End);
        Some(Spanned::new(Declaration::SingleTypeDecl(t.item), Span::new(start, end)))
    }

    /// `namespace <ident> (. <ident>)* .`
    fn namespace_decl(&mut self) -> Option<Spanned<Declaration>> {
        let start = self.peek_span().unwrap_or(0);
        self.eat(Token::Namespace);
        let mut parts = vec![self.ident()?.item.clone()];
        while self.eat(Token::Dot) {
            if let Some(id) = self.ident() {
                parts.push(id.item.clone());
            } else { break; }
        }
        let end = self.peek_span().unwrap_or(start);
        self.eat(Token::End);
        Some(Spanned::new(Declaration::NamespaceDecl(parts), Span::new(start, end)))
    }

    /// Ident + Dot — ambiguous: could be namespace decl or qualified atom.
    fn namespace_or_atom_decl(&mut self) -> Option<Spanned<Declaration>> {
        let saved = self.pos;
        let mut parts = vec![self.ident()?.item.clone()];
        let start_span = self.tokens.get(saved).map(|t| t.span).unwrap_or(Span::new(0, 0));
        while self.eat(Token::Dot) {
            if let Some(id) = self.ident() {
                parts.push(id.item.clone());
            } else { break; }
        }
        if matches!(self.peek(), Some(Token::Colon)) {
            self.pos = saved;
            self.atom_decl()
        } else {
            let end = self.peek_span().unwrap_or(start_span.end);
            self.eat(Token::End);
            Some(Spanned::new(Declaration::NamespaceDecl(parts), Span::new(start_span.start, end)))
        }
    }

    /// `extend <ident> (. <ident>)* .`  or  `extend by "<uri>".`
    /// Backtracks on failure so `extend` can be used as an entity name in atom/prod decls.
    fn extend_decl(&mut self) -> Option<Spanned<Directive>> {
        let saved = self.pos;
        let start = self.peek_span().unwrap_or(0);
        self.eat(Token::Extend);
        // Check for "by" as a regular ident (not a keyword)
        if let Some(TokenWithSpan { token: Token::Ident(s), .. }) = self.tokens.get(self.pos) {
            if s == "by" {
                self.pos += 1;
                let uri = match self.tokens.get(self.pos) {
                    Some(TokenWithSpan { token: Token::QuotedString(s), .. }) => {
                        let s = s.clone();
                        self.pos += 1;
                        s
                    }
                    _ => {
                        self.errors.push(MontParseError::UnexpectedToken {
                            expected: "quoted URI string".into(),
                            found: format!("{:?}", self.peek()),
                            span: Span::new(start, self.pos),
                        });
                        self.pos = saved;
                        return None;
                    }
                };
                let end = self.peek_span().unwrap_or(start);
                self.eat(Token::End);
                return Some(Spanned::new(Directive::ExtendBy { uri }, Span::new(start, end)));
            }
        }
        let id = match self.ident() {
            Some(id) => id,
            None => {
                self.pos = saved;
                return None;
            }
        };
        let mut parts = vec![id.item.clone()];
        while self.eat(Token::Dot) {
            if let Some(id) = self.ident() {
                parts.push(id.item.clone());
            } else { break; }
        }
        let end = self.peek_span().unwrap_or(start);
        self.eat(Token::End);
        Some(Spanned::new(Directive::Extend { namespace: parts }, Span::new(start, end)))
    }

    fn morpheme_decl(&mut self) -> Option<Spanned<Declaration>> {
        let start = self.peek_span().unwrap_or(0);
        self.eat(Token::Morph);
        let surface = self.ident()?;
        if !surface.item.starts_with('+') {
            self.errors.push(MontParseError::InvalidEntityName {
                found: surface.item.clone(),
                span: surface.span,
            });
        }
        self.eat(Token::Colon);
        let ty = self.type_expr()?;
        let mut strips = Vec::new();
        if self.eat(Token::Strips) {
            loop {
                let class = self.ident()?;
                match SpellingClass::from_str(&class.item) {
                    Some(c) => strips.push(c),
                    None => {
                        self.errors.push(MontParseError::UnexpectedToken {
                            expected: "strips class (e, CC, y_i, 's)".into(),
                            found: class.item.clone(),
                            span: class.span,
                        });
                    }
                }
                if !self.eat(Token::Comma) {
                    break;
                }
            }
        }
        let end = self.peek_span().unwrap_or(ty.span.end);
        self.eat(Token::End);
        Some(Spanned::new(
            Declaration::MorphemeDecl {
                surface: surface.item,
                ty,
                strips,
            },
            Span::new(start, end),
        ))
    }

    fn directive(&mut self) -> Option<Spanned<Directive>> {
        self.eat(Token::Import);
        let first = self.ident()?;
        let start = first.span.start;
        let path = vec![first.item];
        let mut alias = None;
        if self.eat(Token::As) {
            if let Some(a) = self.ident() {
                alias = Some(a.item);
            }
        }
        let end_s = self.peek_span().unwrap_or(start);
        self.eat(Token::End);
        Some(Spanned::new(
            Directive::Import { path, alias },
            Span::new(start, end_s),
        ))
    }

    fn parse_file(&mut self) -> MontFile {
        let mut directives = Vec::new();
        let mut declarations = Vec::new();
        loop {
            match self.peek() {
                Some(Token::Import) => {
                    if let Some(d) = self.directive() {
                        directives.push(d);
                    }
                }
                Some(Token::Extend) => {
                    if let Some(d) = self.extend_decl() {
                        directives.push(d);
                    } else {
                        // extend_decl backtracked — "extend" used as entity name,
                        // not as a directive.  Skip the keyword token so the
                        // remaining tokens (e.g. type expression) don't leak.
                        self.pos += 1;
                    }
                }
                Some(_) => {
                    if let Some(d) = self.declaration() {
                        declarations.push(d);
                    } else {
                        self.pos += 1;
                    }
                }
                None => break,
            }
        }
        MontFile {
            directives,
            declarations,
        }
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

pub fn parse(src: &str) -> (MontFile, Vec<MontParseError>) {
    let mut scanner = Scanner::new(src);
    let mut tokens = Vec::new();
    while let Some(tok) = scanner.next() {
        tokens.push(tok);
    }
    let mut parser = Parser {
        tokens: &tokens,
        pos: 0,
        errors: Vec::new(),
    };
    let file = parser.parse_file();
    (file, parser.errors)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn check(src: &str) -> MontFile {
        let (f, errs) = parse(src);
        assert!(errs.is_empty(), "errors: {errs:?}");
        f
    }

    #[test]
    fn parse_empty() {
        let (f, e) = parse("");
        assert!(e.is_empty());
        assert!(f.declarations.is_empty());
    }
    #[test]
    fn parse_type_decl() {
        let f = check("Type = Noun | Sentence | Person.");
        match &f.declarations[0].item {
            Declaration::TypeDecl(ts) => assert_eq!(ts, &["Noun", "Sentence", "Person"]),
            _ => panic!(),
        }
    }
    #[test]
    fn parse_subtype_decl() {
        let f = check("Person :< Noun.");
        match &f.declarations[0].item {
            Declaration::SubtypeDecl { sub, sup } => {
                assert_eq!(sub, "Person");
                assert_eq!(sup, "Noun");
            }
            _ => panic!(),
        }
    }
    #[test]
    fn parse_atom_decl() {
        let f = check("nate: Person.");
        match &f.declarations[0].item {
            Declaration::AtomDecl { entity, ty, .. } => {
                assert_eq!(entity, "nate");
                assert!(matches!(ty.item,TypeExpr::TypeIdent(ref s) if s=="Person"));
            }
            _ => panic!(),
        }
    }
    #[test]
    fn parse_atom_with_doc() {
        let f = check("-- | The philosopher\nsocrates: Person.");
        match &f.declarations[0].item {
            Declaration::AtomDecl { doc, entity, .. } => {
                assert_eq!(doc.as_deref(), Some("The philosopher"));
                assert_eq!(entity, "socrates");
            }
            _ => panic!(),
        }
    }
    #[test]
    fn parse_production_decl() {
        let f = check("like, love --> Like.");
        match &f.declarations[0].item {
            Declaration::ProductionDecl { words, entity } => {
                assert_eq!(words, &["like", "love"]);
                assert_eq!(entity, "Like");
            }
            _ => panic!(),
        }
    }
    #[test]
    fn parse_quoted_multiword_production() {
        let f = check(r#""as well as" --> as_well_as."#);
        match &f.declarations[0].item {
            Declaration::ProductionDecl { words, entity } => {
                assert_eq!(words, &["as_well_as"]);
                assert_eq!(entity, "as_well_as");
            }
            _ => panic!(),
        }
    }
    #[test]
    fn parse_mixed_production_with_quoted() {
        let f = check(r#"man, "as well as" --> man_noun."#);
        match &f.declarations[0].item {
            Declaration::ProductionDecl { words, entity } => {
                assert_eq!(words, &["man", "as_well_as"]);
                assert_eq!(entity, "man_noun");
            }
            _ => panic!(),
        }
    }
    #[test]
    fn parse_type_expr_arrow() {
        let f = check("walk: N \\ S.");
        match &f.declarations[0].item {
            Declaration::AtomDecl { ty, .. } => {
                assert!(matches!(ty.item, TypeExpr::RightArrow(..)))
            }
            _ => panic!(),
        }
    }
    #[test]
    fn parse_type_expr_slash() {
        let f = check("the: NP / N.");
        match &f.declarations[0].item {
            Declaration::AtomDecl { ty, .. } => assert!(matches!(ty.item, TypeExpr::LeftArrow(..))),
            _ => panic!(),
        }
    }
    #[test]
    fn parse_type_expr_union() {
        let f = check("dog: Noun | Animal.");
        match &f.declarations[0].item {
            Declaration::AtomDecl { ty, .. } => assert!(matches!(ty.item, TypeExpr::Union(..))),
            _ => panic!(),
        }
    }
    #[test]
    fn parse_full_example() {
        let f = check("Type = Noun | Sentence | Person | Adjective.\nPerson :< Noun.\nsocrates: Person.\nman, men --> Man.\nmortal: Adjective.");
        assert_eq!(f.declarations.len(), 5);
    }

    #[test]
    fn parse_morph_decl_basic() {
        let f = check("MORPH +s : (NP \\ S) \\ (NP \\ S).");
        match &f.declarations[0].item {
            Declaration::MorphemeDecl { surface, strips, .. } => {
                assert_eq!(surface, "+s");
                assert!(strips.is_empty());
            }
            _ => panic!("expected MorphemeDecl"),
        }
    }

    #[test]
    fn parse_morph_decl_with_strips() {
        let f = check("MORPH +ed : (NP \\ S) \\ (NP \\ S) STRIPS e, CC.");
        match &f.declarations[0].item {
            Declaration::MorphemeDecl { surface, strips, .. } => {
                assert_eq!(surface, "+ed");
                assert_eq!(strips.len(), 2);
                assert_eq!(strips[0], SpellingClass::EDeletion);
                assert_eq!(strips[1], SpellingClass::ConsonantDoubling);
            }
            _ => panic!("expected MorphemeDecl"),
        }
    }

    #[test]
    fn parse_morph_decl_y_i() {
        let f = check("MORPH +ly : Adv \\ Adj STRIPS y_i.");
        match &f.declarations[0].item {
            Declaration::MorphemeDecl { strips, .. } => {
                assert_eq!(strips.len(), 1);
                assert_eq!(strips[0], SpellingClass::YToI);
            }
            _ => panic!("expected MorphemeDecl"),
        }
    }

    #[test]
    fn parse_morph_decl_possessive() {
        let f = check("MORPH +'s : (NP / N) \\ NP STRIPS poss.");
        match &f.declarations[0].item {
            Declaration::MorphemeDecl { surface, strips, .. } => {
                assert_eq!(surface, "+'s");
                assert_eq!(strips.len(), 1);
                assert_eq!(strips[0], SpellingClass::Poss);
            }
            _ => panic!("expected MorphemeDecl"),
        }
    }
    #[test]
    fn parse_single_type_decl() {
        let f = check("type Noun.");
        match &f.declarations[0].item {
            Declaration::SingleTypeDecl(t) => assert_eq!(t, "Noun"),
            _ => panic!("expected SingleTypeDecl"),
        }
    }

    #[test]
    fn parse_namespace_decl() {
        let f = check("namespace montague.en_grammar_basic.");
        match &f.declarations[0].item {
            Declaration::NamespaceDecl(parts) => {
                assert_eq!(parts, &["montague", "en_grammar_basic"]);
            }
            _ => panic!("expected NamespaceDecl"),
        }
    }

    #[test]
    fn parse_extend_decl() {
        let f = check("extend montague.en_grammar_basic.");
        match &f.directives[0].item {
            Directive::Extend { ref namespace } => {
                assert_eq!(namespace, &["montague", "en_grammar_basic"]);
            }
            _ => panic!("expected Extend directive"),
        }
    }

    #[test]
    fn parse_extend_by_uri() {
        let f = check(r#"extend by "file:///path/to/grammar.mont"."#);
        match &f.directives[0].item {
            Directive::ExtendBy { uri } => {
                assert_eq!(uri, "file:///path/to/grammar.mont");
            }
            _ => panic!("expected ExtendBy directive"),
        }
    }

    #[test]
    fn parse_dot_disambiguation() {
        let src = "type S.\ntype NP.\n";
        let f = check(src);
        assert_eq!(f.declarations.len(), 2);
    }

    #[test]
    fn parse_mixed_old_new_type_syntax() {
        let src = "Type = A | B.\ntype C.";
        let f = check(src);
        assert_eq!(f.declarations.len(), 2);
        assert!(matches!(f.declarations[0].item, Declaration::TypeDecl(_)));
        assert!(matches!(f.declarations[1].item, Declaration::SingleTypeDecl(_)));
    }
}