use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    LineComment(String),
    TrueLiteral,
    FalseLiteral,
    IntLiteral(usize),
    FloatLiteral(f64),
    TypeLiteral(String),
    Identifier(String),
    CharLiteral(char),
    StringLiteral(String),
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equals,
    EqualsEquals,
    Bang,
    BangEquals,
    OpenAngle,
    CloseAngle,
    OpenAngleEquals,
    CloseAngleEquals,
    DoubleQuote,
    SingleQuote,
    Amp,
    AmpAmp,
    Pipe,
    PipePipe,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenSquare,
    CloseSquare,
    Carot,
    Semicolon,
    Colon,
    Comma,
    Dot,
    DotDot,
    FatArrow,
    SkinnyArrow,
    FnKeyword,
    ReturnKeyword,
    WhileKeyword,
    IfKeyword,
    ElseKeyword,
    TypeKeyword,
    StructKeyword,
    EnumKeyword,
    UnionKeyword,
    ForKeyword,
    InKeyword,
    MatchKeyword,
    DeferKeyword,
    UsesKeyword,
    AsKeyword,
    RawKeyword,
}

#[derive(Debug, Clone)]
pub struct Span {
    pub offset: usize,
    pub length: usize,
}

impl Span {
    pub fn of(start: &Span, end: &Span) -> Span {
        Span {
            offset: start.offset,
            length: end.offset + end.length - start.offset,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum LexError {
    UnexpectedCharacter { character: char, offset: usize },
    UnterminatedCharacterLiteral { span: Span },
}

impl LexError {
    pub fn get_span(&self) -> Span {
        match self {
            LexError::UnexpectedCharacter { offset, .. } => Span {
                offset: *offset,
                length: 1,
            },
            LexError::UnterminatedCharacterLiteral { span } => span.clone(),
        }
    }
}

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::UnexpectedCharacter { character, .. } => {
                write!(f, "unexpected character `{character}`")
            }
            LexError::UnterminatedCharacterLiteral { .. } => {
                write!(f, "unterminated character literal")
            }
        }
    }
}

fn trim_left(line: &str) -> usize {
    let mut offset = 0;
    while offset < line.len() && line.chars().nth(offset).unwrap().is_whitespace() {
        offset += 1;
    }
    offset
}

pub fn lex(source: &String) -> Result<Vec<Token>, LexError> {
    let mut tokens = Vec::new();

    let mut offset = 0;
    while offset < source.len() {
        offset += trim_left(&source[offset..]);

        if offset < source.len() {
            let c = source.chars().nth(offset).unwrap(); //TODO: This might be really slow

            let token = match c {
                '+' => Token {
                    kind: TokenKind::Plus,
                    span: Span { offset, length: 1 },
                },
                '-' => {
                    let peek = source.chars().nth(offset + 1);
                    if peek.is_some() && peek.unwrap() == '>' {
                        Token {
                            kind: TokenKind::SkinnyArrow,
                            span: Span { offset, length: 2 },
                        }
                    } else {
                        Token {
                            kind: TokenKind::Minus,
                            span: Span { offset, length: 1 },
                        }
                    }
                }
                '*' => Token {
                    kind: TokenKind::Star,
                    span: Span { offset, length: 1 },
                },
                '/' => {
                    let peek = source.chars().nth(offset + 1);
                    if peek.is_some() && peek.unwrap() == '/' {
                        let mut end_offset = offset + 2; // Skip the "//"
                        while end_offset < source.len()
                            && source.chars().nth(end_offset).unwrap() != '\n'
                        {
                            end_offset += 1;
                        }

                        let comment_text: String = source[(offset + 2)..end_offset].to_string();
                        let length = end_offset - offset;

                        Token {
                            kind: TokenKind::LineComment(comment_text),
                            span: Span { offset, length },
                        }
                    } else {
                        Token {
                            kind: TokenKind::Slash,
                            span: Span { offset, length: 1 },
                        }
                    }
                }
                '%' => Token {
                    kind: TokenKind::Percent,
                    span: Span { offset, length: 1 },
                },
                '=' => {
                    let peek = source.chars().nth(offset + 1);
                    if peek.is_some() && peek.unwrap() == '=' {
                        Token {
                            kind: TokenKind::EqualsEquals,
                            span: Span { offset, length: 2 },
                        }
                    } else if peek.is_some() && peek.unwrap() == '>' {
                        Token {
                            kind: TokenKind::FatArrow,
                            span: Span { offset, length: 2 },
                        }
                    } else {
                        Token {
                            kind: TokenKind::Equals,
                            span: Span { offset, length: 1 },
                        }
                    }
                }
                '!' => {
                    let peek = source.chars().nth(offset + 1);
                    if peek.is_some() && peek.unwrap() == '=' {
                        Token {
                            kind: TokenKind::BangEquals,
                            span: Span { offset, length: 2 },
                        }
                    } else {
                        Token {
                            kind: TokenKind::Bang,
                            span: Span { offset, length: 1 },
                        }
                    }
                }
                '<' => {
                    let peek = source.chars().nth(offset + 1);
                    if peek.is_some() && peek.unwrap() == '=' {
                        Token {
                            kind: TokenKind::OpenAngleEquals,
                            span: Span { offset, length: 2 },
                        }
                    } else {
                        Token {
                            kind: TokenKind::OpenAngle,
                            span: Span { offset, length: 1 },
                        }
                    }
                }
                '>' => {
                    let peek = source.chars().nth(offset + 1);
                    if peek.is_some() && peek.unwrap() == '=' {
                        Token {
                            kind: TokenKind::CloseAngleEquals,
                            span: Span { offset, length: 2 },
                        }
                    } else {
                        Token {
                            kind: TokenKind::CloseAngle,
                            span: Span { offset, length: 1 },
                        }
                    }
                }
                '&' => {
                    let peek = source.chars().nth(offset + 1);
                    if peek.is_some() && peek.unwrap() == '&' {
                        Token {
                            kind: TokenKind::AmpAmp,
                            span: Span { offset, length: 2 },
                        }
                    } else {
                        Token {
                            kind: TokenKind::Amp,
                            span: Span { offset, length: 1 },
                        }
                    }
                }
                '|' => {
                    let peek = source.chars().nth(offset + 1);
                    if peek.is_some() && peek.unwrap() == '|' {
                        Token {
                            kind: TokenKind::PipePipe,
                            span: Span { offset, length: 2 },
                        }
                    } else {
                        Token {
                            kind: TokenKind::Pipe,
                            span: Span { offset, length: 1 },
                        }
                    }
                }
                '(' => Token {
                    kind: TokenKind::OpenParen,
                    span: Span { offset, length: 1 },
                },
                ')' => Token {
                    kind: TokenKind::CloseParen,
                    span: Span { offset, length: 1 },
                },
                '{' => Token {
                    kind: TokenKind::OpenCurly,
                    span: Span { offset, length: 1 },
                },
                '}' => Token {
                    kind: TokenKind::CloseCurly,
                    span: Span { offset, length: 1 },
                },
                '[' => Token {
                    kind: TokenKind::OpenSquare,
                    span: Span { offset, length: 1 },
                },
                ']' => Token {
                    kind: TokenKind::CloseSquare,
                    span: Span { offset, length: 1 },
                },
                ';' => Token {
                    kind: TokenKind::Semicolon,
                    span: Span { offset, length: 1 },
                },
                ':' => Token {
                    kind: TokenKind::Colon,
                    span: Span { offset, length: 1 },
                },
                ',' => Token {
                    kind: TokenKind::Comma,
                    span: Span { offset, length: 1 },
                },
                '^' => Token {
                    kind: TokenKind::Carot,
                    span: Span { offset, length: 1 },
                },
                '.' => {
                    let peek = source.chars().nth(offset + 1);
                    if peek.is_some() && peek.unwrap() == '.' {
                        Token {
                            kind: TokenKind::DotDot,
                            span: Span { offset, length: 2 },
                        }
                    } else {
                        Token {
                            kind: TokenKind::Dot,
                            span: Span { offset, length: 1 },
                        }
                    }
                }
                '\'' => {
                    let mut end_offset = offset + 1; //skip opening '
                                                     //TODO: escape characters, make common with string parsing
                    let character = source.chars().nth(end_offset).unwrap();
                    end_offset += 1;

                    let peek = source.chars().nth(offset);
                    if peek.is_some() && peek.unwrap() != '\'' {
                        return Err(LexError::UnterminatedCharacterLiteral {
                            span: Span {
                                offset: end_offset,
                                length: 1,
                            },
                        });
                    }
                    end_offset += 1;
                    let text: String = source[(offset + 1)..end_offset].to_string();
                    let length = text.len() + 1;

                    Token {
                        kind: TokenKind::CharLiteral(character),
                        span: Span { offset, length },
                    }
                }
                '"' => {
                    let mut end_offset = offset + 1; // Skip opening "
                    let mut escaped = false; // To track escape sequences
                    let mut text = String::new();

                    while end_offset < source.len() {
                        let ch = source.chars().nth(end_offset).unwrap();

                        if escaped {
                            // If the previous character was a backslash, we need to handle the escape sequence
                            match ch {
                                '"' => text.push('"'),   // Handle escaped quote
                                '\\' => text.push('\\'), // Handle escaped backslash
                                'n' => text.push('\n'),  // Handle escaped newline
                                't' => text.push('\t'),  // Handle escaped tab
                                _ => text.push(ch), // Handle other escape sequences as literal characters
                            }
                            escaped = false;
                        } else if ch == '\\' {
                            // If this character is a backslash, the next character will be escaped
                            escaped = true;
                        } else if ch == '"' {
                            // If this character is a quote, we are at the end of the string
                            break;
                        } else {
                            // Otherwise, add the character to the string
                            text.push(ch);
                        }

                        end_offset += 1;
                    }

                    // Move end_offset past the closing quote
                    end_offset += 1;

                    let length = end_offset - offset;

                    Token {
                        kind: TokenKind::StringLiteral(text),
                        span: Span { offset, length },
                    }
                }

                '0'..='9' => {
                    let mut end_offset = offset + 1;
                    let mut is_float = false;

                    // Parse the integer part
                    while end_offset < source.len()
                        && source.chars().nth(end_offset).unwrap().is_ascii_digit()
                    {
                        end_offset += 1;
                    }

                    // Check for the decimal point
                    if end_offset < source.len()
                        && source.chars().nth(end_offset).unwrap() == '.'
                        && source.chars().nth(end_offset + 1).is_some()
                        && source.chars().nth(end_offset + 1).unwrap() != '.'
                    {
                        is_float = true;
                        end_offset += 1;

                        // Parse the fractional part
                        while end_offset < source.len()
                            && source.chars().nth(end_offset).unwrap().is_ascii_digit()
                        {
                            end_offset += 1;
                        }
                    }

                    if is_float {
                        let float_literal: f64 = source[offset..end_offset].parse().unwrap();
                        Token {
                            kind: TokenKind::FloatLiteral(float_literal),
                            span: Span {
                                offset,
                                length: end_offset - offset,
                            },
                        }
                    } else {
                        let int_literal: usize = source[offset..end_offset].parse().unwrap();
                        Token {
                            kind: TokenKind::IntLiteral(int_literal),
                            span: Span {
                                offset,
                                length: end_offset - offset,
                            },
                        }
                    }
                }

                x if char::is_alphabetic(x) || x == '_' => {
                    let mut end_offset = offset;
                    while end_offset < source.len()
                        && (source.chars().nth(end_offset).unwrap().is_alphanumeric()
                            || source.chars().nth(end_offset).unwrap() == '_')
                    //TODO: this is horrendous
                    {
                        end_offset += 1;
                    }
                    let text: String = source[offset..end_offset].to_string();
                    let length = text.len();

                    let kind = match text.as_str() {
                        "true" => TokenKind::TrueLiteral,
                        "false" => TokenKind::FalseLiteral,
                        "fn" => TokenKind::FnKeyword,
                        "return" => TokenKind::ReturnKeyword,
                        "while" => TokenKind::WhileKeyword,
                        "if" => TokenKind::IfKeyword,
                        "else" => TokenKind::ElseKeyword,
                        "type" => TokenKind::TypeKeyword,
                        "struct" => TokenKind::StructKeyword,
                        "enum" => TokenKind::EnumKeyword,
                        "union" => TokenKind::UnionKeyword,
                        "for" => TokenKind::ForKeyword,
                        "in" => TokenKind::InKeyword,
                        "match" => TokenKind::MatchKeyword,
                        "defer" => TokenKind::DeferKeyword,
                        "uses" => TokenKind::UsesKeyword,
                        "as" => TokenKind::AsKeyword,
                        "raw" => TokenKind::RawKeyword,
                        //TypeLiterals
                        "bool" | "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64"
                        | "f64" | "string" | "char" => TokenKind::TypeLiteral(text),
                        _ => TokenKind::Identifier(text),
                    };

                    Token {
                        kind,
                        span: Span { offset, length },
                    }
                }
                _ => {
                    return Err(LexError::UnexpectedCharacter {
                        character: c,
                        offset,
                    });
                }
            };

            offset += token.span.length;
            tokens.push(token);
        }
    }

    Ok(tokens)
}
