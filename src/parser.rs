use std::fmt::Display;

use crate::{
    lexer::{Span, Token, TokenKind},
};

#[derive(Debug, Clone)]
pub enum TypeDefinition {
    Struct {
        struct_keyword: Token,
        open_curly: Token,
        members: Vec<Expression>,
        close_curly: Token,
    },
    Enum {
        enum_keyword: Token,
        open_curly: Token,
        members: Vec<Token>,
        close_curly: Token,
    },
    Union {
        union_keyword: Token,
        open_curly: Token,
        variants: Vec<Expression>,
        close_curly: Token,
    },
}

#[derive(Debug, Clone)]
pub enum Statement {
    FunctionDeclaration {
        fn_keyword: Token,
        identifier: Token,
        open_paren: Token,
        parameters: Vec<Expression>,
        close_paren: Token,
        return_type_expression: Option<Expression>,
        body: Box<Statement>,
    },
    ArrowFunctionDeclaration {
        fn_keyword: Token,
        identifier: Token,
        open_paren: Token,
        parameters: Vec<Expression>,
        close_paren: Token,
        arrow: Token,
        body: Box<Statement>,
    },
    Expression {
        expression: Expression,
        semicolon: Token,
    },
    Block {
        open_curly: Token,
        statements: Vec<Statement>,
        close_curly: Token,
    },
    While {
        while_keyword: Token,
        condition: Expression,
        body: Box<Statement>,
    },
    If {
        if_keyword: Token,
        condition: Expression,
        body: Box<Statement>,
        else_clause: Option<Box<Statement>>,
    },
    Guard {
        guard_keyword: Token,
        condition: Expression,
        else_keyword: Option<Token>,
        body: Box<Statement>,
        capture: Option<Box<Statement>>,
    },
    Else {
        else_keyword: Token,
        body: Box<Statement>,
    },
    TypeDefinition {
        type_keyword: Token,
        identifier: Token,
        colon: Token,
        type_definition: TypeDefinition,
    },
    For {
        for_keyword: Token,
        iterator: Token,
        in_keyword: Token,
        iterable: Expression,
        body: Box<Statement>,
    },
    Break {
        break_keyword: Token,
        semicolon: Token,
    },
    Continue {
        continue_keyword: Token,
        semicolon: Token,
    },
    Match {
        match_keyword: Token,
        expression: Expression,
        open_curly: Token,
        arms: Vec<Expression>,
        close_curly: Token,
    },
    Uses {
        uses_keyword: Token,
        module_path: Vec<Token>, //TODO: make this more complex for paths and importing specific types and functions, also renaming
        semicolon: Token,
    },
    Defer {
        defer_keyword: Token,
        statement: Box<Statement>,
    },
    Return {
        return_keyword: Token,
        expression: Option<Box<Expression>>,
        semicolon: Token,
    },
    Raw {
        raw_keyword: Token,
        open_curly: Token,
        lines: Vec<Token>,
        close_curly: Token,
    },
    Capture {
        open_pipe: Token,
        identifier: Token,
        close_pipe: Token,
    },
}

impl Statement {
    pub fn get_span(&self) -> Span {
        match self {
            Statement::FunctionDeclaration {
                fn_keyword, body, ..
            }
            | Statement::ArrowFunctionDeclaration {
                fn_keyword, body, ..
            } => Span::of(&fn_keyword.span, &body.get_span()),
            Statement::Expression {
                expression,
                semicolon,
            } => Span::of(&expression.get_span(), &semicolon.span),
            Statement::Block {
                open_curly,
                close_curly,
                ..
            } => Span::of(&open_curly.span, &close_curly.span),
            Statement::While {
                while_keyword,
                body,
                ..
            } => Span::of(&while_keyword.span, &body.get_span()),
            Statement::Guard {
                guard_keyword,
                body,
                ..
            } => Span::of(&guard_keyword.span, &body.get_span()),
            Statement::If {
                if_keyword,
                body,
                else_clause,
                ..
            } => match else_clause {
                Some(else_clause) => Span::of(&if_keyword.span, &else_clause.get_span()),
                None => Span::of(&if_keyword.span, &body.get_span()),
            },
            Statement::Else { else_keyword, body } => {
                Span::of(&else_keyword.span, &body.get_span())
            }
            Statement::TypeDefinition {
                type_keyword,
                identifier,
                ..
            } => Span::of(&type_keyword.span, &identifier.span),
            Statement::For {
                for_keyword, body, ..
            } => Span::of(&for_keyword.span, &body.get_span()),
            Statement::Match {
                match_keyword,
                close_curly,
                ..
            } => Span::of(&match_keyword.span, &close_curly.span),
            Statement::Uses {
                uses_keyword,
                semicolon,
                ..
            } => Span::of(&uses_keyword.span, &semicolon.span),
            Statement::Defer {
                defer_keyword,
                statement,
            } => Span::of(&defer_keyword.span, &statement.get_span()),
            Statement::Return {
                return_keyword,
                semicolon,
                ..
            } => Span::of(&return_keyword.span, &semicolon.span),
            Statement::Raw {
                raw_keyword,
                close_curly,
                ..
            } => Span::of(&raw_keyword.span, &close_curly.span),
            Statement::Break {
                break_keyword,
                semicolon,
            } => Span::of(&break_keyword.span, &semicolon.span),
            Statement::Continue {
                continue_keyword,
                semicolon,
            } => Span::of(&continue_keyword.span, &semicolon.span),
            Statement::Capture {
                open_pipe,
                close_pipe,
                ..
            } => Span::of(&open_pipe.span, &close_pipe.span),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Gt,
    GtEq,
    Lt,
    LtEq,
    Eq,
    NEq,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    Range,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Ref,
    Not,
    Neg,
}

#[derive(Debug, Clone)]
pub struct UnaryOperation {
    pub token: Token,
    pub op: UnaryOperator,
}

#[derive(Debug, Clone)]
pub enum TypeExpression {
    Basic(Token),
    Pointer {
        carot: Token,
        reference_type: Box<TypeExpression>,
    },
    Optional {
        question: Token,
        reference_type: Box<TypeExpression>,
    },
    Array {
        open_square: Token,
        size: Token,
        close_square: Token,
        element_type: Box<TypeExpression>,
    },
    Slice {
        open_square: Token,
        close_square: Token,
        element_type: Box<TypeExpression>,
    },
    Function {
        open_paren: Token,
        inputs: Vec<TypeExpression>,
        close_paren: Token,
        skinny_arrow: Token,
        output: Box<TypeExpression>,
    },
}

impl TypeExpression {
    pub fn get_span(&self) -> Span {
        match self {
            TypeExpression::Basic(token) => token.span.clone(),
            TypeExpression::Pointer {
                carot,
                reference_type,
            } => Span::of(&carot.span, &reference_type.get_span()),
            TypeExpression::Optional {
                question,
                reference_type,
            } => Span::of(&question.span, &reference_type.get_span()),
            TypeExpression::Array {
                open_square,
                element_type,
                ..
            }
            | TypeExpression::Slice {
                open_square,
                element_type,
                ..
            } => Span::of(&open_square.span, &element_type.get_span()),
            TypeExpression::Function {
                open_paren,
                inputs: _,
                close_paren: _,
                skinny_arrow: _,
                output,
            } => Span::of(&open_paren.span, &output.get_span()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnionVariantKind {
    Enum,
    Struct { parameters: Vec<Expression> },
    //Tuple { parameters: Vec<Token> },
}

#[derive(Debug, Clone)]
pub enum UnionLiteralKind {
    Enum {
        identifier: Token,
    },
    Struct {
        identifier: Token,
        open_curly: Token,
        arguments: Vec<Expression>,
        close_curly: Token,
    },
    //Tuple { identifier: Token, arguments: Vec<Expression> },
}

impl UnionLiteralKind {
    pub fn get_span(&self) -> Span {
        match self {
            UnionLiteralKind::Enum { identifier } => identifier.span.clone(),
            UnionLiteralKind::Struct {
                identifier,
                close_curly,
                ..
            } => Span::of(&identifier.span, &close_curly.span),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    //Range, Literal etc etc
    Wildcard {
        span: Span,
    },
    Member {
        left: Option<Expression>,
        dot: Token,
        member: Token,
    },
    Struct {
        left: Option<Expression>,
        dot: Token,
        identifier: Token,
        open_curly: Token,
        params: Vec<Token>,
        close_curly: Token,
    },
}

impl Pattern {
    pub fn get_span(&self) -> Span {
        match self {
            Pattern::Wildcard { span } => span.clone(),
            Pattern::Member { left, dot, member } => match left {
                Some(left) => Span::of(&left.get_span(), &member.span),
                None => Span::of(&dot.span, &member.span),
            },
            Pattern::Struct {
                left,
                dot,
                close_curly,
                ..
            } => match left {
                Some(left) => Span::of(&left.get_span(), &close_curly.span),
                None => Span::of(&dot.span, &close_curly.span),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    NoneLiteral {
        span: Span,
    },
    IntLiteral {
        value: usize,
        span: Span,
    },
    FloatLiteral {
        value: f64,
        span: Span,
    },
    BoolLiteral {
        value: bool,
        span: Span,
    },
    StringLiteral {
        value: String,
        span: Span,
    },
    CharLiteral {
        value: char,
        span: Span,
    },
    VariableDeclaration {
        identifiers: Vec<Token>,
        colon: Token,
        type_expression: Option<Box<TypeExpression>>,
        assignment_token: Token,
        initialiser: Box<Expression>,
    },
    //TODO: this gets reused by other kinds of identifiers
    Variable {
        name: String,
        span: Span,
    },
    FunctionCall {
        identifier: Token,
        open_paren: Token,
        arguments: Vec<Expression>,
        close_paren: Token,
    },
    Binary {
        left: Box<Expression>,
        op: BinaryOperator,
        right: Box<Expression>,
    },
    Unary {
        op: UnaryOperation,
        operand: Box<Expression>,
    },
    Parenthesised {
        open_paren: Token,
        expression: Box<Expression>,
        close_paren: Token,
    },
    Type {
        expression: TypeExpression,
    },
    TypeAnnotation {
        colon: Token,
        expression: TypeExpression,
    },
    Assignment {
        left: Box<Expression>,
        equals: Token,
        assignment: Box<Expression>,
    },
    FunctionParameter {
        identifiers: Vec<Token>,
        type_annotation: Box<Expression>,
    },
    NamedArgument {
        name: Token,
        colon: Token,
        argument: Box<Expression>,
    },
    StructLiteral {
        identifier: Token,
        open_curly: Token,
        arguments: Vec<Expression>,
        close_curly: Token,
    },
    MemberAccess {
        left: Option<Box<Expression>>,
        dot: Token,
        member: Token,
    },
    ArrayLiteral {
        type_expression: TypeExpression,
        open_curly: Token,
        elements: Vec<Expression>,
        close_curly: Token,
    },
    ArrayIndex {
        array: Box<Expression>,
        open_square: Token,
        index: Box<Expression>,
        close_square: Token,
    },
    MatchArm {
        pattern: Box<Pattern>,
        fat_arrow: Token,
        body: Box<Statement>,
    },
    UnionVariant {
        identifier: Token,
        kind: UnionVariantKind,
    },
    Dereference {
        expression: Box<Expression>,
        carot: Token,
    },
    UnwrapOptional {
        expression: Box<Expression>,
        question: Token,
    },
    MethodCall {
        callee: Box<Expression>,
        dot: Token,
        identifier: Token,
        open_paren: Token,
        arguments: Vec<Expression>,
        close_paren: Token,
    },
    //TODO: unify union and enum so that an enum is just a redundant case of a union
    UnionLiteral {
        left: Option<Box<Expression>>,
        dot: Token,
        kind: UnionLiteralKind,
    },
    Cast {
        expression: Box<Expression>,
        as_keyword: Token,
        destination_type: TypeExpression,
    },
    Lambda {
        open_paren: Token,
        parameters: Vec<Expression>,
        close_paren: Token,
        arrow: Token,
        body: Box<Expression>,
    },
    Guard {
        guard_keyword: Token,
        expression: Box<Expression>,
        else_keyword: Token,
        body: Box<Statement>,
    },
}

impl Expression {
    pub fn get_span(&self) -> Span {
        match self {
            Expression::IntLiteral { span, .. }
            | Expression::FloatLiteral { span, .. }
            | Expression::BoolLiteral { span, .. }
            | Expression::StringLiteral { span, .. }
            | Expression::CharLiteral { span, .. }
            | Expression::Variable { span, .. } => span.clone(),
            Expression::VariableDeclaration {
                identifiers,
                initialiser,
                ..
            } => Span::of(&identifiers.first().unwrap().span, &initialiser.get_span()),
            Expression::FunctionCall {
                identifier,
                close_paren,
                ..
            } => Span::of(&identifier.span, &close_paren.span),
            Expression::MethodCall {
                callee,
                close_paren,
                ..
            } => Span::of(&callee.get_span(), &close_paren.span),
            Expression::Binary { left, op: _, right } => {
                Span::of(&left.get_span(), &right.get_span())
            }
            Expression::Unary { op, operand } => Span::of(&op.token.span, &operand.get_span()),
            Expression::Parenthesised {
                open_paren,
                close_paren,
                ..
            } => Span::of(&open_paren.span, &close_paren.span),
            Expression::Type { expression: _ } => todo!(),
            Expression::TypeAnnotation { colon, expression } => {
                Span::of(&colon.span, &expression.get_span())
            }
            Expression::Assignment {
                left, assignment, ..
            } => Span::of(&left.get_span(), &assignment.get_span()),
            Expression::FunctionParameter {
                identifiers,
                type_annotation,
            } => Span::of(
                &identifiers.first().unwrap().span,
                &type_annotation.get_span(),
            ),
            Expression::NamedArgument { name, argument, .. } => {
                Span::of(&name.span, &argument.get_span())
            }
            Expression::StructLiteral {
                identifier,
                close_curly,
                ..
            } => Span::of(&identifier.span, &close_curly.span),
            Expression::MemberAccess {
                left, dot, member, ..
            } => match left {
                Some(left) => Span::of(&left.get_span(), &member.span),
                None => Span::of(&dot.span, &member.span),
            },
            Expression::ArrayLiteral {
                type_expression,
                close_curly,
                ..
            } => Span::of(&type_expression.get_span(), &close_curly.span),
            Expression::ArrayIndex {
                array,
                close_square,
                ..
            } => Span::of(&array.get_span(), &close_square.span),
            Expression::MatchArm { pattern, body, .. } => {
                Span::of(&pattern.get_span(), &body.get_span())
            }
            Expression::UnionVariant { identifier, .. } => identifier.span.clone(),
            Expression::Dereference { expression, carot } => {
                Span::of(&expression.get_span(), &carot.span)
            }
            Expression::UnwrapOptional {
                expression,
                question,
            } => Span::of(&expression.get_span(), &question.span),
            Expression::UnionLiteral {
                left, dot, kind, ..
            } => match left {
                Some(left) => Span::of(&left.get_span(), &kind.get_span()),
                None => Span::of(&dot.span, &kind.get_span()),
            },
            Expression::Cast {
                expression,
                destination_type,
                ..
            } => Span::of(&expression.get_span(), &destination_type.get_span()),
            Expression::Lambda {
                open_paren, body, ..
            } => Span::of(&open_paren.span, &body.get_span()),
            Expression::NoneLiteral { span } => span.clone(),
            Expression::Guard {
                guard_keyword,
                body,
                ..
            } => Span::of(&guard_keyword.span, &body.get_span()),
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(TokenKind, Span),
    NotAStatement(Expression),
    UnexpectedEOF,
    Expected(String),
    TokenMismatch {
        expected: TokenKind,
        actual: TokenKind,
        span: Span,
    },
    TokenMismatchExpected {
        expected: String,
        actual: TokenKind,
        span: Span,
    },
    InvalidPattern {
        span: Span,
    },
}

impl ParseError {
    pub fn get_span(&self) -> Span {
        match self {
            ParseError::UnexpectedToken(_, span)
            | ParseError::TokenMismatch { span, .. }
            | ParseError::InvalidPattern { span } => span.clone(),
            ParseError::NotAStatement(expr) => expr.get_span(),
            ParseError::UnexpectedEOF => todo!(),
            ParseError::Expected(_) => todo!(),
            ParseError::TokenMismatchExpected { span, .. } => span.clone(),
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken(kind, _) => write!(f, "unexpected token {kind:?}"),
            ParseError::NotAStatement(_) => write!(f, "not a statement"),
            ParseError::UnexpectedEOF => write!(f, "unexpected end of file"),
            ParseError::Expected(_) => todo!(),
            ParseError::TokenMismatch {
                expected,
                actual,
                span: _,
            } => write!(f, "expected {expected:?} but got {actual:?}"),
            ParseError::TokenMismatchExpected {
                expected, actual, ..
            } => {
                write!(f, "expected {expected} but got {actual:?}")
            }
            ParseError::InvalidPattern { .. } => write!(f, "invalid pattern"),
        }
    }
}

pub struct Parser {
    cursor: usize,
    tokens: Vec<Token>,
    allow_struct_literals: bool,
    allow_multi_assign: bool,
    allow_var_decl: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            cursor: 0,
            allow_struct_literals: true,
            allow_multi_assign: true,
            allow_var_decl: true,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements: Vec<Statement> = Vec::new();

        while !self.finished_parsing() {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        if self.cursor >= self.tokens.len() {
            return Err(ParseError::UnexpectedEOF);
        }

        let token = self.get_head()?;

        match token.kind {
            TokenKind::FnKeyword => self.parse_fn_declaration(),
            TokenKind::WhileKeyword => self.parse_while_loop(),
            TokenKind::IfKeyword => self.parse_if(),
            TokenKind::GuardKeyword => self.parse_guard(),
            TokenKind::ForKeyword => self.parse_for(),
            TokenKind::BreakKeyword => {
                let break_keyword = self.expect_token(TokenKind::BreakKeyword)?;
                let semicolon = self.expect_token(TokenKind::Semicolon)?;

                Ok(Statement::Break {
                    break_keyword,
                    semicolon,
                })
            }
            TokenKind::ContinueKeyword => {
                let continue_keyword = self.expect_token(TokenKind::ContinueKeyword)?;
                let semicolon = self.expect_token(TokenKind::Semicolon)?;

                Ok(Statement::Continue {
                    continue_keyword,
                    semicolon,
                })
            }
            TokenKind::MatchKeyword => self.parse_match(),
            TokenKind::RawKeyword => {
                let raw_keyword = self.expect_token(TokenKind::RawKeyword)?;
                let open_curly = self.expect_token(TokenKind::OpenCurly)?;

                let mut lines = Vec::new();
                while self.get_head()?.kind != TokenKind::CloseCurly {
                    lines.push(self.expect_string_literal()?);
                }

                let close_curly = self.expect_token(TokenKind::CloseCurly)?;

                Ok(Statement::Raw {
                    raw_keyword,
                    open_curly,
                    lines,
                    close_curly,
                })
            }
            TokenKind::ReturnKeyword => {
                let return_keyword = self.expect_token(TokenKind::ReturnKeyword)?;
                if self.get_head()?.kind == TokenKind::Semicolon {
                    let semicolon = self.expect_token(TokenKind::Semicolon)?;
                    return Ok(Statement::Return {
                        return_keyword,
                        expression: None,
                        semicolon,
                    });
                }
                let expression = self.parse_binary_expression(0)?;
                let semicolon = self.expect_token(TokenKind::Semicolon)?;
                Ok(Statement::Return {
                    return_keyword,
                    expression: Some(Box::new(expression)),
                    semicolon,
                })
            }
            TokenKind::DeferKeyword => {
                let defer_keyword = self.expect_token(TokenKind::DeferKeyword)?;
                let statement = self.parse_statement()?;

                Ok(Statement::Defer {
                    defer_keyword,
                    statement: Box::new(statement),
                })
            }
            TokenKind::OpenCurly => {
                let open_curly = self.expect_token(TokenKind::OpenCurly)?;

                let mut statements = Vec::new();
                while let Ok(token) = self.get_head() {
                    if token.kind == TokenKind::CloseCurly {
                        let close_curly = self.expect_token(TokenKind::CloseCurly)?;
                        return Ok(Statement::Block {
                            open_curly,
                            statements,
                            close_curly,
                        });
                    }
                    statements.push(self.parse_statement()?);
                }

                Err(ParseError::UnexpectedEOF)
            }
            TokenKind::TypeKeyword => self.parse_type_definition(),
            TokenKind::UsesKeyword => {
                let uses_keyword = self.expect_token(TokenKind::UsesKeyword)?;

                let mut module_path = Vec::new();
                module_path.push(self.expect_identifier()?);
                while self.get_head()?.kind != TokenKind::Semicolon {
                    self.expect_token(TokenKind::Dot)?;
                    module_path.push(self.expect_identifier()?);
                }
                let semicolon = self.expect_token(TokenKind::Semicolon)?;

                Ok(Statement::Uses {
                    uses_keyword,
                    module_path,
                    semicolon,
                })
            }
            //Default = expression statement
            _ => {
                let expression = self.parse_binary_expression(0)?;
                let semicolon = self.expect_token(TokenKind::Semicolon)?;

                //Check it's allowed as a statement
                match expression {
                    Expression::FunctionCall { .. }
                    | Expression::MethodCall { .. }
                    | Expression::VariableDeclaration { .. }
                    | Expression::Assignment { .. } => Ok(Statement::Expression {
                        expression,
                        semicolon,
                    }),
                    _ => Err(ParseError::NotAStatement(expression)),
                }
            }
        }
    }

    fn parse_var_declaration(&mut self, identifier: Token) -> Result<Expression, ParseError> {
        let mut identifiers = Vec::new();
        identifiers.push(identifier);
        while self.get_head()?.kind == TokenKind::Comma {
            self.expect_token(TokenKind::Comma)?;
            identifiers.push(self.expect_identifier()?);
        }

        let colon = self.expect_token(TokenKind::Colon)?;

        let type_expression = if self.get_head()?.kind != TokenKind::Colon
            && self.get_head()?.kind != TokenKind::Equals
        {
            Some(Box::new(self.parse_type_expression()?))
        } else {
            None
        };

        let head = &self.get_head()?.kind;
        let assignment_token = match head {
            TokenKind::Equals => self.expect_token(TokenKind::Equals)?,
            TokenKind::Colon => self.expect_token(TokenKind::Colon)?,
            _ => {
                return Err(ParseError::TokenMismatchExpected {
                    expected: "one of `=` or `:`".to_string(),
                    actual: head.clone(),
                    span: self.tokens[self.cursor].span.clone(),
                });
            }
        };

        let initialiser = self.parse_binary_expression(0)?;
        Ok(Expression::VariableDeclaration {
            identifiers,
            colon,
            type_expression,
            assignment_token,
            initialiser: Box::new(initialiser),
        })
    }

    fn parse_fn_declaration(&mut self) -> Result<Statement, ParseError> {
        let fn_keyword = self.expect_token(TokenKind::FnKeyword)?;
        let identifier = self.expect_identifier()?;
        let open_paren = self.expect_token(TokenKind::OpenParen)?;

        let mut parameters = Vec::new();
        while self.get_head()?.kind != TokenKind::CloseParen {
            parameters.push(self.parse_function_parameter()?);

            if self.get_head()?.kind != TokenKind::CloseParen {
                self.expect_token(TokenKind::Comma)?;
            }
        }

        let close_paren = self.expect_token(TokenKind::CloseParen)?;

        if self.get_head()?.kind == TokenKind::SkinnyArrow {
            let arrow = self.expect_token(TokenKind::SkinnyArrow)?;
            let return_expression = self.parse_binary_expression(0)?;
            let semicolon = self.expect_token(TokenKind::Semicolon)?;

            return Ok(Statement::ArrowFunctionDeclaration {
                fn_keyword,
                identifier,
                open_paren,
                parameters,
                close_paren,
                arrow,
                body: Box::new(Statement::Expression {
                    expression: return_expression,
                    semicolon,
                }),
            });
        }

        let return_type_expression = if self.get_head()?.kind == TokenKind::Colon {
            Some(self.parse_type_annotation_expression()?)
        } else {
            None
        };

        let body = self.parse_statement()?;

        match body {
            Statement::Block { .. } => Ok(Statement::FunctionDeclaration {
                fn_keyword,
                identifier,
                open_paren,
                parameters,
                close_paren,
                return_type_expression,
                body: Box::new(body),
            }),
            _ => Err(ParseError::Expected("BLOCK".to_string())),
        }
    }

    fn parse_while_loop(&mut self) -> Result<Statement, ParseError> {
        let while_keyword = self.expect_token(TokenKind::WhileKeyword)?;
        let condition = self.parse_binary_expression(0)?;
        let before = self.allow_struct_literals;
        self.allow_struct_literals = false;
        let body = self.parse_statement()?;
        self.allow_struct_literals = before;

        Ok(Statement::While {
            while_keyword,
            condition,
            body: Box::new(body),
        })
    }

    fn parse_if(&mut self) -> Result<Statement, ParseError> {
        let if_keyword = self.expect_token(TokenKind::IfKeyword)?;
        let condition = self.parse_binary_expression(0)?;
        let before = self.allow_struct_literals;
        self.allow_struct_literals = false;
        let body = self.parse_statement()?;
        self.allow_struct_literals = before;

        let else_clause = if self.get_head()?.kind == TokenKind::ElseKeyword {
            Some(Box::new(self.parse_else()?))
        } else {
            None
        };

        Ok(Statement::If {
            if_keyword,
            condition,
            body: Box::new(body),
            else_clause,
        })
    }

    fn parse_guard(&mut self) -> Result<Statement, ParseError> {
        let guard_keyword = self.expect_token(TokenKind::GuardKeyword)?;
        let before = self.allow_struct_literals;
        self.allow_struct_literals = false;
        //Higher precedence to prevent parsing | as bitwise or instead of a capture
        let condition = self.parse_binary_expression(5)?;

        let mut capture = if self.get_head()?.kind == TokenKind::Pipe {
            let open_pipe = self.expect_token(TokenKind::Pipe)?;
            let identifier = self.expect_identifier()?;
            let close_pipe = self.expect_token(TokenKind::Pipe)?;
            Some(Box::new(Statement::Capture {
                open_pipe,
                identifier,
                close_pipe,
            }))
        } else {
            None
        };

        let else_keyword = if self.get_head()?.kind == TokenKind::ElseKeyword {
            Some(self.expect_token(TokenKind::ElseKeyword)?)
        } else {
            None
        };

        if self.get_head()?.kind == TokenKind::Pipe {
            if capture.is_none() {
                let open_pipe = self.expect_token(TokenKind::Pipe)?;
                let identifier = self.expect_identifier()?;
                let close_pipe = self.expect_token(TokenKind::Pipe)?;
                capture = Some(Box::new(Statement::Capture {
                    open_pipe,
                    identifier,
                    close_pipe,
                }));
            } else {
                return Err(ParseError::UnexpectedToken(
                    TokenKind::Pipe,
                    self.get_head()?.span.clone(),
                ));
            }
        }

        let body = self.parse_statement()?;
        self.allow_struct_literals = before;

        Ok(Statement::Guard {
            guard_keyword,
            condition,
            capture,
            else_keyword,
            body: Box::new(body),
        })
    }

    fn parse_for(&mut self) -> Result<Statement, ParseError> {
        let for_keyword = self.expect_token(TokenKind::ForKeyword)?;
        let iterator = self.expect_identifier()?;
        let in_keyword = self.expect_token(TokenKind::InKeyword)?;

        let before = self.allow_struct_literals;
        self.allow_struct_literals = false;
        let iterable = self.parse_binary_expression(0)?;
        self.allow_struct_literals = before;

        let body = self.parse_statement()?;

        Ok(Statement::For {
            for_keyword,
            iterator,
            in_keyword,
            iterable,
            body: Box::new(body),
        })
    }

    fn parse_match(&mut self) -> Result<Statement, ParseError> {
        let match_keyword = self.expect_token(TokenKind::MatchKeyword)?;
        self.allow_struct_literals = false;
        let expression = self.parse_binary_expression(0)?;
        self.allow_struct_literals = true;
        let open_curly = self.expect_token(TokenKind::OpenCurly)?;

        let mut arms = Vec::new();
        while self.get_head()?.kind != TokenKind::CloseCurly {
            let arm = self.parse_match_arm()?;
            arms.push(arm);
        }
        let close_curly = self.expect_token(TokenKind::CloseCurly)?;

        Ok(Statement::Match {
            match_keyword,
            expression,
            open_curly,
            arms,
            close_curly,
        })
    }

    fn parse_match_arm(&mut self) -> Result<Expression, ParseError> {
        self.allow_struct_literals = false;
        let pattern = self.parse_pattern()?;
        self.allow_struct_literals = true;

        let fat_arrow = self.expect_token(TokenKind::FatArrow)?;

        let body = self.parse_statement()?;

        Ok(Expression::MatchArm {
            pattern: Box::new(pattern),
            fat_arrow,
            body: Box::new(body),
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        let expression = self.parse_binary_expression(0)?;
        let expression_span = expression.get_span();

        match expression {
            Expression::Variable { name, span } => {
                if name == "_" {
                    return Ok(Pattern::Wildcard { span });
                }
                Err(ParseError::InvalidPattern { span })
            }
            Expression::MemberAccess { left, dot, member } => {
                let next_token = self.get_head()?.clone();
                match &next_token.kind {
                    TokenKind::FatArrow => Ok(Pattern::Member {
                        left: left.map(|e| *e),
                        dot,
                        member,
                    }),
                    TokenKind::OpenCurly => {
                        let open_curly = self.expect_token(TokenKind::OpenCurly)?;
                        let mut params = Vec::new();
                        while self.get_head()?.kind != TokenKind::CloseCurly {
                            params.push(self.expect_identifier()?); //TODO: allow renaming, `radius: r`
                            if self.get_head()?.kind != TokenKind::CloseCurly {
                                self.expect_token(TokenKind::Comma)?;
                            }
                        }
                        let close_curly = self.expect_token(TokenKind::CloseCurly)?;
                        Ok(Pattern::Struct {
                            left: left.map(|e| *e),
                            dot,
                            identifier: member,
                            open_curly,
                            params,
                            close_curly,
                        })
                    }
                    _ => Err(ParseError::UnexpectedToken(
                        next_token.kind,
                        next_token.span,
                    )),
                }
            }
            _ => Err(ParseError::InvalidPattern {
                span: expression_span,
            }),
        }
    }

    fn parse_else(&mut self) -> Result<Statement, ParseError> {
        let else_keyword = self.expect_token(TokenKind::ElseKeyword)?;
        let body = self.parse_statement()?;

        Ok(Statement::Else {
            else_keyword,
            body: Box::new(body),
        })
    }

    fn parse_type_definition(&mut self) -> Result<Statement, ParseError> {
        let type_keyword = self.expect_token(TokenKind::TypeKeyword)?;
        let identifier = self.expect_identifier()?;
        let colon = self.expect_token(TokenKind::Colon)?;

        //TODO: other kinds of type definitions: enum, dependant types, match on the next keyword in future

        let head = &self.get_head()?.kind;
        let type_definition = match head {
            TokenKind::StructKeyword => self.parse_struct_definition()?,
            TokenKind::EnumKeyword => self.parse_enum_definition()?,
            TokenKind::UnionKeyword => self.parse_union_definition()?,
            _ => {
                return Err(ParseError::UnexpectedToken(
                    head.clone(),
                    self.tokens.get(self.cursor).unwrap().span.clone(),
                ));
            }
        };

        Ok(Statement::TypeDefinition {
            type_keyword,
            identifier,
            colon,
            type_definition,
        })
    }

    fn parse_enum_definition(&mut self) -> Result<TypeDefinition, ParseError> {
        let enum_keyword = self.expect_token(TokenKind::EnumKeyword)?;
        let open_curly = self.expect_token(TokenKind::OpenCurly)?;

        let mut members = Vec::new();
        while self.get_head()?.kind != TokenKind::CloseCurly {
            let member = self.expect_identifier()?;
            members.push(member);
            if self.get_head()?.kind != TokenKind::CloseCurly {
                self.expect_token(TokenKind::Comma)?;
            }
        }
        let close_curly = self.expect_token(TokenKind::CloseCurly)?;

        Ok(TypeDefinition::Enum {
            enum_keyword,
            open_curly,
            members,
            close_curly,
        })
    }

    fn parse_union_definition(&mut self) -> Result<TypeDefinition, ParseError> {
        let union_keyword = self.expect_token(TokenKind::UnionKeyword)?;
        let open_curly = self.expect_token(TokenKind::OpenCurly)?;

        let mut variants = Vec::new();
        while self.get_head()?.kind != TokenKind::CloseCurly {
            let identifier = self.expect_identifier()?;
            match self.get_head()?.kind {
                TokenKind::OpenCurly => {
                    let mut parameters = Vec::new();
                    self.expect_token(TokenKind::OpenCurly)?;
                    while self.get_head()?.kind != TokenKind::CloseCurly {
                        let param = self.parse_function_parameter()?;
                        parameters.push(param);
                        if self.get_head()?.kind != TokenKind::CloseCurly {
                            self.expect_token(TokenKind::Comma)?;
                        }
                    }
                    self.expect_token(TokenKind::CloseCurly)?;
                    variants.push(Expression::UnionVariant {
                        identifier,
                        kind: UnionVariantKind::Struct { parameters },
                    });
                }
                //TokenKind::OpenParen => parse tuple variant
                _ => {
                    variants.push(Expression::UnionVariant {
                        identifier,
                        kind: UnionVariantKind::Enum,
                    });
                }
            }
            if self.get_head()?.kind != TokenKind::CloseCurly {
                self.expect_token(TokenKind::Comma)?;
            }
        }
        let close_curly = self.expect_token(TokenKind::CloseCurly)?;

        Ok(TypeDefinition::Union {
            union_keyword,
            open_curly,
            variants,
            close_curly,
        })
    }

    fn parse_struct_definition(&mut self) -> Result<TypeDefinition, ParseError> {
        let struct_keyword = self.expect_token(TokenKind::StructKeyword)?;
        let open_curly = self.expect_token(TokenKind::OpenCurly)?;

        let mut members = Vec::new();
        while self.get_head()?.kind != TokenKind::CloseCurly {
            let member = self.parse_function_parameter()?;
            members.push(member);
            if self.get_head()?.kind != TokenKind::CloseCurly {
                self.expect_token(TokenKind::Comma)?;
            }
        }
        let close_curly = self.expect_token(TokenKind::CloseCurly)?;

        Ok(TypeDefinition::Struct {
            struct_keyword,
            open_curly,
            members,
            close_curly,
        })
    }

    fn parse_type_annotation_expression(&mut self) -> Result<Expression, ParseError> {
        let colon = self.expect_token(TokenKind::Colon)?;
        let type_expression = self.parse_type_expression()?;

        Ok(Expression::TypeAnnotation {
            colon,
            expression: type_expression,
        })
    }

    fn parse_type_expression(&mut self) -> Result<TypeExpression, ParseError> {
        //guaranteed for a token in peek()

        let head = &self.get_head()?.kind;
        match head {
            TokenKind::Identifier(_) => {
                let identifier = self.expect_identifier()?;
                Ok(TypeExpression::Basic(identifier))
            }
            TokenKind::Carot => {
                let carot = self.expect_token(TokenKind::Carot)?;
                let reference_type = self.parse_type_expression()?;
                Ok(TypeExpression::Pointer {
                    carot,
                    reference_type: Box::new(reference_type),
                })
            }
            TokenKind::Question => {
                let question = self.expect_token(TokenKind::Question)?;
                let reference_type = self.parse_type_expression()?;
                Ok(TypeExpression::Optional {
                    question,
                    reference_type: Box::new(reference_type),
                })
            }
            TokenKind::TypeLiteral(_) => {
                let type_literal = self.expect_type_literal()?;
                Ok(TypeExpression::Basic(type_literal))
            }
            TokenKind::OpenSquare => {
                let open_square = self.expect_token(TokenKind::OpenSquare)?;

                if self.get_head()?.kind == TokenKind::CloseSquare {
                    let close_square = self.expect_token(TokenKind::CloseSquare)?;
                    let element_type = self.parse_type_expression()?;

                    return Ok(TypeExpression::Slice {
                        open_square,
                        close_square,
                        element_type: Box::new(element_type),
                    });
                }

                let size = self.expect_int_literal()?;
                let close_square = self.expect_token(TokenKind::CloseSquare)?;
                let element_type = self.parse_type_expression()?;

                Ok(TypeExpression::Array {
                    open_square,
                    size,
                    close_square,
                    element_type: Box::new(element_type),
                })
            }
            TokenKind::OpenParen => {
                let open_paren = self.expect_token(TokenKind::OpenParen)?;

                let mut inputs = Vec::new();
                while self.get_head()?.kind != TokenKind::CloseParen {
                    inputs.push(self.parse_type_expression()?);

                    if self.get_head()?.kind != TokenKind::CloseParen {
                        self.expect_token(TokenKind::Comma)?;
                    }
                }

                let close_paren = self.expect_token(TokenKind::CloseParen)?;
                let skinny_arrow = self.expect_token(TokenKind::SkinnyArrow)?;

                let output = self.parse_type_expression()?; //TODO: optional return

                Ok(TypeExpression::Function {
                    open_paren,
                    inputs,
                    close_paren,
                    skinny_arrow,
                    output: Box::new(output),
                })
            }
            _ => Err(ParseError::UnexpectedToken(
                head.clone(),
                self.tokens[self.cursor].span.clone(),
            )),
        }
    }

    fn parse_binary_expression(
        &mut self,
        parent_precedence: usize,
    ) -> Result<Expression, ParseError> {
        let mut next_token = self.get_head()?.clone();
        let mut precedence = Self::get_unary_precedence(&next_token.kind);

        let mut left = if precedence <= parent_precedence {
            self.parse_primary()?
        } else {
            let unary_token = next_token;
            let unary_op = Self::parse_unary_operator(&unary_token.kind, &unary_token.span)?;
            self.cursor += 1;

            let operand = self.parse_binary_expression(precedence)?;

            Expression::Unary {
                op: UnaryOperation {
                    token: unary_token,
                    op: unary_op,
                },
                operand: Box::new(operand),
            }
        };

        if self.finished_parsing() {
            return Ok(left);
        }

        next_token = self.get_head()?.clone();
        //TODO: Is this the best way to do lookaheads?
        left = self.lookahead(left, &mut next_token)?;
        precedence = Self::get_binary_precedence(&next_token.kind);

        while !self.finished_parsing() && precedence > parent_precedence {
            let op = Self::parse_binary_operator(&next_token.kind, &next_token.span)?;
            self.cursor += 1;

            let right = self.parse_binary_expression(precedence)?;

            left = Expression::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };

            if self.finished_parsing() {
                return Ok(left);
            }

            next_token = self.get_head()?.clone();
            left = self.lookahead(left, &mut next_token)?;
            precedence = Self::get_binary_precedence(&next_token.kind);
        }

        Ok(left)
    }

    fn lookahead(
        &mut self,
        left: Expression,
        next_token: &mut Token,
    ) -> Result<Expression, ParseError> {
        let mut lookahead = true;
        let mut parsed = left;
        while lookahead {
            parsed = match next_token.kind {
                TokenKind::Equals => {
                    let lookahead = self.parse_assignment_expression(parsed)?;
                    *next_token = self.get_head()?.clone();
                    lookahead
                }
                TokenKind::Dot => {
                    let lookahead = self.parse_member_access_expression(parsed)?;
                    *next_token = self.get_head()?.clone();
                    lookahead
                }
                TokenKind::OpenSquare => {
                    let lookahead = self.parse_array_index_expression(parsed)?;
                    *next_token = self.get_head()?.clone();
                    lookahead
                }
                TokenKind::OpenParen => {
                    if let Expression::MemberAccess { .. } = parsed {
                        let lookahead = self.parse_method_call(parsed)?;
                        *next_token = self.get_head()?.clone();
                        lookahead
                    } else {
                        lookahead = false;
                        parsed
                    }
                }
                TokenKind::OpenCurly => {
                    if !self.allow_struct_literals {
                        return Ok(parsed);
                    }

                    if let Expression::MemberAccess { left, dot, member } = parsed {
                        let open_curly = self.expect_token(TokenKind::OpenCurly)?;

                        let mut arguments = Vec::new();
                        while self.get_head()?.kind != TokenKind::CloseCurly {
                            let identifier = self.expect_identifier()?;

                            if self.get_head()?.kind == TokenKind::Colon {
                                //TODO: The name + colon should be optional if the argument is a variable that matches the name of the member
                                let colon = self.expect_token(TokenKind::Colon)?;
                                let argument = self.parse_binary_expression(0)?;
                                arguments.push(Expression::NamedArgument {
                                    name: identifier,
                                    colon,
                                    argument: Box::new(argument),
                                });
                            } else if let TokenKind::Identifier(name) = identifier.kind {
                                arguments.push(Expression::Variable {
                                    name,
                                    span: identifier.span,
                                });
                            } else {
                                unreachable!()
                            }
                            if self.get_head()?.kind != TokenKind::CloseCurly {
                                self.expect_token(TokenKind::Comma)?;
                            }
                        }

                        let close_curly = self.expect_token(TokenKind::CloseCurly)?;

                        let lookahead = Expression::UnionLiteral {
                            left,
                            dot,
                            kind: UnionLiteralKind::Struct {
                                identifier: member,
                                open_curly,
                                arguments,
                                close_curly,
                            },
                        };

                        *next_token = self.get_head()?.clone();
                        lookahead
                    } else {
                        return Ok(parsed);
                    }
                }
                TokenKind::Carot => {
                    let carot = self.expect_token(TokenKind::Carot)?;
                    let lookahead = Expression::Dereference {
                        expression: Box::new(parsed),
                        carot,
                    };
                    *next_token = self.get_head()?.clone();
                    lookahead
                }
                TokenKind::Question => {
                    let question = self.expect_token(TokenKind::Question)?;
                    let lookahead = Expression::UnwrapOptional {
                        expression: Box::new(parsed),
                        question,
                    };
                    *next_token = self.get_head()?.clone();
                    lookahead
                }
                TokenKind::AsKeyword => {
                    let as_keyword = self.expect_token(TokenKind::AsKeyword)?;
                    let destination_type = self.parse_type_expression()?;

                    let lookahead = Expression::Cast {
                        expression: Box::new(parsed),
                        as_keyword,
                        destination_type,
                    };
                    *next_token = self.get_head()?.clone();
                    lookahead
                }
                _ => {
                    lookahead = false;
                    parsed
                }
            };
        }
        Ok(parsed)
    }

    fn parse_binary_operator(
        token_kind: &TokenKind,
        span: &Span,
    ) -> Result<BinaryOperator, ParseError> {
        match token_kind {
            TokenKind::Plus => Ok(BinaryOperator::Add),
            TokenKind::Minus => Ok(BinaryOperator::Sub),
            TokenKind::Star => Ok(BinaryOperator::Mul),
            TokenKind::Slash => Ok(BinaryOperator::Div),
            TokenKind::Percent => Ok(BinaryOperator::Mod),
            TokenKind::OpenAngle => Ok(BinaryOperator::Lt),
            TokenKind::OpenAngleEquals => Ok(BinaryOperator::LtEq),
            TokenKind::CloseAngle => Ok(BinaryOperator::Gt),
            TokenKind::CloseAngleEquals => Ok(BinaryOperator::GtEq),
            TokenKind::EqualsEquals => Ok(BinaryOperator::Eq),
            TokenKind::BangEquals => Ok(BinaryOperator::NEq),
            TokenKind::Amp => Ok(BinaryOperator::BitwiseAnd),
            TokenKind::Pipe => Ok(BinaryOperator::BitwiseOr),
            TokenKind::AmpAmp => Ok(BinaryOperator::LogicalAnd),
            TokenKind::PipePipe => Ok(BinaryOperator::LogicalOr),
            TokenKind::DotDot => Ok(BinaryOperator::Range),
            _ => Err(ParseError::UnexpectedToken(
                token_kind.clone(),
                span.clone(),
            )),
        }
    }

    fn parse_unary_operator(
        token_kind: &TokenKind,
        span: &Span,
    ) -> Result<UnaryOperator, ParseError> {
        match token_kind {
            TokenKind::Carot => Ok(UnaryOperator::Ref),
            TokenKind::Minus => Ok(UnaryOperator::Neg),
            TokenKind::Bang => Ok(UnaryOperator::Not),
            _ => Err(ParseError::UnexpectedToken(
                token_kind.clone(),
                span.clone(),
            )),
        }
    }

    fn get_binary_precedence(token_kind: &TokenKind) -> usize {
        match token_kind {
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => 10,
            TokenKind::Plus | TokenKind::Minus => 9,
            TokenKind::OpenAngle
            | TokenKind::OpenAngleEquals
            | TokenKind::CloseAngle
            | TokenKind::CloseAngleEquals => 8,
            TokenKind::EqualsEquals | TokenKind::BangEquals => 7,
            TokenKind::Amp => 6,
            TokenKind::Pipe => 4,
            TokenKind::AmpAmp => 3,
            TokenKind::PipePipe => 2,
            TokenKind::DotDot => 1,
            _ => 0,
        }
    }

    fn get_unary_precedence(token_kind: &TokenKind) -> usize {
        match token_kind {
            TokenKind::Minus => 3,
            TokenKind::Bang => 2,
            TokenKind::Carot => 1,
            _ => 0,
        }
    }

    fn parse_assignment_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        let equals = self.expect_token(TokenKind::Equals)?;
        let assignment = self.parse_binary_expression(0)?;

        Ok(Expression::Assignment {
            left: Box::new(left),
            equals,
            assignment: Box::new(assignment),
        })
    }

    fn parse_member_access_expression(
        &mut self,
        left: Expression,
    ) -> Result<Expression, ParseError> {
        let dot = self.expect_token(TokenKind::Dot)?;
        let member = self.expect_identifier()?;

        Ok(Expression::MemberAccess {
            left: Some(Box::new(left)),
            dot,
            member,
        })
    }

    fn parse_array_index_expression(
        &mut self,
        array: Expression,
    ) -> Result<Expression, ParseError> {
        let open_square = self.expect_token(TokenKind::OpenSquare)?;
        let index = self.parse_binary_expression(0)?;
        let close_square = self.expect_token(TokenKind::CloseSquare)?;

        Ok(Expression::ArrayIndex {
            array: Box::new(array),
            open_square,
            index: Box::new(index),
            close_square,
        })
    }

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        if self.finished_parsing() {
            return Err(ParseError::UnexpectedEOF);
        }

        let token = self.get_head()?.clone();
        self.cursor += 1;

        match &token.kind {
            TokenKind::NoneKeyword => Ok(Expression::NoneLiteral {
                span: token.span.clone(),
            }),
            TokenKind::IntLiteral(value) => Ok(Expression::IntLiteral {
                value: *value,
                span: token.span.clone(),
            }),
            TokenKind::FloatLiteral(value) => Ok(Expression::FloatLiteral {
                value: *value,
                span: token.span.clone(),
            }),
            TokenKind::TrueLiteral => Ok(Expression::BoolLiteral {
                value: true,
                span: token.span.clone(),
            }),
            TokenKind::FalseLiteral => Ok(Expression::BoolLiteral {
                value: false,
                span: token.span.clone(),
            }),
            TokenKind::StringLiteral(value) => Ok(Expression::StringLiteral {
                value: value.to_string(),
                span: token.span.clone(),
            }),
            TokenKind::CharLiteral(value) => Ok(Expression::CharLiteral {
                value: *value,
                span: token.span.clone(),
            }),
            TokenKind::OpenParen => {
                let open_paren = token.clone();
                let before = self.allow_var_decl;
                self.allow_var_decl = false;
                let expression = self.parse_binary_expression(0)?;

                if let Expression::FunctionParameter { .. } = &expression {
                    let mut parameters = Vec::new();
                    parameters.push(expression);
                    while self.get_head()?.kind != TokenKind::CloseParen {
                        self.expect_token(TokenKind::Comma)?;
                        let parameter = self.parse_binary_expression(0)?;
                        parameters.push(parameter);
                    }
                    let close_paren = self.expect_token(TokenKind::CloseParen)?;

                    let arrow = self.expect_token(TokenKind::SkinnyArrow)?;
                    let body = self.parse_binary_expression(0)?;

                    self.allow_var_decl = before;

                    return Ok(Expression::Lambda {
                        open_paren,
                        parameters,
                        close_paren,
                        arrow,
                        body: Box::new(body),
                    });
                }

                let close_paren = self.expect_token(TokenKind::CloseParen)?;
                self.allow_var_decl = before;

                Ok(Expression::Parenthesised {
                    open_paren,
                    expression: Box::new(expression),
                    close_paren,
                })
            }
            TokenKind::OpenSquare => {
                //step back to parse with the opening '['
                self.cursor -= 1;
                self.parse_array_literal()
            }
            TokenKind::Identifier(identifier) => {
                let allow_var_decl = self.allow_var_decl;
                let allow_multi_assign = self.allow_multi_assign;
                let next = &self.get_head()?.kind;
                if (next == &TokenKind::Colon && allow_var_decl)
                    || (next == &TokenKind::Comma && allow_multi_assign)
                {
                    return self.parse_var_declaration(token.clone());
                }

                if next == &TokenKind::Colon && !allow_var_decl {
                    let type_annotation = self.parse_type_annotation_expression()?;

                    return Ok(Expression::FunctionParameter {
                        identifiers: vec![token],
                        type_annotation: Box::new(type_annotation),
                    });
                }

                if next == &TokenKind::OpenParen {
                    return self.parse_function_call(&token);
                }

                if next == &TokenKind::OpenCurly && self.allow_struct_literals {
                    return self.parse_struct_literal(&token);
                }

                Ok(Expression::Variable {
                    name: identifier.clone(),
                    span: token.span.clone(),
                })
            }
            TokenKind::Dot => {
                let dot = token;
                let member = self.expect_identifier()?;

                let parsed = Expression::MemberAccess {
                    left: None,
                    dot,
                    member,
                };

                let mut next_token = self.get_head()?.clone();

                self.lookahead(parsed, &mut next_token)
            }

            TokenKind::GuardKeyword => {
                //guard expression! else is mandatory!
                let guard_keyword = token;
                let expression = self.parse_binary_expression(5)?;

                if self.get_head()?.kind == TokenKind::Pipe {
                    todo!("Captures in positive guard expressions")
                }

                let else_keyword = self.expect_token(TokenKind::ElseKeyword)?;

                if self.get_head()?.kind == TokenKind::Pipe {
                    todo!("Captures in negative guard expressions")
                }

                let body = self.parse_statement()?;

                let parsed = Expression::Guard {
                    guard_keyword,
                    expression: Box::new(expression),
                    else_keyword,
                    body: Box::new(body),
                };

                let mut next_token = self.get_head()?.clone();

                self.lookahead(parsed, &mut next_token)
            }
            _ => Err(ParseError::UnexpectedToken(
                token.kind.clone(),
                token.span.clone(),
            )),
        }
    }

    fn parse_struct_literal(&mut self, token: &Token) -> Result<Expression, ParseError> {
        let identifier = token.clone();
        let open_curly = self.expect_token(TokenKind::OpenCurly)?;

        let mut arguments = Vec::new();

        let before_decl = self.allow_var_decl;
        let before_multi = self.allow_multi_assign;
        self.allow_var_decl = false;
        self.allow_multi_assign = false;
        while self.get_head()?.kind != TokenKind::CloseCurly {
            let identifier = self.expect_identifier()?;

            if self.get_head()?.kind == TokenKind::Colon {
                //TODO: The name + colon should be optional if the argument is a variable that matches the name of the member
                let colon = self.expect_token(TokenKind::Colon)?;
                let argument = self.parse_binary_expression(0)?;
                arguments.push(Expression::NamedArgument {
                    name: identifier,
                    colon,
                    argument: Box::new(argument),
                });
            } else if let TokenKind::Identifier(name) = identifier.kind {
                arguments.push(Expression::Variable {
                    name,
                    span: identifier.span,
                });
            } else {
                unreachable!()
            }
            if self.get_head()?.kind != TokenKind::CloseCurly {
                self.expect_token(TokenKind::Comma)?;
            }
        }
        self.allow_var_decl = before_decl;
        self.allow_multi_assign = before_multi;
        let close_curly = self.expect_token(TokenKind::CloseCurly)?;

        Ok(Expression::StructLiteral {
            identifier,
            open_curly,
            arguments,
            close_curly,
        })
    }

    fn parse_function_call(&mut self, token: &Token) -> Result<Expression, ParseError> {
        let identifier = token.clone();
        let open_paren = self.expect_token(TokenKind::OpenParen)?;
        let mut arguments = Vec::new();
        self.allow_multi_assign = false;
        while self.get_head()?.kind != TokenKind::CloseParen {
            let argument = self.parse_binary_expression(0)?;
            arguments.push(argument);

            if self.get_head()?.kind != TokenKind::CloseParen {
                self.expect_token(TokenKind::Comma)?;
            }
        }
        self.allow_multi_assign = true;
        let close_paren = self.expect_token(TokenKind::CloseParen)?;
        Ok(Expression::FunctionCall {
            identifier,
            open_paren,
            arguments,
            close_paren,
        })
    }

    fn parse_method_call(&mut self, callee: Expression) -> Result<Expression, ParseError> {
        if let Expression::MemberAccess { left, dot, member } = callee {
            if let Some(left) = left {
                let open_paren = self.expect_token(TokenKind::OpenParen)?;
                let mut arguments = Vec::new();
                self.allow_multi_assign = false;
                while self.get_head()?.kind != TokenKind::CloseParen {
                    let argument = self.parse_binary_expression(0)?;
                    arguments.push(argument);
                    if self.get_head()?.kind != TokenKind::CloseParen {
                        self.expect_token(TokenKind::Comma)?;
                    }
                }
                self.allow_multi_assign = true;
                let close_paren = self.expect_token(TokenKind::CloseParen)?;
                Ok(Expression::MethodCall {
                    callee: left,
                    dot,
                    identifier: member,
                    open_paren,
                    arguments,
                    close_paren,
                })
            } else {
                unreachable!()
            }
        } else {
            todo!("inferring the callee of a method?");
        }
    }

    fn parse_function_parameter(&mut self) -> Result<Expression, ParseError> {
        //TODO: any modifiers
        let mut identifiers = Vec::new();
        identifiers.push(self.expect_identifier()?);

        while self.get_head()?.kind == TokenKind::Comma {
            self.expect_token(TokenKind::Comma)?;
            identifiers.push(self.expect_identifier()?);
        }

        let type_annotation = self.parse_type_annotation_expression()?;

        Ok(Expression::FunctionParameter {
            identifiers,
            type_annotation: Box::new(type_annotation),
        })
    }

    fn parse_array_literal(&mut self) -> Result<Expression, ParseError> {
        let array_type = self.parse_type_expression()?;
        let open_curly = self.expect_token(TokenKind::OpenCurly)?;

        let mut elements = Vec::new();
        while self.get_head()?.kind != TokenKind::CloseCurly {
            elements.push(self.parse_binary_expression(0)?);
            if self.get_head()?.kind != TokenKind::CloseCurly {
                self.expect_token(TokenKind::Comma)?;
            }
        }
        let close_curly = self.expect_token(TokenKind::CloseCurly)?;
        Ok(Expression::ArrayLiteral {
            type_expression: array_type,
            open_curly,
            elements,
            close_curly,
        })
    }

    fn expect_token(&mut self, expected: TokenKind) -> Result<Token, ParseError> {
        match self.tokens.get(self.cursor) {
            Some(token) => {
                if token.kind == expected {
                    self.cursor += 1;
                    return Ok(token.clone());
                }
                Err(ParseError::TokenMismatch {
                    expected,
                    actual: token.kind.clone(),
                    span: token.span.clone(),
                })
            }
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn expect_identifier(&mut self) -> Result<Token, ParseError> {
        match self.tokens.get(self.cursor) {
            Some(token) => match &token.kind {
                TokenKind::Identifier(_) => {
                    self.cursor += 1;
                    Ok(token.clone())
                }
                _ => Err(ParseError::TokenMismatchExpected {
                    expected: "identifier".to_string(),
                    actual: token.kind.clone(),
                    span: token.span.clone(),
                }),
            },
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn expect_int_literal(&mut self) -> Result<Token, ParseError> {
        match self.tokens.get(self.cursor) {
            Some(token) => match &token.kind {
                TokenKind::IntLiteral(_) => {
                    self.cursor += 1;
                    Ok(token.clone())
                }
                _ => Err(ParseError::TokenMismatchExpected {
                    expected: "integer literal".to_string(),
                    actual: token.kind.clone(),
                    span: token.span.clone(),
                }),
            },
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn expect_string_literal(&mut self) -> Result<Token, ParseError> {
        match self.tokens.get(self.cursor) {
            Some(token) => match &token.kind {
                TokenKind::StringLiteral(_) => {
                    self.cursor += 1;
                    Ok(token.clone())
                }
                _ => Err(ParseError::TokenMismatchExpected {
                    expected: "string literal".to_string(),
                    actual: token.kind.clone(),
                    span: token.span.clone(),
                }),
            },
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn expect_type_literal(&mut self) -> Result<Token, ParseError> {
        match self.tokens.get(self.cursor) {
            Some(token) => match &token.kind {
                TokenKind::TypeLiteral(_) => {
                    self.cursor += 1;
                    Ok(token.clone())
                }
                _ => Err(ParseError::TokenMismatchExpected {
                    expected: "integer literal".to_string(),
                    actual: token.kind.clone(),
                    span: token.span.clone(),
                }),
            },
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn get_head(&mut self) -> Result<&Token, ParseError> {
        match self.tokens.get(self.cursor) {
            Some(token) => {
                let mut token = token;
                while let TokenKind::LineComment(_) = token.kind {
                    self.cursor += 1;
                    match self.tokens.get(self.cursor) {
                        Some(next) => {
                            token = next;
                        }
                        None => {
                            return Err(ParseError::UnexpectedEOF);
                        }
                    }
                }
                Ok(token)
            }
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn finished_parsing(&self) -> bool {
        self.cursor >= self.tokens.len()
    }
}
