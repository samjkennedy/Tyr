use rand::{distributions::Alphanumeric, thread_rng, Rng};
use std::{collections::HashMap, fmt::Display, fs, iter::zip, process::exit};

use crate::{
    display_lex_error, display_parse_error, display_type_error,
    lexer::{lex, Span, Token, TokenKind},
    parser::{
        BinaryOperator, Expression, Parser, Pattern, Statement, TypeDefinition, TypeExpression,
        UnaryOperator,
        UnionLiteralKind::{self, Enum},
        UnionVariantKind,
    },
};

#[derive(Debug, Clone)]
pub struct Module {
    //Todo: make own struct for these types of statements?
    //pub imports: Vec<CheckedStatement>,
    pub modules: Vec<Module>,
    pub types: Vec<CheckedStatement>,
    pub functions: Vec<CheckedStatement>,
    pub global_variables: Vec<CheckedStatement>,
}

impl Module {
    fn new() -> Module {
        Module {
            modules: Vec::new(),
            types: Vec::new(),
            functions: Vec::new(),
            global_variables: Vec::new(),
        }
    }
}

struct Scope {
    variables: HashMap<String, CheckedVariable>,
    functions: HashMap<String, CheckedFunction>,
    types: HashMap<String, TypeKind>,
    return_context: TypeKind,
    assign_context: Option<TypeKind>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
            types: HashMap::new(),
            return_context: TypeKind::Unit,
            assign_context: None,
        }
    }

    fn try_declare_variable(&mut self, variable: CheckedVariable) -> Result<(), TypeErrorKind> {
        if self.variables.contains_key(&variable.name) {
            return Err(TypeErrorKind::VariableAlreadyDeclaredInScope {
                name: variable.name,
            });
        }

        self.variables
            .insert(variable.name.clone(), variable.clone());

        if let TypeKind::Function { inputs, output } = variable.type_kind {
            //Force insert, don't check
            self.functions.insert(
                variable.name.clone(),
                CheckedFunction {
                    name: variable.name,
                    parameters: inputs
                        .iter()
                        .enumerate()
                        .map(|(i, v)| CheckedFunctionParameter {
                            variable: CheckedVariable {
                                name: i.to_string(),
                                type_kind: v.clone(),
                                mutable: false,
                            },
                        })
                        .collect::<Vec<CheckedFunctionParameter>>(),
                    return_type: *output,
                },
            );
        }

        Ok(())
    }

    fn try_get_variable(&self, name: &String) -> Option<CheckedVariable> {
        return if self.variables.contains_key(name) {
            Some(self.variables.get(name).unwrap().clone())
        } else {
            None
        };
    }

    fn try_declare_function(&mut self, function: CheckedFunction) -> Result<(), TypeErrorKind> {
        if self.functions.contains_key(&function.name) {
            return Err(TypeErrorKind::FunctionAlreadyDeclaredInScope {
                name: function.name,
            });
        }

        self.functions
            .insert(function.name.clone(), function.clone());

        self.try_declare_variable(CheckedVariable {
            name: function.name,
            type_kind: TypeKind::Function {
                inputs: function
                    .parameters
                    .iter()
                    .map(|p| p.variable.type_kind.clone())
                    .collect::<Vec<TypeKind>>(),
                output: Box::new(function.return_type),
            },
            mutable: false,
        })?;

        Ok(())
    }

    fn try_get_function(&self, name: &String) -> Option<CheckedFunction> {
        return if self.functions.contains_key(name) {
            Some(self.functions.get(name).unwrap().clone())
        } else {
            None
        };
    }

    fn try_declare_type(&mut self, type_kind: TypeKind) -> Result<(), TypeErrorKind> {
        match &type_kind {
            TypeKind::Struct { name, .. }
            | TypeKind::Enum { name, .. }
            | TypeKind::Union { name, .. } => {
                if self.types.contains_key(&name.clone()) {
                    return Err(TypeErrorKind::TypeAlreadyDefinedInScope { name: name.clone() });
                }

                self.types.insert(name.clone(), type_kind);

                Ok(())
            }
            _ => {
                self.types.insert(type_kind.to_string(), type_kind);
                Ok(())
            }
        }
    }

    fn try_get_type(&self, name: &String) -> Option<TypeKind> {
        return if self.types.contains_key(name) {
            Some(self.types.get(name).unwrap().clone())
        } else {
            None
        };
    }
}

pub struct TypeChecker {
    path: String,
    module: Module,
    scopes: Vec<Scope>,
    deferred_stmts: Vec<CheckedStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CheckedUnionVariantKind {
    Enum,
    Struct {
        members: Vec<CheckedFunctionParameter>,
    },
    // Tuple {
    //     members: Vec<String>,
    // },
}

#[derive(Debug, Clone, PartialEq)]
pub struct CheckedUnionVariant {
    pub name: String,
    pub tag: usize,
    pub kind: CheckedUnionVariantKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CheckedTypeDefinition {
    Struct {
        members: Vec<CheckedFunctionParameter>,
    },
    Enum {
        members: Vec<String>,
    },
    Union {
        variants: Vec<CheckedUnionVariant>,
    },
    UnionVariant {
        tag: usize,
        members: Vec<CheckedFunctionParameter>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Unit,
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    F128,
    Char,
    String,
    Array {
        size: usize,
        element_type: Box<TypeKind>,
    },
    Slice {
        element_type: Box<TypeKind>,
    },
    Type {
        type_kind: Box<TypeKind>,
    },
    Range {
        type_kind: Box<TypeKind>,
    },
    Struct {
        name: String,
        members: Vec<CheckedFunctionParameter>,
    },
    Enum {
        name: String,
        members: Vec<String>,
    },
    Union {
        name: String,
        variants: Vec<CheckedUnionVariant>,
    },
    UnionVariant {
        name: String,
        variant: CheckedUnionVariant,
    },
    Pointer {
        reference_type: Box<TypeKind>,
    },
    Function {
        inputs: Vec<TypeKind>,
        output: Box<TypeKind>,
    },
    FileHandle,
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeKind::Unit => write!(f, "()"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::I8 => write!(f, "i8"),
            TypeKind::I16 => write!(f, "i16"),
            TypeKind::I32 => write!(f, "i32"),
            TypeKind::I64 => write!(f, "i64"),
            TypeKind::U8 => write!(f, "u8"),
            TypeKind::U16 => write!(f, "u16"),
            TypeKind::U32 => write!(f, "u32"),
            TypeKind::U64 => write!(f, "u64"),
            TypeKind::F32 => write!(f, "f32"),
            TypeKind::F64 => write!(f, "f64"),
            TypeKind::F128 => write!(f, "f128"),
            TypeKind::Char => write!(f, "char"),
            TypeKind::String => write!(f, "string"),
            TypeKind::Type { type_kind } => write!(f, "type<{}>", type_kind),
            TypeKind::Range { type_kind } => write!(f, "range<{}>", type_kind),
            TypeKind::Array { size, element_type } => write!(f, "[{}]{}", size, element_type),
            TypeKind::Slice { element_type } => write!(f, "[]{}", element_type),
            TypeKind::Struct { name, .. }
            | TypeKind::Enum { name, .. }
            | TypeKind::Union { name, .. }
            | TypeKind::UnionVariant { name, .. } => write!(f, "{}", name),
            TypeKind::Pointer { reference_type } => write!(f, "^{}", reference_type),
            TypeKind::Function { inputs, output } => {
                write!(
                    f,
                    "({}) -> {}",
                    inputs
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    output
                )
            }
            TypeKind::FileHandle => write!(f, "FILE_HANDLE"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CheckedStatement {
    FunctionDeclaration {
        function: CheckedFunction,
        body: Box<CheckedStatement>,
    },
    Return {
        expression: Option<CheckedExpression>,
    },
    Expression {
        expression: CheckedExpression,
    },
    Block {
        statements: Vec<CheckedStatement>,
    },
    While {
        condition: CheckedExpression,
        body: Box<CheckedStatement>,
    },
    If {
        condition: CheckedExpression,
        body: Box<CheckedStatement>,
        else_clause: Option<Box<CheckedStatement>>,
    },
    Else {
        body: Box<CheckedStatement>,
    },
    TypeDefinition {
        type_kind: TypeKind,
    },
    For {
        iterator: CheckedVariable,
        iterable: CheckedExpression,
        body: Box<CheckedStatement>,
    },
    Match {
        expression: Box<CheckedExpression>,
        arms: Vec<CheckedExpression>,
    },
    Uses {
        module: Module,
    },
    Raw {
        lines: Vec<String>,
    },
    Noop,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CheckedVariable {
    pub name: String,
    pub type_kind: TypeKind,
    pub mutable: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CheckedFunctionParameter {
    pub variable: CheckedVariable,
}

#[derive(Debug, Clone)]
pub struct CheckedFunction {
    pub name: String,
    pub parameters: Vec<CheckedFunctionParameter>,
    pub return_type: TypeKind,
}

#[derive(Debug, Clone)]
pub struct CheckedBinaryOp {
    pub op: BinaryOperator,
    pub return_type: TypeKind,
}

#[derive(Debug, Clone)]
pub struct CheckedUnaryOp {
    pub op: UnaryOperator,
    pub return_type: TypeKind,
}

#[derive(Debug, Clone)]
pub enum CheckedExpression {
    IntLiteral {
        value: usize,
        type_kind: TypeKind,
    },
    FloatLiteral {
        value: f64,
        type_kind: TypeKind,
    },
    BoolLiteral {
        value: bool,
    },
    CharLiteral {
        value: char,
    },
    StringLiteral {
        value: String,
    },
    ArrayLiteral {
        type_kind: TypeKind,
        elements: Vec<CheckedExpression>,
    },
    VariableDeclaration {
        variables: Vec<CheckedVariable>,
        initialiser: Box<CheckedExpression>,
    },
    Variable(CheckedVariable),
    FunctionCall {
        function: CheckedFunction,
        arguments: Vec<CheckedExpression>,
    },
    Binary {
        left: Box<CheckedExpression>,
        op: CheckedBinaryOp,
        right: Box<CheckedExpression>,
    },
    Unary {
        op: CheckedUnaryOp,
        operand: Box<CheckedExpression>,
    },
    Parenthesised {
        expression: Box<CheckedExpression>,
    },
    Assignment {
        left: Box<CheckedExpression>,
        assignment: Box<CheckedExpression>,
    },
    StructLiteral {
        type_kind: TypeKind,
        arguments: Vec<CheckedExpression>,
    },
    NamedArgument {
        name: String,
        argument: Box<CheckedExpression>,
    },
    MemberAccess {
        accessee: Option<Box<CheckedExpression>>,
        member: CheckedVariable,
    },
    Type {
        type_kind: TypeKind,
    },
    Path {
        segments: Vec<CheckedExpression>,
        member: Box<CheckedExpression>,
    },
    ArrayIndex {
        array: Box<CheckedExpression>,
        index: Box<CheckedExpression>,
    },
    Slice {
        array: Box<CheckedExpression>,
        range: Box<CheckedExpression>,
    },
    MatchArm {
        //TODO match expressions
        pattern: Box<CheckedPattern>,
        body: Box<CheckedStatement>,
    },
    Dereference {
        expression: Box<CheckedExpression>,
    },
    UnionLiteral {
        union_type: TypeKind,
        variant: CheckedUnionVariant,
        arguments: Vec<CheckedExpression>,
    },
    Cast {
        expression: Box<CheckedExpression>,
        destination: TypeKind,
    },
    Lambda {
        name: String, // c-specific, move this out
        parameters: Vec<CheckedFunctionParameter>,
        body: Box<CheckedExpression>,
        type_kind: TypeKind, //precomputed for you
    },
}

#[derive(Debug, Clone)]
pub enum CheckedPattern {
    //TODO: Merge the paths
    VariablePath {
        path: Vec<CheckedExpression>,
        value: CheckedVariable,
    },
    UnionVariantPath {
        path: Vec<CheckedExpression>,
        struct_type: TypeKind,
        tag: usize,
        arguments: Vec<CheckedFunctionParameter>,
    },
    Wildcard(TypeKind),
}

impl Display for CheckedPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CheckedPattern::VariablePath { value, .. } => {
                write!(f, "{}", value.name)
            }
            CheckedPattern::UnionVariantPath { struct_type, .. } => {
                write!(f, "{}", struct_type)
            }
            CheckedPattern::Wildcard(_) => write!(f, "wildcard"),
        }
    }
}

impl CheckedPattern {
    pub fn get_type(&self) -> TypeKind {
        match self {
            CheckedPattern::VariablePath { path: _, value } => value.type_kind.clone(),
            CheckedPattern::UnionVariantPath { struct_type, .. } => struct_type.clone(),
            CheckedPattern::Wildcard(type_kind) => type_kind.clone(),
        }
    }
}

impl CheckedExpression {
    pub fn get_type(&self) -> TypeKind {
        match self {
            CheckedExpression::IntLiteral { type_kind, .. }
            | CheckedExpression::FloatLiteral { type_kind, .. } => type_kind.clone(),
            CheckedExpression::BoolLiteral { .. } => TypeKind::Bool,
            CheckedExpression::CharLiteral { .. } => TypeKind::Char,
            CheckedExpression::StringLiteral { .. } => TypeKind::String,
            CheckedExpression::VariableDeclaration { initialiser, .. } => initialiser.get_type(),
            CheckedExpression::Variable(variable) => variable.type_kind.clone(),
            CheckedExpression::FunctionCall { function, .. } => function.return_type.clone(),
            CheckedExpression::Binary { op, .. } => op.return_type.clone(),
            CheckedExpression::Unary { op, .. } => op.return_type.clone(),
            CheckedExpression::Parenthesised { expression } => expression.get_type(),
            CheckedExpression::Assignment { left, .. } => left.get_type(),
            CheckedExpression::StructLiteral { type_kind, .. } => type_kind.to_owned(),
            CheckedExpression::NamedArgument { argument, .. } => argument.get_type(),
            CheckedExpression::MemberAccess { member, .. } => member.type_kind.clone(),
            CheckedExpression::Path { segments, member } => match segments.last() {
                Some(last) => match last.get_type() {
                    TypeKind::Type { type_kind } => *type_kind.clone(),
                    _ => last.get_type(),
                },
                None => member.get_type(),
            },
            CheckedExpression::Type { type_kind } => TypeKind::Type {
                type_kind: Box::new(type_kind.clone()),
            },
            CheckedExpression::ArrayLiteral { type_kind, .. } => type_kind.clone(),
            CheckedExpression::ArrayIndex { array, .. } => match array.get_type() {
                TypeKind::Array { element_type, .. } | TypeKind::Slice { element_type, .. } => {
                    *element_type
                }
                TypeKind::Pointer { reference_type } => *reference_type,
                TypeKind::String => TypeKind::Char,
                _ => unreachable!(),
            },

            CheckedExpression::Slice { array, .. } => match array.get_type() {
                TypeKind::Array { element_type, .. } | TypeKind::Slice { element_type, .. } => {
                    TypeKind::Slice { element_type }
                }
                TypeKind::Pointer { reference_type } => TypeKind::Slice {
                    element_type: reference_type,
                },
                TypeKind::String => TypeKind::Slice {
                    element_type: Box::new(TypeKind::Char),
                },
                _ => unreachable!(),
            },
            CheckedExpression::MatchArm { pattern, .. } => pattern.get_type(),
            CheckedExpression::Dereference { expression } => {
                if let TypeKind::Pointer { reference_type } = expression.get_type() {
                    *reference_type.clone()
                } else {
                    unreachable!()
                }
            }
            CheckedExpression::UnionLiteral { union_type, .. } => union_type.clone(),
            CheckedExpression::Cast { destination, .. } => destination.clone(),
            CheckedExpression::Lambda { type_kind, .. } => type_kind.clone(),
        }
    }
}

#[derive(Debug)]
pub enum TypeErrorKind {
    VariableAlreadyDeclaredInScope {
        name: String,
    },
    NoSuchVariableDeclaredInScope {
        name: String,
    },
    FunctionAlreadyDeclaredInScope {
        name: String,
    },
    NoSuchFunctionDeclaredInScope {
        name: String,
        types: Vec<TypeKind>,
    },
    NoSuchFunctionNameDeclaredInScope {
        name: String,
    },
    TypeAlreadyDefinedInScope {
        name: String,
    },
    NoSuchTypeDefinedInScope {
        name: String,
    },
    TypeMismatch {
        expected: TypeKind,
        actual: TypeKind,
    },
    BinaryOperationNotApplicableToTypes {
        left: TypeKind,
        op: BinaryOperator,
        right: TypeKind,
    },
    CannotMutableConstValue {
        variable: CheckedVariable,
    },
    StructHasNoMember {
        struct_name: String,
        name: String,
    },
    StructMembersMissing {
        struct_name: String,
        expected_parameters: Vec<CheckedVariable>,
    },
    TypeHasNoMembers {
        type_kind: TypeKind,
    },
    NoSuchMemberInStruct {
        type_kind: TypeKind,
        name: String,
    },
    MemberAlreadyDeclaredInType {
        name: String,
    },
    EnumHasNoMember {
        enum_name: String,
        member_name: String,
    },
    NotAnArrayType,
    ArrayElementsCountMismatch {
        expected: usize,
        actual: usize,
    },
    CannotIndexType {
        type_kind: TypeKind,
    },
    CannotSliceType {
        type_kind: TypeKind,
    },
    TypeIsNotIterable {
        type_kind: TypeKind,
    },
    NotAllPathsReturnAValue,
    MatchAlreadyExhaustive,
    MatchArmAlreadyChecked {
        pattern: CheckedPattern,
    },
    MatchIsNotExhaustive,
    CannotInferType,
    UnaryOperationNotApplicableToType {
        op: UnaryOperator,
        operand: TypeKind,
    },
    CannotDereferenceType {
        type_kind: TypeKind,
    },
    MainMustHaveNoReturn,
    IncorrectMainParameters,
    ExpectedIdentifier,
    VariantIsNotAStruct,
    CannotCastToType {
        from: TypeKind,
        to: TypeKind,
    },
}

impl Display for TypeErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeErrorKind::VariableAlreadyDeclaredInScope { name } => {
                write!(f, "variable `{name}` is already declared in scope")
            }
            TypeErrorKind::NoSuchVariableDeclaredInScope { name } => {
                write!(f, "no such variable `{name}` in scope")
            }
            TypeErrorKind::FunctionAlreadyDeclaredInScope { name } => {
                write!(f, "function `{name}` is already declared in scope")
            }
            TypeErrorKind::NoSuchFunctionDeclaredInScope { name, types } => {
                write!(
                    f,
                    "no such function `{name}({})` declared in scope",
                    types
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            TypeErrorKind::NoSuchFunctionNameDeclaredInScope { name } => {
                write!(f, "no such function `{name}` declared in scope",)
            }
            TypeErrorKind::TypeAlreadyDefinedInScope { name } => {
                write!(f, "type `{name}` is already defined in scope")
            }
            TypeErrorKind::NoSuchTypeDefinedInScope { name } => {
                write!(f, "no such type `{name}` declared in scope")
            }
            TypeErrorKind::TypeMismatch { expected, actual } => {
                write!(f, "expected `{expected}` but got `{actual}`")
            }
            TypeErrorKind::BinaryOperationNotApplicableToTypes { left, op, right } => {
                write!(
                    f,
                    "binary operation `{op:?}` is not defined for types `{left}` and `{right}`"
                )
            }
            TypeErrorKind::UnaryOperationNotApplicableToType { op, operand } => {
                write!(
                    f,
                    "unary operation `{op:?}` is not defined for type `{operand}`"
                )
            }
            TypeErrorKind::CannotMutableConstValue { variable } => {
                write!(f, "cannot mutate constant value `{}`", variable.name)
            }
            TypeErrorKind::StructHasNoMember { struct_name, name } => {
                write!(f, "struct `{}` has no member `{}`", struct_name, name)
            }
            TypeErrorKind::StructMembersMissing {
                struct_name,
                expected_parameters,
            } => write!(
                f,
                "missing members `{}` for struct `{}`",
                expected_parameters
                    .iter()
                    .map(|v| v.name.clone())
                    .collect::<Vec<String>>()
                    .join(", "),
                struct_name
            ),
            TypeErrorKind::TypeHasNoMembers { type_kind } => {
                write!(f, "type `{}` is primitive and has no members", type_kind)
            }
            TypeErrorKind::NoSuchMemberInStruct { type_kind, name } => {
                write!(f, "type `{}` has no member `{}`", type_kind, name)
            }

            TypeErrorKind::MemberAlreadyDeclaredInType { name } => {
                write!(f, "member `{}` is already declared in type", name)
            }
            TypeErrorKind::EnumHasNoMember {
                enum_name,
                member_name,
            } => write!(f, "enum `{}` has no member `{}`", enum_name, member_name),

            TypeErrorKind::NotAnArrayType => write!(f, "not an array type"),
            TypeErrorKind::ArrayElementsCountMismatch { expected, actual } => {
                write!(
                    f,
                    "array element count mismatch, expected `{expected}` elements but got `{actual}`",
                )
            }
            TypeErrorKind::CannotIndexType { type_kind } => {
                write!(f, "cannot index type `{}`", type_kind)
            }
            TypeErrorKind::CannotSliceType { type_kind } => {
                write!(f, "cannot slice type `{}`", type_kind)
            }
            TypeErrorKind::TypeIsNotIterable { type_kind } => {
                write!(f, "type `{}` is not iterable", type_kind)
            }
            TypeErrorKind::NotAllPathsReturnAValue => write!(f, "not all paths return a value"),
            TypeErrorKind::MatchAlreadyExhaustive => {
                write!(f, "redundant match arm, match is already exhaustive")
            }
            TypeErrorKind::MatchArmAlreadyChecked { pattern } => {
                write!(
                    f,
                    "redundant match arm, pattern `{}` is already checked",
                    pattern
                )
            }
            TypeErrorKind::MatchIsNotExhaustive => write!(f, "match is not exhaustive"),
            TypeErrorKind::CannotInferType => write!(f, "cannot infer type"),
            TypeErrorKind::CannotDereferenceType { type_kind } => {
                write!(f, "cannot dereference non pointer type `{}`", type_kind)
            }
            TypeErrorKind::MainMustHaveNoReturn => {
                write!(f, "main function must have no return type")
            }
            TypeErrorKind::IncorrectMainParameters => write!(
                f,
                "incorrect parameters to main, must be none or `[]string`"
            ),
            TypeErrorKind::ExpectedIdentifier => write!(f, "expected identifier"),
            TypeErrorKind::VariantIsNotAStruct => write!(f, "variant is not a struct"),
            TypeErrorKind::CannotCastToType { from, to } => {
                write!(f, "cannot cast from {from} to {to}")
            }
        }
    }
}

#[derive(Debug)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Span,
}

impl TypeChecker {
    pub fn new(path: String) -> TypeChecker {
        let mut scopes = Vec::new();
        let mut global_scope = Scope::new();

        let built_in_types = vec![
            TypeKind::Bool,
            TypeKind::U8,
            TypeKind::U16,
            TypeKind::U32,
            TypeKind::U64,
            TypeKind::I8,
            TypeKind::I16,
            TypeKind::I32,
            TypeKind::I64,
            TypeKind::F32,
            TypeKind::F64,
            TypeKind::F128,
            TypeKind::Char,
            TypeKind::String,
            TypeKind::FileHandle,
        ];

        for built_in_type in built_in_types {
            global_scope
                .try_declare_type(built_in_type)
                .expect("Could not declare built in type in global scope");
        }
        scopes.push(global_scope);

        let module = Module::new();

        TypeChecker {
            path,
            scopes,
            module,
            deferred_stmts: Vec::new(),
        }
    }

    pub fn type_check(&mut self, statements: Vec<Statement>) -> Result<Module, TypeError> {
        for statement in statements {
            let checked_statement = self.type_check_statement(statement)?;

            match &checked_statement {
                CheckedStatement::FunctionDeclaration { .. } => {
                    self.module.functions.push(checked_statement)
                }
                CheckedStatement::TypeDefinition { type_kind } => match &type_kind {
                    TypeKind::Struct { name: _, .. }
                    | TypeKind::Enum { name: _, .. }
                    | TypeKind::Union { name: _, .. } => {
                        self.module.types.push(checked_statement);
                    }
                    _ => unreachable!("Cannot put {:?} into top level module", checked_statement),
                },
                CheckedStatement::Uses { module } => {
                    self.module.modules.push(module.clone());
                }
                _ => unreachable!("Cannot put {:?} into top level module", checked_statement),
            }
        }

        Ok(self.module.clone())
    }

    fn define_slice_type_in_module(&mut self, element_type: &TypeKind) {
        let name = format!("slice_{}", element_type);

        let definition_statement = CheckedStatement::TypeDefinition {
            type_kind: TypeKind::Struct {
                name: name.clone(),
                members: vec![
                    CheckedFunctionParameter {
                        variable: CheckedVariable {
                            name: "offset".to_string(),
                            type_kind: TypeKind::I32,
                            mutable: false,
                        },
                    },
                    CheckedFunctionParameter {
                        variable: CheckedVariable {
                            name: "len".to_string(),
                            type_kind: TypeKind::I32,
                            mutable: false,
                        },
                    },
                    CheckedFunctionParameter {
                        variable: CheckedVariable {
                            name: "data".to_string(),
                            type_kind: TypeKind::Pointer {
                                reference_type: Box::new(element_type.clone()),
                            },
                            mutable: false,
                        },
                    },
                ],
            },
        };

        self.module.types.push(definition_statement);
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn try_declare_variable(
        &mut self,
        span: Span,
        variable: CheckedVariable,
    ) -> Result<(), TypeError> {
        if let TypeKind::Slice { element_type } = &variable.type_kind {
            self.define_slice_type_in_module(element_type);
        }

        let local_scope = self.scopes.last_mut().unwrap();

        match local_scope.try_declare_variable(variable) {
            Ok(_) => Ok(()),
            Err(kind) => Err(TypeError { kind, span }),
        }
    }

    fn try_get_variable(
        &mut self,
        span: Span,
        name: &String,
    ) -> Result<CheckedVariable, TypeError> {
        for i in (0..self.scopes.len()).rev() {
            let scope = self.scopes.get_mut(i).unwrap();
            if let Some(variable) = scope.try_get_variable(name) {
                return Ok(variable);
            }
        }
        Err(TypeError {
            kind: TypeErrorKind::NoSuchVariableDeclaredInScope { name: name.clone() },
            span,
        })
    }

    fn try_declare_function(
        &mut self,
        span: Span,
        function: CheckedFunction,
    ) -> Result<(), TypeError> {
        let local_scope = self.scopes.last_mut().unwrap();

        match local_scope.try_declare_function(function) {
            Ok(_) => Ok(()),
            Err(kind) => Err(TypeError { kind, span }),
        }
    }

    fn try_get_function_name_only(
        &mut self,
        span: Span,
        name: &String,
    ) -> Result<CheckedFunction, TypeError> {
        //Temp
        if name == "print" {
            return Ok(CheckedFunction {
                name: "print".to_string(),
                parameters: Vec::new(),
                return_type: TypeKind::Unit,
            });
        }
        if name == "fopen" {
            return Ok(CheckedFunction {
                name: "fopen".to_string(),
                parameters: Vec::new(),
                return_type: TypeKind::FileHandle,
            });
        }
        if name == "fwrite" {
            return Ok(CheckedFunction {
                name: "fwrite".to_string(),
                parameters: Vec::new(),
                return_type: TypeKind::Unit,
            });
        }
        if name == "fclose" {
            return Ok(CheckedFunction {
                name: "fclose".to_string(),
                parameters: Vec::new(),
                return_type: TypeKind::Unit,
            });
        }
        for i in (0..self.scopes.len()).rev() {
            let scope = self.scopes.get_mut(i).unwrap();
            if let Some(function) = scope.try_get_function(name) {
                return Ok(function);
            }
        }
        Err(TypeError {
            kind: TypeErrorKind::NoSuchFunctionNameDeclaredInScope { name: name.clone() },
            span,
        })
    }

    fn try_get_function(
        &mut self,
        span: Span,
        name: &String,
        types: Vec<TypeKind>,
    ) -> Result<CheckedFunction, TypeError> {
        //Temp
        if name == "print" {
            return Ok(CheckedFunction {
                name: "print".to_string(),
                parameters: Vec::new(),
                return_type: TypeKind::Unit,
            });
        }
        if name == "fopen" {
            return Ok(CheckedFunction {
                name: "fopen".to_string(),
                parameters: Vec::new(),
                return_type: TypeKind::FileHandle,
            });
        }
        if name == "fwrite" {
            return Ok(CheckedFunction {
                name: "fwrite".to_string(),
                parameters: vec![],
                return_type: TypeKind::Unit,
            });
        }
        if name == "fclose" {
            return Ok(CheckedFunction {
                name: "fclose".to_string(),
                parameters: Vec::new(),
                return_type: TypeKind::Unit,
            });
        }

        let types_count = types.len();

        'scopes: for i in (0..self.scopes.len()).rev() {
            let scope = self.scopes.get_mut(i).unwrap();
            if let Some(function) = scope.try_get_function(name) {
                let parameters = function.parameters.clone();
                if parameters.len() != types_count {
                    continue;
                }
                for (param, type_kind) in zip(parameters, types.clone()) {
                    if !Self::type_is_assignable_from(&param.variable.type_kind, &type_kind) {
                        continue 'scopes;
                    }
                }
                return Ok(function);
            }
        }
        Err(TypeError {
            kind: TypeErrorKind::NoSuchFunctionDeclaredInScope {
                name: name.clone(),
                types,
            },
            span,
        })
    }

    fn try_declare_type(&mut self, span: Span, type_kind: TypeKind) -> Result<(), TypeError> {
        let local_scope = self.scopes.last_mut().unwrap();

        match local_scope.try_declare_type(type_kind) {
            Ok(_) => Ok(()),
            Err(kind) => Err(TypeError { kind, span }),
        }
    }

    fn try_get_type(&mut self, span: Span, name: &String) -> Result<TypeKind, TypeError> {
        if name == "FILE_HANDLE" {
            return Ok(TypeKind::FileHandle);
        }

        for i in (0..self.scopes.len()).rev() {
            let scope = self.scopes.get_mut(i).unwrap();
            if let Some(type_kind) = scope.try_get_type(name) {
                return Ok(type_kind);
            }
        }
        Err(TypeError {
            kind: TypeErrorKind::NoSuchTypeDefinedInScope { name: name.clone() },
            span,
        })
    }

    fn explicit_cast(
        &self,
        expression: CheckedExpression,
        destination: TypeKind,
        span: Span,
    ) -> Result<CheckedExpression, TypeError> {
        let expression_type = expression.get_type();
        //TODO: reducing width should be an error unless explicit with `truncate`
        if self.is_numeric_type(&expression_type) && self.is_numeric_type(&destination) {
            return Ok(CheckedExpression::Cast {
                expression: Box::new(expression),
                destination,
            });
        }

        //TODO: maybe impl is_compatible(&self, other: TypeKind) -> bool in TypeKind for this?
        if expression_type == TypeKind::Char && destination == TypeKind::U8 {
            return Ok(CheckedExpression::Cast {
                expression: Box::new(expression),
                destination,
            });
        }

        Err(TypeError {
            kind: TypeErrorKind::CannotCastToType {
                from: expression_type,
                to: destination,
            },
            span,
        })
    }

    fn type_is_assignable_from(assignee: &TypeKind, assignment: &TypeKind) -> bool {
        match (assignee, assignment) {
            (
                TypeKind::Slice {
                    element_type: slice_el_type,
                },
                TypeKind::Array {
                    element_type: array_el_type,
                    ..
                },
            ) => Self::type_is_assignable_from(slice_el_type, array_el_type),
            _ => assignee == assignment,
        }
    }

    fn expect_type(
        &self,
        expected: &TypeKind,
        actual: &TypeKind,
        span: Span,
    ) -> Result<(), TypeError> {
        if !Self::type_is_assignable_from(expected, actual) {
            return Err(TypeError {
                kind: TypeErrorKind::TypeMismatch {
                    expected: expected.clone(),
                    actual: actual.clone(),
                },
                span,
            });
        }
        Ok(())
    }

    fn type_check_statement(
        &mut self,
        statement: Statement,
    ) -> Result<CheckedStatement, TypeError> {
        match statement {
            Statement::FunctionDeclaration {
                identifier,
                parameters,
                return_type_expression,
                body,
                ..
            } => {
                let name = Self::get_identifier_name(&identifier)?;
                let identifier_span = identifier.span;
                let is_main = name == "main";
                self.push_scope();
                let return_type = match &return_type_expression {
                    Some(type_expression) => {
                        if let Expression::TypeAnnotation { expression, .. } = type_expression {
                            self.check_type_expression(expression.clone())?
                        } else {
                            unreachable!()
                        }
                    }
                    None => TypeKind::Unit,
                };
                self.set_return_context(&return_type);
                if is_main && return_type != TypeKind::Unit {
                    //TODO: Eventually allow main to have more sophisticated returns for errors
                    return Err(TypeError {
                        kind: TypeErrorKind::MainMustHaveNoReturn,
                        span: return_type_expression.unwrap().get_span(),
                    });
                }

                let mut checked_parameters = Vec::new();
                for param in parameters {
                    let span = param.get_span();
                    if let Expression::FunctionParameter {
                        identifiers,
                        type_annotation,
                    } = param
                    {
                        for identifier in identifiers {
                            let type_kind =
                                if let Expression::TypeAnnotation { expression, .. } =
                                    *type_annotation.clone()
                                {
                                    self.check_type_expression(expression)?
                                } else {
                                    unreachable!()
                                };

                            let param_name = Self::get_identifier_name(&identifier)?;
                            let variable = CheckedVariable {
                                name: param_name,
                                type_kind,
                                mutable: false,
                            };

                            self.try_declare_variable(span.clone(), variable.clone())?;

                            checked_parameters.push(CheckedFunctionParameter { variable });
                        }
                    } else {
                        unreachable!()
                    }
                }

                if is_main && checked_parameters.len() > 1 {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncorrectMainParameters,
                        span: identifier_span,
                    });
                }

                let function = CheckedFunction {
                    name: name.clone(),
                    parameters: checked_parameters,
                    return_type: return_type.clone(), //cmon rust
                };

                //Declare within the body scope in case of recursion
                self.try_declare_function(identifier_span.clone(), function.clone())?;

                let checked_body = self.type_check_statement(*body)?;

                if return_type != TypeKind::Unit && !Self::check_all_paths_return(&checked_body) {
                    return Err(TypeError {
                        kind: TypeErrorKind::NotAllPathsReturnAValue,
                        span: identifier_span,
                    });
                }

                self.clear_return_context();

                self.pop_scope();

                self.try_declare_function(identifier_span, function.clone())?;

                Ok(CheckedStatement::FunctionDeclaration {
                    function,
                    body: Box::new(checked_body),
                })
            }

            Statement::ArrowFunctionDeclaration {
                identifier,
                parameters,
                body,
                ..
            } => {
                let name = Self::get_identifier_name(&identifier)?;
                self.push_scope();

                if let Statement::Expression {
                    expression: body_expression,
                    ..
                } = *body
                {
                    let mut checked_parameters = Vec::new();
                    for param in parameters {
                        if let Expression::FunctionParameter {
                            identifiers,
                            type_annotation,
                        } = param
                        {
                            for identifier in identifiers {
                                let span = Span::of(&identifier.span, &type_annotation.get_span());
                                let type_kind =
                                    if let Expression::TypeAnnotation { expression, .. } =
                                        *type_annotation.clone()
                                    {
                                        self.check_type_expression(expression)?
                                    } else {
                                        unreachable!()
                                    };

                                let param_name = Self::get_identifier_name(&identifier)?;
                                let variable = CheckedVariable {
                                    name: param_name,
                                    type_kind,
                                    mutable: false,
                                };

                                self.try_declare_variable(span, variable.clone())?;

                                checked_parameters.push(CheckedFunctionParameter { variable });
                            }
                        } else {
                            unreachable!()
                        }
                    }

                    //TODO: currently arrow functions cannot be recursive since we don't know their return type until we type check them,
                    //      but that sounds desirable, how to do that?
                    let checked_body = self.type_check_expression(body_expression)?;
                    let return_type = checked_body.get_type();

                    let function = CheckedFunction {
                        name: name.clone(),
                        parameters: checked_parameters,
                        return_type,
                    };

                    self.pop_scope();

                    self.try_declare_function(identifier.span, function.clone())?;
                    Ok(CheckedStatement::FunctionDeclaration {
                        function,
                        body: Box::new(CheckedStatement::Block {
                            statements: vec![CheckedStatement::Return {
                                expression: Some(checked_body),
                            }],
                        }),
                    })
                } else {
                    unreachable!()
                }
            }
            Statement::Expression { expression, .. } => {
                let checked_expression = self.type_check_expression(expression)?;

                Ok(CheckedStatement::Expression {
                    expression: checked_expression,
                })
            }
            Statement::Block { statements, .. } => {
                self.push_scope();

                let mut checked_statements = Vec::new();
                for statement in statements {
                    checked_statements.push(self.type_check_statement(statement)?);
                }

                if !self.deferred_stmts.is_empty() {
                    while let Some(element) = self.deferred_stmts.pop() {
                        checked_statements.push(element);
                    }
                }

                self.pop_scope();

                Ok(CheckedStatement::Block {
                    statements: checked_statements,
                })
            }
            Statement::While {
                condition, body, ..
            } => {
                let condition_span = condition.get_span().clone();
                let checked_condition = self.type_check_expression(condition)?;

                self.expect_type(
                    &TypeKind::Bool,
                    &checked_condition.get_type(),
                    condition_span,
                )?;

                let checked_body = self.type_check_statement(*body)?;

                Ok(CheckedStatement::While {
                    condition: checked_condition,
                    body: Box::new(checked_body),
                })
            }
            Statement::If {
                condition,
                body,
                else_clause,
                ..
            } => {
                let condition_span = condition.get_span().clone();
                let checked_condition = self.type_check_expression(condition)?;

                self.expect_type(
                    &TypeKind::Bool,
                    &checked_condition.get_type(),
                    condition_span,
                )?;

                let checked_body = self.type_check_statement(*body)?;

                let checked_else = match else_clause {
                    Some(else_clause) => Some(Box::new(self.type_check_statement(*else_clause)?)),
                    None => None,
                };

                Ok(CheckedStatement::If {
                    condition: checked_condition,
                    body: Box::new(checked_body),
                    else_clause: checked_else,
                })
            }
            Statement::Else { body, .. } => {
                let checked_body = self.type_check_statement(*body)?;

                Ok(CheckedStatement::Else {
                    body: Box::new(checked_body),
                })
            }
            Statement::For {
                iterator,
                iterable,
                body,
                ..
            } => {
                self.push_scope();
                let name = Self::get_identifier_name(&iterator)?;
                let iterable_span = iterable.get_span();
                let checked_iterable = self.type_check_expression(iterable)?;

                if !self.is_iterable_type(&checked_iterable.get_type()) {
                    return Err(TypeError {
                        kind: TypeErrorKind::TypeIsNotIterable {
                            type_kind: checked_iterable.get_type(),
                        },
                        span: iterable_span,
                    });
                }

                let iterator_type = match checked_iterable.get_type() {
                    TypeKind::Range { type_kind } => type_kind,
                    TypeKind::Array { element_type, .. } | TypeKind::Slice { element_type } => {
                        element_type
                    }
                    TypeKind::String => Box::new(TypeKind::Char),
                    _ => {
                        unreachable!(
                            "Iterating type `{}` is not yet implemented",
                            checked_iterable.get_type()
                        )
                    }
                };

                let iterator_variable = CheckedVariable {
                    name,
                    type_kind: *iterator_type,
                    mutable: false,
                };

                self.try_declare_variable(iterator.span, iterator_variable.clone())?;

                let checked_body = self.type_check_statement(*body)?;
                self.pop_scope();

                Ok(CheckedStatement::For {
                    iterator: iterator_variable,
                    iterable: checked_iterable,
                    body: Box::new(checked_body),
                })
            }
            Statement::TypeDefinition {
                identifier,
                type_definition,
                ..
            } => {
                let name = Self::get_identifier_name(&identifier)?;
                let span = identifier.span;
                let checked_type_definition = self.check_type_definition(type_definition)?;

                let type_kind = match checked_type_definition {
                    CheckedTypeDefinition::Struct { members } => TypeKind::Struct { name, members },
                    CheckedTypeDefinition::Enum { members } => TypeKind::Enum { name, members },
                    CheckedTypeDefinition::Union { variants } => TypeKind::Union { name, variants },
                    CheckedTypeDefinition::UnionVariant { tag: _, members: _ } => todo!(),
                };

                if let TypeKind::Enum { name, .. } = &type_kind {
                    //Declare a variable in the scope of the type so we can do access on it, e.g. Color.Red
                    self.try_declare_variable(
                        span.clone(),
                        CheckedVariable {
                            name: name.to_string(),
                            type_kind: TypeKind::Type {
                                type_kind: Box::new(type_kind.clone()),
                            },
                            mutable: false,
                        },
                    )?;
                } else if let TypeKind::Union { name, .. } = &type_kind {
                    //Declare a variable in the scope of the type so we can do access on it, e.g. Color.Red
                    self.try_declare_variable(
                        span.clone(),
                        CheckedVariable {
                            name: name.to_string(),
                            type_kind: TypeKind::Type {
                                type_kind: Box::new(type_kind.clone()),
                            },
                            mutable: false,
                        },
                    )?;
                }

                self.try_declare_type(span, type_kind.clone())?;

                Ok(CheckedStatement::TypeDefinition { type_kind })
            }
            Statement::Match {
                match_keyword,
                expression,
                arms,
                ..
            } => {
                let checked_expression = self.type_check_expression(expression)?;

                self.set_assign_context(&Some(checked_expression.get_type()));

                let mut is_exhaustive = false;
                let mut matched_members = Vec::new();

                match checked_expression.get_type() {
                    TypeKind::Enum { name: _, members } => {
                        let mut checked_arms = Vec::new();
                        for arm in arms {
                            let arm_span = arm.get_span();
                            if let Expression::MatchArm { pattern, body, .. } = arm {
                                let checked_pattern = self
                                    .type_check_pattern(*pattern, checked_expression.get_type())?;

                                match &checked_pattern {
                                    CheckedPattern::VariablePath { path: _, value } => {
                                        if !matched_members.contains(&value.name) {
                                            matched_members.push(value.name.clone());
                                        } else {
                                            return Err(TypeError {
                                                kind: TypeErrorKind::MatchArmAlreadyChecked {
                                                    pattern: checked_pattern,
                                                },
                                                span: arm_span,
                                            });
                                        }

                                        if matched_members.len() == members.len() {
                                            is_exhaustive = true;
                                        }
                                    }
                                    CheckedPattern::Wildcard(_) => {
                                        if is_exhaustive {
                                            return Err(TypeError {
                                                kind: TypeErrorKind::MatchAlreadyExhaustive,
                                                span: arm_span,
                                            });
                                        }
                                        is_exhaustive = true;
                                    }
                                    CheckedPattern::UnionVariantPath {
                                        path: _,
                                        tag: _,
                                        struct_type: _,
                                        arguments: _,
                                    } => unreachable!(), //TODO: technically reachable but not allowed
                                }

                                let checked_body = self.type_check_statement(*body)?;

                                checked_arms.push(CheckedExpression::MatchArm {
                                    pattern: Box::new(checked_pattern),
                                    body: Box::new(checked_body),
                                });
                            } else {
                                unreachable!()
                            }
                        }

                        self.set_assign_context(&None);

                        if !is_exhaustive {
                            return Err(TypeError {
                                kind: TypeErrorKind::MatchIsNotExhaustive,
                                span: match_keyword.span,
                            });
                        }

                        Ok(CheckedStatement::Match {
                            expression: Box::new(checked_expression),
                            arms: checked_arms,
                        })
                    }
                    TypeKind::Union { name: _, variants } => {
                        let mut is_exhaustive = false;
                        let mut matched_variants = Vec::new();

                        let mut checked_arms = Vec::new();
                        for arm in arms {
                            self.push_scope();

                            let arm_span = arm.get_span();
                            if let Expression::MatchArm { pattern, body, .. } = arm {
                                let checked_pattern = self
                                    .type_check_pattern(*pattern, checked_expression.get_type())?;

                                match checked_pattern.clone() {
                                    CheckedPattern::VariablePath { path: _, value } => {
                                        if !matched_members.contains(&value.name) {
                                            matched_members.push(value.name.clone());
                                        } else {
                                            return Err(TypeError {
                                                kind: TypeErrorKind::MatchArmAlreadyChecked {
                                                    pattern: checked_pattern,
                                                },
                                                span: arm_span,
                                            });
                                        }

                                        if matched_members.len() == variants.len() {
                                            is_exhaustive = true;
                                        }

                                        for variant in &variants {
                                            if value.name == variant.name {
                                                matched_variants.push(variant.tag);
                                                break;
                                            }
                                        }
                                    }
                                    CheckedPattern::Wildcard(_) => {
                                        if is_exhaustive {
                                            return Err(TypeError {
                                                kind: TypeErrorKind::MatchAlreadyExhaustive,
                                                span: arm_span,
                                            });
                                        }
                                        is_exhaustive = true;
                                    }
                                    CheckedPattern::UnionVariantPath { tag, arguments, .. } => {
                                        for argument in arguments {
                                            self.try_declare_variable(
                                                arm_span.clone(),
                                                argument.variable.clone(),
                                            )?;
                                        }

                                        matched_variants.push(tag);

                                        if matched_variants.len() == variants.len() {
                                            is_exhaustive = true;
                                        }
                                    }
                                }

                                let checked_body = self.type_check_statement(*body)?;

                                checked_arms.push(CheckedExpression::MatchArm {
                                    pattern: Box::new(checked_pattern),
                                    body: Box::new(checked_body),
                                });
                            } else {
                                unreachable!()
                            }

                            self.pop_scope();
                        }

                        if !is_exhaustive {
                            return Err(TypeError {
                                kind: TypeErrorKind::MatchIsNotExhaustive,
                                span: match_keyword.span,
                            });
                        }

                        Ok(CheckedStatement::Match {
                            expression: Box::new(checked_expression),
                            arms: checked_arms,
                        })
                    }
                    _ => todo!("{:?}", checked_expression.get_type()),
                }
            }
            Statement::Uses { module_path, .. } => {
                //Find module
                let path = format!(
                    "{}.tyr",
                    module_path
                        .iter()
                        .map(|id| Self::get_identifier_name(id).expect("not an identifier"))
                        .collect::<Vec<String>>()
                        .join("/")
                );
                let source = fs::read_to_string(&path).unwrap_or_else(|err| {
                    eprintln!("Error reading file {}: {}", path, err);
                    exit(1);
                });

                let tokens = match lex(&source) {
                    Ok(tokens) => tokens,
                    Err(e) => {
                        display_lex_error(e, &source);
                        exit(0);
                    }
                };

                let mut parser = Parser::new(tokens);

                let statements = match parser.parse() {
                    Ok(statements) => statements,
                    Err(e) => {
                        display_parse_error(e, &source);
                        exit(0);
                    }
                };

                //TODO: This is very bad, I'm checking once in its own TC to get accurate errors then again in this TC to declare things in this scope
                let mut type_checker = TypeChecker::new(self.path.clone());
                let module = match type_checker.type_check(statements.clone()) {
                    Ok(module) => module,
                    Err(e) => {
                        display_type_error(e, &source);
                        exit(0);
                    }
                };
                self.type_check(statements)?;
                //I think this TC needs to keep a map of all the modules imported directly, which will keep all their imported modules
                //then when a new uses statement is encountered, first check this map for an existing definition to use to prevent multiple imports of the same module

                Ok(CheckedStatement::Uses { module })
            }
            Statement::Defer { statement, .. } => {
                let checked_statement = self.type_check_statement(*statement)?;
                self.deferred_stmts.push(checked_statement);
                Ok(CheckedStatement::Noop)
            }
            Statement::Return { expression, .. } => match expression {
                None => Ok(CheckedStatement::Return { expression: None }),
                Some(expression) => {
                    let expression_span = expression.get_span();

                    self.set_assign_context(&Some(self.get_return_context()));

                    let checked_expression = self.type_check_expression(*expression)?;

                    self.expect_type(
                        &self.get_return_context(),
                        &checked_expression.get_type(),
                        expression_span,
                    )?;
                    self.set_assign_context(&None);

                    if self.deferred_stmts.is_empty() {
                        return Ok(CheckedStatement::Return {
                            expression: Some(checked_expression),
                        });
                    }

                    let mut checked_statements = Vec::new();
                    while let Some(element) = self.deferred_stmts.pop() {
                        checked_statements.push(element);
                    }
                    checked_statements.push(CheckedStatement::Return {
                        expression: Some(checked_expression),
                    });

                    Ok(CheckedStatement::Block {
                        statements: checked_statements,
                    })
                }
            },
            Statement::Raw { lines, .. } => {
                let mut raw_c_strings = Vec::new();
                for line in lines {
                    if let TokenKind::StringLiteral(raw_c_string) = line.kind {
                        raw_c_strings.push(raw_c_string);
                    }
                }
                Ok(CheckedStatement::Raw {
                    lines: raw_c_strings,
                })
            }
        }
    }

    fn type_check_pattern(
        &mut self,
        pattern: Pattern,
        expression_to_match_type: TypeKind,
    ) -> Result<CheckedPattern, TypeError> {
        let pattern_span = pattern.get_span();
        match pattern {
            Pattern::Wildcard { .. } => Ok(CheckedPattern::Wildcard(expression_to_match_type)),
            Pattern::Member { left, member, .. } => {
                let member_name = Self::get_identifier_name(&member)?;
                match &expression_to_match_type {
                    TypeKind::Enum { .. } | TypeKind::Union { .. } => match left {
                        Some(left) => {
                            let checked_path = self.type_check_expression(left)?;
                            Ok(CheckedPattern::VariablePath {
                                path: vec![checked_path],
                                value: CheckedVariable {
                                    name: member_name,
                                    type_kind: expression_to_match_type,
                                    mutable: false,
                                },
                            })
                        }
                        None => Ok(CheckedPattern::VariablePath {
                            path: vec![],
                            value: CheckedVariable {
                                name: member_name,
                                type_kind: expression_to_match_type,
                                mutable: false,
                            },
                        }),
                    },
                    _ => todo!(),
                }
            }
            Pattern::Struct {
                identifier, params, ..
            } => {
                let variant_name = Self::get_identifier_name(&identifier)?;
                if let TypeKind::Union {
                    name: union_name,
                    variants,
                } = &expression_to_match_type
                {
                    //find variant
                    for variant in variants {
                        if variant.name == variant_name {
                            if let CheckedUnionVariantKind::Struct { members } = &variant.kind {
                                // match members to params and declare in scope

                                let mut expected_parameters = HashMap::new();
                                for member in members {
                                    let member = &member.variable;
                                    expected_parameters.insert(member.name.clone(), member);
                                }
                                let mut checked_arguments = Vec::new();
                                for param in params {
                                    let param_name = Self::get_identifier_name(&param)?;

                                    match expected_parameters.get(&param_name) {
                                        Some(expected_param) => {
                                            checked_arguments.push(CheckedFunctionParameter {
                                                variable: CheckedVariable {
                                                    name: param_name.clone(),
                                                    type_kind: expected_param.type_kind.clone(),
                                                    mutable: false,
                                                },
                                            });

                                            //If we get here then we're all good
                                            expected_parameters.remove(&param_name);
                                        }
                                        None => {
                                            return Err(TypeError {
                                                kind: TypeErrorKind::StructHasNoMember {
                                                    struct_name: variant_name,
                                                    name: param_name,
                                                },
                                                span: param.span,
                                            })
                                        }
                                    }
                                }
                                if expected_parameters.is_empty() {
                                    return Ok(CheckedPattern::UnionVariantPath {
                                        path: vec![],
                                        struct_type: TypeKind::Struct {
                                            name: variant_name,
                                            members: members.to_vec(),
                                        },
                                        tag: variant.tag,
                                        arguments: checked_arguments,
                                    });
                                }
                                return Err(TypeError {
                                    kind: TypeErrorKind::StructMembersMissing {
                                        struct_name: variant_name,
                                        expected_parameters: expected_parameters
                                            .values()
                                            .cloned()
                                            .cloned()
                                            .collect(),
                                    },
                                    span: pattern_span,
                                });
                            } else {
                                return Err(TypeError {
                                    kind: TypeErrorKind::VariantIsNotAStruct,
                                    span: pattern_span,
                                });
                            }
                        }
                    }
                    Err(TypeError {
                        kind: TypeErrorKind::EnumHasNoMember {
                            enum_name: union_name.clone(),
                            member_name: variant_name,
                        },
                        span: pattern_span,
                    })
                } else {
                    unreachable!()
                }
            }
        }
    }

    fn check_type_definition(
        &mut self,
        type_definition: TypeDefinition,
    ) -> Result<CheckedTypeDefinition, TypeError> {
        match type_definition {
            TypeDefinition::Struct { members, .. } => {
                self.push_scope();

                let mut checked_members = Vec::new();

                for member in members {
                    let span = member.get_span();
                    if let Expression::FunctionParameter {
                        identifiers,
                        type_annotation,
                    } = member
                    {
                        let type_kind = if let Expression::TypeAnnotation { expression, .. } =
                            *type_annotation
                        {
                            self.check_type_expression(expression)?
                        } else {
                            unreachable!()
                        };
                        for identifier in identifiers {
                            let name = Self::get_identifier_name(&identifier)?;
                            let variable = CheckedVariable {
                                name,
                                type_kind: type_kind.clone(),
                                mutable: false,
                            };

                            //TODO: This will cause an error that says "variable `{}` is already declared in scope"
                            //      But it should be something like "member `{}` already defined in struct"
                            //      Match on the result and return a new error value
                            self.try_declare_variable(span.clone(), variable.clone())?;

                            checked_members.push(CheckedFunctionParameter { variable });
                        }
                    } else {
                        unreachable!()
                    }
                }

                self.pop_scope();

                Ok(CheckedTypeDefinition::Struct {
                    members: checked_members,
                })
            }
            TypeDefinition::Enum { members, .. } => {
                let mut member_names = Vec::new();

                for member in members {
                    let name = Self::get_identifier_name(&member)?;
                    if member_names.contains(&name) {
                        return Err(TypeError {
                            //TODO: This is a great candidate for extending errors with Hint locations and messages
                            kind: TypeErrorKind::MemberAlreadyDeclaredInType { name },
                            span: member.span,
                        });
                    }
                    member_names.push(name);
                }
                Ok(CheckedTypeDefinition::Enum {
                    members: member_names,
                })
            }
            TypeDefinition::Union { variants, .. } => {
                let mut checked_variants = Vec::new();
                //TODO: Check for duplicate variants
                for (i, variant) in variants.iter().enumerate() {
                    if let Expression::UnionVariant { identifier, kind } = variant.clone() {
                        let variant_name = Self::get_identifier_name(&identifier)?;
                        match kind {
                            UnionVariantKind::Enum => {
                                checked_variants.push(CheckedUnionVariant {
                                    name: variant_name,
                                    tag: i,
                                    kind: CheckedUnionVariantKind::Enum,
                                });
                            }
                            UnionVariantKind::Struct { parameters } => {
                                let mut checked_parameters = Vec::new();
                                self.push_scope();
                                for parameter in parameters {
                                    if let Expression::FunctionParameter {
                                        identifiers,
                                        type_annotation,
                                    } = parameter
                                    {
                                        let span =
                                            Span::of(&identifier.span, &type_annotation.get_span());
                                        let type_kind = if let Expression::TypeAnnotation {
                                            expression,
                                            ..
                                        } = *type_annotation
                                        {
                                            self.check_type_expression(expression)?
                                        } else {
                                            unreachable!()
                                        };
                                        for identifier in identifiers {
                                            let param_name =
                                                Self::get_identifier_name(&identifier)?;

                                            let variable = CheckedVariable {
                                                name: param_name,
                                                type_kind: type_kind.clone(),
                                                mutable: false,
                                            };

                                            self.try_declare_variable(
                                                span.clone(),
                                                variable.clone(),
                                            )?;

                                            checked_parameters
                                                .push(CheckedFunctionParameter { variable });
                                        }
                                    } else {
                                        unreachable!()
                                    }
                                }
                                self.pop_scope();
                                checked_variants.push(CheckedUnionVariant {
                                    name: variant_name,
                                    tag: i,
                                    kind: CheckedUnionVariantKind::Struct {
                                        members: checked_parameters,
                                    },
                                });
                            }
                        }
                    } else {
                        unreachable!()
                    }
                }
                Ok(CheckedTypeDefinition::Union {
                    variants: checked_variants,
                })
            }
        }
    }

    fn check_type_expression(
        &mut self,
        type_expression: TypeExpression,
    ) -> Result<TypeKind, TypeError> {
        match &type_expression {
            TypeExpression::Basic(identifier) => match &identifier.kind {
                TokenKind::Identifier(name) | TokenKind::TypeLiteral(name) => {
                    Ok(self.try_get_type(type_expression.get_span(), name)?)
                }
                _ => unreachable!("{:?}", identifier.kind),
            },
            TypeExpression::Array {
                size, element_type, ..
            } => {
                if let TokenKind::IntLiteral(size) = size.kind {
                    let element_type = self.check_type_expression((**element_type).clone())?;

                    Ok(TypeKind::Array {
                        size,
                        element_type: Box::new(element_type),
                    })
                } else {
                    unreachable!()
                }
            }
            TypeExpression::Slice { element_type, .. } => {
                let element_type = self.check_type_expression((**element_type).clone())?;

                Ok(TypeKind::Slice {
                    element_type: Box::new(element_type),
                })
            }
            TypeExpression::Pointer { reference_type, .. } => {
                let reference_type = self.check_type_expression((**reference_type).clone())?;
                Ok(TypeKind::Pointer {
                    reference_type: Box::new(reference_type),
                })
            }
            TypeExpression::Function { inputs, output, .. } => {
                let mut checked_inputs = Vec::new();
                for input in inputs {
                    checked_inputs.push(self.check_type_expression(input.clone())?);
                }
                let checked_output = self.check_type_expression(*output.clone())?;

                Ok(TypeKind::Function {
                    inputs: checked_inputs,
                    output: Box::new(checked_output),
                })
            }
        }
    }

    fn type_check_expression(
        &mut self,
        expression: Expression,
    ) -> Result<CheckedExpression, TypeError> {
        let expression_span = expression.get_span().clone();
        match expression {
            Expression::IntLiteral { value, .. } => {
                let type_kind = match self.get_assign_context() {
                    Some(assign_context) => match assign_context {
                        TypeKind::I8 => Ok(assign_context),
                        TypeKind::I16 => Ok(assign_context),
                        TypeKind::I32 => Ok(assign_context),
                        TypeKind::I64 => Ok(assign_context),
                        TypeKind::U8 => Ok(assign_context),
                        TypeKind::U16 => Ok(assign_context),
                        TypeKind::U32 => Ok(assign_context),
                        TypeKind::U64 => Ok(assign_context),
                        TypeKind::F32 => Ok(assign_context),
                        TypeKind::F64 => Ok(assign_context),
                        TypeKind::F128 => Ok(assign_context),
                        _ => Ok(TypeKind::I32),
                    },
                    None => Ok(TypeKind::I32),
                }?;
                Ok(CheckedExpression::IntLiteral { value, type_kind })
            }
            Expression::FloatLiteral { value, .. } => {
                let type_kind = match self.get_assign_context() {
                    Some(assign_context) => match assign_context {
                        TypeKind::F32 => Ok(assign_context),
                        TypeKind::F64 => Ok(assign_context),
                        TypeKind::F128 => Ok(assign_context),
                        _ => Ok(TypeKind::F64),
                    },
                    None => Ok(TypeKind::F64),
                }?;
                Ok(CheckedExpression::FloatLiteral { value, type_kind })
            }
            Expression::BoolLiteral { value, .. } => Ok(CheckedExpression::BoolLiteral { value }),
            Expression::CharLiteral { value, .. } => Ok(CheckedExpression::CharLiteral { value }),
            Expression::StringLiteral { value, .. } => {
                Ok(CheckedExpression::StringLiteral { value })
            }
            Expression::Variable { name, span } => {
                let variable = self.try_get_variable(span, &name)?;

                Ok(CheckedExpression::Variable(variable))
            }
            Expression::FunctionCall {
                identifier,
                arguments,
                close_paren,
                ..
            } => {
                let name = Self::get_identifier_name(&identifier)?;
                let mut checked_arguments = Vec::new();

                let function_by_name = self.try_get_function_name_only(
                    Span::of(&identifier.span, &close_paren.span),
                    &name,
                )?;

                if name == "print" || name == "fopen" || name == "fwrite" || name == "fclose" {
                    //TODO: these functions being special is causing problems, put them in a proper module
                    for arg in arguments {
                        checked_arguments.push(self.type_check_expression(arg)?);
                    }
                } else {
                    if function_by_name.parameters.len() != arguments.len() {
                        return Err(TypeError {
                            kind: TypeErrorKind::NoSuchFunctionNameDeclaredInScope { name },
                            span: Span::of(&identifier.span, &close_paren.span),
                        });
                    }

                    for (arg, param) in zip(arguments, function_by_name.parameters) {
                        self.set_assign_context(&Some(param.variable.type_kind));
                        checked_arguments.push(self.type_check_expression(arg)?);
                        self.set_assign_context(&None);
                    }
                }

                let types = checked_arguments
                    .iter()
                    .map(|a| a.get_type())
                    .collect::<Vec<TypeKind>>();

                let function = self.try_get_function(
                    Span::of(&identifier.span, &close_paren.span),
                    &name,
                    types,
                )?;

                Ok(CheckedExpression::FunctionCall {
                    function,
                    arguments: checked_arguments,
                })
            }
            Expression::MethodCall {
                callee,
                identifier,
                arguments,
                close_paren,
                ..
            } => {
                //some_expr.func(arg1, arg2, ..., argN) => func(^some_expr, arg1, arg2, ..., argN)
                let name = Self::get_identifier_name(&identifier)?;
                let checked_callee = self.type_check_expression(*callee)?;

                let mut checked_arguments = Vec::new();
                for arg in arguments {
                    checked_arguments.push(self.type_check_expression(arg)?);
                }

                //that's a lot of code just to take a pointer
                // let callee_pointer = if let TypeKind::Pointer { .. } = checked_callee.get_type() {
                //     checked_callee
                // } else {
                //     CheckedExpression::Unary {
                //         op: CheckedUnaryOp {
                //             op: UnaryOperator::Ref,
                //             return_type: TypeKind::Pointer {
                //                 reference_type: Box::new(checked_callee.get_type()),
                //             },
                //         },
                //         operand: Box::new(checked_callee),
                //     }
                // };

                checked_arguments.insert(0, checked_callee);

                let types = checked_arguments
                    .iter()
                    .map(|a| a.get_type())
                    .collect::<Vec<TypeKind>>();

                let function = self.try_get_function(
                    Span::of(&identifier.span, &close_paren.span),
                    &name,
                    types,
                )?;

                Ok(CheckedExpression::FunctionCall {
                    function,
                    arguments: checked_arguments,
                })
            }
            Expression::Binary { left, op, right } => {
                let span = Span::of(&left.get_span(), &right.get_span());
                let checked_left = self.type_check_expression(*left)?;
                self.set_assign_context(&Some(checked_left.get_type()));
                let checked_right = self.type_check_expression(*right)?;
                self.set_assign_context(&None);

                let checked_op = self.type_check_binary_op(
                    checked_left.get_type(),
                    checked_right.get_type(),
                    op,
                    span,
                )?;

                Ok(CheckedExpression::Binary {
                    left: Box::new(checked_left),
                    op: checked_op,
                    right: Box::new(checked_right),
                })
            }
            Expression::Unary { op, operand } => {
                let span = Span::of(&op.token.span, &operand.get_span());
                let checked_operand = self.type_check_expression(*operand)?;

                let checked_op =
                    self.type_check_unary_op(op.op, checked_operand.get_type(), span)?;

                Ok(CheckedExpression::Unary {
                    op: checked_op,
                    operand: Box::new(checked_operand),
                })
            }
            Expression::Parenthesised { expression, .. } => {
                let checked_expression = self.type_check_expression(*expression)?;

                Ok(CheckedExpression::Parenthesised {
                    expression: Box::new(checked_expression),
                })
            }
            Expression::Assignment {
                left, assignment, ..
            } => {
                let right_span = assignment.get_span();
                let left_span = left.get_span();
                let checked_left = self.type_check_expression(*left)?;

                self.set_assign_context(&Some(checked_left.get_type()));
                let checked_assignment = self.type_check_expression(*assignment)?;
                self.set_assign_context(&None);

                match checked_left {
                    CheckedExpression::Variable(variable) => {
                        if !variable.mutable {
                            return Err(TypeError {
                                kind: TypeErrorKind::CannotMutableConstValue { variable },
                                span: left_span,
                            });
                        }

                        self.expect_type(
                            &variable.type_kind,
                            &checked_assignment.get_type(),
                            right_span,
                        )?;

                        Ok(CheckedExpression::Assignment {
                            left: Box::new(CheckedExpression::Variable(variable)),
                            assignment: Box::new(checked_assignment),
                        })
                    }
                    _ => {
                        self.expect_type(
                            &checked_left.get_type(),
                            &checked_assignment.get_type(),
                            right_span,
                        )?;

                        Ok(CheckedExpression::Assignment {
                            left: Box::new(checked_left),
                            assignment: Box::new(checked_assignment),
                        })
                    }
                }
            }
            //These won't be checked individually
            Expression::Type { .. }
            | Expression::FunctionParameter { .. }
            | Expression::TypeAnnotation { .. } => unreachable!(),
            Expression::NamedArgument {
                name: _,
                colon: _,
                argument: _,
            } => todo!(),
            Expression::StructLiteral {
                identifier,
                arguments,
                ..
            } => {
                let name = Self::get_identifier_name(&identifier)?;
                let identifier_span = identifier.span;
                //first check if the type even exists
                let struct_type = self.try_get_type(identifier_span, &name)?;
                //is it a struct type?
                match struct_type.clone() {
                    TypeKind::Struct { name, members } => {
                        let struct_name = name;
                        self.check_struct_literal(
                            members,
                            arguments,
                            struct_name,
                            struct_type,
                            &expression_span,
                        )
                    }
                    _ => todo!("{:?}", struct_type),
                }
            }
            Expression::MemberAccess { left, member, .. } => match left {
                Some(left) => {
                    let checked_accessee = self.type_check_expression(*left)?;

                    let member_name = Self::get_identifier_name(&member)?;
                    Self::check_member_access(
                        checked_accessee,
                        member_name,
                        &member,
                        expression_span,
                    )
                }
                None => match self.get_assign_context() {
                    Some(assign_context) => {
                        let member_name = Self::get_identifier_name(&member)?;
                        match assign_context.clone() {
                            TypeKind::Struct { name: _, members } => {
                                match members.iter().find(|p| p.variable.name == member_name) {
                                    Some(param) => Ok(CheckedExpression::MemberAccess {
                                        accessee: None,
                                        member: param.variable.clone(),
                                    }),
                                    None => Err(TypeError {
                                        kind: TypeErrorKind::NoSuchMemberInStruct {
                                            type_kind: assign_context,
                                            name: member_name,
                                        },
                                        span: member.span,
                                    }),
                                }
                            }
                            TypeKind::Enum {
                                name: enum_name,
                                members,
                            } => {
                                if members.contains(&member_name) {
                                    Ok(CheckedExpression::MemberAccess {
                                        accessee: None,
                                        member: CheckedVariable {
                                            name: member_name,
                                            type_kind: assign_context,
                                            mutable: false,
                                        },
                                    })
                                } else {
                                    Err(TypeError {
                                        kind: TypeErrorKind::EnumHasNoMember {
                                            enum_name: enum_name.to_string(),
                                            member_name,
                                        },
                                        span: member.span,
                                    })
                                }
                            }
                            TypeKind::Union { name: _, variants } => {
                                for variant in variants {
                                    if variant.name == member_name {
                                        match variant.kind {
                                            CheckedUnionVariantKind::Enum => {
                                                return Ok(CheckedExpression::MemberAccess {
                                                    accessee: None,
                                                    member: CheckedVariable {
                                                        name: member_name,
                                                        type_kind: assign_context,
                                                        mutable: false,
                                                    },
                                                });
                                            }
                                            CheckedUnionVariantKind::Struct { members: _ } => {
                                                todo!()
                                            }
                                        }
                                    }
                                }

                                todo!()
                            }
                            _ => Err(TypeError {
                                kind: TypeErrorKind::TypeHasNoMembers {
                                    type_kind: assign_context,
                                },
                                span: expression_span,
                            }),
                        }
                    }
                    None => Err(TypeError {
                        kind: TypeErrorKind::CannotInferType,
                        span: expression_span,
                    }),
                },
            },
            Expression::ArrayLiteral {
                type_expression,
                open_curly,
                elements,
                close_curly,
            } => {
                let type_expression_span = type_expression.get_span().clone();
                let checked_type = self.check_type_expression(type_expression)?;

                if let TypeKind::Array { size, element_type } = &checked_type {
                    if size != &elements.len() {
                        return Err(TypeError {
                            kind: TypeErrorKind::ArrayElementsCountMismatch {
                                expected: *size,
                                actual: elements.len(),
                            },
                            span: Span::of(&open_curly.span, &close_curly.span),
                        });
                    }
                    let mut checked_els = Vec::new();
                    for el in elements {
                        let el_span = el.get_span();
                        let checked_el = self.type_check_expression(el)?;

                        self.expect_type(element_type, &checked_el.get_type(), el_span)?;
                        checked_els.push(checked_el);
                    }

                    Ok(CheckedExpression::ArrayLiteral {
                        type_kind: checked_type,
                        elements: checked_els,
                    })
                } else {
                    Err(TypeError {
                        kind: TypeErrorKind::NotAnArrayType,
                        span: type_expression_span,
                    })
                }
            }
            Expression::ArrayIndex { array, index, .. } => {
                let checked_array = self.type_check_expression(*array)?;

                let index_span = index.get_span();
                let checked_index = self.type_check_expression(*index)?;

                match checked_index.get_type() {
                    TypeKind::I32 => match checked_array.get_type() {
                        TypeKind::Array { .. } | TypeKind::Slice { .. } | TypeKind::String => {
                            Ok(CheckedExpression::ArrayIndex {
                                array: Box::new(checked_array),
                                index: Box::new(checked_index),
                            })
                        }
                        _ => Err(TypeError {
                            kind: TypeErrorKind::CannotIndexType {
                                type_kind: checked_array.get_type(),
                            },
                            span: expression_span,
                        }),
                    },
                    TypeKind::Range {
                        type_kind: range_type,
                    } => {
                        self.expect_type(&TypeKind::I32, &range_type, index_span)?;

                        match checked_array.get_type() {
                            TypeKind::Array { .. } | TypeKind::Slice { .. } | TypeKind::String => {
                                Ok(CheckedExpression::Slice {
                                    array: Box::new(checked_array),
                                    range: Box::new(checked_index),
                                })
                            }
                            _ => Err(TypeError {
                                kind: TypeErrorKind::CannotSliceType {
                                    type_kind: checked_array.get_type(),
                                },
                                span: expression_span,
                            }),
                        }
                    }
                    _ => Err(TypeError {
                        kind: TypeErrorKind::TypeMismatch {
                            expected: TypeKind::I32,
                            actual: checked_index.get_type(),
                        },
                        span: index_span,
                    }),
                }
            }
            //Not to be type checked automatically
            Expression::MatchArm { .. } => todo!(),
            Expression::UnionVariant { .. } => todo!(),
            Expression::VariableDeclaration {
                identifiers,
                colon: _,
                type_expression,
                assignment_token,
                initialiser,
            } => {
                let initialiser_span = initialiser.get_span().clone();
                let mutable = match assignment_token.kind {
                    TokenKind::Equals => true,
                    TokenKind::Colon => false,
                    _ => unreachable!(),
                };

                let annotation_type = match type_expression {
                    Some(type_expression) => Some(self.check_type_expression(*type_expression)?),
                    None => None,
                };

                self.set_assign_context(&annotation_type);

                //TODO: initialisers shouldn't be mandatory, or should they?
                let checked_initialiser = self.type_check_expression(*initialiser)?;

                self.set_assign_context(&None);

                let checked_type = match annotation_type.clone() {
                    Some(type_kind) => type_kind.clone(),
                    None => checked_initialiser.get_type(),
                };

                self.expect_type(
                    &checked_type,
                    &checked_initialiser.get_type(),
                    initialiser_span,
                )?;

                let mut variables = Vec::new();

                for identifier in identifiers {
                    let name = Self::get_identifier_name(&identifier)?;
                    let variable = CheckedVariable {
                        name: name.clone(),
                        type_kind: checked_type.clone(),
                        mutable,
                    };
                    self.try_declare_variable(identifier.span, variable.clone())?;

                    variables.push(variable);
                }
                Ok(CheckedExpression::VariableDeclaration {
                    variables,
                    initialiser: Box::new(checked_initialiser),
                })
            }
            Expression::Dereference { expression, .. } => {
                let checked_expression = self.type_check_expression(*expression)?;

                if let TypeKind::Pointer { .. } = checked_expression.get_type() {
                    Ok(CheckedExpression::Dereference {
                        expression: Box::new(checked_expression),
                    })
                } else {
                    Err(TypeError {
                        kind: TypeErrorKind::CannotDereferenceType {
                            type_kind: checked_expression.get_type(),
                        },
                        span: expression_span,
                    })
                }
            }
            Expression::UnionLiteral { left, dot: _, kind } => {
                let union_type = match left {
                    Some(left) => {
                        let checked_left = self.type_check_expression(*left)?;
                        if let TypeKind::Type { type_kind } = checked_left.get_type() {
                            if let TypeKind::Union {
                                name: _union_name,
                                variants: _,
                            } = *type_kind.clone()
                            {
                                *type_kind.clone()
                            } else {
                                unreachable!()
                            };
                        }
                        todo!() //Err not an enum etc etc
                    }
                    None => self.get_assign_context(),
                };
                match union_type {
                    Some(union_type) => {
                        if let TypeKind::Union {
                            name: union_name,
                            variants,
                        } = &union_type
                        {
                            match &kind {
                                Enum { identifier: _ } => todo!(), //I don't think this ever gets reached at the moment
                                UnionLiteralKind::Struct {
                                    identifier,
                                    arguments,
                                    ..
                                } => {
                                    //match up variant to identifier
                                    let name = Self::get_identifier_name(identifier)?;
                                    for variant in variants {
                                        if variant.name == name {
                                            if let CheckedUnionVariantKind::Struct { members } =
                                                &variant.kind
                                            {
                                                if let CheckedExpression::StructLiteral {
                                                    arguments,
                                                    ..
                                                } = self.check_struct_literal(
                                                    members.to_vec(),
                                                    arguments.to_vec(),
                                                    variant.name.clone(),
                                                    union_type.clone(),
                                                    &expression_span,
                                                )? {
                                                    return Ok(CheckedExpression::UnionLiteral {
                                                        union_type: union_type.clone(),
                                                        variant: variant.clone(),
                                                        arguments,
                                                    });
                                                }
                                            } else {
                                                todo!("put error here, variant isn't a struct but you tried to initialise it as one")
                                            }
                                            todo!()
                                        }
                                    }
                                    Err(TypeError {
                                        kind: TypeErrorKind::EnumHasNoMember {
                                            enum_name: union_name.to_string(),
                                            member_name: name,
                                        },
                                        span: kind.get_span(),
                                    })
                                }
                            }
                        } else {
                            unreachable!()
                        }
                    }
                    None => Err(TypeError {
                        kind: TypeErrorKind::CannotInferType,
                        span: expression_span,
                    }),
                }
            }
            Expression::Cast {
                expression,
                destination_type,
                ..
            } => {
                let checked_expression = self.type_check_expression(*expression)?;
                let destination_type = self.check_type_expression(destination_type)?;

                self.explicit_cast(checked_expression, destination_type, expression_span)
            }
            Expression::Lambda {
                parameters, body, ..
            } => {
                let mut checked_parameters = Vec::new();
                self.push_scope();
                for param in parameters {
                    if let Expression::FunctionParameter {
                        identifiers,
                        type_annotation,
                    } = param
                    {
                        for identifier in identifiers {
                            let type_kind =
                                if let Expression::TypeAnnotation { expression, .. } =
                                    *type_annotation.clone()
                                {
                                    self.check_type_expression(expression)?
                                } else {
                                    unreachable!()
                                };

                            let param_name = Self::get_identifier_name(&identifier)?;
                            let variable = CheckedVariable {
                                name: param_name,
                                type_kind,
                                mutable: false,
                            };

                            self.try_declare_variable(identifier.span.clone(), variable.clone())?;

                            checked_parameters.push(CheckedFunctionParameter { variable });
                        }
                    } else {
                        unreachable!()
                    }
                }

                let checked_body = self.type_check_expression(*body)?;

                let inputs = checked_parameters
                    .iter()
                    .map(|p| p.variable.type_kind.clone())
                    .collect::<Vec<TypeKind>>();
                let type_kind = TypeKind::Function {
                    inputs,
                    output: Box::new(checked_body.get_type()),
                };

                //This is a bit C specific but need to declare the lambda as a named function
                let name = format!("lambda_{}", Self::random_string(16));

                let function = CheckedFunction {
                    name: name.clone(),
                    parameters: checked_parameters.clone(),
                    return_type: checked_body.get_type(),
                };

                let function_declaration = CheckedStatement::FunctionDeclaration {
                    function,
                    body: Box::new(CheckedStatement::Block {
                        statements: vec![CheckedStatement::Return {
                            expression: Some(checked_body.clone()),
                        }],
                    }),
                };
                self.pop_scope();

                self.module.functions.push(function_declaration);

                Ok(CheckedExpression::Lambda {
                    name,
                    parameters: checked_parameters,
                    body: Box::new(checked_body),
                    type_kind,
                })
            }
        }
    }

    fn random_string(n: usize) -> String {
        thread_rng()
            .sample_iter(&Alphanumeric)
            .take(n)
            .map(char::from) // From link above, this is needed in later versions
            .collect()
    }

    fn check_struct_literal(
        &mut self,
        members: Vec<CheckedFunctionParameter>,
        arguments: Vec<Expression>,
        struct_name: String,
        struct_kind: TypeKind,
        expression_span: &Span,
    ) -> Result<CheckedExpression, TypeError> {
        //match up the arguments
        //make a map of the expected parameters
        let mut expected_parameters = HashMap::new();
        for member in members {
            let member = member.variable;
            expected_parameters.insert(member.name.clone(), member);
        }
        let mut checked_arguments = Vec::new();
        for arg in arguments {
            match &arg {
                Expression::NamedArgument { name, argument, .. } => {
                    match name.kind.clone() {
                        TokenKind::Identifier(name) => {
                            match expected_parameters.get(&name) {
                                Some(expected_param) => {
                                    let arg_span = argument.get_span().clone();
                                    self.set_assign_context(&Some(
                                        expected_param.type_kind.clone(),
                                    ));
                                    let checked_arg =
                                        self.type_check_expression((**argument).clone())?;

                                    self.expect_type(
                                        &expected_param.type_kind,
                                        &checked_arg.get_type(),
                                        arg_span,
                                    )?;
                                    self.set_assign_context(&None);

                                    checked_arguments.push(CheckedExpression::NamedArgument {
                                        name: name.clone(),
                                        argument: Box::new(checked_arg),
                                    });

                                    //If we get here then we're all good
                                    expected_parameters.remove(&name);
                                }
                                None => {
                                    return Err(TypeError {
                                        kind: TypeErrorKind::StructHasNoMember {
                                            struct_name,
                                            name,
                                        },
                                        span: arg.get_span(),
                                    })
                                }
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                Expression::Variable { name, span } => {
                    let variable = self.try_get_variable(span.clone(), name)?;
                    match expected_parameters.get(name) {
                        Some(expected_param) => {
                            self.expect_type(
                                &expected_param.type_kind,
                                &variable.type_kind,
                                span.clone(),
                            )?;

                            checked_arguments.push(CheckedExpression::NamedArgument {
                                name: name.clone(),
                                argument: Box::new(CheckedExpression::Variable(variable)),
                            });

                            //If we get here then we're all good
                            expected_parameters.remove(name);
                        }
                        None => {
                            return Err(TypeError {
                                kind: TypeErrorKind::StructHasNoMember {
                                    struct_name,
                                    name: name.to_string(),
                                },
                                span: arg.get_span(),
                            })
                        }
                    }
                }
                _ => todo!("make an error for this"),
            }
        }
        if expected_parameters.is_empty() {
            return Ok(CheckedExpression::StructLiteral {
                type_kind: struct_kind,
                arguments: checked_arguments,
            });
        }
        return Err(TypeError {
            kind: TypeErrorKind::StructMembersMissing {
                struct_name,
                expected_parameters: expected_parameters.values().cloned().collect(),
            },
            span: expression_span.clone(),
        });
    }

    fn type_check_binary_op(
        &self,
        left: TypeKind,
        right: TypeKind,
        op: BinaryOperator,
        span: Span,
    ) -> Result<CheckedBinaryOp, TypeError> {
        match op {
            BinaryOperator::Add
            | BinaryOperator::Sub
            | BinaryOperator::Mul
            | BinaryOperator::Div
            | BinaryOperator::Mod
            | BinaryOperator::BitwiseAnd
            | BinaryOperator::BitwiseOr => {
                if self.is_numeric_type(&left) && self.is_numeric_type(&right) {
                    Ok(CheckedBinaryOp {
                        op,
                        return_type: left,
                    })
                } else {
                    Err(TypeError {
                        kind: TypeErrorKind::BinaryOperationNotApplicableToTypes {
                            left,
                            op,
                            right,
                        },
                        span,
                    })
                }
            }
            BinaryOperator::Gt
            | BinaryOperator::GtEq
            | BinaryOperator::Lt
            | BinaryOperator::LtEq => {
                if self.is_numeric_type(&left) && self.is_numeric_type(&right) {
                    Ok(CheckedBinaryOp {
                        op,
                        return_type: TypeKind::Bool,
                    })
                } else {
                    Err(TypeError {
                        kind: TypeErrorKind::BinaryOperationNotApplicableToTypes {
                            left,
                            op,
                            right,
                        },
                        span,
                    })
                }
            }
            BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => match (&left, &right) {
                (TypeKind::Bool, TypeKind::Bool) => Ok(CheckedBinaryOp {
                    op,
                    return_type: TypeKind::Bool,
                }),
                _ => Err(TypeError {
                    kind: TypeErrorKind::BinaryOperationNotApplicableToTypes { left, op, right },
                    span,
                }),
            },
            BinaryOperator::Eq | BinaryOperator::NEq => {
                if Self::type_is_assignable_from(&left, &right) {
                    Ok(CheckedBinaryOp {
                        op,
                        return_type: TypeKind::Bool,
                    })
                } else {
                    Err(TypeError {
                        kind: TypeErrorKind::BinaryOperationNotApplicableToTypes {
                            left,
                            op,
                            right,
                        },
                        span,
                    })
                }
            }
            BinaryOperator::Range => {
                if !self.is_iterable_type(&left) {
                    return Err(TypeError {
                        kind: TypeErrorKind::TypeIsNotIterable { type_kind: left },
                        span,
                    });
                }
                if Self::type_is_assignable_from(&left, &right) {
                    Ok(CheckedBinaryOp {
                        op,
                        return_type: TypeKind::Range {
                            type_kind: Box::new(left),
                        },
                    })
                } else {
                    Err(TypeError {
                        kind: TypeErrorKind::BinaryOperationNotApplicableToTypes {
                            left,
                            op,
                            right,
                        },
                        span,
                    })
                }
            }
        }
    }

    fn is_numeric_type(&self, type_kind: &TypeKind) -> bool {
        matches!(
            type_kind,
            TypeKind::I8
                | TypeKind::I16
                | TypeKind::I32
                | TypeKind::I64
                | TypeKind::U8
                | TypeKind::U16
                | TypeKind::U32
                | TypeKind::U64
                | TypeKind::F32
                | TypeKind::F64
                | TypeKind::F128
        )
    }

    fn type_check_unary_op(
        &self,
        op: UnaryOperator,
        operand: TypeKind,
        span: Span,
    ) -> Result<CheckedUnaryOp, TypeError> {
        match op {
            UnaryOperator::Ref => Ok(CheckedUnaryOp {
                op,
                return_type: TypeKind::Pointer {
                    reference_type: Box::new(operand),
                },
            }),
            UnaryOperator::Not => match operand {
                TypeKind::Bool => Ok(CheckedUnaryOp {
                    op,
                    return_type: TypeKind::Bool,
                }),
                _ => Err(TypeError {
                    kind: TypeErrorKind::UnaryOperationNotApplicableToType { op, operand },
                    span,
                }),
            },
            UnaryOperator::Neg => match operand {
                TypeKind::I32 => Ok(CheckedUnaryOp {
                    op,
                    return_type: TypeKind::I32,
                }),
                TypeKind::F64 => Ok(CheckedUnaryOp {
                    op,
                    return_type: TypeKind::F64,
                }),
                _ => Err(TypeError {
                    kind: TypeErrorKind::UnaryOperationNotApplicableToType { op, operand },
                    span,
                }),
            },
        }
    }

    fn is_iterable_type(&self, type_kind: &TypeKind) -> bool {
        matches!(
            type_kind,
            TypeKind::I8
                | TypeKind::I16
                | TypeKind::I32
                | TypeKind::I64
                | TypeKind::U8
                | TypeKind::U16
                | TypeKind::U32
                | TypeKind::U64
                | TypeKind::F32
                | TypeKind::F64
                | TypeKind::F128
                | TypeKind::String
                | TypeKind::Range { .. }
                | TypeKind::Array { .. }
                | TypeKind::Slice { .. }
        )
    }

    //These methods are kinda goofy
    fn set_return_context(&mut self, return_type: &TypeKind) {
        self.scopes.first_mut().unwrap().return_context = return_type.clone();
    }

    fn clear_return_context(&mut self) {
        self.scopes.first_mut().unwrap().return_context = TypeKind::Unit;
    }

    fn get_return_context(&self) -> TypeKind {
        self.scopes.first().unwrap().return_context.clone()
    }
    fn set_assign_context(&mut self, assign_type: &Option<TypeKind>) {
        self.scopes.first_mut().unwrap().assign_context = assign_type.clone();
    }

    fn get_assign_context(&self) -> Option<TypeKind> {
        self.scopes.first().unwrap().assign_context.clone()
    }

    fn check_all_paths_return(checked_body: &CheckedStatement) -> bool {
        match checked_body {
            CheckedStatement::Block { statements } => {
                if statements.is_empty() {
                    return false;
                }
                return Self::check_all_paths_return(statements.last().unwrap());
            }
            CheckedStatement::If {
                body, else_clause, ..
            } => match else_clause {
                Some(else_clause) => {
                    Self::check_all_paths_return(body) && Self::check_all_paths_return(else_clause)
                }
                None => Self::check_all_paths_return(body),
            },
            CheckedStatement::Else { body } => Self::check_all_paths_return(body),
            CheckedStatement::Return { .. } => true,
            CheckedStatement::Match { arms, .. } => {
                for arm in arms {
                    if let CheckedExpression::MatchArm { body, .. } = arm {
                        if !Self::check_all_paths_return(body) {
                            return false;
                        }
                    }
                }
                true
            }
            _ => false,
        }
    }
    fn check_member_access(
        checked_accessee: CheckedExpression,
        member_name: String,
        member: &Token,
        expression_span: Span,
    ) -> Result<CheckedExpression, TypeError> {
        let member_span = member.span.clone();
        match checked_accessee.get_type() {
            TypeKind::Struct { name: _, members } => {
                match members.iter().find(|p| p.variable.name == member_name) {
                    Some(param) => Ok(CheckedExpression::MemberAccess {
                        accessee: Some(Box::new(checked_accessee)),
                        member: param.variable.clone(),
                    }),
                    None => Err(TypeError {
                        kind: TypeErrorKind::NoSuchMemberInStruct {
                            type_kind: checked_accessee.get_type(),
                            name: member_name,
                        },
                        span: member_span,
                    }),
                }
            }
            TypeKind::Slice { .. } => match member_name.as_str() {
                "len" => Ok(CheckedExpression::MemberAccess {
                    accessee: Some(Box::new(checked_accessee)),
                    member: CheckedVariable {
                        name: "len".to_string(),
                        type_kind: TypeKind::I32,
                        mutable: false,
                    },
                }),
                _ => Err(TypeError {
                    kind: TypeErrorKind::NoSuchMemberInStruct {
                        type_kind: checked_accessee.get_type(),
                        name: member_name,
                    },
                    span: member_span,
                }),
            },
            TypeKind::String => match member_name.as_str() {
                "len" => Ok(CheckedExpression::MemberAccess {
                    accessee: Some(Box::new(checked_accessee)),
                    member: CheckedVariable {
                        name: "len".to_string(),
                        type_kind: TypeKind::I32,
                        mutable: false,
                    },
                }),
                _ => Err(TypeError {
                    kind: TypeErrorKind::NoSuchMemberInStruct {
                        type_kind: checked_accessee.get_type(),
                        name: member_name,
                    },
                    span: member_span,
                }),
            },
            TypeKind::FileHandle => match member_name.as_str() {
                "size" => Ok(CheckedExpression::MemberAccess {
                    accessee: Some(Box::new(checked_accessee)),
                    member: CheckedVariable {
                        name: "size".to_string(),
                        type_kind: TypeKind::U32,
                        mutable: false,
                    },
                }),
                _ => Err(TypeError {
                    kind: TypeErrorKind::NoSuchMemberInStruct {
                        type_kind: checked_accessee.get_type(),
                        name: member_name,
                    },
                    span: member_span,
                }),
            },
            TypeKind::Pointer { .. } => {
                //This is a bit C-Specific, but it turns p.x into (*p).x,
                //it would be better to be aware of this and emit p->x but it's all the same in the end
                let checked_accessee = CheckedExpression::Parenthesised {
                    expression: Box::new(CheckedExpression::Dereference {
                        expression: Box::new(checked_accessee),
                    }),
                };
                Self::check_member_access(checked_accessee, member_name, member, expression_span)
            }
            TypeKind::Type { type_kind } => match &*type_kind {
                TypeKind::Enum {
                    name: enum_name,
                    members,
                } => {
                    if members.contains(&member_name) {
                        Ok(CheckedExpression::MemberAccess {
                            accessee: Some(Box::new(checked_accessee)),
                            member: CheckedVariable {
                                name: member_name,
                                type_kind: *type_kind,
                                mutable: false,
                            },
                        })
                    } else {
                        Err(TypeError {
                            kind: TypeErrorKind::EnumHasNoMember {
                                enum_name: enum_name.to_string(),
                                member_name,
                            },
                            span: member_span,
                        })
                    }
                }
                TypeKind::Union {
                    name: union_name,
                    variants,
                } => {
                    for variant in variants {
                        if variant.name == member_name {
                            match &variant.kind {
                                CheckedUnionVariantKind::Enum => {
                                    return Ok(CheckedExpression::MemberAccess {
                                        accessee: Some(Box::new(checked_accessee)),
                                        member: CheckedVariable {
                                            name: member_name,
                                            type_kind: *type_kind,
                                            mutable: false,
                                        },
                                    });
                                }
                                CheckedUnionVariantKind::Struct { members: _ } => {}
                            }
                        } else {
                            return Err(TypeError {
                                kind: TypeErrorKind::EnumHasNoMember {
                                    enum_name: union_name.to_string(),
                                    member_name,
                                },
                                span: member_span,
                            });
                        }
                    }
                    Err(TypeError {
                        kind: TypeErrorKind::EnumHasNoMember {
                            enum_name: union_name.to_string(),
                            member_name,
                        },
                        span: member_span,
                    })
                }
                _ => Err(TypeError {
                    kind: TypeErrorKind::TypeHasNoMembers {
                        type_kind: checked_accessee.get_type(),
                    },
                    span: expression_span,
                }),
            },
            _ => Err(TypeError {
                kind: TypeErrorKind::TypeHasNoMembers {
                    type_kind: checked_accessee.get_type(),
                },
                span: expression_span,
            }),
        }
    }

    fn get_identifier_name(token: &Token) -> Result<String, TypeError> {
        if let TokenKind::Identifier(name) = &token.kind {
            return Ok(name.clone());
        }
        Err(TypeError {
            kind: TypeErrorKind::ExpectedIdentifier,
            span: token.span.clone(),
        })
    }
}
