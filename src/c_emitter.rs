use std::{
    fs::File,
    io::{self, Write},
};

use crate::{
    parser::{BinaryOperator, UnaryOperator},
    type_checker::{
        CheckedExpression, CheckedPattern, CheckedStatement, CheckedUnionVariantKind, Module,
        TypeKind,
    },
};

pub struct Emitter {
    out_file: File,
}

impl Emitter {
    pub fn new(out_file: File) -> Emitter {
        Emitter { out_file }
    }

    pub fn emit(&mut self, module: Module) -> io::Result<()> {
        self.out_file.write_all(b"#include <stdio.h>\n")?;
        self.out_file.write_all(b"#include <stdbool.h>\n")?;
        self.out_file.write_all(b"#include <stdint.h>\n")?;
        self.out_file.write_all(
            b"void print_bool(bool b) { b ? printf(\"true\\n\") : printf(\"false\\n\"); }\n",
        )?;
        //self.out_file.write_all(b"#include <stdlib.h>\n")?;
        self.out_file.write_all(b"")?;

        for type_definition in module.types {
            self.emit_statement(type_definition)?;
        }

        for global_variable_declaration in module.global_variables {
            self.emit_statement(global_variable_declaration)?;
        }

        for statement in module.functions {
            self.emit_statement(statement)?;
        }

        Ok(())
    }

    fn get_c_type(tyr_type: &TypeKind) -> String {
        match tyr_type {
            TypeKind::Unit => "void".to_string(),
            TypeKind::Bool => "bool".to_string(),
            TypeKind::U8 => "unsigned char".to_string(),
            TypeKind::U16 => "unsigned int".to_string(),
            TypeKind::U32 => "unsigned long".to_string(),
            TypeKind::U64 => "unsigned long long".to_string(),
            TypeKind::I8 => "signed char".to_string(),
            TypeKind::I16 => "signed int".to_string(),
            TypeKind::I32 => "signed long".to_string(),
            TypeKind::I64 => "signed long long".to_string(),
            TypeKind::F32 => "float".to_string(),
            TypeKind::F64 => "double".to_string(),
            TypeKind::F128 => "long double".to_string(),
            TypeKind::Char => "char".to_string(),
            TypeKind::String => "char *".to_string(),
            TypeKind::FileHandle => "FILE *".to_string(),
            TypeKind::Struct { name, .. }
            | TypeKind::Enum { name, .. }
            | TypeKind::Union { name, .. }
            | TypeKind::UnionVariant { name, .. } => name.clone(),
            TypeKind::Type { type_kind } => Self::get_c_type(type_kind),
            TypeKind::Range { .. } => todo!(),
            TypeKind::Array { element_type, .. } => {
                format!("{}[]", Self::get_c_type(element_type))
            }
            TypeKind::Slice { element_type } => {
                format!("slice_{}", element_type) //these types were created in the type checker so do not use the c-type names
            }
            TypeKind::Pointer { reference_type } => {
                format!("{}*", Self::get_c_type(reference_type))
            }
            TypeKind::Function { .. } => {
                todo!()
            }
        }
    }

    fn get_c_variable_decl(&self, name: &String, type_kind: &TypeKind, _mutable: bool) -> String {
        //TODO const variables if mutable == false
        match type_kind {
            TypeKind::Array { size, element_type } => {
                //TODO: Obviously this needs to be recursive
                if let TypeKind::Array {
                    size: inner_size,
                    element_type: inner_type,
                } = (**element_type).clone()
                {
                    return format!(
                        "{} {}[{}][{}]",
                        Self::get_c_type(&inner_type),
                        name,
                        size,
                        inner_size,
                    );
                }
                format!("{} {}[{}]", Self::get_c_type(element_type), name, size)
            }
            TypeKind::Pointer { reference_type } => {
                format!("{} *{}", Self::get_c_type(reference_type), name)
            }
            TypeKind::Function { inputs, output } => {
                format!(
                    "{} (*{})({})",
                    Self::get_c_type(output),
                    name,
                    inputs
                        .iter()
                        .map(Self::get_c_type)
                        .collect::<Vec<String>>()
                        .join(",")
                )
            }
            _ => format!("{} {}", Self::get_c_type(type_kind), name),
        }
    }

    fn emit_statement(&mut self, statement: CheckedStatement) -> io::Result<()> {
        match statement {
            CheckedStatement::FunctionDeclaration { function, body } => {
                if function.name == "main" {
                    let params = function.parameters;
                    if params.is_empty() {
                        write!(self.out_file, "int main() {{")?;
                    } else if params.len() > 1 {
                        todo!("Catch this at the type check stage")
                    } else {
                        let param = &params.first().unwrap().variable;

                        if let TypeKind::Slice { element_type } = &param.type_kind {
                            if **element_type == TypeKind::String {
                                write!(self.out_file, "int main(int argc, char **argv) {{")?;
                                write!(
                                    self.out_file,
                                    "slice_string {} = (slice_string){{0, argc, argv}};",
                                    param.name
                                )?;
                            } else {
                                todo!("Catch this at the type check stage")
                            }
                        } else {
                            todo!("Catch this at the type check stage")
                        }
                    }
                    if let CheckedStatement::Block { statements } = *body {
                        for statement in statements {
                            self.emit_statement(statement)?;
                        }
                        writeln!(self.out_file, "return 0;")?;
                        writeln!(self.out_file, "}}")?;
                    }
                    return Ok(());
                }

                write!(
                    self.out_file,
                    "{} {}(",
                    Self::get_c_type(&function.return_type),
                    function.name
                )?;

                let param_count = function.parameters.len();
                let mut i = 1;
                for param in function.parameters {
                    write!(
                        self.out_file,
                        "{}",
                        self.get_c_variable_decl(
                            &param.variable.name,
                            &param.variable.type_kind,
                            param.variable.mutable
                        ),
                    )?;

                    if i < param_count {
                        write!(self.out_file, ", ",)?;
                    }

                    i += 1;
                }
                write!(self.out_file, ") {{",)?;
                match *body {
                    CheckedStatement::Block { statements } => {
                        for statement in statements {
                            self.emit_statement(statement)?;
                        }
                        writeln!(self.out_file, "}}")?;
                    }
                    CheckedStatement::Expression { expression } => {
                        self.emit_expression(&expression)?;
                        writeln!(self.out_file, ";}}")?;
                    }
                    _ => (),
                }
                Ok(())
            }
            CheckedStatement::Expression { expression } => {
                self.emit_expression(&expression)?;
                writeln!(self.out_file, ";")?;
                Ok(())
            }
            CheckedStatement::Block { statements } => {
                writeln!(self.out_file, "{{")?;
                for statement in statements {
                    self.emit_statement(statement)?;
                }
                writeln!(self.out_file, "}}")?;
                Ok(())
            }
            CheckedStatement::While { condition, body } => {
                write!(self.out_file, "while (")?;
                self.emit_expression(&condition)?;
                write!(self.out_file, ") {{")?;
                self.emit_statement(*body)?;
                write!(self.out_file, "}}")?;
                Ok(())
            }
            CheckedStatement::If {
                condition,
                body,
                else_clause,
            } => {
                write!(self.out_file, "if (")?;
                self.emit_expression(&condition)?;
                write!(self.out_file, ") {{")?;
                self.emit_statement(*body)?;
                write!(self.out_file, "}}")?;

                if let Some(else_clause) = else_clause {
                    self.emit_statement(*else_clause)?;
                }

                Ok(())
            }
            CheckedStatement::Else { body } => {
                write!(self.out_file, "else {{")?;
                self.emit_statement(*body)?;
                write!(self.out_file, "}}")?;
                Ok(())
            }
            CheckedStatement::TypeDefinition { type_kind } => match type_kind {
                TypeKind::Struct { name, members } => {
                    write!(self.out_file, "typedef struct {{")?;
                    for member in &members {
                        writeln!(
                            self.out_file,
                            "{} {};",
                            Self::get_c_type(&member.variable.type_kind),
                            member.variable.name
                        )?;
                    }
                    write!(self.out_file, "}} {};", name)?;
                    Ok(())
                }
                TypeKind::Enum { name, members } => {
                    let members_count = members.len();
                    write!(self.out_file, "typedef enum {{")?;
                    for member in &members {
                        //Prefixes the member with the enum name, e.g. Color_Red
                        writeln!(self.out_file, "{}_{},", name, member)?;
                    }
                    write!(self.out_file, "}} {};", name)?;
                    //Add string array of names
                    //static const char *Color_Values[3] = {"Red", "Green", "Blue"};
                    write!(
                        self.out_file,
                        "static const char *{}_Values[{}] = {{",
                        name, members_count
                    )?;
                    for member in &members {
                        writeln!(self.out_file, "\"{}\",", member)?;
                    }
                    write!(self.out_file, "}};")?;
                    Ok(())
                }
                TypeKind::Union { name, variants } => {
                    writeln!(self.out_file, "typedef struct {} {{", name)?;
                    writeln!(self.out_file, "int tag;")?;
                    writeln!(self.out_file, "union {}_variants {{", name)?;
                    for variant in variants {
                        match variant.kind {
                            CheckedUnionVariantKind::Enum => {
                                writeln!(self.out_file, "struct {} {{", variant.name)?;
                                writeln!(self.out_file, "int tag;")?;
                                writeln!(self.out_file, "}} {};", variant.name)?;
                            }
                            CheckedUnionVariantKind::Struct { members } => {
                                writeln!(self.out_file, "struct {} {{", variant.name)?;
                                writeln!(self.out_file, "int tag;")?;
                                for param in members {
                                    writeln!(
                                        self.out_file,
                                        "{};",
                                        self.get_c_variable_decl(
                                            &param.variable.name,
                                            &param.variable.type_kind,
                                            param.variable.mutable
                                        )
                                    )?;
                                }
                                writeln!(self.out_file, "}} {};", variant.name)?;
                            }
                        }
                    }
                    writeln!(self.out_file, "}} {};", name)?;
                    writeln!(self.out_file, "}} {};", name)?;
                    Ok(())
                }
                // CheckedTypeDefinition::UnionVariant { tag, members } => todo!(),
                _ => unreachable!(),
            },
            CheckedStatement::For {
                iterator,
                iterable,
                body,
            } => {
                match iterable {
                    CheckedExpression::Binary {
                        left: lower,
                        op,
                        right: upper,
                    } if op.op == BinaryOperator::Range => {
                        write!(
                            self.out_file,
                            "for ({} {} = ",
                            Self::get_c_type(&iterator.type_kind),
                            iterator.name
                        )?;
                        self.emit_expression(&lower)?;

                        write!(self.out_file, "; {} < ", iterator.name)?;
                        self.emit_expression(&upper)?;
                        write!(self.out_file, "; {}++)", iterator.name)?;
                        self.emit_statement(*body)?;
                    }
                    CheckedExpression::Binary { .. } => unreachable!(),
                    CheckedExpression::Variable(variable) => match variable.type_kind {
                        TypeKind::Array { size, element_type } => {
                            write!(
                                self.out_file,
                                "for (int it = {}.offset; it < {}; it++) {{",
                                variable.name, size
                            )?;
                            write!(
                                self.out_file,
                                "{} {} = {}[it];",
                                Self::get_c_type(&element_type),
                                iterator.name,
                                variable.name
                            )?;
                            self.emit_statement(*body)?;
                            write!(self.out_file, "}}")?;
                        }
                        TypeKind::Slice { element_type } => {
                            write!(
                                self.out_file,
                                "for (int it = {}.offset; it < {}.len; it++) {{",
                                variable.name, variable.name
                            )?;
                            write!(
                                self.out_file,
                                "{} {} = {}.data[it];",
                                Self::get_c_type(&element_type),
                                iterator.name,
                                variable.name
                            )?;
                            self.emit_statement(*body)?;
                            write!(self.out_file, "}}")?;
                        }
                        TypeKind::String => {
                            write!(
                                self.out_file,
                                "for (int it = 0; it < strlen({}); it++) {{",
                                variable.name
                            )?;
                            write!(
                                self.out_file,
                                "char {} = {}[it];",
                                iterator.name, variable.name
                            )?;
                            self.emit_statement(*body)?;
                            write!(self.out_file, "}}")?;
                        }
                        _ => todo!(
                            "iteration of type `{}` is not yet emitted",
                            variable.type_kind
                        ),
                    },
                    CheckedExpression::Slice { array: _, range: _ } => {
                        todo!("I can't be bothered to write this right now")
                        // write!(
                        //     self.out_file,
                        //     "for (int it = {}.offset; it <= {}.len; it++) {{",
                        //     variable.name, variable.name
                        // )?;
                        // write!(
                        //     self.out_file,
                        //     "{} {} = {}.data[it];",
                        //     Self::get_c_type(&element_type),
                        //     iterator.name,
                        //     variable.name
                        // )?;
                        // self.emit_statement(*body)?;
                        // write!(self.out_file, "}}")?;
                    }
                    _ => todo!("{:?}", iterable),
                }
                Ok(())
            }
            CheckedStatement::Match { expression, arms } => {
                write!(self.out_file, "switch (")?;
                match expression.get_type() {
                    TypeKind::Struct {
                        name: _,
                        members: _,
                    } => todo!(),
                    TypeKind::Enum {
                        name: _,
                        members: _,
                    } => self.emit_expression(&expression)?,
                    TypeKind::Union { .. } => {
                        self.emit_expression(&expression)?;
                        write!(self.out_file, ".tag")?;
                    }
                    _ => todo!(),
                }
                write!(self.out_file, ") {{")?;

                for arm in arms {
                    if let CheckedExpression::MatchArm { pattern, body } = arm {
                        self.emit_match_arm(&expression, &pattern, &body)?;
                    }
                }
                write!(self.out_file, "}}")?;
                Ok(())
            }
            CheckedStatement::Uses { module: _ } => todo!(),
            CheckedStatement::Noop => Ok(()),
            CheckedStatement::Return { expression } => {
                write!(self.out_file, "return ")?;
                if let Some(expression) = expression {
                    self.emit_expression(&expression)?
                }
                writeln!(self.out_file, ";")?;
                Ok(())
            }
            CheckedStatement::Raw { lines } => {
                for line in lines {
                    writeln!(self.out_file, "{}", line)?;
                }
                Ok(())
            }
        }
    }

    fn emit_expression(&mut self, expression: &CheckedExpression) -> io::Result<()> {
        match expression {
            CheckedExpression::IntLiteral { value, .. } => write!(self.out_file, "{}", value),
            CheckedExpression::FloatLiteral { value, .. } => write!(self.out_file, "{}", value),
            CheckedExpression::BoolLiteral { value } => write!(self.out_file, "{}", value),
            CheckedExpression::CharLiteral { value } => write!(self.out_file, "'{}'", value),
            CheckedExpression::StringLiteral { value } => write!(self.out_file, "\"{}\"", value),
            CheckedExpression::VariableDeclaration {
                variables,
                initialiser,
            } => {
                for (i, variable) in variables.iter().enumerate() {
                    write!(
                        self.out_file,
                        "{} = ",
                        self.get_c_variable_decl(
                            &variable.name,
                            &variable.type_kind,
                            variable.mutable
                        )
                    )?;
                    self.emit_expression(initialiser)?;
                    if i < variables.len() - 1 {
                        writeln!(self.out_file, ";")?;
                    }
                }
                Ok(())
            }
            CheckedExpression::Variable(variable) => write!(self.out_file, "{}", variable.name),
            CheckedExpression::FunctionCall {
                function,
                arguments,
            } => {
                if function.name == "print" {
                    let arg = arguments.first().unwrap();
                    match arg.get_type() {
                        TypeKind::Unit => todo!(),
                        TypeKind::Bool => write!(self.out_file, "print_bool(")?,
                        TypeKind::I8 => write!(self.out_file, "printf(\"%d\\n\", ")?,
                        TypeKind::I16 => write!(self.out_file, "printf(\"%hi\\n\", ")?,
                        TypeKind::I32 => write!(self.out_file, "printf(\"%li\\n\", ")?,
                        TypeKind::I64 => write!(self.out_file, "printf(\"%lli\\n\", ")?,
                        TypeKind::U8 => write!(self.out_file, "printf(\"%d\\n\", ")?,
                        TypeKind::U16 => write!(self.out_file, "printf(\"%hu\\n\", ")?,
                        TypeKind::U32 => write!(self.out_file, "printf(\"%lu\\n\", ")?,
                        TypeKind::U64 => write!(self.out_file, "printf(\"%llu\\n\", ")?,
                        TypeKind::F32 => write!(self.out_file, "printf(\"%g\\n\", ")?,
                        TypeKind::F64 => write!(self.out_file, "printf(\"%lg\\n\", ")?,
                        TypeKind::F128 => write!(self.out_file, "printf(\"%Lg\\n\", ")?,
                        TypeKind::Char => write!(self.out_file, "printf(\"%c\\n\", ")?,
                        TypeKind::String => write!(self.out_file, "printf(\"%s\\n\", ")?,
                        TypeKind::Struct { .. }
                        | TypeKind::Union { .. }
                        | TypeKind::UnionVariant { .. } => todo!(),
                        TypeKind::Enum { name, members: _ } => {
                            write!(self.out_file, "printf(\"%s\\n\", {}_Values[", name)?;
                            self.emit_expression(arg)?;
                            write!(self.out_file, "])")?;
                            return Ok(());
                        }
                        TypeKind::Type { type_kind: _ } => todo!(),
                        TypeKind::Range { type_kind: _ } => todo!(),
                        TypeKind::Array {
                            size: _,
                            element_type: _,
                        } => todo!(),
                        TypeKind::Slice { element_type: _ } => todo!(),
                        TypeKind::Pointer { .. } => write!(self.out_file, "printf(\"%zu\\n\", ")?,
                        TypeKind::Function {
                            inputs: _,
                            output: _,
                        } => todo!(),
                        TypeKind::FileHandle => todo!(),
                    }
                    self.emit_expression(arg)?;
                    write!(self.out_file, ")")?;
                    Ok(())
                } else {
                    write!(self.out_file, "{}(", function.name)?;
                    let mut i = 1;
                    let arg_count = arguments.len();
                    for arg in arguments {
                        //TODO this is a bad way to do this but it works for now, in future have a ConvertExpression{from: TypeKind, to: TypeKind} or something

                        if let TypeKind::Array { size, element_type } = arg.get_type() {
                            if let TypeKind::Slice { .. } =
                                function.parameters.get(i - 1).unwrap().variable.type_kind
                            {
                                //what a mouthful
                                write!(self.out_file, "(slice_{}){{0, {}, &", element_type, size)?;
                                self.emit_expression(arg)?;
                                write!(self.out_file, "}}")?;
                                if i < arg_count {
                                    write!(self.out_file, ", ")?;
                                    i += 1;
                                }
                                continue;
                            }
                        }

                        self.emit_expression(arg)?;
                        if i < arg_count {
                            write!(self.out_file, ", ")?;
                            i += 1;
                        }
                    }
                    write!(self.out_file, ")")?;
                    Ok(())
                }
            }
            CheckedExpression::Binary { left, op, right } => {
                self.emit_expression(left)?;
                self.emit_binary_op(&op.op)?;
                self.emit_expression(right)?;
                Ok(())
            }
            CheckedExpression::Unary { op, operand } => {
                self.emit_unary_op(&op.op)?;
                self.emit_expression(operand)?;
                Ok(())
            }
            CheckedExpression::Parenthesised { expression } => {
                write!(self.out_file, "(")?;
                self.emit_expression(expression)?;
                write!(self.out_file, ")")?;
                Ok(())
            }
            CheckedExpression::Assignment { left, assignment } => {
                self.emit_expression(left)?;
                write!(self.out_file, " = ")?;
                self.emit_expression(assignment)?;
                Ok(())
            }
            CheckedExpression::StructLiteral {
                type_kind,
                arguments,
            } => {
                match type_kind {
                    TypeKind::Struct {
                        name: _,
                        members: _,
                    } => {
                        //(point){.x=1, .y=1}
                        write!(self.out_file, "({}){{", Self::get_c_type(type_kind))?;
                        let mut i = 1;
                        let arg_count = arguments.len();
                        for arg in arguments {
                            self.emit_expression(arg)?;

                            if i < arg_count {
                                write!(self.out_file, ", ")?;
                            }
                            i += 1;
                        }
                        write!(self.out_file, "}}")?;

                        Ok(())

                        // CheckedTypeDefinition::UnionVariant { tag, members } => {
                        //     //(Shape){.tag = 1, .Shape.Circle.tag = 1, .Shape.Circle.radius = 4.5};
                        //     todo!("{}", name)
                        // }
                    }
                    _ => unreachable!(),
                }
            }
            CheckedExpression::NamedArgument { name, argument } => {
                write!(self.out_file, ".{}=", name)?;
                self.emit_expression(argument)?;
                Ok(())
            }
            CheckedExpression::MemberAccess { accessee, member } => {
                if let TypeKind::Enum { name, .. } = &member.type_kind {
                    write!(self.out_file, "{}_{}", name, member.name)?;
                    return Ok(());
                }
                if let TypeKind::Union { name, variants } = &member.type_kind {
                    //(Shape){.tag = 0, .Shape.Point.tag = 0};
                    for variant in variants {
                        if member.name == variant.name {
                            write!(
                                self.out_file,
                                "({}){{.tag = {}, .{}.{}.tag = {}}}",
                                name, variant.tag, name, variant.name, variant.tag
                            )?;
                            return Ok(());
                        }
                    }
                    unreachable!()
                }
                if let TypeKind::String = &accessee.clone().unwrap().get_type() {
                    if member.name == "len" {
                        write!(self.out_file, "strlen(")?;
                        self.emit_expression(&accessee.clone().unwrap())?;
                        write!(self.out_file, ")")?;
                        return Ok(());
                    }
                }
                self.emit_expression(&accessee.clone().unwrap())?; //I swear to god rust
                write!(self.out_file, ".{}", member.name)?;
                Ok(())
            }
            CheckedExpression::Path { segments, member } => {
                if segments.len() > 1 {
                    todo!("multi-segment paths")
                }
                match (**member).clone() {
                    CheckedExpression::Variable(variable) => match segments.last() {
                        Some(accessee) => match accessee.get_type() {
                            TypeKind::Type { type_kind } => match *type_kind {
                                TypeKind::Struct {
                                    name: _,
                                    members: _,
                                } => todo!(),
                                TypeKind::Enum { name, members: _ } => {
                                    write!(self.out_file, "{}_{}", name, variable.name)?;
                                }
                                _ => todo!("{}", accessee.get_type()),
                            },
                            _ => todo!("{}", accessee.get_type()),
                        },
                        None => self.emit_expression(member)?,
                    },
                    CheckedExpression::StructLiteral {
                        type_kind: struct_type,
                        arguments,
                    } => {
                        match segments.last() {
                            Some(accessee) => match accessee.get_type() {
                                TypeKind::Type { type_kind: _ } => match struct_type {
                                    TypeKind::Struct { name, members } => {
                                        todo!("{:?} : {:?}", name, members)
                                    }
                                    TypeKind::Enum {
                                        name: _,
                                        members: _,
                                    } => todo!(),
                                    // CheckedTypeDefinition::UnionVariant { tag, members } => {
                                    //     write!(
                                    //         self.out_file,
                                    //         "({})",
                                    //         Self::get_c_type(&accessee.get_type())
                                    //     )?;
                                    //     write!(self.out_file, "{{.tag={}", tag)?;
                                    //     for (arg, member) in zip(arguments, members) {
                                    //         write!(
                                    //             self.out_file,
                                    //             ", .{}.{}",
                                    //             Self::get_c_type(&accessee.get_type()),
                                    //             name,
                                    //         )?;
                                    //         self.emit_expression(&arg)?;
                                    //     }
                                    //     write!(self.out_file, "}}")?;
                                    // }
                                    TypeKind::UnionVariant { name, variant } => {
                                        //(Shape){.tag = 1, .Shape.Circle.radius = 3.14};
                                        write!(
                                            self.out_file,
                                            "({})",
                                            Self::get_c_type(&accessee.get_type())
                                        )?;
                                        write!(self.out_file, "{{.tag = {}", variant.tag)?;
                                        match variant.kind {
                                            CheckedUnionVariantKind::Enum => todo!(),
                                            CheckedUnionVariantKind::Struct { .. } => {
                                                for arg in arguments {
                                                    write!(
                                                        self.out_file,
                                                        ", .{}.{}",
                                                        Self::get_c_type(&accessee.get_type()),
                                                        name,
                                                    )?;
                                                    self.emit_expression(&arg)?;
                                                }
                                            }
                                        }
                                        write!(self.out_file, "}}")?;
                                    }
                                    _ => todo!("{}", struct_type),
                                },
                                _ => todo!("{}", accessee.get_type()),
                            },
                            None => self.emit_expression(member)?,
                        }
                    }
                    _ => {
                        todo!()
                    }
                }

                Ok(())
            }
            CheckedExpression::Type { type_kind: _ } => todo!(),
            CheckedExpression::ArrayLiteral {
                type_kind,
                elements,
            } => {
                if let TypeKind::Array { .. } = type_kind {
                    // write!(
                    //     self.out_file,
                    //     "({}[{}])",
                    //     Self::get_c_type(element_type),
                    //     size
                    // )?;
                    write!(self.out_file, "{{")?;
                    for element in elements {
                        self.emit_expression(element)?;
                        write!(self.out_file, ", ")?;
                    }
                    write!(self.out_file, "}}")?;
                } else {
                    unreachable!()
                }
                Ok(())
            }
            CheckedExpression::ArrayIndex { array, index } => {
                self.emit_expression(array)?;

                if let TypeKind::Slice { .. } = array.get_type() {
                    write!(self.out_file, ".data")?;
                }

                write!(self.out_file, "[")?;
                self.emit_expression(index)?;
                write!(self.out_file, "]")?;
                Ok(())
            }
            CheckedExpression::MatchArm {
                pattern: _,
                body: _,
            } => {
                todo!()
            }
            CheckedExpression::Dereference { expression } => {
                write!(self.out_file, "*")?;
                self.emit_expression(expression)?;
                Ok(())
            }
            CheckedExpression::Slice { array, range } => {
                self.emit_slice_expression(array, range)?;
                Ok(())
            }
            CheckedExpression::UnionLiteral {
                union_type,
                variant,
                arguments,
            } => {
                if let TypeKind::Union {
                    name: union_name, ..
                } = union_type
                {
                    write!(
                        self.out_file,
                        "({}){{.tag = {}, .{}.{}.tag = {}",
                        union_name, variant.tag, union_name, variant.name, variant.tag
                    )?;
                    for argument in arguments {
                        if let CheckedExpression::NamedArgument { name, argument } = argument {
                            write!(
                                self.out_file,
                                ", .{}.{}.{} = ",
                                union_name, variant.name, name
                            )?;
                            self.emit_expression(argument)?;
                        } else {
                            todo!("implicit union variant arguments")
                        }
                    }
                    write!(self.out_file, "}}")?;
                    Ok(())
                } else {
                    unreachable!()
                }
            }
            CheckedExpression::Cast {
                expression,
                destination,
            } => {
                write!(self.out_file, "({})", Self::get_c_type(destination))?;
                self.emit_expression(expression)?;
                Ok(())
            }
            CheckedExpression::Lambda { name, .. } => write!(self.out_file, "{}", name),
        }
    }

    fn emit_match_arm(
        &mut self,
        expression: &CheckedExpression,
        pattern: &CheckedPattern,
        body: &CheckedStatement,
    ) -> Result<(), io::Error> {
        match pattern {
            CheckedPattern::VariablePath { path, value } => {
                write!(self.out_file, "case ")?;
                if path.len() > 1 {
                    todo!("multi-segment paths")
                }
                match pattern.get_type() {
                    TypeKind::Struct { .. } => todo!(),
                    TypeKind::Enum { name, .. } => {
                        write!(self.out_file, "{}_{}", name, value.name)?
                    }
                    TypeKind::Union {
                        name: _union_name,
                        variants,
                    } => {
                        //TODO: need a better way of getting the tag out from a variable of type union but this works for now
                        for variant in variants {
                            if variant.name == value.name {
                                write!(self.out_file, "{}", variant.tag)?;
                                break;
                            }
                        }
                    }
                    _ => todo!("{}", pattern.get_type()),
                }
                write!(self.out_file, ": {{")?;
            }
            CheckedPattern::Wildcard(_) => {
                write!(self.out_file, "default: {{")?;
            }
            CheckedPattern::UnionVariantPath {
                path,
                struct_type,
                tag,
                arguments,
            } => {
                if path.len() > 1 {
                    todo!("multi-segment paths")
                }
                if let TypeKind::Struct { members, .. } = struct_type {
                    write!(self.out_file, "case {}: {{", tag)?;
                    //Declare destructured values
                    for (i, argument) in arguments.iter().enumerate() {
                        write!(
                            self.out_file,
                            "{} =",
                            self.get_c_variable_decl(
                                &argument.variable.name,
                                &argument.variable.type_kind,
                                argument.variable.mutable
                            )
                        )?;
                        self.emit_expression(expression)?;
                        writeln!(
                            self.out_file,
                            ".{}.{}.{};",
                            Self::get_c_type(&expression.get_type()),
                            Self::get_c_type(struct_type),
                            members.get(i).unwrap().variable.name, //TODO: not necessarily declared in the same order
                        )?;
                    }
                } else {
                    unreachable!("{:?}", struct_type)
                }
            }
        }
        self.emit_statement(body.clone())?;
        write!(self.out_file, "; break; }}")?;
        Ok(())
    }

    fn emit_binary_op(&mut self, op: &BinaryOperator) -> io::Result<()> {
        match op {
            BinaryOperator::Add => write!(self.out_file, "+"),
            BinaryOperator::Sub => write!(self.out_file, "-"),
            BinaryOperator::Mul => write!(self.out_file, "*"),
            BinaryOperator::Div => write!(self.out_file, "/"),
            BinaryOperator::Mod => write!(self.out_file, "%"),
            BinaryOperator::Gt => write!(self.out_file, ">"),
            BinaryOperator::GtEq => write!(self.out_file, ">="),
            BinaryOperator::Lt => write!(self.out_file, "<"),
            BinaryOperator::LtEq => write!(self.out_file, "<="),
            BinaryOperator::Eq => write!(self.out_file, "=="),
            BinaryOperator::NEq => write!(self.out_file, "!="),
            BinaryOperator::LogicalAnd => write!(self.out_file, "&&"),
            BinaryOperator::LogicalOr => write!(self.out_file, "||"),
            BinaryOperator::BitwiseAnd => write!(self.out_file, "&"),
            BinaryOperator::BitwiseOr => write!(self.out_file, "|"),
            BinaryOperator::Range => todo!(),
        }
    }

    fn emit_unary_op(&mut self, op: &UnaryOperator) -> io::Result<()> {
        match op {
            UnaryOperator::Ref => write!(self.out_file, "&"),
            UnaryOperator::Not => write!(self.out_file, "!"),
            UnaryOperator::Neg => write!(self.out_file, "-"),
        }
    }

    fn emit_slice_expression(
        &mut self,
        array: &CheckedExpression,
        range: &CheckedExpression,
    ) -> io::Result<()> {
        if let CheckedExpression::Binary {
            left: lower,
            op,
            right: upper,
        } = range
        {
            if op.op != BinaryOperator::Range {
                unreachable!("range is not a range");
            }
            match array.get_type() {
                TypeKind::Array { element_type, .. } => {
                    //(slice_i32){lower, upper - lower, &array}
                    write!(self.out_file, "(slice_{}){{", element_type)?;
                    self.emit_expression(lower)?;
                    write!(self.out_file, ", ")?;
                    self.emit_expression(upper)?;
                    write!(self.out_file, " - ")?;
                    self.emit_expression(lower)?;
                    write!(self.out_file, ", &")?;
                    self.emit_expression(array)?;
                    write!(self.out_file, "}}")?;
                }
                TypeKind::String => {
                    write!(self.out_file, "(slice_char){{")?;
                    self.emit_expression(lower)?;
                    write!(self.out_file, ", ")?;
                    self.emit_expression(upper)?;
                    write!(self.out_file, " - ")?;
                    self.emit_expression(lower)?;
                    write!(self.out_file, ", ")?;
                    self.emit_expression(array)?;
                    write!(self.out_file, "}}")?;
                }
                _ => todo!("emitting code for slicing {}", array.get_type()),
            }
        }
        Ok(())
    }
}
