use crate::{
    parser::{BinaryOperator},
    type_checker::{
        CheckedBinaryOp, CheckedExpression, CheckedStatement, CheckedVariable,
        TypeKind,
    },
};
use rand::{distributions::Alphanumeric, thread_rng, Rng};

pub struct Rewriter {}

impl Default for Rewriter {
    fn default() -> Self {
        Self::new()
    }
}

impl Rewriter {
    pub fn new() -> Rewriter {
        Rewriter {}
    }

    pub fn rewrite(&mut self, statements: Vec<CheckedStatement>) -> Vec<CheckedStatement> {
        let mut rewritten_statements = Vec::new();
        for statement in statements {
            for rewritten_statement in self.rewrite_statement(statement) {
                rewritten_statements.push(rewritten_statement);
            }
        }
        rewritten_statements
    }

    //TODO This doesn't quite work right
    fn flatten(&self, statement: CheckedStatement) -> CheckedStatement {
        match statement {
            CheckedStatement::Block { ref statements } => {
                if statements.len() == 1 {
                    let stmt = statements.first().unwrap();
                    if let CheckedStatement::Block { statements: _ } = &stmt {
                        return self.flatten(stmt.to_owned());
                    }
                }
                let mut flattened_statements = Vec::new();
                for statement in statements {
                    flattened_statements.push(self.flatten(statement.to_owned()));
                }
                CheckedStatement::Block {
                    statements: flattened_statements,
                }
            }
            _ => statement,
        }
    }

    fn rewrite_statement(&mut self, statement: CheckedStatement) -> Vec<CheckedStatement> {
        match statement {
            CheckedStatement::FunctionDeclaration { function, body } => {
                let rewritten_body = CheckedStatement::Block {
                    statements: self.rewrite_statement(*body),
                };
                let flattened_body = self.flatten(rewritten_body);

                vec![CheckedStatement::FunctionDeclaration {
                    function,
                    body: Box::new(flattened_body),
                }]
            }
            CheckedStatement::Block { statements } => {
                let mut rewritten_statements = Vec::new();
                for statement in statements {
                    for rewritten_statement in self.rewrite_statement(statement) {
                        rewritten_statements.push(rewritten_statement);
                    }
                }
                vec![CheckedStatement::Block {
                    statements: rewritten_statements,
                }]
            }
            CheckedStatement::If {
                condition,
                body,
                else_clause,
            } => {
                //cba to deal with rewriting the condition and else clause
                let rewritten_body = self.rewrite_statement(*body);
                // let rewritten_else_clause = match else_clause {
                //     Some(else_clause) => self.rewrite_statement(*else_clause),
                //     None => vec![],
                // };
                let flattened_body = self.flatten(CheckedStatement::Block {
                    statements: rewritten_body,
                });

                vec![CheckedStatement::If {
                    condition,
                    body: Box::new(flattened_body),
                    else_clause,
                }]
            }
            CheckedStatement::For {
                iterator,
                iterable,
                body,
            } => {
                //TODO: really all loops should be unified at this stage but I can't be bothered right now

                let rewritten_body = CheckedStatement::Block {
                    statements: self.rewrite_statement(*body),
                };
                let flattened_body = self.flatten(rewritten_body);

                vec![CheckedStatement::For {
                    iterator,
                    iterable,
                    body: Box::new(flattened_body),
                }]
            }
            CheckedStatement::Return { expression } => {
                match expression {
                    Some(expression) => vec![CheckedStatement::Return {
                        expression: Some(expression),
                    }],
                    None => vec![CheckedStatement::Return { expression: None }],
                }
            }
            CheckedStatement::Guard {
                condition,
                capture,
                body,
                condition_false,
            } => {
                if let TypeKind::Optional(inner_type) = condition.get_type() {
                    let tag_access = CheckedExpression::MemberAccess {
                        accessee: Some(Box::new(condition.clone())),
                        member: CheckedVariable {
                            name: "tag".to_string(),
                            type_kind: TypeKind::I16,
                            mutable: false,
                        },
                    };

                    let rewritten_body = match capture {
                        Some(captured_variable) => {
                            let capture_decl = CheckedExpression::VariableDeclaration {
                                variables: vec![captured_variable],
                                initialiser: Some(Box::new(CheckedExpression::UnwrapOptional {
                                    expression: Box::new(condition),
                                    type_kind: *inner_type,
                                })),
                            };

                            Box::new(CheckedStatement::Block {
                                statements: vec![
                                    CheckedStatement::Expression {
                                        expression: capture_decl,
                                    },
                                    *body.to_owned(),
                                ],
                            })
                        }
                        None => body.to_owned(),
                    };

                    
                    self.rewrite_statement(CheckedStatement::If {
                        condition: CheckedExpression::Binary {
                            left: Box::new(tag_access),
                            op: CheckedBinaryOp {
                                op: if condition_false {
                                    BinaryOperator::Eq
                                } else {
                                    BinaryOperator::NEq
                                },
                                return_type: TypeKind::Bool,
                            },
                            right: Box::new(CheckedExpression::IntLiteral {
                                value: 0,
                                type_kind: TypeKind::I16,
                            }),
                        },
                        body: rewritten_body,
                        else_clause: None,
                    })
                } else {
                    vec![CheckedStatement::Guard {
                        condition,
                        capture,
                        body,
                        condition_false,
                    }]
                }
            }
            CheckedStatement::Expression { expression } => {
                self.rewrite_expression(expression)
            }
            _ => vec![statement],
        }
    }

    fn rewrite_expression(&mut self, expression: CheckedExpression) -> Vec<CheckedStatement> {
        match expression {
            CheckedExpression::VariableDeclaration {
                variables,
                initialiser,
            } => {
                //here's where a lot of rewriting happens since we assign to things that c can't do
                if let Some(initialiser) = initialiser {
                    match initialiser.as_ref() {
                        CheckedExpression::Guard {
                            expression,
                            body,
                            type_kind,
                        } => {
                            //turn
                            //foo := guard x else { body }; ...
                            //into
                            //tmp_var := x; if tmp_var.tag == 0 { body }; foo := tmp_var.Some.value; ...

                            let tmp_var = CheckedVariable {
                                name: format!("guard_var_{}", Self::random_string(16)),
                                type_kind: TypeKind::Optional(Box::new(type_kind.to_owned())),
                                mutable: false,
                            };

                            let mut tmp_var_decl =
                                self.rewrite_statement(CheckedStatement::Expression {
                                    expression: CheckedExpression::VariableDeclaration {
                                        variables: vec![tmp_var.to_owned()],
                                        initialiser: Some(expression.to_owned()),
                                    },
                                });

                            let tag_access = CheckedExpression::MemberAccess {
                                accessee: Some(Box::new(CheckedExpression::Variable(
                                    tmp_var.clone(),
                                ))),
                                member: CheckedVariable {
                                    name: "tag".to_string(),
                                    type_kind: TypeKind::I16,
                                    mutable: false,
                                },
                            };

                            let mut if_guard = self.rewrite_statement(CheckedStatement::If {
                                condition: CheckedExpression::Binary {
                                    left: Box::new(tag_access),
                                    op: CheckedBinaryOp {
                                        op: BinaryOperator::Eq,
                                        return_type: TypeKind::Bool,
                                    },
                                    right: Box::new(CheckedExpression::IntLiteral {
                                        value: 0,
                                        type_kind: TypeKind::I16,
                                    }),
                                },
                                body: body.to_owned(),
                                else_clause: None,
                            });

                            let mut variables_declaration =
                                self.rewrite_statement(CheckedStatement::Expression {
                                    expression: CheckedExpression::VariableDeclaration {
                                        variables,
                                        initialiser: Some(Box::new(
                                            CheckedExpression::UnwrapOptional {
                                                expression: Box::new(CheckedExpression::Variable(
                                                    tmp_var,
                                                )),
                                                type_kind: type_kind.to_owned(),
                                            },
                                        )),
                                    },
                                });

                            let mut statements = Vec::new();
                            statements.append(&mut tmp_var_decl);
                            statements.append(&mut if_guard);
                            statements.append(&mut variables_declaration);
                            statements
                        }
                        _ => {
                            vec![CheckedStatement::Expression {
                                expression: CheckedExpression::VariableDeclaration {
                                    variables,
                                    initialiser: Some(initialiser),
                                },
                            }]
                        }
                    }
                } else {
                    vec![CheckedStatement::Expression {
                        expression: CheckedExpression::VariableDeclaration {
                            variables,
                            initialiser,
                        },
                    }]
                }
            }
            _ => vec![CheckedStatement::Expression { expression }],
        }
    }

    fn random_string(n: usize) -> String {
        thread_rng()
            .sample_iter(&Alphanumeric)
            .take(n)
            .map(char::from) // From link above, this is needed in later versions
            .collect()
    }
}
