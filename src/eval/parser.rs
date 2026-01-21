use super::ast::{BinaryOp, Block, ConditionalBranch, Expr, Program, Statement, UnaryOp};
use super::token::Token;
use super::value::{TypeAnnotation, Value, MAX_TUPLE_ELEMENTS};

/// Parse a program from tokens
pub fn parse_program(tokens: &[Token]) -> Result<Program, String> {
    let mut parser = ProgramParser::new(tokens);
    parser.parse_program()
}

/// Token-based program parser for if statements
struct ProgramParser<'a> {
    tokens: &'a [Token],
    position: usize,
}

impl<'a> ProgramParser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    fn advance(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.position);
        if token.is_some() {
            self.position += 1;
        }
        token
    }

    fn expect(&mut self, expected: &Token) -> Result<(), String> {
        match self.peek() {
            Some(token) if token == expected => {
                self.advance();
                Ok(())
            }
            Some(token) => Err(format!("expected {:?}, got {:?}", expected, token)),
            None => Err(format!("expected {:?}, got end of input", expected)),
        }
    }

    fn skip_newlines(&mut self) {
        while let Some(Token::Newline) = self.peek() {
            self.advance();
        }
    }

    fn parse_program(&mut self) -> Result<Program, String> {
        let mut statements = Vec::new();

        self.skip_newlines();

        while let Some(token) = self.peek() {
            if *token == Token::Eof {
                break;
            }

            let stmt = self.parse_statement()?;
            statements.push(stmt);

            self.skip_newlines();
        }

        Ok(Program { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.peek() {
            Some(Token::If) => self.parse_if_statement(),
            Some(Token::While) => self.parse_while_statement(),
            Some(Token::Fn) => self.parse_function_def(),
            Some(Token::Return) => self.parse_return_statement(),
            _ => {
                let expr = self.parse_expression()?;
                // Consume optional newline after expression
                if let Some(Token::Newline) = self.peek() {
                    self.advance();
                }
                Ok(Statement::Expression(expr))
            }
        }
    }

    fn parse_function_def(&mut self) -> Result<Statement, String> {
        self.expect(&Token::Fn)?;

        // Parse function name
        let name = match self.peek() {
            Some(Token::Ident(n)) => {
                let n = n.clone();
                self.advance();
                n
            }
            _ => return Err("expected function name".to_string()),
        };

        // Parse parameters
        self.expect(&Token::LParen)?;
        let params = self.parse_param_list()?;
        self.expect(&Token::RParen)?;

        // Parse optional return type
        let return_type = if let Some(Token::Arrow) = self.peek() {
            self.advance();
            self.parse_type_annotation()?
        } else {
            None
        };

        // Parse function body
        self.expect(&Token::Colon)?;
        self.expect(&Token::Newline)?;
        let body = self.parse_block()?;

        Ok(Statement::FuncDef {
            name,
            params,
            return_type,
            body,
        })
    }

    fn parse_param_list(&mut self) -> Result<Vec<(String, Option<TypeAnnotation>)>, String> {
        let mut params = Vec::new();

        // Check for empty parameter list
        if let Some(Token::RParen) = self.peek() {
            return Ok(params);
        }

        // Parse first parameter
        let first_param = self.parse_param()?;
        params.push(first_param);

        // Parse remaining parameters
        while let Some(Token::Comma) = self.peek() {
            self.advance();
            let param = self.parse_param()?;
            params.push(param);
        }

        Ok(params)
    }

    fn parse_param(&mut self) -> Result<(String, Option<TypeAnnotation>), String> {
        // Parse parameter name
        let name = match self.peek() {
            Some(Token::Ident(n)) => {
                let n = n.clone();
                self.advance();
                n
            }
            _ => return Err("expected parameter name".to_string()),
        };

        // Parse optional type annotation
        let type_annotation = if let Some(Token::Colon) = self.peek() {
            self.advance();
            self.parse_type_annotation()?
        } else {
            None
        };

        Ok((name, type_annotation))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.expect(&Token::Return)?;

        // Check if there's an expression to return
        let expr = match self.peek() {
            Some(Token::Newline) | Some(Token::Eof) | Some(Token::Dedent) | None => None,
            _ => Some(self.parse_expression()?),
        };

        // Consume optional newline
        if let Some(Token::Newline) = self.peek() {
            self.advance();
        }

        Ok(Statement::Return(expr))
    }

    fn parse_arg_list(&mut self) -> Result<Vec<Expr>, String> {
        let mut args = Vec::new();

        // Check for empty argument list
        if let Some(Token::RParen) = self.peek() {
            return Ok(args);
        }

        // Parse first argument (use parse_comparison to avoid tuple unpacking)
        let first_arg = self.parse_comparison()?;
        args.push(first_arg);

        // Parse remaining arguments
        while let Some(Token::Comma) = self.peek() {
            self.advance();
            let arg = self.parse_comparison()?;
            args.push(arg);
        }

        Ok(args)
    }

    fn parse_if_statement(&mut self) -> Result<Statement, String> {
        // Parse if branch
        self.expect(&Token::If)?;
        let if_condition = self.parse_expression()?;
        self.expect(&Token::Colon)?;
        self.expect(&Token::Newline)?;
        let if_body = self.parse_block()?;

        let if_branch = ConditionalBranch {
            condition: if_condition,
            body: if_body,
        };

        // Parse elif branches
        let mut elif_branches = Vec::new();
        while let Some(Token::Elif) = self.peek() {
            self.advance();
            let condition = self.parse_expression()?;
            self.expect(&Token::Colon)?;
            self.expect(&Token::Newline)?;
            let body = self.parse_block()?;
            elif_branches.push(ConditionalBranch { condition, body });
        }

        // Parse optional else
        let else_body = if let Some(Token::Else) = self.peek() {
            self.advance();
            self.expect(&Token::Colon)?;
            self.expect(&Token::Newline)?;
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(Statement::If {
            if_branch,
            elif_branches,
            else_body,
        })
    }

    fn parse_while_statement(&mut self) -> Result<Statement, String> {
        self.expect(&Token::While)?;
        let condition = self.parse_expression()?;
        self.expect(&Token::Colon)?;
        self.expect(&Token::Newline)?;
        let body = self.parse_block()?;

        Ok(Statement::While { condition, body })
    }

    fn parse_block(&mut self) -> Result<Block, String> {
        self.expect(&Token::Indent)?;

        let mut statements = Vec::new();

        loop {
            // Check for dedent or EOF
            match self.peek() {
                Some(Token::Dedent) | Some(Token::Eof) | None => break,
                Some(Token::Newline) => {
                    self.advance();
                    continue;
                }
                _ => {}
            }

            let stmt = self.parse_statement()?;
            statements.push(stmt);
        }

        // Consume dedent if present
        if let Some(Token::Dedent) = self.peek() {
            self.advance();
        }

        Ok(Block { statements })
    }

    fn parse_expression(&mut self) -> Result<Expr, String> {
        self.parse_assignment()
    }

    /// Parse a type annotation, including union types (e.g., int | float)
    fn parse_type_annotation(&mut self) -> Result<Option<TypeAnnotation>, String> {
        let first = self.parse_type_primary()?;
        match first {
            None => Ok(None),
            Some(first_type) => {
                if let Some(Token::Pipe) = self.peek() {
                    let mut types = vec![first_type];
                    while let Some(Token::Pipe) = self.peek() {
                        self.advance();
                        match self.parse_type_primary()? {
                            Some(t) => types.push(t),
                            None => return Err("expected type after '|'".to_string()),
                        }
                    }
                    let normalized = Self::normalize_union(types);
                    if normalized.len() == 1 {
                        Ok(Some(normalized.into_iter().next().unwrap()))
                    } else {
                        Ok(Some(TypeAnnotation::Union(normalized)))
                    }
                } else {
                    Ok(Some(first_type))
                }
            }
        }
    }

    /// Parse a primary type (basic type, literal type, tuple type, or function signature)
    fn parse_type_primary(&mut self) -> Result<Option<TypeAnnotation>, String> {
        match self.peek() {
            Some(Token::IntType) => {
                self.advance();
                Ok(Some(TypeAnnotation::Int))
            }
            Some(Token::FloatType) => {
                self.advance();
                Ok(Some(TypeAnnotation::Float))
            }
            Some(Token::BoolType) => {
                self.advance();
                Ok(Some(TypeAnnotation::Bool))
            }
            Some(Token::StrType) => {
                self.advance();
                Ok(Some(TypeAnnotation::Str))
            }
            Some(Token::AnyType) => {
                self.advance();
                Ok(Some(TypeAnnotation::Any))
            }
            Some(Token::FuncType) => {
                self.advance();
                // Check if there's a signature: func(int, int) -> int
                if let Some(Token::LParen) = self.peek() {
                    self.advance(); // consume '('

                    // Parse parameter types
                    let mut param_types = Vec::new();
                    if !matches!(self.peek(), Some(Token::RParen)) {
                        // Parse first parameter type
                        if let Some(first_type) = self.parse_type_annotation()? {
                            param_types.push(first_type);

                            // Parse remaining parameter types
                            while let Some(Token::Comma) = self.peek() {
                                self.advance();
                                if let Some(next_type) = self.parse_type_annotation()? {
                                    param_types.push(next_type);
                                } else {
                                    return Err("expected type after comma in function signature"
                                        .to_string());
                                }
                            }
                        } else {
                            return Err("expected type in function signature".to_string());
                        }
                    }

                    self.expect(&Token::RParen)?;

                    // Parse optional return type
                    let return_type = if let Some(Token::Arrow) = self.peek() {
                        self.advance();
                        if let Some(ret_type) = self.parse_type_annotation()? {
                            Some(Box::new(ret_type))
                        } else {
                            return Err("expected return type after '->'".to_string());
                        }
                    } else {
                        None
                    };

                    Ok(Some(TypeAnnotation::FuncSig {
                        params: param_types,
                        return_type,
                    }))
                } else {
                    // Just `func` without signature - backward compatible
                    Ok(Some(TypeAnnotation::Func))
                }
            }
            // Literal type: integer
            Some(Token::Number(Value::Int(n))) => {
                let n = *n;
                self.advance();
                Ok(Some(TypeAnnotation::LiteralInt(n)))
            }
            // Literal type: boolean
            Some(Token::True) => {
                self.advance();
                Ok(Some(TypeAnnotation::LiteralBool(true)))
            }
            Some(Token::False) => {
                self.advance();
                Ok(Some(TypeAnnotation::LiteralBool(false)))
            }
            // Literal type: string
            Some(Token::StringLiteral(s)) => {
                let s = s.clone();
                self.advance();
                Ok(Some(TypeAnnotation::LiteralStr(s)))
            }
            Some(Token::LParen) => {
                // Tuple type annotation: (int, float, ...)
                self.advance();
                let mut types = Vec::new();

                // Parse first type
                if let Some(first_type) = self.parse_type_annotation()? {
                    types.push(first_type);

                    // Parse remaining types
                    while let Some(Token::Comma) = self.peek() {
                        self.advance();
                        if let Some(next_type) = self.parse_type_annotation()? {
                            types.push(next_type);
                        } else {
                            return Err("expected type after comma in tuple type".to_string());
                        }
                    }
                }

                self.expect(&Token::RParen)?;

                if types.len() < 2 {
                    return Err("tuple type must have at least 2 elements".to_string());
                }
                if types.len() > MAX_TUPLE_ELEMENTS {
                    return Err(format!(
                        "tuple type cannot have more than {} elements",
                        MAX_TUPLE_ELEMENTS
                    ));
                }

                Ok(Some(TypeAnnotation::Tuple(types)))
            }
            _ => Ok(None),
        }
    }

    /// Normalize a union type by flattening nested unions and removing duplicates
    fn normalize_union(types: Vec<TypeAnnotation>) -> Vec<TypeAnnotation> {
        let mut result = Vec::new();
        for t in types {
            match t {
                TypeAnnotation::Union(inner) => {
                    for inner_t in inner {
                        if !result.contains(&inner_t) {
                            result.push(inner_t);
                        }
                    }
                }
                _ => {
                    if !result.contains(&t) {
                        result.push(t);
                    }
                }
            }
        }
        result
    }

    fn parse_assignment(&mut self) -> Result<Expr, String> {
        let expr = self.parse_or()?;

        // Check for tuple unpacking: a, b = (1, 2) or a, b, c = (1, 2, 3)
        if let Expr::Variable(first_name) = &expr {
            if let Some(Token::Comma) = self.peek() {
                // This might be tuple unpacking
                let mut targets = vec![(first_name.clone(), None)];

                while let Some(Token::Comma) = self.peek() {
                    self.advance(); // consume comma

                    // Parse next identifier
                    match self.peek() {
                        Some(Token::Ident(name)) => {
                            let name = name.clone();
                            self.advance();
                            targets.push((name, None));
                        }
                        _ => return Err("expected identifier in tuple unpacking".to_string()),
                    }
                }

                // Check for assignment
                if let Some(Token::Equal) = self.peek() {
                    self.advance();
                    let value = self.parse_assignment()?;
                    return Ok(Expr::TupleUnpack {
                        targets,
                        value: Box::new(value),
                    });
                } else {
                    return Err("expected '=' in tuple unpacking".to_string());
                }
            }
        }

        // Check for type annotation pattern: ident: type = value
        if let Expr::Variable(name) = &expr {
            if let Some(Token::Colon) = self.peek() {
                // Check if next token is a type keyword or literal type before consuming ':'
                let is_type_annotation = self.tokens.get(self.position + 1).is_some_and(|t| {
                    matches!(
                        t,
                        Token::IntType
                            | Token::FloatType
                            | Token::BoolType
                            | Token::StrType
                            | Token::AnyType
                            | Token::FuncType
                            | Token::LParen
                            // Literal types for union type support
                            | Token::Number(Value::Int(_))
                            | Token::True
                            | Token::False
                            | Token::StringLiteral(_)
                    )
                });
                if is_type_annotation {
                    self.advance(); // consume ':'
                    let type_annotation = self.parse_type_annotation()?;
                    self.expect(&Token::Equal)?;
                    let value = self.parse_assignment()?;
                    return Ok(Expr::Assign {
                        name: name.clone(),
                        type_annotation,
                        value: Box::new(value),
                    });
                }
            }
        }

        if let Some(Token::Equal) = self.peek() {
            // Check if lhs is a variable
            if let Expr::Variable(name) = expr {
                self.advance();
                let value = self.parse_assignment()?;
                return Ok(Expr::Assign {
                    name,
                    type_annotation: None,
                    value: Box::new(value),
                });
            }
            // If not a variable, just return the expression
        }

        Ok(expr)
    }

    fn parse_or(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_and()?;
        while let Some(Token::Or) = self.peek() {
            self.advance();
            let right = self.parse_and()?;
            left = Expr::BinaryOp {
                op: BinaryOp::Or,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_and(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_not()?;
        while let Some(Token::And) = self.peek() {
            self.advance();
            let right = self.parse_not()?;
            left = Expr::BinaryOp {
                op: BinaryOp::And,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_not(&mut self) -> Result<Expr, String> {
        if let Some(Token::Not) = self.peek() {
            self.advance();
            let operand = self.parse_not()?; // 右結合（not not x のため）
            return Ok(Expr::UnaryOp {
                op: UnaryOp::Not,
                operand: Box::new(operand),
            });
        }
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_additive()?;

        loop {
            let op = match self.peek() {
                Some(Token::EqualEqual) => BinaryOp::Equal,
                Some(Token::NotEqual) => BinaryOp::NotEqual,
                Some(Token::Less) => BinaryOp::LessThan,
                Some(Token::Greater) => BinaryOp::GreaterThan,
                Some(Token::LessEqual) => BinaryOp::LessThanOrEqual,
                Some(Token::GreaterEqual) => BinaryOp::GreaterThanOrEqual,
                _ => break,
            };
            self.advance();
            let right = self.parse_additive()?;
            left = Expr::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_multiplicative()?;

        loop {
            let op = match self.peek() {
                Some(Token::Plus) => BinaryOp::Add,
                Some(Token::Minus) => BinaryOp::Subtract,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative()?;
            left = Expr::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_power()?;

        loop {
            let op = match self.peek() {
                Some(Token::Star) => BinaryOp::Multiply,
                Some(Token::Slash) => BinaryOp::Divide,
                Some(Token::Percent) => BinaryOp::Modulo,
                Some(Token::SlashSlash) => BinaryOp::FloorDivide,
                _ => break,
            };
            self.advance();
            let right = self.parse_power()?;
            left = Expr::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_power(&mut self) -> Result<Expr, String> {
        let base = self.parse_primary()?;

        if let Some(Token::StarStar) = self.peek() {
            self.advance();
            // Right-associative
            let exponent = self.parse_power()?;
            return Ok(Expr::BinaryOp {
                op: BinaryOp::Power,
                left: Box::new(base),
                right: Box::new(exponent),
            });
        }

        Ok(base)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.peek() {
            Some(Token::Minus) => {
                // Unary minus
                self.advance();
                let expr = self.parse_primary()?;
                Ok(Expr::BinaryOp {
                    op: BinaryOp::Subtract,
                    left: Box::new(Expr::Number(Value::Int(0))),
                    right: Box::new(expr),
                })
            }
            Some(Token::Number(value)) => {
                let value = value.clone();
                self.advance();
                Ok(Expr::Number(value))
            }
            Some(Token::StringLiteral(s)) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::Number(Value::Str(s)))
            }
            Some(Token::True) => {
                self.advance();
                Ok(Expr::Number(Value::Bool(true)))
            }
            Some(Token::False) => {
                self.advance();
                Ok(Expr::Number(Value::Bool(false)))
            }
            Some(Token::Ident(name)) => {
                let name = name.clone();
                self.advance();
                let mut expr = Expr::Variable(name);

                // Check for function call (possibly chained)
                while let Some(Token::LParen) = self.peek() {
                    self.advance();
                    let args = self.parse_arg_list()?;
                    self.expect(&Token::RParen)?;
                    expr = Expr::FuncCall {
                        callee: Box::new(expr),
                        args,
                    };
                }

                Ok(expr)
            }
            Some(Token::LParen) => {
                self.advance();
                let first_expr = self.parse_expression()?;

                // Check if this is a tuple
                if let Some(Token::Comma) = self.peek() {
                    let mut elements = vec![first_expr];

                    while let Some(Token::Comma) = self.peek() {
                        self.advance(); // consume comma
                        let next_expr = self.parse_expression()?;
                        elements.push(next_expr);
                    }

                    self.expect(&Token::RParen)?;

                    if elements.len() < 2 {
                        return Err("tuple must have at least 2 elements".to_string());
                    }
                    if elements.len() > MAX_TUPLE_ELEMENTS {
                        return Err(format!(
                            "tuple cannot have more than {} elements",
                            MAX_TUPLE_ELEMENTS
                        ));
                    }

                    return Ok(Expr::Tuple(elements));
                }

                // Not a tuple, just a grouped expression
                self.expect(&Token::RParen)?;
                Ok(first_expr)
            }
            Some(token) => Err(format!("unexpected token: {:?}", token)),
            None => Err("unexpected end of input".to_string()),
        }
    }
}
