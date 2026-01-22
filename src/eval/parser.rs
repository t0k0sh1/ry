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

    /// Peek at the next token (one position ahead of current)
    fn peek_next(&self) -> Option<&Token> {
        self.tokens.get(self.position + 1)
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
            Some(Token::Ident(_)) => {
                // Check if this is Command Syntax: identifier followed by value
                // Not command syntax if next token is: =, :, (, ., or binary operators
                match self.peek_next() {
                    // Normal expression: assignment, type annotation, function call, method call
                    Some(Token::Equal) | Some(Token::Colon) | Some(Token::LParen)
                    | Some(Token::Dot) | Some(Token::Comma) => self.parse_expression_statement(),
                    // Binary operators (except Minus which could be unary) - normal expression
                    Some(Token::Plus)
                    | Some(Token::Star)
                    | Some(Token::Slash)
                    | Some(Token::Percent)
                    | Some(Token::StarStar)
                    | Some(Token::SlashSlash)
                    | Some(Token::EqualEqual)
                    | Some(Token::NotEqual)
                    | Some(Token::Less)
                    | Some(Token::Greater)
                    | Some(Token::LessEqual)
                    | Some(Token::GreaterEqual)
                    | Some(Token::And)
                    | Some(Token::Or) => self.parse_expression_statement(),
                    // Minus after identifier is always treated as binary subtraction
                    // e.g., x - y → subtract, a - 5 → subtract
                    // To pass negative numbers in command syntax, use parentheses: add (-5), 10
                    Some(Token::Minus) => self.parse_expression_statement(),
                    // End of statement - just a variable expression
                    Some(Token::Newline) | Some(Token::Eof) | Some(Token::Dedent) | None => {
                        self.parse_expression_statement()
                    }
                    // Command Syntax: identifier followed by a value
                    // e.g., add 10, 20 or print "hello"
                    _ => self.parse_command_call(),
                }
            }
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let expr = self.parse_expression()?;
        // Consume optional newline after expression
        if let Some(Token::Newline) = self.peek() {
            self.advance();
        }
        Ok(Statement::Expression(expr))
    }

    /// Parse Command Syntax: identifier arg1, arg2, ...
    /// Examples:
    ///   add 10, 20     -> add(10, 20)
    ///   print "hello"  -> print("hello")
    ///   square 5       -> square(5)
    fn parse_command_call(&mut self) -> Result<Statement, String> {
        let name = match self.peek() {
            Some(Token::Ident(n)) => {
                let n = n.clone();
                self.advance();
                n
            }
            _ => return Err("expected function name".to_string()),
        };

        let args = self.parse_command_args()?;

        // Consume optional newline
        if let Some(Token::Newline) = self.peek() {
            self.advance();
        }

        Ok(Statement::Expression(Expr::FuncCall {
            callee: Box::new(Expr::Variable(name)),
            args,
        }))
    }

    /// Parse command arguments (comma-separated expressions)
    fn parse_command_args(&mut self) -> Result<Vec<Expr>, String> {
        // Parse first argument
        let first = self.parse_comparison()?;
        let mut args = vec![first];

        // Parse remaining arguments
        while let Some(Token::Comma) = self.peek() {
            self.advance();
            let arg = self.parse_comparison()?;
            args.push(arg);
        }

        Ok(args)
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
        let base = self.parse_postfix()?;

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

    /// Parse postfix operations: method calls (UFCS) and function calls
    /// Examples:
    ///   10.add(20)       -> add(10, 20)
    ///   (1 + 2).double() -> double(1 + 2)
    ///   foo()()          -> chained function calls
    fn parse_postfix(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary()?;

        loop {
            match self.peek() {
                Some(Token::Dot) => {
                    self.advance();
                    // Parse method name
                    let method_name = match self.peek() {
                        Some(Token::Ident(name)) => {
                            let name = name.clone();
                            self.advance();
                            name
                        }
                        _ => return Err("expected method name after '.'".to_string()),
                    };

                    // Expect '(' for method call
                    self.expect(&Token::LParen)?;

                    // Parse arguments (receiver is the first argument)
                    let mut args = vec![expr];
                    args.extend(self.parse_arg_list()?);
                    self.expect(&Token::RParen)?;

                    expr = Expr::FuncCall {
                        callee: Box::new(Expr::Variable(method_name)),
                        args,
                    };
                }
                Some(Token::LParen) => {
                    // Regular function call on expression result
                    self.advance();
                    let args = self.parse_arg_list()?;
                    self.expect(&Token::RParen)?;
                    expr = Expr::FuncCall {
                        callee: Box::new(expr),
                        args,
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.peek() {
            Some(Token::Minus) => {
                // Unary minus
                self.advance();
                // Parse power so that `**` has higher precedence than unary minus
                let expr = self.parse_power()?;
                Ok(Expr::UnaryOp {
                    op: UnaryOp::Neg,
                    operand: Box::new(expr),
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
                // Function calls are handled by parse_postfix()
                Ok(Expr::Variable(name))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::lexer::Lexer;

    fn parse_expr(input: &str) -> Result<Expr, String> {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
        let mut parser = ProgramParser::new(&tokens);
        parser.parse_expression()
    }

    fn parse(input: &str) -> Result<Program, String> {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
        parse_program(&tokens)
    }

    // ========================================
    // Operator Precedence Tests
    // ========================================

    #[test]
    fn test_precedence_additive_over_comparison() {
        // 1 + 2 < 3 + 4 should parse as (1 + 2) < (3 + 4)
        let expr = parse_expr("1 + 2 < 3 + 4").unwrap();
        match expr {
            Expr::BinaryOp {
                op: BinaryOp::LessThan,
                left,
                right,
            } => {
                assert!(matches!(
                    left.as_ref(),
                    Expr::BinaryOp {
                        op: BinaryOp::Add,
                        ..
                    }
                ));
                assert!(matches!(
                    right.as_ref(),
                    Expr::BinaryOp {
                        op: BinaryOp::Add,
                        ..
                    }
                ));
            }
            _ => panic!("Expected comparison at top level, got {:?}", expr),
        }
    }

    #[test]
    fn test_precedence_multiplicative_over_additive() {
        // 1 + 2 * 3 should parse as 1 + (2 * 3)
        let expr = parse_expr("1 + 2 * 3").unwrap();
        match expr {
            Expr::BinaryOp {
                op: BinaryOp::Add,
                left,
                right,
            } => {
                assert!(matches!(left.as_ref(), Expr::Number(_)));
                assert!(matches!(
                    right.as_ref(),
                    Expr::BinaryOp {
                        op: BinaryOp::Multiply,
                        ..
                    }
                ));
            }
            _ => panic!("Expected addition at top level, got {:?}", expr),
        }
    }

    #[test]
    fn test_precedence_power_over_multiplicative() {
        // 2 * 3 ** 4 should parse as 2 * (3 ** 4)
        let expr = parse_expr("2 * 3 ** 4").unwrap();
        match expr {
            Expr::BinaryOp {
                op: BinaryOp::Multiply,
                left,
                right,
            } => {
                assert!(matches!(left.as_ref(), Expr::Number(_)));
                assert!(matches!(
                    right.as_ref(),
                    Expr::BinaryOp {
                        op: BinaryOp::Power,
                        ..
                    }
                ));
            }
            _ => panic!("Expected multiplication at top level, got {:?}", expr),
        }
    }

    #[test]
    fn test_precedence_unary_minus_with_power() {
        // -2 ** 3 should parse as -(2 ** 3)
        let expr = parse_expr("-2 ** 3").unwrap();
        match expr {
            Expr::UnaryOp {
                op: UnaryOp::Neg,
                operand,
            } => {
                assert!(matches!(
                    operand.as_ref(),
                    Expr::BinaryOp {
                        op: BinaryOp::Power,
                        ..
                    }
                ));
            }
            _ => panic!("Expected unary minus at top level, got {:?}", expr),
        }
    }

    // ========================================
    // Associativity Tests
    // ========================================

    #[test]
    fn test_power_right_associative() {
        // 2 ** 3 ** 4 should parse as 2 ** (3 ** 4)
        let expr = parse_expr("2 ** 3 ** 4").unwrap();
        match expr {
            Expr::BinaryOp {
                op: BinaryOp::Power,
                left,
                right,
            } => {
                assert!(matches!(left.as_ref(), Expr::Number(_)));
                // Right side should be another power expression
                assert!(matches!(
                    right.as_ref(),
                    Expr::BinaryOp {
                        op: BinaryOp::Power,
                        ..
                    }
                ));
            }
            _ => panic!("Expected power at top level, got {:?}", expr),
        }
    }

    #[test]
    fn test_subtraction_left_associative() {
        // 10 - 3 - 2 should parse as (10 - 3) - 2
        let expr = parse_expr("10 - 3 - 2").unwrap();
        match expr {
            Expr::BinaryOp {
                op: BinaryOp::Subtract,
                left,
                right,
            } => {
                // Right should be just a number (2)
                assert!(matches!(right.as_ref(), Expr::Number(_)));
                // Left should be another subtraction
                assert!(matches!(
                    left.as_ref(),
                    Expr::BinaryOp {
                        op: BinaryOp::Subtract,
                        ..
                    }
                ));
            }
            _ => panic!("Expected subtraction at top level, got {:?}", expr),
        }
    }

    // ========================================
    // Unary Operator Tests
    // ========================================

    #[test]
    fn test_unary_minus_simple() {
        let expr = parse_expr("-5").unwrap();
        match expr {
            Expr::UnaryOp {
                op: UnaryOp::Neg,
                operand,
            } => {
                assert!(matches!(operand.as_ref(), Expr::Number(_)));
            }
            _ => panic!("Expected unary minus, got {:?}", expr),
        }
    }

    #[test]
    fn test_unary_not_simple() {
        let expr = parse_expr("not true").unwrap();
        match expr {
            Expr::UnaryOp {
                op: UnaryOp::Not,
                operand,
            } => {
                assert!(matches!(operand.as_ref(), Expr::Number(_)));
            }
            _ => panic!("Expected unary not, got {:?}", expr),
        }
    }

    #[test]
    fn test_double_negation() {
        let expr = parse_expr("--5").unwrap();
        match expr {
            Expr::UnaryOp {
                op: UnaryOp::Neg,
                operand,
            } => {
                assert!(matches!(
                    operand.as_ref(),
                    Expr::UnaryOp {
                        op: UnaryOp::Neg,
                        ..
                    }
                ));
            }
            _ => panic!("Expected nested unary minus, got {:?}", expr),
        }
    }

    // ========================================
    // Statement Parsing Tests
    // ========================================

    #[test]
    fn test_parse_if_statement() {
        let program = parse("if x > 0:\n    y = 1\n").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::If {
                if_branch,
                elif_branches,
                else_body,
            } => {
                // if_branch contains the condition
                assert!(matches!(&if_branch.condition, Expr::BinaryOp { .. }));
                assert!(elif_branches.is_empty());
                assert!(else_body.is_none());
            }
            _ => panic!("Expected if statement"),
        }
    }

    #[test]
    fn test_parse_if_else_statement() {
        let program = parse("if x > 0:\n    y = 1\nelse:\n    y = 2\n").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::If {
                elif_branches,
                else_body,
                ..
            } => {
                assert!(elif_branches.is_empty());
                assert!(else_body.is_some());
            }
            _ => panic!("Expected if-else statement"),
        }
    }

    #[test]
    fn test_parse_if_elif_else_statement() {
        let program =
            parse("if x > 0:\n    y = 1\nelif x < 0:\n    y = -1\nelse:\n    y = 0\n").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::If {
                elif_branches,
                else_body,
                ..
            } => {
                assert_eq!(elif_branches.len(), 1); // 1 elif branch
                assert!(else_body.is_some());
            }
            _ => panic!("Expected if-elif-else statement"),
        }
    }

    #[test]
    fn test_parse_while_statement() {
        let program = parse("while x > 0:\n    x = x - 1\n").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(&program.statements[0], Statement::While { .. }));
    }

    #[test]
    fn test_parse_function_definition() {
        let program = parse("fn add(a, b):\n    return a + b\n").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::FuncDef { name, params, .. } => {
                assert_eq!(name, "add");
                assert_eq!(params.len(), 2);
            }
            _ => panic!("Expected function definition"),
        }
    }

    #[test]
    fn test_parse_typed_function() {
        let program = parse("fn add(a: int, b: int) -> int:\n    return a + b\n").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::FuncDef {
                name,
                params,
                return_type,
                ..
            } => {
                assert_eq!(name, "add");
                assert_eq!(params.len(), 2);
                assert!(return_type.is_some());
            }
            _ => panic!("Expected function definition"),
        }
    }

    #[test]
    fn test_parse_return_statement() {
        let program = parse("fn foo():\n    return 42\n").unwrap();
        match &program.statements[0] {
            Statement::FuncDef { body, .. } => {
                assert_eq!(body.statements.len(), 1);
                assert!(matches!(&body.statements[0], Statement::Return(_)));
            }
            _ => panic!("Expected function definition"),
        }
    }

    // ========================================
    // Error Cases
    // ========================================

    #[test]
    fn test_error_unexpected_token() {
        let result = parse_expr("1 + + 2");
        assert!(result.is_err());
    }

    #[test]
    fn test_error_unclosed_paren() {
        let result = parse_expr("(1 + 2");
        assert!(result.is_err());
    }

    #[test]
    fn test_error_empty_input() {
        let result = parse_expr("");
        assert!(result.is_err());
    }

    // ========================================
    // Tuple Parsing Tests
    // ========================================

    #[test]
    fn test_parse_tuple() {
        let expr = parse_expr("(1, 2, 3)").unwrap();
        match expr {
            Expr::Tuple(elements) => {
                assert_eq!(elements.len(), 3);
            }
            _ => panic!("Expected tuple, got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_parenthesized_not_tuple() {
        let expr = parse_expr("(1 + 2)").unwrap();
        // Should NOT be a tuple, just a grouped expression
        assert!(matches!(
            expr,
            Expr::BinaryOp {
                op: BinaryOp::Add,
                ..
            }
        ));
    }

    // ========================================
    // UFCS (Uniform Function Call Syntax) Tests
    // ========================================

    #[test]
    fn test_ufcs_basic() {
        // 10.add(20) -> FuncCall { callee: Variable("add"), args: [Number(10), Number(20)] }
        let expr = parse_expr("10.add(20)").unwrap();
        match expr {
            Expr::FuncCall { callee, args } => {
                assert!(matches!(callee.as_ref(), Expr::Variable(name) if name == "add"));
                assert_eq!(args.len(), 2);
                assert!(matches!(&args[0], Expr::Number(Value::Int(10))));
                assert!(matches!(&args[1], Expr::Number(Value::Int(20))));
            }
            _ => panic!("Expected function call, got {:?}", expr),
        }
    }

    #[test]
    fn test_ufcs_chained() {
        // 10.add(5).mul(2) -> mul(add(10, 5), 2)
        let expr = parse_expr("10.add(5).mul(2)").unwrap();
        match expr {
            Expr::FuncCall { callee, args } => {
                // Outer call should be mul
                assert!(matches!(callee.as_ref(), Expr::Variable(name) if name == "mul"));
                assert_eq!(args.len(), 2);

                // First argument should be the result of add(10, 5)
                match &args[0] {
                    Expr::FuncCall {
                        callee,
                        args: inner_args,
                    } => {
                        assert!(matches!(callee.as_ref(), Expr::Variable(name) if name == "add"));
                        assert_eq!(inner_args.len(), 2);
                    }
                    _ => panic!("Expected nested function call"),
                }
            }
            _ => panic!("Expected function call, got {:?}", expr),
        }
    }

    #[test]
    fn test_ufcs_on_expression() {
        // (1 + 2).double() -> double(1 + 2)
        let expr = parse_expr("(1 + 2).double()").unwrap();
        match expr {
            Expr::FuncCall { callee, args } => {
                assert!(matches!(callee.as_ref(), Expr::Variable(name) if name == "double"));
                assert_eq!(args.len(), 1);
                assert!(matches!(
                    &args[0],
                    Expr::BinaryOp {
                        op: BinaryOp::Add,
                        ..
                    }
                ));
            }
            _ => panic!("Expected function call, got {:?}", expr),
        }
    }

    #[test]
    fn test_ufcs_with_variable() {
        // x.clamp(0, 10) -> clamp(x, 0, 10)
        let expr = parse_expr("x.clamp(0, 10)").unwrap();
        match expr {
            Expr::FuncCall { callee, args } => {
                assert!(matches!(callee.as_ref(), Expr::Variable(name) if name == "clamp"));
                assert_eq!(args.len(), 3);
                assert!(matches!(&args[0], Expr::Variable(name) if name == "x"));
            }
            _ => panic!("Expected function call, got {:?}", expr),
        }
    }

    #[test]
    fn test_float_literal_preserved() {
        // 10.5 should remain a float literal, not UFCS
        let expr = parse_expr("10.5").unwrap();
        assert!(matches!(expr, Expr::Number(Value::Float(f)) if (f - 10.5).abs() < f64::EPSILON));
    }

    #[test]
    fn test_ufcs_no_args() {
        // 42.to_string() -> to_string(42)
        let expr = parse_expr("42.to_string()").unwrap();
        match expr {
            Expr::FuncCall { callee, args } => {
                assert!(matches!(callee.as_ref(), Expr::Variable(name) if name == "to_string"));
                assert_eq!(args.len(), 1);
                assert!(matches!(&args[0], Expr::Number(Value::Int(42))));
            }
            _ => panic!("Expected function call, got {:?}", expr),
        }
    }

    // ========================================
    // Command Syntax Tests
    // ========================================

    #[test]
    fn test_command_basic() {
        // add 10, 20 -> add(10, 20)
        let program = parse("add 10, 20").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expr::FuncCall { callee, args }) => {
                assert!(matches!(callee.as_ref(), Expr::Variable(name) if name == "add"));
                assert_eq!(args.len(), 2);
            }
            _ => panic!("Expected function call expression"),
        }
    }

    #[test]
    fn test_command_single_arg() {
        // square 5 -> square(5)
        let program = parse("square 5").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expr::FuncCall { callee, args }) => {
                assert!(matches!(callee.as_ref(), Expr::Variable(name) if name == "square"));
                assert_eq!(args.len(), 1);
            }
            _ => panic!("Expected function call expression"),
        }
    }

    #[test]
    fn test_command_expression_args() {
        // add 10 + 5, 20 * 2 -> add(15, 40) (as expressions, not evaluated)
        let program = parse("add 10 + 5, 20 * 2").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expr::FuncCall { callee, args }) => {
                assert!(matches!(callee.as_ref(), Expr::Variable(name) if name == "add"));
                assert_eq!(args.len(), 2);
                // First arg should be 10 + 5
                assert!(matches!(
                    &args[0],
                    Expr::BinaryOp {
                        op: BinaryOp::Add,
                        ..
                    }
                ));
                // Second arg should be 20 * 2
                assert!(matches!(
                    &args[1],
                    Expr::BinaryOp {
                        op: BinaryOp::Multiply,
                        ..
                    }
                ));
            }
            _ => panic!("Expected function call expression"),
        }
    }

    #[test]
    fn test_command_string_arg() {
        // print "hello" -> print("hello")
        let program = parse("print \"hello\"").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expr::FuncCall { callee, args }) => {
                assert!(matches!(callee.as_ref(), Expr::Variable(name) if name == "print"));
                assert_eq!(args.len(), 1);
                assert!(matches!(&args[0], Expr::Number(Value::Str(s)) if s == "hello"));
            }
            _ => panic!("Expected function call expression"),
        }
    }

    #[test]
    fn test_negative_arg_requires_normal_call_syntax() {
        // For negative number arguments, use normal function call syntax: add(-5, 10)
        // Command syntax "add -5, 10" would be parsed as binary subtraction "add - 5, 10"
        let program = parse("add(-5, 10)").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expr::FuncCall { callee, args }) => {
                assert!(matches!(callee.as_ref(), Expr::Variable(name) if name == "add"));
                assert_eq!(args.len(), 2);
                // First arg should be unary minus
                assert!(matches!(
                    &args[0],
                    Expr::UnaryOp {
                        op: UnaryOp::Neg,
                        ..
                    }
                ));
            }
            _ => panic!("Expected function call expression"),
        }
    }

    #[test]
    fn test_regular_function_call_not_command() {
        // foo(1, 2) should remain a regular function call, not command syntax
        let program = parse("foo(1, 2)").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expr::FuncCall { callee, args }) => {
                assert!(matches!(callee.as_ref(), Expr::Variable(name) if name == "foo"));
                assert_eq!(args.len(), 2);
            }
            _ => panic!("Expected function call expression"),
        }
    }

    #[test]
    fn test_assignment_not_command() {
        // x = 10 should remain an assignment, not command syntax
        let program = parse("x = 10").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expr::Assign { name, .. }) => {
                assert_eq!(name, "x");
            }
            _ => panic!("Expected assignment expression"),
        }
    }

    #[test]
    fn test_variable_expression_not_command() {
        // Just "x" should remain a variable expression, not command syntax
        let program = parse("x").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expr::Variable(name)) => {
                assert_eq!(name, "x");
            }
            _ => panic!("Expected variable expression"),
        }
    }

    #[test]
    fn test_binary_subtraction_not_command() {
        // x - y should be parsed as binary subtraction, not as command syntax x(-y)
        let program = parse("x - y").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expr::BinaryOp { op, left, right }) => {
                assert!(matches!(op, BinaryOp::Subtract));
                assert!(matches!(left.as_ref(), Expr::Variable(name) if name == "x"));
                assert!(matches!(right.as_ref(), Expr::Variable(name) if name == "y"));
            }
            _ => panic!("Expected binary subtraction expression"),
        }
    }

    #[test]
    fn test_binary_subtraction_with_numbers_not_command() {
        // a - 5 should be parsed as binary subtraction, not as command syntax
        let program = parse("a - 5").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expr::BinaryOp { op, left, right }) => {
                assert!(matches!(op, BinaryOp::Subtract));
                assert!(matches!(left.as_ref(), Expr::Variable(name) if name == "a"));
                assert!(matches!(right.as_ref(), Expr::Number(Value::Int(5))));
            }
            _ => panic!("Expected binary subtraction expression"),
        }
    }
}
