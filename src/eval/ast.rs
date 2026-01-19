use super::value::{TypeAnnotation, Value};

#[derive(Debug, Clone)]
pub enum Expr {
    Number(Value),
    Variable(String),
    Assign {
        name: String,
        type_annotation: Option<TypeAnnotation>,
        value: Box<Expr>,
    },
    BinaryOp {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
    FloorDivide,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

/// Block containing a list of statements (has its own scope)
#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

/// Conditional branch (condition + body)
#[derive(Debug, Clone)]
pub struct ConditionalBranch {
    pub condition: Expr,
    pub body: Block,
}

/// Statement types
#[derive(Debug, Clone)]
pub enum Statement {
    /// Expression statement
    Expression(Expr),
    /// If/elif/else statement
    If {
        if_branch: ConditionalBranch,
        elif_branches: Vec<ConditionalBranch>,
        else_body: Option<Block>,
    },
}

/// Program is a collection of statements
#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}
