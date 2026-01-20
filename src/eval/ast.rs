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
    Tuple(Vec<Expr>),
    TupleUnpack {
        targets: Vec<(String, Option<TypeAnnotation>)>,
        value: Box<Expr>,
    },
    /// Function call: callee(arg1, arg2, ...)
    FuncCall {
        callee: Box<Expr>,
        args: Vec<Expr>,
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
    /// While loop statement
    While { condition: Expr, body: Block },
    /// Function definition
    FuncDef {
        name: String,
        params: Vec<(String, Option<TypeAnnotation>)>,
        return_type: Option<TypeAnnotation>,
        body: Block,
    },
    /// Return statement
    Return(Option<Expr>),
}

/// Program is a collection of statements
#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}
