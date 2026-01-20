use super::*;

#[test]
fn test_evaluate_addition() {
    assert_eq!(evaluate_expression("1+1").unwrap(), Value::Int(2));
    assert_eq!(evaluate_expression("1 + 1").unwrap(), Value::Int(2));
    assert_eq!(evaluate_expression("10+20").unwrap(), Value::Int(30));
}

#[test]
fn test_evaluate_subtraction() {
    assert_eq!(evaluate_expression("2-1").unwrap(), Value::Int(1));
    assert_eq!(evaluate_expression("2 - 1").unwrap(), Value::Int(1));
    assert_eq!(evaluate_expression("10-5").unwrap(), Value::Int(5));
}

#[test]
fn test_evaluate_multiplication() {
    assert_eq!(evaluate_expression("3*6").unwrap(), Value::Int(18));
    assert_eq!(evaluate_expression("3 * 6").unwrap(), Value::Int(18));
    assert_eq!(evaluate_expression("4*5").unwrap(), Value::Int(20));
}

#[test]
fn test_evaluate_division() {
    assert_eq!(evaluate_expression("5/10").unwrap(), Value::Float(0.5));
    assert_eq!(evaluate_expression("5 / 10").unwrap(), Value::Float(0.5));
    assert_eq!(evaluate_expression("10/2").unwrap(), Value::Float(5.0));
}

#[test]
fn test_operator_precedence() {
    assert_eq!(evaluate_expression("2*3+4").unwrap(), Value::Int(10));
    assert_eq!(evaluate_expression("2 * 3 + 4").unwrap(), Value::Int(10));
    assert_eq!(evaluate_expression("10/2-1").unwrap(), Value::Float(4.0));
    assert_eq!(
        evaluate_expression("10 / 2 - 1").unwrap(),
        Value::Float(4.0)
    );
    assert_eq!(evaluate_expression("1+2*3").unwrap(), Value::Int(7));
    assert_eq!(evaluate_expression("8/2*2").unwrap(), Value::Float(8.0));
}

#[test]
fn test_division_by_zero() {
    let result = evaluate_expression("5/0");
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Division by zero"));
}

#[test]
fn test_invalid_expression() {
    let result = evaluate_expression("1+");
    assert!(result.is_err());

    let result = evaluate_expression("+1");
    assert!(result.is_err());

    let result = evaluate_expression("1++2");
    assert!(result.is_err());
}

#[test]
fn test_decimal_numbers() {
    assert_eq!(evaluate_expression("1.5+2.5").unwrap(), Value::Float(4.0));
    assert_eq!(evaluate_expression("3.14*2").unwrap(), Value::Float(6.28));
    assert_eq!(evaluate_expression("10.0/2.0").unwrap(), Value::Float(5.0));
}

#[test]
fn test_type_promotion_int_to_float() {
    assert_eq!(evaluate_expression("1+1.5").unwrap(), Value::Float(2.5));
    assert_eq!(evaluate_expression("1.5+1").unwrap(), Value::Float(2.5));
    assert_eq!(evaluate_expression("5-2.5").unwrap(), Value::Float(2.5));
    assert_eq!(evaluate_expression("5.5-2").unwrap(), Value::Float(3.5));
    assert_eq!(evaluate_expression("2*1.5").unwrap(), Value::Float(3.0));
    assert_eq!(evaluate_expression("2.5*2").unwrap(), Value::Float(5.0));
}

#[test]
fn test_integer_operations() {
    assert_eq!(evaluate_expression("1+2").unwrap(), Value::Int(3));
    assert_eq!(evaluate_expression("5-2").unwrap(), Value::Int(3));
    assert_eq!(evaluate_expression("2*3").unwrap(), Value::Int(6));
    assert_eq!(evaluate_expression("10/2").unwrap(), Value::Float(5.0));
    assert_eq!(evaluate_expression("7/2").unwrap(), Value::Float(3.5));
}

#[test]
fn test_evaluate_modulo() {
    assert_eq!(evaluate_expression("10%3").unwrap(), Value::Int(1));
    assert_eq!(evaluate_expression("10 % 3").unwrap(), Value::Int(1));
    assert_eq!(evaluate_expression("15%4").unwrap(), Value::Int(3));
    assert_eq!(evaluate_expression("10.5%3").unwrap(), Value::Float(1.5));
}

#[test]
fn test_evaluate_power() {
    assert_eq!(evaluate_expression("2**3").unwrap(), Value::Float(8.0));
    assert_eq!(evaluate_expression("2 ** 3").unwrap(), Value::Float(8.0));
    assert_eq!(evaluate_expression("3**2").unwrap(), Value::Float(9.0));
    assert_eq!(
        evaluate_expression("2**3.5").unwrap(),
        Value::Float(11.313708498984761)
    );
    assert_eq!(evaluate_expression("10**0").unwrap(), Value::Float(1.0));
}

#[test]
fn test_evaluate_floor_divide() {
    assert_eq!(evaluate_expression("10//3").unwrap(), Value::Int(3));
    assert_eq!(evaluate_expression("10 // 3").unwrap(), Value::Int(3));
    assert_eq!(evaluate_expression("7//2").unwrap(), Value::Int(3));
    assert_eq!(evaluate_expression("10.7//3").unwrap(), Value::Float(3.0));
    assert_eq!(evaluate_expression("10.7//3.2").unwrap(), Value::Float(3.0));
}

#[test]
fn test_new_operator_precedence() {
    assert_eq!(evaluate_expression("2**3+4").unwrap(), Value::Float(12.0));
    assert_eq!(evaluate_expression("2*3%2").unwrap(), Value::Int(0));
    assert_eq!(evaluate_expression("10//3*2").unwrap(), Value::Int(6));
    assert_eq!(evaluate_expression("2**3**2").unwrap(), Value::Float(512.0));
    assert_eq!(evaluate_expression("2*3**2").unwrap(), Value::Float(18.0));
}

#[test]
fn test_modulo_by_zero() {
    let result = evaluate_expression("5%0");
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Division by zero"));

    let result = evaluate_expression("5.5%0");
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Division by zero"));
}

#[test]
fn test_floor_divide_by_zero() {
    let result = evaluate_expression("5//0");
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Division by zero"));

    let result = evaluate_expression("5.5//0");
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Division by zero"));
}

#[test]
fn test_mixed_types_with_new_operators() {
    assert_eq!(evaluate_expression("10.5%3").unwrap(), Value::Float(1.5));
    assert_eq!(
        evaluate_expression("2**3.5").unwrap(),
        Value::Float(11.313708498984761)
    );
    assert_eq!(evaluate_expression("10.7//3").unwrap(), Value::Float(3.0));
    assert_eq!(evaluate_expression("10//3.2").unwrap(), Value::Float(3.0));
}

#[test]
fn test_bool_literals() {
    assert_eq!(evaluate_expression("true").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("false").unwrap(), Value::Bool(false));
}

#[test]
fn test_comparison_equal() {
    assert_eq!(evaluate_expression("1==1").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("1 == 1").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("1==2").unwrap(), Value::Bool(false));
    assert_eq!(evaluate_expression("1.5==1.5").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("1==1.0").unwrap(), Value::Bool(true));
    assert_eq!(
        evaluate_expression("true==true").unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        evaluate_expression("true==false").unwrap(),
        Value::Bool(false)
    );
}

#[test]
fn test_comparison_not_equal() {
    assert_eq!(evaluate_expression("1!=1").unwrap(), Value::Bool(false));
    assert_eq!(evaluate_expression("1 != 1").unwrap(), Value::Bool(false));
    assert_eq!(evaluate_expression("1!=2").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("1.5!=1.5").unwrap(), Value::Bool(false));
    assert_eq!(evaluate_expression("1!=1.0").unwrap(), Value::Bool(false));
    assert_eq!(
        evaluate_expression("true!=true").unwrap(),
        Value::Bool(false)
    );
    assert_eq!(
        evaluate_expression("true!=false").unwrap(),
        Value::Bool(true)
    );
}

#[test]
fn test_comparison_less_than() {
    assert_eq!(evaluate_expression("1<2").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("1 < 2").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("2<1").unwrap(), Value::Bool(false));
    assert_eq!(evaluate_expression("1<1").unwrap(), Value::Bool(false));
    assert_eq!(evaluate_expression("1.5<2.5").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("1<2.5").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("2.5<1").unwrap(), Value::Bool(false));
}

#[test]
fn test_comparison_greater_than() {
    assert_eq!(evaluate_expression("2>1").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("2 > 1").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("1>2").unwrap(), Value::Bool(false));
    assert_eq!(evaluate_expression("1>1").unwrap(), Value::Bool(false));
    assert_eq!(evaluate_expression("2.5>1.5").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("2.5>1").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("1>2.5").unwrap(), Value::Bool(false));
}

#[test]
fn test_comparison_less_than_or_equal() {
    assert_eq!(evaluate_expression("1<=2").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("1 <= 2").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("1<=1").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("2<=1").unwrap(), Value::Bool(false));
    assert_eq!(evaluate_expression("1.5<=2.5").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("1.5<=1.5").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("2.5<=1.5").unwrap(), Value::Bool(false));
}

#[test]
fn test_comparison_greater_than_or_equal() {
    assert_eq!(evaluate_expression("2>=1").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("2 >= 1").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("1>=1").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("1>=2").unwrap(), Value::Bool(false));
    assert_eq!(evaluate_expression("2.5>=1.5").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("1.5>=1.5").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("1.5>=2.5").unwrap(), Value::Bool(false));
}

#[test]
fn test_comparison_operator_precedence() {
    assert_eq!(evaluate_expression("1+2<3*4").unwrap(), Value::Bool(true));
    assert_eq!(
        evaluate_expression("1 + 2 < 3 * 4").unwrap(),
        Value::Bool(true)
    );
    assert_eq!(evaluate_expression("10/2>3").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("2*3==6").unwrap(), Value::Bool(true));
    assert_eq!(evaluate_expression("1+2<=3").unwrap(), Value::Bool(true));
}

#[test]
fn test_variable_assignment() {
    let mut ctx = Context::new();
    assert_eq!(
        evaluate_expression_with_context("x = 5", &mut ctx).unwrap(),
        Value::Int(5)
    );
    assert_eq!(ctx.get("x"), Some(&Value::Int(5)));
}

#[test]
fn test_variable_reference() {
    let mut ctx = Context::new();
    evaluate_expression_with_context("x = 5", &mut ctx).unwrap();
    assert_eq!(
        evaluate_expression_with_context("x", &mut ctx).unwrap(),
        Value::Int(5)
    );
}

#[test]
fn test_variable_in_expression() {
    let mut ctx = Context::new();
    evaluate_expression_with_context("x = 5", &mut ctx).unwrap();
    assert_eq!(
        evaluate_expression_with_context("x + 1", &mut ctx).unwrap(),
        Value::Int(6)
    );
    assert_eq!(
        evaluate_expression_with_context("x * 2", &mut ctx).unwrap(),
        Value::Int(10)
    );
}

#[test]
fn test_assignment_precedence() {
    let mut ctx = Context::new();
    // x = 1 + 2 should be x = (1 + 2) = 3
    assert_eq!(
        evaluate_expression_with_context("x = 1 + 2", &mut ctx).unwrap(),
        Value::Int(3)
    );
    assert_eq!(ctx.get("x"), Some(&Value::Int(3)));
}

#[test]
fn test_chain_assignment() {
    let mut ctx = Context::new();
    // x = y = 5 should set both x and y to 5
    assert_eq!(
        evaluate_expression_with_context("x = y = 5", &mut ctx).unwrap(),
        Value::Int(5)
    );
    assert_eq!(ctx.get("x"), Some(&Value::Int(5)));
    assert_eq!(ctx.get("y"), Some(&Value::Int(5)));
}

#[test]
fn test_undefined_variable() {
    let mut ctx = Context::new();
    let result = evaluate_expression_with_context("x", &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Undefined variable"));
}

#[test]
fn test_variable_reassignment() {
    let mut ctx = Context::new();
    evaluate_expression_with_context("x = 5", &mut ctx).unwrap();
    evaluate_expression_with_context("x = 10", &mut ctx).unwrap();
    assert_eq!(ctx.get("x"), Some(&Value::Int(10)));
}

#[test]
fn test_variable_with_underscore() {
    let mut ctx = Context::new();
    assert_eq!(
        evaluate_expression_with_context("my_var = 42", &mut ctx).unwrap(),
        Value::Int(42)
    );
    assert_eq!(
        evaluate_expression_with_context("_private = 1", &mut ctx).unwrap(),
        Value::Int(1)
    );
}

#[test]
fn test_variable_assign_expression() {
    let mut ctx = Context::new();
    evaluate_expression_with_context("x = 5", &mut ctx).unwrap();
    evaluate_expression_with_context("y = 10", &mut ctx).unwrap();
    assert_eq!(
        evaluate_expression_with_context("z = x + y", &mut ctx).unwrap(),
        Value::Int(15)
    );
    assert_eq!(ctx.get("z"), Some(&Value::Int(15)));
}

#[test]
fn test_variable_with_comparison() {
    let mut ctx = Context::new();
    evaluate_expression_with_context("x = 5", &mut ctx).unwrap();
    assert_eq!(
        evaluate_expression_with_context("x == 5", &mut ctx).unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        evaluate_expression_with_context("x > 3", &mut ctx).unwrap(),
        Value::Bool(true)
    );
}

// Tests for if statements
fn execute_test_program(
    input: &str,
    ctx: &mut Context,
) -> std::result::Result<Option<Value>, String> {
    let mut lexer = super::lexer::Lexer::new(input);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
    let program = super::parser::parse_program(&tokens)?;
    super::execute_program(&program, ctx).map_err(|e| e.to_string())
}

#[test]
fn test_if_true_branch() {
    let mut ctx = Context::new();
    let input = "x = 10\nif x > 5:\n    y = 20\n    y\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(result, Some(Value::Int(20)));
}

#[test]
fn test_if_false_branch() {
    let mut ctx = Context::new();
    let input = "x = 3\nif x > 5:\n    y = 20\n    y\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(result, None);
}

#[test]
fn test_if_else() {
    let mut ctx = Context::new();
    let input = "x = 3\nif x > 5:\n    10\nelse:\n    20\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(result, Some(Value::Int(20)));
}

#[test]
fn test_if_elif_else() {
    let mut ctx = Context::new();
    let input = "x = 5\nif x > 10:\n    1\nelif x > 3:\n    2\nelse:\n    3\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(result, Some(Value::Int(2)));
}

#[test]
fn test_scope_isolation_in_if() {
    let mut ctx = Context::new();
    let input = "x = 10\nif true:\n    y = 20\n";
    execute_test_program(input, &mut ctx).unwrap();

    // x is accessible
    assert_eq!(ctx.get("x"), Some(&Value::Int(10)));
    // y is not accessible (was in if block scope)
    assert!(ctx.get("y").is_none());
}

#[test]
fn test_outer_variable_update_in_if() {
    let mut ctx = Context::new();
    let input = "x = 10\nif true:\n    x = 100\n";
    execute_test_program(input, &mut ctx).unwrap();

    // x was updated in the outer scope
    assert_eq!(ctx.get("x"), Some(&Value::Int(100)));
}

#[test]
fn test_nested_if() {
    let mut ctx = Context::new();
    // y is defined outside the inner if, so it can be accessed after
    let input =
        "x = 10\ny = 0\nif x > 5:\n    if x > 8:\n        y = 1\n    else:\n        y = 2\n    y\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(result, Some(Value::Int(1)));
    // y was updated in outer scope
    assert_eq!(ctx.get("y"), Some(&Value::Int(1)));
}

#[test]
fn test_if_with_complex_condition() {
    let mut ctx = Context::new();
    let input = "x = 10\ny = 5\nif x > y:\n    z = x + y\n    z\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(result, Some(Value::Int(15)));
}

#[test]
fn test_truthy_integer() {
    let mut ctx = Context::new();
    let input = "if 1:\n    10\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(result, Some(Value::Int(10)));
}

#[test]
fn test_falsy_integer() {
    let mut ctx = Context::new();
    let input = "if 0:\n    10\nelse:\n    20\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(result, Some(Value::Int(20)));
}

// Type annotation tests
#[test]
fn test_typed_int_assignment() {
    let mut ctx = Context::new();
    assert_eq!(
        evaluate_expression_with_context("x: int = 5", &mut ctx).unwrap(),
        Value::Int(5)
    );
    assert_eq!(ctx.get("x"), Some(&Value::Int(5)));
}

#[test]
fn test_typed_float_assignment() {
    let mut ctx = Context::new();
    assert_eq!(
        evaluate_expression_with_context("y: float = 3.14", &mut ctx).unwrap(),
        Value::Float(3.14)
    );
    assert_eq!(ctx.get("y"), Some(&Value::Float(3.14)));
}

#[test]
fn test_typed_bool_assignment() {
    let mut ctx = Context::new();
    assert_eq!(
        evaluate_expression_with_context("flag: bool = true", &mut ctx).unwrap(),
        Value::Bool(true)
    );
    assert_eq!(ctx.get("flag"), Some(&Value::Bool(true)));
}

#[test]
fn test_typed_mismatch_int_expects_float() {
    let mut ctx = Context::new();
    let result = evaluate_expression_with_context("x: int = 3.14", &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Type mismatch"));
}

#[test]
fn test_typed_mismatch_float_expects_int() {
    let mut ctx = Context::new();
    let result = evaluate_expression_with_context("x: float = 5", &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Type mismatch"));
}

#[test]
fn test_typed_mismatch_bool_expects_int() {
    let mut ctx = Context::new();
    let result = evaluate_expression_with_context("x: bool = 5", &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Type mismatch"));
}

#[test]
fn test_typed_variable_reassignment_same_type() {
    let mut ctx = Context::new();
    evaluate_expression_with_context("x: int = 5", &mut ctx).unwrap();
    // Reassigning with same type should work
    assert_eq!(
        evaluate_expression_with_context("x = 10", &mut ctx).unwrap(),
        Value::Int(10)
    );
    assert_eq!(ctx.get("x"), Some(&Value::Int(10)));
}

#[test]
fn test_typed_variable_reassignment_different_type() {
    let mut ctx = Context::new();
    evaluate_expression_with_context("x: int = 5", &mut ctx).unwrap();
    // Reassigning with different type should fail
    let result = evaluate_expression_with_context("x = 3.14", &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Type mismatch"));
}

#[test]
fn test_untyped_variable_allows_type_change() {
    let mut ctx = Context::new();
    // Untyped variable assignment
    evaluate_expression_with_context("x = 5", &mut ctx).unwrap();
    // Can reassign to different type
    assert_eq!(
        evaluate_expression_with_context("x = 3.14", &mut ctx).unwrap(),
        Value::Float(3.14)
    );
    assert_eq!(ctx.get("x"), Some(&Value::Float(3.14)));
}

#[test]
fn test_typed_assignment_with_expression() {
    let mut ctx = Context::new();
    assert_eq!(
        evaluate_expression_with_context("x: int = 2 + 3", &mut ctx).unwrap(),
        Value::Int(5)
    );
    assert_eq!(ctx.get("x"), Some(&Value::Int(5)));
}

#[test]
fn test_typed_variable_in_program() {
    let mut ctx = Context::new();
    let input = "x: int = 10\ny: float = 3.14\nx + 1\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(result, Some(Value::Int(11)));
}

#[test]
fn test_typed_variable_reassignment_in_program() {
    let mut ctx = Context::new();
    let input = "x: int = 10\nx = 20\nx\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(result, Some(Value::Int(20)));
}

#[test]
fn test_typed_variable_type_error_in_program() {
    let mut ctx = Context::new();
    let input = "x: int = 10\nx = 3.14\n";
    let result = execute_test_program(input, &mut ctx);
    assert!(result.is_err());
}

// String tests
#[test]
fn test_string_literal() {
    assert_eq!(
        evaluate_expression("\"hello\"").unwrap(),
        Value::Str("hello".to_string())
    );
}

#[test]
fn test_string_concatenation() {
    assert_eq!(
        evaluate_expression("\"hello\" + \" world\"").unwrap(),
        Value::Str("hello world".to_string())
    );
}

#[test]
fn test_string_variable() {
    let mut ctx = Context::new();
    assert_eq!(
        evaluate_expression_with_context("x = \"test\"", &mut ctx).unwrap(),
        Value::Str("test".to_string())
    );
    assert_eq!(
        evaluate_expression_with_context("x", &mut ctx).unwrap(),
        Value::Str("test".to_string())
    );
}

#[test]
fn test_typed_string_variable() {
    let mut ctx = Context::new();
    assert_eq!(
        evaluate_expression_with_context("x: str = \"hello\"", &mut ctx).unwrap(),
        Value::Str("hello".to_string())
    );
    assert_eq!(ctx.get("x"), Some(&Value::Str("hello".to_string())));
}

#[test]
fn test_typed_string_mismatch() {
    let mut ctx = Context::new();
    let result = evaluate_expression_with_context("x: str = 42", &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Type mismatch"));
}

#[test]
fn test_string_equality() {
    assert_eq!(
        evaluate_expression("\"hello\" == \"hello\"").unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        evaluate_expression("\"hello\" == \"world\"").unwrap(),
        Value::Bool(false)
    );
    assert_eq!(
        evaluate_expression("\"hello\" != \"world\"").unwrap(),
        Value::Bool(true)
    );
}

#[test]
fn test_string_with_int_error() {
    let result = evaluate_expression("\"hello\" + 1");
    assert!(result.is_err());
}

#[test]
fn test_string_truthy_in_program() {
    let mut ctx = Context::new();
    let input = "if \"hello\":\n    10\nelse:\n    20\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(result, Some(Value::Int(10)));
}

#[test]
fn test_empty_string_falsy_in_program() {
    let mut ctx = Context::new();
    let input = "if \"\":\n    10\nelse:\n    20\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(result, Some(Value::Int(20)));
}

// While loop tests
#[test]
fn test_while_basic() {
    let mut ctx = Context::new();
    let input = "count = 0\nwhile count < 3:\n    count = count + 1\ncount\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(result, Some(Value::Int(3)));
}

#[test]
fn test_while_false_condition() {
    let mut ctx = Context::new();
    let input = "x = 0\nwhile false:\n    x = 1\nx\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    // Body is never executed, x remains 0
    assert_eq!(result, Some(Value::Int(0)));
}

#[test]
fn test_while_scope_isolation() {
    let mut ctx = Context::new();
    let input = "count = 0\nwhile count < 1:\n    count = count + 1\n    temp = 42\n";
    execute_test_program(input, &mut ctx).unwrap();
    // count was updated in outer scope
    assert_eq!(ctx.get("count"), Some(&Value::Int(1)));
    // temp is not accessible (was in while block scope)
    assert!(ctx.get("temp").is_none());
}

#[test]
fn test_while_nested() {
    let mut ctx = Context::new();
    let input = "i = 0\nsum = 0\nwhile i < 3:\n    j = 0\n    while j < 2:\n        sum = sum + 1\n        j = j + 1\n    i = i + 1\nsum\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    // 3 outer iterations * 2 inner iterations = 6
    assert_eq!(result, Some(Value::Int(6)));
}

#[test]
fn test_while_with_truthy_integer() {
    let mut ctx = Context::new();
    let input = "n = 3\nresult = 0\nwhile n:\n    result = result + n\n    n = n - 1\nresult\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    // 3 + 2 + 1 = 6
    assert_eq!(result, Some(Value::Int(6)));
}

#[test]
fn test_while_returns_last_value() {
    let mut ctx = Context::new();
    let input = "count = 0\nwhile count < 3:\n    count = count + 1\n    count * 10\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    // Last iteration: count = 3, returns 3 * 10 = 30
    assert_eq!(result, Some(Value::Int(30)));
}

// Any type tests
#[test]
fn test_any_type_assignment() {
    let mut ctx = Context::new();
    assert_eq!(
        evaluate_expression_with_context("x: any = 5", &mut ctx).unwrap(),
        Value::Int(5)
    );
    assert_eq!(ctx.get("x"), Some(&Value::Int(5)));
}

#[test]
fn test_any_type_allows_reassignment_to_different_types() {
    let mut ctx = Context::new();
    // Initial assignment with any type
    evaluate_expression_with_context("x: any = 5", &mut ctx).unwrap();
    assert_eq!(ctx.get("x"), Some(&Value::Int(5)));

    // Reassign to float
    evaluate_expression_with_context("x = 3.14", &mut ctx).unwrap();
    assert_eq!(ctx.get("x"), Some(&Value::Float(3.14)));

    // Reassign to bool
    evaluate_expression_with_context("x = true", &mut ctx).unwrap();
    assert_eq!(ctx.get("x"), Some(&Value::Bool(true)));

    // Reassign to string
    evaluate_expression_with_context("x = \"hello\"", &mut ctx).unwrap();
    assert_eq!(ctx.get("x"), Some(&Value::Str("hello".to_string())));
}

#[test]
fn test_any_type_in_program() {
    let mut ctx = Context::new();
    let input = "x: any = 5\nx = 3.14\nx = true\nx = \"hello\"\nx\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(result, Some(Value::Str("hello".to_string())));
}

#[test]
fn test_any_type_with_float_initial() {
    let mut ctx = Context::new();
    assert_eq!(
        evaluate_expression_with_context("y: any = 3.14", &mut ctx).unwrap(),
        Value::Float(3.14)
    );
    // Can reassign to int
    evaluate_expression_with_context("y = 42", &mut ctx).unwrap();
    assert_eq!(ctx.get("y"), Some(&Value::Int(42)));
}

#[test]
fn test_any_type_with_bool_initial() {
    let mut ctx = Context::new();
    assert_eq!(
        evaluate_expression_with_context("flag: any = true", &mut ctx).unwrap(),
        Value::Bool(true)
    );
    // Can reassign to string
    evaluate_expression_with_context("flag = \"yes\"", &mut ctx).unwrap();
    assert_eq!(ctx.get("flag"), Some(&Value::Str("yes".to_string())));
}

#[test]
fn test_any_type_with_string_initial() {
    let mut ctx = Context::new();
    assert_eq!(
        evaluate_expression_with_context("s: any = \"hello\"", &mut ctx).unwrap(),
        Value::Str("hello".to_string())
    );
    // Can reassign to int
    evaluate_expression_with_context("s = 100", &mut ctx).unwrap();
    assert_eq!(ctx.get("s"), Some(&Value::Int(100)));
}

// Tuple tests
#[test]
fn test_tuple_basic() {
    assert_eq!(
        evaluate_expression("(1, 2)").unwrap(),
        Value::Tuple(vec![Value::Int(1), Value::Int(2)])
    );
}

#[test]
fn test_tuple_three_elements() {
    assert_eq!(
        evaluate_expression("(1, 2, 3)").unwrap(),
        Value::Tuple(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
    );
}

#[test]
fn test_tuple_with_expressions() {
    assert_eq!(
        evaluate_expression("(1 + 1, 2 * 3)").unwrap(),
        Value::Tuple(vec![Value::Int(2), Value::Int(6)])
    );
}

#[test]
fn test_tuple_nested() {
    assert_eq!(
        evaluate_expression("((1, 2), 3)").unwrap(),
        Value::Tuple(vec![
            Value::Tuple(vec![Value::Int(1), Value::Int(2)]),
            Value::Int(3)
        ])
    );
}

#[test]
fn test_tuple_equality() {
    assert_eq!(
        evaluate_expression("(1, 2) == (1, 2)").unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        evaluate_expression("(1, 2) == (1, 3)").unwrap(),
        Value::Bool(false)
    );
}

#[test]
fn test_tuple_inequality() {
    assert_eq!(
        evaluate_expression("(1, 2) != (1, 3)").unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        evaluate_expression("(1, 2) != (1, 2)").unwrap(),
        Value::Bool(false)
    );
}

#[test]
fn test_tuple_unpack_basic() {
    let mut ctx = Context::new();
    let input = "a, b = (1, 2)\n";
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("a"), Some(&Value::Int(1)));
    assert_eq!(ctx.get("b"), Some(&Value::Int(2)));
}

#[test]
fn test_tuple_unpack_three_elements() {
    let mut ctx = Context::new();
    let input = "a, b, c = (1, 2, 3)\n";
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("a"), Some(&Value::Int(1)));
    assert_eq!(ctx.get("b"), Some(&Value::Int(2)));
    assert_eq!(ctx.get("c"), Some(&Value::Int(3)));
}

#[test]
fn test_tuple_unpack_from_expression() {
    let mut ctx = Context::new();
    let input = "x, y = (1 + 1, 2 * 3)\n";
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("x"), Some(&Value::Int(2)));
    assert_eq!(ctx.get("y"), Some(&Value::Int(6)));
}

#[test]
fn test_tuple_unpack_mismatch() {
    let mut ctx = Context::new();
    let input = "a, b = (1, 2, 3)\n";
    let result = execute_test_program(input, &mut ctx);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .contains("Tuple unpack mismatch: expected 2 elements, got 3"));
}

#[test]
fn test_tuple_unpack_non_tuple() {
    let mut ctx = Context::new();
    let input = "a, b = 5\n";
    let result = execute_test_program(input, &mut ctx);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .contains("Cannot unpack non-tuple value of type int"));
}

#[test]
fn test_typed_tuple_assignment() {
    let mut ctx = Context::new();
    let input = "p: (int, int) = (1, 2)\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(
        result,
        Some(Value::Tuple(vec![Value::Int(1), Value::Int(2)]))
    );
}

#[test]
fn test_typed_tuple_mixed_types() {
    let mut ctx = Context::new();
    let input = "q: (int, float) = (1, 2.5)\n";
    let result = execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(
        result,
        Some(Value::Tuple(vec![Value::Int(1), Value::Float(2.5)]))
    );
}

#[test]
fn test_typed_tuple_type_mismatch() {
    let mut ctx = Context::new();
    let input = "p: (int, int) = (1, 2.5)\n";
    let result = execute_test_program(input, &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Type mismatch"));
}

#[test]
fn test_tuple_display() {
    let tuple = Value::Tuple(vec![Value::Int(1), Value::Int(2)]);
    assert_eq!(format!("{}", tuple), "(1, 2)");
}

#[test]
fn test_tuple_type_name() {
    let tuple = Value::Tuple(vec![Value::Int(1), Value::Float(2.5)]);
    assert_eq!(tuple.type_name(), "(int, float)");
}

#[test]
fn test_parenthesized_expression_not_tuple() {
    // Single expression in parens should be grouping, not a tuple
    assert_eq!(evaluate_expression("(1 + 2)").unwrap(), Value::Int(3));
}
