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
    // Trailing operator - should report an error
    let result = evaluate_expression("1+");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("unexpected") || err.contains("Unexpected") || err.contains("expected"),
        "Error message should indicate parsing failure: {}",
        err
    );

    // Leading operator - should report error (+ is not a valid unary operator in ry)
    let result = evaluate_expression("+1");
    assert!(result.is_err());

    // Double operator - should report unexpected token
    let result = evaluate_expression("1++2");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("unexpected") || err.contains("Unexpected") || err.contains("expected"),
        "Error message should indicate parsing failure: {}",
        err
    );
}

#[test]
fn test_decimal_numbers() {
    assert_eq!(evaluate_expression("1.5+2.5").unwrap(), Value::Float(4.0));
    assert_eq!(evaluate_expression("3.14*2").unwrap(), Value::Float(6.28));
    assert_eq!(evaluate_expression("10.0/2.0").unwrap(), Value::Float(5.0));
}

#[test]
fn test_scientific_notation_numbers() {
    assert_eq!(evaluate_expression("1e10").unwrap(), Value::Float(1e10));
    assert_eq!(evaluate_expression("2.5e-3").unwrap(), Value::Float(0.0025));
    assert_eq!(evaluate_expression("1E+6").unwrap(), Value::Float(1e6));
}

#[test]
fn test_hex_numbers() {
    assert_eq!(evaluate_expression("0xFF").unwrap(), Value::Int(255));
    assert_eq!(evaluate_expression("0x10").unwrap(), Value::Int(16));
    assert_eq!(
        evaluate_expression("0xabcdef").unwrap(),
        Value::Int(0xabcdef)
    );
}

#[test]
fn test_hex_arithmetic() {
    assert_eq!(evaluate_expression("0xFF + 1").unwrap(), Value::Int(256));
    assert_eq!(evaluate_expression("0x10 * 2").unwrap(), Value::Int(32));
}

#[test]
fn test_octal_numbers() {
    assert_eq!(evaluate_expression("0o77").unwrap(), Value::Int(63));
    assert_eq!(evaluate_expression("0o10").unwrap(), Value::Int(8));
    assert_eq!(evaluate_expression("0O755").unwrap(), Value::Int(493));
}

#[test]
fn test_octal_arithmetic() {
    assert_eq!(evaluate_expression("0o10 + 0o10").unwrap(), Value::Int(16));
}

#[test]
fn test_binary_numbers() {
    assert_eq!(evaluate_expression("0b1010").unwrap(), Value::Int(10));
    assert_eq!(evaluate_expression("0b0").unwrap(), Value::Int(0));
    assert_eq!(evaluate_expression("0B1111").unwrap(), Value::Int(15));
}

#[test]
fn test_binary_arithmetic() {
    assert_eq!(
        evaluate_expression("0b1010 + 0b0101").unwrap(),
        Value::Int(15)
    );
}

#[test]
fn test_mixed_base_arithmetic() {
    // 0xFF (255) + 0o10 (8) + 0b1010 (10) = 273
    assert_eq!(
        evaluate_expression("0xFF + 0o10 + 0b1010").unwrap(),
        Value::Int(273)
    );
}

#[test]
fn test_hex_overflow() {
    // Test that parsing a large hex number works (within i64 range)
    assert_eq!(
        evaluate_expression("0x7FFFFFFFFFFFFFFF").unwrap(),
        Value::Int(i64::MAX)
    );
}

#[test]
fn test_binary_large() {
    // Test parsing a 63-bit binary number
    assert_eq!(
        evaluate_expression("0b111111111111111111111111111111111111111111111111111111111111111")
            .unwrap(),
        Value::Int(i64::MAX)
    );
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

// ===================
// Function tests
// ===================

#[test]
fn test_function_definition_and_call() {
    let mut ctx = Context::new();
    let input = r#"
fn add(a, b):
    return a + b

result = add(1, 2)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(3)));
}

#[test]
fn test_function_with_type_annotations() {
    let mut ctx = Context::new();
    let input = r#"
fn add(a: int, b: int) -> int:
    return a + b

result = add(1, 2)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(3)));
}

#[test]
fn test_function_type_mismatch_argument() {
    let mut ctx = Context::new();
    let input = r#"
fn add(a: int, b: int):
    return a + b

result = add(1, 2.5)
"#;
    let result = execute_test_program(input, &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Type mismatch for parameter"));
}

#[test]
fn test_function_type_mismatch_return() {
    let mut ctx = Context::new();
    let input = r#"
fn get_int() -> int:
    return 3.14

result = get_int()
"#;
    let result = execute_test_program(input, &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Return type mismatch"));
}

#[test]
fn test_function_argument_count_mismatch() {
    let mut ctx = Context::new();
    let input = r#"
fn add(a, b):
    return a + b

result = add(1)
"#;
    let result = execute_test_program(input, &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("expects 2 arguments, got 1"));
}

#[test]
fn test_function_implicit_return() {
    let mut ctx = Context::new();
    let input = r#"
fn set_x():
    x = 10

result = set_x()
"#;
    execute_test_program(input, &mut ctx).unwrap();
    // Function without explicit return implicitly returns the last expression value
    assert_eq!(ctx.get("result"), Some(&Value::Int(10)));
}

#[test]
fn test_function_as_value() {
    let mut ctx = Context::new();
    let input = r#"
fn add(a, b):
    return a + b

my_func = add
result = my_func(3, 4)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(7)));
}

#[test]
fn test_higher_order_function() {
    let mut ctx = Context::new();
    let input = r#"
fn double(n):
    return n * 2

fn apply(f, x):
    return f(x)

result = apply(double, 5)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(10)));
}

#[test]
fn test_function_recursion() {
    let mut ctx = Context::new();
    let input = r#"
fn factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

result = factorial(5)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(120)));
}

#[test]
fn test_function_early_return() {
    let mut ctx = Context::new();
    let input = r#"
fn abs(n):
    if n < 0:
        return 0 - n
    return n

result1 = abs(5)
result2 = abs(-5)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result1"), Some(&Value::Int(5)));
    assert_eq!(ctx.get("result2"), Some(&Value::Int(5)));
}

#[test]
fn test_function_modifies_outer_scope() {
    // In ry, functions can modify variables from outer scopes
    // This is similar to Python's behavior without `global` keyword
    // but ry allows modification of outer scope variables directly
    let mut ctx = Context::new();
    let input = r#"
x = 10

fn set_x():
    x = 99
    return x

result = set_x()
outer_x = x
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(99)));
    // x in outer scope is modified by the function
    assert_eq!(ctx.get("outer_x"), Some(&Value::Int(99)));
}

#[test]
fn test_function_local_variable() {
    // Variables that don't exist in outer scope are local to the function
    let mut ctx = Context::new();
    let input = r#"
fn create_local():
    local_var = 42
    return local_var

result = create_local()
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(42)));
    // local_var should not exist in outer scope
    assert_eq!(ctx.get("local_var"), None);
}

#[test]
fn test_function_closure_reads_outer() {
    let mut ctx = Context::new();
    let input = r#"
multiplier = 10

fn times_multiplier(n):
    return n * multiplier

result = times_multiplier(5)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(50)));
}

#[test]
fn test_function_not_callable_error() {
    let mut ctx = Context::new();
    let input = r#"
x = 5
result = x(1, 2)
"#;
    let result = execute_test_program(input, &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Cannot call value of type"));
}

#[test]
fn test_function_with_while_loop() {
    let mut ctx = Context::new();
    let input = r#"
fn sum_to(n):
    total = 0
    i = 1
    while i <= n:
        total = total + i
        i = i + 1
    return total

result = sum_to(5)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(15)));
}

#[test]
fn test_function_nested_calls() {
    let mut ctx = Context::new();
    let input = r#"
fn add(a, b):
    return a + b

fn multiply(a, b):
    return a * b

result = add(multiply(2, 3), multiply(4, 5))
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(26)));
}

#[test]
fn test_function_return_function() {
    // Note: True closures with captured environments are not fully supported.
    // This test demonstrates returning a function, but inner functions
    // can only access variables that exist in the global/outer scope at call time.
    let mut ctx = Context::new();
    let input = r#"
n = 5

fn make_adder():
    fn add(x):
        return x + n
    return add

add_func = make_adder()
result = add_func(10)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    // Works because n is in the outer scope accessible during add_func(10) call
    assert_eq!(ctx.get("result"), Some(&Value::Int(15)));
}

// ===================
// Function signature tests
// ===================

#[test]
fn test_function_signature_basic() {
    let mut ctx = Context::new();
    let input = r#"
fn double(n: int) -> int:
    return n * 2

fn apply(f: func(int) -> int, x: int) -> int:
    return f(x)

result = apply(double, 5)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(10)));
}

#[test]
fn test_function_signature_two_params() {
    let mut ctx = Context::new();
    let input = r#"
fn add(a: int, b: int) -> int:
    return a + b

fn apply2(f: func(int, int) -> int, a: int, b: int) -> int:
    return f(a, b)

result = apply2(add, 3, 4)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(7)));
}

#[test]
fn test_function_signature_no_return_type() {
    let mut ctx = Context::new();
    let input = r#"
fn print_num(n: int):
    x = n

fn foreach(f: func(int), x: int):
    f(x)

foreach(print_num, 42)
"#;
    // Should work - no return type in signature means any return is acceptable
    let result = execute_test_program(input, &mut ctx);
    assert!(result.is_ok());
}

#[test]
fn test_function_signature_backward_compat_func() {
    // `func` without signature accepts any function
    let mut ctx = Context::new();
    let input = r#"
fn double(n: int) -> int:
    return n * 2

fn apply_any(f: func, x: int) -> int:
    return f(x)

result = apply_any(double, 5)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(10)));
}

#[test]
fn test_function_signature_backward_compat_func_any_params() {
    // `func` without signature accepts functions with any number of params
    let mut ctx = Context::new();
    let input = r#"
fn add(a: int, b: int) -> int:
    return a + b

fn apply_any_binary(f: func, a: int, b: int) -> int:
    return f(a, b)

result = apply_any_binary(add, 3, 4)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(7)));
}

#[test]
fn test_function_signature_any_param_type() {
    let mut ctx = Context::new();
    let input = r#"
fn identity(x: int) -> int:
    return x

fn apply_any_arg(f: func(any) -> int, x: int) -> int:
    return f(x)

result = apply_any_arg(identity, 42)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(42)));
}

#[test]
fn test_function_signature_any_return_type() {
    let mut ctx = Context::new();
    let input = r#"
fn double(n: int) -> int:
    return n * 2

fn apply_any_ret(f: func(int) -> any, x: int) -> any:
    return f(x)

result = apply_any_ret(double, 5)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(10)));
}

#[test]
fn test_function_signature_mismatch_param_count() {
    let mut ctx = Context::new();
    let input = r#"
fn add(a: int, b: int) -> int:
    return a + b

fn apply_unary(f: func(int) -> int, x: int) -> int:
    return f(x)

result = apply_unary(add, 5)
"#;
    let result = execute_test_program(input, &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Function signature mismatch"));
}

#[test]
fn test_function_signature_mismatch_param_type() {
    let mut ctx = Context::new();
    let input = r#"
fn process_float(x: float) -> float:
    return x * 2.0

fn apply_int(f: func(int) -> int, x: int) -> int:
    return f(x)

result = apply_int(process_float, 5)
"#;
    let result = execute_test_program(input, &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Function signature mismatch"));
}

#[test]
fn test_function_signature_mismatch_return_type() {
    let mut ctx = Context::new();
    let input = r#"
fn to_float(n: int) -> float:
    return n * 1.0

fn apply_int_ret(f: func(int) -> int, x: int) -> int:
    return f(x)

result = apply_int_ret(to_float, 5)
"#;
    let result = execute_test_program(input, &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Function signature mismatch"));
}

#[test]
fn test_function_signature_mismatch_non_function() {
    let mut ctx = Context::new();
    let input = r#"
fn apply(f: func(int) -> int, x: int) -> int:
    return f(x)

result = apply(42, 5)
"#;
    let result = execute_test_program(input, &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Function signature mismatch"));
}

#[test]
fn test_function_signature_untyped_func_compatible() {
    // Function without type annotations should be compatible with any signature expectation
    let mut ctx = Context::new();
    let input = r#"
fn double(n):
    return n * 2

fn apply(f: func(int) -> int, x: int) -> int:
    return f(x)

result = apply(double, 5)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(10)));
}

#[test]
fn test_function_signature_in_variable() {
    let mut ctx = Context::new();
    let input = r#"
fn double(n: int) -> int:
    return n * 2

my_func: func(int) -> int = double
result = my_func(5)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(10)));
}

#[test]
fn test_function_signature_variable_mismatch() {
    let mut ctx = Context::new();
    let input = r#"
fn add(a: int, b: int) -> int:
    return a + b

my_func: func(int) -> int = add
"#;
    let result = execute_test_program(input, &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Type mismatch"));
}

// ===================
// Edge case tests
// ===================

#[test]
fn test_deeply_nested_scope() {
    let mut ctx = Context::new();
    let input = r#"
x = 1
if true:
    y = 2
    if true:
        z = 3
        if true:
            w = 4
            result = x + y + z + w
"#;
    let result = execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(result, Some(Value::Int(10)));
}

#[test]
fn test_nested_function_definition() {
    let mut ctx = Context::new();
    let input = r#"
fn outer():
    fn inner():
        return 42
    return inner()

result = outer()
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(42)));
}

#[test]
fn test_deep_recursion() {
    let mut ctx = Context::new();
    let input = r#"
fn countdown(n: int) -> int:
    if n == 0:
        return 0
    return countdown(n - 1)

result = countdown(100)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(0)));
}

// ===================
// Function overloading tests
// ===================

#[test]
fn test_overload_by_argument_count() {
    let mut ctx = Context::new();
    let input = r#"
fn greet():
    return "Hello!"

fn greet(name: str):
    return "Hello, " + name + "!"

r1 = greet()
r2 = greet("Alice")
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("r1"), Some(&Value::Str("Hello!".to_string())));
    assert_eq!(
        ctx.get("r2"),
        Some(&Value::Str("Hello, Alice!".to_string()))
    );
}

#[test]
fn test_overload_by_type() {
    let mut ctx = Context::new();
    let input = r#"
fn double(x: int) -> int:
    return x * 2

fn double(x: float) -> float:
    return x * 2.0

fn double(s: str) -> str:
    return s + s

r1 = double(5)
r2 = double(2.5)
r3 = double("ab")
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("r1"), Some(&Value::Int(10)));
    assert_eq!(ctx.get("r2"), Some(&Value::Float(5.0)));
    assert_eq!(ctx.get("r3"), Some(&Value::Str("abab".to_string())));
}

#[test]
fn test_overload_mixed_count_and_type() {
    let mut ctx = Context::new();
    let input = r#"
fn process(x: int) -> int:
    return x + 1

fn process(x: int, y: int) -> int:
    return x + y

fn process(x: str) -> str:
    return "processed: " + x

r1 = process(5)
r2 = process(3, 4)
r3 = process("test")
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("r1"), Some(&Value::Int(6)));
    assert_eq!(ctx.get("r2"), Some(&Value::Int(7)));
    assert_eq!(
        ctx.get("r3"),
        Some(&Value::Str("processed: test".to_string()))
    );
}

#[test]
fn test_overload_no_matching_signature() {
    let mut ctx = Context::new();
    let input = r#"
fn add(a: int, b: int) -> int:
    return a + b

fn add(a: float, b: float) -> float:
    return a + b

result = add("x", "y")
"#;
    let result = execute_test_program(input, &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("No matching overload"));
}

#[test]
fn test_overload_duplicate_signature_error() {
    let mut ctx = Context::new();
    let input = r#"
fn foo(x: int) -> int:
    return x

fn foo(y: int) -> int:
    return y * 2
"#;
    let result = execute_test_program(input, &mut ctx);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("Duplicate function signature"));
}

#[test]
fn test_overload_untyped_vs_typed() {
    // Typed parameter should take precedence over untyped
    let mut ctx = Context::new();
    let input = r#"
fn process(x):
    return "untyped"

fn process(x: int) -> str:
    return "int"

r1 = process(5)
r2 = process("text")
"#;
    execute_test_program(input, &mut ctx).unwrap();
    // Typed (int) should match for int argument
    assert_eq!(ctx.get("r1"), Some(&Value::Str("int".to_string())));
    // Untyped should match for string argument (no typed match)
    assert_eq!(ctx.get("r2"), Some(&Value::Str("untyped".to_string())));
}

#[test]
fn test_overload_three_arities() {
    let mut ctx = Context::new();
    let input = r#"
fn sum() -> int:
    return 0

fn sum(a: int) -> int:
    return a

fn sum(a: int, b: int) -> int:
    return a + b

fn sum(a: int, b: int, c: int) -> int:
    return a + b + c

r0 = sum()
r1 = sum(1)
r2 = sum(1, 2)
r3 = sum(1, 2, 3)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("r0"), Some(&Value::Int(0)));
    assert_eq!(ctx.get("r1"), Some(&Value::Int(1)));
    assert_eq!(ctx.get("r2"), Some(&Value::Int(3)));
    assert_eq!(ctx.get("r3"), Some(&Value::Int(6)));
}

#[test]
fn test_overload_with_recursion() {
    let mut ctx = Context::new();
    let input = r#"
fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

fn factorial(n: int, acc: int) -> int:
    if n <= 1:
        return acc
    return factorial(n - 1, acc * n)

r1 = factorial(5)
r2 = factorial(5, 1)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("r1"), Some(&Value::Int(120)));
    assert_eq!(ctx.get("r2"), Some(&Value::Int(120)));
}

#[test]
fn test_overload_function_as_value() {
    // When assigning an overloaded function to a variable,
    // the variable holds the overload collection
    let mut ctx = Context::new();
    let input = r#"
fn double(x: int) -> int:
    return x * 2

fn double(x: str) -> str:
    return x + x

my_double = double
r1 = my_double(5)
r2 = my_double("ab")
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("r1"), Some(&Value::Int(10)));
    assert_eq!(ctx.get("r2"), Some(&Value::Str("abab".to_string())));
}

#[test]
fn test_overload_bool_type() {
    let mut ctx = Context::new();
    let input = r#"
fn to_string(x: int) -> str:
    return "int"

fn to_string(x: bool) -> str:
    return "bool"

fn to_string(x: float) -> str:
    return "float"

r1 = to_string(42)
r2 = to_string(true)
r3 = to_string(3.14)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("r1"), Some(&Value::Str("int".to_string())));
    assert_eq!(ctx.get("r2"), Some(&Value::Str("bool".to_string())));
    assert_eq!(ctx.get("r3"), Some(&Value::Str("float".to_string())));
}

#[test]
fn test_single_function_still_works() {
    // Ensure single function (not overloaded) still works
    let mut ctx = Context::new();
    let input = r#"
fn add(a: int, b: int) -> int:
    return a + b

result = add(3, 4)
"#;
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(7)));
}

#[test]
fn test_overload_type_name() {
    let mut ctx = Context::new();
    let input = r#"
fn f(x: int):
    return x

fn f(x: str):
    return x
"#;
    execute_test_program(input, &mut ctx).unwrap();
    // The variable f should hold an overload collection
    let f_value = ctx.get("f").unwrap();
    assert!(f_value.type_name().contains("overloads"));
}

// ===================
// Logical operator tests
// ===================

// Basic and tests
#[test]
fn test_and_true_true() {
    assert_eq!(
        evaluate_expression("true and true").unwrap(),
        Value::Bool(true)
    );
}

#[test]
fn test_and_true_false() {
    assert_eq!(
        evaluate_expression("true and false").unwrap(),
        Value::Bool(false)
    );
}

#[test]
fn test_and_false_true() {
    assert_eq!(
        evaluate_expression("false and true").unwrap(),
        Value::Bool(false)
    );
}

#[test]
fn test_and_false_false() {
    assert_eq!(
        evaluate_expression("false and false").unwrap(),
        Value::Bool(false)
    );
}

// Basic or tests
#[test]
fn test_or_true_true() {
    assert_eq!(
        evaluate_expression("true or true").unwrap(),
        Value::Bool(true)
    );
}

#[test]
fn test_or_true_false() {
    assert_eq!(
        evaluate_expression("true or false").unwrap(),
        Value::Bool(true)
    );
}

#[test]
fn test_or_false_true() {
    assert_eq!(
        evaluate_expression("false or true").unwrap(),
        Value::Bool(true)
    );
}

#[test]
fn test_or_false_false() {
    assert_eq!(
        evaluate_expression("false or false").unwrap(),
        Value::Bool(false)
    );
}

// Basic not tests
#[test]
fn test_not_true() {
    assert_eq!(evaluate_expression("not true").unwrap(), Value::Bool(false));
}

#[test]
fn test_not_false() {
    assert_eq!(evaluate_expression("not false").unwrap(), Value::Bool(true));
}

// Short-circuit evaluation tests
#[test]
fn test_and_short_circuit() {
    let mut ctx = Context::new();
    // false and undefined_var should return false without evaluating undefined_var
    let result = evaluate_expression_with_context("false and undefined_var", &mut ctx);
    assert_eq!(result.unwrap(), Value::Bool(false));
}

#[test]
fn test_or_short_circuit() {
    let mut ctx = Context::new();
    // true or undefined_var should return true without evaluating undefined_var
    let result = evaluate_expression_with_context("true or undefined_var", &mut ctx);
    assert_eq!(result.unwrap(), Value::Bool(true));
}

// Truthy value tests (Python-style return values)
#[test]
fn test_and_truthy_values() {
    // 1 and 2 -> 2 (returns last evaluated value)
    assert_eq!(evaluate_expression("1 and 2").unwrap(), Value::Int(2));
    // 0 and 2 -> 0 (returns first falsy value)
    assert_eq!(evaluate_expression("0 and 2").unwrap(), Value::Int(0));
}

#[test]
fn test_or_truthy_values() {
    // 1 or 2 -> 1 (returns first truthy value)
    assert_eq!(evaluate_expression("1 or 2").unwrap(), Value::Int(1));
    // 0 or 2 -> 2 (returns last evaluated value when first is falsy)
    assert_eq!(evaluate_expression("0 or 2").unwrap(), Value::Int(2));
}

#[test]
fn test_not_truthy_values() {
    // not 0 -> true (0 is falsy)
    assert_eq!(evaluate_expression("not 0").unwrap(), Value::Bool(true));
    // not 1 -> false (1 is truthy)
    assert_eq!(evaluate_expression("not 1").unwrap(), Value::Bool(false));
    // not "" -> true (empty string is falsy)
    assert_eq!(evaluate_expression("not \"\"").unwrap(), Value::Bool(true));
    // not "hello" -> false (non-empty string is truthy)
    assert_eq!(
        evaluate_expression("not \"hello\"").unwrap(),
        Value::Bool(false)
    );
}

// Precedence tests
#[test]
fn test_or_and_precedence() {
    // true or false and false -> true (and binds tighter than or)
    // Equivalent to: true or (false and false) = true or false = true
    assert_eq!(
        evaluate_expression("true or false and false").unwrap(),
        Value::Bool(true)
    );
}

#[test]
fn test_not_and_precedence() {
    // not false and true -> true (not binds tighter than and)
    // Equivalent to: (not false) and true = true and true = true
    assert_eq!(
        evaluate_expression("not false and true").unwrap(),
        Value::Bool(true)
    );
}

#[test]
fn test_comparison_and_precedence() {
    // 1 < 2 and 3 < 4 -> true (comparison has higher precedence than and)
    assert_eq!(
        evaluate_expression("1 < 2 and 3 < 4").unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        evaluate_expression("1 < 2 and 3 > 4").unwrap(),
        Value::Bool(false)
    );
}

// Double not test
#[test]
fn test_not_not() {
    assert_eq!(
        evaluate_expression("not not true").unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        evaluate_expression("not not false").unwrap(),
        Value::Bool(false)
    );
}

// Unary minus tests
#[test]
fn test_unary_minus_integer() {
    assert_eq!(evaluate_expression("-5").unwrap(), Value::Int(-5));
}

#[test]
fn test_unary_minus_float() {
    assert_eq!(evaluate_expression("-5.5").unwrap(), Value::Float(-5.5));
}

#[test]
fn test_unary_minus_in_expression() {
    // 2 * -3 = -6
    assert_eq!(evaluate_expression("2 * -3").unwrap(), Value::Int(-6));
}

#[test]
fn test_unary_minus_double_negation() {
    // --5 = 5
    assert_eq!(evaluate_expression("--5").unwrap(), Value::Int(5));
}

#[test]
fn test_unary_minus_precedence_with_power() {
    // -2 ** 3 should be -(2 ** 3) = -8, not (-2) ** 3 = -8
    // (Both happen to equal -8, so test with -2 ** 2 instead)
    // -2 ** 2 = -(2 ** 2) = -4, not (-2) ** 2 = 4
    // Note: ** returns Float, so we check for Float(-4.0)
    assert_eq!(evaluate_expression("-2 ** 2").unwrap(), Value::Float(-4.0));
}

#[test]
fn test_unary_minus_i64_min_overflow() {
    // Negating i64::MIN should fall back to float since -i64::MIN overflows
    // Note: We can't write i64::MIN as a literal because the lexer can't parse
    // 9223372036854775808 as i64 (it's larger than i64::MAX).
    // Instead, we construct i64::MIN via arithmetic and test negation via context.
    let mut ctx = Context::new();
    ctx.set("x".to_string(), Value::Int(i64::MIN));
    let result = evaluate_expression_with_context("-x", &mut ctx).unwrap();
    // Since -i64::MIN overflows, it should become a float
    match result {
        Value::Float(f) => {
            // Negating i64::MIN (-9223372036854775808) should give 9223372036854775808.0
            assert!((f - 9223372036854775808.0).abs() < 1e10);
        }
        _ => panic!("Expected Float for negation of i64::MIN, got {:?}", result),
    }
}

#[test]
fn test_unary_minus_type_error() {
    // Negating a boolean should produce a type error
    let result = evaluate_expression("-true");
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("cannot negate"));
}

// Complex expressions
#[test]
fn test_complex_logical_expression() {
    // (1 < 2) and (3 > 2) or false
    // = true and true or false
    // = true or false
    // = true
    assert_eq!(
        evaluate_expression("1 < 2 and 3 > 2 or false").unwrap(),
        Value::Bool(true)
    );
}

// Logical operators in if statements
#[test]
fn test_logical_in_if_statement() {
    let mut ctx = Context::new();
    let input = "x = 5\nresult = 0\nif x > 3 and x < 10:\n    result = 1\n";
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(1)));
}

#[test]
fn test_or_in_if_statement() {
    let mut ctx = Context::new();
    let input = "x = 15\nresult = 0\nif x < 3 or x > 10:\n    result = 1\n";
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(1)));
}

#[test]
fn test_not_in_if_statement() {
    let mut ctx = Context::new();
    let input = "flag = false\nresult = 0\nif not flag:\n    result = 1\n";
    execute_test_program(input, &mut ctx).unwrap();
    assert_eq!(ctx.get("result"), Some(&Value::Int(1)));
}

// Chained logical operators
#[test]
fn test_chained_and() {
    assert_eq!(
        evaluate_expression("true and true and true").unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        evaluate_expression("true and false and true").unwrap(),
        Value::Bool(false)
    );
}

#[test]
fn test_chained_or() {
    assert_eq!(
        evaluate_expression("false or false or true").unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        evaluate_expression("false or false or false").unwrap(),
        Value::Bool(false)
    );
}
