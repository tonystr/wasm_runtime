#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Value {
    I32(i32),
    I64(i64),
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::I32(value)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::I64(value)
    }
}

/// Adds two same-type numbers
/// 
/// # Example
/// ```
/// use crate::wasm_runtime::execution::value::Value;
/// assert_eq!(Value::I32(5) + Value::I32(8), Value::I32(13)); // 5 + 8 = 13
/// assert_ne!(Value::I64(8) + Value::I64(8), Value::I64(10)); // 8 + 8 != 10
/// ```
/// 
/// # Mixed types panic
/// ```should_panic
/// use crate::wasm_runtime::execution::value::Value;
/// let _ = Value::I32(5) + Value::I64(3);
/// ```
impl std::ops::Add for Value {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::I32(left), Value::I32(right)) => Value::I32(left + right),
            (Value::I64(left), Value::I64(right)) => Value::I64(left + right),
            _ => panic!("type mismatch")
        }
    }
}
