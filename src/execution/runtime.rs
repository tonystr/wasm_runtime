use crate::{
    binary::{instruction::Instruction, module::Module, types::{ExportDesc, ValueType}},
    execution::{
        import::Import,
        store::{ExternalFuncInst, FuncInst, InternalFuncInst, Store},
        value::Value,
    },
};
use anyhow::{Result, anyhow, bail};

/// Stack frame which usually represents a function call
#[derive(Default)]
pub struct Frame {
    pub pc: isize,                  // program counter
    pub sp: usize,                  // stack pointer (base pointer?)
    pub insts: Vec<Instruction>,    // Instructions
    pub arity: usize,               // Num of return values
    pub locals: Vec<Value>,         // Local variables
}

/// WASM runtime for decoding and executing wasm bytecode
#[derive(Default)]
pub struct Runtime {
    pub store: Store,
    pub stack: Vec<Value>,
    pub call_stack: Vec<Frame>,
    pub import: Import,
}

impl Runtime {
    /// Parses wasm bytecode into static structures needed to execute code
    pub fn instantiate(wasm: impl AsRef<[u8]>) -> Result<Self> {
        let module = Module::new(wasm.as_ref())?;
        let store = Store::new(module)?;

        Ok(Self {
            store,
            ..Default::default()
        })
    }

    pub fn add_import(
        &mut self,
        module_name: impl Into<String>,
        func_name: impl Into<String>,
        func: impl FnMut(&mut Store, Vec<Value>) -> Result<Option<Value>> + 'static,
    ) -> Result<()> {
        let import = self.import.entry(module_name.into()).or_default();
        import.insert(func_name.into(), Box::new(func));
        Ok(())
    }

    /// Starts an execute() loop, calling `name` exported function as the initial stack frame
    pub fn call(&mut self, name: impl Into<String>, args: Vec<Value>) -> Result<Option<Value>> {
        let index = match self.store.module.exports
            .get(&name.into())
            .ok_or(anyhow!("export function not found"))?
            .desc
        {
            ExportDesc::Func(index) => index as usize,
        };
        let Some(func_inst) = self.store.funcs.get(index) else {
            bail!("func not found");
        };
        for arg in args {
            self.stack.push(arg);
        }
        match func_inst {
            FuncInst::Internal(func) => self.invoke_internal(func.clone()),
            FuncInst::External(func) => self.invoke_external(func.clone()),
        }
    }

    /// Pushes a frame onto the stack. Useful when already in an execute() loop
    fn push_frame(&mut self, func: &InternalFuncInst) {
        let bottom = self.stack.len() - func.func_type.params.len();
        let mut locals = self.stack.split_off(bottom);

        for local in func.code.locals.iter() {
            match local {
                ValueType::I32 => locals.push(Value::I32(0)),
                ValueType::I64 => locals.push(Value::I64(0)),
            }
        }

        let arity = func.func_type.results.len();

        let frame = Frame {
            pc: -1,
            sp: self.stack.len(),
            insts: func.code.body.clone(),
            arity,
            locals,
        };

        self.call_stack.push(frame);
    }

    /// Invokes internal function instance in an execute() loop
    fn invoke_internal(&mut self, func: InternalFuncInst) -> Result<Option<Value>> {
        let arity = func.func_type.results.len();
        self.push_frame(&func);

        if let Err(e) = self.execute() {
            self.cleanup();
            bail!("failed to execute instructions: {}", e);
        }

        if arity > 0 {
            let Some(value) = self.stack.pop() else {
                bail!("Finished with non-existing return value. Aborting...")
            };
            return Ok(Some(value));
        }

        Ok(None)
    }

    fn invoke_external(&mut self, func: ExternalFuncInst) -> Result<Option<Value>> {
        let args = self.stack
            .split_off(self.stack.len() - func.func_type.params.len());
        let import_func = self.import
            .get_mut(&func.module)
            .ok_or(anyhow!("module not found"))?
            .get_mut(&func.func)
            .ok_or(anyhow!("func not found"))?;

        import_func(&mut self.store, args)
    }

    /// Execute call stack
    pub fn execute(&mut self) -> Result<()> {
        loop {
            let Some(frame) = self.call_stack.last_mut() else {
                break;
            };

            frame.pc += 1;

            let Some(instruction) = frame.insts.get(frame.pc as usize) else {
                break;
            };

            match instruction {
                Instruction::LocalGet(index) => {
                    let Some(value) = frame.locals.get(*index as usize) else {
                        bail!("Tried to get non-existent local");
                    };
                    self.stack.push(*value);
                }
                Instruction::LocalSet(index) => {
                    let Some(value) = self.stack.pop() else {
                        bail!("no value found in stack");
                    };
                    frame.locals[*index as usize] = value;
                }
                Instruction::I32Const(value) => self.stack.push(Value::I32(*value)),
                Instruction::I32Add => {
                    let (Some(right), Some(left)) = (self.stack.pop(), self.stack.pop()) else {
                        bail!("Tried to pop non-existent value from stack");
                    };
                    let result = left + right;
                    self.stack.push(result);
                }
                Instruction::End => {
                    let Some(frame) = self.call_stack.pop() else {
                        bail!("Stack frame not found");
                    };
                    let Frame { sp, arity, .. } = frame;
                    stack_unwind(&mut self.stack, sp, arity)?;
                }
                Instruction::Call(index) => {
                    let Some(func_inst) = self.store.funcs.get(*index as usize) else {
                        bail!("Tried to call non-existing function by index");
                    };
                    match func_inst.clone() {
                        FuncInst::Internal(func) => self.push_frame(&func),
                        FuncInst::External(func) => {
                            if let Some(value) = self.invoke_external(func)? {
                                self.stack.push(value);
                            }
                        }
                    }
                }
                _ => todo!(),
            }
        }

        Ok(())
    }

    /// Resets stack and callstack
    fn cleanup(&mut self) {
        self.stack = vec![];
        self.call_stack = vec![];
    }
}

pub fn stack_unwind(stack: &mut Vec<Value>, sp: usize, arity: usize) -> Result<()> {
    if arity > 0 {
        let Some(value) = stack.pop() else {
            bail!("No return value found while unwinding stack with arity > 0");
        };
        stack.drain(sp..);
        stack.push(value);
    } else {
        stack.drain(sp..);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::Runtime;
    use crate::execution::value::Value;
    use anyhow::Result;

    #[test]
    fn execute_i32_add() -> Result<()> {
        let wasm = wat::parse_str("(module\
            (func (export \"add\") (param i32 i32) (result i32)\
                (local.get 0)\
                (local.get 1)\
                i32.add))\
        ")?;
        let mut runtime = Runtime::instantiate(wasm)?;
        let test = vec![(2, 3, 5), (10, 5, 15), (1, 1, 2)];

        for (left, right, expected) in test {
            let args = vec![Value::I32(left), Value::I32(right)];
            let result = runtime.call("add", args)?;
            assert_eq!(result, Some(Value::I32(expected)));
        }

        Ok(())
    }

    #[test]
    fn export_function_not_found() -> Result<()> {
        let wasm = wat::parse_str("(module (func (export \"add\")))")?;
        let mut runtime = Runtime::instantiate(wasm)?;
        let result = runtime.call("NONEXISTENT_FUNCTION", vec![]);
        assert!(result.is_err());

        Ok(())
    }

    #[test]
    fn function_call() -> Result<()> {
        let wasm = wat::parse_str("(module
            (func (export \"call_doubler\") (param i32) (result i32)
                (local.get 0)
                (call $double))
            (func $double (param i32) (result i32)
                (local.get 0)
                (local.get 0)
                i32.add))
        ")?;
        let mut runtime = Runtime::instantiate(wasm)?;
        let tests = vec![(2, 4), (10, 20), (1, 2)];

        for (arg, target) in tests {
            let result = runtime.call("call_doubler", vec![Value::I32(arg)])?;
            assert_eq!(result, Some(Value::I32(target)));
        }

        Ok(())
    }

    #[test]
    fn call_imported_function() -> Result<()> {
        let wasm = wat::parse_str("(module
            (func $add (import \"env\" \"add\") (param i32) (result i32))
            (func (export \"call_add\") (param i32) (result i32)
                (local.get 0)
                (call $add)))
        ")?;
        let mut runtime = Runtime::instantiate(wasm)?;
        runtime.add_import("env", "add", |_, args| {
            let arg = args[0];
            Ok(Some(arg + arg))
        })?;
        let tests = vec![(2, 4), (10, 20), (1, 2)];

        for (arg, target) in tests {
            let args = vec![Value::I32(arg)];
            let result = runtime.call("call_add", args)?;
            assert_eq!(result, Some(Value::I32(target)));
        }
        Ok(())
    }

    #[test]
    fn imported_function_not_found() -> Result<()> {
        let wasm = wat::parse_str("(module
            (func $add (import \"env\" \"add\") (param i32) (result i32))
            (func (export \"call_add\") (param i32) (result i32)
                (local.get 0)
                (call $add)))
        ")?;
        let mut runtime = Runtime::instantiate(wasm)?;
        runtime.add_import("env", "fooooo", |_, _| Ok(None))?;
        let result = runtime.call("nonexistent_function", vec![Value::I32(1)]);
        assert!(result.is_err());
        Ok(())
    }

    #[test]
    fn i32_const() -> Result<()> {
        let wasm = wat::parse_str("(module
            (func $i32_const (result i32)
                (i32.const 42))
            (export \"i32_const\" (func $i32_const)))
        ")?;
        let mut runtime = Runtime::instantiate(wasm)?;
        let result = runtime.call("i32_const", vec![])?;
        assert_eq!(result, Some(Value::I32(42)));
        Ok(())
    }

    #[test]
    fn local_set() -> Result<()> {
        let wasm = wat::parse_str("(module
            (func $local_set (result i32)
                (local $x i32)
                (local.set $x (i32.const 42))
                (local.get 0))
            (export \"local_set\" (func $local_set)))
        ")?;
        let mut runtime = Runtime::instantiate(wasm)?;
        let result = runtime.call("local_set", vec![])?;
        assert_eq!(result, Some(Value::I32(42)));
        Ok(())
    }
}
