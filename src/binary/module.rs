use crate::binary::types::{Import, ImportDesc};

use super::{
    instruction::Instruction,
    opcode::Opcode,
    section::{Function, SectionCode},
    types::{Export, ExportDesc, FuncType, FunctionLocal, ValueType},
};
use nom::{
    IResult,
    bytes::complete::{ tag, take },
    multi::many0,
    number::complete::{ le_u32, le_u8 },
    sequence::pair,
};
use nom_leb128::{leb128_i32, leb128_u32};
use num_traits::FromPrimitive as _;

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub magic: String,
    pub version: u32,
    pub type_section: Option<Vec<FuncType>>,    // List of signatures
    pub function_section: Option<Vec<u32>>,     // List of signature ids
    pub code_section: Option<Vec<Function>>,
    pub export_section: Option<Vec<Export>>,
    pub import_section: Option<Vec<Import>>,
}

impl Default for Module {
    fn default() -> Self {
        Self {
            magic: "\0asm".to_string(),
            version: 1,
            type_section: None,
            function_section: None,
            code_section: None,
            export_section: None,
            import_section: None,
        }
    }
}

impl Module {
    pub fn new(input: &[u8]) -> anyhow::Result<Module> {
        let (_, module) = Module::decode(input).map_err(|e| anyhow::anyhow!("failed to parse wasm: {}", e))?;
        Ok(module)
    }

    fn decode(input: &[u8]) -> IResult<&[u8], Module> {
        // parse (consume) "\0asm" or error
        let (input, _) = tag(&b"\0asm"[..])(input)?;
        // parse little endian u32 (4 bytes, lowest byte first) or error
        let (input, version) = le_u32(input)?;

        let mut module = Module {
            magic: "\0asm".into(),
            version,
            ..Default::default()
        };

        let mut remaining = input;

        while !remaining.is_empty() {
            match decode_section_header(remaining) {
                Ok((input, (code, size))) => {
                    let (rest, section_contents) = take(size)(input)?;

                    match code {
                        SectionCode::Custom => {
                            // skip
                        }
                        SectionCode::Type => {
                            let (_, types) = decode_type_section(section_contents)?;
                            module.type_section = Some(types);
                        }
                        SectionCode::Function => {
                            let (_, func_ids) = decode_function_section(section_contents)?;
                            module.function_section = Some(func_ids);
                        }
                        SectionCode::Code => {
                            let (_, funcs) = decode_code_section(section_contents)?;
                            module.code_section = Some(funcs);
                        }
                        SectionCode::Export => {
                            let (_, exports) = decode_export_section(section_contents)?;
                            module.export_section = Some(exports);
                        }
                        SectionCode::Import => {
                            let (_, imports) = decode_import_section(section_contents)?;
                            module.import_section = Some(imports);
                        }
                        _ => todo!(),
                    };

                    remaining = rest;
                }
                Err(err) => return Err(err),
            }
        }

        Ok((input, module))
    }
}

fn decode_section_header(input: &[u8]) -> IResult<&[u8], (SectionCode, u32)> {
    let (input, (code, size)) = pair(le_u8, leb128_u32)(input)?;

    Ok((
        input,
        (
            SectionCode::from_u8(code).expect("unexpected section code"),
            size,
        ),
    ))
}

fn decode_value_type(input: &[u8]) -> IResult<&[u8], ValueType> {
    let (input, value_type) = le_u8(input)?;
    Ok((input, value_type.into()))
}

fn decode_type_section(input: &[u8]) -> IResult<&[u8], Vec<FuncType>> {
    let mut func_types: Vec<FuncType> = vec![];

    let (mut input, count) = leb128_u32(input)?;

    for _ in 0..count {
        let (rest, _) = le_u8(input)?;
        let mut func = FuncType::default();

        // Parse param types
        let (rest, size) = leb128_u32(rest)?;
        let (rest, types) = take(size)(rest)?;
        let (_, types) = many0(decode_value_type)(types)?;
        func.params = types;

        // Parse return types
        let (rest, size) = leb128_u32(rest)?;
        let (rest, types) = take(size)(rest)?;
        let (_, types) = many0(decode_value_type)(types)?;
        func.results = types;

        func_types.push(func);
        input = rest;
    }

    Ok((&[], func_types))
}

fn decode_function_section(input: &[u8]) -> IResult<&[u8], Vec<u32>> {
    let (mut input, func_count) = leb128_u32(input)?;
    let mut func_ids = Vec::<u32>::new();

    for _ in 0..func_count {
        let (rest, id) = leb128_u32(input)?;
        func_ids.push(id);
        input = rest;
    }

    Ok((input, func_ids))
}

fn decode_code_section(input: &[u8]) -> IResult<&[u8], Vec<Function>> {
    let mut functions: Vec<Function> = vec![];

    let (mut input, count) = leb128_u32(input)?;

    for _ in 0..count {
        let (rest, size) = leb128_u32(input)?;
        let (rest, body) = take(size)(rest)?;
        let (_, body) = decode_function_body(body)?;
        functions.push(body);
        input = rest;
    }

    Ok((&[], functions))
}

fn decode_function_body(input: &[u8]) -> IResult<&[u8], Function> {
    let mut body = Function::default();

    let (mut input, decl_count) = leb128_u32(input)?;

    for _ in 0..decl_count {
        let (rest, type_count) = leb128_u32(input)?;
        let (rest, value_type) = decode_value_type(rest)?;
        body.locals.push(FunctionLocal {
            type_count,
            value_type,
        });
        input = rest;
    }

    let mut remaining = input;

    while !remaining.is_empty() {
        let (rest, inst) = decode_instructions(remaining)?;
        body.code.push(inst);
        remaining = rest;
    }

    Ok((&[], body))
}

fn decode_instructions(input: &[u8]) -> IResult<&[u8], Instruction> {
    let (input, byte) = le_u8(input)?;
    let op = Opcode::from_u8(byte).unwrap_or_else(|| panic!("invalid opcode: {:X}", byte));

    let (rest, inst) = match op {
        Opcode::LocalGet => {
            let (rest, index) = leb128_u32(input)?;
            (rest, Instruction::LocalGet(index))
        }
        Opcode::LocalSet => {
            let (rest, index) = leb128_u32(input)?;
            (rest, Instruction::LocalSet(index))
        }
        Opcode::I32Const => {
            let (rest, value) = leb128_i32(input)?;
            (rest, Instruction::I32Const(value))
        }
        Opcode::I32Add => (input, Instruction::I32Add),
        Opcode::End => (input, Instruction::End),
        Opcode::Call => {
            let (rest, func_index) = leb128_u32(input)?;
            (rest, Instruction::Call(func_index))
        },
    };

    Ok((rest, inst))
}

fn decode_export_section(input: &[u8]) -> IResult<&[u8], Vec<Export>> {
    let (mut input, count) = leb128_u32(input)?;
    let mut exports = vec![];

    for _ in 0..count {
        let (rest, name) = decode_name(input)?;
        let (rest, export_kind) = le_u8(rest)?;
        let (rest, index) = leb128_u32(rest)?;
        let desc = match export_kind {
            0x00 => ExportDesc::Func(index),
            _ => unimplemented!("unsupported export kind: {:X}", export_kind),
        };
        exports.push(Export { name, desc });

        input = rest;
    }

    Ok((input, exports))
}

fn decode_import_section(input: &[u8]) -> IResult<&[u8], Vec<Import>> {
    let (mut input, count) = leb128_u32(input)?;
    let mut imports = vec![];

    for _ in 0..count {
        let (rest, module) = decode_name(input)?;
        let (rest, field) = decode_name(rest)?;
        let (rest, import_kind) = le_u8(rest)?;
        let (rest, desc) = match import_kind {
            0x00 => {
                let (rest, index) = leb128_u32(rest)?;
                (rest, ImportDesc::Func(index))
            }
            _ => unimplemented!("unsupported import kind: {:X}", import_kind),
        };
        imports.push(Import { module, field, desc });
        input = rest;
    }

    Ok((input, imports))
}

/// # Decode "name" `string`
/// * (1) Decode `length` leb128 u32 number  
/// * (2) Decode `name` utf8 string, `length` bytes long  
fn decode_name(input: &[u8]) -> IResult<&[u8], String> {
    let (rest, name_len) = leb128_u32(input)?;
    let (rest, name_bytes) = take(name_len)(rest)?;
    Ok((
        rest,
        String::from_utf8(name_bytes.to_vec()).expect("invalid utf-8 string"),
    ))
}

#[cfg(test)]
mod tests {
    use crate::binary::{
        instruction::Instruction,
        module::Module,
        section::Function,
        types::{Export, ExportDesc, FuncType, FunctionLocal, Import, ImportDesc, ValueType},
    };
    use anyhow::Result;

    #[test]
    fn decode_simplest_module() -> Result<()> {
        // Generate wasm binary with only preamble present
        let wasm = wat::parse_str("(module)")?;
        // Decode binary and generate Module structure
        let module = Module::new(&wasm)?;
        // Compare whether the generated Module structure is as expected
        assert_eq!(module, Module::default());
        Ok(())
    }

    #[test]
    fn decode_simplest_function() -> Result<()> {
        let wasm = wat::parse_str("(module (func))")?;
        let module = Module::new(&wasm)?;
        assert_eq!(
            module,
            Module {
                type_section: Some(vec![FuncType::default()]),
                function_section: Some(vec![0]),
                code_section: Some(vec![Function {
                    locals: vec![],
                    code: vec![Instruction::End],
                }]),
                ..Default::default()
            }
        );
        Ok(())
    }

    #[test]
    fn decode_function_parameters() -> Result<()> {
        let wasm = wat::parse_str("(module (func (param i32 i64)))")?;
        let module = Module::new(&wasm)?;

        assert_eq!(
            module,
            Module {
                type_section: Some(vec![FuncType {
                    params: vec![ValueType::I32, ValueType::I64],
                    results: vec![],
                }]),
                function_section: Some(vec![0]),
                code_section: Some(vec![Function {
                    locals: vec![],
                    code: vec![Instruction::End],
                }]),
                ..Default::default()
            }
        );

        Ok(())
    }

    #[test]
    fn decode_function_locals() -> Result<()> {
        let wasm = wat::parse_str("(module (func (local i32) (local i64 i64)))")?;
        let module = Module::new(&wasm)?;
        assert_eq!(
            module,
            Module {
                type_section: Some(vec![FuncType::default()]),
                function_section: Some(vec![0]),
                code_section: Some(vec![Function {
                    locals: vec![
                        FunctionLocal {
                            value_type: ValueType::I32,
                            type_count: 1,
                        },
                        FunctionLocal {
                            value_type: ValueType::I64,
                            type_count: 2,
                        }
                    ],
                    code: vec![Instruction::End],
                }]),
                ..Default::default()
            }
        );
        Ok(())
    }

    #[test]
    fn decode_function_add() -> Result<()> {
        let wasm = wat::parse_str("(module (func (export \"add\") (param i32 i32) (result i32) (local.get 0) (local.get 1) i32.add))")?;
        let module = Module::new(&wasm)?;
        assert_eq!(
            module,
            Module {
                type_section: Some(vec![FuncType {
                    params: vec![ValueType::I32, ValueType::I32],
                    results: vec![ValueType::I32]
                }]),
                function_section: Some(vec![0]),
                code_section: Some(vec![Function {
                    locals: vec![],
                    code: vec![
                        Instruction::LocalGet(0),
                        Instruction::LocalGet(1),
                        Instruction::I32Add,
                        Instruction::End,
                    ],
                }]),
                export_section: Some(vec![Export {
                    name: "add".into(),
                    desc: ExportDesc::Func(0),
                }]),
                ..Default::default()
            }
        );
        Ok(())
    }

    #[test]
    fn decode_function_call() -> Result<()> {
        let wasm = wat::parse_str("(module
            (func (export \"call_doubler\") (param i32) (result i32)
                (local.get 0)
                (call $double))
            (func $double (param i32) (result i32)
                (local.get 0)
                (local.get 0)
                i32.add))
        ")?;
        let module = Module::new(&wasm)?;
        assert_eq!(
            module,
            Module {
                type_section: Some(vec![FuncType {
                    params: vec![ValueType::I32],
                    results: vec![ValueType::I32],
                }]),
                function_section: Some(vec![0, 0]),
                code_section: Some(vec![
                    Function {
                        locals: vec![],
                        code: vec![
                            Instruction::LocalGet(0),
                            Instruction::Call(1),
                            Instruction::End,
                        ],
                    },
                    Function {
                        locals: vec![],
                        code: vec![
                            Instruction::LocalGet(0),
                            Instruction::LocalGet(0),
                            Instruction::I32Add,
                            Instruction::End,
                        ],
                    }
                ]),
                export_section: Some(vec![Export {
                    name: "call_doubler".into(),
                    desc: ExportDesc::Func(0),
                }]),
                ..Default::default()
            }
        );

        Ok(())
    }

    #[test]
    fn decode_import_section() -> Result<()> {
        let wasm = wat::parse_str("(module
            (func $add (import \"env\" \"add\") (param i32) (result i32))
            (func (export \"call_add\") (param i32) (result i32)
                (local.get 0)
                (call $add)))
        ")?;
        let module = Module::new(&wasm)?;
        assert_eq!(
            module,
            Module {
                type_section: Some(vec![FuncType {
                    params: vec![ValueType::I32],
                    results: vec![ValueType::I32]
                }]),
                import_section: Some(vec![Import {
                    module: "env".into(),
                    field: "add".into(),
                    desc: ImportDesc::Func(0)
                }]),
                export_section: Some(vec![Export {
                    name: "call_add".into(),
                    desc: ExportDesc::Func(1)
                }]),
                function_section: Some(vec![0]),
                code_section: Some(vec![Function {
                    locals: vec![],
                    code: vec![
                        Instruction::LocalGet(0),
                        Instruction::Call(0),
                        Instruction::End,
                    ],
                }]),
                ..Default::default()
            }
        );
        Ok(())
    }
}
