use crate::execution::store::Store;
use crate::execution::value::Value;
use anyhow::{anyhow, Result};
use std::fs::File;
use std::io::prelude::*;
#[cfg(unix)]
use std::os::fd::FromRawFd;
#[cfg(windows)]
use std::os::windows::io::{FromRawHandle, RawHandle};
use winapi::um::processenv::GetStdHandle;
use winapi::um::winbase::{STD_INPUT_HANDLE, STD_OUTPUT_HANDLE, STD_ERROR_HANDLE};

#[derive(Default)]
pub struct WasiSnapshotPreview1 {
    pub file_table: Vec<Box<File>>,
}

impl WasiSnapshotPreview1 {
    pub fn new() -> Self {
        #[cfg(unix)]
        unsafe {
            Self {
                file_table: vec![
                    Box::new(File::from_raw_fd(0)),
                    Box::new(File::from_raw_fd(1)),
                    Box::new(File::from_raw_fd(2)),
                ],
            }
        }
        #[cfg(windows)]
        unsafe {
            Self {
                file_table: vec![
                    Box::new(File::from_raw_handle(GetStdHandle(STD_INPUT_HANDLE) as RawHandle)),
                    Box::new(File::from_raw_handle(GetStdHandle(STD_OUTPUT_HANDLE) as RawHandle)),
                    Box::new(File::from_raw_handle(GetStdHandle(STD_ERROR_HANDLE) as RawHandle)),
                ],
            }
        }
    }

    pub fn invoke(
        &mut self,
        store: &mut Store,
        func: &str,
        args: Vec<Value>,
    ) -> Result<Option<Value>> {
        match func {
            "fd_write" => self.fd_write(store, args),
            _ => unimplemented!("{}", func),
        }
    }

    fn fd_write(&mut self, store: &mut Store, args: Vec<Value>) -> Result<Option<Value>>{
        let args: Vec<i32> = args.into_iter().map(Into::into).collect();

        let fd = args[0];
        let mut iovs = args[1] as usize;
        let iovs_len = args[2];
        let rp = args[3] as usize;

        let file = self.file_table
            .get_mut(fd as usize)
            .ok_or(anyhow!("fd not found"))?;

        let memory = store.memories
            .get_mut(0)
            .ok_or(anyhow!("memory not found"))?;

        let mut nwritten = 0;

        for _ in 0..iovs_len {
            let start = memory_read_i32(&memory.data, iovs)? as usize;
            iovs += 4;

            let len: i32 = memory_read_i32(&memory.data, iovs)?;
            iovs += 4;

            let end = start + len as usize;
            nwritten += file.write(&memory.data[start..end])?;
        }

        memory_write(&mut memory.data, rp, &nwritten.to_le_bytes())?;

        Ok(Some(0.into()))
    }
}

fn memory_read_i32(buf: &[u8], start: usize) -> Result<i32> {
    let end = start + 4;
    Ok(<i32>::from_le_bytes(buf[start..end].try_into()?))
}

fn memory_write(buf: &mut [u8], start: usize, data: &[u8]) -> Result<()> {
    let end = start + data.len();
    buf[start..end].copy_from_slice(data);
    Ok(())
}
