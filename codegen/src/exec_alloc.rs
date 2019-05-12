/// This module is used as part of the lowering to allocate memory pages, copy
/// the code into them, patch the code based on relocation informations, and map
/// these pages as executable.
use mmap::*;
use region;
use region::Protection;
use error::LowerResult;
use codegen::binemit::{RelocSink, CodeOffset, Reloc, Addend};
use codegen::ir;

/// This structure hold the code while it is being written down in memory, and
/// mutated based on relocation informations provided by Cranelift.
pub struct WrittableCode {
    page: MemoryMap,
}

/// This structure hold the code in a executable-only state (readable if
/// needed), and with no way to convert it back to a writtable format. The
/// implicit Drop trait implemented for it will discard the executable code as
/// soon as there is no more references to it.
pub struct ExecutableCode {
    page: MemoryMap,
}

impl WrittableCode {
    /// Allocate a new set of pages in which we can copy the result of a
    /// compilation into.
    pub fn with_capacity(size: usize) -> LowerResult<WrittableCode> {
        Ok(WrittableCode {
            page: MemoryMap::new(size, &[ MapOption::MapWritable ])?,
        })
    }

    /// Get a mutable pointer to write the code into.
    pub fn as_ptr(&mut self) -> *mut u8 {
        // Note: Based on mmap.data(), we only require a &self argument.
        self.page.data()
    }

    /// Map the pages as executable, and replace the write permission by an
    /// executable permission. Return the executable code.
    pub fn make_executable(self, _reloc: NullRelocSink, _trap: NullTrapSink) -> LowerResult<ExecutableCode> {
        let WrittableCode { page } = self;
        unsafe { region::protect(page.data(), page.len(), Protection::ReadExecute)?; }
        Ok(ExecutableCode { page })
    }
}

impl ExecutableCode {
    pub fn as_ptr(&self) -> *const u8 {
        self.page.data()
    }
}

pub use codegen::binemit::NullTrapSink;
pub struct NullRelocSink {}

impl RelocSink for NullRelocSink {
    fn reloc_ebb(&mut self, _offset: CodeOffset, _reloc: Reloc, _ebb_offset: CodeOffset) {
        unimplemented!();
    }

    fn reloc_external(&mut self, _offset: CodeOffset, _reloc: Reloc, _name: &ir::ExternalName, _addend: Addend) {
        unimplemented!();
    }

    fn reloc_jt(&mut self, _offset: CodeOffset, _reloc: Reloc, _jt: ir::JumpTable) {
        unimplemented!();
    }
}
