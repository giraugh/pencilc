use crate::{error::CodegenError, tyc::tir};
use inkwell::{builder::Builder, context::Context, module::Module};

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    modules: Vec<Module<'ctx>>,
}

type Result<T> = std::result::Result<T, CodegenError>;

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        Codegen {
            builder: context.create_builder(),
            modules: Vec::new(),
            context,
        }
    }

    pub fn compile(&mut self, root_module: tir::Module) -> Result<()> {
        // Create LLVM module
        let mut module = self.context.create_module(&root_module.name);

        // Compile each function
        for function in root_module.functions {
            self.compile_function(function);
        }

        Ok(())
    }

    fn compile_function(&mut self, fn_node: tir::FnDef) -> Result<()> {
        todo!()
    }
}
