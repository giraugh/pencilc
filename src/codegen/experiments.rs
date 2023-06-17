use std::path::Path;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    /// Manually create a test function `foo` that adds two i64s
    pub fn build_foo_fn(&self) -> Option<()> {
        // Get the i64 type
        let i64_type = self.context.i64_type();

        // Create a function type
        let foo_fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into()], false);

        // Create a function
        // We provide no linkage
        let foo_fn = self.module.add_function("foo", foo_fn_type, None);

        // Create a basic block and append it to the function
        let basic_block = self.context.append_basic_block(foo_fn, "entry");

        // Point the builder at the end of the block
        self.builder.position_at_end(basic_block);

        // Get values for function params
        let x = foo_fn.get_nth_param(0)?.into_int_value();
        let y = foo_fn.get_nth_param(1)?.into_int_value();

        // Create instruction to add values into `int` temporary
        let sum = self.builder.build_int_add(x, y, "sum");

        // Create instruction to return sum value
        self.builder.build_return(Some(&sum));

        Some(())
    }

    pub fn write_bitcode(&self) {
        self.module.write_bitcode_to_path(Path::new("foo.bc"));
    }
}

#[test]
fn test_stuff() {
    let context = Context::create();
    let module = context.create_module("foo");
    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder(),
    };
    codegen.build_foo_fn();
    codegen.write_bitcode();
}
