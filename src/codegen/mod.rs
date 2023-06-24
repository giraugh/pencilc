use std::{collections::HashMap, env, path::Path, rc::Rc, sync::RwLock};

use crate::{
    ast::{BinaryOpt, UnaryOpt},
    error::CodegenError,
    id::{Idx, NameId},
    lex::LiteralValue,
    tyc::{
        tir,
        ty::{self, PrimitiveTy, Ty},
    },
};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum},
    values::{BasicValueEnum, FunctionValue, PointerValue},
};

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    name_ptr_map: Rc<RwLock<HashMap<NameId, PointerValue<'ctx>>>>,
}

type Result<T> = std::result::Result<T, CodegenError>;

impl<'ctx> Codegen<'ctx> {
    pub fn emit_llvm_ir(&self, output_path: &Path) {
        self.module.print_to_file(output_path).unwrap();
    }

    pub fn emit_llvm_bc(&self, output_path: &Path) {
        self.module.write_bitcode_to_path(output_path);
    }

    pub fn emit_obj(&self, output_path: &Path) {
        todo!()
    }

    pub fn codegen_module(
        context: &'ctx Context,
        root_module_node: tir::Module,
    ) -> Result<Codegen<'ctx>> {
        // Create llvm context and module
        let module = context.create_module(&root_module_node.name);

        // Create codegen
        let codegen = Codegen {
            context: &context,
            builder: context.create_builder(),
            name_ptr_map: Default::default(),
            module,
        };

        // Codegen a prototype for each function
        let mut fns = vec![];
        for function in root_module_node.functions {
            let func_val = codegen.codegen_function_proto(*function.decl.clone())?;
            fns.push((function, func_val));
        }

        // Codegen each function
        for (fn_node, fn_val) in fns {
            codegen.codegen_function(fn_val, fn_node)?;
        }

        // Validate module
        codegen
            .module
            .verify()
            .map_err(|llvm_str| CodegenError::ModuleVerificationError(llvm_str))?;

        Ok(codegen)
    }

    fn codegen_function_proto(&self, fn_decl_node: tir::FnDecl) -> Result<FunctionValue<'ctx>> {
        // Resolve parameter types
        let param_types = fn_decl_node
            .sig
            .params
            .into_iter()
            .map(|param| self.resolve_ty_node(param.ty).into())
            .collect::<Vec<_>>();

        // Create function type
        let fn_type = match fn_decl_node.sig.ty {
            ty::Ty::Never => self
                .context
                .void_type()
                .fn_type(param_types.as_slice(), false),
            ty => self
                .resolve_ty_node(ty)
                .fn_type(param_types.as_slice(), false),
        };

        // Add function to module
        let fn_name = fn_decl_node.name.to_string();
        let func = self.module.add_function(&fn_name, fn_type, None);

        Ok(func)
    }

    fn codegen_function(&self, func: FunctionValue<'ctx>, fn_node: tir::FnDef) -> Result<()> {
        // Create entry basic block
        let basic_block = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(basic_block);

        // Create allocas for the parameters
        {
            let mut name_ptr_map = self.name_ptr_map.try_write().unwrap();
            for (name_id, param) in fn_node.body.param_names.iter().zip(func.get_param_iter()) {
                let alloca = self.create_entry_alloca(param.get_type(), *name_id)?;
                self.builder.build_store(alloca, param);
                name_ptr_map.insert(*name_id, alloca);
            }
        }

        // Now we codegen each statement
        // if we hit control flow then we create new basic blocks
        for statement in fn_node.body.block.statements {
            self.codegen_statement(statement)?;
        }

        // Create return?
        if !fn_node.body.has_return {
            self.builder.build_return(None);
        }

        Ok(())
    }

    fn codegen_statement(&self, statement_node: tir::Statement) -> Result<()> {
        match statement_node.kind {
            tir::StatementKind::Expr(expr) => {
                self.codegen_expr(*expr)?;
            }
            tir::StatementKind::Return(expr) => match expr {
                None => {
                    self.builder.build_return(None);
                }
                Some(expr) => {
                    let expr = self.codegen_expr(*expr)?;
                    self.builder.build_return(Some(&expr));
                }
            },
        }

        Ok(())
    }

    fn create_entry_alloca(
        &self,
        ty: BasicTypeEnum<'ctx>,
        name_id: NameId,
    ) -> Result<PointerValue<'ctx>> {
        // Get entry block
        let current_block = self.builder.get_insert_block().unwrap();
        let func = current_block.get_parent().unwrap();
        let entry_block = func.get_first_basic_block().unwrap();

        // Create a temporary builder and point it at the entry block
        let temp_builder = self.context.create_builder();
        temp_builder.position_at_end(entry_block);

        // Create alloca
        let alloca =
            temp_builder.build_alloca(ty.as_basic_type_enum(), &format!("n{}", &name_id.index()));

        Ok(alloca)
    }

    fn codegen_expr(&self, expr_node: tir::Expr) -> Result<BasicValueEnum> {
        let value = match expr_node.kind {
            tir::ExprKind::Let(name_id, expr) => {
                // Create value to bind
                let value = self.codegen_expr(*expr.clone())?;

                // Create variable
                // TODO: sidenote, it would be nice if the name_id also contained the original
                // symbol id. that way we can print it to get like x0 etc
                let ty = self.resolve_ty_node(expr.ty);
                let alloca = self.create_entry_alloca(ty, name_id)?;

                // Store value in alloca
                self.builder.build_store(alloca, value);

                // Remember pointer for when its referred to
                self.name_ptr_map
                    .try_write()
                    .unwrap()
                    .insert(name_id, alloca);

                alloca.into()
            }

            tir::ExprKind::Literal(value) => match value {
                LiteralValue::Str(_) => todo!("Cant handle strings yet!"),
                LiteralValue::Float(value) => self.context.f64_type().const_float(value).into(),
                LiteralValue::Int(value) => match expr_node.ty {
                    Ty::Primitive(PrimitiveTy::SInt) => {
                        // Reinterpret as signed
                        let value = value as i64;
                        let value = u64::from_le_bytes(value.to_le_bytes());
                        self.context.i64_type().const_int(value, false)
                    }
                    Ty::Primitive(PrimitiveTy::UInt) => {
                        self.context.i64_type().const_int(value, false)
                    }
                    _ => unreachable!(),
                }
                .into(),
            },

            tir::ExprKind::Unary(op, expr) => {
                // Evaluate expression
                let expr = self.codegen_expr(*expr)?;

                // Codegen operation
                match op {
                    UnaryOpt::Negate => match expr_node.ty {
                        Ty::Primitive(PrimitiveTy::SInt | PrimitiveTy::UInt) => self
                            .builder
                            .build_int_neg(
                                expr.into_int_value(),
                                &format!("{}_neg", expr_node.id.index()),
                            )
                            .into(),

                        Ty::Primitive(PrimitiveTy::Float) => self
                            .builder
                            .build_float_neg(
                                expr.into_float_value(),
                                &format!("{}_neg", expr_node.id.index()),
                            )
                            .into(),

                        _ => todo!("Don't know how to negate that"),
                    },
                }
            }

            tir::ExprKind::Binary(op, (lhs, rhs)) => {
                // Codegen operands
                let lhs = self.codegen_expr(*lhs)?;
                let rhs = self.codegen_expr(*rhs)?;

                // Codegen operation
                let value = match op {
                    BinaryOpt::Add => match expr_node.ty {
                        // Adding ints and uints is the same instr
                        Ty::Primitive(PrimitiveTy::SInt | PrimitiveTy::UInt) => self
                            .builder
                            .build_int_add(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                &format!("{}_sum", expr_node.id.index()),
                            )
                            .into(),

                        Ty::Primitive(PrimitiveTy::Float) => self
                            .builder
                            .build_float_add(
                                lhs.into_float_value(),
                                rhs.into_float_value(),
                                &format!("{}_sum", expr_node.id.index()),
                            )
                            .into(),

                        _ => todo!("Dont know how to add that"),
                    },

                    BinaryOpt::Multiply => match expr_node.ty {
                        Ty::Primitive(PrimitiveTy::UInt | PrimitiveTy::SInt) => self
                            .builder
                            .build_int_mul(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                &format!("{}_mul", expr_node.id.index()),
                            )
                            .into(),

                        Ty::Primitive(PrimitiveTy::Float) => self
                            .builder
                            .build_float_mul(
                                lhs.into_float_value(),
                                rhs.into_float_value(),
                                &format!("{}_mul", expr_node.id.index()),
                            )
                            .into(),

                        _ => todo!("Dont know how to mul that"),
                    },

                    BinaryOpt::Subtract => match expr_node.ty {
                        Ty::Primitive(PrimitiveTy::UInt | PrimitiveTy::SInt) => self
                            .builder
                            .build_int_sub(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                &format!("{}_sub", expr_node.id.index()),
                            )
                            .into(),

                        Ty::Primitive(PrimitiveTy::Float) => self
                            .builder
                            .build_float_sub(
                                lhs.into_float_value(),
                                rhs.into_float_value(),
                                &format!("{}_sub", expr_node.id.index()),
                            )
                            .into(),

                        _ => todo!("Dont know how to sub that"),
                    },

                    BinaryOpt::Divide => match expr_node.ty {
                        Ty::Primitive(PrimitiveTy::UInt) => self
                            .builder
                            .build_int_unsigned_div(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                &format!("{}_udiv", expr_node.id.index()),
                            )
                            .into(),

                        Ty::Primitive(PrimitiveTy::SInt) => self
                            .builder
                            .build_int_signed_div(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                &format!("{}_sdiv", expr_node.id.index()),
                            )
                            .into(),

                        Ty::Primitive(PrimitiveTy::Float) => self
                            .builder
                            .build_float_div(
                                lhs.into_float_value(),
                                rhs.into_float_value(),
                                &format!("{}_fdiv", expr_node.id.index()),
                            )
                            .into(),

                        _ => todo!("Dont know how to div that"),
                    },

                    BinaryOpt::Exponentiate => todo!("dont know how to exponentiate"),
                };

                value
            }

            tir::ExprKind::FnCall(symbol, arguments) => {
                let func = self.module.get_function(&symbol.get().to_string()).unwrap();
                let args = arguments
                    .into_iter()
                    .map(|arg_expr| self.codegen_expr(arg_expr).map(|v| v.into()))
                    .collect::<Result<Vec<_>>>()?;
                let call = self.builder.build_call(
                    func,
                    args.as_slice(),
                    &format!("{}_call", expr_node.id.index()),
                );
                call.try_as_basic_value().unwrap_left()
            }

            tir::ExprKind::Name(name_id) => {
                // Get pointer to named value
                let pointer = self
                    .name_ptr_map
                    .try_read()
                    .unwrap()
                    .get(&name_id)
                    .map(|v| *v)
                    .ok_or(CodegenError::NoSuchNamedIdent(name_id.clone()))?;

                // Determine value type and then load it
                let ty = self.resolve_ty_node(expr_node.ty);
                let load =
                    self.builder
                        .build_load(ty, pointer, &format!("{}_load", expr_node.id.index()));

                // Return loaded valued
                load.into()
            }

            tir::ExprKind::Assign(name_id, expr) => {
                // Find the pointer for the name
                let ptr_value = *self.name_ptr_map.try_read().unwrap().get(&name_id).unwrap();

                // Codegen the expr
                let value = self.codegen_expr(*expr)?;

                // Store it
                self.builder.build_store(ptr_value, value);

                // The ident ref is the value of this expr
                ptr_value.into()
            }

            tir::ExprKind::Block(block) => {
                // Codegen each statement
                todo!()
            }
        };

        Ok(value)
    }

    fn resolve_ty_node(&self, ty_node: ty::Ty) -> BasicTypeEnum<'ctx> {
        match ty_node {
            ty::Ty::Primitive(ty::PrimitiveTy::Str) => todo!("Strings are scary"),
            ty::Ty::Primitive(ty::PrimitiveTy::SInt) => self.context.i64_type().into(),
            ty::Ty::Primitive(ty::PrimitiveTy::UInt) => self.context.i64_type().into(),
            ty::Ty::Primitive(ty::PrimitiveTy::Float) => self.context.f64_type().into(),
            ty::Ty::Primitive(ty::PrimitiveTy::Bool) => self.context.bool_type().into(),

            ty::Ty::Never => unreachable!("Cant create a basic variable for a never"),
            ty::Ty::Infer(_) => unreachable!("all type variables should be inferred at this point"),
        }
    }
}
