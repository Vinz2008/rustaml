use std::{fs::File, io::Write, path::Path};
use debug_with_context::DebugWrapContext;

use crate::{ast::{ASTRef, Type}, rustaml::RustamlContext, string_intern::StringRef};

struct TypeFound {
    node : ASTRef,
    type_found : Type,
    var_name : StringRef,
}

impl TypeFound {
    fn new(node : ASTRef, type_found : Type, var_name : StringRef) -> TypeFound {
        TypeFound { node, type_found, var_name }
    }
}

struct DumpInferInner {
    vars_inferred : Vec<(StringRef, Type)>,
    types_found : Vec<TypeFound>, 
}

pub struct DumpInfer { 
    inner : Option<DumpInferInner>, // TODO : add context (like the ast ref where the infer was done ?)
}


impl DumpInferInner {
    fn new() -> DumpInferInner {
        DumpInferInner {
            vars_inferred: Vec::new(),
            types_found:  Vec::new(),
        }
    }
}

impl DumpInfer {

    pub fn new(dump_inference : bool) -> DumpInfer {
        let inner = if dump_inference {
            Some(DumpInferInner::new())
        } else {
            None
        };
        DumpInfer { inner }
    }

    pub fn add_var_inferred(&mut self, var_name : StringRef, type_inferred : Type){
        if let Some(i) = &mut self.inner {
            i.vars_inferred.push((var_name, type_inferred));
        }
    }

    pub fn add_type_found(&mut self, var_name : StringRef, ast_node : ASTRef, t : &Option<Type>) {
        let t = match t {
            Some(t) => t,
            None => return,
        };
        
        if let Some(i) = &mut self.inner {
            i.types_found.push(TypeFound::new(ast_node, t.clone(), var_name))
        }
    }

    pub fn dump(&self, path : &Path, rustaml_context : &RustamlContext) -> std::io::Result<()>{
        if let Some(inner) = &self.inner {
            let mut file = File::create(path)?;

            for TypeFound { node: body, type_found, var_name } in &inner.types_found {
                writeln!(&mut file, "found type in {:?} for {} : {:?}", DebugWrapContext::new(body, rustaml_context), var_name.get_str(&rustaml_context.str_interner), type_found)?;
            }

            writeln!(&mut file)?;
            for (v_name, v_type) in &inner.vars_inferred {
                writeln!(&mut file, "{} : {:?}", v_name.get_str(&rustaml_context.str_interner), v_type)?;
            }
        }
    
        Ok(())
    }   
}