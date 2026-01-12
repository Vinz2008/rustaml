// TODO : add dot format printing for the AST tree

use std::{fs::File, io::{self, Write}};

use petgraph::{Graph, dot::Dot, graph::NodeIndex};

use crate::{ast::{ASTNode, ASTRef}, rustaml::RustamlContext};

pub(crate) fn _generate_ast_dot(graph : &mut Graph<String, String>, rustaml_context : &RustamlContext, ast : ASTRef) -> NodeIndex {
    match ast.get(&rustaml_context.ast_pool){
        
        ASTNode::FunctionDefinition { name, args, body, type_annotation } => {
            let func_def = graph.add_node("func_def".to_string());
            let func_name = graph.add_node(name.get_str(&rustaml_context.str_interner).to_string());
            graph.add_edge(func_def, func_name, "func_name".to_string());
            let body_node = _generate_ast_dot(graph, rustaml_context, *body);
            graph.add_edge(func_def, body_node, "body".to_string());
            func_def
        }
        ASTNode::AnonFunc { args, body, type_annotation } => {
            let anon_node = graph.add_node("anon_func".to_string());
            let body_node = _generate_ast_dot(graph, rustaml_context, *body);
            graph.add_edge(anon_node, body_node, "body".to_string());
            anon_node
        }
        ASTNode::ExternFunc { name, type_annotation, lang, so_str } => {
            let extern_func = graph.add_node("extern_func".to_string());
            let func_name = graph.add_node(name.get_str(&rustaml_context.str_interner).to_string());
            graph.add_edge(extern_func, func_name, "name".to_string());
            extern_func
        }
        ASTNode::TypeAlias { name, type_alias } => {
            let type_alias = graph.add_node("type_alias".to_string());
            let type_name = graph.add_node(name.get_str(&rustaml_context.str_interner).to_string());
            graph.add_edge(type_alias, type_name, "name".to_string());
            // TODO : add the type that is aliased
            type_alias
        }
        ASTNode::FunctionCall { callee, args } => {
            let func_call = graph.add_node("call".to_string());
            let callee_node = _generate_ast_dot(graph, rustaml_context, *callee);
            graph.add_edge(func_call, callee_node, "callee".to_string());
            for (idx, a) in args.iter().enumerate() {
                let arg_node = _generate_ast_dot(graph, rustaml_context, *a);
                graph.add_edge(func_call, arg_node, format!("arg{}", idx));
            }
            func_call
        }
        ASTNode::VarDecl { name, val, body, var_type } => {
            let let_node = graph.add_node("let".to_string());
            let var_name = graph.add_node(name.get_str(&rustaml_context.str_interner).to_string());
            graph.add_edge(let_node, var_name, "var_name".to_string());
            let val_node = _generate_ast_dot(graph, rustaml_context, *val);
            graph.add_edge(let_node, val_node, "val".to_string());
            if let Some(b) = body.as_ref() {
                let body_node = _generate_ast_dot(graph, rustaml_context, *b);
                graph.add_edge(let_node, body_node, "body".to_string());
            }
            
            let_node
        }
        ASTNode::IfExpr { cond_expr, then_body, else_body } => {
            let if_node = graph.add_node("if".to_string());
            let cond_node = _generate_ast_dot(graph, rustaml_context, *cond_expr);
            graph.add_edge(if_node, cond_node, "cond".to_string());
            let then_node = _generate_ast_dot(graph, rustaml_context, *then_body);
            graph.add_edge(if_node, then_node, "then".to_string());
            let else_node = _generate_ast_dot(graph, rustaml_context, *else_body);
            graph.add_edge(if_node, else_node, "else".to_string());
            if_node
        }
        ASTNode::MatchExpr { matched_expr, patterns } => {
            let match_node = graph.add_node("match".to_string());
            let matched_expr = _generate_ast_dot(graph, rustaml_context, *matched_expr);
            graph.add_edge(match_node, matched_expr, "matched_expr".to_string());
            for (idx, (p, a)) in patterns.iter().enumerate() {
                // TODO better formatting for patterns
                let pat_node = graph.add_node(format!("pat{}", idx));
                graph.add_edge(match_node, pat_node, "pattern".to_string());
                let expr_node = _generate_ast_dot(graph, rustaml_context, *a);
                graph.add_edge(pat_node, expr_node, "expr".to_string());
            }
            match_node
        }
        ASTNode::Cast { to_type, expr } => {
            let cast_node = graph.add_node("cast".to_string());
            let to_type_node = graph.add_node(format!("{:?}", to_type));
            graph.add_edge(cast_node, to_type_node, "to_type".to_string());
            let expr_node = _generate_ast_dot(graph, rustaml_context, *expr);
            graph.add_edge(cast_node, expr_node, "expr".to_string());
            cast_node
        }
        ASTNode::BinaryOp { op, lhs, rhs } => {
            let binop_node = graph.add_node(format!("{:?}", op));
            let lhs_node = _generate_ast_dot(graph, rustaml_context, *lhs);
            let rhs_node = _generate_ast_dot(graph, rustaml_context, *rhs);
            graph.add_edge(binop_node, lhs_node, "lhs".to_string());
            graph.add_edge(binop_node, rhs_node, "rhs".to_string());
            binop_node
        }
        ASTNode::UnaryOp { op, expr } => {
            let un_op = graph.add_node(format!("{:?}", op));
            let expr_node = _generate_ast_dot(graph, rustaml_context, *expr);
            graph.add_edge(un_op, expr_node, "expr".to_string());
            un_op
        }
        ASTNode::VarUse { name } => {
            graph.add_node(name.get_str(&rustaml_context.str_interner).to_string())
        }
        ASTNode::List { list } => {
            let list_node = graph.add_node("list".to_string());
            for (idx, l) in list.iter().enumerate() {
                let list_element = _generate_ast_dot(graph, rustaml_context, *l);
                graph.add_edge(list_node, list_element, format!("e{}", idx));
            }
            list_node
        }
        ASTNode::Integer { nb } => {
            graph.add_node(format!("{}", nb))
        },
        ASTNode::Float { nb } => {
            graph.add_node(format!("{}", nb))
        }
        ASTNode::String { str } => {
            graph.add_node(str.get_str(&rustaml_context.str_interner).to_string())
        }
        ASTNode::Boolean { b } => {
            graph.add_node(format!("{:?}", b))
        }
        ASTNode::Unit => {
            graph.add_node("()".to_string())
        }
        ASTNode::Variant { name, arg } => {
            graph.add_node(name.get_str(&rustaml_context.str_interner).to_string())
        }
        ASTNode::Char { c } => {
            graph.add_node(format!("\'{}\'", c))
        }
        ASTNode::TopLevel { nodes } => {
            let toplevel_node = graph.add_node("toplevel".to_string());
            for n in nodes {
                let node = _generate_ast_dot(graph, rustaml_context, *n);
                graph.add_edge(toplevel_node, node, "".to_string());
            }
            toplevel_node
        }
        //a => panic!("not implemented for {:?}", DebugWrapContext::new(a, rustaml_context)),
    }
}

pub(crate) fn generate_ast_dot(rustaml_context : &RustamlContext, ast : ASTRef) -> io::Result<()>{
    let mut graph = Graph::<_, String>::new();
    _generate_ast_dot(&mut graph, rustaml_context, ast);
    let dot = Dot::with_config(&graph, &[]);
    let mut dot_file = File::create("ast.dot")?;
    writeln!(&mut dot_file, "{:?}", dot)?;

    Ok(())
} 