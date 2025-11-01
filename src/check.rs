use std::{cmp::max, ops::RangeInclusive, path::Path};

use crate::{ast::{ASTNode, ASTRef, Pattern, PatternRef, Type}, print_warnings::{Warning, WarningData, print_warning}, rustaml::RustamlContext, types::TypeInfos};

// need this for some checks that needs types (ex : match exhaustiveness)
// so there are here also some other checks that could be in parsing, but are here for more clean code

// for analyzing the ranges of match (make it smarter ?)
// TODO : use this function to check in the AST to have a warning for non exhaustive match
pub fn match_is_all_range(rustaml_context : &RustamlContext, matched_val_type : &Type, patterns : &[(PatternRef, ASTRef)]) -> bool {
    match matched_val_type {
        Type::Bool => {
            let has_true = patterns.iter().any(|e| matches!(e.0.get(&rustaml_context.pattern_pool), Pattern::Bool(true)));
            let has_false = patterns.iter().any(|e| matches!(e.0.get(&rustaml_context.pattern_pool), Pattern::Bool(false)));
            has_true && has_false
        },
        Type::Integer => {
            let mut ranges : Vec<RangeInclusive<i64>> = Vec::new();

            for (p, _) in patterns {
                match p.get(&rustaml_context.pattern_pool) {
                    Pattern::Integer(nb) => ranges.push(*nb..=*nb),
                    Pattern::Range(start, end, inclusivity) => {
                        //dbg!((start, end, inclusivity));
                        if *inclusivity {
                            ranges.push(*start..=*end);
                        } else {
                            let end_exclusive = max(end-1, *start);
                            ranges.push(*start..=end_exclusive);
                        }
                    },
                    _ => {}
                }
            }

            let mut merged_range = ranges.first().cloned();

            ranges.sort_by_key(|e| *e.start());

            for range in ranges.into_iter().skip(1){

                if let Some(merged_range) = &mut merged_range {
                    if range.start() < merged_range.start(){
                        *merged_range = *range.start()..=*merged_range.end();
                    }

                    if range.end() > merged_range.end(){
                        *merged_range = *merged_range.start()..=*range.end();
                    }
                } else {
                    merged_range = Some(range);
                }
            }

            merged_range == Some(i64::MIN..=i64::MAX)
        }
        _ => false, // TODO
    }
}

// TODO : if needs for more complex things with this function (which is used at multiple places), return the iter directly instead ?
pub fn match_fallback_match_nb(rustaml_context : &RustamlContext, patterns : &[(PatternRef, ASTRef)]) -> usize {
    patterns.iter().filter(|(p, _)| matches!(p.get(&rustaml_context.pattern_pool), Pattern::VarName(_) | Pattern::Underscore)).count()
}

fn is_exhaustive_match(rustaml_context : &RustamlContext, matched_val_type : &Type, patterns : &[(PatternRef, ASTRef)]) -> bool {
    match_is_all_range(rustaml_context, matched_val_type, patterns) || match_fallback_match_nb(rustaml_context, patterns) > 0
}

struct CheckContext<'a> {
    rustaml_context : &'a RustamlContext,
    typeinfos : &'a TypeInfos,
    filename : &'a Path,
    content : &'a String
}

fn check<'a>(check_context : &CheckContext<'a>, ast : ASTRef){
    match ast.get(&check_context.rustaml_context.ast_pool){
        ASTNode::TopLevel { nodes } => {
            for e in nodes {
                check(check_context, *e);
            }
        }
        ASTNode::FunctionDefinition { name, args, body, type_annotation } => {
            check(check_context, *body);
        }
        ASTNode::AnonFunc { args, body, type_annotation } => {
            check(check_context, *body);
        }
        ASTNode::VarDecl { name, val, body, var_type } => {
            check(check_context, *val);
            if let Some(b) = body {
                check(check_context, *b);
            }
        }
        ASTNode::IfExpr { cond_expr, then_body, else_body } => {
            check(check_context, *cond_expr);
            check(check_context, *then_body);
            check(check_context, *else_body);
        }
        ASTNode::MatchExpr { matched_expr, patterns } => {
            check(check_context, *matched_expr);
            for (_, e) in patterns {
                check(check_context, *e);
            }

            let matched_val_type = matched_expr.get_type(&check_context.rustaml_context.ast_pool);

            if !is_exhaustive_match(check_context.rustaml_context, matched_val_type, patterns){
                print_warning(Warning::new(WarningData::MatchNotExhaustive { matched_type: matched_val_type.clone() }, ast.get_range(&check_context.rustaml_context.ast_pool)), check_context.filename, check_context.content);
            }
        }
        ASTNode::Integer { nb } => {}, // TODO : check for number not in i64 (except abs(INT_MIN))
        ASTNode::Float { nb } => {}, // TODO : check for number not in f64
        ASTNode::List { list } => {
            for e in list {
                check(check_context, *e);
            }
        }
        ASTNode::BinaryOp { op, lhs, rhs } => {
            check(check_context, *lhs);
            check(check_context, *rhs);
        }
        ASTNode::UnaryOp { op, expr } => check(check_context, *expr),
        ASTNode::FunctionCall { callee, args } => {
            check(check_context, *callee);
            args.iter().for_each(|e| check(check_context, *e));
        }
        ASTNode::Cast { to_type, expr } => check(check_context, *expr),        
        
        ASTNode::Unit | ASTNode::Boolean { .. } | ASTNode::String { .. } | ASTNode::VarUse { .. } | ASTNode::ExternFunc { .. } => {},
    }
}

pub fn check_ast(rustaml_context : &RustamlContext, typeinfos : &TypeInfos, filename : &Path, content : &String, ast : ASTRef){
    let check_context = CheckContext {
        rustaml_context,
        typeinfos,
        filename,
        content,
    };
    check(&check_context, ast);
}