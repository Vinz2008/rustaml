use std::{cmp::max, ops::{Range, RangeInclusive}, path::Path};

use enum_tags::Tag;

use crate::{ast::{ASTNode, ASTRef, Pattern, PatternRef, Type}, print_warnings::{Warning, WarningData, print_warning}, rustaml::RustamlContext};

// need this for some checks that needs types (ex : match exhaustiveness)
// so there are here also some other checks that could be in parsing, but are here for more clean code

// TODO : add a warning if a Sum type variant has the same name as a variable / a var with the same name as a sum type variant
// TODO : add an error (here ? in the parsing ?) when there is two sum type variant with the same name

// for analyzing the ranges of match (make it smarter ?)
pub(crate) fn match_is_all_range(rustaml_context : &RustamlContext, matched_val_type : &Type, patterns : &[(PatternRef, ASTRef)]) -> bool {
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
        Type::SumType(sum_type) => {
            for v in &sum_type.variants {
                let has_found_variant = patterns.iter().any(|(p, _)| {
                    match p.get(&rustaml_context.pattern_pool) {
                        Pattern::SumTypeVariant(v_name) => {
                            if v_name.get_str(&rustaml_context.str_interner) == v.get_name(){
                                return true;
                            }
                        },
                        _ => {},
                    }
                    false
                });
                if !has_found_variant {
                    return false;
                }
            }
            true
        }
        Type::List(_) => {
            // TODO : make this smarter ? (for example if there is [], true :: l, and false :: l, it should be all cases)
            let mut has_empty_pattern= false;
            let mut has_more_than_one_pattern = false;
            for (p, _) in patterns {
                match p.get(&rustaml_context.pattern_pool) {
                    Pattern::List(l) => {
                        if l.is_empty(){
                            has_empty_pattern = true;
                        }
                    }
                    Pattern::ListDestructure(_e, l) => {
                        if matches!(l.get(&rustaml_context.pattern_pool), Pattern::Underscore | Pattern::VarName(_)) {
                            has_more_than_one_pattern = true;
                        }
                    }
                    _ => {}
                }
            }
            has_empty_pattern && has_more_than_one_pattern
        }
        _ => false, // TODO
    }
}

#[derive(Tag)]
pub(crate) enum CheckErrorData {
    IntegerOutOfRange {
        nb : i128,
    }
}


pub(crate) struct CheckError {
    pub(crate) err_data : CheckErrorData,
    pub(crate) range: Range<usize>,
}

impl CheckError {
    pub(crate) const INT_LITERAL_RANGE : Range<i128> = {
        let min_nb = i64::MIN as i128; // can technically never be negative because the - is an unary op that is applied after on the integer literal, but you can never be too careful
        let max = -(i64::MIN as i128);
        min_nb..max
    };

    pub(crate) fn new(err_data : CheckErrorData, range: Range<usize>) -> CheckError {
        CheckError { 
            err_data, 
            range 
        }
    }
}

// TODO : if needs for more complex things with this function (which is used at multiple places), return the iter directly instead ?
pub(crate) fn match_fallback_match_nb(rustaml_context : &RustamlContext, patterns : &[(PatternRef, ASTRef)]) -> usize {
    patterns.iter().filter(|(p, _)| matches!(p.get(&rustaml_context.pattern_pool), Pattern::VarName(_) | Pattern::Underscore)).count()
}

fn is_exhaustive_match(rustaml_context : &RustamlContext, matched_val_type : &Type, patterns : &[(PatternRef, ASTRef)]) -> bool {
    match_is_all_range(rustaml_context, matched_val_type, patterns) || match_fallback_match_nb(rustaml_context, patterns) > 0
}



struct CheckContext<'a> {
    rustaml_context : &'a RustamlContext,
    filename : &'a Path,
    content : &'a str
}

fn check<'a>(check_context : &CheckContext<'a>, ast : ASTRef) -> Result<(), CheckError> {
    match ast.get(&check_context.rustaml_context.ast_pool){
        ASTNode::TopLevel { nodes } => {
            for e in nodes {
                check(check_context, *e)?;
            }
        }
        ASTNode::FunctionDefinition { name: _, args: _, body, type_annotation: _ } => {
            check(check_context, *body)?;
        }
        ASTNode::AnonFunc { args: _, body, type_annotation: _ } => {
            check(check_context, *body)?;
        }
        ASTNode::VarDecl { name: _, val, body, var_type: _ } => {
            check(check_context, *val)?;
            if let Some(b) = body {
                check(check_context, *b)?;
            }
        }
        ASTNode::IfExpr { cond_expr, then_body, else_body } => {
            check(check_context, *cond_expr)?;
            check(check_context, *then_body)?;
            check(check_context, *else_body)?;
        }
        ASTNode::MatchExpr { matched_expr, patterns } => {
            check(check_context, *matched_expr)?;
            for (_, e) in patterns {
                check(check_context, *e)?;
            }

            let matched_val_type = matched_expr.get_type(&check_context.rustaml_context.ast_pool);

            if !is_exhaustive_match(check_context.rustaml_context, matched_val_type, patterns){
                print_warning(Warning::new(WarningData::MatchNotExhaustive { matched_type: matched_val_type.clone() }, ast.get_range(&check_context.rustaml_context.ast_pool)), check_context.filename, check_context.content);
            }
        }
        ASTNode::Integer { nb } => {
            if *nb > CheckError::INT_LITERAL_RANGE.end || *nb < CheckError::INT_LITERAL_RANGE.start {
                // TODO : do an error
                return Err(CheckError::new(CheckErrorData::IntegerOutOfRange { nb: *nb }, ast.get_range(&check_context.rustaml_context.ast_pool)));
            }
        },
        ASTNode::List { list } => {
            for e in list {
                check(check_context, *e)?;
            }
        }
        ASTNode::BinaryOp { op: _, lhs, rhs } => {
            check(check_context, *lhs)?;
            check(check_context, *rhs)?;
        }
        ASTNode::UnaryOp { op: _, expr } => check(check_context, *expr)?,
        ASTNode::FunctionCall { callee, args } => {
            check(check_context, *callee)?;
            for e in args {
                check(check_context, *e)?;
            }
        }
        ASTNode::Cast { to_type: _, expr } => check(check_context, *expr)?,        
        
        ASTNode::Unit | ASTNode::Float { .. } | ASTNode::Char { .. } | ASTNode::Boolean { .. } | ASTNode::String { .. } | ASTNode::Variant { .. } | ASTNode::VarUse { .. } | ASTNode::ExternFunc { .. } | ASTNode::TypeAlias { .. } => {},
    }
    Ok(())
}

pub(crate) fn check_ast(rustaml_context : &RustamlContext, filename : &Path, content : &str, ast : ASTRef) -> Result<(), CheckError> {
    let check_context = CheckContext {
        rustaml_context,
        filename,
        content,
    };
    return check(&check_context, ast);
}