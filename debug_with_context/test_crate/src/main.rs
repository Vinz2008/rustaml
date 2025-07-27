#![feature(debug_closure_helpers)]

use debug_with_context::{DebugWithContext, DebugWrapContext};
use std::fmt::Debug;


struct Context;

/*#[derive(DebugWithContext)]
#[debug_context(Context)]
struct Test {
    a : i32,
    b : u64,
    c : usize,
    d : String,
}*/

/*#[derive(DebugWithContext)]
#[debug_context(Context)]
struct TestGenerics<T, A> {
    a : T,
    b : A,
}*/

#[derive(DebugWithContext)]
#[debug_context(Context)]
enum TestE {
    VariantA { a: i32 },
    VariantB { b: i64 },
}

impl Debug for TestE {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VariantA { a } => f.debug_struct("VariantA").field("a", a).finish(),
            Self::VariantB { b } => f.debug_struct("VariantB").field("b", b).finish(),
        }
    }
}

fn main(){
    let context = Context;
    /*let test = Test {
        a: 3,
        b : 6,
        c : 7,
        d: "oo".to_string(),
    };*/
    let test = TestE::VariantA { a: 2 };
    println!("{:?}", DebugWrapContext::new(&test, &context));
}