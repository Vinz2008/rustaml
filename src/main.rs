use std::{fs, path::PathBuf};

use clap::{Parser, Subcommand};

mod ast;
mod intepreter;
mod lexer;

#[derive(Subcommand, Debug)]
enum Commands {
    /// intepret file
    Interpret {
        #[arg(value_name = "FILE")]
        filename: PathBuf,
    },
    /// compile file
    Compile {
        #[arg(value_name = "FILE")]
        filename: PathBuf,
    },
}

#[derive(Parser, Default, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Option<Commands>,
}

// TODO : use https://crates.io/crates/ariadne for printing errors

fn main() {
    let args = Args::parse();

    match args.command.expect("No subcommand specified!") {
        Commands::Interpret { filename } => {
            let content = fs::read_to_string(&filename).unwrap_or_else(|err| {
                panic!("Error when opening {} : {}", filename.display(), err)
            });
            let tokens = lexer::lex(content.chars().collect());
            let ast = ast::parse(tokens);
            intepreter::interpret(ast)
        }
        Commands::Compile { filename } => {
            let content = fs::read_to_string(&filename).unwrap_or_else(|err| {
                panic!("Error when opening {} : {}", filename.display(), err)
            });
            let tokens = lexer::lex(content.chars().collect());
            let ast = ast::parse(tokens);
            todo!()
        }
    }
}
