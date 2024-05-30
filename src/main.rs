use c_emitter::Emitter;
use colored::Colorize;
use lexer::{lex, LexError, Span};
use parser::{ParseError, Parser};
use std::{
    env,
    fs::{self, File},
    path::{Path, PathBuf},
    process::{exit, Command},
    str::FromStr,
};
use type_checker::{TypeChecker, TypeError};

pub mod c_emitter;
pub mod lexer;
pub mod parser;
pub mod type_checker;

//MAIN

struct ErrorInfo {
    line_number: usize,
    error_line_offset: usize,
    line: String,
}

fn get_error_info(source: &str, span: &Span) -> ErrorInfo {
    let mut line_offset = 0;
    let mut line_number = 1;
    let lines = source.lines();

    for line in lines {
        if line.is_empty() {
            line_number += 1;
            line_offset += 2; //TODO: +2 for windows /r/n
            continue;
        }
        if line_offset + line.len() > span.offset {
            return ErrorInfo {
                line_number,
                error_line_offset: span.offset - line_offset,
                line: line.to_string(),
            };
        }

        line_offset += line.len() + 2; //TODO: +2 for windows /r/n
        line_number += 1;
    }

    todo!()
}

fn display_lex_error(lex_error: LexError, source: &str) {
    let error_info = get_error_info(source, &lex_error.get_span());

    eprintln!("{}: {}:", "ERROR".red(), lex_error);
    println!("{} |", " ".repeat(error_info.line_number.to_string().len()));
    println!("{} |{}", error_info.line_number, error_info.line);
    println!(
        "{} |{}{}",
        " ".repeat(error_info.line_number.to_string().len()),
        " ".repeat(error_info.error_line_offset),
        "^".repeat(lex_error.get_span().length).red()
    );
}

fn display_parse_error(parse_error: ParseError, source: &str) {
    let error_info = get_error_info(source, &parse_error.get_span());

    eprintln!("{}: {}:", "ERROR".red(), parse_error);
    println!("{} |", " ".repeat(error_info.line_number.to_string().len()));
    println!("{} |{}", error_info.line_number, error_info.line);
    println!(
        "{} |{}{}",
        " ".repeat(error_info.line_number.to_string().len()),
        " ".repeat(error_info.error_line_offset),
        "^".repeat(parse_error.get_span().length).red()
    );
}

fn display_type_error(type_error: TypeError, source: &str) {
    let error_info = get_error_info(source, &type_error.span);

    eprintln!("{}: {}:", "ERROR".red(), type_error.kind);
    println!("{} |", " ".repeat(error_info.line_number.to_string().len()));
    println!("{} |{}", error_info.line_number, error_info.line);
    println!(
        "{} |{}{}",
        " ".repeat(error_info.line_number.to_string().len()),
        " ".repeat(error_info.error_line_offset),
        "^".repeat(type_error.span.length).red()
    );
}

fn main() {
    let args: Vec<String> = env::args().collect();

    // Ensure a file path argument is provided
    if args.len() < 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        exit(1);
    }

    let path = &args[1];

    let mut pathbuf = PathBuf::from_str(path.as_str()).unwrap();
    pathbuf.pop();

    let source = fs::read_to_string(path).unwrap_or_else(|err| {
        eprintln!("Error reading file {}: {}", path, err);
        exit(1);
    });

    let tokens = match lex(&source) {
        Ok(tokens) => tokens,
        Err(e) => {
            display_lex_error(e, &source);
            exit(0);
        }
    };

    let mut parser = Parser::new(tokens);

    let statements = match parser.parse() {
        Ok(statements) => statements,
        Err(e) => {
            display_parse_error(e, &source);
            exit(0);
        }
    };

    let directory_path = pathbuf.as_os_str().to_str().expect("todo").to_owned();
    let mut type_checker = TypeChecker::new(directory_path);

    let module = match type_checker.type_check(statements) {
        Ok(module) => module,
        Err(e) => {
            display_type_error(e, &source);
            exit(0);
        }
    };

    let base_name = Path::new(path)
        .file_stem()
        .expect("Couldn't get file stem")
        .to_string_lossy();
    let c_file_name = format!("{}.c", base_name);
    let exe_file_name = format!("{}", base_name);

    let mut emitter = Emitter::new(File::create(&c_file_name).expect("Couldn't open C file"));

    emitter.emit(module).expect("couldn't emit");

    // Compile the C code
    Command::new("gcc")
        .arg("-o")
        .arg(&exe_file_name)
        .arg(&c_file_name)
        .output()
        .expect("Couldn't make executable");

    // Remove the intermediate C file
    //fs::remove_file(&c_file_name).expect("Couldn't remove intermediate C file");
}
