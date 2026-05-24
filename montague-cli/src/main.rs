//! `mont` — the Montague command-line interface.
//!
//! Subcommands:
//!   mont parse <file>           Parse a .mont file, print the AST
//!   mont lower <file> <backend> Parse, resolve, and lower the lexicon
//!   mont tree <file> <sentence> Parse a sentence using a .mont lexicon, print parse tree
//!   mont help                   Print this message

use std::env;
use std::fs;
use std::process;

use montague::core::{
    self,
    reduction::{ReductionCtx, ReductionEngine},
    Semantics,
};
use montague::mont::{parser, resolver};
use montague::pretty::{display_lambek_as_sexp, tree};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        print_usage();
        process::exit(1);
    }

    match args[1].as_str() {
        "parse" => cmd_parse(&args),
        "lower" => cmd_lower(&args),
        "tree" => cmd_tree(&args),
        "help" | "--help" | "-h" => print_usage(),
        _ => {
            eprintln!("mont: unknown command `{}`", args[1]);
            print_usage();
            process::exit(1);
        }
    }
}

fn print_usage() {
    eprintln!(
        "mont — Montague DSL tool\n\
         \n\
         USAGE:\n\
           mont parse <file>             Parse a .mont file, print the AST\n\
           mont lower <file> <backend>   Parse, resolve, and lower: `prolog` or `sexp`\n\
           mont tree <file> <sentence>   Parse a sentence, print the parse tree\n\
           mont help                     Print this message"
    );
}

fn cmd_parse(args: &[String]) {
    let file = require_arg(args, 2, "mont parse <file>");
    let src = read_file(file);
    let (ast, errs) = parser::parse(&src);

    for e in &errs {
        eprintln!("parse error: {e}");
    }
    if errs.is_empty() {
        println!("{ast}");
    } else {
        process::exit(1);
    }
}

fn cmd_lower(args: &[String]) {
    let file = require_arg(args, 2, "mont lower <file> <backend>");
    let backend_name = require_arg(args, 3, "mont lower <file> <backend>");

    let src = read_file(file);
    let (ast, parse_errs) = parser::parse(&src);
    if !parse_errs.is_empty() {
        eprintln!("Parse errors:");
        for e in &parse_errs {
            eprintln!("  {e}");
        }
        process::exit(1);
    }

    let reg = core::registry::Registry::empty();
    let lex = match resolver::resolve(&ast, &reg) {
        Ok(lex) => lex,
        Err(errs) => {
            eprintln!("Resolution errors:");
            for e in &errs {
                eprintln!("  {e}");
            }
            process::exit(1);
        }
    };

    match backend_name {
        "sexp" => {
            for a in &lex.atoms {
                let ty_sexp = display_lambek_as_sexp(&a.type_expr, None);
                if let Some(ref doc) = a.doc {
                    println!(";; | {doc}");
                }
                println!("({} : {ty_sexp})", a.entity);
            }
            if !lex.productions.is_empty() {
                println!();
                for p in &lex.productions {
                    println!("(--> (\"{}\") {})", p.words.join("\" \""), p.entity);
                }
            }
        }
        "prolog" => {
            for a in &lex.atoms {
                println!("% atom {} : {:?}", a.entity, a.type_expr);
            }
            for p in &lex.productions {
                println!("% production {} --> {}", p.words.join(", "), p.entity);
            }
        }
        _ => {
            eprintln!("mont: unknown backend `{backend_name}`. Use `prolog` or `sexp`.");
            process::exit(1);
        }
    }
}

fn cmd_tree(args: &[String]) {
    let file = require_arg(args, 2, "mont tree <file> <sentence>");
    let sentence = args.get(3..).map(|w| w.join(" ")).unwrap_or_default();
    if sentence.is_empty() {
        eprintln!("mont: missing sentence. Usage: mont tree <file> <sentence>");
        process::exit(1);
    }

    // 1. Parse lexicon
    let src = read_file(file);
    let (ast, parse_errs) = parser::parse(&src);
    if !parse_errs.is_empty() {
        for e in &parse_errs {
            eprintln!("parse error: {e}");
        }
        process::exit(1);
    }

    // 2. Resolve
    let reg = core::registry::Registry::empty();
    let lex = match resolver::resolve(&ast, &reg) {
        Ok(lex) => lex,
        Err(errs) => {
            for e in &errs {
                eprintln!("resolve error: {e}");
            }
            process::exit(1);
        }
    };

    // 3. Build semantics and engine
    let sem: Semantics<String, String, core::AnnotatedTerm<String, String>> =
        resolver::build_semantics(&lex);
    let engine = ReductionEngine::standard();
    let ctx = ReductionCtx::new(&lex.lattice);

    // 4. Parse the sentence
    let parses = core::get_all_parses(&engine, &ctx, &sem, &sentence);

    if parses.is_empty() {
        eprintln!("No parse found for: {sentence:?}");
        process::exit(1);
    }

    // 5. Build type lookup from the lexicon
    let atom_types: std::collections::HashMap<String, montague::core::LambekType<String>> = lex
        .atoms
        .iter()
        .map(|a| (a.entity.clone(), a.type_expr.clone()))
        .collect();

    // 6. Show all parses as trees
    for (i, at) in parses.iter().enumerate() {
        if i > 0 {
            println!();
        }
        if parses.len() > 1 {
            println!("=== Parse {} ===", i + 1);
        }
        let typed_tree = tree::build_typed_tree(&at.term, &at.ty, &|name: &str| {
            atom_types.get(name).cloned()
        });
        println!("{}", tree::render_typed_tree(&typed_tree));
    }
}

fn require_arg<'a>(args: &'a [String], idx: usize, usage: &str) -> &'a str {
    if args.len() <= idx {
        eprintln!("mont: missing argument. Usage: {usage}");
        process::exit(1);
    }
    &args[idx]
}

fn read_file(path: &str) -> String {
    fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("mont: cannot read `{path}`: {e}");
        process::exit(1);
    })
}
