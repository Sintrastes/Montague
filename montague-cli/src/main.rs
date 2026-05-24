//! `mont` — the Montague command-line interface.
//!
//! Subcommands:
//!   mont parse <file>           Parse a .mont file, print the AST
//!   mont lower <file> <backend> Parse, resolve, and lower the lexicon
//!   mont tree <file> <sentence> Parse a sentence using a .mont lexicon, print parse tree
//!   mont help                   Print this message

use std::env;
use std::fs;
use std::io::{self, BufRead, Write};
use std::process;

use montague::core::{
    self,
    reduction::{ReductionCtx, ReductionEngine},
    Semantics,
};
use montague::mont::{parser, resolver};
use montague::pretty::{display_lambek_as_sexp, display_term_as_sexp, tree};
use montague::prolog::lower_term_to_prolog;
use scryer_prolog::MachineBuilder;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        print_usage();
        process::exit(1);
    }

    match args[1].as_str() {
        "parse" => cmd_parse(&args),
        "ask" => cmd_ask(&args),
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
           montague ask   <file>             Interactive Q&A: assertions and queries\n\
           montague parse <file>             Parse a .mont file, print the AST\n\
           montague lower <file> <backend>   Parse, resolve, and lower the lexicon\n\
           montague tree  <file> <sentence>  Parse a sentence, print the parse tree\n\
           montague help                     Print this message"
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
    let file = require_arg(args, 2, "montague lower <file> <backend> [sentence]");
    let backend_name = require_arg(args, 3, "montague lower <file> <backend> [sentence]");
    let sentence: Option<String> = args.get(4..).map(|w| w.join(" ")).filter(|s| !s.is_empty());

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

    // If a sentence was provided, lower the parsed sentence instead of the lexicon.
    if let Some(ref sent) = sentence {
        let sem: Semantics<String, String, core::AnnotatedTerm<String, String>> =
            resolver::build_semantics(&lex);
        let engine = ReductionEngine::standard();
        let ctx = ReductionCtx::new(&lex.lattice);
        let parses = core::get_all_parses(&engine, &ctx, &sem, sent);

        if parses.is_empty() {
            eprintln!("No parse found for: {sent:?}");
            process::exit(1);
        }

        for (i, at) in parses.iter().enumerate() {
            if i > 0 {
                println!();
            }
            match backend_name {
                "prolog" => {
                    if let Some(clause) = lower_term_to_prolog(&at.term) {
                        println!("{clause}");
                    }
                }
                "sexp" => {
                    println!("{}", display_term_as_sexp(&at.term));
                }
                _ => {
                    eprintln!("montague: unknown backend `{backend_name}`");
                    process::exit(1);
                }
            }
        }
        return;
    }

    // Lexicon mode (no sentence).
    match backend_name {
        "sexp" => lower_lexicon_sexp(&lex),
        "prolog" => lower_lexicon_prolog(&lex),
        _ => {
            eprintln!("montague: unknown backend `{backend_name}`. Use `prolog` or `sexp`.");
            process::exit(1);
        }
    }
}

/// Lower a lexicon to S-expression format.
fn lower_lexicon_sexp(lex: &resolver::ResolvedLexicon) {
    for a in &lex.atoms {
        let ty_sexp = display_lambek_as_sexp(&a.type_expr, None);
        if let Some(ref doc) = a.doc {
            println!(";; | {doc}");
        }
        println!("(: {} {ty_sexp})", a.entity);
    }
    if !lex.productions.is_empty() {
        println!();
        for p in &lex.productions {
            if p.words.len() == 1 {
                println!("(--> \"{}\" {})", p.words[0], p.entity);
            } else {
                let words: Vec<String> = p.words.iter().map(|w| format!("\"{w}\"")).collect();
                println!("(--> ({}) {})", words.join(" "), p.entity);
            }
        }
    }
}

/// Lower a lexicon to Prolog facts (typeOf/2 only for Basic-typed atoms).
fn lower_lexicon_prolog(lex: &resolver::ResolvedLexicon) {
    for a in &lex.atoms {
        if let montague::core::LambekType::Basic(ref t) = a.type_expr {
            // t is a String — use it directly as the type name.
            let type_name = t.to_lowercase();
            println!("typeOf({}, {}).", a.entity, type_name);
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

fn cmd_ask(args: &[String]) {
    let file = require_arg(args, 2, "montague ask <file>");

    // 1. Load lexicon
    let src = read_file(file);
    let (ast, parse_errs) = parser::parse(&src);
    if !parse_errs.is_empty() {
        for e in &parse_errs {
            eprintln!("parse error: {e}");
        }
        process::exit(1);
    }
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
    let sem: Semantics<String, String, core::AnnotatedTerm<String, String>> =
        resolver::build_semantics(&lex);
    let engine = ReductionEngine::standard();
    let ctx = ReductionCtx::new(&lex.lattice);

    // 2. Initialize Scryer Prolog
    let mut machine = MachineBuilder::default().build();
    let mut clause_count = 0u32;

    eprintln!("Montague Q&A — type assertions or questions. :q to quit.\n");

    // 3. REPL
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    loop {
        print!("montague> ");
        stdout.flush().unwrap();
        let mut input = String::new();
        if stdin.lock().read_line(&mut input).is_err() {
            break;
        }
        let input = input.trim().to_string();
        if input.is_empty() {
            continue;
        }
        if input == ":q" || input == ":quit" {
            eprintln!("bye.");
            break;
        }

        // Detect question: trailing ? or leading wh-word
        let is_question = input.ends_with('?')
            || input.starts_with("who ")
            || input.starts_with("what ")
            || input.starts_with("is ")
            || input.starts_with("does ");
        let clean_input = input.trim_end_matches('?').trim().to_string();

        let parses = core::get_all_parses(&engine, &ctx, &sem, &clean_input);
        if parses.is_empty() {
            eprintln!("  No parse found.");
            continue;
        }

        // Take the first parse
        let at = &parses[0];

        if is_question {
            // Question: emit as Prolog query and run.
            let query = lower_term_to_prolog(&at.term)
                .map(|s| s.trim_end_matches('.').to_string())
                .unwrap_or_else(|| format!("{:?}", at.term));
            eprintln!("  query: {query}?");

            let query_str = format!("{query}.");
            let answers: Result<Vec<scryer_prolog::LeafAnswer>, _> =
                machine.run_query(&query_str).collect();
            match answers {
                Ok(results) => {
                    if results.is_empty() {
                        eprintln!("  no.");
                    } else if results == vec![scryer_prolog::LeafAnswer::True] {
                        eprintln!("  yes.");
                    } else {
                        for a in &results {
                            eprintln!("  {a:?}");
                        }
                    }
                }
                Err(e) => {
                    eprintln!("  query error: {e:?}");
                }
            }
        } else {
            // Assertion: lower to Prolog and load.
            if let Some(clause) = lower_term_to_prolog(&at.term) {
                let module = format!("clause_{clause_count}");
                machine.load_module_string(&module, &clause);
                clause_count += 1;
                eprintln!("  asserted: {clause}");
            } else {
                eprintln!("  (could not lower to Prolog)");
            }
        }

        if parses.len() > 1 {
            eprintln!("  ({} parses total)", parses.len());
        }
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
