//! `montague` — the Montague command-line interface.
//!
//! Subcommands:
//!   montague ask   <file>             Interactive Q&A: assertions and queries
//!   montague init  <description>      Bootstrap a .mont lexicon from a description
//!   montague parse <file>             Parse a .mont file, print the AST
//!   montague lower <file> <backend>   Parse, resolve, and lower the lexicon
//!   montague tree  <file> <sentence>  Parse a sentence, print the parse tree
//!   montague help                     Print this message

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

#[cfg(feature = "llm")]
use montague_llm::{self as llm_mod, BackendPreference, Llm};

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
        "init" => cmd_init(&args),
        "help" | "--help" | "-h" => print_usage(),
        _ => {
            eprintln!("montague: unknown command `{}`", args[1]);
            print_usage();
            process::exit(1);
        }
    }
}

fn print_usage() {
    eprintln!(
        "montague — Montague DSL tool\n\
         \n\
         USAGE:\n\
           montague ask   [--llm] [--backend <name>] [--debug] [--auto-accept] [--transparent] <file>\n\
           montague init  [--llm] [--backend <name>] [--output <file>] <description>\n\
           montague parse <file>\n\
           montague lower <file> <backend> [sentence]\n\
           montague tree  <file> <sentence>\n\
           montague help\n\
         \n\
         FLAGS (for ask/init):\n\
           --llm             Enable LLM features\n\
           --backend <name>  LLM backend: anthropic, mistralrs, or ollama\n\
           --debug           Show raw LLM prompts and responses\n\
           --auto-accept     Auto-accept LLM lexicon suggestions\n\
           --transparent     LLM-as-frontend chat mode (ask only)\n\
           --output <file>   Output file for bootstrap (init only)"
    );
}

// ---------------------------------------------------------------------------
// cmd_parse
// ---------------------------------------------------------------------------

fn cmd_parse(args: &[String]) {
    let file = require_arg(args, 2, "montague parse <file>");
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

// ---------------------------------------------------------------------------
// cmd_lower
// ---------------------------------------------------------------------------

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
        let parses = core::get_all_parses_chart(&engine, &ctx, &sem, sent);

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
                    if let Some(clauses) = lower_term_to_prolog(&at.term) {
                        for clause in &clauses {
                            println!("{clause}");
                        }
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

fn lower_lexicon_prolog(lex: &resolver::ResolvedLexicon) {
    for a in &lex.atoms {
        if let montague::core::LambekType::Basic(ref t) = a.type_expr {
            let type_name = t.to_lowercase();
            println!("typeOf({}, {}).", a.entity, type_name);
        }
    }
}

// ---------------------------------------------------------------------------
// cmd_tree
// ---------------------------------------------------------------------------

fn cmd_tree(args: &[String]) {
    let file = require_arg(args, 2, "montague tree <file> <sentence>");
    let sentence = args.get(3..).map(|w| w.join(" ")).unwrap_or_default();
    if sentence.is_empty() {
        eprintln!("montague: missing sentence. Usage: montague tree <file> <sentence>");
        process::exit(1);
    }

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

    let parses = core::get_all_parses_chart(&engine, &ctx, &sem, &sentence);

    if parses.is_empty() {
        eprintln!("No parse found for: {sentence:?}");
        process::exit(1);
    }

    let atom_types: std::collections::HashMap<String, montague::core::LambekType<String>> = lex
        .atoms
        .iter()
        .map(|a| (a.entity.clone(), a.type_expr.clone()))
        .collect();

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

// ---------------------------------------------------------------------------
// cmd_ask — Interactive Q&A with optional LLM augmentation
// ---------------------------------------------------------------------------

struct AskFlags {
    file: String,
    llm_enabled: bool,
    backend: Option<String>,
    debug: bool,
    auto_accept: bool,
    transparent: bool,
    compose: bool,
}

fn parse_ask_flags(args: &[String]) -> AskFlags {
    let mut flags = AskFlags {
        file: String::new(),
        llm_enabled: false,
        backend: None,
        debug: false,
        auto_accept: false,
        transparent: false,
        compose: false,
    };

    // Scan all args: flags can appear before or after the file. The last
    // non-flag, non-flag-value argument is the file.
    let mut i = 2;
    while i < args.len() {
        match args[i].as_str() {
            "--llm" => flags.llm_enabled = true,
            "--no-llm" => flags.llm_enabled = false,
            "--debug" => flags.debug = true,
            "--auto-accept" => flags.auto_accept = true,
            "--transparent" => flags.transparent = true,
            "--compose" => flags.compose = true,
            "--backend" => {
                i += 1;
                if i < args.len() {
                    flags.backend = Some(args[i].clone());
                }
            }
            arg if !arg.starts_with("--") => {
                flags.file = arg.to_string();
                // Don't break — flags can follow the file.
            }
            _ => {
                eprintln!("montague: unknown flag `{}`", args[i]);
                process::exit(1);
            }
        }
        i += 1;
    }

    if flags.file.is_empty() {
        eprintln!("montague: missing file. Usage: montague ask [flags] <file>");
        process::exit(1);
    }

    flags
}

fn cmd_ask(args: &[String]) {
    let flags = parse_ask_flags(args);

    // 1. Load lexicon
    let src = read_file(&flags.file);
    let (ast, parse_errs) = parser::parse(&src);
    if !parse_errs.is_empty() {
        for e in &parse_errs {
            eprintln!("parse error: {e}");
        }
        process::exit(1);
    }
    let reg = core::registry::Registry::empty();
    let mut lex = match resolver::resolve(&ast, &reg) {
        Ok(lex) => lex,
        Err(errs) => {
            for e in &errs {
                eprintln!("resolve error: {e}");
            }
            process::exit(1);
        }
    };
    let mut sem: Semantics<String, String, core::AnnotatedTerm<String, String>> =
        resolver::build_semantics(&lex);
    let engine = if flags.compose {
        ReductionEngine::with_composition()
    } else {
        ReductionEngine::standard()
    };
    let mut ctx = ReductionCtx::new(&lex.lattice);

    // 2. Initialize Scryer Prolog with a session knowledge base.
    let mut machine = MachineBuilder::default().build();
    // Silence existence_error for undefined predicates — return "no." instead.
    let _ = machine.run_query(":- set_prolog_flag(unknown, fail).");
    let mut dynamic_preds: std::collections::HashSet<String> = std::collections::HashSet::new();

    // Helper: assert a clause into the Prolog database using assertz/1.
    // Uses dynamic/1 declarations so predicates can be extended incrementally.
    fn assert_clause(
        machine: &mut scryer_prolog::Machine,
        clause: &str,
        dynamic_preds: &mut std::collections::HashSet<String>,
    ) {
        let body = clause.trim().trim_end_matches('.');

        // Extract predicate name/arity for dynamic declaration.
        let pred_sig = if let Some(open) = body.find('(') {
            let name = &body[..open].trim();
            // Count args by counting commas between the parens
            let close = body.rfind(')').unwrap_or(body.len());
            let args_str = &body[open + 1..close];
            let arity = if args_str.trim().is_empty() { 0 } else { args_str.matches(',').count() + 1 };
            format!("{name}/{arity}")
        } else {
            format!("{body}/0")
        };

        // Declare predicate as dynamic the first time it appears.
        if dynamic_preds.insert(pred_sig.clone()) {
            let _ = machine.run_query(&format!(":- dynamic({pred_sig})."));
        }

        // Assert the clause.
        let query = format!("assertz(({body})).");
        let _ = machine.run_query(&query).collect::<Vec<_>>();
    }

    // 3. Initialize LLM (if enabled)
    #[cfg(feature = "llm")]
    let mut llm: Option<Box<dyn Llm>> = if flags.llm_enabled {
        match init_llm_backend(flags.backend.as_deref()) {
            Ok(backend) => {
                if flags.debug {
                    eprintln!("[debug] LLM backend initialized: {}", flags.backend.as_deref().unwrap_or("auto"));
                }
                Some(backend)
            }
            Err(e) => {
                eprintln!("LLM init error: {e}");
                None
            }
        }
    } else {
        None
    };
    #[cfg(not(feature = "llm"))]
    {
        let _ = (&flags.llm_enabled, &flags.transparent);
        if flags.llm_enabled {
            eprintln!("LLM features require building with `--features llm`");
        }
    }

    // 4. Greeting
    if flags.transparent && flags.llm_enabled {
        eprintln!("Montague Q&A (transparent LLM mode) — type anything. :q to quit.\n");
    } else {
        eprintln!("Montague Q&A — type assertions or questions. :q to quit.\n");
    }

    // 5. REPL
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    loop {
        print!("montague> ");
        stdout.flush().unwrap();
        let mut input = String::new();
        match stdin.lock().read_line(&mut input) {
            Ok(0) => break, // EOF
            Err(_) => break,
            _ => {}
        }
        let input = input.trim().to_string();
        if input.is_empty() {
            continue;
        }
        if input == ":q" || input == ":quit" {
            eprintln!("bye.");
            break;
        }

        // Transparent LLM chat mode
        #[cfg(feature = "llm")]
        if flags.transparent && flags.llm_enabled {
            if let Some(ref mut backend) = llm {
                run_transparent_chat(backend, &input, &lex, flags.debug);
                continue;
            }
        }

        // Split input into sentences
        let sentences = split_sentences(&input);
        let sentence_count = sentences.len();

        for sent in &sentences {
            if sentences.len() > 1 && !flags.transparent {
                eprintln!("--- {:?} ---", sent);
            }

            // Detect question vs assertion (case-insensitive)
            let input_lower = sent.to_lowercase();
            let is_question = sent.ends_with('?')
                || input_lower.starts_with("who ")
                || input_lower.starts_with("what ")
                || input_lower.starts_with("is ")
                || input_lower.starts_with("does ")
                || input_lower.starts_with("are ")
                || input_lower.starts_with("do ");
            let clean_input = sent.trim_end_matches('?').trim().to_string();
            let tokenized = tokenize_multiword(&clean_input, &lex.productions);

            let parses = core::get_all_parses_chart(&engine, &ctx, &sem, &tokenized);

            if parses.is_empty() {
                // Failed parse — try LLM recovery (up to 3 attempts)
                #[cfg(feature = "llm")]
                if flags.llm_enabled {
                    if let Some(ref mut backend) = llm {
                        if let Some((mut atoms, mut prods, mut rules)) = try_llm_recover(
                            backend,
                            &lex,
                            sent,
                            &clean_input,
                            flags.debug,
                            flags.auto_accept,
                        ) {
                            let mut parsed = false;
                            for attempt in 0..3 {
                                // Load new Prolog rules
                                for rule in &rules {
                                    assert_clause(&mut machine, rule, &mut dynamic_preds);
                                    if attempt == 0 {
                                        eprintln!("  asserted LLM rule: {rule}");
                                    }
                                }

                                // Add new atoms and productions to lexicon
                                for (name, ty) in &atoms {
                                    lex.atoms.push(montague::mont::ast::AtomEntry {
                                        entity: name.clone(),
                                        doc: None,
                                        type_expr: montague::core::LambekType::Basic(ty.clone()),
                                        span: montague::mont::ast::Span::new(0, 0),
                                    });
                                }
                                for (word, entity) in &prods {
                                    lex.productions.push(montague::mont::ast::ProductionEntry {
                                        words: vec![word.clone()],
                                        entity: entity.clone(),
                                        span: montague::mont::ast::Span::new(0, 0),
                                    });
                                }

                                // Rebuild semantics and re-parse
                                sem = resolver::build_semantics(&lex);
                                ctx = ReductionCtx::new(&lex.lattice);
                                let retokenized = tokenize_multiword(&clean_input, &lex.productions);
                                let retry_parses = core::get_all_parses_chart(&engine, &ctx, &sem, &retokenized);

                                if !retry_parses.is_empty() {
                                    eprintln!("  Re-parse succeeded with LLM suggestions.");
                                    let at = &retry_parses[0];
                                    if is_question {
                                        let query = lower_term_to_prolog(&at.term)
                                            .map(|clauses| {
                                                clauses.iter()
                                                    .map(|s| s.trim_end_matches('.').to_string())
                                                    .collect::<Vec<_>>()
                                                    .join(", ")
                                            })
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
                                                        match a {
                                                            scryer_prolog::LeafAnswer::LeafAnswer { bindings, .. } => {
                                                                let parts: Vec<String> = bindings
                                                                    .iter()
                                                                    .map(|(k, v)| {
    let val = match v {
        scryer_prolog::Term::Atom(s) => s.clone(),
        scryer_prolog::Term::Integer(n) => n.to_string(),
        scryer_prolog::Term::Float(f) => f.to_string(),
        other => format!("{other:?}"),
    };
    format!("{k} = {val}")
})
                                                                    .collect();
                                                                eprintln!("  {}", parts.join(", "));
                                                            }
                                                            _ => eprintln!("  {a:?}"),
                                                        }
                                                    }
                                                }
                                            }
                                            Err(e) => {
                                                let err_str = format!("{e:?}");
                                                if err_str.contains("existence_error") {
                                                    eprintln!("  no.");
                                                } else {
                                                    eprintln!("  query error: {e:?}");
                                                }
                                            }
                                        }
                                    } else {
                                        if let Some(clauses) = lower_term_to_prolog(&at.term) {
                                            for clause in &clauses {
                                                assert_clause(&mut machine, clause, &mut dynamic_preds);
                                                eprintln!("  asserted: {clause}");
                                            }
                                        }
                                    }
                                    parsed = true;
                                    break;
                                }

                                // Re-parse failed — try again with LLM feedback (up to 2 more)
                                if attempt < 2 {
                                    // Diagnose: are there atoms without matching productions?
                                    let prod_entities: Vec<&str> =
                                        prods.iter().map(|(_, e)| e.as_str()).collect();
                                    let atoms_without_prod: Vec<&str> = atoms
                                        .iter()
                                        .filter(|(n, _)| !prod_entities.contains(&n.as_str()))
                                        .map(|(n, _)| n.as_str())
                                        .collect();

                                    let follow_up = if !atoms_without_prod.is_empty() {
                                        let missing = atoms_without_prod.join(", ");
                                        format!(
                                            "The parse failed. The atom(s) `{missing}` were added \
                                             but have no production mapping a surface word to them. \
                                             Please provide the missing PROD entry.\n\
                                             \n\
                                             For example:\n\
                                             PROD immortal --> immortal\n\
                                             \n\
                                             Output in a ``` block."
                                        )
                                    } else {
                                        let lexicon_summary = format_lexicon_summary(&lex);
                                        format!(
                                            "The parse still failed. Here is the current full lexicon:\n\
                                             {lexicon_summary}\n\
                                             \n\
                                             The sentence is: {sent}\n\
                                             \n\
                                             What is missing or wrong? Look at how similar sentences \
                                             parse in this lexicon. What additional entries are \
                                             needed? Output in a ``` block."
                                        )
                                    };
                                    if flags.debug {
                                        eprintln!("[debug] --- LLM retry prompt ---\n{follow_up}");
                                    }
                                    match backend.complete(
                                        "You are a Montague grammar assistant. The user tried your \
                                         suggestions but the parse still failed. Diagnose what\u{2019}s \
                                         missing and provide the missing entries.\n\
                                         Remember Prolog syntax: negation is \\+ not not/1, \
                                         variables are uppercase, constants are lowercase.",
                                        &follow_up,
                                        800,
                                    ) {
                                        Ok(resp) => {
                                            if flags.debug {
                                                eprintln!("[debug] --- LLM retry response ---\n{resp}");
                                            }
                                            let (a2, p2, r2) = apply_llm_suggestions(&resp, &lex);
                                            if a2.is_empty() && p2.is_empty() && r2.is_empty() {
                                                eprintln!("  LLM retry produced no new entries.");
                                                break;
                                            }
                                            eprintln!("  LLM retry suggests additional entries:");
                                            for (n, t) in &a2 {
                                                eprintln!("    + atom:    {n}: {t}.");
                                            }
                                            for (w, e) in &p2 {
                                                eprintln!("    + production: {w} --> {e}.");
                                            }
                                            for r in &r2 {
                                                eprintln!("    + rule:    {r}");
                                            }
                                            atoms = a2;
                                            prods = p2;
                                            rules = r2;
                                        }
                                        Err(e) => {
                                            eprintln!("  LLM retry error: {e}");
                                            break;
                                        }
                                    }
                                }
                            }
                            if parsed {
                                continue;
                            }
                            eprintln!("  Re-parse still failed after LLM suggestions.");
                            continue;
                        }
                    }
                }

                eprintln!("  No parse found for: {sent:?}");
                continue;
            }

            // Take first parse
            let at = &parses[0];

            if is_question {
                // Query
                let query = lower_term_to_prolog(&at.term)
                    .map(|clauses| {
                        clauses.iter()
                            .map(|s| s.trim_end_matches('.').to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    })
                    .unwrap_or_else(|| format!("{:?}", at.term));
                eprintln!("  query: {query}?");

                let query_str = format!("{query}.");
                let answers: Result<Vec<scryer_prolog::LeafAnswer>, _> =
                    machine.run_query(&query_str).collect();
                match answers {
                    Ok(results) => {
                        if results.is_empty() {
                            eprintln!("  no.");

                            // Knowledge gap — LLM suggestion
                            #[cfg(feature = "llm")]
                            if flags.llm_enabled {
                                if let Some(ref mut backend) = llm {
                                    suggest_knowledge_gap(
                                        backend, &lex, &query_str, flags.debug,
                                    );
                                }
                            }
                        } else if results == vec![scryer_prolog::LeafAnswer::True] {
                            eprintln!("  yes.");
                        } else {
                            for a in &results {
                                match a {
                                    scryer_prolog::LeafAnswer::LeafAnswer { bindings, .. } => {
                                        if bindings.is_empty() {
                                            eprintln!("  yes.");
                                        } else {
                                            let parts: Vec<String> = bindings
                                                .iter()
                                                .map(|(k, v)| {
    let val = match v {
        scryer_prolog::Term::Atom(s) => s.clone(),
        scryer_prolog::Term::Integer(n) => n.to_string(),
        scryer_prolog::Term::Float(f) => f.to_string(),
        other => format!("{other:?}"),
    };
    format!("{k} = {val}")
})
                                                .collect();
                                            eprintln!("  {}", parts.join(", "));
                                        }
                                    }
                                    _ => eprintln!("  {a:?}"),
                                }
                            }
                        }
                    }
                    Err(e) => {
                        let err_str = format!("{e:?}");
                        if err_str.contains("existence_error") {
                            eprintln!("  no.");
                        } else {
                            eprintln!("  query error: {e:?}");
                        }
                    }
                }
            } else {
                // Assertion
                if let Some(clauses) = lower_term_to_prolog(&at.term) {
                    for clause in &clauses {
                        assert_clause(&mut machine, clause, &mut dynamic_preds);
                        eprintln!("  asserted: {clause}");
                    }
                } else {
                    eprintln!("  (could not lower to Prolog)");
                }
            }

            // Multiple parses — disambiguate via LLM
            if parses.len() > 1 {
                if sentence_count == 1 {
                    eprintln!("  ({} parses total)", parses.len());
                }
                #[cfg(feature = "llm")]
                if flags.llm_enabled {
                    if let Some(ref mut backend) = llm {
                        disambiguate_parses(backend, sent, &parses, flags.debug);
                    }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// tokenize_multiword — longest-match-first multi-word token replacement
// ---------------------------------------------------------------------------

/// Collect multi-word surface forms from the lexicon (those containing `_`
/// after canonicalization in the .mont parser), sort by length descending,
/// and replace occurrences in the input with underscore-joined canonical
/// forms so that `annotate()`'s `split_whitespace()` correctly produces
/// single tokens for multi-word phrases.
fn tokenize_multiword(input: &str, productions: &[montague::mont::ast::ProductionEntry]) -> String {
    // Collect multi-word surface forms (those with underscores).
    let mut phrases: Vec<&str> = productions
        .iter()
        .flat_map(|p| p.words.iter())
        .filter(|w| w.contains('_'))
        .map(|w| w.as_str())
        .collect();
    if phrases.is_empty() {
        return input.to_string();
    }
    // Sort longest first for greedy longest-match.
    phrases.sort_by_key(|p| -(p.len() as isize));
    // Deduplicate (same phrase may appear in multiple productions).
    phrases.dedup();

    let mut result = input.to_lowercase();
    for phrase in &phrases {
        // The canonical form uses underscores; the user typed spaces.
        let user_form = phrase.replace('_', " ");
        if result.contains(&user_form) {
            result = result.replace(&user_form, phrase);
        }
    }
    result
}

// ---------------------------------------------------------------------------
// split_sentences — break input on sentence boundaries
// ---------------------------------------------------------------------------

fn split_sentences(input: &str) -> Vec<String> {
    let mut sentences = Vec::new();
    let mut current = String::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let ch = chars[i];
        current.push(ch);

        if (ch == '.' || ch == '?' || ch == '!') &&
            (i + 1 >= chars.len() || chars[i + 1] == ' ' || chars[i + 1] == '\n')
        {
            let trimmed = current.trim().to_string();
            if !trimmed.is_empty() {
                sentences.push(trimmed);
            }
            current.clear();
            // Skip the space after punctuation
            if i + 1 < chars.len() && chars[i + 1] == ' ' {
                i += 1;
            }
        }
        i += 1;
    }

    let trimmed = current.trim().to_string();
    if !trimmed.is_empty() {
        sentences.push(trimmed);
    }

    if sentences.is_empty() {
        vec![input.trim().to_string()]
    } else {
        sentences
    }
}

// ---------------------------------------------------------------------------
// format_lexicon_summary — human-readable lexicon for LLM prompts
// ---------------------------------------------------------------------------

#[allow(dead_code)]
fn format_lexicon_summary(lex: &resolver::ResolvedLexicon) -> String {
    let mut s = String::new();

    // Collect unique basic types
    let mut basic_types: Vec<String> = Vec::new();
    for a in &lex.atoms {
        if let montague::core::LambekType::Basic(ref t) = a.type_expr {
            if !basic_types.contains(t) {
                basic_types.push(t.clone());
            }
        }
    }
    if !basic_types.is_empty() {
        s.push_str("Types: ");
        s.push_str(&basic_types.join(", "));
        s.push('\n');
    }

    // Atoms with their types
    if !lex.atoms.is_empty() {
        s.push_str("\nAtoms:\n");
        for a in &lex.atoms {
            let ty_sexp = display_lambek_as_sexp(&a.type_expr, None);
            s.push_str(&format!("  {} : {ty_sexp}\n", a.entity));
        }
    }

    // Productions
    if !lex.productions.is_empty() {
        s.push_str("\nProductions:\n");
        for p in &lex.productions {
            let words = p.words.join(", ");
            s.push_str(&format!("  {} --> {}\n", words, p.entity));
        }
    }

    s
}

// ---------------------------------------------------------------------------
// LLM helper functions (only compiled with the `llm` feature)
// ---------------------------------------------------------------------------

#[cfg(feature = "llm")]
fn init_llm_backend(backend_name: Option<&str>) -> Result<Box<dyn Llm>, String> {
    let pref = match backend_name {
        Some("anthropic") => BackendPreference::Anthropic,
        Some("mistralrs") => BackendPreference::MistralRs,
        Some("ollama") => BackendPreference::Ollama,
        Some(other) => return Err(format!("unknown backend: {other}. Use anthropic, mistralrs, or ollama.")),
        None => {
            // Auto-detect: Anthropic if token set, otherwise MistralRs (GGUF local)
            if env::var("ANTHROPIC_AUTH_TOKEN").is_ok() {
                BackendPreference::Anthropic
            } else {
                BackendPreference::MistralRs
            }
        }
    };

    llm_mod::detect_backend(pref).map_err(|e| format!("{e}"))
}

/// Show raw LLM prompt/response if --debug is on.
#[cfg(feature = "llm")]
fn debug_complete(
    llm: &mut Box<dyn Llm>,
    system_prompt: &str,
    user_prompt: &str,
    max_tokens: usize,
    debug: bool,
) -> Result<String, String> {
    if debug {
        eprintln!("\n[debug] --- LLM system prompt ---\n{system_prompt}");
        eprintln!("[debug] --- LLM user prompt ---\n{user_prompt}");
    }
    let response = llm.complete(system_prompt, user_prompt, max_tokens)
        .map_err(|e| format!("LLM error: {e}"))?;
    if debug {
        eprintln!("[debug] --- LLM response ---\n{response}");
    }
    Ok(response)
}

/// Build the system prompt for failed-parse recovery.
/// Gives the LLM room to analyze before outputting structured entries in a code block.
#[cfg(feature = "llm")]
fn build_recovery_prompt(
    lexicon_summary: &str,
    sentence: &str,
    unknown_words: &[String],
) -> (String, String) {
    let system = String::from(
        "You are a Montague grammar assistant. Your job is to propose minimal lexicon \
         additions so that a sentence can be parsed by a Lambek-calculus parser.\n\
         \n\
         How to determine the right type for an unknown word:\n\
         - Look at similar words already in the lexicon and copy their type.\n\
         - If the sentence is \"Socrates is X\" and the lexicon has \"mortal: Adjective\", \
         then X is also an Adjective (it appears in the same position after \"is\").\n\
         - Nouns are things (people, cats, cities). Adjectives are properties (wise, mortal).\n\
         - Prefer the simplest type that fits. Use Basic types like Noun, Adjective, Person.\n\
         \n\
         How to write RULE lines:\n\
         - Only add a RULE if the new word logically depends on EXISTING words.\n\
         - The RULE HEAD must be the NEW word you are defining.\n\
         - The RULE BODY must only reference words that ALREADY EXIST in the lexicon.\n\
         - Example: if adding \"immortal\" and \"mortal\" already exists, the rule is:\n\
           RULE immortal(X) :- \\+ mortal(X).\n\
           This defines immortal (new) in terms of mortal (existing).\n\
         - WRONG: RULE mortal(X) :- \\+ immortal(X). (head is an existing word)\n\
         \n\
         Prolog syntax rules for RULE lines:\n\
         - Variables are uppercase: X, Y, Name (never lowercase).\n\
         - Constants are lowercase: socrates, mortal, cat.\n\
         - Negation uses \\+ (backslash-plus): \\+ mortal(X) means \"not mortal(X)\".\n\
         - NEVER use not/1 — it does not exist in this Prolog.\n\
         - Conjunction uses comma: a(X), b(X).\n\
         - Implication uses :- (head :- body).\n\
         - Every clause ends with a period.\n\
         \n\
         First analyze each unknown word: what role does it play in the sentence? \
         What existing lexicon word is most similar? Then output entries in a code block.",
    );

    let unknown_list = unknown_words.join(", ");
    let user = format!(
        "Current lexicon:\n\
         {lexicon_summary}\n\
         \n\
         Sentence: {sentence}\n\
         Unknown words: {unknown_list}\n\
         \n\
         First, analyze each unknown word — its grammatical role and the most similar \
         existing word in the lexicon. Then output your proposed entries in a code block \
         using this format:\n\
         \n\
         ```\n\
         ATOM name type\n\
         PROD word --> name\n\
         PROD \"multi word phrase\" --> name\n\
         RULE head(args) :- body(args).\n\
         ```\n\
         \n\
         IMPORTANT about PROD: Use double-quoted strings for multi-word phrases.\n\
         For example: PROD \"as well as\" --> as_well_as\n\
         Spaces inside quotes are automatically canonicalized to underscores.\n\
         \n\
         Examples:\n\
         \n\
         If the lexicon has `mortal: Adjective` and the sentence is \"Socrates is wise\":\n\
         Since \"wise\" appears in the same position as \"mortal\" (after \"is\"), it\u{2019}s an Adjective.\n\
         ```\n\
         ATOM wise Adjective\n\
         PROD wise --> wise\n\
         ```\n\
         \n\
         If the lexicon has `cat: Noun` and the sentence is \"a dog sleeps\":\n\
         ```\n\
         ATOM dog Noun\n\
         PROD dog --> dog\n\
         ```\n\
         \n\
         If the word is the negation of an existing word (e.g. \"immortal\" vs \"mortal\"):\n\
         ```\n\
         ATOM immortal Adjective\n\
         PROD immortal --> immortal\n\
         RULE immortal(X) :- \\+ mortal(X).\n\
         ```\n\
         \n\
         For multi-word conjunctions (\"as well as\", \"in order to\"):\n\
         ```\n\
         ATOM as_well_as (Adjective \\ Adjective) / Adjective\n\
         PROD \"as well as\" --> as_well_as\n\
         ```"
    );

    (system, user)
}

/// Parse LLM recovery suggestions from the response.
/// Collects entries from ALL ``` blocks, then falls back to full response.
#[cfg(feature = "llm")]
fn apply_llm_suggestions(
    response: &str,
    _lex: &resolver::ResolvedLexicon,
) -> (Vec<(String, String)>, Vec<(String, String)>, Vec<String>) {
    let mut atoms: Vec<(String, String)> = Vec::new();
    let mut prods: Vec<(String, String)> = Vec::new();
    let mut rules: Vec<String> = Vec::new();

    let mut parse_body = |body: &str| {
        for line in body.lines() {
            let line = line.trim();
            if line.is_empty() || line == "```" {
                continue;
            }

            if let Some(rest) = line.strip_prefix("ATOM ") {
                let parts: Vec<&str> = rest.split_whitespace().collect();
                if parts.len() >= 2 {
                    let name = parts[0].to_string();
                    let ty = parts[1..].join(" ");
                    atoms.push((name, ty));
                }
            } else if let Some(rest) = line.strip_prefix("PROD ") {
                if let Some(idx) = rest.find("-->") {
                    let word = rest[..idx].trim().to_string();
                    let entity = rest[idx + 3..].trim().to_string();
                    if !word.is_empty() && !entity.is_empty() {
                        prods.push((word, entity));
                    }
                }
            } else if let Some(rest) = line.strip_prefix("RULE ") {
                let rule = rest.trim_end_matches('.').trim().to_string();
                if !rule.is_empty() {
                    rules.push(format!("{rule}."));
                }
            }
        }
    };

    // Scan ALL ``` blocks in the response.
    let mut remaining = response;
    let mut found_blocks = false;
    while let Some(start) = remaining.find("```") {
        let after_start = &remaining[start + 3..];
        if let Some(end) = after_start.find("```") {
            parse_body(&after_start[..end]);
            found_blocks = true;
            remaining = &after_start[end + 3..];
        } else {
            break;
        }
    }

    // Fallback: if no ``` blocks found, scan the full response.
    if !found_blocks {
        parse_body(response);
    }

    // Sanitize: fix common Prolog syntax mistakes in rules.
    for rule in &mut rules {
        // Replace `not(` with `\+(` — `not/1` doesn't exist in Scryer Prolog.
        *rule = rule.replace("not(", "\\+ (");
    }

    (atoms, prods, rules)
}

/// Try LLM recovery for a failed parse. Returns accepted (atoms, prods, rules) or None.
#[cfg(feature = "llm")]
fn try_llm_recover(
    llm: &mut Box<dyn Llm>,
    lex: &resolver::ResolvedLexicon,
    original_sentence: &str,
    clean_sentence: &str,
    debug: bool,
    auto_accept: bool,
) -> Option<(Vec<(String, String)>, Vec<(String, String)>, Vec<String>)> {
    // Identify unknown words by checking each word against the lexicon
    let known_words: std::collections::HashSet<String> = lex
        .productions
        .iter()
        .flat_map(|p| p.words.iter().cloned())
        .collect();

    let unknown_words: Vec<String> = clean_sentence
        .split_whitespace()
        .filter(|w| {
            let lower = w.to_lowercase();
            let stripped = lower.trim_end_matches(|c: char| c == '.' || c == ',' || c == '!' || c == '?');
            !known_words.contains(stripped) && stripped.len() > 1
        })
        .map(|w| w.to_lowercase())
        .collect();

    if unknown_words.is_empty() {
        return None;
    }

    let lexicon_summary = format_lexicon_summary(lex);
    let (system_prompt, user_prompt) =
        build_recovery_prompt(&lexicon_summary, original_sentence, &unknown_words);

    let response = match debug_complete(llm, &system_prompt, &user_prompt, 800, debug) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("  LLM recovery error: {e}");
            return None;
        }
    };

    let (atoms, prods, rules) = apply_llm_suggestions(&response, lex);

    if atoms.is_empty() && prods.is_empty() && rules.is_empty() {
        if debug {
            eprintln!("  [debug] LLM response parsed to no entries");
        }
        return None;
    }

    // Display suggestions
    eprintln!("\n  LLM suggests:");
    for (name, ty) in &atoms {
        eprintln!("    + atom:    {name}: {ty}.");
    }
    for (word, entity) in &prods {
        eprintln!("    + production: {word} --> {entity}.");
    }
    for rule in &rules {
        eprintln!("    + rule:    {rule}");
    }

    // Accept or prompt
    let accept = if auto_accept {
        eprintln!("  (auto-accepted)");
        true
    } else {
        eprint!("\n  Accept? [Y/n] ");
        io::stdout().flush().unwrap();
        let mut answer = String::new();
        if io::stdin().lock().read_line(&mut answer).is_err() {
            return None;
        }
        let answer = answer.trim().to_lowercase();
        answer.is_empty() || answer == "y" || answer == "yes"
    };

    if accept {
        Some((atoms, prods, rules))
    } else {
        None
    }
}

/// Suggest bridging rules when a Prolog query returns "no".
#[cfg(feature = "llm")]
fn suggest_knowledge_gap(
    llm: &mut Box<dyn Llm>,
    lex: &resolver::ResolvedLexicon,
    query: &str,
    debug: bool,
) {
    let lexicon_summary = format_lexicon_summary(lex);

    let system = String::from(
        "You are a Montague logic assistant. A Prolog query returned 'no' \
         (no results). Given the knowledge base summary and the query, \
         suggest bridging rules that might fill the gap.\n\
         Prolog syntax: variables are uppercase (X, Y), constants are lowercase, \
         negation is \\+ not not/1, conjunction is comma.\n\
         Respond with RULE lines or 'NONE' if no reasonable rule exists.",
    );

    let user = format!(
        "Knowledge base:\n\
         {lexicon_summary}\n\
         \n\
         Query that returned no: {query}\n\
         \n\
         Suggest bridging rules using this format:\n\
         RULE head(args) :- body(args).\n\
         \n\
         Example: if the query is \"mortal(socrates).\" and the KB has \
         \"man(socrates)\" but no definition of mortality:\n\
         RULE mortal(X) :- man(X).\n\
         \n\
         Example with negation: if the query is \"immortal(socrates).\" and \
         \"mortal\" is defined:\n\
         RULE immortal(X) :- \\+ mortal(X).\n\
         \n\
         Respond ONLY with rule suggestions or NONE."
    );

    match debug_complete(llm, &system, &user, 400, debug) {
        Ok(response) => {
            let trimmed = response.trim();
            if trimmed == "NONE" || trimmed.is_empty() {
                return;
            }
            eprintln!("\n  LLM suggests bridging rules:");
            for line in trimmed.lines() {
                if let Some(rule) = line.strip_prefix("RULE ") {
                    eprintln!("    {rule}");
                }
            }
        }
        Err(e) => {
            if debug {
                eprintln!("  [debug] Knowledge gap LLM error: {e}");
            }
        }
    }
}

/// Ask the LLM to disambiguate multiple parses.
#[cfg(feature = "llm")]
fn disambiguate_parses(
    llm: &mut Box<dyn Llm>,
    sentence: &str,
    parses: &[montague::core::AnnotatedTerm<String, String>],
    debug: bool,
) {
    if parses.len() <= 1 {
        return;
    }

    let system = String::from(
        "You are a linguistic disambiguation assistant. Given a sentence and \
         multiple parse trees, rank them by plausibility in a typical conversation. \
         Respond with the number of the most plausible parse and a brief reason.",
    );

    let mut parse_list = String::new();
    for (i, at) in parses.iter().enumerate() {
        let sexp = display_term_as_sexp(&at.term);
        parse_list.push_str(&format!("{}. {sexp}\n", i + 1));
    }

    let user = format!(
        "Sentence: {sentence}\n\
         \n\
         Parses:\n\
         {parse_list}\n\
         Which reading is most plausible? Respond with just the number and a brief reason."
    );

    match debug_complete(llm, &system, &user, 200, debug) {
        Ok(response) => {
            eprintln!("\n  LLM disambiguation: {response}");
        }
        Err(e) => {
            if debug {
                eprintln!("  [debug] Disambiguation LLM error: {e}");
            }
        }
    }
}

/// Transparent chat mode: LLM sees Montague state as system context.
#[cfg(feature = "llm")]
fn run_transparent_chat(
    llm: &mut Box<dyn Llm>,
    user_input: &str,
    lex: &resolver::ResolvedLexicon,
    _debug: bool,
) {
    let lexicon_summary = format_lexicon_summary(lex);

    let system = format!(
        "You are Montague, a linguistic assistant with access to a Lambek calculus \
         parser and Prolog inference engine. You can parse sentences and answer \
         questions using the lexicon and knowledge base described below.\n\
         \n\
         Current lexicon:\n\
         {lexicon_summary}\n\
         \n\
         Respond naturally to the user. If they ask something you can't answer with \
         the available tools, let them know."
    );

    match llm.complete(&system, user_input, 500) {
        Ok(response) => {
            println!("{response}");
        }
        Err(e) => {
            eprintln!("  LLM error: {e}");
        }
    }
}

// ---------------------------------------------------------------------------
// cmd_init — bootstrap a .mont lexicon from a natural-language description
// ---------------------------------------------------------------------------

#[cfg(feature = "llm")]
fn cmd_init(args: &[String]) {
    let mut llm_enabled = false;
    let mut backend_name: Option<String> = None;
    let mut debug = false;
    let mut output_file: Option<String> = None;
    let mut description = String::new();

    let mut i = 2;
    while i < args.len() {
        match args[i].as_str() {
            "--llm" => llm_enabled = true,
            "--debug" => debug = true,
            "--backend" => {
                i += 1;
                if i < args.len() {
                    backend_name = Some(args[i].clone());
                } else {
                    eprintln!("montague: --backend requires a value");
                    process::exit(1);
                }
            }
            "--output" => {
                i += 1;
                if i < args.len() {
                    output_file = Some(args[i].clone());
                } else {
                    eprintln!("montague: --output requires a file path");
                    process::exit(1);
                }
            }
            arg if !arg.starts_with("--") => {
                if !description.is_empty() {
                    description.push(' ');
                }
                description.push_str(arg);
            }
            _ => {
                eprintln!("montague: unknown flag `{}`", args[i]);
                process::exit(1);
            }
        }
        i += 1;
    }

    if !llm_enabled {
        eprintln!("montague: `init` requires --llm. Usage: montague init --llm [flags] <description>");
        process::exit(1);
    }

    if description.is_empty() {
        eprintln!("montague: missing description. Usage: montague init --llm <description>");
        process::exit(1);
    }

    let mut llm = match init_llm_backend(backend_name.as_deref()) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("LLM init error: {e}");
            process::exit(1);
        }
    };

    eprintln!("Bootstrapping lexicon from description...");

    let system = String::from(
        "You are a Montague DSL expert. Given a natural language description of \
         the kinds of sentences to parse, generate a complete .mont lexicon file. \
         Include Type declarations, atoms with Lambek types, subtype relationships, \
         and surface-form productions.\n\
         \n\
         The .mont format:\n\
         Type = TypeName | OtherType.\n\
         SubType :< SuperType.\n\
         atom_name: (Type / Type) \\ Type.\n\
         word --> atom_name.\n\
         word1, word2 --> atom_name.\n\
         \"multi word\" --> atom_name.\n\
         \n\
         Multi-word productions: use double-quoted strings on the LHS of -->.\n\
         Spaces inside quotes are canonicalized to underscores. Example:\n\
         \"as well as\" --> as_well_as.\n\
         \n\
         Common patterns:\n\
         - Proper names: entity: Noun.  with production: entity --> entity.\n\
         - Adjectives: adj: Adjective.  with production: adj --> adj.\n\
         - Intransitive verbs: verb: Noun \\ Sentence.  production: verb --> verb.\n\
         - Transitive verbs: verb: (Noun \\ Sentence) / Noun.  production: verb --> verb.\n\
         - Copula \"is\": is_cop: (Noun \\ Sentence) / Adjective.\n\
         - Articles: a_art: Adjective / Noun.\n\
         - Quantifiers: all_q: (Sentence / (Noun \\ Sentence)) / Noun.\n\
         \n\
         For questions, include: ask_op: Question / Sentence.\n\
         \n\
         Respond ONLY with the .mont file content. No explanation."
    );

    let user = format!(
        "Generate a complete .mont lexicon for: {description}\n\
         \n\
         Include Type declarations, atoms with types, subtype relations, and \
         surface-form productions. Follow the patterns shown in the system prompt."
    );

    let response = match debug_complete(&mut llm, &system, &user, 2000, debug) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("LLM error: {e}");
            process::exit(1);
        }
    };

    // Strip any markdown fences or extra text
    let content = response
        .trim()
        .trim_start_matches("```mont")
        .trim_start_matches("```")
        .trim_end_matches("```")
        .trim();

    // Validate by parsing
    let (ast, errs) = parser::parse(content);
    if !errs.is_empty() {
        eprintln!("Generated .mont has parse errors:");
        for e in &errs {
            eprintln!("  {e}");
        }
        eprintln!("\nRaw output:\n{content}");
        process::exit(1);
    }

    // Also try resolving
    let reg = core::registry::Registry::empty();
    match resolver::resolve(&ast, &reg) {
        Ok(lex) => {
            let atom_count = lex.atoms.len();
            let prod_count = lex.productions.len();
            eprintln!("Generated valid lexicon: {atom_count} atoms, {prod_count} productions.");
        }
        Err(errs) => {
            eprintln!("Generated .mont has resolution errors:");
            for e in &errs {
                eprintln!("  {e}");
            }
            eprintln!("\nRaw output:\n{content}");
            process::exit(1);
        }
    }

    // Write output
    if let Some(ref path) = output_file {
        fs::write(path, content).unwrap_or_else(|e| {
            eprintln!("Cannot write `{path}`: {e}");
            process::exit(1);
        });
        eprintln!("Written to {path}");
    } else {
        println!("{content}");
    }
}

#[cfg(not(feature = "llm"))]
fn cmd_init(args: &[String]) {
    eprintln!("montague: `init` requires LLM support. Build with `cargo build --features llm`.");
    eprintln!("Usage: montague init --llm <description>");
    let _ = args;
    process::exit(1);
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn require_arg<'a>(args: &'a [String], idx: usize, usage: &str) -> &'a str {
    if args.len() <= idx {
        eprintln!("montague: missing argument. Usage: {usage}");
        process::exit(1);
    }
    &args[idx]
}

fn read_file(path: &str) -> String {
    fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("montague: cannot read `{path}`: {e}");
        process::exit(1);
    })
}
