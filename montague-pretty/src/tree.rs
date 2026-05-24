//! Linguistics-style parse-tree rendering.
//!
//! Produces a top-down tree with Lambek types at every interior node
//! and surface words at the leaves in left-to-right sentence order.

use montague_core::types::{LambekType, Term};

// ---------------------------------------------------------------------------
// Type formatting
// ---------------------------------------------------------------------------

pub fn format_type<T: std::fmt::Debug>(ty: &LambekType<T>) -> String {
    match ty {
        LambekType::Basic(t) => format!("{t:?}").trim_matches('"').to_string(),
        LambekType::RightArrow(a, b) => {
            let aa = format_type(a);
            let bb = format_type(b);
            let a_p = if needs_paren(a) {
                format!("({aa})")
            } else {
                aa
            };
            format!("{a_p}\\{bb}")
        }
        LambekType::LeftArrow(a, b) => {
            let aa = format_type(a);
            let bb = format_type(b);
            let a_p = if needs_paren(a) {
                format!("({aa})")
            } else {
                aa
            };
            let b_p = if needs_paren(b) {
                format!("({bb})")
            } else {
                bb
            };
            format!("{a_p}/{b_p}")
        }
        LambekType::Conj(a, b) => format!("{}∧{}", format_type(a), format_type(b)),
        LambekType::Disj(a, b) => format!("{}∨{}", format_type(a), format_type(b)),
        _ => format!("{ty:?}"),
    }
}

fn needs_paren<T: std::fmt::Debug>(ty: &LambekType<T>) -> bool {
    !matches!(ty, LambekType::Basic(_))
}

// ---------------------------------------------------------------------------
// Typed tree structure
// ---------------------------------------------------------------------------

pub struct TypedTree {
    pub ty_label: String,
    pub children: Vec<TypedTree>,
    pub word: Option<String>,
    pub subtype_note: Option<String>,
}

pub type TypeLookup<'a> = dyn Fn(&str) -> Option<LambekType<String>> + 'a;

/// Build a typed parse tree from a reduced Term and a type lookup.
pub fn build_typed_tree<A: std::fmt::Display>(
    term: &Term<A>,
    root_ty: &LambekType<String>,
    type_of: &TypeLookup,
) -> TypedTree {
    let (tree, _) = build_node(term, root_ty, type_of);
    tree
}

fn term_type<A: std::fmt::Display>(
    term: &Term<A>,
    type_of: &TypeLookup,
) -> Option<LambekType<String>> {
    match term {
        Term::Atom(a) => type_of(&format!("{a}")),
        Term::App(f, _) => {
            let f_ty = term_type(f, type_of)?;
            match f_ty {
                LambekType::LeftArrow(a, _) => Some(*a),
                LambekType::RightArrow(_, b) => Some(*b),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Build a right-branching chain for `App(f, [a1, ..., aN])`.
fn build_app_chain<A: std::fmt::Display>(
    f: &Term<A>,
    args: &[Term<A>],
    f_ty: &LambekType<String>,
    type_of: &TypeLookup,
) -> (TypedTree, LambekType<String>) {
    let mut current_ty = f_ty.clone();
    let (mut current_tree, _) = build_node(f, &current_ty, type_of);

    for arg in args.iter() {
        let (next_result, next_arg_ty) = match &current_ty {
            LambekType::LeftArrow(a, b) => ((**a).clone(), (**b).clone()),
            LambekType::RightArrow(a, b) => ((**b).clone(), (**a).clone()),
            _ => {
                let (arg_tree, _) = build_node(arg, &current_ty, type_of);
                let kids = if current_tree.children.is_empty() && current_tree.word.is_some() {
                    vec![current_tree, arg_tree]
                } else {
                    let mut kids = current_tree.children;
                    kids.push(arg_tree);
                    kids
                };
                current_tree = TypedTree {
                    ty_label: format_type(&current_ty),
                    children: kids,
                    word: None,
                    subtype_note: None,
                };
                continue;
            }
        };
        let (arg_tree, _) = build_node(arg, &next_arg_ty, type_of);
        // RightArrow: argument absorbs from the LEFT (appears before function).
        // LeftArrow: argument absorbs from the RIGHT (appears after function).
        let (left, right) = if matches!(&current_ty, LambekType::RightArrow(..)) {
            (arg_tree, current_tree)
        } else {
            (current_tree, arg_tree)
        };
        current_tree = TypedTree {
            ty_label: format_type(&next_result),
            children: vec![left, right],
            word: None,
            subtype_note: None,
        };
        current_ty = next_result;
    }

    (current_tree, current_ty)
}

fn build_node<A: std::fmt::Display>(
    term: &Term<A>,
    result_ty: &LambekType<String>,
    type_of: &TypeLookup,
) -> (TypedTree, LambekType<String>) {
    match term {
        Term::Atom(a) => {
            let name = format!("{a}");
            let actual = type_of(&name).unwrap_or_else(|| result_ty.clone());
            let actual_fmt = format_type(&actual);
            let result_fmt = format_type(result_ty);
            let subtype = if actual_fmt != result_fmt {
                Some(actual_fmt)
            } else {
                None
            };
            (
                TypedTree {
                    ty_label: subtype.clone().unwrap_or_else(|| result_fmt.clone()),
                    children: vec![],
                    word: Some(name),
                    subtype_note: subtype,
                },
                actual,
            )
        }
        Term::Var(s) => (
            TypedTree {
                ty_label: s.clone(),
                children: vec![],
                word: Some(s.clone()),
                subtype_note: None,
            },
            result_ty.clone(),
        ),
        Term::Lambda(_s, body) => {
            let (child, _) = build_node(body, result_ty, type_of);
            (
                TypedTree {
                    ty_label: format_type(result_ty),
                    children: vec![child],
                    word: None,
                    subtype_note: None,
                },
                result_ty.clone(),
            )
        }
        Term::App(f, args) => {
            let f_ty = term_type(f, type_of).unwrap_or_else(|| result_ty.clone());
            build_app_chain(f, args, &f_ty, type_of)
        }
    }
}

// ---------------------------------------------------------------------------
// Rendering: grid-based top-down tree
// ---------------------------------------------------------------------------

/// Layout info for one node at one tree depth.
struct RowEntry {
    /// Start leaf index (inclusive).
    col: usize,
    /// Number of leaves spanned.
    span: usize,
    /// Type label displayed at this position.
    label: String,
}

/// Collect leaves in left-to-right order.
fn leaves(tree: &TypedTree) -> Vec<String> {
    let mut out = vec![];
    gather_leaves(tree, &mut out);
    out
}

fn gather_leaves(tree: &TypedTree, out: &mut Vec<String>) {
    if let Some(ref w) = tree.word {
        if tree.children.is_empty() {
            out.push(w.clone());
            return;
        }
        // Interior nodes can also have a word (function atom) — skip for leaf purposes.
    }
    for c in &tree.children {
        gather_leaves(c, out);
    }
}

/// Total leaf count under a node.
fn leaf_span(tree: &TypedTree) -> usize {
    if tree.children.is_empty() {
        1
    } else {
        tree.children.iter().map(leaf_span).sum()
    }
}

/// Collect all type-labeled nodes into rows indexed by depth.
///
/// After initial collection, leaf entries are padded down to the maximum
/// depth so that every parent can find its children in the immediately
/// following row. This is necessary because leaf children of a shallow
/// parent would otherwise skip the depths occupied by a sibling's deeper
/// subtree.
fn collect_rows(tree: &TypedTree, col: usize, depth: usize, rows: &mut Vec<Vec<RowEntry>>) {
    while rows.len() <= depth {
        rows.push(vec![]);
    }
    if tree.children.is_empty() {
        if !tree.ty_label.is_empty() {
            rows[depth].push(RowEntry {
                col,
                span: 1,
                label: tree.ty_label.clone(),
            });
        }
        return;
    }
    if !tree.ty_label.is_empty() {
        let span = leaf_span(tree);
        rows[depth].push(RowEntry {
            col,
            span,
            label: tree.ty_label.clone(),
        });
    }
    let mut child_col = col;
    for child in &tree.children {
        let cs = leaf_span(child);
        collect_rows(child, child_col, depth + 1, rows);
        child_col += cs;
    }
}

fn span_center(col: usize, span: usize, leaves: &[String], spacing: usize, pad: usize) -> usize {
    let mut pos = pad; // offset for left padding
    for (i, leaf) in leaves.iter().enumerate() {
        if i == col {
            let mut w = 0usize;
            for (k, leaf) in leaves[col..col + span].iter().enumerate() {
                if k > 0 {
                    w += spacing;
                }
                w += leaf.len();
            }
            return pos + w / 2;
        }
        pos += leaf.len() + spacing;
    }
    pos
}

/// Widest type label in the tree.
fn max_label_width(tree: &TypedTree) -> usize {
    let mut w = tree.ty_label.len();
    for child in &tree.children {
        w = w.max(max_label_width(child));
    }
    w
}

/// Render the tree as a string.
pub fn render_typed_tree(tree: &TypedTree) -> String {
    let leaves = leaves(tree);
    if leaves.is_empty() {
        return String::new();
    }

    // Tight spacing — the non-destructive label writer handles minor overlaps
    // gracefully. Padding on the sides prevents edge labels from being clipped.
    let max_lbl = max_label_width(tree);
    let spacing = (max_lbl * 2 / 5).max(3);
    let pad = (max_lbl / 3).max(2);
    let leaf_line = format!(
        "{}{}{}",
        " ".repeat(pad),
        leaves.join(&" ".repeat(spacing)),
        " ".repeat(pad),
    );
    let total_width = leaf_line.len().max(1);

    let mut rows: Vec<Vec<RowEntry>> = vec![];
    collect_rows(tree, 0, 0, &mut rows);

    // Pad shallow leaf entries down to the max depth so every parent
    // finds its children in the row immediately below.
    let max_depth = rows.len();
    for d in 0..max_depth {
        let to_pad: Vec<(usize, usize)> = rows[d]
            .iter()
            .filter(|e| e.span == 1 && d + 1 < max_depth)
            .map(|e| (e.col, e.span))
            .collect();
        for (col, span) in to_pad {
            let has_below = rows[d + 1].iter().any(|e| e.col == col);
            if !has_below {
                rows[d + 1].push(RowEntry {
                    col,
                    span,
                    label: String::new(),
                });
            }
        }
    }

    let mut out = String::new();

    // Build the display rows: interior rows + leaf word row at the bottom.
    let leaf_word_row: Vec<RowEntry> = leaves
        .iter()
        .enumerate()
        .map(|(i, _w)| RowEntry {
            col: i,
            span: 1,
            label: String::new(),
        })
        .collect();
    let display_rows: Vec<&[RowEntry]> = rows
        .iter()
        .map(|r| r.as_slice())
        .chain(std::iter::once(leaf_word_row.as_slice()))
        .collect();

    for (ri, row) in display_rows.iter().enumerate() {
        let is_last = ri == display_rows.len() - 1;

        if is_last {
            out.push_str(&leaf_line);
            out.push('\n');
        } else {
            let next_row = display_rows[ri + 1];

            // Build label line: labels first, then pipes in empty spots.
            let mut line_chars = vec![' '; total_width];

            // 1. Labels (highest priority).
            for entry in row.iter().filter(|e| !e.label.is_empty()) {
                let ctr = span_center(entry.col, entry.span, &leaves, spacing, pad);
                let half = entry.label.len() / 2;
                let start = ctr.saturating_sub(half);
                for (i, ch) in entry.label.chars().enumerate() {
                    let pos = start + i;
                    if pos < total_width {
                        line_chars[pos] = ch;
                    }
                }
            }

            // 2. Pipes from ancestors passing through (only into empty slots).
            if ri > 0 {
                for entry in display_rows[ri - 1].iter() {
                    let has_kids_here = row.iter().any(|c| {
                        c.col >= entry.col && c.col < entry.col + entry.span && !c.label.is_empty()
                    });
                    if !has_kids_here {
                        let ctr = span_center(entry.col, entry.span, &leaves, spacing, pad);
                        if ctr < total_width && line_chars[ctr] == ' ' {
                            line_chars[ctr] = '│';
                        }
                    }
                }
            }
            // 3. Pipes from this level's entries with no children below.
            for entry in row.iter() {
                let has_kids = next_row.iter().any(|c| {
                    c.col >= entry.col && c.col < entry.col + entry.span && !c.label.is_empty()
                });
                if !has_kids {
                    let ctr = span_center(entry.col, entry.span, &leaves, spacing, pad);
                    if ctr < total_width && line_chars[ctr] == ' ' {
                        line_chars[ctr] = '│';
                    }
                }
            }
            let label_line: String = line_chars.into_iter().collect();
            out.push_str(label_line.trim_end());
            out.push('\n');

            let next_row = display_rows[ri + 1];
            let mut conn_chars = vec![' '; total_width];
            for parent in *row {
                let kids: Vec<&RowEntry> = next_row
                    .iter()
                    .filter(|c| c.col >= parent.col && c.col < parent.col + parent.span)
                    .collect();
                if kids.is_empty() {
                    continue;
                }
                if kids.len() == 1 {
                    let c = span_center(kids[0].col, kids[0].span, &leaves, spacing, pad);
                    if c < total_width {
                        conn_chars[c] = '│';
                    }
                } else {
                    let first = span_center(kids[0].col, kids[0].span, &leaves, spacing, pad);
                    let last = span_center(
                        kids[kids.len() - 1].col,
                        kids[kids.len() - 1].span,
                        &leaves,
                        spacing,
                        pad,
                    );
                    for ch in &mut conn_chars[first..=last.min(total_width - 1)] {
                        *ch = '─';
                    }
                    if first < total_width {
                        conn_chars[first] = '┌';
                    }
                    if last < total_width {
                        conn_chars[last] = '┐';
                    }
                    for k in &kids[1..kids.len() - 1] {
                        let j = span_center(k.col, k.span, &leaves, spacing, pad);
                        if j < total_width {
                            conn_chars[j] = '┴';
                        }
                    }
                }
            }
            let conn_line: String = conn_chars.into_iter().collect();
            let conn_line = conn_line.trim_end().to_string();
            if !conn_line.is_empty() {
                out.push_str(&conn_line);
                out.push('\n');
            }
        }
    }

    out
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    type SType = LambekType<String>;

    fn basic(s: &str) -> SType {
        LambekType::Basic(s.to_string())
    }
    fn slash(a: SType, b: SType) -> SType {
        LambekType::LeftArrow(Box::new(a), Box::new(b))
    }
    fn backslash(a: SType, b: SType) -> SType {
        LambekType::RightArrow(Box::new(a), Box::new(b))
    }

    fn make_lookup<'a>(map: &'a HashMap<String, SType>) -> Box<TypeLookup<'a>> {
        Box::new(move |name: &str| map.get(name).cloned())
    }

    #[test]
    fn format_simple() {
        let ty = backslash(basic("NP"), basic("S"));
        assert_eq!(format_type(&ty), "NP\\S");
    }

    #[test]
    fn simple_tree_renders() {
        let mut m: HashMap<String, SType> = HashMap::new();
        m.insert("walks".into(), backslash(basic("NP"), basic("S")));
        m.insert("john".into(), basic("NP"));

        let lookup = make_lookup(&m);
        let term = Term::App(Box::new(Term::Atom("walks")), vec![Term::Atom("john")]);
        let root = basic("S");
        let tree = build_typed_tree(&term, &root, &lookup);
        let out = render_typed_tree(&tree);
        assert!(out.contains("S"), "out: {out}");
        assert!(out.contains("john"));
        assert!(out.contains("walks"));
    }

    #[test]
    fn nested_app_tree_renders() {
        let mut m: HashMap<String, SType> = HashMap::new();
        m.insert("walks".into(), backslash(basic("NP"), basic("S")));
        m.insert("a_art".into(), slash(basic("NP"), basic("N")));
        m.insert("man".into(), basic("N"));

        let lookup = make_lookup(&m);
        let term = Term::App(
            Box::new(Term::Atom("walks")),
            vec![Term::App(
                Box::new(Term::Atom("a_art")),
                vec![Term::Atom("man")],
            )],
        );
        let root = basic("S");
        let tree = build_typed_tree(&term, &root, &lookup);
        let out = render_typed_tree(&tree);
        assert!(out.contains("S"), "out: {out}");
        assert!(out.contains("NP"));
        assert!(out.contains("walks"));
        assert!(out.contains("a_art"));
        assert!(out.contains("man"));
    }
}
