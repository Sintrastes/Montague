/// Types in the Lambek grammar parameterized by a set of base types `T`.
///
/// - `RightArrow(x, y)`: absorbs an argument of type `x` from the LEFT, produces `y`
/// - `LeftArrow(x, y)`: absorbs an argument of type `y` from the RIGHT, produces `x`
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LambekType<T> {
    Basic(T),
    RightArrow(Box<LambekType<T>>, Box<LambekType<T>>),
    LeftArrow(Box<LambekType<T>>, Box<LambekType<T>>),
}

/// Lattice partial order for subtyping — distinct from `std::cmp::PartialOrd`.
///
/// Arrows are contravariant in their first argument and covariant in their second,
/// matching the Haskell `PartialOrd (LambekType t)` instance.
pub trait LatticeOrd {
    fn leq(&self, other: &Self) -> bool;
}

impl<T: LatticeOrd> LatticeOrd for LambekType<T> {
    fn leq(&self, other: &Self) -> bool {
        match (self, other) {
            (LambekType::Basic(a), LambekType::Basic(b)) => a.leq(b),
            (LambekType::RightArrow(x, y), LambekType::RightArrow(x2, y2)) => {
                x2.leq(x) && y.leq(y2)
            }
            (LambekType::LeftArrow(x, y), LambekType::LeftArrow(x2, y2)) => {
                x2.leq(x) && y.leq(y2)
            }
            _ => false,
        }
    }
}

impl<T: LatticeOrd> LambekType<T> {
    pub fn geq(&self, other: &Self) -> bool {
        other.leq(self)
    }
}

/// Terms in the Lambek calculus.
#[derive(Debug, Clone, PartialEq)]
pub enum Term<A> {
    Atom(A),
    Var(String),
    Lambda(String, Box<Term<A>>),
    App(Box<Term<A>>, Vec<Term<A>>),
}

impl<A: Clone> Term<A> {
    /// Returns true if this term is an `App` (i.e., partially applied).
    pub fn is_partial_pred(&self) -> bool {
        matches!(self, Term::App(_, _))
    }

    /// If `self` is `App(f, args)`, push `arg` onto args.
    /// Otherwise wrap in a fresh `App(self, [arg])`.
    pub fn apply_partial(self, arg: Term<A>) -> Term<A> {
        match self {
            Term::App(f, mut args) => {
                args.push(arg);
                Term::App(f, args)
            }
            other => Term::App(Box::new(other), vec![arg]),
        }
    }
}

impl<A: std::fmt::Display> std::fmt::Display for Term<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Atom(a) => write!(f, "{}", a),
            Term::Var(s) => write!(f, "{}", s),
            Term::Lambda(s, body) => write!(f, "λ{}.{}", s, body),
            Term::App(func, args) => {
                write!(f, "{}(", func)?;
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{}", a)?;
                }
                write!(f, ")")
            }
        }
    }
}

/// A term paired with its Lambek type.
#[derive(Debug, Clone, PartialEq)]
pub struct AnnotatedTerm<A, T> {
    pub term: Term<A>,
    pub ty: LambekType<T>,
}

/// Non-deterministic result: all possible values of a parse.
pub type NonDet<A> = Vec<A>;
