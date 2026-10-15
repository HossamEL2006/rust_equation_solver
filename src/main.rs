use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    ops,
};

#[derive(Clone, PartialEq)]
struct Unknown {
    name: char,
}

#[derive(Clone, PartialEq)]
enum Expr {
    Number(i32),
    Variable(Unknown),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
}

use Expr::{Add, Mul, Neg, Number, Sub, Variable};

impl ops::Add for Expr {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Add(Box::new(self), Box::new(rhs))
    }
}

impl ops::Sub for Expr {
    type Output = Expr;

    fn sub(self, rhs: Self) -> Self::Output {
        Sub(Box::new(self), Box::new(rhs))
    }
}

impl ops::Mul for Expr {
    type Output = Expr;

    fn mul(self, rhs: Self) -> Self::Output {
        Mul(Box::new(self), Box::new(rhs))
    }
}

impl ops::Neg for Expr {
    type Output = Expr;

    fn neg(self) -> Self::Output {
        Neg(Box::new(self))
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number(n) => write!(f, "{}", n),
            Variable(unknown) => write!(f, "{}", unknown.name),
            Add(exp1, exp2) => write!(f, "({} + {})", exp1, exp2),
            Sub(exp1, exp2) => write!(f, "({} - {})", exp1, exp2),
            Mul(exp1, exp2) => write!(f, "{}*{}", exp1, exp2),
            Neg(exp) => write!(f, "-({})", exp),
        }
    }
}

impl Expr {
    fn init_num(n: i32) -> Expr {
        Number(n)
    }

    fn init_var(name: char) -> Expr {
        Variable(Unknown { name })
    }
}

impl Expr {
    fn as_terms(&self) -> Vec<Term> {
        // distributes the expression into a sum of simple terms
        match self {
            Number(n) => vec![Term {
                scalar: *n,
                unit_term: HashMap::new(),
            }],
            Variable(unknown) => vec![Term {
                scalar: 1,
                unit_term: HashMap::from([(unknown.name, 1)]),
            }],
            Add(exp1, exp2) => [&**exp1, &**exp2]
                .into_iter()
                .flat_map(Self::as_terms)
                .collect(),
            Sub(exp1, exp2) => {
                let mut rhs_terms = exp2.as_terms();
                rhs_terms.iter_mut().for_each(|term| term.scale(-1));
                exp1.as_terms().into_iter().chain(rhs_terms).collect()
            }
            Mul(exp1, exp2) => {
                let terms1 = &exp1.as_terms();
                let terms2 = &exp2.as_terms();
                terms1
                    .iter()
                    .flat_map(|term1| terms2.iter().map(move |term2| term1 * term2))
                    .collect()
                // Rust analyzer suggested adding the `move` keyword
            }
            Neg(exp) => {
                let mut terms = exp.as_terms();
                terms.iter_mut().for_each(|term| term.scale(-1));
                terms
            }
        }
    }

    fn as_simp_terms(&self) -> Vec<Term> {
        cancel_zeros(merge_terms(self.as_terms()))
    }
}

type UnitTerm = HashMap<char, i32>;

#[derive(Clone)]
struct Term {
    scalar: i32,
    unit_term: UnitTerm,
}

impl Term {
    fn scale(&mut self, n: i32) {
        self.scalar *= n
    }

    fn into_scaled(mut self, n: i32) -> Term {
        self.scale(n);
        self
    }
}

impl ops::Mul for &Term {
    type Output = Term;

    fn mul(self, rhs: Self) -> Self::Output {
        Term {
            scalar: self.scalar * rhs.scalar,
            unit_term: self.unit_term.iter().chain(&rhs.unit_term).fold(
                HashMap::new(),
                |mut unit_term: UnitTerm, (key, val)| {
                    unit_term
                        .entry(*key)
                        .and_modify(|counter| *counter = ops::Add::add(*counter, *val))
                        /* used `ops::Add::add` instead of `+` to avoid clippy's
                         * suspicious_arithmetic_impl warning */
                        .or_insert(*val);
                    unit_term
                },
            ), // Source: https://doc.rust-lang.org/std/collections/struct.HashMap.html#method.entry
        }
    }
}

impl ops::Mul for Term {
    type Output = Term;

    fn mul(self, rhs: Self) -> Self::Output {
        &self * &rhs
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut fully_written_scalar = false;
        match self.scalar {
            -1 => write!(f, "-")?,
            1 => (),
            _ => {
                fully_written_scalar = true;
                write!(f, "{}", self.scalar)?
            }
        }

        let mut no_unknowns = true;
        for (key, val) in &self.unit_term {
            if *val == 1 {
                write!(f, "{}", key)?;
                no_unknowns = false;
            } else if *val > 1 {
                write!(f, "{}^{}", key, val)?;
                no_unknowns = false;
            }
        }

        if !fully_written_scalar & no_unknowns {
            write!(f, "1")?;
        }
        Ok(())
    }
}

impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        self.scalar == other.scalar && unit_term_partialeq(&self.unit_term, &other.unit_term)
    }
}

fn unit_term_partialeq(unit_term1: &UnitTerm, unit_term2: &UnitTerm) -> bool {
    let mut verified_keys: HashSet<char> = HashSet::new();
    for key in unit_term1.keys().chain(unit_term2.keys()) {
        if !verified_keys.insert(*key) {
            continue;
        }
        match (unit_term1.get(key), unit_term2.get(key)) {
            (None, Some(n)) | (Some(n), None) if *n != 0 => return false,
            (Some(n), Some(m)) if *n != *m => return false,
            _ => (),
        }
    }
    true
}

fn merge_terms(terms: Vec<Term>) -> Vec<Term> {
    fn unit_term_to_arr(unit_term: &UnitTerm) -> [i32; 256] {
        let mut output = [0; 256];
        unit_term
            .iter()
            .for_each(|(name, pow)| output[*name as u8 as usize] += pow);
        // missing error handling for `char as u8`
        // TODO : Change term.name from `char` to `u8`
        output
    }

    let mut term_indicies_map: HashMap<[i32; 256], usize> = HashMap::new();
    let mut simplified_terms: Vec<Term> = Vec::new();
    let mut counter = 0;

    for term in terms {
        let term_arr: [i32; 256] = unit_term_to_arr(&term.unit_term);
        if let Some(idx) = term_indicies_map.get(&term_arr) {
            simplified_terms[*idx].scalar += term.scalar;
            // error should not occur in theory
        } else {
            term_indicies_map.insert(term_arr, counter);
            counter += 1;

            simplified_terms.push(term);
        }
    }

    simplified_terms
}

fn cancel_zeros(terms: Vec<Term>) -> Vec<Term> {
    let output: Vec<Term> = terms.into_iter().filter(|term| term.scalar != 0).collect();
    if output.is_empty() {
        vec![Term {
            scalar: 0,
            unit_term: HashMap::new(),
        }]
    } else {
        output
    }
}

fn term_to_expr(term: &Term) -> Expr {
    let mut decomp_unit_term: Vec<Expr> = Vec::new();
    for (key, val) in &term.unit_term {
        for _ in 0..*val {
            decomp_unit_term.push(Expr::init_var(*key));
        }
    }

    let l = decomp_unit_term.len();

    let unit_terms_expr = match l {
        0 => Expr::init_num(1),
        1 => decomp_unit_term.into_iter().next().unwrap(),
        _ => {
            let mut decomp_unit_term = decomp_unit_term.into_iter();
            let fst_unit_term = decomp_unit_term.next().unwrap();
            let snd_unit_term = decomp_unit_term.next().unwrap();
            decomp_unit_term.fold(
                Mul(Box::new(fst_unit_term), Box::new(snd_unit_term)),
                |cuml, exp| Mul(Box::new(cuml), Box::new(exp)),
            )
        }
    };

    match (term.scalar, l) {
        (0, _) => Expr::init_num(0),
        (1, _) => unit_terms_expr,
        (-1, _) => Neg(Box::new(unit_terms_expr)),
        (n, 0) => Expr::init_num(n),
        (n, _) => Mul(Box::new(Expr::init_num(n)), Box::new(unit_terms_expr)),
    }
}

fn terms_to_expr(terms: &[Term]) -> Expr {
    // Clippy suggested changing the type of `terms` form `&Vec<Term>` to `&[Term]`
    let l = terms.len();

    match l {
        0 => Expr::init_num(0),
        1 => term_to_expr(terms.iter().next().unwrap()),
        _ => {
            let mut terms_iter = terms.iter();
            let fst_term = terms_iter.next().unwrap();
            let snd_term = terms_iter.next().unwrap();
            terms_iter.fold(
                Add(
                    Box::new(term_to_expr(fst_term)),
                    Box::new(term_to_expr(snd_term)),
                ),
                |cuml, next_term| Add(Box::new(cuml), Box::new(term_to_expr(next_term))),
            )
        }
    }
}

fn isolate_from_zero_sum_terms(terms: Vec<Term>, termmm: &Term) -> Option<Vec<Term>> {
    let terms: Vec<Term> = cancel_zeros(merge_terms(terms));
    let mut other_terms: Vec<Term> = Vec::new();
    let mut term_to_isolate: Option<Term> = None;
    let mut multiplying_factor = 1;

    for term in terms {
        if !unit_term_partialeq(&termmm.unit_term, &term.unit_term) {
            other_terms.push(term);
        } else if term_to_isolate.is_none() {
            if termmm.scalar % term.scalar != 0 {
                return None;
            }
            multiplying_factor = termmm.scalar / term.scalar;
            term_to_isolate = Some(term);
        } else {
            panic!("Unexpected panic: `terms` shouldn't have duplicate unit terms");
        }
    }

    other_terms
        .iter_mut()
        .for_each(|term| term.scale(-multiplying_factor));

    term_to_isolate.map(|_| other_terms)
}

fn print_terms(terms: &Vec<Term>) {
    let mut fst_loop: bool = true;
    for term in terms {
        if fst_loop {
            print!("{}", term);
            fst_loop = false;
            continue;
        }
        print!(" + {}", term);
    }
}

#[derive(Clone)]
struct Equation {
    left: Expr,
    right: Expr,
}

impl Equation {
    fn as_zero_sum_simp_terms(&self) -> Vec<Term> {
        let mut right_terms = self.right.as_terms();
        right_terms.iter_mut().for_each(|term| term.scale(-1));
        let zero_sum_terms = self
            .left
            .as_terms()
            .into_iter()
            .chain(right_terms)
            .collect();
        cancel_zeros(merge_terms(zero_sum_terms))
    }

    fn as_rearranged_simp(&self) -> Equation {
        Equation {
            left: terms_to_expr(&self.as_zero_sum_simp_terms()),
            right: Expr::init_num(0),
        }
    }

    fn as_isolated(&self, unit_term: &UnitTerm) -> Option<Self> {
        let terms: Vec<Term> = self.as_zero_sum_simp_terms();

        // search for the term to islolate
        let mut other_terms: Vec<Term> = Vec::new();
        let mut term_to_isolate: Option<Term> = None;

        for term in terms {
            if !unit_term_partialeq(unit_term, &term.unit_term) {
                other_terms.push(term);
            } else if term_to_isolate.is_none() {
                term_to_isolate = Some(term);
            } else {
                panic!("Unexpected panic: `terms` shouldn't have duplicate unit terms");
            }
        }

        other_terms.iter_mut().for_each(|term| term.scale(-1));

        term_to_isolate.map(|term| Equation {
            left: term_to_expr(&term),
            right: terms_to_expr(&other_terms),
        })
    }
}

impl Display for Equation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.left, self.right)
    }
}

struct Problem(Vec<Equation>);

impl Problem {
    fn as_rearranged(&self) -> Problem {
        Problem(self.0.iter().map(|eq| eq.as_rearranged_simp()).collect())
    }
}

impl Display for Problem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut fst_loop = true;
        for eq in &self.0 {
            if !fst_loop {
                writeln!(f)?;
            } else {
                fst_loop = false;
            }
            write!(f, "{}", eq)?;
        }
        Ok(())
    }
}

fn main() {
    // Nonlinear system of equations :
    // x(y + z) - yz = xy - 3
    // y(x + z) + xz = 2xz + 5
    // (x + 1)(y + 1)(z - 2) + -(xyz) = xz - 2x - 1
    // This system has 3 solutions in C^3, but only one in R^3 that happens to
    // also be in N^3

    println!("Nonlinear system of equations :");

    let num = Expr::init_num;
    let var = Expr::init_var;

    let eq1 = Equation {
        left: var('x') * (var('y') + var('z')) - var('y') * var('z'),
        right: var('x') * var('y') - num(3),
    };

    let eq2 = Equation {
        left: var('y') * (var('x') + var('z')) + var('x') * var('z'),
        right: num(2) * var('x') * var('z') + num(5),
    };

    let eq3 = Equation {
        left: (var('x') + num(1)) * (var('y') + num(1)) * (var('z') - num(2))
            + -(var('x') * var('y') * var('z')),
        right: var('x') * var('z') - num(2) * var('x') - num(1),
    };

    let prob: Problem = Problem(vec![eq1, eq2, eq3]);

    println!("{}", prob);

    println!("\n***\n");

    println!("Simplified terms of each expression :");
    for eq in &prob.0 {
        for expr in [&eq.left, &eq.right] {
            println!("Simplified terms of {} :", expr);
            expr.as_simp_terms()
                .iter()
                .for_each(|term| println!("{}", term));
            println!();
        }
    }

    println!("***\n");

    println!("Gemini-generated edge cases :");

    // Gemini generated edge cases
    let edge_cases: [Expr; 8] = [
        // 1. Full Term Cancellation
        var('x') * var('y') - var('y') * var('x'),
        (var('x') + num(1)) * (var('x') - num(1)) - (var('x') * var('x') - num(1)),
        // 2. Complex Commutativity & Multi-Variable Products
        var('z') * var('x') * var('y') + var('y') * var('x') * var('z'),
        // 3. Unary Minus and Double Negatives
        -(-(var('x') * var('y') + var('z'))),
        -(var('x') + var('y')) * (var('z') - var('w')),
        // 4. Constant Folding & Collection
        (var('x') + num(2) + num(3)) * (var('y') + num(4) - num(1)),
        // 5. Explicit Zero and Identity Coefficients
        num(0) * var('x') * var('y') + num(5),
        num(1) * var('x') * var('y'),
    ];

    for expr in edge_cases {
        println!("Simplified terms of {} :", expr);
        expr.as_simp_terms()
            .iter()
            .for_each(|term| println!("{}", term));
        println!();
    }

    println!("***\n");

    println!("Zero sum terms of equations :");
    for eq in &prob.0 {
        println!("Rearranged form of {} :", eq);
        let zero_sum_terms: Vec<Term> = eq.as_zero_sum_simp_terms();
        print_terms(&zero_sum_terms);
        println!(" = 0\n")
    }

    println!("***\n");

    println!("Rearranging the system of equations :");
    println!("{}", prob.as_rearranged());

    println!("\n***\n");

    println!("Isolating terms :");

    for eq in &prob.0 {
        println!("Isolating terms of {} :", eq);

        let left_side: Expr = eq.as_rearranged_simp().left;

        for term in left_side.as_simp_terms() {
            let isolated_eq: Equation = eq.as_isolated(&term.unit_term).unwrap();
            println!("{}", isolated_eq);
        }
        println!();
    }

    println!("***\n");

    println!("Manually solving the problem :");

    let terms_eq1 = prob.0[0].as_zero_sum_simp_terms();
    print_terms(&terms_eq1);
    println!(" = 0");

    let term_to_isolate = terms_eq1[0].clone().into_scaled(-1);
    let replacement_terms = isolate_from_zero_sum_terms(terms_eq1, &term_to_isolate).unwrap();

    let terms_eq2 = prob.0[1].as_zero_sum_simp_terms();
    print_terms(&terms_eq2);
    println!(" = 0");

    print!("Replacing {} by ", term_to_isolate);
    print_terms(&replacement_terms);
    println!(" in equation 2 :");

    let terms_eq2v2 = terms_eq2
        .into_iter()
        .flat_map(|term| {
            if term == term_to_isolate {
                replacement_terms.clone()
            } else {
                vec![term]
            }
        })
        .collect();

    print_terms(&terms_eq2v2);
    println!(" = 0");

    println!("After simplification :");
    let terms_eq2v3 = cancel_zeros(merge_terms(terms_eq2v2));
    print_terms(&terms_eq2v3);
    println!(" = 0");

    // TODO : Implement string to Equation parsing
    // TODO : Implement prob::solve_for(unknown: char)
}
