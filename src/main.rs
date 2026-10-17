use num::integer::lcm;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    ops, slice, vec,
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
                unit_term: UnitTerm(HashMap::new()),
            }],
            Variable(unknown) => vec![Term {
                scalar: 1,
                unit_term: UnitTerm(HashMap::from([(unknown.name, 1)])),
            }],
            Add(exp1, exp2) => [&**exp1, &**exp2]
                .into_iter()
                .flat_map(Self::as_terms)
                .collect(),
            Sub(exp1, exp2) => {
                let mut rhs_terms: Vec<Term> = exp2.as_terms();
                rhs_terms
                    .iter_mut()
                    .for_each(|term: &mut Term| term.scale(-1));
                exp1.as_terms().into_iter().chain(rhs_terms).collect()
            }
            Mul(exp1, exp2) => {
                let terms1: &Vec<Term> = &exp1.as_terms();
                let terms2: &Vec<Term> = &exp2.as_terms();
                terms1
                    .iter()
                    .flat_map(|term1: &Term| terms2.iter().map(move |term2: &Term| term1 * term2))
                    .collect()
                // Rust analyzer suggested adding the `move` keyword
            }
            Neg(exp) => {
                let mut terms: Vec<Term> = exp.as_terms();
                terms.iter_mut().for_each(|term: &mut Term| term.scale(-1));
                terms
            }
        }
    }

    fn as_simp_terms(&self) -> Vec<Term> {
        cancel_zeros(merge_terms(self.as_terms()))
    }
}

#[derive(Clone)]
struct UnitTerm(HashMap<char, i32>);

impl PartialEq for UnitTerm {
    fn eq(&self, other: &Self) -> bool {
        let mut verified_keys: HashSet<char> = HashSet::new();
        for key in self.0.keys().chain(other.0.keys()) {
            if !verified_keys.insert(*key) {
                continue;
            }
            match (self.0.get(key), other.0.get(key)) {
                (None, Some(n)) | (Some(n), None) if *n != 0 => return false,
                (Some(n), Some(m)) if *n != *m => return false,
                _ => (),
            }
        }
        true
    }
}

impl ops::Mul for &UnitTerm {
    type Output = UnitTerm;

    fn mul(self, rhs: Self) -> Self::Output {
        UnitTerm(
            self.0.iter().chain(&rhs.0).fold(
                HashMap::new(),
                |mut unit_term, (key, val): (&char, &i32)| {
                    unit_term
                        .entry(*key)
                        .and_modify(|counter: &mut i32| *counter = ops::Add::add(*counter, *val))
                        /* used `ops::Add::add` instead of `+` to avoid clippy's
                         * suspicious_arithmetic_impl warning */
                        .or_insert(*val);
                    unit_term
                },
            ), // Source: https://doc.rust-lang.org/std/collections/struct.HashMap.html#method.entry
        )
    }
}

impl UnitTerm {
    fn as_power_array(&self) -> [i32; 256] {
        let mut output: [i32; 256] = [0; 256];
        self.0
            .iter()
            .for_each(|(name, pow)| output[*name as u8 as usize] += pow);
        // missing error handling for `char as u8`
        // TODO : Change term.name from `char` to `u8`
        output
    }
}

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
            unit_term: &self.unit_term * &rhs.unit_term,
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
        let mut fully_written_scalar: bool = false;
        match self.scalar {
            -1 => write!(f, "-")?,
            1 => (),
            _ => {
                fully_written_scalar = true;
                write!(f, "{}", self.scalar)?
            }
        }

        let mut no_unknowns: bool = true;
        for (key, val) in &self.unit_term.0 {
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
        self.scalar == other.scalar && self.unit_term == other.unit_term
    }
}

fn merge_terms(terms: Vec<Term>) -> Vec<Term> {
    let mut term_indicies_map: HashMap<[i32; 256], usize> = HashMap::new();
    let mut simplified_terms: Vec<Term> = Vec::new();
    let mut counter: usize = 0;

    for term in terms {
        let term_arr: [i32; 256] = term.unit_term.as_power_array();
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
    let output: Vec<Term> = terms
        .into_iter()
        .filter(|term: &Term| term.scalar != 0)
        .collect();
    if output.is_empty() {
        vec![Term {
            scalar: 0,
            unit_term: UnitTerm(HashMap::new()),
        }]
    } else {
        output
    }
}

fn term_to_expr(term: &Term) -> Expr {
    let mut decomp_unit_term: Vec<Expr> = Vec::new();
    for (key, val) in &term.unit_term.0 {
        for _ in 0..*val {
            decomp_unit_term.push(Expr::init_var(*key));
        }
    }

    let l: usize = decomp_unit_term.len();

    let unit_terms_expr: Expr = match l {
        0 => Expr::init_num(1),
        1 => decomp_unit_term.into_iter().next().unwrap(),
        _ => {
            let mut decomp_unit_term: vec::IntoIter<Expr> = decomp_unit_term.into_iter();
            let fst_unit_term: Expr = decomp_unit_term.next().unwrap();
            let snd_unit_term: Expr = decomp_unit_term.next().unwrap();
            decomp_unit_term.fold(
                Mul(Box::new(fst_unit_term), Box::new(snd_unit_term)),
                |cuml: Expr, exp: Expr| Mul(Box::new(cuml), Box::new(exp)),
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
    match terms.len() {
        0 => Expr::init_num(0),
        1 => term_to_expr(terms.iter().next().unwrap()),
        _ => {
            let mut terms_iter: slice::Iter<'_, Term> = terms.iter();
            let fst_term: &Term = terms_iter.next().unwrap();
            let snd_term: &Term = terms_iter.next().unwrap();
            terms_iter.fold(
                Add(
                    Box::new(term_to_expr(fst_term)),
                    Box::new(term_to_expr(snd_term)),
                ),
                |cuml: Expr, next_term: &Term| {
                    Add(Box::new(cuml), Box::new(term_to_expr(next_term)))
                },
            )
        }
    }
}

// TODO : Refactor into ZeroSumTerms::as_isolated
// Must have a `simplified` field to check if the terms are simplified
fn as_isolated_from_zero_sum_terms(
    terms: &Vec<Term>,
    term_sought_to_isolate: &Term,
) -> Option<Vec<Term>> {
    // let terms: Vec<Term> = cancel_zeros(merge_terms(terms));
    let mut other_terms: Vec<Term> = Vec::new();
    let mut term_to_isolate: Option<Term> = None;
    let mut multiplying_factor: i32 = 1;

    for term in terms {
        if term_sought_to_isolate.unit_term != term.unit_term {
            other_terms.push(term.clone());
        } else if term_to_isolate.is_none() {
            if term_sought_to_isolate.scalar % term.scalar != 0 {
                return None;
            }
            multiplying_factor = term_sought_to_isolate.scalar / term.scalar;
            term_to_isolate = Some(term.clone());
        } else {
            panic!("Unexpected panic: `terms` shouldn't have duplicate unit terms");
        }
    }

    other_terms
        .iter_mut()
        .for_each(|term: &mut Term| term.scale(-multiplying_factor));

    term_to_isolate.map(|_| other_terms)
}

fn get_unit_term_coef(unit_term: &UnitTerm, terms: &Vec<Term>) -> Option<i32> {
    let mut output: Option<i32> = None;

    for term in terms {
        if &term.unit_term == unit_term {
            if output.is_some() {
                panic!("Non simplified list of terms");
            }
            output = Some(term.scalar)
        }
    }

    output
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
        let mut right_terms: Vec<Term> = self.right.as_terms();
        right_terms
            .iter_mut()
            .for_each(|term: &mut Term| term.scale(-1));
        let zero_sum_terms: Vec<Term> = self
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
}

impl Display for Equation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.left, self.right)
    }
}

struct Problem(Vec<Equation>);

impl Problem {
    fn as_rearranged(&self) -> Problem {
        Problem(
            self.0
                .iter()
                .map(|eq: &Equation| eq.as_rearranged_simp())
                .collect(),
        )
    }
}

impl Display for Problem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut fst_loop: bool = true;
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

    let num: fn(i32) -> Expr = Expr::init_num;
    let var: fn(char) -> Expr = Expr::init_var;

    let eq1: Equation = Equation {
        left: var('x') * (var('y') + var('z')) - var('y') * var('z'),
        right: var('x') * var('y') - num(3),
    };

    let eq2: Equation = Equation {
        left: var('y') * (var('x') + var('z')) + var('x') * var('z'),
        right: num(2) * var('x') * var('z') + num(5),
    };

    let eq3: Equation = Equation {
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
            print_terms(&expr.as_simp_terms());
            println!();
        }
    }

    println!("\n***\n");

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
        print_terms(&expr.as_simp_terms());
        println!();
    }

    println!("\n***\n");

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

        let zero_sum_terms: Vec<Term> = eq.as_zero_sum_simp_terms();

        for term_to_isolate in &zero_sum_terms {
            let rhs_terms: Vec<Term> =
                as_isolated_from_zero_sum_terms(&zero_sum_terms, term_to_isolate).unwrap();
            print!("{} = ", term_to_isolate);
            print_terms(&rhs_terms);
            println!();
        }
        println!();
    }

    println!("***\n");

    println!("Manually solving the problem :");

    println!("Initial problem :");

    let mut zero_sum_equations: Vec<Vec<Term>> = prob
        .0
        .iter()
        .map(Equation::as_zero_sum_simp_terms)
        .collect();

    zero_sum_equations.iter().for_each(|terms: &Vec<Term>| {
        print_terms(terms);
        println!(" = 0")
    });

    // TODO : Implement UnitTerm::init(&str)
    let unit_term_to_replace: UnitTerm = zero_sum_equations[0][0].unit_term.clone();
    let idx_from_eq: usize = 0; // from equation : the equation to replace from
    let idx_into_eq: usize = 1; // into equation : the equation to replace into

    let coef_from_eq: i32 =
        get_unit_term_coef(&unit_term_to_replace, &zero_sum_equations[idx_from_eq]).unwrap();
    let coef_into_eq: i32 =
        get_unit_term_coef(&unit_term_to_replace, &zero_sum_equations[idx_into_eq]).unwrap();
    let coef_lcm: i32 = lcm(coef_from_eq, coef_into_eq);
    println!(
        "LCM of coefficients of the {} term : {}",
        Term {
            scalar: 1,
            unit_term: unit_term_to_replace.clone(),
            // Unnecessary clone above
            // TODO : Implement Display for UnitTerm to avoid this
        },
        coef_lcm
    );

    println!("Scaled equations :");

    if coef_lcm != coef_from_eq {
        zero_sum_equations[idx_from_eq]
            .iter_mut()
            .for_each(|term: &mut Term| term.scale(coef_lcm / coef_from_eq));
    }

    if coef_lcm != coef_into_eq {
        zero_sum_equations[idx_into_eq]
            .iter_mut()
            .for_each(|term: &mut Term| term.scale(coef_lcm / coef_into_eq));
    }

    zero_sum_equations.iter().for_each(|terms: &Vec<Term>| {
        print_terms(terms);
        println!(" = 0")
    });

    // changing `replacement_terms` into a function avoided a clone during term replacement
    let get_replacement_terms = || {
        as_isolated_from_zero_sum_terms(
            &zero_sum_equations[idx_from_eq],
            &Term {
                scalar: coef_lcm,
                unit_term: unit_term_to_replace.clone(),
                // Unnecessary clone above
                // TODO : Optimise somehow
            },
        )
        .unwrap()
    };

    zero_sum_equations[idx_into_eq] = cancel_zeros(merge_terms(
        zero_sum_equations[idx_into_eq]
            .clone()
            //TODO : Implement this replacement instruction into a method as the following
            //       ZeroSumTerms::replace_term(&mut ZeroSumTerms, &Term, &Vec<Term>)
            //       This will avoid the clone above
            .into_iter()
            .flat_map(|term: Term| {
                if term.unit_term == unit_term_to_replace {
                    get_replacement_terms()
                } else {
                    vec![term]
                }
            })
            .collect(),
    ));

    println!("Equations after replacement :");

    zero_sum_equations.iter().for_each(|terms: &Vec<Term>| {
        print_terms(terms);
        println!(" = 0")
    });

    // TODO : Implement string to Equation parsing
    // TODO : Implement prob::solve_for(unknown: char)
}
