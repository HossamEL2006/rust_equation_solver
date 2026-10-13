use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    ops,
};

struct Unknown {
    name: char,
}

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

struct Term {
    scalar: i32,
    unknowns: HashMap<char, i32>,
}

impl Term {
    fn scale(&mut self, n: i32) {
        self.scalar *= n
    }
}

impl ops::Mul for &Term {
    type Output = Term;

    fn mul(self, rhs: Self) -> Self::Output {
        Term {
            scalar: self.scalar * rhs.scalar,
            unknowns: self.unknowns.iter().chain(&rhs.unknowns).fold(
                HashMap::new(),
                |mut map, (key, val)| {
                    map.entry(*key)
                        .and_modify(|counter| *counter = ops::Add::add(*counter, *val))
                        /* used `ops::Add::add` instead of `+` to avoid clippy's
                         * suspicious_arithmetic_impl warning */
                        .or_insert(*val);
                    map
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
        for (key, val) in &self.unknowns {
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
        fn term_hashmap_partialeq(map1: &HashMap<char, i32>, map2: &HashMap<char, i32>) -> bool {
            let mut verified_keys: HashSet<char> = HashSet::new();
            for key in map1.keys().chain(map2.keys()) {
                if !verified_keys.insert(*key) {
                    continue;
                }
                match (map1.get(key), map2.get(key)) {
                    (None, Some(n)) | (Some(n), None) if *n != 0 => return false,
                    (Some(n), Some(m)) if *n != *m => return false,
                    _ => (),
                }
            }
            true
        }

        self.scalar == other.scalar && term_hashmap_partialeq(&self.unknowns, &other.unknowns)
    }
}

impl Expr {
    fn as_terms(&self) -> Vec<Term> {
        // distributes the expression into a sum of simple terms
        match self {
            Number(n) => vec![Term {
                scalar: *n,
                unknowns: HashMap::new(),
            }],
            Variable(unknown) => vec![Term {
                scalar: 1,
                unknowns: HashMap::from([(unknown.name, 1)]),
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

fn var(name: char) -> Expr {
    Variable(Unknown { name })
}

fn num(n: i32) -> Expr {
    Number(n)
}

fn merge_terms(terms: Vec<Term>) -> Vec<Term> {
    fn term_hashmap_to_arr(map: &HashMap<char, i32>) -> [i32; 256] {
        let mut output = [0; 256];
        map.iter()
            .for_each(|(name, pow)| output[*name as u8 as usize] += pow);
        // missing error handling for `char as u8`
        // TODO : Change term.name from `char` to `u8`
        output
    }

    let mut term_indicies_map: HashMap<[i32; 256], usize> = HashMap::new();
    let mut simplified_terms: Vec<Term> = Vec::new();
    let mut counter = 0;

    for term in terms {
        let term_arr: [i32; 256] = term_hashmap_to_arr(&term.unknowns);
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
            unknowns: HashMap::new(),
        }]
    } else {
        output
    }
}

fn term_to_expr(term: Term) -> Expr {
    let mut decomp_unknowns: Vec<Expr> = Vec::new();
    for (key, val) in term.unknowns {
        for _ in 0..val {
            decomp_unknowns.push(var(key));
        }
    }

    let l = decomp_unknowns.len();

    let unknowns_expr = match l {
        0 => num(1),
        1 => decomp_unknowns.into_iter().next().unwrap(),
        _ => {
            let mut decomp_unknowns = decomp_unknowns.into_iter();
            let fst_unknown = decomp_unknowns.next().unwrap();
            let snd_unknown = decomp_unknowns.next().unwrap();
            decomp_unknowns.fold(
                Mul(Box::new(fst_unknown), Box::new(snd_unknown)),
                |cuml, exp| Mul(Box::new(cuml), Box::new(exp)),
            )
        }
    };

    match (term.scalar, l) {
        (0, _) => num(0),
        (1, _) => unknowns_expr,
        (-1, _) => Neg(Box::new(unknowns_expr)),
        (n, 0) => num(n),
        (n, _) => Mul(Box::new(num(n)), Box::new(unknowns_expr)),
    }
}

fn terms_to_expr(terms: Vec<Term>) -> Expr {
    let l = terms.len();

    match l {
        0 => num(0),
        1 => term_to_expr(terms.into_iter().next().unwrap()),
        _ => {
            let mut terms_iter = terms.into_iter();
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

struct Equation {
    left: Expr,
    right: Expr,
}

impl Equation {
    fn as_zero_sum_terms(&self) -> Vec<Term> {
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

    fn as_rearranged(&self) -> Equation {
        Equation {
            left: terms_to_expr(self.as_zero_sum_terms()),
            right: num(0),
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
        Problem(self.0.iter().map(|eq| eq.as_rearranged()).collect())
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

    println!("Zero sum terms of equations:");
    for eq in &prob.0 {
        println!("Rearranged form of {} :", eq);
        let zero_sum_terms = eq.as_zero_sum_terms();
        let mut fst_loop = true;
        for term in zero_sum_terms {
            if fst_loop {
                print!("{}", term);
                fst_loop = false;
                continue;
            }
            print!(" + {}", term);
        }
        println!(" = 0\n")
    }

    println!("***\n");

    println!("Rearranging the system of equations:");
    println!("{}", prob.as_rearranged());

    // TODO : Implement string to Equation parsing
    // TODO : Implement prob::solve_for(unknown: char)
}
