use num::integer::lcm;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    marker::PhantomData,
    ops, vec,
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
    fn as_terms(&self) -> TermSum<ArbitrarySum, UnsimplifiedSum> {
        // distributes the expression into a sum of simple terms
        let terms: Vec<Term> = match self {
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
                .flat_map(|expr| Self::as_terms(expr).0)
                .collect(),
            Sub(exp1, exp2) => {
                let mut rhs_terms: Vec<Term> = exp2.as_terms().0;
                rhs_terms
                    .iter_mut()
                    .for_each(|term: &mut Term| term.scale(-1));
                exp1.as_terms().0.into_iter().chain(rhs_terms).collect()
            }
            Mul(exp1, exp2) => {
                let terms1: &Vec<Term> = &exp1.as_terms().0;
                let terms2: &Vec<Term> = &exp2.as_terms().0;
                terms1
                    .iter()
                    .flat_map(|term1: &Term| terms2.iter().map(move |term2: &Term| term1 * term2))
                    .collect()
                // Rust analyzer suggested adding the `move` keyword
            }
            Neg(exp) => {
                let mut terms: Vec<Term> = exp.as_terms().0;
                terms.iter_mut().for_each(|term: &mut Term| term.scale(-1));
                terms
            }
        };
        TermSum(terms, PhantomData)
    }

    fn as_simp_terms(&self) -> TermSum<ArbitrarySum, SimplifiedSum> {
        self.as_terms().into_simplified()
    }
}

#[derive(Clone)]
struct UnitTerm(HashMap<char, i32>);

impl Display for UnitTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (key, val) in &self.0 {
            if *val == 1 {
                write!(f, "{}", key)?;
            } else if *val > 1 {
                write!(f, "{}^{}", key, val)?;
            }
        }
        Ok(())
    }
}

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

        write!(f, "{}", self.unit_term)?;

        let mut no_unknowns: bool = true;
        for val in self.unit_term.0.values() {
            if *val > 0 {
                no_unknowns = false;
                break;
            } else if *val < 0 {
                panic!("Unexpected panic: UnitTerm has negative power");
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

struct ArbitrarySum;
struct ZeroSum;
struct SimplifiedSum;
struct UnsimplifiedSum;

struct TermSum<SumState, SimpState>(Vec<Term>, PhantomData<(SumState, SimpState)>);

impl<SumState> TermSum<SumState, UnsimplifiedSum> {
    fn from(terms: Vec<Term>) -> TermSum<SumState, UnsimplifiedSum> {
        TermSum(terms, PhantomData)
    }
}

impl<SumState, SimpState> TermSum<SumState, SimpState> {
    fn fmt_terms(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut fst_loop: bool = true;
        for term in &self.0 {
            if fst_loop {
                write!(f, "{}", term)?;
                fst_loop = false;
                continue;
            }
            write!(f, " + {}", term)?;
        }
        Ok(())
    }
}

impl<SimpState> Display for TermSum<ArbitrarySum, SimpState> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Self::fmt_terms(self, f)?;
        Ok(())
    }
}

impl<SimpState> Display for TermSum<ZeroSum, SimpState> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Self::fmt_terms(self, f)?;
        write!(f, " = 0")?;
        Ok(())
    }
}

impl TermSum<ZeroSum, SimplifiedSum> {
    // TODO : Maybe consider making it an into_isolated
    fn as_isolated(&self, term_to_isolate: &Term) -> Option<TermSum<ArbitrarySum, SimplifiedSum>> {
        let mut other_terms: Vec<Term> = Vec::new();
        let mut isolated_term: Option<Term> = None;
        let mut multiplying_factor: i32 = 1;

        for term in &self.0 {
            if term_to_isolate.unit_term != term.unit_term {
                other_terms.push(term.clone());
            } else if isolated_term.is_none() {
                if term_to_isolate.scalar % term.scalar != 0 {
                    return None;
                }
                multiplying_factor = term_to_isolate.scalar / term.scalar;
                isolated_term = Some(term.clone());
            } else {
                panic!("Unexpected panic: TermSum({}) should be simplified", self);
            }
        }

        other_terms
            .iter_mut()
            .for_each(|term: &mut Term| term.scale(-multiplying_factor));

        isolated_term.map(|_| TermSum(other_terms, PhantomData))
    }
}

impl<SumState> TermSum<SumState, UnsimplifiedSum> {
    fn into_simplified(self) -> TermSum<SumState, SimplifiedSum> {
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

        TermSum(cancel_zeros(merge_terms(self.0)), PhantomData)
    }
}

impl<SumState, SimpState> TermSum<SumState, SimpState> {
    fn get_unit_term_coef(&self, unit_term: &UnitTerm) -> Option<i32> {
        let mut output: Option<i32> = None;

        for term in &self.0 {
            if &term.unit_term == unit_term {
                if output.is_some() {
                    panic!("Non simplified list of terms");
                }
                output = Some(term.scalar)
            }
        }

        output
    }
}

#[derive(Clone)]
struct Equation {
    left: Expr,
    right: Expr,
}

impl Equation {
    fn as_zero_sum_simp_terms(&self) -> TermSum<ZeroSum, SimplifiedSum> {
        let mut right_terms: Vec<Term> = self.right.as_terms().0;
        right_terms
            .iter_mut()
            .for_each(|term: &mut Term| term.scale(-1));
        let zero_sum_terms: Vec<Term> = self
            .left
            .as_terms()
            .0
            .into_iter()
            .chain(right_terms)
            .collect();
        TermSum::from(zero_sum_terms).into_simplified()
    }
}

impl Display for Equation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.left, self.right)
    }
}

struct Problem(Vec<Equation>);

impl Problem {
    fn as_zero_sums(&self) -> Vec<TermSum<ZeroSum, SimplifiedSum>> {
        self.0
            .iter()
            .map(Equation::as_zero_sum_simp_terms)
            .collect()
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

    println!("# Nonlinear system of equations :");

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
    // TODO : Wrap all printed math expressions in a LaTeX block $...$

    println!("\n***\n");

    println!("# Simplified terms of each expression :");
    for eq in &prob.0 {
        for expr in [&eq.left, &eq.right] {
            println!("- Simplified terms of {} :", expr);
            println!("\t{}", expr.as_simp_terms());
        }
    }

    println!("\n***\n");

    println!("# Gemini-generated edge cases :");

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
        println!("- Simplified terms of {} :", expr);
        println!("\t{}", expr.as_simp_terms());
    }

    println!("\n***\n");

    println!("# Zero sum equations :");
    for eq in &prob.0 {
        println!("- Rearranged form of {} :", eq);
        let zero_sum_terms: TermSum<ZeroSum, SimplifiedSum> = eq.as_zero_sum_simp_terms();
        println!("\t{}", zero_sum_terms);
    }

    println!("\n***\n");

    println!("# Isolating terms :");

    for eq in &prob.0 {
        println!("## Isolating terms of {} :", eq);

        let zero_sum_terms: TermSum<ZeroSum, SimplifiedSum> = eq.as_zero_sum_simp_terms();

        for term_to_isolate in &zero_sum_terms.0 {
            let rhs_terms: TermSum<ArbitrarySum, SimplifiedSum> =
                zero_sum_terms.as_isolated(term_to_isolate).unwrap();
            print!("- {} = ", term_to_isolate);
            println!("{}", rhs_terms);
        }
    }

    println!("\n***\n");

    println!("# Manually solving the problem :");

    println!("## Initial problem :");

    let mut zero_sum_equations: Vec<TermSum<ZeroSum, SimplifiedSum>> = prob.as_zero_sums();

    zero_sum_equations
        .iter()
        .for_each(|zero_sum: &TermSum<ZeroSum, SimplifiedSum>| println!("- {}", zero_sum));

    // TODO : Implement UnitTerm::init(&str)
    let unit_term_to_replace: UnitTerm = zero_sum_equations[0].0[0].unit_term.clone();
    let idx_from_eq: usize = 0; // from equation : the equation to replace from
    let idx_into_eq: usize = 1; // into equation : the equation to replace into

    let coef_from_eq: i32 = zero_sum_equations[idx_from_eq]
        .get_unit_term_coef(&unit_term_to_replace)
        .unwrap();
    let coef_into_eq: i32 = zero_sum_equations[idx_into_eq]
        .get_unit_term_coef(&unit_term_to_replace)
        .unwrap();
    let coef_lcm: i32 = lcm(coef_from_eq, coef_into_eq);

    println!("## Simplification choice :");

    println!("- Unit term to simplify : {}", unit_term_to_replace);

    println!(
        "- LCM of coefficients of the {} term : {}",
        unit_term_to_replace, coef_lcm
    );

    println!("## Scaled equations :");

    if coef_lcm != coef_from_eq {
        zero_sum_equations[idx_from_eq]
            .0
            .iter_mut()
            .for_each(|term: &mut Term| term.scale(coef_lcm / coef_from_eq));
    }

    if coef_lcm != coef_into_eq {
        zero_sum_equations[idx_into_eq]
            .0
            .iter_mut()
            .for_each(|term: &mut Term| term.scale(coef_lcm / coef_into_eq));
    }

    zero_sum_equations
        .iter()
        .for_each(|zero_sum: &TermSum<ZeroSum, SimplifiedSum>| println!("- {}", zero_sum));

    println!("## Equation replacement :");

    // changing `replacement_terms` into a function avoided a clone during term replacement
    let get_replacement_terms = || {
        zero_sum_equations[idx_from_eq]
            .as_isolated(&Term {
                scalar: coef_lcm,
                unit_term: unit_term_to_replace.clone(),
            })
            .unwrap()
            .0
    };

    zero_sum_equations[idx_into_eq] = TermSum(
        zero_sum_equations[idx_into_eq]
            .0
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
        PhantomData,
    )
    .into_simplified();

    zero_sum_equations
        .iter()
        .for_each(|zero_sum: &TermSum<ZeroSum, SimplifiedSum>| println!("- {}", zero_sum));

    // TODO : Implement string to Equation parsing
    // TODO : Implement prob::solve_for(unknown: char)
}
