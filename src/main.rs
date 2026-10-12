use std::{collections::HashMap, fmt::{Display, write}, ops};

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

#[derive(Debug)]
struct Term {
    scalar: i32,
    unknowns: HashMap<char, i32>,
}

impl Term {
    fn scale(mut self, n: i32) -> Self {
        self.scalar *= n;
        self
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
            Sub(exp1, exp2) => exp1
                .as_terms()
                .into_iter()
                .chain(exp2.as_terms().into_iter().map(|term| term.scale(-1)))
                .collect(),
            Mul(exp1, exp2) => {
                let terms1 = &exp1.as_terms();
                let terms2 = &exp2.as_terms();
                terms1
                    .iter()
                    .flat_map(|term1| terms2.iter().map(move |term2| term1 * term2))
                    .collect()
                // Rust analyzer suggested adding the `move` keyword
            }
            Neg(exp) => exp
                .as_terms()
                .into_iter()
                .map(|term| term.scale(-1))
                .collect(),
        }
    }
}

fn var(name: char) -> Expr {
    Variable(Unknown { name })
}

fn num(n: i32) -> Expr {
    Number(n)
}

struct Equation {
    left: Expr,
    right: Expr,
}

impl Display for Equation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.left, self.right)
    }
}

struct Problem(Vec<Equation>);

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

    for eq in &prob.0 {
        for expr in [&eq.left, &eq.right] {
            println!("Terms of {} :", expr);
            expr.as_terms().iter().for_each(|term| println!("{}", term));
            println!();
        }
    }

    //TODO : Implement prob.solve_for('x');
}
