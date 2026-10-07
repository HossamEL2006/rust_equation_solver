use std::{fmt::Display, ops};

struct Unknown {
    name: char,
}

enum Expression {
    Number(i32),
    Variable(Unknown),
    Add(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Neg(Box<Expression>),
}

use Expression::{Add, Mul, Neg, Number, Sub, Variable};

impl ops::Add for Expression {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Add(Box::new(self), Box::new(rhs))
    }
}

impl ops::Sub for Expression {
    type Output = Expression;

    fn sub(self, rhs: Self) -> Self::Output {
        Sub(Box::new(self), Box::new(rhs))
    }
}

impl ops::Mul for Expression {
    type Output = Expression;

    fn mul(self, rhs: Self) -> Self::Output {
        Mul(Box::new(self), Box::new(rhs))
    }
}

impl ops::Neg for Expression {
    type Output = Expression;

    fn neg(self) -> Self::Output {
        Neg(Box::new(self))
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number(n) => write!(f, "{}", n),
            Variable(unknown) => write!(f, "{}", unknown.name),
            Add(exp1, exp2) => write!(f, "({} + {})", exp1, exp2),
            Mul(exp1, exp2) => write!(f, "{}*{}", exp1, exp2),
            Sub(exp1, exp2) => write!(f, "({} - {})", exp1, exp2),
            Neg(exp) => write!(f, "-({})", exp),
        }
    }
}

fn var(name: char) -> Expression {
    Variable(Unknown { name })
}

fn num(n: i32) -> Expression {
    Number(n)
}

struct Equation {
    left: Expression,
    right: Expression,
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
    //TODO : Implement prob.solve_for('x');
}
