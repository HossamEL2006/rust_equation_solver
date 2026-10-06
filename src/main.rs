use std::ops::{Add, Mul, Neg, Sub};

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

impl Add for Expression {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Expression::Add(Box::new(self), Box::new(rhs))
    }
}

impl Sub for Expression {
    type Output = Expression;

    fn sub(self, rhs: Self) -> Self::Output {
        Expression::Sub(Box::new(self), Box::new(rhs))
    }
}

impl Mul for Expression {
    type Output = Expression;

    fn mul(self, rhs: Self) -> Self::Output {
        Expression::Mul(Box::new(self), Box::new(rhs))
    }
}

impl Neg for Expression {
    type Output = Expression;

    fn neg(self) -> Self::Output {
        Expression::Neg(Box::new(self))
    }
}

fn var(name: char) -> Expression {
    Expression::Variable(Unknown { name })
}

fn num(n: i32) -> Expression {
    Expression::Number(n)
}

struct Equation {
    left: Expression,
    right: Expression,
}

type Problem = Vec<Equation>;

fn main() {
    // Nonlinear system of equations :
    // x(y + z) - yz = xy - 3
    // y(x + z) + xz = 2xz + 5
    // (x + 1)(y + 1)(z - 2) + (-xyz) = xz - 2x - 1
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

    let prob: Problem = vec![eq1, eq2, eq3];
    //TODO : Implement prob.solve_for('x');
}
