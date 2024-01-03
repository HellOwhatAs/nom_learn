use nom::IResult;
use std::error::Error;
use nom::character::complete as c;
use nom::branch::alt;
use nom::sequence::{tuple, delimited};
use nom::bytes::complete::tag;
use nom::multi::many0;

#[derive(Debug)]
pub enum Expr<'a> {
    BinOp(&'a str, Box<Expr<'a>>, Box<Expr<'a>>),
    Int(i64)
}

impl<'a> Expr<'a> {
    fn eval(&self) -> i64 {
        match self {
            Expr::BinOp(op, l, r) => match op {
                &"+" => l.eval() + r.eval(),
                &"-" => l.eval() - r.eval(),
                &"*" => l.eval() * r.eval(),
                &"/" => l.eval() / r.eval(),
                &"^" => l.eval().pow(r.eval().try_into().unwrap()),
                _ => unreachable!()
            },
            Expr::Int(i) => *i,
        }
    }
}

pub fn parse_i64(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, res) = c::i64(input)?;
    Ok((rem, Box::new(Expr::Int(res))))
}

pub fn parse_single(input: &str) -> IResult<&str, Box<Expr>> {
    alt((
        parse_i64,
        delimited(tag("("), parse_expr, tag(")")),
    ))(input)
}

pub fn parse_pow(input: &str) -> IResult<&str, Box<Expr>> {
    match tuple((parse_single, tag("^"), parse_pow))(input) {
        Ok((rem, res)) => Ok((rem, Box::new(Expr::BinOp("^", res.0, res.2)))),
        _ => {
            parse_single(input)
        },
    }
}

pub fn parse_higher_binop(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, (mut res, res1)) = tuple((parse_pow, many0(tuple((alt((tag("*"), tag("/"))), parse_pow)))))(input)?;
    for (op, e) in res1.into_iter() {
        res = Box::new(Expr::BinOp(op, res, e));
    }
    Ok((rem, res))
}

pub fn parse_lower_binop(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, (mut res, res1)) = tuple((parse_higher_binop, many0(tuple((alt((tag("+"), tag("-"))), parse_higher_binop)))))(input)?;
    for (op, e) in res1.into_iter() {
        res = Box::new(Expr::BinOp(op, res, e));
    }
    Ok((rem, res))
}

pub fn parse_expr(input: &str) -> IResult<&str, Box<Expr>> {
    parse_lower_binop(input)
}

fn main() -> Result<(), Box<dyn Error>> {
    let (remaining_input, output) = parse_expr("1000000/(1+2*3^4+5-7*5474/9110)")?;
    println!("{:?} {:?}", remaining_input, output);
    println!("{}", output.eval());
  Ok(())
}