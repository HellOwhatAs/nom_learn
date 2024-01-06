use std::collections::HashMap;
use std::error::Error;
use nom::IResult;
use nom::character::complete as c;
use nom::branch::alt;
use nom::sequence::{tuple, delimited};
use nom::bytes::complete::tag;
use nom::multi::many0;
use nom::combinator::{recognize, opt};
use text_io::read;

#[derive(Debug)]
pub enum Expr<'a> {
    BinOp(&'a str, Box<Expr<'a>>, Box<Expr<'a>>),
    UnOp(&'a str, Box<Expr<'a>>),
    Call(&'a str, Option<Box<Expr<'a>>>),
    Ident(&'a str),
    Int(u128)
}

impl<'a> Expr<'a> {
    fn eval(&self, registers: &'a HashMap<&'a str, i128>) -> i128 {
        match self {
            Expr::BinOp(op, l, r) => match op {
                &"+" => l.eval(registers) + r.eval(registers),
                &"-" => l.eval(registers) - r.eval(registers),
                &"*" => l.eval(registers) * r.eval(registers),
                &"/" => l.eval(registers) / r.eval(registers),
                &"%" => l.eval(registers) % r.eval(registers),
                &"^" => l.eval(registers).pow(r.eval(registers).try_into().unwrap()),
                &">" => (l.eval(registers) > r.eval(registers)) as i128,
                &">=" => (l.eval(registers) >= r.eval(registers)) as i128,
                &"<" => (l.eval(registers) < r.eval(registers)) as i128,
                &"<=" => (l.eval(registers) <= r.eval(registers)) as i128,
                &"==" => (l.eval(registers) == r.eval(registers)) as i128,
                &"!=" => (l.eval(registers) != r.eval(registers)) as i128,
                &"&&" => (l.eval(registers) !=0  && r.eval(registers) != 0) as i128,
                &"||" => (l.eval(registers) !=0  || r.eval(registers) != 0) as i128,
                _ => unreachable!()
            },
            Expr::UnOp(op, e) => match op {
                &"+" => e.eval(registers),
                &"-" => - e.eval(registers),
                &"*" => todo!(),
                &"!" => (e.eval(registers) == 0) as i128,
                _ => unreachable!()
            },
            Expr::Call(fname, opt_e) => match (fname, opt_e) {
                (&"write_int", Some(e)) => {
                    let res = e.eval(registers);
                    print!("{}", res);
                    res
                },
                (&"write_char", Some(e)) => {
                    let res = e.eval(registers);
                    print!("{}", res as u8 as char);
                    res
                },
                (&"read_int", None) => {
                    let res: i128 = read!();
                    res
                },
                (&"read_char", None) => {
                    let res: char = read!();
                    res as i128
                },
                _ => unreachable!()
            },
            Expr::Int(i) => (*i).try_into().unwrap(),
            Expr::Ident(x) => *registers.get(x).unwrap_or(&0)
        }
    }
}

pub fn identifier<'a>(s: &'a str) -> IResult<&'a str, &'a str> {
    recognize(tuple((
        alt((tag("_"), c::alpha1)),
        many0(alt((tag("_"), c::alphanumeric1)))
    )))(s)
}

pub fn parse_u128(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, res) = c::u128(input)?;
    Ok((rem, Box::new(Expr::Int(res))))
}

pub fn parse_ident(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, res) = identifier(input)?;
    Ok((rem, Box::new(Expr::Ident(res))))
}

pub fn parse_call(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, res) = tuple((identifier, tag("("), opt(parse_expr), tag(")")))(input)?;
    Ok((rem, Box::new(Expr::Call(res.0, res.2))))
}

pub fn parse_single(input: &str) -> IResult<&str, Box<Expr>> {
    alt((
        parse_u128,
        parse_call,
        parse_ident,
        delimited(tag("("), parse_expr, tag(")")),
    ))(input)
}

pub fn parse_pow(input: &str) -> IResult<&str, Box<Expr>> {
    match tuple((parse_single, tag("^"), parse_pow))(input) {
        Ok((rem, res)) => Ok((rem, Box::new(Expr::BinOp("^", res.0, res.2)))),
        _ => parse_single(input),
    }
}

pub fn parse_higher_unop(input: &str) -> IResult<&str, Box<Expr>> {
    fn higher_unop(input: &str) -> IResult<&str, Box<Expr>> {
        let (rem, res) = tuple((tag("*"), parse_higher_unop))(input)?;
        Ok((rem, Box::new(Expr::UnOp(res.0, res.1))))
    }
    alt((higher_unop, parse_pow))(input)
}

pub fn parse_higher_binop(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, (mut res, res1)) = tuple((parse_higher_unop, many0(tuple((alt((tag("*"), tag("/"), tag("%"))), parse_higher_unop)))))(input)?;
    for (op, e) in res1.into_iter() {
        res = Box::new(Expr::BinOp(op, res, e));
    }
    Ok((rem, res))
}

pub fn parse_lower_unop(input: &str) -> IResult<&str, Box<Expr>> {
    fn lower_unop(input: &str) -> IResult<&str, Box<Expr>> {
        let (rem, res) = tuple((alt((tag("+"), tag("-"))), parse_lower_unop))(input)?;
        Ok((rem, Box::new(Expr::UnOp(res.0, res.1))))
    }
    alt((lower_unop, parse_higher_binop))(input)
}

pub fn parse_lower_binop(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, (mut res, res1)) = tuple((parse_lower_unop, many0(tuple((alt((tag("+"), tag("-"))), parse_lower_unop)))))(input)?;
    for (op, e) in res1.into_iter() {
        res = Box::new(Expr::BinOp(op, res, e));
    }
    Ok((rem, res))
}

pub fn parse_cmp_binop(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, (mut res, res1)) = tuple((parse_lower_binop, many0(tuple((alt((tag(">"), tag(">="), tag("<"), tag("<="), tag("=="), tag("!="))), parse_lower_binop)))))(input)?;
    for (op, e) in res1.into_iter() {
        res = Box::new(Expr::BinOp(op, res, e));
    }
    Ok((rem, res))
}

pub fn parse_not_unop(input: &str) -> IResult<&str, Box<Expr>> {
    fn not_unop(input: &str) -> IResult<&str, Box<Expr>> {
        let (rem, res) = tuple((tag("!"), parse_not_unop))(input)?;
        Ok((rem, Box::new(Expr::UnOp(res.0, res.1))))
    }
    alt((not_unop, parse_cmp_binop))(input)
}

pub fn parse_and_binop(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, (mut res, res1)) = tuple((parse_not_unop, many0(tuple((tag("&&"), parse_not_unop)))))(input)?;
    for (op, e) in res1.into_iter() {
        res = Box::new(Expr::BinOp(op, res, e));
    }
    Ok((rem, res))
}

pub fn parse_or_binop(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, (mut res, res1)) = tuple((parse_and_binop, many0(tuple((tag("||"), parse_and_binop)))))(input)?;
    for (op, e) in res1.into_iter() {
        res = Box::new(Expr::BinOp(op, res, e));
    }
    Ok((rem, res))
}

pub fn parse_expr(input: &str) -> IResult<&str, Box<Expr>> {
    parse_or_binop(input)
}

fn main() -> Result<(), Box<dyn Error>> {
    let (remaining_input, output) = parse_expr("write_int(read_int()*k)+write_char(10)-10")?;
    let mut registers = HashMap::new();
    registers.insert("k", 3000);
    println!("{:?} {:?}", remaining_input, output);
    println!("{}", output.eval(&registers));
    Ok(())
}