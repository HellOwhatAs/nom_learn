mod mem;

use mem::Mem;
use std::collections::HashMap;
use std::error::Error;
use nom::IResult;
use nom::character::complete as c;
use nom::branch::alt;
use nom::sequence::{tuple, delimited, preceded, terminated};
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
    Int(i128)
}

impl<'a> Expr<'a> {
    fn eval(&self, registers: &'a HashMap<&'a str, i128>, mem: &mut Mem<i128>) -> i128 {
        match self {
            Expr::BinOp(op, l, r) => match op {
                &"+" => l.eval(registers, mem) + r.eval(registers, mem),
                &"-" => l.eval(registers, mem) - r.eval(registers, mem),
                &"*" => l.eval(registers, mem) * r.eval(registers, mem),
                &"/" => l.eval(registers, mem) / r.eval(registers, mem),
                &"%" => l.eval(registers, mem) % r.eval(registers, mem),
                &"^" => l.eval(registers, mem).pow(r.eval(registers, mem) as u32),
                &">" => (l.eval(registers, mem) > r.eval(registers, mem)) as i128,
                &">=" => (l.eval(registers, mem) >= r.eval(registers, mem)) as i128,
                &"<" => (l.eval(registers, mem) < r.eval(registers, mem)) as i128,
                &"<=" => (l.eval(registers, mem) <= r.eval(registers, mem)) as i128,
                &"==" => (l.eval(registers, mem) == r.eval(registers, mem)) as i128,
                &"!=" => (l.eval(registers, mem) != r.eval(registers, mem)) as i128,
                &"&&" => (l.eval(registers, mem) !=0  && r.eval(registers, mem) != 0) as i128,
                &"||" => (l.eval(registers, mem) !=0  || r.eval(registers, mem) != 0) as i128,
                _ => unreachable!()
            },
            Expr::UnOp(op, e) => match op {
                &"+" => e.eval(registers, mem),
                &"-" => - e.eval(registers, mem),
                &"*" => {
                    let start = e.eval(registers, mem);
                    match mem.mem.get(start as usize) {
                        Some(Some(res)) => *res,
                        _ => panic!("visiting invalid memory")
                    }
                },
                &"!" => (e.eval(registers, mem) == 0) as i128,
                _ => unreachable!()
            },
            Expr::Call(fname, opt_e) => match (fname, opt_e) {
                (&"write_int", Some(e)) => {
                    let res = e.eval(registers, mem);
                    print!("{}", res);
                    res
                },
                (&"write_char", Some(e)) => {
                    let res = e.eval(registers, mem);
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
                (&"malloc", Some(e)) => {
                    let size = e.eval(registers, mem) as usize;
                    mem.malloc(size, 0) as i128
                },
                (&"free", Some(e)) => {
                    let start = e.eval(registers, mem) as usize;
                    mem.free(start) as i128
                },
                otherwise => panic!("invalid function call: {:?}", otherwise)
            },
            Expr::Int(i) => (*i) as i128,
            Expr::Ident(x) => *registers.get(x).expect(&format!("undefined variable {x}"))
        }
    }
}

pub fn identifier<'a>(s: &'a str) -> IResult<&'a str, &'a str> {
    recognize(tuple((
        alt((tag("_"), c::alpha1)),
        many0(alt((tag("_"), c::alphanumeric1)))
    )))(s)
}

pub fn parse_uint(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, res) = c::u128(input)?;
    Ok((rem, Box::new(Expr::Int(res as i128))))
}

pub fn parse_ident(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, res) = identifier(input)?;
    Ok((rem, Box::new(Expr::Ident(res))))
}

pub fn parse_call(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, res) = tuple((identifier, delimited(c::space0, tag("("), c::space0), opt(parse_expr), preceded(c::space0, tag(")"))))(input)?;
    Ok((rem, Box::new(Expr::Call(res.0, res.2))))
}

pub fn parse_single(input: &str) -> IResult<&str, Box<Expr>> {
    alt((
        parse_uint,
        parse_call,
        parse_ident,
        delimited(terminated(tag("("), c::space0), parse_expr, preceded(c::space0, tag(")"))),
    ))(input)
}

pub fn parse_pow(input: &str) -> IResult<&str, Box<Expr>> {
    match tuple((parse_single, delimited(c::space0, tag("^"), c::space0), parse_pow))(input) {
        Ok((rem, res)) => Ok((rem, Box::new(Expr::BinOp("^", res.0, res.2)))),
        _ => parse_single(input),
    }
}

pub fn parse_higher_unop(input: &str) -> IResult<&str, Box<Expr>> {
    fn higher_unop(input: &str) -> IResult<&str, Box<Expr>> {
        let (rem, res) = tuple((terminated(tag("*"), c::space0), parse_higher_unop))(input)?;
        Ok((rem, Box::new(Expr::UnOp(res.0, res.1))))
    }
    alt((higher_unop, parse_pow))(input)
}

pub fn parse_higher_binop(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, (mut res, res1)) = tuple((parse_higher_unop, many0(tuple((delimited(c::space0, alt((tag("*"), tag("/"), tag("%"))), c::space0), parse_higher_unop)))))(input)?;
    for (op, e) in res1.into_iter() {
        res = Box::new(Expr::BinOp(op, res, e));
    }
    Ok((rem, res))
}

pub fn parse_lower_unop(input: &str) -> IResult<&str, Box<Expr>> {
    fn lower_unop(input: &str) -> IResult<&str, Box<Expr>> {
        let (rem, res) = tuple((terminated(alt((tag("+"), tag("-"))), c::space0), parse_lower_unop))(input)?;
        Ok((rem, Box::new(Expr::UnOp(res.0, res.1))))
    }
    alt((lower_unop, parse_higher_binop))(input)
}

pub fn parse_lower_binop(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, (mut res, res1)) = tuple((parse_lower_unop, many0(tuple((delimited(c::space0, alt((tag("+"), tag("-"))), c::space0), parse_lower_unop)))))(input)?;
    for (op, e) in res1.into_iter() {
        res = Box::new(Expr::BinOp(op, res, e));
    }
    Ok((rem, res))
}

pub fn parse_cmp_binop(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, (mut res, res1)) = tuple((parse_lower_binop, many0(tuple((delimited(c::space0, alt((tag(">"), tag(">="), tag("<"), tag("<="), tag("=="), tag("!="))), c::space0), parse_lower_binop)))))(input)?;
    for (op, e) in res1.into_iter() {
        res = Box::new(Expr::BinOp(op, res, e));
    }
    Ok((rem, res))
}

pub fn parse_not_unop(input: &str) -> IResult<&str, Box<Expr>> {
    fn not_unop(input: &str) -> IResult<&str, Box<Expr>> {
        let (rem, res) = tuple((terminated(tag("!"), c::space0), parse_not_unop))(input)?;
        Ok((rem, Box::new(Expr::UnOp(res.0, res.1))))
    }
    alt((not_unop, parse_cmp_binop))(input)
}

pub fn parse_and_binop(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, (mut res, res1)) = tuple((parse_not_unop, many0(tuple((delimited(c::space0, tag("&&"), c::space0), parse_not_unop)))))(input)?;
    for (op, e) in res1.into_iter() {
        res = Box::new(Expr::BinOp(op, res, e));
    }
    Ok((rem, res))
}

pub fn parse_or_binop(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, (mut res, res1)) = tuple((parse_and_binop, many0(tuple((delimited(c::space0, tag("||"), c::space0), parse_and_binop)))))(input)?;
    for (op, e) in res1.into_iter() {
        res = Box::new(Expr::BinOp(op, res, e));
    }
    Ok((rem, res))
}

pub fn parse_expr(input: &str) -> IResult<&str, Box<Expr>> {
    parse_or_binop(input)
}

fn main() -> Result<(), Box<dyn Error>> {
    let (remaining_input, output) = parse_expr("write_int ( read_int ( ) * k ) + write_char ( 10 ) - 10 + * ( malloc ( 2 ) + 1 )")?;
    let mut registers = HashMap::new();
    registers.insert("k", 3000);
    println!("{:?} {:?}", remaining_input, output);
    println!("{}", output.eval(&registers, &mut Mem::new()));
    Ok(())
}