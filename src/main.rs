mod mem;

use mem::Mem;
use std::borrow::Borrow;
use std::collections::HashMap;
use nom::IResult;
use nom::character::complete as c;
use nom::branch::alt;
use nom::sequence::{tuple, delimited, preceded, terminated, separated_pair, pair};
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
    pub fn eval(&self, registers: &'a HashMap<&'a str, i128>, mem: &mut Mem<i128>) -> i128 {
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

pub fn identifier(s: &str) -> IResult<&str, &str> {
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
    let (rem, res) = tuple((
        identifier,
        delimited(c::multispace0, tag("("), c::multispace0),
        opt(parse_expr),
        preceded(c::multispace0, tag(")"))
    ))(input)?;
    Ok((rem, Box::new(Expr::Call(res.0, res.2))))
}

pub fn parse_single_expr(input: &str) -> IResult<&str, Box<Expr>> {
    alt((
        parse_uint,
        parse_call,
        parse_ident,
        delimited(
            terminated(tag("("), c::multispace0), 
            parse_expr, 
            preceded(c::multispace0, tag(")"))
        )
    ))(input)
}

pub fn parse_pow(input: &str) -> IResult<&str, Box<Expr>> {
    match tuple((parse_single_expr, delimited(c::multispace0, tag("^"), c::multispace0), parse_pow))(input) {
        Ok((rem, res)) => Ok((rem, Box::new(Expr::BinOp("^", res.0, res.2)))),
        _ => parse_single_expr(input),
    }
}

pub fn parse_higher_unop(input: &str) -> IResult<&str, Box<Expr>> {
    fn higher_unop(input: &str) -> IResult<&str, Box<Expr>> {
        let (rem, res) = tuple((terminated(tag("*"), c::multispace0), parse_higher_unop))(input)?;
        Ok((rem, Box::new(Expr::UnOp(res.0, res.1))))
    }
    alt((higher_unop, parse_pow))(input)
}

pub fn parse_higher_binop(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, (mut res, res1)) = tuple((
        parse_higher_unop,
        many0(tuple((
            delimited(c::multispace0, alt((tag("*"), tag("/"), tag("%"))), c::multispace0),
            parse_higher_unop
        )))
    ))(input)?;
    for (op, e) in res1.into_iter() {
        res = Box::new(Expr::BinOp(op, res, e));
    }
    Ok((rem, res))
}

pub fn parse_lower_unop(input: &str) -> IResult<&str, Box<Expr>> {
    fn lower_unop(input: &str) -> IResult<&str, Box<Expr>> {
        let (rem, res) = tuple((terminated(alt((tag("+"), tag("-"))), c::multispace0), parse_lower_unop))(input)?;
        Ok((rem, Box::new(Expr::UnOp(res.0, res.1))))
    }
    alt((lower_unop, parse_higher_binop))(input)
}

pub fn parse_lower_binop(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, (mut res, res1)) = tuple((
        parse_lower_unop,
        many0(tuple((
            delimited(c::multispace0, alt((tag("+"), tag("-"))), c::multispace0),
            parse_lower_unop
        )))
    ))(input)?;
    for (op, e) in res1.into_iter() {
        res = Box::new(Expr::BinOp(op, res, e));
    }
    Ok((rem, res))
}

pub fn parse_cmp_binop(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, (mut res, res1)) = tuple((
        parse_lower_binop,
        many0(tuple((
            delimited(
                c::multispace0, 
                alt((tag(">="), tag(">"), tag("<="), tag("<"), tag("=="), tag("!="))), 
                c::multispace0),
            parse_lower_binop
        )))
    ))(input)?;
    for (op, e) in res1.into_iter() {
        res = Box::new(Expr::BinOp(op, res, e));
    }
    Ok((rem, res))
}

pub fn parse_not_unop(input: &str) -> IResult<&str, Box<Expr>> {
    fn not_unop(input: &str) -> IResult<&str, Box<Expr>> {
        let (rem, res) = tuple((terminated(tag("!"), c::multispace0), parse_not_unop))(input)?;
        Ok((rem, Box::new(Expr::UnOp(res.0, res.1))))
    }
    alt((not_unop, parse_cmp_binop))(input)
}

pub fn parse_and_binop(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, (mut res, res1)) = tuple((
        parse_not_unop,
        many0(tuple((
            delimited(c::multispace0, tag("&&"), c::multispace0), 
            parse_not_unop
        )))
    ))(input)?;
    for (op, e) in res1.into_iter() {
        res = Box::new(Expr::BinOp(op, res, e));
    }
    Ok((rem, res))
}

pub fn parse_or_binop(input: &str) -> IResult<&str, Box<Expr>> {
    let (rem, (mut res, res1)) = tuple((
        parse_and_binop,
        many0(tuple((
            delimited(c::multispace0, tag("||"), c::multispace0), 
            parse_and_binop
        )))
    ))(input)?;
    for (op, e) in res1.into_iter() {
        res = Box::new(Expr::BinOp(op, res, e));
    }
    Ok((rem, res))
}

pub fn parse_expr(input: &str) -> IResult<&str, Box<Expr>> {
    parse_or_binop(input)
}

#[derive(Debug)]
pub enum Cmd<'a> {
    Expr(Box<Expr<'a>>),
    Decl(&'a str),
    Assign(Box<Expr<'a>>, Box<Expr<'a>>),
    Seq(Vec<Box<Cmd<'a>>>),
    If(Box<Expr<'a>>, Box<Cmd<'a>>, Box<Cmd<'a>>),
    While(Box<Expr<'a>>, Box<Cmd<'a>>),
}

impl<'a> Cmd<'a> {
    pub fn exec(&self, registers: &mut HashMap<&'a str, i128>, mem: &mut Mem<i128>) {
        match self {
            Cmd::Expr(e) => {
                e.eval(registers, mem);
            },
            Cmd::Decl(ident) => {
                registers.insert(ident, 0);
            },
            Cmd::Assign(e1, e2) => {
                match e1.borrow() {
                    Expr::UnOp("*", e1) => {
                        let tmp = e2.eval(registers, mem);
                        let index = e1.eval(registers, mem) as usize;
                        match mem.mem.get_mut(index) {
                            Some(m) => {
                                *m = Some(tmp);
                            },
                            None => panic!("cannot assign to invalid memory")
                        }
                    },
                    Expr::Ident(ident) => {
                        let tmp = e2.eval(registers, mem);
                        registers.insert(ident, tmp);
                    },
                    _ => panic!("cannot assign to {:?}", e1)
                }
            },
            Cmd::Seq(arr) => {
                for c in arr.iter() {
                    c.exec(registers, mem);
                }
            },
            Cmd::If(cond, c1, c2) => {
                if cond.eval(registers, mem) != 0 {
                    c1.exec(registers, mem);
                }
                else {
                    c2.exec(registers, mem);
                }
            },
            Cmd::While(cond, c) => {
                while cond.eval(registers, mem) != 0 {
                    c.exec(registers, mem);
                }
            },
        };
    }
}

pub fn parse_expr_cmd(input: &str) -> IResult<&str, Box<Cmd>> {
    let (rem, res) = parse_expr(input)?;
    Ok((rem, Box::new(Cmd::Expr(res))))
}

pub fn parse_decl(input: &str) -> IResult<&str, Box<Cmd>> {
    let (rem, res) = preceded(tuple((tag("var"), c::multispace1)), identifier)(input)?;
    Ok((rem, Box::new(Cmd::Decl(res))))
}

pub fn parse_assign(input: &str) -> IResult<&str, Box<Cmd>> {
    let (rem, res) = separated_pair(
        parse_expr, 
        delimited(c::multispace0, tag("="), c::multispace0), 
        parse_expr
    )(input)?;
    Ok((rem, Box::new(Cmd::Assign(res.0, res.1))))
}

pub fn parse_single_cmd(input: &str) -> IResult<&str, Box<Cmd>> {
    alt((parse_decl, parse_assign, parse_expr_cmd))(input)
}

pub fn parse_if(input: &str) -> IResult<&str, Box<Cmd>> {
    let (rem, res) = tuple((
        recognize(tuple((tag("if"), c::multispace1))),
        parse_expr,
        recognize(tuple((opt(preceded(c::multispace1, tag("then"))), c::multispace0, tag("{"), c::multispace0))),
        parse_cmd,
        recognize(pair(c::multispace0, tag("}"))),
        opt(delimited(
            tuple((c::multispace0, tag("else"), c::multispace0, tag("{"), c::multispace0)),
            parse_cmd,
            tuple((c::multispace0, tag("}"))),
        ))
    ))(input)?;
    Ok((
        rem,
        Box::new(Cmd::If(
            res.1,
            res.3,
            match res.5 {
                Some(x) => x,
                None => Box::new(Cmd::Seq(vec![]))
            }
        ))
    ))
}

fn parse_while(input: &str) -> IResult<&str, Box<Cmd>> {
    let (rem, res) = tuple((
        tag("while"),
        preceded(c::multispace1, parse_expr),
        opt(preceded(c::multispace1, tag("do"))),
        delimited(c::multispace0, tag("{"), c::multispace0),
        parse_cmd,
        preceded(c::multispace0, tag("}")),
    ))(input)?;
    Ok((rem, Box::new(Cmd::While(res.1, res.4))))
}

pub fn parse_block_cmd(input: &str) -> IResult<&str, Box<Cmd>> {
    alt((parse_if, parse_while))(input)
}

pub fn parse_cmd(input: &str) -> IResult<&str, Box<Cmd>> {
    let (rem, (mut res, opt_cmd)) = tuple((
        many0(preceded(c::multispace0, alt((
            terminated(parse_single_cmd, preceded(c::multispace0, tag(";"))),
            terminated(parse_block_cmd, opt(preceded(c::multispace0, tag(";"))))
        )))),
        opt(preceded(c::multispace0, parse_single_cmd))
    ))(input)?;
    if let Some(cmd) = opt_cmd {
        res.push(cmd);
    }
    Ok((rem, Box::new(Cmd::Seq(res))))
}

fn main() {

}

#[test]
fn test_parse_expr() {
    let src = "1000000 / (1 + 2 * 3 ^ 4 + 5 - 7 * 5474 / 9110)";
    let (remaining_input, output) = parse_expr(src).unwrap();
    println!("{:?} {:?}", remaining_input, output);
}

#[test]
fn test_eval_expr() {
    let src = "write_int ( read_int ( ) * k ) + write_char ( 10 ) - 10 + * ( malloc ( 2 ) + 1 )";
    let (remaining_input, output) = parse_expr(src).unwrap();
    let mut registers = HashMap::new();
    registers.insert("k", 3000);
    println!("{:?} {:?}", remaining_input, output);
    println!("{}", output.eval(&registers, &mut Mem::new()));
}

#[test]
fn test_parse_cmd() {
    let src = "
    var n; var i; var p; var q; var s;
    n = read_int();
    i = 0; p = 0;
    while (i < n) do {
        q = malloc(16);
        * q = read_int();
        * (q + 8) = p;
        p = q;
        i = i + 1
    };
    s = 0;
    while (p != 0) do {
        s = s + * p;
        p = * (p + 8)
    };
    write_int(s);
    write_char(10)
    ";
    let (remaining_input, output) = delimited(c::multispace0, parse_cmd, c::multispace0)(&src).unwrap();
    println!("{:?} {:?}", remaining_input, output);
}

#[test]
fn test_exec_cmd() {
    let (registers, mem) = (&mut HashMap::new(), &mut Mem::new());
    parse_cmd("
    var n; var i; var p; var q; var s;
    n = read_int();
    i = 0; p = 0;
    while (i < n) do {
        q = malloc(2);
        * q = read_int();
        * (q + 1) = p;
        p = q;
        i = i + 1
    };
    s = 0;
    while (p != 0) do {
        s = s + * p;
        tmp = * (p + 1);
        free(p);
        p = tmp;
    };
    write_int(s);
    write_char(10)
    ").unwrap().1.exec(registers, mem);
    println!("{:?}", (registers, mem));
}