use std::{collections::HashMap, fmt::Display, io, num::ParseFloatError};

const ERROR_MARGIN: f64 = 0.00000001;

macro_rules! ensure_tonicity {
    ($check_fn: expr) => {{
        |args: &[RispExpr]| -> Result<RispExpr, RispErr> {
            let floats = parse_floats(args)?;
            let first = *floats
                .first()
                .ok_or_else(|| RispErr::Reason("Expected at least one number".to_string()))?;
            let rest = &floats[1..];
            fn func(prev: &f64, xs: &[f64]) -> bool {
                match xs.first() {
                    Some(x) => $check_fn(prev, x) && func(x, &xs[1..]),
                    None => true,
                }
            }
            Ok(RispExpr::Bool(func(&first, rest)))
        }
    }};
}

#[derive(Clone, Debug)]
pub struct RispEnv {
    data: HashMap<String, RispExpr>,
}

impl Default for RispEnv {
    fn default() -> Self {
        let mut data: HashMap<String, RispExpr> = HashMap::new();
        data.insert(
            "+".to_string(),
            RispExpr::Func(|args: &[RispExpr]| -> Result<RispExpr, RispErr> {
                let sum = parse_floats(args)?.iter().fold(0.0, |sum, a| sum + a);
                Ok(RispExpr::Number(sum))
            }),
        );
        data.insert(
            "*".to_string(),
            RispExpr::Func(|args: &[RispExpr]| -> Result<RispExpr, RispErr> {
                let product = parse_floats(args)?.iter().product();
                Ok(RispExpr::Number(product))
            }),
        );
        data.insert(
            "/".to_string(),
            RispExpr::Func(|args: &[RispExpr]| -> Result<RispExpr, RispErr> {
                let floats = parse_floats(args)?;
                let dividend = *floats
                    .first()
                    .ok_or_else(|| RispErr::Reason("Expected at least one number".to_string()))?;
                let quotient: f64 = floats[1..].iter().product();
                Ok(RispExpr::Number(dividend / quotient))
            }),
        );
        data.insert(
            "-".to_string(),
            RispExpr::Func(|args: &[RispExpr]| -> Result<RispExpr, RispErr> {
                let floats = parse_floats(args)?;
                let first = *floats
                    .first()
                    .ok_or_else(|| RispErr::Reason("Expected at least one number".to_string()))?;
                let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

                Ok(RispExpr::Number(first - sum_of_rest))
            }),
        );
        data.insert(
            "abs".to_string(),
            RispExpr::Func(|args: &[RispExpr]| -> Result<RispExpr, RispErr> {
                let floats = parse_floats(args)?;
                match floats.len() == 1 {
                    true => Ok(RispExpr::Number(floats.first().unwrap().abs())),
                    false => Err(RispErr::Reason("Expected one number".to_string())),
                }
            }),
        );
        data.insert(
            "not".to_string(),
            RispExpr::Func(|args: &[RispExpr]| -> Result<RispExpr, RispErr> {
                Err(RispErr::Reason("not is still not implmented".to_string()))
            }),
        );
        // data.insert(
        //     "!=".to_string(),
        //     RispExpr::Func(ensure_tonicity!(
        //         |a: &f64, b: &f64| (a - b).abs() > ERROR_MARGIN
        //     )),
        // );
        data.insert(
            "=".to_string(),
            RispExpr::Func(ensure_tonicity!(
                |a: &f64, b: &f64| (a - b).abs() < ERROR_MARGIN
            )),
        );
        data.insert(
            ">".to_string(),
            RispExpr::Func(ensure_tonicity!(|a, b| a > b)),
        );
        data.insert(
            ">=".to_string(),
            RispExpr::Func(ensure_tonicity!(|a, b| a >= b)),
        );
        data.insert(
            "<".to_string(),
            RispExpr::Func(ensure_tonicity!(|a, b| a < b)),
        );
        data.insert(
            "<=".to_string(),
            RispExpr::Func(ensure_tonicity!(|a, b| a <= b)),
        );
        Self { data }
    }
}

fn parse_floats(args: &[RispExpr]) -> Result<Vec<f64>, RispErr> {
    args.iter().map(|x| parse_single_float(x)).collect()
}

fn parse_single_float(exp: &RispExpr) -> Result<f64, RispErr> {
    match exp {
        RispExpr::Number(num) => Ok(*num),
        _ => Err(RispErr::Reason("Expected a number".to_string())),
    }
}

#[allow(dead_code)]
#[derive(Clone)]
pub enum RispExpr {
    Bool(bool),
    Symbol(String),
    Number(f64),
    List(Vec<RispExpr>),
    Func(fn(&[RispExpr]) -> Result<RispExpr, RispErr>),
}

impl Display for RispExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            RispExpr::Symbol(symbol) => symbol.clone(),
            RispExpr::Number(n) => n.to_string(),
            RispExpr::List(list) => {
                let expressions: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", expressions.join(","))
            }
            RispExpr::Func(_) => "Function {}".to_string(),
            RispExpr::Bool(b) => b.to_string(),
        };
        write!(f, "{}", str)
    }
}

impl std::fmt::Debug for RispExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Symbol(arg0) => f.debug_tuple("Symbol").field(arg0).finish(),
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::List(arg0) => f.debug_tuple("List").field(arg0).finish(),
            Self::Func(_) => write!(f, "Function called"),
        }
    }
}

fn eval(exp: &RispExpr, env: &mut RispEnv) -> Result<RispExpr, RispErr> {
    match exp {
        RispExpr::Symbol(k) => env
            .data
            .get(k)
            .ok_or_else(|| RispErr::Reason(format!("Unexpectd symbol k=`{}`", k)))
            .map(|x| x.clone()),
        RispExpr::Number(_) => Ok(exp.clone()),
        RispExpr::List(list) => {
            let first_form = list
                .first()
                .ok_or_else(|| RispErr::Reason("Expected a non-empty list".to_string()))?;
            let arg_forms = &list[1..];
            let first_eval = eval(first_form, env)?;
            match first_eval {
                RispExpr::Func(f) => {
                    let args_eval = arg_forms
                        .iter()
                        .map(|x| eval(x, env))
                        .collect::<Result<Vec<RispExpr>, RispErr>>();
                    f(&args_eval?)
                }
                _ => Err(RispErr::Reason("First form must be a function".to_string())),
            }
        }
        RispExpr::Func(_) => Err(RispErr::Reason("Unexpectd form".to_string())),
        RispExpr::Bool(_) => Ok(exp.clone()),
    }
}

pub fn parse_eval(expr: String, env: &mut RispEnv) -> Result<RispExpr, RispErr> {
    let (parsed_expr, _) = parse(&tokenize(&expr))?;
    let evaluated_expr = eval(&parsed_expr, env)?;
    Ok(evaluated_expr)
}

pub fn slurp_expr() -> String {
    let mut expr = String::new();
    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");
    expr
}

#[derive(Debug)]
pub enum RispErr {
    /// Syntax error returning line number and char
    SyntaxErr(u32, u32),
    /// Parens not balanced; contains number of parens needed
    UnbalancedParens(usize),
    /// General error
    Reason(String),
}

impl Display for RispErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RispErr::SyntaxErr(line, column) => {
                write!(f, "Syntax error at line {}, column {}", line, column)
            }
            RispErr::UnbalancedParens(n) => {
                write!(f, "Use your % key more! add {} more parenthesis", n)
            }
            RispErr::Reason(reason) => write!(f, "{}", reason),
        }
    }
}

// tokenizes an expression
fn tokenize(expr: &str) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

fn parse(tokens: &[String]) -> Result<(RispExpr, &[String]), RispErr> {
    // Reads tokens and splits at first
    // Returns an error if there is no tokens
    let (token, rest) = tokens
        .split_first()
        .ok_or_else(|| RispErr::Reason("Could not get token".to_string()))?;
    // eventually it will just result in a Symbol, Number, or another expression
    match &token[..] {
        "(" => read_seq(rest),
        ")" => Err(RispErr::UnbalancedParens(1)),
        _ => Ok((parse_atom(token), rest)),
    }
}

fn read_seq(tokens: &[String]) -> Result<(RispExpr, &[String]), RispErr> {
    let mut expressions_parsed: Vec<RispExpr> = Vec::new();
    let mut to_be_parse = tokens;
    loop {
        let (token, rest) = to_be_parse
            .split_first()
            .ok_or_else(|| RispErr::Reason("Could not find closing `)`".to_string()))?;
        if token == ")" {
            // skip `)`, head to the token after
            // rest at this point could be exhausted and be empty
            return Ok((RispExpr::List(expressions_parsed), rest));
        }
        let (parsed, left_to_parse) = parse(to_be_parse)?;
        expressions_parsed.push(parsed);
        to_be_parse = left_to_parse;
    }
}

fn parse_atom(token: &str) -> RispExpr {
    match token {
        "true" => RispExpr::Bool(true),
        "false" => RispExpr::Bool(false),
        _ => {
            let potential_float: Result<f64, ParseFloatError> = token.parse();
            match potential_float {
                Ok(n) => RispExpr::Number(n),
                Err(_) => RispExpr::Symbol(token.to_string()),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn error_test() {
        let error = RispErr::Reason("Your program sucks".to_string());
        println!("{}", error)
    }

    #[test]
    fn tokenize_test() {
        assert_eq!(vec!["(", "+", "5", "4", ")"], tokenize("(+ 5 4)"))
    }

    #[test]
    fn parse_test() {
        let tokens = tokenize("(begin (define r 10) (* pi (* r r)))");
        // println!("{:?}", tokens);
        let parse = parse(&tokens);
        println!("{:?}", parse.unwrap().0)
    }

    #[test]
    fn eval_test() {
        let program = "(/ (* (+ (abs (- 5 10)) 2) 2) 2)";
        let tokenize = tokenize(program);
        let parse = parse(&tokenize);
        let eval = eval(&parse.unwrap().0, &mut RispEnv::default());
        println!("{:?}", eval)
    }
}
