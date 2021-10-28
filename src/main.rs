use risp::{parse_eval, slurp_expr, RispEnv};

fn main() {
    let env = &mut RispEnv::default();
    loop {
        println!("risp >");
        let expr = slurp_expr();
        match parse_eval(expr, env) {
            Ok(resp) => println!("// 🔥 => {}", resp),
            Err(e) => match e {
                risp::RispErr::Reason(msg) => println!("// 🙀 => {}", msg),
                _ => println!("Wow. you broke it even more"),
            },
        }
    }
}
