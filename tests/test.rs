use std::collections::HashMap;
use iter_num_tools::lin_space;

use itertools::Itertools;
use rsymb;

#[test]
fn test() {

    let tokens = rsymb::lex("t^2").unwrap();
    let mut expr = rsymb::parse(tokens).unwrap();

    let mut pairs: HashMap<String, &[f32]> = HashMap::new();
    let buf: Vec<f32> = lin_space(0.0..=1.0, 11).into_iter().collect_vec();
    pairs.insert("t".to_string(), &buf);
    expr.subs(&pairs);

    println!("{:?}", expr);
    println!("{:?}", expr.eval());
    
}
