use std::{num::ParseFloatError, str::{CharIndices}, fmt::Display, vec, cmp::max, collections::HashMap, f32::consts::{E, PI}, iter::zip};

use itertools::{izip, enumerate};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ConstantType {
    Pi,
    E
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LiteralType {
    Value(f32),
    Constant(ConstantType)
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FunctionType {
    Sin,
    Cos
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum OperatorType {
    Add,
    Sub,
    Mul,
    Div,
    Exp
}

pub fn precedence(op: OperatorType) -> i32 {
    match op {
        OperatorType::Add => 1,
        OperatorType::Sub => 1,
        OperatorType::Mul => 2,
        OperatorType::Div => 2,
        OperatorType::Exp => 3
    }
}

pub fn symbol(op: &OperatorType) -> char {
    match op {
        OperatorType::Add => '+',
        OperatorType::Sub => '-',
        OperatorType::Mul => '*',
        OperatorType::Div => '/',
        OperatorType::Exp => '^',
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DelimiterType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Literal(LiteralType),
    Variable(String),
    Function(FunctionType),
    Operator(OperatorType),
    Delimiter(DelimiterType)
}

#[derive(Clone, Debug, PartialEq)]
pub enum LexError {
    UnknownCharacter(char, usize),
    InvalidLiteral(ParseFloatError, usize)
}

pub fn lex<'a>(s: &'a str) -> Result<Vec<Token>, LexError> {
    
    let mut tokens: Vec<Token> = Vec::new();
    let mut it: CharIndices = s.char_indices();

    while let Some((idx, c)) = it.next() {

        if c.is_whitespace() {
            continue;
        }

        if c.is_alphabetic() {
            match c {
                'π' => tokens.push(Token::Literal(LiteralType::Constant(ConstantType::Pi))),
                'e' => tokens.push(Token::Literal(LiteralType::Constant(ConstantType::E))),
                _ => {
                    let mut fwd = it.clone().peekable();
                    let mut fwd_idx: usize = idx;
                    while let Some((peek_idx, d)) = fwd.peek() {
                        if !d.is_alphabetic() {
                            break;
                        }
                        fwd_idx = *peek_idx;
                        fwd.next();
                    }
                    fwd_idx += 1;

                    let s: String = s[idx..fwd_idx].chars().collect();
                    match s.as_str() {
                        "sin" => tokens.push(Token::Function(FunctionType::Sin)),
                        "cos" => tokens.push(Token::Function(FunctionType::Cos)),
                        _ => tokens.push(Token::Variable(s))
                    }

                    (idx..(fwd_idx-1)).for_each(|_e| { it.next().unwrap(); });
                }
            }
        }

        else if c.is_numeric() || c == '.' {
            let mut fwd = it.clone().peekable();
            let mut fwd_idx: usize = idx;
            while let Some((peek_idx, d)) = fwd.peek() {
                if !(d.is_numeric() || *d == '.') {
                    break;
                }
                fwd_idx = *peek_idx;
                fwd.next();
            }
            fwd_idx += 1;

            let f: f32 = s[idx..fwd_idx].parse().map_err(|e| LexError::InvalidLiteral(e, idx))?;
            tokens.push(Token::Literal(LiteralType::Value(f)));
            (idx..(fwd_idx-1)).for_each(|_e| { it.next().unwrap(); });
        }

        else {
            match c {
                '+' => tokens.push(Token::Operator(OperatorType::Add)),
                '-' => tokens.push(Token::Operator(OperatorType::Sub)),
                '*' => tokens.push(Token::Operator(OperatorType::Mul)),
                '/' => tokens.push(Token::Operator(OperatorType::Div)),
                '^' => tokens.push(Token::Operator(OperatorType::Exp)),
                '(' => tokens.push(Token::Delimiter(DelimiterType::LeftParen)),
                ')' => tokens.push(Token::Delimiter(DelimiterType::RightParen)),
                '[' => tokens.push(Token::Delimiter(DelimiterType::LeftBrace)),
                ']' => tokens.push(Token::Delimiter(DelimiterType::RightBrace)),
                ',' => tokens.push(Token::Delimiter(DelimiterType::Comma)),
                _ => return Err(LexError::UnknownCharacter(c, idx))
            }
        }
    }

    Ok(tokens)

}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'a> {
    Literal(LiteralType),
    Variable(String, Option<&'a [f32]>),
    Function(FunctionType),
    Operator(OperatorType, Vec<Expr<'a>>),
    Paren(Box<Expr<'a>>),
    Array(Vec<usize>, Vec<Expr<'a>>)
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParseError {
    InvalidSyntax(usize),
    UnmatchedParen(usize),
    UnmatchedBrace(usize),
    InternalError(&'static str)
}

fn descend<'a>(tokens: &Vec<Token>, start: usize, stop: usize) -> Result<Expr<'a>, ParseError> {

    let mut paren_depth: i32 = 0;
    let mut brace_depth: i32 = 0;
    let mut min_op_prec: i32 = i32::MAX;
    let mut min_op_idx: Option<usize> = None;

    println!("{:?}", &tokens[start..stop]);

    for i in start..stop {
        match &tokens[i] {
            Token::Delimiter(delim_type) => match delim_type {
                DelimiterType::LeftParen => paren_depth += 1,
                DelimiterType::RightParen => paren_depth -= 1,
                DelimiterType::LeftBrace => brace_depth += 1,
                DelimiterType::RightBrace => brace_depth -= 1,
                _ => ()
            }
            Token::Operator(op) => {
                if paren_depth == 0 && brace_depth == 0 && precedence(*op) < min_op_prec {
                    min_op_prec = precedence(*op);
                    min_op_idx = Some(i);
                }
            }
            _ => ()
        }
        if paren_depth < 0 {
            return Err(ParseError::UnmatchedParen(i));
        }
        else if brace_depth < 0 {
            return Err(ParseError::UnmatchedBrace(i));
        }
    }

    if let Some(idx) = min_op_idx {
        match tokens[idx] {
            Token::Operator(op) => {
                match op {
                    OperatorType::Add | OperatorType::Mul | OperatorType::Div | OperatorType::Exp => {
                        let lhs = descend(tokens, start, idx)?;
                        let rhs = descend(tokens, idx+1, stop)?;
                        let children = vec![lhs, rhs];
                        Ok(Expr::Operator(op, children))
                    }
                    OperatorType::Sub => {
                        let lhs = if idx > 0 { descend(tokens, start, idx)? } else { Expr::Literal(LiteralType::Value(-1.0)) };
                        let rhs = descend(tokens, idx+1, stop)?;
                        let children = vec![lhs, rhs];
                        Ok(Expr::Operator(op, children))
                    }
                }
            }
            _ => return Err(ParseError::InternalError("Invalid state. Index of operator token contains non-operator."))
        }
    }
    else {
        match &tokens[start] {
            Token::Literal(lt) => Ok(Expr::Literal(*lt)),
            Token::Variable(s) => Ok(Expr::Variable(s.clone(), None)),
            Token::Function(ft) => Ok(Expr::Function(*ft)),
            Token::Operator(ot) => Err(ParseError::InternalError("Invalid state. Undetected operator.")),
            Token::Delimiter(dt) => match dt {
                DelimiterType::LeftParen => Ok(Expr::Paren(Box::new(descend(tokens, start+1, stop-1)?))),
                DelimiterType::RightParen => Err(ParseError::UnmatchedParen(start)),
                DelimiterType::LeftBrace => {

                    let mut sub_exprs: Vec<Expr> = Vec::new();
                    let mut sep_idxs: Vec<usize> = vec![start];
                    let mut brace_depth: i32 = 0;

                    for i in start+1..stop-1 {
                        match tokens[i] {
                            Token::Delimiter(dt) => match dt {
                                DelimiterType::LeftBrace => brace_depth += 1,
                                DelimiterType::RightBrace => brace_depth -= 1,
                                DelimiterType::Comma => {
                                    if brace_depth == 0 {
                                        sep_idxs.push(i);
                                    }
                                },
                                _ => ()
                            },
                            _ => ()
                        }
                    }
                    
                    sep_idxs.push(stop-1);
                    for arr in sep_idxs.windows(2) {
                        sub_exprs.push(descend(tokens, arr[0]+1, arr[1])?);
                    }

                    let shape = match &sub_exprs[0] {
                        Expr::Array(sh, _exs) => {
                            let mut v = vec![sub_exprs.len()];
                            v.extend(sh);
                            v
                        }
                        _ => vec![sub_exprs.len()]
                    };

                    Ok(Expr::Array(shape, sub_exprs))
                },
                DelimiterType::RightBrace => Err(ParseError::UnmatchedBrace(start)),
                DelimiterType::Comma => Err(ParseError::InvalidSyntax(start))
            }
        }
    }
}

pub fn parse<'a>(tokens: Vec<Token>) -> Result<Expr<'a>, ParseError> {
    descend(&tokens, 0, tokens.len())
}

fn get_fmt_rows(e: &Expr) -> Vec<String> {
    match e {
        Expr::Literal(lt) => match lt {
            LiteralType::Constant(ct) => match ct {
                ConstantType::Pi => vec!["π".to_owned()],
                ConstantType::E => vec!["e".to_owned()]
            },
            LiteralType::Value(f) => vec![format!("{:.4}", f)]
        },
        Expr::Variable(s, _) => vec![s.to_owned()],
        Expr::Function(ft) => match ft {
            _ => unimplemented!(),
            FunctionType::Cos => vec!["cos".to_owned()],
            FunctionType::Sin => vec!["sin".to_owned()]
        },
        Expr::Operator(op, children) => {
            let lhs_fmt_rows = get_fmt_rows(&children[0]);
            let rhs_fmt_rows = get_fmt_rows(&children[1]);
            
            let lhs_rows = lhs_fmt_rows.len();
            let rhs_rows = rhs_fmt_rows.len();
            let lhs_cols = lhs_fmt_rows[0].chars().count();
            let rhs_cols = rhs_fmt_rows[0].chars().count();

            let max_rows = max(lhs_rows, rhs_rows);
            let max_cols = max(lhs_cols, rhs_cols);
            
            match op {
                OperatorType::Add | OperatorType::Mul |  OperatorType::Sub | OperatorType::Exp => {

                    let lhs_row_pad = (max_rows - lhs_rows) / 2;
                    let rhs_row_pad = (max_rows - rhs_rows) / 2;
                    let op_loc = max_rows / 2;

                    let lhs_upper_pad = vec![" ".to_owned().repeat(lhs_cols); lhs_row_pad];
                    let lhs_lower_pad = vec![" ".to_owned().repeat(lhs_cols); max_rows - (lhs_row_pad + lhs_rows)];
                    let rhs_upper_pad = vec![" ".to_owned().repeat(rhs_cols); rhs_row_pad];
                    let rhs_lower_pad = vec![" ".to_owned().repeat(rhs_cols); max_rows - (rhs_row_pad + rhs_rows)];

                    let mut lhs = lhs_upper_pad;
                    lhs.extend(lhs_fmt_rows);
                    lhs.extend(lhs_lower_pad);

                    let mut rhs = rhs_upper_pad;
                    rhs.extend(rhs_fmt_rows);
                    rhs.extend(rhs_lower_pad);

                    let mut op_rows = vec![" ".to_owned().repeat(3); max_rows];
                    op_rows[op_loc] = format!(" {} ", symbol(op));

                    let mut rows: Vec<String> = Vec::new();
                    if *op == OperatorType::Add || *op == OperatorType::Mul {
                        for (lhsr, opr, rhsr) in izip!(&lhs, &op_rows, &rhs) {
                            rows.push(format!("{}{}{}", lhsr, opr, rhsr));
                        }
                    }
                    else {
                        if max_rows <= 1 {
                            for (lhsr, opr, rhsr) in izip!(&lhs, &op_rows, &rhs) {
                                rows.push(format!("{}{}({})", lhsr, opr, rhsr));
                            }
                        }
                        else {
                            let mrm1 = max_rows - 1;
                            for (row_idx, (lhsr, opr, rhsr)) in enumerate(izip!(&lhs, &op_rows, &rhs)) {
                                if row_idx == 0 {
                                    rows.push(format!("{}{}⌈{}⌉)", lhsr, opr, rhsr));
                                }
                                else if row_idx == mrm1 {
                                    rows.push(format!("{}{}⌊{}⌋)", lhsr, opr, rhsr));
                                }
                                else {
                                    rows.push(format!("{}{}|{}|", lhsr, opr, rhsr));
                                }
                            }
                        }
                    }
                    rows
                }
                OperatorType::Div => {
                    let lhs_col_pad = (max_cols - lhs_cols) / 2;
                    let rhs_col_pad = (max_cols - rhs_cols) / 2;
                    
                    let lhs_left_pad  = vec![" ".to_owned().repeat(lhs_col_pad); lhs_rows];
                    let lhs_right_pad = vec![" ".to_owned().repeat(max_cols - (lhs_col_pad + lhs_cols)); lhs_rows];
                    let rhs_left_pad  = vec![" ".to_owned().repeat(rhs_col_pad); rhs_rows];
                    let rhs_right_pad = vec![" ".to_owned().repeat(max_cols - (rhs_col_pad + rhs_cols)); rhs_rows];

                    let mut rows: Vec<String> = Vec::new();
                    for (lp, l, rp) in izip!(&lhs_left_pad, &lhs_fmt_rows, &lhs_right_pad) {
                        rows.push(format!("{}{}{}", lp, l, rp));
                    }
                    rows.push("―".to_owned().repeat(max_cols));
                    for (lp, r, rp) in izip!(&rhs_left_pad, &rhs_fmt_rows, &rhs_right_pad) {
                        rows.push(format!("{}{}{}", lp, r, rp));
                    }
                    rows
                }
            }
        },
        Expr::Paren(inner) => {
            let inner_fmt_rows = get_fmt_rows(inner);
            let nrows = inner_fmt_rows.len();
            if nrows <= 1 {
                vec![format!("({})", inner_fmt_rows[0])]
            }
            else {
                let mut rows = Vec::new();
                let nrm1 = nrows - 1;
                for (row_idx, row) in enumerate(&inner_fmt_rows) {
                    if row_idx == 0 {
                        rows.push(format!("⌈ {} ⌉", row));
                    }
                    else if row_idx == nrm1 {
                        rows.push(format!("⌊ {} ⌋", row));
                    }
                    else {
                        rows.push(format!("| {} |", row));
                    }
                }
                rows
            }
        },
        Expr::Array(shape, children) => unimplemented!()
    }
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let rows = get_fmt_rows(&self);
        let l = rows.len();
        for (i, r) in enumerate(rows) {
            write!(f, "{}", r)?;
            if i < l-1 {
                write!(f, "\n")?;
            }
        }
        Ok(())
    }
}


#[derive(Clone, Debug, PartialEq)]
pub enum EvalError {
    EmptyVariable,
    VariableSizeMismatch
}

impl<'a> Expr<'a> {
    pub fn subs(&mut self, pairs: &HashMap<String, &'a [f32]>) {
        match self {
            Expr::Variable(name, vals) => {
                if let Some(vs) = pairs.get(name) {
                    *vals = Some(*vs);
                }
            },
            Expr::Operator(_op, children) => {
                for c in children {
                    c.subs(pairs);
                }
            },
            Expr::Array(_shape, children) => {
                for c in children {
                    c.subs(pairs);
                }
            },
            Expr::Paren(c) => c.subs(pairs),
            _ => ()
        }
    }

    pub fn eval(&'a self) -> Result<Vec<f32>, EvalError> {
        match self {
            Expr::Literal(lt) => {
                match lt {
                    LiteralType::Constant(ct) => {
                        match ct {
                            ConstantType::E => Ok(vec![E]),
                            ConstantType::Pi => Ok(vec![PI])
                        }
                    },
                    LiteralType::Value(v) => Ok(vec![*v])
                }
            },
            Expr::Variable(_name, ovs) => {
                if let Some(vs) = ovs {
                    Ok(vs.to_vec())
                }
                else {
                    Err(EvalError::EmptyVariable)
                }
            },
            Expr::Function(ft) => unimplemented!(),
            Expr::Operator(op, children) => {
                let lhs = children[0].eval()?;
                let rhs = children[1].eval()?;

                let l = max(lhs.len(), rhs.len());
                let v1 = match lhs.len() {
                    1 => lhs.repeat(l),
                    l => lhs,
                    _ => return Err(EvalError::VariableSizeMismatch)
                };
                let v2 = match rhs.len() {
                    1 => rhs.repeat(l),
                    l => rhs,
                    _ => return Err(EvalError::VariableSizeMismatch)
                };

                let v3 = match op {
                    OperatorType::Add => zip(v1, v2).map(|(a, b)| a+b).collect(),
                    OperatorType::Sub => zip(v1, v2).map(|(a, b)| a-b).collect(),
                    OperatorType::Mul => zip(v1, v2).map(|(a, b)| a*b).collect(),
                    OperatorType::Div => zip(v1, v2).map(|(a, b)| a/b).collect(),
                    OperatorType::Exp => zip(v1, v2).map(|(a, b)| f32::powf(a, b)).collect(),
                };
                Ok(v3)
            },
            Expr::Paren(c) => c.eval(),
            Expr::Array(shape, chldren) => unimplemented!()

        }
    }
}