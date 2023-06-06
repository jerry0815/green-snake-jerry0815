use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::{hashmap, HashMap};
use im::HashSet;

struct Program {
  defs: Vec<Definition>,
  main: Expr,
}

enum Definition {
  Fun(String, Vec<String>, Expr),
}

use Definition::*;

#[derive(Debug)]
enum Op1 { Add1, Sub1, IsNum, IsBool, Print}

#[derive(Debug)]
enum ConditionType {Equal, Greater, GreaterEqual, Less, LessEqual}

#[derive(Debug)]
enum ArithType { Plus, Minus, Times }

#[derive(Debug)]
enum Op2 { Arith(ArithType), Cond(ConditionType) }

#[derive(Debug)]
enum Expr {
    Number(i64),
    True,
    False,
    Nil,
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
    Call(String, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Tuple(Vec<Expr>),
}


struct CompilationContext<'a> {
    brake: &'a String,
    l: &'a mut i32,
    fun_env: &'a mut HashMap<String, i32>,
    is_def: bool,
    arr_env: &'a mut HashMap<String, i32>,
    is_tail: bool,
    arg_offset: i32,
}


fn parse_bind(s: &Sexp) -> (String, Expr) {
    let keywords = vec!["let", "add1", "sub1", "if", "loop", "break", "block", "set!", "true", "false","isnum", "isbool", "print", "input", "fun", "nil", "tuple", "index"];
    match s {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(name)), _e] if keywords.contains(&&name[..]) => panic!("keyword"),
            [Sexp::Atom(S(name)), e]  => (name.to_string(), parse_expr(e)),
            _ => panic!("Invalid bind"),
        },
        _ => panic!("Invalid bind"),
    }
}


fn parse_expr(s: &Sexp) -> Expr {
    let keywords = vec!["let", "add1", "sub1", "if", "loop", "break", "block", "set!", "true", "false","isnum", "isbool","print", "fun", "nil", "tuple", "index"];
    match s {
        Sexp::Atom(I(n)) => Expr::Number(i64::try_from(*n).unwrap()),
        Sexp::Atom(S(name)) if name == "true" => Expr::True,
        Sexp::Atom(S(name)) if name == "false" => Expr::False,
        Sexp::Atom(S(name)) if name == "nil" => Expr::Nil,
        Sexp::Atom(S(name)) if keywords.contains(&&name[..]) => panic!("keyword {}", name.to_string()),
        Sexp::Atom(S(name)) => Expr::Id(String::try_from(name).unwrap()),
        Sexp::List(vec) if vec.len() == 0 => panic!("Invalid empty list"),
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1,Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1,Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum,Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool,Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "print" => Expr::UnOp(Op1::Print,Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => {
                Expr::BinOp(Op2::Arith(ArithType::Plus), Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => {
                Expr::BinOp(Op2::Arith(ArithType::Minus), Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => {
                Expr::BinOp(Op2::Arith(ArithType::Times), Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(keyword)), bind, body] if keyword == "let" => {
                match bind {
                    Sexp::List(bindings) => {
                        if bindings.len() == 0 {
                            panic!("Invalid let");
                        }
                        let mut binds = Vec::new();
                        for binding in bindings {
                            binds.push(parse_bind(binding));
                        }
                        Expr::Let(binds, Box::new(parse_expr(body)))
                    }
                    _ => panic!("Invalid let"),
                }
                    
            },
            [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                if keywords.contains(&&name[..]) || name == "input" {
                    panic!("keyword");
                }
                Expr::Set(name.to_string(), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                if exprs.len() == 0 {
                    panic!("Invalid block");
                }
                Expr::Block(exprs.into_iter().map(parse_expr).collect())
            }
            [Sexp::Atom(S(op)), e] if op == "loop" => {
                Expr::Loop(Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "break" => {
                Expr::Break(Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => {
                Expr::BinOp(Op2::Cond(ConditionType::Equal), Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },
            [Sexp::Atom(S(op)), e1, e2] if op == ">=" => {
                Expr::BinOp(Op2::Cond(ConditionType::GreaterEqual), Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" => {
                Expr::BinOp(Op2::Cond(ConditionType::LessEqual), Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },
            [Sexp::Atom(S(op)), e1, e2] if op == ">" => {
                Expr::BinOp(Op2::Cond(ConditionType::Greater), Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },
            [Sexp::Atom(S(op)), e1, e2] if op == "<" => {
                Expr::BinOp(Op2::Cond(ConditionType::Less), Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },
            [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => Expr::If(
              Box::new(parse_expr(cond)),
              Box::new(parse_expr(thn)),
              Box::new(parse_expr(els)),
            ),
            [Sexp::Atom(S(op)), e1, e2] if op == "index" => {
                Expr::Index(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            },
            _ => {
                let op = match &vec[0] {
                    Sexp::Atom(S(name)) => name,
                    _ => panic!("Invalid expression"),
                };
                match op.as_str() {
                    "tuple" => {
                        if vec.len() == 1 {
                            panic!("Invalid tuple");
                        }
                        let mut arr = Vec::new();
                        for ele in &vec[1..] {
                            arr.push(parse_expr(ele));
                        }
                        Expr::Tuple(arr)
                    },
                    _ => {
                        let funname = op;
                        if keywords.contains(&&funname[..]) || funname == "input" {
                            panic!("Invalid func call");
                        }
                        let mut args = Vec::new();
                        for sub_expr in &vec[1..] {
                            args.push(parse_expr(sub_expr));
                        }
                        Expr::Call(funname.to_string(), args)
                    }
                }
               
            }
        },
        _ => panic!("Invalid expression"),
    }
}


fn is_def(s: &Sexp) -> bool {
  match s {
      Sexp::List(def_vec) => match &def_vec[..] {
          [Sexp::Atom(S(keyword)), Sexp::List(_), _] if keyword == "fun" => true,
          _ => false
      }
      _ => false,
  }
}

fn parse_definition(s: &Sexp, fun_env: &mut HashMap<String, i32>) -> Definition {
  let keywords = vec!["let", "add1", "sub1", "if", "loop", "break", "block", "set!", "true", "false","isnum", "isbool", "print", "input", "fun", "nil", "tuple", "index"];
  match s {
      Sexp::List(def_vec) => match &def_vec[..] {
          [Sexp::Atom(S(keyword)), Sexp::List(name_vec), body] if keyword == "fun" => {
              if name_vec.len() == 0 {
                  panic!("Invalid fundef");
              }
              let funname = match &name_vec[0] {
                Sexp::Atom(S(name)) => name.to_string(),
                _ => panic!("Invalid fundef funname"),
              };
              if keywords.contains(&&funname[..]) {
                  panic!("keyword");
              }
              let mut args = Vec::new();
              for name in &name_vec[1..] {
                  let arg_name = match name {
                    Sexp::Atom(S(name)) => name.to_string(),
                    _ => panic!("Invalid fundef arg"),
                  };
                  if keywords.contains(&&arg_name[..]){
                      panic!("keyword");
                  }
                  if args.contains(&arg_name) {
                      panic!("Invalid duplicate arg name");
                  }
                  args.push(arg_name.to_string());
              }
              if fun_env.contains_key(&funname) {
                  panic!("Invalid duplicate function name");
              }
              fun_env.insert(funname.to_string(), args.len() as i32);
              Fun(funname, args, parse_expr(body))
          },
          _ => panic!("Invalid fundef def_vec"),
      },
      _ => panic!("Invalid fundef s"),
  }
}

fn parse_program(s: &Sexp, fun_env: &mut HashMap<String, i32>) -> Program {
  match s {
      Sexp::List(vec) => {
          let mut defs: Vec<Definition> = vec![];
          for idx in 0..vec.len() {
              let def_or_exp = &vec[idx];
              if is_def(def_or_exp) {
                  defs.push(parse_definition(def_or_exp, fun_env));
              } else {
                  if idx != vec.len() - 1 {
                      panic!("Invalid multiple bodies");
                  }
                  return Program {
                      defs: defs,
                      main: parse_expr(def_or_exp),
                  };
              }
          }
          panic!("Invalid Only found definitions");
      }
      _ => panic!("Invalid Program should be a list")
  }
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}


fn depth(e: &Expr) -> i32 {
  match e {
      Expr::Number(_) => 0,
      Expr::True => 0,
      Expr::False => 0,
      Expr::Nil => 0,
      Expr::UnOp(_,expr) => depth(expr),
      Expr::BinOp(Op2::Arith(_),expr1, expr2) => depth(expr2).max(depth(expr1) + 1),
      Expr::Let(binds, expr) => {
        let mut max_depth = 0;
        // enumearte through binds with index and bind
        for (i, bind) in binds.iter().enumerate() {
          max_depth = max_depth.max(depth(&bind.1) + i as i32);
        }
        max_depth.max(depth(expr) + binds.len() as i32)
      }
      Expr::Id(_) => 0,
      Expr::BinOp(Op2::Cond(_) , e1, e2) => depth(e1).max(depth(e2) + 1),
      Expr::If(expr1, expr2, expr3) => depth(expr1).max(depth(expr2)).max(depth(expr3)),
      Expr::Loop(expr) => depth(expr),
      Expr::Block(exprs) => exprs.iter().map(|expr| depth(expr)).max().unwrap_or(0),
      Expr::Break(expr) => depth(expr),
      Expr::Set(_, expr) => depth(expr),
      Expr::Call(_, args) => {
        let mut max_depth = args.len() as i32;
        // enumearte through binds with index and bind
        for (i, arg) in args.iter().enumerate() {
          max_depth = max_depth.max(depth(arg) + i as i32);
        }
        max_depth
      },
      Expr::Index(expr1, expr2) => depth(expr2).max(depth(expr1) + 1),
      Expr::Tuple(exprs) => {
        let mut max_depth = exprs.len() as i32;
        for (i, expr) in exprs.iter().enumerate() {
            max_depth = max_depth.max(depth(expr) + i as i32);
          }
        max_depth
      }
  }
}



// fn compile_to_instrs(e: &Expr, si: i32, env: &HashMap<String, i32>, brake: &String, l: &mut i32, fun_env: &mut HashMap<String, i32>, is_def: bool) -> String  {
fn compile_to_instrs(e: &Expr, si: i32, env: &HashMap<String, i32>, ctx: &mut CompilationContext) -> String {
    let cur_is_tail = ctx.is_tail;
    ctx.is_tail = false;
    match e {   
        Expr::Number(n) => {
            let base:i64 = 2;
            if n >= &base.pow(62) || n < &(-base.pow(62)){
                panic!("Invalid")
            }
            else {
                format!("mov rax, {}", *n * 2)
            }
        }
        Expr::Id(s) if s == "input" => 
        {
          if ctx.is_def {
            panic!("Invalid input in def");
          }
          format!("  mov rax, rdi")
        }
        Expr::Id(s) => {
            if !env.contains_key(s) {
                panic!("Unbound variable identifier {s}");
            }

            let offset = env.get(s).unwrap() * 8;
            format!("mov rax, [rsp + {offset}]")
        }
        Expr::UnOp(Op1::Add1,subexpr) => {
            let e1_instrs = compile_to_instrs(subexpr, si, env, ctx);
            format!("
  {e1_instrs}
  test rax, 1
  mov rdx, 1
  jnz throw_error
  add rax, 2
  mov rdx, 2
  jo throw_error")
        }
        Expr::UnOp(Op1::Sub1,subexpr) => {
            let e1_instrs = compile_to_instrs(subexpr,si, env, ctx);
            format!("
  {e1_instrs}
  test rax, 1
  mov rdx, 1
  jnz throw_error
  sub rax, 2
  mov rdx, 2
  jo throw_error")
        }
        Expr::UnOp(Op1::IsNum,subexpr) => {
            compile_to_instrs(subexpr, si, env, ctx) + "\n  and rax, 1\n  cmp rax, 0\n  mov rbx, 3\n  mov rax, 1\n  cmove rax,rbx"
        }
        Expr::UnOp(Op1::IsBool,subexpr) => {
            compile_to_instrs(subexpr, si, env, ctx) + "\n  or rax, 0\n  cmp rax, 1\n  mov rbx, 3\n  mov rax, 1\n  cmove rax,rbx"
        }
        Expr::UnOp(Op1::Print,subexpr) => {
          let e_is = compile_to_instrs(subexpr, si, env, ctx);
          let index = if si % 2 == 1 { si + 2 } else { si + 1 };
          let offset = index * 8;
          format!("
{e_is}
sub rsp, {offset}
mov [rsp], rdi
mov rdi, rax
call snek_print
mov rdi, [rsp]
add rsp, {offset}
")
}
        Expr::Set(name, val) => {
          let offset = env.get(name).expect(&format!("Unbound variable identifier {name}")) * 8;

          let save = format!("mov [rsp + {offset}], rax");
          let val_is = compile_to_instrs(val, si, env, ctx);
          format!("
{val_is}
{save}")
      }
        Expr::BinOp(Op2::Arith(op), e1, e2) => {
            let e1_instrs = compile_to_instrs(e1, si+1, env, ctx);
            let e2_instrs = compile_to_instrs(e2, si, env, ctx);
            let stack_offset = si * 8;
            let arith_instr = match op {
                ArithType::Plus => format!("add rax, [rsp + {stack_offset}]"),
                ArithType::Minus => format!("sub rax, [rsp + {stack_offset}]"),
                ArithType::Times => format!("sar rax, 1\n  imul rax, [rsp + {stack_offset}]"),
            };
            
            format!("
  {e2_instrs}
  test rax, 1
  mov rdx, 1
  jnz throw_error
  mov [rsp + {stack_offset}], rax
  {e1_instrs}
  test rax, 1
  mov rdx, 1
  jnz throw_error
  {arith_instr}
  mov rdx, 2
  jo throw_error")
        }
        Expr::Let(vec , body) => {
            let mut instrs = String::new();
            let mut count = 0;
            let mut sub_env = env.clone();
            // create a set and check if there are duplicate bindings
            let mut set = HashSet::new();
            for binding in vec {
                let name = &binding.0;
                let val = &binding.1;
                if set.contains(name) {
                    panic!("Duplicate binding");
                }
                else {
                    set.insert(name);
                    match val {
                        Expr::Tuple(es) => {
                            let arr_len = es.len() as i32;
                            ctx.arr_env.insert(name.to_string(), arr_len);
                        }
                        _ => {}
                    }
                    let val_is = compile_to_instrs(val, si+count, &sub_env, ctx);
                    instrs = instrs + "\n" + &val_is;
                    // check if hashmap contains name, if so, panic "Duplicate binding" error:
                    let stack_offset = (si + count) * 8;
                    instrs = instrs + &format!("\n  mov [rsp + {stack_offset}], rax");
                    sub_env.insert(name.to_string(), si + count);
                    count += 1;
                }
            }
            instrs + "\n" + &compile_to_instrs(body, si+count, &sub_env, ctx)
        }
        Expr::False => format!("mov rax, {}", 3),
        Expr::True => format!("mov rax, {}", 7),
        Expr::Nil => format!("mov rax, {}", 1),
        Expr::Break(e) => {
            if ctx.brake == "" {
                panic!("break");
            }
            ctx.is_tail = cur_is_tail;
            let e_is = compile_to_instrs(e, si, env, ctx);
            format!("
  {e_is}
  jmp {}",ctx.brake)
        }
        Expr::Loop(e) => {
            let startloop = new_label(ctx.l, "loop");
            let endloop = new_label(ctx.l, "loopend");
            let mut new_ctx = CompilationContext {
                brake: &endloop,
                l: ctx.l,
                fun_env: ctx.fun_env,
                is_def: ctx.is_def,
                arr_env: ctx.arr_env,
                is_tail: ctx.is_tail,
                arg_offset: ctx.arg_offset,
              };
            let e_is = compile_to_instrs(e, si, env, &mut new_ctx);
            format!("
  {startloop}:
    {e_is}
    jmp {startloop}
  {endloop}:")
        }
        Expr::Block(es) => {
            es.into_iter().map(|e| { compile_to_instrs(e, si, env, ctx) }).collect::<Vec<String>>().join("\n")
        }
        Expr::BinOp(Op2::Cond(cond) , e1, e2) => {
            let skip_label = new_label(ctx.l, "skip");
            let cond_instrs = match cond {
                ConditionType::Equal => format!("jne {skip_label}"),
                ConditionType::Less => format!("jge {skip_label}"),
                ConditionType::Greater => format!("jle {skip_label}"),
                ConditionType::LessEqual => format!("jg {skip_label}"),
                ConditionType::GreaterEqual => format!("jl {skip_label}"),

            };
            let e1_instrs = compile_to_instrs(e1, si, env, ctx);
            let e2_instrs = compile_to_instrs(e2, si+1, env, ctx);
            let offset = si * 8;
            format!("
  {e1_instrs}
  mov [rsp + {offset}], rax
  {e2_instrs}
  mov rbx, rax
  xor rbx, [rsp + {offset}]
  test rbx, 1
  mov rdx, 1
  jne throw_error
  cmp [rsp + {offset}], rax 
  mov rbx, 3
  mov rax, 1
  {cond_instrs}
  mov rax, rbx
  {skip_label}:")
        }
        Expr::If(cond, thn, els) => {
            let end_label = new_label(ctx.l, "ifend");
            let else_label = new_label(ctx.l, "ifelse");
            let cond_instrs = compile_to_instrs(cond, si, env, ctx);
            ctx.is_tail = cur_is_tail;
            let thn_instrs = compile_to_instrs(thn, si, env, ctx);
            let els_instrs = compile_to_instrs(els, si, env, ctx);
            format!("
  {cond_instrs}
  cmp rax, 1
  je {else_label}
  {thn_instrs}
  jmp {end_label}
  {else_label}:
    {els_instrs}
  {end_label}:")
        }
        Expr::Call(name, args) => {
            let mut instrs = String::new();
            let arg_len = args.len() as i32;
            if arg_len != *ctx.fun_env.get(name).expect(&format!("Invalid function not found {name}")) {
                panic!("Invalid Wrong number of arguments expected {} got {}", ctx.fun_env.get(name).unwrap(), arg_len);
            }
            let mut offset = (arg_len+1) * 8;
            for (i,arg) in args.iter().enumerate() {
                let stack_offset = (si + i as i32) * 8;
                let arg_is = compile_to_instrs(arg, si + i as i32, env, ctx);
                instrs = instrs + "\n" + &arg_is + "\n" + &format!("mov [rsp+{stack_offset}], rax");
            }
            ctx.is_tail = cur_is_tail;
            if ctx.is_tail {
                if offset == ctx.arg_offset {
                    let rdi_offset = arg_len * 8;
                    instrs = instrs + &format!("
    mov [rsp+{rdi_offset}], rdi
    call {name}
    mov rdi, [rsp+{rdi_offset}]");
                    return instrs;
                }
                offset = offset - ctx.arg_offset;
            }
            instrs = instrs + &format!("\nsub rsp, {offset}\n");
            for i in 0..arg_len {
                let origin = (si + i as i32) * 8 + offset;
                let dest = i * 8;
                instrs = instrs + &format!("
    mov rbx, [rsp+{origin}]
    mov [rsp+{dest}], rbx
                ");
            }
            let rdi_offset = arg_len * 8;
            instrs = instrs + &format!("
    mov [rsp+{rdi_offset}], rdi
    call {name}
    mov rdi, [rsp+{rdi_offset}]");
            instrs = instrs + &format!("\nadd rsp, {offset}\n");
            instrs
        },
        Expr::Index(e1, e2) => {
            let e1_instrs = compile_to_instrs(e1, si+1, env, ctx);
            let e2_instrs = compile_to_instrs(e2, si, env, ctx);
            let offset = si * 8;
            format!("
    {e2_instrs}
    mov [rsp + {offset}], rax
    {e1_instrs}
    mov rbx, rax
    cmp rbx, 7
    mov rdx, 4
    jle throw_error
    and rbx, 1
    cmp rbx, 1
    mov rdx, 4
    jne throw_error
    mov rbx, [rax-1]
    cmp rbx, [rsp + {offset}]
    mov rdx, 3
    jle throw_error
    mov rbx, [rsp+{offset}]
    imul rbx, 4
    add rbx, 7
    mov rax, [rax+rbx]
            ")
        },
        Expr::Tuple(es) => {
            let mut instrs = String::new();
            let arr_len = es.len() as i32;
            for (i,e) in es.iter().enumerate() {
                let e_is = compile_to_instrs(e, si + i as i32, env, ctx);
                let offset = i * 8;
                instrs = instrs + "\n" + &e_is + "\n" + &format!("mov [r15+{offset}], rax");
            }
            instrs = instrs + &format!("
            mov rax, r15
            add rax, 1
            add r15, {}
            ", arr_len*8);
            instrs
        },
    }
    
}




fn compile_program(p: &Program, fun_env: &mut HashMap<String, i32>) -> (String, String) {
  let mut labels : i32 = 0;
  let mut defs : String = String::new();
  let mut dep = depth(&p.main) + 2;
  if dep % 2 == 1 {
      dep += 1;
  }
  let offset = dep * 8;
  for def in &p.defs[..] {
    defs.push_str(&compile_definition(&def, &mut labels, fun_env));
  }
  let mut ctx = CompilationContext {
    brake: &String::from(""),
    l: &mut labels,
    fun_env: fun_env,
    is_def: false,
    arr_env: &mut HashMap::new(),
    is_tail: false,
    arg_offset: 0,
  };
  let main = compile_to_instrs(&p.main, 0, &mut HashMap::new(), &mut ctx);
  let main_with_offset = format!("
  sub rsp, {offset}
  {main}
  add rsp, {offset}
  ");
  (defs, main_with_offset)
}



fn compile_definition(d: &Definition, labels: &mut i32, fun_env: &mut HashMap<String, i32>) -> String {
  match d {
      Fun(name, args, body) => {
          let dep = depth(body);
          let offset = dep * 8;
          let mut body_env = hashmap! {};
          let arg_len = args.len() as i32;
          for (i,arg) in args.iter().enumerate() {
              body_env.insert(arg.to_string(), dep + (i as i32) + 1);
          }
          let mut ctx = CompilationContext {
            brake: &String::from(""),
            l: labels,
            fun_env: fun_env,
            is_def: true,
            arr_env: &mut HashMap::new(),
            is_tail: true,
            arg_offset: (arg_len+1) * 8,
            };
          let body_is = compile_to_instrs(body, 0, &mut body_env, &mut ctx);
          format!(
              "{name}:
  sub rsp, {offset}
  {body_is}
  add rsp, {offset}
  ret
              "
          )
      }
  }
}



fn main() -> std::io::Result<()> {
  let args: Vec<String> = env::args().collect();

  let in_name = &args[1];
  let out_name = &args[2];

  let mut in_file = File::open(in_name)?;
  let mut in_contents = String::new();
  in_file.read_to_string(&mut in_contents)?;

  let prog = "(".to_owned() + &in_contents + ")";
  let mut fun_env = HashMap::<String,i32>::new();
  let prog = parse_program(&parse(&prog).expect("Invalid"), &mut fun_env);
  let (defs, main) = compile_program(&prog, &mut fun_env);



  let asm_program = format!(
      "
section .text
global our_code_starts_here
extern snek_error
extern snek_print
throw_error:
  mov rdi, rdx
  push rsp
  call snek_error
  ret
{}
our_code_starts_here:
  mov r15, rsi
  {}
  ret
",
      defs,
      main
  );

  let mut out_file = File::create(out_name)?;
  out_file.write_all(asm_program.as_bytes())?;

  Ok(())
}