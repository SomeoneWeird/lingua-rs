#![allow(incomplete_features)]
#![feature(let_chains)]

#[macro_use]
extern crate lazy_static;

mod tokeniser;
mod ast;

const INPUT: &'static str = r#"
  twas a frog named fred
  transmute fred to 'F R E D'
  shazam frog
"#;

fn main () -> Result<(), String> {
  let tokens = tokeniser::tokenize(INPUT)?;
  let ast = ast::to_ast(&tokens)?;

  println!("input: {:?}", INPUT);
  println!("tokens: {:?}", tokens);
  println!("ast: {:?}", ast);

  Ok(())
}
