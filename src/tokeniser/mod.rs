use regex::Regex;
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
  Definition,
  FunctionArguments,
  AssignmentOperator,
  FunctionDefinition,
  Return,
  IfStatement,
  ElseStatement,
  DefinitionFinish,
  ConsoleLog,
  ImportStatement,
  ClassDefinition,
  ClassFunctionDefinition,
  Word(&'static str),
  LineBreak,
  Literal(String),
  StringLiteral(String),
  SingleLineComment(String),
  // MultiLineComment(String)
}

const MAPPED_TOKENS: &'static [(&'static str, Token)] = &[
  ("twas", Token::Definition),
  ("it required", Token::FunctionArguments),
  ("transmute", Token::AssignmentOperator),
  ("incantation", Token::FunctionDefinition),
  ("shazam", Token::Return),
  ("fancy that", Token::IfStatement),
  ("albeit", Token::ElseStatement),
  ("terminus", Token::DefinitionFinish),
  ("scribe", Token::ConsoleLog),
  ("summon", Token::ImportStatement),
  ("archetype", Token::ClassDefinition),
  ("enchant", Token::ClassFunctionDefinition),
  ("and", Token::Word("and")),
  ("an", Token::Word("an")),
  ("a", Token::Word("a")),
  ("named", Token::Word("named")),
  ("called", Token::Word("called")),
  ("with", Token::Word("with")),
  ("to", Token::Word("to")),
  ("\n", Token::LineBreak)
];

fn lookahead_string (input: &str, current_position: usize, str: &str) -> Option<usize> {
  let len = str.len();

  if current_position + len > input.len() {
    return None
  }

  let chars = &input[current_position..current_position + len];

  if chars == str {
    Some(len)
  } else {
    None
  }
}

fn regex_matcher (
  match_fn: &dyn Fn(String) -> bool,
  input_bytes: &[u8],
  current_position: usize
) -> Option<(String, usize)> {
  let mut bucket: Vec<char> = Vec::new();
  let mut i = current_position;

  loop {
    if i >= input_bytes.len() {
      break;
    }

    let token = input_bytes[i] as char;
    let token_matched = match_fn(token.to_string());

    if token_matched == false {
      break;
    }

    bucket.push(token);

    i += 1;
  }

  let bucket_iter = bucket.into_iter();
  let bucket_len = bucket_iter.clone().len();
  let bucket_string = bucket_iter.collect();

  return Some(( bucket_string, bucket_len ));
}

fn match_literal (input_bytes: &[u8], current_position: usize) -> Option<(String, usize)> {
  lazy_static! {
    static ref LITERAL_REGEX: Regex = Regex::new("[a-zA-Z]").unwrap();
    static ref LITERAL_REGEX_NEXT: Regex = Regex::new("[a-zA-Z0-9\\.]").unwrap();
  }

  let current_token = input_bytes[current_position] as char;

  let first_matched = LITERAL_REGEX.is_match(&current_token.to_string());

  if first_matched == false {
    return None
  }

  let m = |s: String| { LITERAL_REGEX_NEXT.is_match(s.as_str()) };

  return regex_matcher(&m, input_bytes, current_position);
}

fn match_string (input_bytes: &[u8], current_position: usize) -> Option<(String, usize)> {
  lazy_static! {
    static ref STRING_LITERAL_MATCHER: Regex = Regex::new("[^']").unwrap();
  }

  let m = |s: String| { STRING_LITERAL_MATCHER.is_match(s.as_str()) };

  return regex_matcher(&m, input_bytes, current_position);
}

fn match_single_comment (input_bytes: &[u8], current_position: usize) -> Option<(String, usize)> {
  lazy_static! {
    static ref COMMENT_MATCHER: Regex = Regex::new("[^\n]").unwrap();
  }

  let m = |s: String| { COMMENT_MATCHER.is_match(s.as_str()) };

  return regex_matcher(&m, input_bytes, current_position);
}

// fn match_multi_comment (input_bytes: &[u8], current_position: usize) -> Option<(String, usize)> {
//   lazy_static! {
//     static ref COMMENT_MATCHER: Regex = Regex::new("(?!\\*\\/)").unwrap();
//   }
  
//   let m = |s: String| { COMMENT_MATCHER.is_match(s.as_str()) };

//   return regex_matcher(&m, input_bytes, current_position);
// }

pub fn tokenize (input: &str) -> Result<Vec<Token>, String> {
  let input_bytes = input.as_bytes();
  let mut tokens: Vec<Token> = Vec::new();

  let mut current_position: usize = 0;

  'outer: while current_position < input_bytes.len() {
    let current_token = input_bytes[current_position];
    let current_token_char = current_token as char;

    if current_token_char == ' ' {
      // Skip blank space
      current_position += 1;
      continue;
    }

    let do_lookahead_string = move |x| lookahead_string(input, current_position, x);

    for (s, m) in MAPPED_TOKENS {
      let result = do_lookahead_string(*s);

      // We matched
      if result.is_some() {
        let len = result.unwrap();
        tokens.push(m.clone());
        current_position += len;
        continue 'outer;
      }
    }

    let did_match_literal = match_literal(input_bytes, current_position);

    if did_match_literal.is_some() {
      let (string, length) = did_match_literal.unwrap();
      tokens.push(Token::Literal(string));
      current_position += length;
      continue 'outer;
    }

    // If our current token is ', try
    // and match a string literal.
    if current_token_char == '\'' {
      // Skip our current char
      current_position += 1;

      let did_match_string_literal = match_string(input_bytes, current_position);

      if did_match_string_literal.is_some() {
        let (string, length) = did_match_string_literal.unwrap();
        tokens.push(Token::StringLiteral(string));
        current_position += length;

        // Add an additional position
        // increment as we want to skip
        // the last '.
        current_position += 1;

        continue 'outer;
      }
    }

    // If our current token is /, try
    // and parse a comment out of it.
    if current_token_char == '/' {
      current_position += 1;
      // Check what our next character is,
      // this will influence single vs multiline.

      let next_char = input_bytes[current_position] as char;

      if next_char == '/' {
        current_position += 1;

        let did_match_comment = match_single_comment(input_bytes, current_position);

        if did_match_comment.is_some() {
          let (string, length) = did_match_comment.unwrap();
          tokens.push(Token::SingleLineComment(string));
          current_position += length;
  
          continue 'outer;
        }
      } else if next_char == '*' {
        // current_position += 1;

        // let did_match_comment = match_multi_comment(input_bytes, current_position);

        // if did_match_comment.is_some() {
        //   let (string, length) = did_match_comment.unwrap();
        //   tokens.push(Token::MultiLineComment(string));
        //   current_position += length;
  
        //   continue 'outer;
        // }
      }
    }

    println!("Unknown token to parse: {:?}", current_token_char);

    return Err(format!("unknown token: {:?}", current_token_char));
  };

  Ok(tokens)
}

#[cfg(test)]
mod test {
  use super::{Token, MAPPED_TOKENS, tokenize};

  #[test]
  fn test_mapped_tokens () {
    for (s, t) in MAPPED_TOKENS {
      let result = tokenize(*s).unwrap();
      assert_eq!(result.len(), 1);
      assert_eq!(result.first().unwrap(), t);
    }
  }

  #[test]
  fn test_literal_parsing () {
    let result = tokenize("hello").unwrap().first().unwrap().clone();
    let correct = Token::Literal("hello".to_owned());
    assert_eq!(result, correct);
  }

  #[test]
  fn test_string_parsing () {
    let result = tokenize("'world'").unwrap().first().unwrap().clone();
    let correct = Token::StringLiteral("world".to_owned());
    assert_eq!(result, correct);
  }

  #[test]
  fn test_single_comment_parsing () {
    let result = tokenize("// test comment").unwrap().first().unwrap().clone();
    let correct = Token::SingleLineComment(" test comment".to_owned());
    assert_eq!(result, correct);
  }

  #[test]
  fn test_complex_1 () {
    let input = r#"
      twas an incantation named foobar
        it required a name and a location
        scribe name
        scribe location
        shazam five
      terminus
    "#;

    let result = tokenize(input).unwrap();
    let correct: Vec<Token> = vec![
      Token::LineBreak,
      Token::Definition,
      Token::Word("an"),
      Token::FunctionDefinition,
      Token::Word("named"),
      Token::Literal("foobar".to_owned()),
      Token::LineBreak,
      Token::FunctionArguments,
      Token::Word("a"),
      Token::Literal("name".to_owned()),
      Token::Word("and"),
      Token::Word("a"),
      Token::Literal("location".to_owned()),
      Token::LineBreak,
      Token::ConsoleLog,
      Token::Literal("name".to_owned()),
      Token::LineBreak,
      Token::ConsoleLog,
      Token::Literal("location".to_owned()),
      Token::LineBreak,
      Token::Return,
      Token::Literal("five".to_owned()),
      Token::LineBreak,
      Token::DefinitionFinish,
      Token::LineBreak
    ];

    assert_eq!(result, correct);
  }

  #[test]
  fn test_complex_2 () {
    let input = r#"
      archetype Wizard
      enchant Wizard with an incantation named say
        it required a name and a sentence
        scribe 'Hello ' name ': ' sentence
      terminus
      twas a Wizard named Hagrid
      Hagrid cast say 'harry' 'youre a wizard now'
    "#;

    let result = tokenize(input).unwrap();
    let correct: Vec<Token> = vec![
      Token::LineBreak,
      Token::ClassDefinition,
      Token::Literal("Wizard".to_owned()),
      Token::LineBreak,
      Token::ClassFunctionDefinition,
      Token::Literal("Wizard".to_owned()),
      Token::Word("with"),
      Token::Word("an"),
      Token::FunctionDefinition,
      Token::Word("named"),
      Token::Literal("say".to_owned()),
      Token::LineBreak,
      Token::FunctionArguments,
      Token::Word("a"),
      Token::Literal("name".to_owned()),
      Token::Word("and"),
      Token::Word("a"),
      Token::Literal("sentence".to_owned()),
      Token::LineBreak,
      Token::ConsoleLog,
      Token::StringLiteral("Hello ".to_owned()),
      Token::Literal("name".to_owned()),
      Token::StringLiteral(": ".to_owned()),
      Token::Literal("sentence".to_owned()),
      Token::LineBreak,
      Token::DefinitionFinish,
      Token::LineBreak,
      Token::Definition,
      Token::Word("a"),
      Token::Literal("Wizard".to_owned()),
      Token::Word("named"),
      Token::Literal("Hagrid".to_owned()),
      Token::LineBreak,
      Token::Literal("Hagrid".to_owned()),
      Token::Literal("cast".to_owned()),
      Token::Literal("say".to_owned()),
      Token::StringLiteral("harry".to_owned()),
      Token::StringLiteral("youre a wizard now".to_owned()),
      Token::LineBreak
    ];

    assert_eq!(result, correct);
  }
}
