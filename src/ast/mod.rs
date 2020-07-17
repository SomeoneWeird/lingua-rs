use super::tokeniser::Token;

#[derive(Debug)]
pub struct ProgramOpts {
  body: Vec<Node>
}

#[derive(Debug)]
pub struct ValueOpts {
  value: String
}

#[derive(Debug)]
pub struct DefinitionOpts {
  r#type: String,
  name: String
}

#[derive(Debug)]
pub struct ReturnOpts {
  value: Box<Option<Node>>
}

#[derive(Debug)]
pub struct AssignmentOpts {
  name: String,
  value: Box<Node>
}

#[derive(Debug)]
pub enum Node {
  Program(ProgramOpts),
  Definition(DefinitionOpts),
  Literal(ValueOpts),
  StringLiteral(ValueOpts),
  Return(ReturnOpts),
  Assignment(AssignmentOpts)
}

fn process_token (tokens: &Vec<Token>, idx: &mut usize) -> Result<Option<Node>, String> {
  let current_token = &tokens[*idx];

  return match current_token {
    Token::Definition => {
      // TODO: check oob
      let type_node: &Token = &tokens[*idx + 1];
      let name_node: &Token = &tokens[*idx + 2];
  
      return if let (
        Token::Literal(type_string),
        Token::Literal(name_string)
      ) = (
        type_node,
        name_node
      ) {
        // Skip self, then 2 other tokens.
        *idx += 3;
        Ok(Some(Node::Definition(DefinitionOpts {
          r#type: type_string.clone(),
          name: name_string.clone()
        })))
      } else {
        Err("definition type & value must be literals".to_owned())
      }
    },
    Token::Literal(s) => {
      *idx += 1;
      Ok(Some(Node::Literal(ValueOpts { value: s.clone() })))
    },
    Token::StringLiteral(s) => {
      *idx += 1;
      Ok(Some(Node::StringLiteral(ValueOpts { value: s.clone() })))
    },
    Token::Return => {
      let next_token = &tokens[*idx + 1];

      *idx += 1;

      return if let Token::LineBreak = next_token {
        Ok(Some(Node::Return(ReturnOpts {
          value: Box::new(None)
        })))
      } else {
        match process_token(&tokens, &mut *idx) {
          Ok(result) => Ok(Some(Node::Return(ReturnOpts {
            value: Box::new(result)
          }))),
          Err(e) => Err(e)
        }
      }
    },
    Token::LineBreak | Token::SingleLineComment(_) => {
      // Skip
      *idx += 1;
      Ok(None)
    },
    Token::IfStatement => {
      // TODO
      *idx += 1;
      Ok(None)
    },
    Token::AssignmentOperator => {
      *idx += 1;

      // TODO: check oob
      let name_token = &tokens[*idx];

      *idx += 1;

      return if let Token::Literal(name_string) = name_token {
        return match process_token(&tokens, &mut *idx) {
          Ok(maybe_result) => {
            if maybe_result.is_none() {
              return Err("invalid assignment value".to_owned())
            }

            let result = maybe_result.unwrap();

            // TODO: check if node is valid assignment (literal or string)
            Ok(Some(Node::Assignment(AssignmentOpts {
              name: name_string.clone(),
              value: Box::new(result)
            })))
          },
          Err(e) => Err(e)
        }
      } else {
        Err("assignment name must be literal".to_owned())
      }
    }
    // TODO: better handling here, bubble up?
    _ => Err(format!("UNKNOWN TOKEN: {:?}", current_token))
  };
}

pub fn to_ast (tokens: &Vec<Token>) -> Result<Node, String> {
  // Filter out all words from our tokens
  let mut filtered_tokens: Vec<Token> = tokens.clone();
  filtered_tokens.retain(|t| !matches!(t, Token::Word(_)));

  let mut body: Vec<Node> = Vec::new();
  let mut idx = 0;

  while idx < filtered_tokens.len() {
    let processed = process_token(&filtered_tokens, &mut idx);

    match processed {
      Ok(node) => {
        if node.is_some() {
          body.push(node.unwrap())
        }
      },
      Err(e) => panic!(e)
    }
  }

  let root_node_opts = ProgramOpts {
    body
  };

  let root_node = Node::Program(root_node_opts);

  Ok(root_node)
}
