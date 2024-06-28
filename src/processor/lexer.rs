use crate::utils::error::CompilerError;

/// A trait which categorises characters.
trait CharacterCategoriser {
    fn is_operation(&self) -> bool;
}

impl CharacterCategoriser for char {
    fn is_operation(&self) -> bool {
        matches!(self, '!' | '~' | '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '=' | '>' | '<')
    }
}

#[derive(Debug)]
pub enum Operation {
    // UNARY
    Negate,
    BitwiseNegate,

    // BINARY
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    // todo bitwise shifts
    Assign,

    // CONDITIONAL
    And,
    Or,
    GreaterThan,
    GEQ,
    LessThan,
    LEQ,
    Equivalence
}

#[derive(Debug)]
pub enum Loop {
    For,
    While
}

#[derive(Debug)]
pub enum ControlFlow {
    Return,
    Break,
    Continue
}

#[derive(Debug, PartialEq)]
pub enum NumericType {
    Integer,
    Float
}

#[derive(Debug)]
pub enum TokenType {
    /// The end of the line.
    EndOfLine,

    /// A numerical literal.
    Number(NumericType),
    /// A sequence of characters.
    String,
    /// A truthy or falsy value.
    Boolean,
    /// A variable type.
    Type,
    
    /// An operation which takes one value.
    Unary(Operation),
    /// An operation which takes two values.
    Binary(Operation),
    /// An operation which returns true on a condition.
    Conditional(Operation),

    /// A variable declaration token, where `bool` represents mutability.
    VariableDeclaration(bool),
    /// A function declaration.
    FunctionDeclaration,
    /// A class declaration.
    ClassDeclaration,
    /// Overrides a method in a class.
    Override,

    If,
    Else,

    /// A loop initiator.
    Loop(Loop),
    /// Control flow in a loop/function/etc.
    ControlFlow(ControlFlow),
    /// A user-defined word.
    Identifier,

    /// Throws an error.
    Throw,

    OpenParenthesis,
    CloseParenthesis,
    OpenBracket,
    CloseBracket,
    OpenCurlyBracket,
    CloseCurlyBracket,    
    Comma,
    Colon,
    Returns
}

#[derive(Debug)]
pub struct Token {
    /// The true value in source.
    pub value: String,
    /// The type of token.
    pub token_type: TokenType
}

impl Token {
    pub fn new(value: String, token_type: TokenType) -> Self {
        Token { value, token_type }
    }
}

pub struct Lexer {
    /// The source program (in chars).
    pub source: Vec<char>,
    /// The line the lexer is reading.
    pub line: usize,
    /// The character the lexer is reading.
    pub index: usize,

    /// A list of tokens from the source program.
    pub tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(program: String) -> Self {
        let source: Vec<char> = program.chars().collect();
        
        Lexer {
            source,
            line: 1,
            index: 0,
            tokens: Vec::new()
        }
    }

    /// Peeks at the next character.
    fn peek(&self) -> Result<&char, CompilerError> {
        self.source.get(self.index + 1).ok_or(CompilerError::UnexpectedEOF)
    }

    /// Parses an operation symbol.
    fn parse_operation(&mut self) -> Result<Token, CompilerError> {
        let mut operator = self.source[self.index].to_string();

        match operator.as_str() {
            "!" => Ok(Token::new(operator, TokenType::Unary(Operation::Negate))),
            "~" => Ok(Token::new(operator, TokenType::Unary(Operation::BitwiseNegate))),
            "+" => Ok(Token::new(operator, TokenType::Binary(Operation::Add))),
            "-" => { 
                let next = self.peek()?;
                if *next == '>' {
                    operator.push(*next);
                    self.index += 1;
                    Ok(Token::new(operator, TokenType::Returns))
                } else {
                    Ok(Token::new(operator, TokenType::Binary(Operation::Subtract)))
                }
            },
            "*" => Ok(Token::new(operator, TokenType::Binary(Operation::Multiply))),
            "/" => Ok(Token::new(operator, TokenType::Binary(Operation::Divide))),
            "%" => Ok(Token::new(operator, TokenType::Binary(Operation::Modulo))),
            "&" => {
                let next = self.peek()?;
                if *next == '&' {
                    operator.push(*next);
                    self.index += 1;
                    Ok(Token::new(operator, TokenType::Conditional(Operation::And)))
                } else {
                    Ok(Token::new(operator, TokenType::Binary(Operation::BitwiseAnd)))
                }
            },
            "|" => {
                let next = self.peek()?;
                if *next == '|' {
                    operator.push(*next);
                    self.index += 1;
                    Ok(Token::new(operator, TokenType::Conditional(Operation::Or)))
                } else {
                    Ok(Token::new(operator, TokenType::Binary(Operation::BitwiseOr)))
                }
            },
            "^" => Ok(Token::new(operator, TokenType::Binary(Operation::BitwiseXor))),
            "=" => {
                let next = self.peek()?;
                if *next == '=' {
                    operator.push(*next);
                    self.index += 1;
                    Ok(Token::new(operator, TokenType::Conditional(Operation::Equivalence)))
                } else {
                    Ok(Token::new(operator, TokenType::Binary(Operation::Assign)))
                }
            },
            ">" => {
                let next = self.peek()?;
                if *next == '=' {
                    operator.push(*next);
                    self.index += 1;
                    Ok(Token::new(operator, TokenType::Conditional(Operation::GEQ)))
                } else {
                    Ok(Token::new(operator, TokenType::Conditional(Operation::GreaterThan)))
                }
            },
            "<" => {
                let next = self.peek()?;
                if *next == '=' {
                    operator.push(*next);
                    self.index += 1;
                    Ok(Token::new(operator, TokenType::Conditional(Operation::LEQ)))
                } else {
                    Ok(Token::new(operator, TokenType::Conditional(Operation::LessThan)))
                }
            },
            _ => Err(CompilerError::UnidentifiedError(operator, self.line))
        }
    }

    /// Parses a numerical literal.
    fn parse_number(&mut self) -> Result<Token, CompilerError> {
        let mut numeric_type = NumericType::Integer;

        let mut value = self.source[self.index].to_string();
        while self.peek()?.is_numeric() || *self.peek()? == '.' {
            if numeric_type != NumericType::Float &&  *self.peek()? == '.' {
                numeric_type = NumericType::Float;
            }

            self.index += 1;
            value.push(self.source[self.index]);
        }

        Ok(Token::new(value, TokenType::Number(numeric_type)))
    }

    /// Parses an alphabetical token.
    fn parse_word(&mut self) -> Result<Token, CompilerError> {
        let mut word = self.source[self.index].to_string();
        while self.peek()?.is_alphanumeric() || *self.peek()? == '_' {
            self.index += 1;
            word.push(self.source[self.index]);
        }

        match word.as_str() {
            "int" | "float" | "bool" | "string" | "void" | "array" | "hashmap" => Ok(Token::new(word, TokenType::Type)),
            "let" => Ok(Token::new(word, TokenType::VariableDeclaration(true))),
            "const" => Ok(Token::new(word, TokenType::VariableDeclaration(false))),
            "class" => Ok(Token::new(word, TokenType::ClassDeclaration)),
            "override" => Ok(Token::new(word, TokenType::Override)),
            "true" | "false" => Ok(Token::new(word, TokenType::Boolean)),
            "fn" => Ok(Token::new(word, TokenType::FunctionDeclaration)),
            "for" => Ok(Token::new(word, TokenType::Loop(Loop::For))),
            "while" => Ok(Token::new(word, TokenType::Loop(Loop::While))),
            "return" => Ok(Token::new(word, TokenType::ControlFlow(ControlFlow::Return))),
            "break" => Ok(Token::new(word, TokenType::ControlFlow(ControlFlow::Break))),
            "continue" => Ok(Token::new(word, TokenType::ControlFlow(ControlFlow::Continue))),
            "if" => Ok(Token::new(word, TokenType::If)),
            "else" => Ok(Token::new(word, TokenType::Else)),
            "throw" => Ok(Token::new(word, TokenType::Throw)), 
            _ => Ok(Token::new(word, TokenType::Identifier))
        }
    }

    /// Parses a symbol.
    fn parse_symbol(&mut self) -> Result<Token, CompilerError> {
        let symbol = self.source[self.index].to_string();
        
        match symbol.as_str() {
            ";" => Ok(Token::new(symbol, TokenType::EndOfLine)),
            "(" => Ok(Token::new(symbol, TokenType::OpenParenthesis)),
            ")" => Ok(Token::new(symbol, TokenType::CloseParenthesis)),
            "[" => Ok(Token::new(symbol, TokenType::OpenBracket)),
            "]" => Ok(Token::new(symbol, TokenType::CloseBracket)),
            "{" => Ok(Token::new(symbol, TokenType::OpenCurlyBracket)),
            "}" => Ok(Token::new(symbol, TokenType::CloseCurlyBracket)),
            "," => Ok(Token::new(symbol, TokenType::Comma)),
            ":" => Ok(Token::new(symbol, TokenType::Colon)),
            "\"" => {
                let mut string = self.source[self.index].to_string();

                loop {
                    let char = match self.peek().copied() {
                        Ok(c) => c,
                        Err(e) => return Err(e),
                    };
                
                    if char == '\n' {
                        return Err(CompilerError::UnexpectedNewline(self.line));
                    } else {
                        self.index += 1;
                        string.push(char);
                
                        if char == '"' {
                            break;
                        }
                    }
                }

                Ok(Token::new(string, TokenType::String))
            }
            _ => Err(CompilerError::UnidentifiedError(symbol, self.line))
        }
    }

    /// Converts a source program into a vector of tokens.
    pub fn tokenise_program(&mut self) -> Result<(), CompilerError> {
        while let Some(char) = self.source.get(self.index) {
            if char.is_whitespace() {
                if *char == '\n' {
                    self.line += 1;
                }
            } else if *char == '#' {
                while let Ok(c) = self.peek() {
                    if *c == '\n' { break; }
                    else { self.index += 1; }
                }
            } else if char.is_operation() {
                let token = self.parse_operation()?;
                self.tokens.push(token);
            } else if char.is_numeric() {
                let token = self.parse_number()?;
                self.tokens.push(token);
            } else if char.is_alphabetic() {
                let token = self.parse_word()?;
                self.tokens.push(token);
            } else {
                let token = self.parse_symbol()?;
                self.tokens.push(token);
            }

            self.index += 1;
        }

        Ok(())
    }
}