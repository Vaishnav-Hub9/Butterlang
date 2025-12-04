# Code explanation
## A page made so contributing and understanding the system becmes easier

### **lexer.rs**

#### pub enum TokenKind
the public enum TokenKind has all the enum variants such as Literals, Operators, Keywords, Logic, Brackets, and EOF

it is there for sorting the tokens out for the parser effectivly and clean.

#### pub struct Lexer
Contains three fields which are chars, pos, and len. Chars contain all the content of the code and pos has the position or index of where the lexer is currently at. len is there because it is useful to store the length of the code.

#### fn new (Lexer impl)
the function new sets the chars field content to be the source code turned into chars, then so are the other fields are set to be what they are supposed to be initially. It then returns a self struct.

#### fn is_eof (Lexer impl)
the function sees if the position of the lexer is larger or equal to the source code length and returns a bool.

#### fn curr (Lexer impl)
the function returns a Option of char that is in the current index or an error.

#### fn next (Lexer impl)
the function returns a Option of char that is in the current index + 1 or an error.

#### fn bump (Lexer impl)
the function that returns the current char and increments the position by 1.

#### fn skip_whitespace (Lexer impl)
skips the whitespaces

#### fn skip_line_comment (Lexer impl)
skips the line content until it sees a newline character

#### fn ident_or_kw (Lexer impl)
identifies whether the thing is an identifier or not and outputs tokens

#### fn lex_number (Lexer impl)
returns a Float or Int token from the TokenKind enum

#### fn lex_string (Lexer impl)
returns a String token from the TokenKind enum

#### fn next_token (Lexer impl)
goes to the next token and returns the correct token of current

#### fn lex
loops throught the source code and returns the finished tokens vector

### **parser.rs**
