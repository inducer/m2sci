{
  open Matlab_grammar

  let process_bareword word =
    match word with 
    | "for" -> FOR
    | "end" -> END
    | "switch" -> SWITCH
    | "case" -> CASE
    | "otherwise" -> OTHERWISE
    | "while" -> WHILE
    | "if" -> IF
    | "else" -> ELSE
    | "elseif" -> ELSEIF
    | "continue" -> CONTINUE
    | "break" -> BREAK
    | "try" -> TRY
    | "catch" -> CATCH
    | "return" -> RETURN
    | "global" -> GLOBAL
    | "function" -> FUNCTION
    | id -> IDENTIFIER( id )
}

let identifier = ['a'-'z' 'A'-'Z''_']['0'-'9''a'-'z''A'-'Z''_']*
let whitespace = [' ''\t']+
let digit = [ '0' - '9' ]
let digit_sequence = digit +
let fraction = digit_sequence? '.' digit_sequence | digit_sequence
let exponent = 'e' [ '+' '-' ]? digit_sequence
let number = fraction exponent?

rule lex_matlab =
  parse
    | '%' [^ '\n' ]* { COMMENT( Lexing.lexeme lexbuf ) }
    | whitespace { WHITESPACE( Lexing.lexeme lexbuf ) }
    | '\n'| "\r\n" | "\n\r" { NEWLINE }
    | "...\n" { WHITESPACE( Lexing.lexeme lexbuf ) }
    | eof { EOF }

    (* literals *)
    | number { NUMBER_LITERAL( Lexing.lexeme lexbuf ) }
    (* FIXME HACK! *)
    | '\'' [^'\'''\n']* '\'' { STRING_LITERAL( Lexing.lexeme lexbuf ) }

    (* arithmetic operators *)
    | '+' { PLUS }
    | '-' { MINUS }
    | ".*" { ELWISE_TIMES }
    | "./" { ELWISE_DIV }
    | ".\\" { ELWISE_BACK_DIV }
    | ".^" { ELWISE_POWER }
    | "*" { TIMES }
    | "/" { DIV }
    | "\\" { BACK_DIV }
    | "^" { POWER }
    | ":" { RANGE }

    | "'" { CONJ_TRANSPOSE }
    | ".'" { REAL_TRANSPOSE }

    (* relational operators *)
    | "<" { LESS_THAN }
    | "<=" { LESS_EQUAL }
    | ">" { GREATER_THAN }
    | ">=" { GREATER_EQUAL }
    | "==" { EQUAL }
    | "~=" { NOT_EQUAL }

    (* logical operators *)
    | "&" { ELWISE_AND }
    | "|" { ELWISE_OR }
    | "&&" { SHORTCIRC_AND }
    | "||" { SHORTCIRC_OR }
    | "~" { LOG_NEGATION }

    (* symbols *)
    | '(' { OPEN_PAREN }
    | ')' { CLOSE_PAREN }
    | '[' { OPEN_BRACKET }
    | ']' { CLOSE_BRACKET }
    | '{' { OPEN_BRACE }
    | '}' { CLOSE_BRACE }
    | ',' { COMMA }
    | ';' { SEMICOLON }
    | '=' { ASSIGN }

    (* keywords *)
    | identifier { process_bareword( Lexing.lexeme lexbuf ) }




