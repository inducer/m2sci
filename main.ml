let line_number = ref 1
in

let lexbuf = Lexing.from_channel stdin
in

let stringify_token token = 
  match token with
  | Matlab_grammar.COMMENT( str ) -> Printf.sprintf "COMMENT: '%s'" str
  | Matlab_grammar.WHITESPACE( str ) -> Printf.sprintf "WHITESPACE: '%s'" str
  | Matlab_grammar.NEWLINE -> "NEWLINE"
  | Matlab_grammar.EOF -> "EOF"
  | Matlab_grammar.PLUS -> "PLUS"
  | Matlab_grammar.MINUS -> "MINUS"
  | Matlab_grammar.UNARY_MINUS-> "UNARY_MINUS"
  | Matlab_grammar.ELWISE_TIMES -> "ELWISE_TIMES"
  | Matlab_grammar.ELWISE_DIV -> "ELWISE_DIV"
  | Matlab_grammar.ELWISE_BACK_DIV -> "ELWISE_BACK_DIV"
  | Matlab_grammar.ELWISE_POWER-> "ELWISE_POWER"
  | Matlab_grammar.TIMES -> "TIMES"
  | Matlab_grammar.DIV -> "DIV"
  | Matlab_grammar.BACK_DIV -> "BACK_DIV"
  | Matlab_grammar.POWER -> "POWER"
  | Matlab_grammar.RANGE -> "RANGE"
  | Matlab_grammar.REAL_TRANSPOSE -> "REAL_TRANSPOSE"
  | Matlab_grammar.CONJ_TRANSPOSE-> "CONJ_TRANSPOSE"
  | Matlab_grammar.LESS_THAN -> "LESS_THAN"
  | Matlab_grammar.LESS_EQUAL -> "LESS_EQUAL"
  | Matlab_grammar.GREATER_THAN -> "GREATER_THAN"
  | Matlab_grammar.GREATER_EQUAL -> "GREATER_EQUAL"
  | Matlab_grammar.EQUAL -> "EQUAL"
  | Matlab_grammar.NOT_EQUAL-> "NOT_EQUAL"
  | Matlab_grammar.ELWISE_AND -> "ELWISE_AND"
  | Matlab_grammar.ELWISE_OR -> "ELWISE_OR"
  | Matlab_grammar.SHORTCIRC_AND -> "SHORTCIRC_AND"
  | Matlab_grammar.SHORTCIRC_OR-> "SHORTCIRC_ON"
  | Matlab_grammar.LOG_NEGATION-> "LOG_NEGATION"
  | Matlab_grammar.OPEN_PAREN -> "OPEN_PAREN"
  | Matlab_grammar.CLOSE_PAREN -> "CLOSE_PAREN"
  | Matlab_grammar.OPEN_BRACKET -> "OPEN_BRACKET"
  | Matlab_grammar.CLOSE_BRACKET-> "CLOSE_BRACKET"
  | Matlab_grammar.OPEN_BRACE -> "OPEN_BRACE"
  | Matlab_grammar.CLOSE_BRACE -> "CLOSE_BRACE"
  | Matlab_grammar.COMMA -> "COMMA"
  | Matlab_grammar.SEMICOLON-> "SEMICOLON"
  | Matlab_grammar.ASSIGN-> "ASSIGN"
  | Matlab_grammar.IDENTIFIER( str ) -> Printf.sprintf "IDENTIFIER: '%s'" str
  | Matlab_grammar.FOR -> "FOR"
  | Matlab_grammar.END -> "END"
  | Matlab_grammar.SWITCH -> "SWITCH"
  | Matlab_grammar.CASE -> "CASE"
  | Matlab_grammar.OTHERWISE -> "OTHERWISE"
  | Matlab_grammar.WHILE -> "WHILE"
  | Matlab_grammar.IF -> "IF"
  | Matlab_grammar.ELSE -> "ELSE"
  | Matlab_grammar.ELSEIF-> "ELSEI"
  | Matlab_grammar.CONTINUE -> "CONTINUE"
  | Matlab_grammar.BREAK -> "BREAK"
  | Matlab_grammar.TRY -> "TRY"
  | Matlab_grammar.CATCH -> "CATCH"
  | Matlab_grammar.RETURN -> "RETURN"
  | Matlab_grammar.GLOBAL -> "GLOBAL"
  | Matlab_grammar.FUNCTION-> "FUNCTION"
  | Matlab_grammar.STRING_LITERAL( str ) -> Printf.sprintf "STRING_LITERAL: %s" str
  | Matlab_grammar.NUMBER_LITERAL( str ) -> Printf.sprintf "NUMBER_LITERAL: '%s'" str
in

let rec parsing_function my_lexbuf =
  let result = Matlab_lexer.lex_matlab my_lexbuf 
  in
  match result with
  | Matlab_grammar.WHITESPACE( "...\n" )  -> 
      line_number := !line_number + 1; 
      parsing_function my_lexbuf
  | Matlab_grammar.WHITESPACE( _ )  -> parsing_function my_lexbuf
  | Matlab_grammar.NEWLINE -> 
      line_number := !line_number + 1; 
      result
  | _ -> 
      result
in
try
  let language_tree = Matlab_grammar.function_file parsing_function lexbuf
  in
  let indenter = new Write_scilab.indenter stdout
  in
  Write_scilab.write_function_definition_list indenter language_tree
with
  Parsing.Parse_error ->
    Printf.printf "parse error around input line %d\n" !line_number
