%{
  open Matlab_language
%}

%token <string> COMMENT
%token <string> WHITESPACE
%token NEWLINE EOF 

/* arithmetic operators */
%token PLUS MINUS UNARY_MINUS
%token ELWISE_TIMES ELWISE_DIV ELWISE_BACK_DIV ELWISE_POWER
%token TIMES DIV BACK_DIV POWER
%token RANGE 

%token REAL_TRANSPOSE CONJ_TRANSPOSE

/* relational operators */
%token LESS_THAN LESS_EQUAL GREATER_THAN GREATER_EQUAL EQUAL NOT_EQUAL

/* logical operators */
%token ELWISE_AND ELWISE_OR SHORTCIRC_AND SHORTCIRC_OR
%token LOG_NEGATION

/* symbols */
%token OPEN_PAREN CLOSE_PAREN 
%token OPEN_BRACKET CLOSE_BRACKET
%token OPEN_BRACE CLOSE_BRACE
%token COMMA SEMICOLON
%token ASSIGN

/* keywords */
%token <string> IDENTIFIER
%token FOR END SWITCH CASE OTHERWISE WHILE IF ELSE ELSEIF
%token CONTINUE BREAK TRY CATCH RETURN GLOBAL FUNCTION

/* literals */
%token <string> STRING_LITERAL
%token <string> NUMBER_LITERAL




/* table of precedence, lowest first */
%nonassoc ASSIGN
%left SHORTCIRC_OR
%left SHORTCIRC_AND
%left ELWISE_OR
%left ELWISE_AND
%left LESS_THAN LESS_EQUAL GREATER_THAN GREATER_EQUAL EQUAL NOT_EQUAL
%left RANGE
%left PLUS MINUS      
%left TIMES DIV BACK_DIV ELWISE_TIMES ELWISE_DIV ELWISE_BACK_DIV
%nonassoc UNARY_MINUS UNARY_MINUS LOG_NEGATION
%left POWER ELWISE_POWER
%nonassoc REAL_TRANSPOSE CONJ_TRANSPOSE
%left OPEN_PAREN

/* the entry point */
%start script_file function_file
%type <Matlab_language.instruction list> script_file
%type <Matlab_language.function_def list> function_file




%%
function_file:
  | function_definitions EOF { $1 }
;

function_definitions:
  | function_definition { [ $1 ] }
  | function_definition function_definitions { $1 :: $2 }
;

script_file:
  | instruction_list EOF { $1 }
;

instruction_list:
  | /* empty */ { [ ] }
  | instruction instruction_ends instruction_list { $1 :: $3 }
;

instruction:
  | variable_assignment { VariableAssignment( $1 ) }
  | IDENTIFIER script_command_arguments { ScriptCommand( $1, $2 ) }
  | expression { Expression( $1 ) }
  | FOR variable_assignment instruction_ends_opt
    instruction_list END { For( $2, $4 ) }
  | FOR OPEN_PAREN variable_assignment CLOSE_PAREN instruction_ends_opt
    instruction_list END { For( $3, $6 ) }
  | IF condition
      instruction_list 
    else_block_opt
    { IfElse( $2, $3, $4 ) }
  | WHILE condition
      instruction_list
    END { While( $2, $3 ) }
  | SWITCH condition
    case_list 
    otherwise_opt { Switch( $2, $3, $4 ) }
  | TRY instruction_ends 
      instruction_list
    CATCH instruction_ends
      instruction_list
    END { Try( $3, $6 ) }
  | CONTINUE { Continue }
  | BREAK { Break }
  | RETURN { Return }
  | GLOBAL unseparated_identifier_list { Global( $2 ) }
  | COMMENT { Comment( $1 ) }
;

function_definition:
  | FUNCTION IDENTIFIER OPEN_PAREN identifier_list CLOSE_PAREN instruction_ends
      instruction_list
    { { name = $2; input_args = $4; output_args = []; body = $7 } }
  | FUNCTION function_result ASSIGN IDENTIFIER OPEN_PAREN identifier_list
    CLOSE_PAREN instruction_ends
      instruction_list
    { { name = $4; input_args = $6; output_args = $2; body = $9 } }
;

function_result:
  | IDENTIFIER { [ $1 ] }
  | OPEN_BRACKET identifier_list CLOSE_BRACKET { $2 }
;

script_command_arguments:
  | IDENTIFIER { [ $1 ] }
  | IDENTIFIER script_command_arguments { $1 :: $2 }
;

variable_assignment:
  | expression ASSIGN expression { ( [ $1 ], $3 ) } 
  | OPEN_BRACKET index_expression_list CLOSE_BRACKET ASSIGN expression { ( $2, $5 ) } 
;

else_block_opt:
  | END { [] }
  | ELSE instruction_list END { $2 }
  | ELSEIF condition
      instruction_list
      else_block_opt { [ IfElse( $2, $3, $4 ) ] }
;

case_list:
  | CASE condition instruction_list case_list { ( $2, $3 ) :: $4 }
  | /* nothing */ { [ ] }
;

otherwise_opt:
  | OTHERWISE instruction_ends instruction_list END { $3 }
  | END { [ ] }
;

condition:
  | expression instruction_ends { $1 }
  | expression { $1 }
;

identifier_list:
  | /* nothing */ { [ ] }
  | IDENTIFIER { [ $1 ] }
  | IDENTIFIER COMMA identifier_list { $1 :: $3 }
;
    
unseparated_identifier_list:
  | /* nothing */ { [ ] }
  | IDENTIFIER unseparated_identifier_list { $1 :: $2 }
;

expression:
  | NUMBER_LITERAL { NumberLiteral( $1 ) }
  | STRING_LITERAL { StringLiteral( String.sub $1 1 ((String.length $1) -2 )) }
  | OPEN_PAREN expression CLOSE_PAREN { Parenthesized( $2 ) }
  | IDENTIFIER { Identifier( $1 ) }

  /* arithmetic operators */
  | MINUS expression %prec UNARY_MINUS { UnaryPrefixExpression( "-", $2 ) }
  | expression PLUS expression { BinaryExpression( $1 , "+", $3  ) }
  | expression MINUS expression { BinaryExpression( $1 , "-", $3  ) }
  | expression ELWISE_TIMES expression { BinaryExpression( $1, ".*", $3 ) }
  | expression ELWISE_DIV expression { BinaryExpression( $1, "./", $3 ) }
  | expression ELWISE_BACK_DIV expression { BinaryExpression( $1, ".\\", $3 ) }
  | expression ELWISE_POWER expression { BinaryExpression( $1, ".^", $3 ) }
  | expression TIMES expression { BinaryExpression( $1, "*", $3 ) }
  | expression DIV expression { BinaryExpression( $1, "/", $3 ) }
  | expression BACK_DIV expression { BinaryExpression( $1, "\\", $3 ) }
  | expression POWER expression { BinaryExpression( $1, "^", $3 ) }
  | expression RANGE expression { BinaryExpression( $1, ":", $3 ) }

  | expression CONJ_TRANSPOSE { UnaryPostfixExpression( $1, "'" ) }
  | expression REAL_TRANSPOSE { UnaryPostfixExpression( $1, ".'" ) }

  /* relational operators */
  | expression LESS_THAN expression { BinaryExpression( $1, "<", $3 ) }
  | expression LESS_EQUAL expression { BinaryExpression( $1, "<=", $3 ) }
  | expression GREATER_THAN expression { BinaryExpression( $1, ">", $3 ) }
  | expression GREATER_EQUAL expression { BinaryExpression( $1, ">=", $3 ) }
  | expression EQUAL expression { BinaryExpression( $1, "==", $3 ) }
  | expression NOT_EQUAL expression { BinaryExpression( $1, "~=", $3 ) }
  
  /* logical operators */
  | expression ELWISE_AND expression { BinaryExpression( $1, "&", $3 ) }
  | expression ELWISE_OR expression { BinaryExpression( $1, "|", $3 ) }
  | expression SHORTCIRC_AND expression { BinaryExpression( $1, "&&", $3 ) }
  | expression SHORTCIRC_OR expression { BinaryExpression( $1, "||", $3 ) }

  /* function call, array subscript */
  | expression OPEN_PAREN index_expression_list CLOSE_PAREN 
    { FunctionCallOrArraySubscript( $1, $3 ) }

  /* matrix building */
  | OPEN_BRACKET matrix_literal { Matrix( $2 ) }
  | OPEN_BRACE cell_array_literal { CellArray( $2 ) }
;

index_expression:
  | END { End }
  | RANGE { WholeVector }

  | NUMBER_LITERAL { NumberLiteral( $1 ) }
  | STRING_LITERAL { StringLiteral( String.sub $1 1 ((String.length $1) -2 )) }
  | OPEN_PAREN index_expression CLOSE_PAREN { Parenthesized( $2 ) }
  | IDENTIFIER { Identifier( $1 ) }

  /* arithmetic operators */
  | MINUS index_expression %prec UNARY_MINUS { UnaryPrefixExpression( "-", $2 ) }
  | index_expression PLUS index_expression { BinaryExpression( $1 , "+", $3  ) }
  | index_expression MINUS index_expression { BinaryExpression( $1 , "-", $3  ) }
  | index_expression ELWISE_TIMES index_expression { BinaryExpression( $1, ".*", $3 ) }
  | index_expression ELWISE_DIV index_expression { BinaryExpression( $1, "./", $3 ) }
  | index_expression ELWISE_BACK_DIV index_expression { BinaryExpression( $1, ".\\", $3 ) }
  | index_expression ELWISE_POWER index_expression { BinaryExpression( $1, ".^", $3 ) }
  | index_expression TIMES index_expression { BinaryExpression( $1, "*", $3 ) }
  | index_expression DIV index_expression { BinaryExpression( $1, "/", $3 ) }
  | index_expression BACK_DIV index_expression { BinaryExpression( $1, "\\", $3 ) }
  | index_expression POWER index_expression { BinaryExpression( $1, "^", $3 ) }
  | index_expression RANGE index_expression { BinaryExpression( $1, ":", $3 ) }

  | index_expression CONJ_TRANSPOSE { UnaryPostfixExpression( $1, "'" ) }
  | index_expression REAL_TRANSPOSE { UnaryPostfixExpression( $1, ".'" ) }

  /* relational operators */
  | index_expression LESS_THAN index_expression { BinaryExpression( $1, "<", $3 ) }
  | index_expression LESS_EQUAL index_expression { BinaryExpression( $1, "<=", $3 ) }
  | index_expression GREATER_THAN index_expression { BinaryExpression( $1, ">", $3 ) }
  | index_expression GREATER_EQUAL index_expression { BinaryExpression( $1, ">=", $3 ) }
  | index_expression EQUAL index_expression { BinaryExpression( $1, "==", $3 ) }
  | index_expression NOT_EQUAL index_expression { BinaryExpression( $1, "~=", $3 ) }
  
  /* logical operators */
  | index_expression ELWISE_AND index_expression { BinaryExpression( $1, "&", $3 ) }
  | index_expression ELWISE_OR index_expression { BinaryExpression( $1, "|", $3 ) }
  | index_expression SHORTCIRC_AND index_expression { BinaryExpression( $1, "&&", $3 ) }
  | index_expression SHORTCIRC_OR index_expression { BinaryExpression( $1, "||", $3 ) }

  /* function call, array subscript */
  | index_expression OPEN_PAREN index_expression_list CLOSE_PAREN 
    { FunctionCallOrArraySubscript( $1, $3 ) }

  /* matrix building */
  | OPEN_BRACKET matrix_literal { Matrix( $2 ) }
  /*
  | OPEN_BRACE cell_array_literal { CellArray( $2 ) }
  */
;

index_expression_list:
  | /* nothing */ { [ ] }
  | index_expression { [ $1 ] }
  | index_expression COMMA index_expression_list { $1 :: $3 }
;

matrix_literal:
  | CLOSE_BRACKET { [ ] }
  | index_expression CLOSE_BRACKET 
    { [ { value = $1; new_row_after = false } ] }
  | index_expression COMMA matrix_literal 
    { { value = $1; new_row_after = false } :: $3 }
  | index_expression SEMICOLON matrix_literal 
    { { value = $1; new_row_after = true } :: $3 }
;

cell_array_literal:
  | CLOSE_BRACE { [ ] }
  | expression CLOSE_BRACE
    { [ { value = $1; new_row_after = false } ] }
  | expression COMMA cell_array_literal 
    { { value = $1; new_row_after = false } :: $3 }
  | expression SEMICOLON cell_array_literal 
    { { value = $1; new_row_after = true } :: $3 }
;

instruction_ends_opt:
  | instruction_ends { }
  | /* nothing */ { }
;
instruction_ends:
  | instruction_end { }
  | instruction_end instruction_ends { }
;

instruction_end:
  | COMMA { }
  | SEMICOLON { }
  | NEWLINE { }
;
