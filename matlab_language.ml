type matrix_entry =
  {
    value : expression;
    new_row_after : bool
  }
and expression = 
  BinaryExpression of  expression * string * expression
| UnaryPostfixExpression of expression * string
| UnaryPrefixExpression of  string * expression
| Parenthesized of expression
| NumberLiteral of string 
| StringLiteral of string
| FunctionCallOrArraySubscript of expression * expression list
| Matrix of matrix_entry list 
| CellArray of matrix_entry list 
| Identifier of string
| WholeVector
| End
| NoExpression

type variable_assignment = expression list * expression

type instruction =
  VariableAssignment of variable_assignment
| FunctionDefinition of function_def
| For of variable_assignment * instruction list
| IfElse of expression * instruction list * instruction list 
| Switch of expression * ( expression * instruction list ) list * (* otherwise *) instruction list
| While of expression * instruction list
| Try of instruction list * instruction list
| Continue | Break | Return | Global of string list
| Expression of expression
| ScriptCommand of string * string list
| Nop
| Comment of string
and function_def =
  {
    name : string; 
    input_args : string list;
    output_args : string list;
    body : instruction list 
  }
  


