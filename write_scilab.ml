open Matlab_language





(* utility --------------------------------------------------------------------
 *)
class indenter ~channel = object(self)
  val channel = channel
  val mutable depth = 0

  method close () =
    close_out channel

  method plus () =
    depth <- depth + 2

  method minus () =
    depth <- depth - 2

  method write line =
    output_string channel ((String.make depth ' ') ^ line);
    output_char channel '\n'
end





let join ~conversion ~list ?separator ()=
  let realsepfunc = match separator with
    | None -> (fun x -> ", " )
    | Some( sepfunc ) -> sepfunc
  in
  let rec join_internal list =
    match list with
    | head :: [] ->
        (conversion head)
    | head :: tail ->
        (conversion head) ^ (realsepfunc head) ^ (join_internal tail)
    | [] -> ""
  in
  join_internal list




let rec sublist list start_index length =
  if length = 0 then
    []
  else
    match list with
    | head :: tail ->
        if start_index == 0 then
          head :: (sublist tail start_index (length - 1 ))
        else
          sublist tail (start_index - 1) length
    | [] -> []




(* conversion -----------------------------------------------------------------
*)
let map_binary_operator expr1 op expr2 =
  match op with
  (* FIXME will these two work in all cases? *)
  | "&&" -> BinaryExpression( expr1, "&", expr2 )
  | "||" -> BinaryExpression( expr1, "|", expr2 )
  | _ -> BinaryExpression( expr1, op, expr2 )

let map_unary_postfix_operator expr op =
  UnaryPostfixExpression( expr, op)

let map_unary_prefix_operator op expr =
  UnaryPrefixExpression( op, expr )

let map_identifier id =
  match id with
  | "i" -> Identifier( "%i" )
  | "pi" -> Identifier( "%pi" )
  | "inf" -> Identifier( "%inf" )
  (* extension *)
  | "true" -> Identifier( "%t" )
  | "false" -> Identifier( "%f" )
  (* end extension *)
  | "nargin" -> 
      FunctionCallOrArraySubscript( 
        Identifier( "argn" ), [ NumberLiteral( "2" ) ] )
  | "nargout" -> 
      FunctionCallOrArraySubscript( 
        Identifier( "argn" ), [ NumberLiteral( "1" ) ] )
  | "clf" -> 
      FunctionCallOrArraySubscript( 
        Identifier( "xclear" ), [ ] )
  | _ -> Identifier( id )

let map_function_application name arg_list =
  match name with
  | "all" -> 
      FunctionCallOrArraySubscript( 
        Identifier( "and" ), arg_list )
  | "any" -> 
      FunctionCallOrArraySubscript( 
        Identifier( "or" ), arg_list )
  | "isinf" -> 
      BinaryExpression( List.hd arg_list, "==", Identifier( "%inf" ) )
  | "finite" -> 
      BinaryExpression( List.hd arg_list, "~=", Identifier( "%inf" ) )
  | "isnan" -> 
      BinaryExpression( List.hd arg_list, "~=", List.hd arg_list )
  | "isstr" -> 
      BinaryExpression( 
        FunctionCallOrArraySubscript(
          Identifier( "type" ), arg_list), "==", NumberLiteral( "10" ) )
  | "clf" -> 
      FunctionCallOrArraySubscript( 
        Identifier( "xclear" ), [ ] )
  | "fprintf" -> 
      begin
      match List.hd arg_list with
        | StringLiteral( str ) ->
            FunctionCallOrArraySubscript( 
              Identifier( "printf" ), arg_list )
        | _ ->
            FunctionCallOrArraySubscript( 
              Identifier( "fprintf" ), arg_list )
      end
  | "eig" -> 
      FunctionCallOrArraySubscript( 
        Identifier( "m2sci_eig" ), arg_list )
  | "ones" | "zeros" | "eye" -> 
      if List.length arg_list = 1 then
        match List.hd arg_list with
        | FunctionCallOrArraySubscript( Identifier( "size" ), sub_arg_list ) ->
            (* utilize direct like-size functionality in scilab *)
            FunctionCallOrArraySubscript( 
              Identifier( name ), sub_arg_list )
        | _ ->
          FunctionCallOrArraySubscript( 
            (* FIXME this breaks if List.hd arg_list has any side-effects *)
            Identifier( name ), [ List.hd arg_list; List.hd arg_list ] )
      else
        (* leave it as it is *)
        FunctionCallOrArraySubscript( 
          Identifier( name ), arg_list )
  | "fft" -> 
      FunctionCallOrArraySubscript( 
        Identifier( "mtlb_fft" ), arg_list )
  | "sort" -> 
      FunctionCallOrArraySubscript( 
        Identifier( "m2sci_sort" ), arg_list )
  | "plot" -> 
      (* only the first two arguments of plot are portable *)
      FunctionCallOrArraySubscript( 
        Identifier( "mtlb_plot" ), (sublist arg_list 0 2) )
  | "figure" -> 
      FunctionCallOrArraySubscript( 
        Identifier( "xset" ), StringLiteral( "window" ) :: arg_list )
  | "title" -> 
      FunctionCallOrArraySubscript( 
        Identifier( "xtitle" ),  [ (List.hd arg_list) ] )
  | "xlabel" -> 
      FunctionCallOrArraySubscript( 
        Identifier( "xtitle" ),  
        [ StringLiteral( "" ); (List.hd arg_list) ] );
  | "ylabel" -> 
      FunctionCallOrArraySubscript( 
        Identifier( "xtitle" ),  
        [ StringLiteral( "" ); StringLiteral( "" ); (List.hd arg_list) ] );
  | "max" -> 
      FunctionCallOrArraySubscript( 
        Identifier( "max" ),  
          (List.map 
            (fun x -> FunctionCallOrArraySubscript( Identifier( "real" ), [ x ] ) ) 
            arg_list ) )
  | "legend" -> 
      (* FIXME There is no sensible translation *)
      NoExpression
  | _ -> 
      FunctionCallOrArraySubscript( 
        Identifier( name ), arg_list )

let map_script_command name arg_list =
  match name with 
  | "hold" -> 
      ScriptCommand( "mtlb_hold", arg_list )
  | _ -> 
      ScriptCommand( name, arg_list )





(* expressions ----------------------------------------------------------------
 *)
let rec stringify_expression expr =
  match expr with 
  | BinaryExpression( expr1, op, expr2 ) -> 
      stringify_expression_no_map (map_binary_operator expr1 op expr2)
  | UnaryPostfixExpression( expr, op ) -> 
      stringify_expression_no_map (map_unary_postfix_operator expr op) 
  | UnaryPrefixExpression( op, expr ) -> 
      stringify_expression_no_map (map_unary_prefix_operator op expr) 
  | FunctionCallOrArraySubscript( expr, arglist ) ->
      begin
        match expr with
        | Identifier( str ) ->
            stringify_expression_no_map (map_function_application str arglist) 
        | _ ->
            stringify_expression_no_map expr
      end
  | Identifier( idstr ) -> stringify_expression_no_map (map_identifier idstr)
  | _ -> stringify_expression_no_map expr

and stringify_expression_no_map expr =
  match expr with 
  | BinaryExpression( expr1, op, expr2 ) -> 
      Printf.sprintf "%s %s %s" 
        (stringify_expression expr1) op
        (stringify_expression expr2)
  | UnaryPostfixExpression( expr, op ) -> 
      Printf.sprintf "%s%s" (stringify_expression expr) op
  | UnaryPrefixExpression( op, expr ) -> 
      Printf.sprintf "%s%s" op (stringify_expression expr)
  | Parenthesized( expr ) -> 
      Printf.sprintf "( %s )" 
        (stringify_expression expr )
  | NumberLiteral( numstr ) -> numstr
  | StringLiteral( strlit ) -> 
      Printf.sprintf "'%s'" strlit
  | FunctionCallOrArraySubscript( expr, arglist ) ->
      begin
        match expr with
        | Identifier( id ) ->
            Printf.sprintf "%s( %s )" 
              id (join ~conversion:stringify_expression ~list:arglist ())
        | _ ->
          Printf.sprintf "%s( %s )" 
          (stringify_expression expr)
          (join ~conversion:stringify_expression ~list:arglist ())
      end
  | Matrix( entry_list ) ->
      let generate_matrix_separator entry =
          if entry.new_row_after then
            "; "
          else
            ", "
      in
      Printf.sprintf "[ %s ]" 
      (join ~conversion:(fun x-> stringify_expression x.value) ~list:entry_list
            ~separator:generate_matrix_separator ())
  | CellArray( entry_list ) ->
      let generate_cell_array_separator entry =
          if entry.new_row_after then
            "; "
          else
            ", "
      in
      Printf.sprintf "[ %s ]" 
      (join ~conversion:(fun x-> stringify_expression x.value) ~list:entry_list
            ~separator:generate_cell_array_separator ())
  | Identifier( idstr ) -> idstr
  | WholeVector -> ":"
  | End -> "$"
  | NoExpression -> ""




let stringify_variable_assignment ( exlist, rvalue ) =
  if List.length exlist = 1 then
    Printf.sprintf "%s = %s" 
      (stringify_expression (List.hd exlist))
      (stringify_expression rvalue)
  else
    Printf.sprintf "[ %s ] = %s" 
    (join ~conversion:stringify_expression ~list:exlist ())
    (stringify_expression rvalue)




let rec write_function_definition indenter fdef =
  let result = 
    if List.length fdef.output_args = 0 then
      ""
    else
      Printf.sprintf "[ %s ] = " 
      (join ~conversion:(fun x->x) ~list:fdef.output_args ())
  in
  indenter#write 
    (Printf.sprintf "function %s%s( %s )"
      result
      fdef.name
      (join ~conversion:(fun x->x) ~list:fdef.input_args ()));
  indenter#plus();
  write_instruction_list indenter fdef.body;
  indenter#minus();
  indenter#write "endfunction"

and write_instruction indenter insn =
  match insn with 
  | ScriptCommand( name, arg_list ) ->
      write_instruction_no_map indenter (map_script_command name arg_list)
  | _ -> write_instruction_no_map indenter insn

and write_instruction_no_map indenter insn =
  match insn with
  | VariableAssignment( ( exlist, rvalue ) ) ->
      indenter#write ( (stringify_variable_assignment (exlist, rvalue)) ^ ";" )
  | FunctionDefinition( fdef ) ->
      write_function_definition indenter fdef
  | For( assignment, inslist ) ->
      indenter#write (Printf.sprintf "for %s" 
        (stringify_variable_assignment assignment ));
      indenter#plus();
      write_instruction_list indenter inslist;
      indenter#minus();
      indenter#write "end";
  | IfElse( expression, ifpart, elsepart ) ->
      let rec write_else_part elsepart =
        match elsepart with
        | [ IfElse( expr, ifpart, elsepart ) ] ->
            indenter#write (Printf.sprintf "elseif %s" 
              (stringify_expression expr));
            indenter#plus();
            write_instruction_list indenter ifpart;
            indenter#minus();
            write_else_part elsepart
        | _ ->
            indenter#write "else";
            indenter#plus();
            write_instruction_list indenter ifpart;
            indenter#minus();
      in
      indenter#write (Printf.sprintf "if %s" 
        (stringify_expression expression));
      indenter#plus();
      write_instruction_list indenter ifpart;
      indenter#minus();
      indenter#write "end";
      
  | Switch( expression, case_list, otherwise ) ->
      let write_case ( expr, inslist ) =
        indenter#write (Printf.sprintf "case %s" 
          (stringify_expression expr));
        indenter#plus();
        write_instruction_list indenter inslist;
        indenter#minus();
      in
      indenter#write (Printf.sprintf "select %s" 
        (stringify_expression expression ));
      List.iter write_case case_list;
      if List.length otherwise <> 0 then begin
        indenter#write "else";
        indenter#plus();
        write_instruction_list indenter otherwise;
        indenter#minus();
      end;
      indenter#write "end";
      ()
  | While( expression, inslist ) ->
      indenter#write (Printf.sprintf "while %s" 
        (stringify_expression expression ));
      indenter#plus();
      write_instruction_list indenter inslist;
      indenter#minus();
      indenter#write "end";
  | Try( tryblock, catchblock ) ->
      failwith "'try' can't be translated yet"
  | Continue ->
      indenter#write "continue;";
  | Break ->
      indenter#write "break;";
  | Return ->
      indenter#write "return;";
  | Global( varlist ) ->
      indenter#write 
        (Printf.sprintf "global %s;"
          (join ~conversion:(fun x->x) ~list:varlist ~separator:(fun x -> " ") ()))
  | Expression( expr ) ->
      let result = stringify_expression expr
      in
      if result <> "" then
        indenter#write result
  | ScriptCommand( name, arglist ) ->
      indenter#write 
        (Printf.sprintf "%s %s;"
          name
          (join ~conversion:(fun x->x) ~list:arglist ~separator:(fun x -> " ") ()))
  | Nop ->
      () 
  | Comment( comment ) ->
      let regexp = Str.regexp "^%[ \t]*"
      in
      let cleaned_comment = Str.global_replace regexp "" comment
      in
      indenter#write 
        (Printf.sprintf "// %s" cleaned_comment)

and write_instruction_list indenter ilist =
  List.iter (write_instruction indenter) ilist

and write_function_definition_list indenter fdeflist =
  List.iter (write_function_definition indenter) fdeflist



