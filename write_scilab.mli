(* utility --------------------------------------------------------------------
 *)
class indenter : channel : out_channel -> object
  method close : unit -> unit
  method plus : unit -> unit
  method minus : unit -> unit
  method write : string -> unit 
end





val write_function_definition_list : 
  indenter -> Matlab_language.function_def list -> unit
