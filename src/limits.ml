(** Compiler phase to calculate the maximum number of locals and temporary stack
    locations needed for each method. *)

module CAst = Codegenerationast
module LAst = Limitsast
module Inst = Instruction

module LabelMap = Map.Make(String)

(* ********************************************************************** *)
(* ERROR MESSAGE                                                          *)
(* ********************************************************************** *)

let verify_error message =
  raise (Error.InternalCompilerError message)

(* ********************************************************************** *)
(* LIMITS TRAVERSAL                                                       *)
(* ********************************************************************** *)

type stackinfo = { maxstack : int;
		   stackmap : int LabelMap.t }
    
(*  limit_formals_and_body : CAst.formals_and_body -> LAst.formals_and_body  *)
let limit_formals_and_body fab =
  let formals = fab.CAst.formals in
  let body    = fab.CAst.body in

  (* find maximum local by iterating over the instructions of the body *)
  let mymax = 
    let rec find_max_local is max = match is with
      | [] -> max
      | i::is -> 
	find_max_local is (match Inst.local_access i with
	  | None -> max
	  | Some l -> if l+1 > max then l+1 else max)
    in 
    find_max_local body (List.length formals + 1) in

  (* label map that associates instruction sequences to labels *)
  let lmap = 
    let rec build_label_map is lmap = match is with
      | [] -> lmap
      | i::is -> build_label_map is (match i with
	  | Inst.Ilabel l -> (LabelMap.add l is lmap)
	  | _ -> lmap)
    in
    build_label_map body LabelMap.empty in

  (* graph traversal of control-flow graph *)
  let rec traverse is stack stackinfo = match is with
    | [] -> stackinfo
    | i::is -> 
      (match i with
	| Inst.Ilabel l ->
	  let stackinfo' = 
	    if LabelMap.mem l stackinfo.stackmap
	    then let height = LabelMap.find l stackinfo.stackmap in
		 (if height != stack
		  then verify_error ("Stack height does not match at " ^ l ^ ": (" ^
 			     (string_of_int height) ^ " != " ^ (string_of_int stack) ^ ")")
		  else (); stackinfo)
	    else { stackinfo with stackmap = LabelMap.add l stack stackinfo.stackmap } in
	  traverse is stack stackinfo'
	| _ -> 
	  let stack' = stack + (Inst.stack_change i) in
	  let stackinfo' =
	    { stackinfo with
	      maxstack = if stack' > stackinfo.maxstack then stack' else stackinfo.maxstack } in

	  (if stack' < 0 
	   then
	      verify_error ("Negative stack height at " ^ (Inst.to_asm i) ^
				 " (" ^ (string_of_int stack') ^ ")")
	   else ();
	   let stackinfo'' = 
	     if Inst.can_fall_through i  (* then continue along path *)
	     then traverse is stack' stackinfo'
	     else stackinfo' in
	   (match Inst.can_jump i with   (* then explore alternative path *)
	     | None -> stackinfo''
	     | Some l ->
	       if LabelMap.mem l stackinfo''.stackmap
	       then
		 let height = LabelMap.find l stackinfo''.stackmap in (* been here: check consistency *)
		 (if height != stack'
		  then verify_error ("Stack height does not match at " ^ l ^ ": (" ^
 			  (string_of_int height) ^ " != " ^ (string_of_int stack') ^ ")")
		  else (); stackinfo'')
	       else
		 let is' = LabelMap.find l lmap in   (* first time here: "color" l *)
		 let stackinfo''' =
		   { stackinfo'' with stackmap = LabelMap.add l stack' stackinfo''.stackmap } in
		 traverse is' stack' stackinfo''')))
  in

  let stackinfo = traverse body 0 { maxstack = 0; stackmap = LabelMap.empty } in

  { LAst.formals      = formals;
    LAst.body         = body;
    LAst.max_stack    = stackinfo.maxstack;
    LAst.max_locals   = mymax  }


(*  limit_formals_and_body_opt : CAst.formals_and_body option -> LAst.formals_and_body option  *)
let limit_formals_and_body_opt fab = match fab with
  | None -> None
  | Some fab ->
    let fab' = limit_formals_and_body fab in
    Some fab'

(*  limit_field_decl : CAst.field_decl -> LAst.field_decl  *)
let limit_field_decl fdecl = (* identity function *sigh* *)
    { LAst.field_type      = fdecl.CAst.field_type;
      LAst.field_name      = fdecl.CAst.field_name;
      LAst.field_signature = fdecl.CAst.field_signature }

(*  limit_field_decls : CAst.field_decl list -> LAst.field_decl list  *)
let rec limit_field_decls fdecls = match fdecls with
  | [] -> []
  | fdecl::fdecls ->
    let fdecl' = limit_field_decl fdecl in
    let fdecls' = limit_field_decls fdecls in
    fdecl'::fdecls'

(*  limit_method_decl : CAst.method_decl -> LAst.method_decl  *)
let limit_method_decl mdecl =
  let fab' = limit_formals_and_body mdecl.CAst.method_formals_and_body in
  { LAst.method_return_type      = mdecl.CAst.method_return_type;
    LAst.method_name             = mdecl.CAst.method_name;
    LAst.method_formals_and_body = fab';
    LAst.method_signature        = mdecl.CAst.method_signature }

(*  limit_method_decls : CAst.method_decl list -> LAst.method_decl list  *)
let rec limit_method_decls mdecls = match mdecls with
  | [] -> []
  | mdecl::mdecls ->
    let mdecl'  = limit_method_decl mdecl in
    let mdecls' = limit_method_decls mdecls in
    mdecl'::mdecls'

(*  limit_constructor_decl : CAst.constructor_decl -> LAst.constructor_decl  *)
let limit_constructor_decl cdecl =
  let fab' = limit_formals_and_body cdecl.CAst.constructor_formals_and_body in
  { LAst.constructor_name             = cdecl.CAst.constructor_name;
    LAst.constructor_formals_and_body = fab';
    LAst.constructor_signature        = cdecl.CAst.constructor_signature }

(*  limit_class_decl : CAst.class_decl -> LAst.class_decl  *)
let limit_class_decl cdecl =
  let methods' = limit_method_decls cdecl.CAst.class_methods in
  let fields'  = limit_field_decls cdecl.CAst.class_fields in
  let cdecl'   = limit_constructor_decl cdecl.CAst.class_constructor in
  let main'    = limit_formals_and_body_opt cdecl.CAst.class_main in
  { LAst.class_name           = cdecl.CAst.class_name;
    LAst.class_fields         = fields';
    LAst.class_constructor    = cdecl';
    LAst.class_main           = main';
    LAst.class_methods        = methods';
    LAst.class_decl_signature = cdecl.CAst.class_decl_signature }

(*  limit_source_file : CAst.source_file -> LAst.source_file  *)
let limit_source_file src_file =
  let tdecl' = limit_class_decl src_file.CAst.source_file_decl in
  { LAst.source_file_name = src_file.CAst.source_file_name;
    LAst.source_file_decl = tdecl' }

(*  limit_program : CAst.program -> LAst.program  *)
let limit_program prog = List.map limit_source_file prog
