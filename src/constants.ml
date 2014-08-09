module TAst = Typecheckingast


(*  const_exp : TAst.exp -> TAst.exp *)
let rec const_exp exp = 
(*  let pos = exp.TAst.exp_pos in*)

  let mkexp e = {exp with TAst.exp = e} in
(*  let mkboolexp e = {exp with TAst.exp = e; TAst.exp_type = Types.Boolean} in*)

  match exp.TAst.exp with
  | TAst.Binop (exp1,op,exp2) ->
    let exp1' = const_exp exp1 in
    let exp2' = const_exp exp2 in
    (match (exp1'.TAst.exp, exp2'.TAst.exp) with
      | (TAst.IntConst left, TAst.IntConst right) ->
	(match op with
	  | TAst.Plus   -> mkexp (TAst.IntConst (Int32.add left right))
	  | TAst.Minus  -> mkexp (TAst.IntConst (Int32.sub left right))
	  | TAst.Times  -> mkexp (TAst.IntConst (Int32.mul left right))
	  | TAst.Divide ->
	    if right <> 0l
	    then mkexp (TAst.IntConst (Int32.div left right))
	    else exp
	  | TAst.Modulo ->
	    if right <> 0l
	    then mkexp (TAst.IntConst (Int32.rem left right))
	    else exp
	  | TAst.Eq     -> mkexp (TAst.BooleanConst (left = right))
	  | TAst.Ne     -> mkexp (TAst.BooleanConst (left <> right))
	  | TAst.Lt     -> mkexp (TAst.BooleanConst (left < right))
	  | TAst.Le     -> mkexp (TAst.BooleanConst (left <= right))
	  | TAst.Gt     -> mkexp (TAst.BooleanConst (left > right))
	  | TAst.Ge     -> mkexp (TAst.BooleanConst (left >= right))
	  | _           ->
	    raise (Error.InternalCompilerError "Invalid op for int folding") )
      | (TAst.BooleanConst left, TAst.BooleanConst right) ->
	(match op with
	  | TAst.Eq     -> mkexp (TAst.BooleanConst (left = right))
	  | TAst.Ne     -> mkexp (TAst.BooleanConst (left <> right))
	  | TAst.And    -> mkexp (TAst.BooleanConst (left && right))
	  | TAst.Or     -> mkexp (TAst.BooleanConst (left || right))
	  | TAst.Xor    -> mkexp (TAst.BooleanConst (left <> right))
	  | _           ->
	    raise (Error.InternalCompilerError "Invalid op for boolean folding") )
      | (TAst.StringConst left, TAst.StringConst right) ->
	(match op with
	  | TAst.Concat -> mkexp (TAst.StringConst  (left ^ right))
	  | TAst.Aeq    -> mkexp (TAst.BooleanConst (left = right))
	  | TAst.Ane    -> mkexp (TAst.BooleanConst (left <> right))
	  | _           ->
	    raise (Error.InternalCompilerError "Invalid op for string folding")	)
      | (_,_) -> mkexp (TAst.Binop (exp1',op,exp2')))
  | TAst.Unop (op,exp) ->
    let exp' = const_exp exp in
    (match (op, exp'.TAst.exp) with
      | (TAst.Negate,          TAst.IntConst i)     ->
	mkexp (TAst.IntConst (Int32.neg i))
      | (TAst.Complement,      TAst.BooleanConst b) ->
	mkexp (TAst.BooleanConst (not b))
      | (TAst.BooleanToString, TAst.BooleanConst b) ->
	mkexp (TAst.StringConst (string_of_bool b))
      | (TAst.CharToString,    TAst.IntConst i)     ->
	let i' = Int32.to_int i in
	let c = Char.chr i' in
	let str = String.make 1 c in
	mkexp (TAst.StringConst str)
      | (TAst.IntToString,     TAst.IntConst i)     -> 
	mkexp (TAst.StringConst (Int32.to_string i))
      | (TAst.ObjectToString,  _)
      | (_,                    _) -> 
	mkexp (TAst.Unop (op,exp')) )
  | TAst.StringConst s ->
    mkexp(TAst.StringConst (String.sub s 1 (String.length s - 2))) (*do not include ""*)
  | TAst.IntConst _
  | TAst.BooleanConst _
  | TAst.Null
  | TAst.This -> exp
  | TAst.Invoke (exp,id,exps,mtyp) ->
    let exp'  = const_exp exp in
    let exps' = const_exps exps in
    mkexp (TAst.Invoke (exp',id,exps',mtyp))
  | TAst.New (id,exps,constrtyp) ->
    let exps' = const_exps exps in
    mkexp (TAst.New (id,exps',constrtyp))
  | TAst.Lvalue _ ->
    exp
  | TAst.Assignment (lvalue,exp) ->
    let exp'  = const_exp exp in
    mkexp (TAst.Assignment (lvalue,exp'))
  | TAst.Print exp ->
    let exp'  = const_exp exp in
    mkexp (TAst.Print exp')
  | TAst.Read ->
    exp

(*  const_exps : TAst.exp list -> TAst.exp list *)
and const_exps exps = match exps with
  | [] -> []
  | exp::exps ->
    let exp'  = const_exp exp in
    let exps' = const_exps exps in
    exp'::exps'

(*  const_exp_opt : TAst.exp option -> TAst.exp option *)
let const_exp_opt exp_opt = match exp_opt with
  | None -> None
  | Some exp ->
    let exp' = const_exp exp in
    Some exp'

(*  const_stm : TAst.stm -> TAst.stm *)
let rec const_stm stm = 
  let mkstm s = { stm with TAst.stm = s } in

  match stm.TAst.stm with
  | TAst.Exp exp ->
    let exp' = const_exp exp in
    mkstm (TAst.Exp exp')
  | TAst.IfThen (exp,stm) ->
    let exp' = const_exp exp in
    let stm' = const_stm stm in
    mkstm (TAst.IfThen (exp',stm'))
  | TAst.IfThenElse (exp,stm1,stm2) ->
    let exp'  = const_exp exp in
    let stm1' = const_stm stm1 in
    let stm2' = const_stm stm2 in
    mkstm (TAst.IfThenElse (exp',stm1',stm2'))
  | TAst.While (exp,stm) ->
    let exp' = const_exp exp in
    let stm' = const_stm stm in
    (match exp'.TAst.exp with
      | TAst.BooleanConst b ->
	let pos = exp'.TAst.exp_pos in
	Error.error pos "Constant while condition not allowed"
      | _ ->
	mkstm (TAst.While (exp',stm')))
  | TAst.Empty ->
    stm
  | TAst.Block stms ->
    let stms' = const_stms stms in
    mkstm (TAst.Block stms')

(*  const_stms : TAst.stm list -> TAst.stm list *)
and const_stms stms = match stms with
  | [] -> []
  | stm::stms ->
    let stm'  = const_stm stm in
    let stms' = const_stms stms in
    stm'::stms'

(*  const_return_stm : TAst.return_stm -> TAst.return_stm *)
let const_return_stm retstm = match retstm.TAst.return_stm with
  | TAst.VoidReturn      -> retstm
  | TAst.ValueReturn exp ->
    let exp' = const_exp exp in
    { retstm with TAst.return_stm = TAst.ValueReturn exp' }

(*  const_local_decl : TAst.local_decl -> TAst.local_decl *)
let const_local_decl ldecl =
  let (typ,id,exp) = ldecl in
  let exp'             = const_exp exp in
  (typ,id,exp')

(*  const_local_decls : TAst.local_decl list -> TAst.local_decl list *)
let rec const_local_decls ldecls = match ldecls with
  | [] -> []
  | ldecl::ldecls ->
    let ldecl' = const_local_decl ldecl in
    let ldecls' = const_local_decls ldecls in
    ldecl'::ldecls'

(*  const_field_decl : TAst.field_decl -> TAst.field_decl *)
let const_field_decl fdecl =
  let exp_opt' = const_exp_opt fdecl.TAst.field_init in
  { fdecl with TAst.field_init = exp_opt' }

(*  const_field_decls : TAst.field_decl list -> TAst.field_decl list *)
let rec const_field_decls fdecls = match fdecls with
  | [] -> []
  | fdecl::fdecls ->
    let fdecl' = const_field_decl fdecl in
    let fdecls' = const_field_decls fdecls in
    fdecl'::fdecls'

(*  const_formals_and_body : TAst.formals_and_body -> TAst.formals_and_body *)
let const_formals_and_body fab =
  let locals'  = const_local_decls fab.TAst.locals in
  let stms'    = const_stms fab.TAst.statements in
  let return'  = const_return_stm fab.TAst.return in
  { TAst.formals    = fab.TAst.formals;
    TAst.locals     = locals';
    TAst.statements = stms';
    TAst.return     = return' }

(*  const_formals_and_body_opt : 
    TAst.formals_and_body option -> TAst.formals_and_body option *)
let const_formals_and_body_opt fab_opt = match fab_opt with
  | None -> None
  | Some fab ->
    let fab' = const_formals_and_body fab in
    Some fab'

(*  const_constructor_decl : TAst.constructor_decl -> TAst.constructor_decl *)
let const_constructor_decl cdecl =
  let formals_and_body' =
    const_formals_and_body cdecl.TAst.constructor_formals_and_body in
  { cdecl with TAst.constructor_formals_and_body = formals_and_body' }

(*  const_method_decl : TAst.method_decl -> TAst.method_decl *)
let const_method_decl mdecl =
  let formals_and_body' =
    const_formals_and_body mdecl.TAst.method_formals_and_body in
  { mdecl with TAst.method_formals_and_body = formals_and_body' }

(*  const_method_decls : TAst.method_decl list -> TAst.method_decl list *)
let rec const_method_decls mdecls = match mdecls with
  | [] -> []
  | mdecl::mdecls ->
    let mdecl'  = const_method_decl mdecl in
    let mdecls' = const_method_decls mdecls in
    mdecl'::mdecls'

(*  const_class_decl : TAst.class_decl -> TAst.class_decl *)
let const_class_decl cdecl =
  let fdecls'      = const_field_decls cdecl.TAst.class_fields in
  let constructor' = const_constructor_decl cdecl.TAst.class_constructor in
  let main_opt'    = const_formals_and_body_opt cdecl.TAst.class_main in
  let methods'     = const_method_decls cdecl.TAst.class_methods in
  { TAst.class_name        = cdecl.TAst.class_name;
    TAst.class_fields      = fdecls';
    TAst.class_constructor = constructor';
    TAst.class_main        = main_opt';
    TAst.class_methods     = methods'; }

(*  const_src_file : TAst.src_file -> TAst.src_file *)
let const_src_file src_file =
  let src_file' = const_class_decl src_file.TAst.source_file_decl in
  { src_file with TAst.source_file_decl = src_file' }

(*  const_program : TAst.program -> TAst.program *)
let rec const_program p = match p with
  | [] -> []
  | src_file::src_files ->
    let src_file'  = const_src_file src_file in
    let src_files' = const_program src_files in
    src_file'::src_files'
