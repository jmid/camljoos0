(** Compiler phase to calculate resource information,
    such as JVM signatures and local variable indices.
*)

module TAst = Typecheckingast
module RAst = Resourceast

(************************************************************************)
(** {2 Resource Traversal }                                             *)
(************************************************************************)

module LocalsEnv = Map.Make(String)

type info = { next_index : int;         (* OBS: intraprocedural info *)
	      lenv : int LocalsEnv.t }

(*  res_lvalue : TAst.lvalue -> info -> RAst.lvalue  *)
let rec res_lvalue lvalue info = 

  let pos = lvalue.TAst.lvalue_pos in
  let lvalue_type = lvalue.TAst.lvalue_type in
  let mklvalue lvalue' = { RAst.lvalue_pos = pos;
			   RAst.lvalue = lvalue';
			   lvalue_type = lvalue_type } in

  match lvalue.TAst.lvalue with
    | TAst.Local id ->
      let i = LocalsEnv.find id.Ast.identifier info.lenv in
      mklvalue (RAst.Local (id, i))
    | TAst.Field (id,ftyp) ->
      mklvalue (RAst.Field (id,ftyp))

(*  res_exp : TAst.exp -> info -> RAst.exp  *)
and res_exp exp info =

  let pos = exp.TAst.exp_pos in
  let exp_type = exp.TAst.exp_type in
  let mkexp exp' = { RAst.exp_pos = pos; 
		     RAst.exp = exp'; 
		     exp_type = exp_type } in

  match exp.TAst.exp with
    | TAst.Binop (e1,op,e2) ->
      let e1' = res_exp e1 info in
      let e2' = res_exp e2 info in
      mkexp (RAst.Binop (e1',op,e2'))
    | TAst.Unop (op,e) ->
      let e' = res_exp e info in
      mkexp (RAst.Unop (op,e'))
    | TAst.IntConst i     -> mkexp (RAst.IntConst i)
    | TAst.StringConst s  -> mkexp (RAst.StringConst s)
    | TAst.BooleanConst b -> mkexp (RAst.BooleanConst b)
    | TAst.Null           -> mkexp RAst.Null
    | TAst.This           -> mkexp RAst.This
    | TAst.Invoke (e,id,es,mtyp) ->
      let e' = res_exp e info in
      let es' = res_exp_list es info in
      mkexp (RAst.Invoke (e',id,es',mtyp))
    | TAst.New (t,es,contyp) ->
      let es' = res_exp_list es info in
      mkexp (RAst.New (t,es',contyp))
    | TAst.Lvalue lvalue ->
      let lvalue' = res_lvalue lvalue info in
      mkexp (RAst.Lvalue lvalue')
    | TAst.Assignment (lvalue,e) ->
      let lvalue' = res_lvalue lvalue info in
      let e' = res_exp e info in
      mkexp (RAst.Assignment (lvalue',e'))
    | TAst.Print e ->
      let e' = res_exp e info in
      mkexp (RAst.Print e')
    | TAst.Read ->
      mkexp RAst.Read


(*  res_exp_list : TAst.exp list -> info -> RAst.exp list *)
and res_exp_list exps info = match exps with
  | [] -> []
  | e::es ->
    let e'  = res_exp e info in
    let es' = res_exp_list es info in
    e'::es'

(*  res_exp : TAst.exp option -> info -> RAst.exp option  *)
let res_exp_opt exp info = match exp with
  | None -> None
  | Some e -> 
    let e' = res_exp e info in Some e'

(*  res_stm : TAst.stm -> info -> RAst.stm *)
let rec res_stm stm info = 

  let pos = stm.TAst.stm_pos in
  let mkstm stm' = { RAst.stm_pos = pos;
		     RAst.stm     = stm' } in

  match stm.TAst.stm with
    | TAst.Exp e -> 
      let e' = res_exp e info in
      mkstm (RAst.Exp e')
    | TAst.IfThen (e,s) -> 
      let e' = res_exp e info in
      let s' = res_stm s info in
      mkstm (RAst.IfThen (e',s'))
    | TAst.IfThenElse (e,s1,s2) -> 
      let e' = res_exp e info in
      let s1' = res_stm s1 info in
      let s2' = res_stm s2 info in
      mkstm (RAst.IfThenElse (e',s1',s2'))
    | TAst.While (e,s) -> 
      let e' = res_exp e info in
      let s' = res_stm s info in
      mkstm (RAst.While (e',s'))
    | TAst.Empty -> 
      mkstm (RAst.Empty)
    | TAst.Block stms -> 
      let stms' = res_stms stms info in
      mkstm (RAst.Block stms')


(*  res_stms : TAst.stm list -> info -> RAst.stm list  *)
and res_stms stms info = match stms with
  | [] -> []
  | stm::stms ->
    let stm' = res_stm stm info in
    let stms' = res_stms stms info in
    stm'::stms'

(*  res_return_stm : TAst.return_stm -> info -> RAst.return_stm  *)
let res_return_stm retstm info = 

  let pos = retstm.TAst.return_stm_pos in
  let mkretstm stm' = { RAst.return_stm_pos = pos;
			RAst.return_stm     = stm' } in

  match retstm.TAst.return_stm with
    | TAst.VoidReturn ->
      mkretstm (RAst.VoidReturn)
    | TAst.ValueReturn e ->
      let e' = res_exp e info in
      mkretstm (RAst.ValueReturn e')

(*  res_formal_param : TAst.formal_param -> info -> info * RAst.formal_param  *)
let res_formal_param formal info =	
  let (typ,id)   = formal in
  let next_index = info.next_index in
  let formal'    = (typ,id,next_index) in
  let info'      = { next_index = next_index + 1;  (* add another local *)
		     lenv = LocalsEnv.add id.Ast.identifier next_index info.lenv } in
  (info', formal')

(*  res_formal_params : TAst.formal_param list -> info -> info * RAst.formal_param list  *)
let rec res_formal_params formals info = match formals with
  | [] -> (info,[])
  | formal::formals ->
    let (info',formal')   = res_formal_param formal info in
    let (info'',formals') = res_formal_params formals info' in
    (info'',formal'::formals')

(*  res_local_decl : TAst.local_decl -> info -> info * RAst.local_decl  *)
let res_local_decl ldecl info =	
  let (typ,id,exp) = ldecl in
  let exp'         = res_exp exp info in
  let next_index   = info.next_index in
  let formal'      = (typ,id,exp',next_index) in
  let info'        = { next_index = next_index + 1;  (* add another local *)
		       lenv = LocalsEnv.add id.Ast.identifier next_index info.lenv } in
  (info', formal')

(*  res_local_decls : TAst.local_decl list -> info -> info * RAst.local_decl list  *)
let rec res_local_decls ldecls info = match ldecls with
  | [] -> (info,[])
  | ldecl::ldecls ->
    let (info',ldecl')   = res_local_decl ldecl info in
    let (info'',ldecls') = res_local_decls ldecls info' in
    (info'',ldecl'::ldecls')

type tdinfo = { type_sig : string } (* OBS: type decl info *)

(*  res_field_decl : TAst.field_decl -> tdinfo -> RAst.field_decl  *)
let res_field_decl fdecl tdinfo =
  let field_sig = tdinfo.type_sig ^ "/" ^ fdecl.TAst.field_name.Ast.identifier in
  let info = { next_index = 0; (* Note: no locals in opt. field initializer *)
	       lenv = LocalsEnv.empty } in
  let field_init' = res_exp_opt fdecl.TAst.field_init info in
  { RAst.field_type      = fdecl.TAst.field_type;  
    RAst.field_name      = fdecl.TAst.field_name;  
    RAst.field_init      = field_init';
    RAst.field_signature = field_sig
  }

(*  res_field_decls : TAst.field_decl list -> tdinfo -> RAst.field_decl list  *)
let rec res_field_decls fdecls tdinfo = match fdecls with
  | [] -> []
  | fdecl::fdecls ->
    let fdecl' = res_field_decl fdecl tdinfo in
    let fdecls' = res_field_decls fdecls tdinfo in
    fdecl'::fdecls'

(*  res_formals_and_body : TAst.formals_and_body -> RAst.formals_and_body  *)
let res_formals_and_body fab =
  (* All methods are non-static, so first local variable has index 1*)
  let info = { next_index = 1;
	       lenv       = LocalsEnv.empty } in
  (* The formals are traversed first, so they are
     assigned the lowest indices (as they should). *)
  let info',formals' = res_formal_params fab.TAst.formals info in
  let info'',locals' = res_local_decls fab.TAst.locals info' in
  let stms'          = res_stms fab.TAst.statements info'' in
  let retstm         = res_return_stm fab.TAst.return info'' in
  { RAst.formals    = formals';
    RAst.locals     = locals';
    RAst.statements = stms';
    RAst.return     = retstm; }

let make_method_sig tdinfo name formals return_sig =
  let formal_sigs = List.map (fun (t,_) -> Types.typeexp_to_sig t) formals in
  tdinfo.type_sig ^ "/" ^ name ^ "(" ^ (String.concat "" formal_sigs) ^ ")" ^ return_sig 

(*  res_constructor_decl :  TAst.constructor_decl -> tdinfo -> RAst.constructor_decl  *)
let res_constructor_decl cdecl tdinfo = 
  let formals_and_body  = cdecl.TAst.constructor_formals_and_body in
  let formals_and_body' = res_formals_and_body formals_and_body in
  let name = "<init>" in
  let return_sig = "V" in
  let constructor_sig = 
    make_method_sig tdinfo name formals_and_body.TAst.formals return_sig in
  { RAst.constructor_name             = cdecl.TAst.constructor_name;
    RAst.constructor_formals_and_body = formals_and_body';
    RAst.constructor_signature        = constructor_sig
  }

(*  res_main_decl_opt : TAst.formals_and_body_opt -> RAst.formals_and_body_opt *)
let res_main_decl_opt main_opt = match main_opt with
  | None -> None
  | Some main ->
    let main' = res_formals_and_body main in
    Some main'

(*  res_method_decl :  TAst.method_decl -> tdinfo -> RAst.method_decl  *)
let res_method_decl mdecl tdinfo =
  let formals_and_body  = mdecl.TAst.method_formals_and_body in
  let formals_and_body' = res_formals_and_body formals_and_body in
  let name = mdecl.TAst.method_name in
  let return_sig = Types.typeexp_to_sig mdecl.TAst.method_return_type in
  let method_sig = 
    make_method_sig tdinfo name.Ast.identifier formals_and_body.TAst.formals return_sig in
  { RAst.method_return_type      = mdecl.TAst.method_return_type;
    RAst.method_name             = name;
    RAst.method_formals_and_body = formals_and_body';
    RAst.method_signature        = method_sig; }

(*  res_method_decls : TAst.method_decl list -> tdinfo -> RAst.method_decl list *)
let rec res_method_decls mdecls tdinfo = match mdecls with
  | [] -> []
  | mdecl::mdecls ->
    let mdecl'  = res_method_decl mdecl tdinfo in
    let mdecls' = res_method_decls mdecls tdinfo in
    mdecl'::mdecls'

(*  res_class_decl : TAst.class_decl-> RAst.class_decl  *)
let res_class_decl cdecl =
  let class_name = cdecl.TAst.class_name in
  let tdinfo     = { type_sig = class_name.Ast.identifier } in
  let fields'    = res_field_decls cdecl.TAst.class_fields tdinfo in
  let constr'    = res_constructor_decl cdecl.TAst.class_constructor tdinfo in
  let main'      = res_main_decl_opt cdecl.TAst.class_main in
  let methods'   = res_method_decls cdecl.TAst.class_methods tdinfo in
  { RAst.class_name           = class_name;
    RAst.class_fields         = fields';
    RAst.class_constructor    = constr';
    RAst.class_main           = main';
    RAst.class_methods        = methods';
    RAst.class_decl_signature = class_name.Ast.identifier;
  }

(*  res_source_file : TAst.source_file -> RAst.source_file  *)
let res_source_file src_file =
  let cdecl' = res_class_decl src_file.TAst.source_file_decl in
  { RAst.source_file_name = src_file.TAst.source_file_name;
    RAst.source_file_decl = cdecl'; }

(*  res_program : TAst.program -> RAst.program *)
let rec res_program prog = match prog with
  | [] -> []
  | src_file::src_files ->
    let src_file'  = res_source_file src_file in
    let src_files' = res_program src_files in
    src_file'::src_files'
