(** Compiler phase to check the uses of all types and variables
    to their definitions.
*)

module LAst = Linkingast
module Env  = Types.Env

(*************************************************************************)
(** {2 Linking Traversal }                                               *)
(*************************************************************************)

(*  link_typeexp : Ast.typeexp -> Types.class_type Env -> Types.typeexp *)
let link_typeexp texp tenv = match texp.Ast.typeexp with
  | Ast.Void     -> Types.Void
  | Ast.Int      -> Types.Int
  | Ast.Boolean  -> Types.Boolean
  | Ast.String   -> Types.String
  | Ast.Class id ->
    let classname = id.Ast.identifier in
    if not (Env.mem classname tenv)
    then
      let pos = id.Ast.identifier_pos in
      Error.error pos ("The class '" ^ classname ^ "' is not defined.")
    else
      Types.Class classname

(*  link_lvalue : Ast.lvalue -> Types.class_type Env -> Types.class_type -> 
                                                           string list -> unit *)
let link_lvalue lvalue tenv ctype lenv = match lvalue.Ast.lvalue with
  | Ast.Local id ->
    let var_name = id.Ast.identifier in
    if not (List.mem var_name lenv)
    then
      let pos = id.Ast.identifier_pos in
      Error.error pos ("The variable '" ^ var_name ^ "' is not defined.")
    else
      ()
  | Ast.Field f ->
    let field_name = f.Ast.identifier in
    let field_list = ctype.Types.class_fields in
    if not (List.exists (fun ftyp -> ftyp.Types.field_name = field_name) field_list)
    then
      let pos = f.Ast.identifier_pos in
      Error.error pos ("The field '" ^ field_name ^ "' is not defined.")
    else
      ()

(*  link_exp : Ast.exp -> Types.class_type Env -> Types.class_type -> 
                                                           string list -> unit *)
let rec link_exp exp tenv ctype lenv = match exp.Ast.exp with
  | Ast.Binop (e0,b,e1) ->
    begin
      link_exp e0 tenv ctype lenv;
      link_exp e1 tenv ctype lenv;
    end
  | Ast.Unop (op,e) ->
      link_exp e tenv ctype lenv
  | Ast.IntConst _
  | Ast.StringConst _
  | Ast.BooleanConst _
  | Ast.Null
  | Ast.This -> ()
  | Ast.Invoke (e,id,es) ->
    begin
      link_exp e tenv ctype lenv;
      link_exps es tenv ctype lenv;
    end
  | Ast.New (id, exps) ->
    let classname = id.Ast.identifier in
    if not (Env.mem classname tenv)
    then
      let pos = id.Ast.identifier_pos in
      Error.error pos ("The class '" ^ classname ^ "' is not defined.")
    else
      link_exps exps tenv ctype lenv
  | Ast.Lvalue lvalue ->
    link_lvalue lvalue tenv ctype lenv
  | Ast.Assignment (lvalue,exp) ->
    begin
      link_lvalue lvalue tenv ctype lenv;
      link_exp exp tenv ctype lenv;
    end
  | Ast.Print e ->
    link_exp e tenv ctype lenv
  | Ast.Read ->
    ()

(*  link_exps : Ast.exp list -> Types.class_type Env -> 
                                       Types.class_type -> string list -> unit *)
and link_exps exps tenv ctype lenv = match exps with
  | [] -> ()
  | exp::exps ->
    begin
      link_exp exp tenv ctype lenv;
      link_exps exps tenv ctype lenv;
    end

(*  link_exp_opt : Ast.exp option -> Types.class_type Env -> 
                                       Types.class_type -> string list -> unit *)
let link_exp_opt exp_opt tenv ctype lenv = match exp_opt with
  | None -> ()
  | Some exp -> link_exp exp tenv ctype lenv

(*  link_stm : Ast.stm -> Types.class_type Env -> Types.class_type -> 
                                                           string list -> unit *)
let rec link_stm stm tenv ctype lenv = match stm.Ast.stm with
  | Ast.Exp e ->
    link_exp e tenv ctype lenv
  | Ast.IfThen (e,s) ->
    begin
      link_exp e tenv ctype lenv;
      link_stm s tenv ctype lenv;
    end
  | Ast.IfThenElse (e,s1,s2) ->
    begin
      link_exp e tenv ctype lenv;
      link_stm s1 tenv ctype lenv;
      link_stm s2 tenv ctype lenv;
    end
  | Ast.While (e,s) ->
    begin
      link_exp e tenv ctype lenv;
      link_stm s tenv ctype lenv;
    end
  | Ast.Empty ->
    ()
  | Ast.Block stms ->
    link_stms stms tenv ctype lenv

(*  link_stms : Ast.stm list -> Types.class_type Env -> Types.class_type -> 
                                                           string list -> unit *)
and link_stms stms tenv ctype lenv = match stms with
  | [] -> ()
  | stm::stms ->
    begin
      link_stm stm tenv ctype lenv;
      link_stms stms tenv ctype lenv;
    end

(*  link_return_stm : Ast.return_stm -> Types.class_type Env -> 
                                       Types.class_type -> string list -> unit *)
let link_return_stm retstm tenv ctype lenv = match retstm.Ast.return_stm with
  | Ast.VoidReturn ->
    ()
  | Ast.ValueReturn e ->
    link_exp e tenv ctype lenv

(*  link_formal_param : Ast.formal_param -> Types.class_type Env ->
                                                    string * LAst.formal_param *)
let link_formal_param (typ,id) tenv =
  let typ' = link_typeexp typ tenv in
  (id.Ast.identifier, (typ',id))

(*  link_formal_params : Ast.formal_param list -> Types.class_type Env ->
                           string list -> string list * LAst.formal_param list *)
let rec link_formal_params formals tenv lenv = match formals with
  | [] -> (lenv,[])
  | formal::formals ->
    let (id, formal')    = link_formal_param formal tenv in 
    let (lenv',formals') = link_formal_params formals tenv (id::lenv) in
    (lenv',formal'::formals')

(*  link_local_decl : Ast.local_decl -> Types.class_type Env ->
                   Types.class_type -> string list -> string * LAst.local_decl *)
let link_local_decl (typ,id,exp) tenv ctype lenv =
  let typ' = link_typeexp typ tenv in
  let ()   = link_exp exp tenv ctype lenv in
  (id.Ast.identifier, (typ',id,exp))


(*  link_local_decls : Ast.local_decl list -> Types.class_type Env ->
         Types.class_type -> string list -> string list * LAst.local_decl list *)
let rec link_local_decls ldecls tenv ctype lenv = match ldecls with
  | [] -> (lenv,[])
  | ldecl::ldecls ->
    let (id, ldecl')    = link_local_decl ldecl tenv ctype lenv in 
    let (lenv',ldecls') = link_local_decls ldecls tenv ctype (id::lenv) in
    (lenv',ldecl'::ldecls')


(*  link_field_decl : Ast.field_decl -> Types.class_type Env ->
                                           Types.class_type -> LAst.field_decl *)
let link_field_decl fdecl tenv ctype =
  let field_type' = link_typeexp fdecl.Ast.field_type tenv in
  let () = link_exp_opt fdecl.Ast.field_init tenv ctype [] in (*no locals*)
  { LAst.field_type = field_type';
    LAst.field_name = fdecl.Ast.field_name;
    LAst.field_init = fdecl.Ast.field_init; }

(*  link_field_decls : Ast.field_decl list -> Types.class_type Env -> 
                                      Types.class_type -> LAst.field_decl list *)
let rec link_field_decls fdecls tenv ctype = match fdecls with
  | [] -> []
  | fdecl::fdecls ->
    let fdecl'  = link_field_decl fdecl tenv ctype in
    let fdecls' = link_field_decls fdecls tenv ctype in
    fdecl'::fdecls'

(*  link_formals_and_body : Ast.formals_and_body -> Types.class_type Env -> 
                                     Types.class_type -> LAst.formals_and_body *)
let link_formals_and_body fab tenv ctype =
  let (lenv,formals') = link_formal_params fab.Ast.formals tenv [] in
  let (lenv',locals') = link_local_decls fab.Ast.locals tenv ctype lenv in
  let () = link_stms fab.Ast.statements tenv ctype lenv' in
  let () = link_return_stm fab.Ast.return tenv ctype lenv' in
  { LAst.formals = formals';
    LAst.locals  = locals';
    LAst.statements = fab.Ast.statements;
    LAst.return = fab.Ast.return; }

(*  link_constructor_decl : Ast.constructor_decl -> Types.class_type Env -> 
                                     Types.class_type -> LAst.constructor_decl *)
let link_constructor_decl cdecl tenv ctype =
  let fab' =
    link_formals_and_body cdecl.Ast.constructor_formals_and_body tenv ctype in
  { LAst.constructor_name             = cdecl.Ast.constructor_name;
    LAst.constructor_formals_and_body = fab' }

(*  link_main_decl_opt : Ast.main_decl option -> Types.class_type Env ->
                                     Types.class_type -> LAst.main_decl option *)
let link_main_decl_opt mdecl tenv ctype = match mdecl with
  | None -> None
  | Some mdecl -> 
    let mdecl' = link_formals_and_body mdecl tenv ctype in
    Some mdecl'

(*  link_method_decl : Ast.method_decl -> Types.class_type Env -> 
                                          Types.class_type -> LAst.method_decl *)
let link_method_decl mdecl tenv ctype =
  let rettype' = link_typeexp mdecl.Ast.method_return_type tenv in
  let fab' = 
    link_formals_and_body mdecl.Ast.method_formals_and_body tenv ctype in
  { LAst.method_return_type      = rettype';
    LAst.method_name             = mdecl.Ast.method_name;
    LAst.method_formals_and_body = fab' }

(*  link_method_decls : Ast.method_decl list -> Types.class_type Env ->
                                     Types.class_type -> LAst.method_decl list *)
let rec link_method_decls mdecls tenv ctype = match mdecls with
  | [] -> []
  | mdecl::mdecls ->
    let mdecl'  = link_method_decl mdecl tenv ctype in
    let mdecls' = link_method_decls mdecls tenv ctype in
    mdecl'::mdecls'

(*  link_class_decl : Ast.class_decl -> Types.class_type Env -> LAst.class_decl *)
let link_class_decl cdecl tenv =
  let class_name     = cdecl.Ast.class_name.Ast.identifier in
  let class_type     = Env.find class_name tenv in
  let class_fields'  = link_field_decls cdecl.Ast.class_fields tenv class_type in
  let class_constr'  = link_constructor_decl cdecl.Ast.class_constructor tenv class_type in
  let class_main'    = link_main_decl_opt cdecl.Ast.class_main tenv class_type in
  let class_methods' = link_method_decls cdecl.Ast.class_methods tenv class_type in
  { LAst.class_name        = cdecl.Ast.class_name;
    LAst.class_fields      = class_fields';
    LAst.class_constructor = class_constr';
    LAst.class_main        = class_main';
    LAst.class_methods     = class_methods'; }

(*  link_src_file : Ast.source_file -> Types.class_type Env -> LAst.source_file *)
let link_src_file src_file tenv =
  let class_decl' = link_class_decl src_file.Ast.source_file_decl tenv in
  { LAst.source_file_name = src_file.Ast.source_file_name;
    LAst.source_file_decl = class_decl'; }

(*  link_program : Ast.program -> Types.class_type Env -> LAst.program *)
let rec link_program prog tenv = match prog with
  | [] -> []
  | src_file::src_files ->
    let src_file'  = link_src_file src_file tenv in
    let src_files' = link_program src_files tenv in
    src_file'::src_files'
