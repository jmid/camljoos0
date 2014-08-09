(** Compiler phase to type environment to check duplicate declarations
    for all named entities in the program. 
*)
module Env = Types.Env

(*************************************************************************)
(** {2 Helper Functions }                                                *)
(*************************************************************************)

(** Fetches an item from an environment.
    If no item with the given name exists, an error message is printed,
    and the compilation is halted.
    @param env   the environment
    @param kind  a string describing what kind of item is being fetched
    @param id    the name of the item
    @param pos   the position at which the potential error occured
    @return      the item
*)
let lookup_env env kind id pos =
  if Env.mem id env
  then Env.find id env
  else Error.error pos ("The " ^ kind ^ " '" ^ id ^ "' is not defined.")

(*************************************************************************)
(** {2 Environment Traversal }                                           *)
(*************************************************************************)

(*  env_typeexp : Ast.typexp -> Types.typeexp  *)
let env_typeexp texp = match texp.Ast.typeexp with
  | Ast.Void     -> Types.Void
  | Ast.Int      -> Types.Int
  | Ast.Boolean  -> Types.Boolean
  | Ast.String   -> Types.String
  | Ast.Class id -> Types.Class (id.Ast.identifier)

(*  env_formal_param : Ast.formal_param -> Types.typeexp Env ->
                                             Types.typeexp * Types.typeexp Env *)
let env_formal_param (typ, id) lenv =
  let varname = id.Ast.identifier in
  if Env.mem varname lenv
  then 
    let pos = id.Ast.identifier_pos in
    Error.error pos ("The variable '" ^ varname ^ "' is already defined.")
  else 
    let ltyp = env_typeexp typ in
    (ltyp, Env.add varname ltyp lenv)

(*  env_formal_params : Ast.formal_param list -> Types.typeexp Env ->
                                        Types.typeexp list * Types.typeexp Env *)
let rec env_formal_params formals lenv = match formals with
  | [] -> ([],lenv)
  | formal::formals ->
    let (typ,lenv')   = env_formal_param formal lenv in
    let (typs,lenv'') = env_formal_params formals lenv' in
    (typ::typs,lenv'')

(*  env_local_decl : Ast.local_decl -> Types.typeexp Env ->
                                             Types.typeexp * Types.typeexp Env *)
let env_local_decl (typ, id, _) lenv =
  env_formal_param (typ, id) lenv
  
(*  env_local_decls : Ast.local_decl list -> Types.typeexp Env ->
                                        Types.typeexp list * Types.typeexp Env *)
let rec env_local_decls ldecls lenv = match ldecls with
  | [] -> ([],lenv)
  | ldecl::ldecls ->
    let (typ,lenv')   = env_local_decl ldecl lenv in
    let (typs,lenv'') = env_local_decls ldecls lenv' in
    (typ::typs,lenv'')

(*  env_field_decl : Ast.field_decl -> Types.field_type Env -> 
                                       Types.field_type * Types.field_type Env *)
let env_field_decl field_decl fenv =
  let fieldname = field_decl.Ast.field_name.Ast.identifier in
  if Env.mem fieldname fenv
  then 
    let pos = field_decl.Ast.field_name.Ast.identifier_pos in
    Error.error pos ("The field '" ^ fieldname ^ "' is already defined.")
  else
    let fieldtype = env_typeexp field_decl.Ast.field_type in
    let field = { Types.field_type = fieldtype;
		  Types.field_name = fieldname; } in
    (field, Env.add fieldname field fenv)

(*  env_field_decls : Ast.field_decl list -> Types.field_type Env -> 
                                  Types.field_type list * Types.field_type Env *)
let rec env_field_decls field_decls fenv = match field_decls with
  | [] -> ([],fenv)
  | field_decl::field_decls -> 
    let (field_decl',fenv')   = env_field_decl field_decl fenv in
    let (field_decls',fenv'') = env_field_decls field_decls fenv' in
    (field_decl'::field_decls',fenv'')


(*  env_formals_and_body : Ast.formals_and_body -> 
                                        Types.typeexp list * Types.typeexp Env *)
let env_formals_and_body formals_and_body =
  let (ftypes,lenv)  = env_formal_params formals_and_body.Ast.formals Env.empty in
  let (_     , _)    = env_local_decls formals_and_body.Ast.locals lenv in
  ftypes


(*  env_constructor_decl : Ast.constructor_decl -> Types.constructor_type *)
let env_constructor_decl cdecl = 
  let name = cdecl.Ast.constructor_name.Ast.identifier in
  let ftyps = 
    env_formals_and_body cdecl.Ast.constructor_formals_and_body in
  { Types.constructor_name    = name;
    Types.constructor_formals = ftyps }

(* main is a keyword, empty formals list, return type void, body is new call
let env_main_decl_opt main_decl = match mail_decl with
  | None -> None
  | Some mail_decl ->
    let formals env_formals_and_body mail_decl
*)

(*  env_method_decl : Ast.method_decl -> Types.method_type Env -> 
                                     Types.method_type * Types.method_type Env *)
let env_method_decl mdecl menv =
  let methodname = mdecl.Ast.method_name.Ast.identifier in
  if Env.mem methodname menv
  then 
    let pos = mdecl.Ast.method_name.Ast.identifier_pos in
    Error.error pos ("The method '" ^ methodname ^ "' is already defined.")
  else 
    let rettype = env_typeexp mdecl.Ast.method_return_type in
    let ftypes  = env_formals_and_body mdecl.Ast.method_formals_and_body in
    let mdecl'  = { Types.method_result  = rettype;
		    Types.method_name    = methodname;
		    Types.method_formals = ftypes; } in
    (mdecl', Env.add methodname mdecl' menv)

(*  env_method_decls : Ast.method_decl list -> Types.method_type Env -> 
                                Types.method_type list * Types.method_type Env *)
let rec env_method_decls mdecls menv = match mdecls with
  | [] -> ([], menv)
  | mdecl::mdecls ->
    let (mdecl',menv')   = env_method_decl mdecl menv in
    let (mdecls',menv'') = env_method_decls mdecls menv' in
    (mdecl'::mdecls',menv'')

(*  env_class_decl : Ast.class_decl -> Types.class_type Env -> 
                                                          Types.class_type Env *)
let env_class_decl decl env =
  let name = decl.Ast.class_name.Ast.identifier in
  if Env.mem name env
  then
    let pos = decl.Ast.class_name.Ast.identifier_pos in
    Error.error pos ("The class '" ^ name ^ "' is already defined.")
  else
    let (mdecls',_) = env_method_decls decl.Ast.class_methods Env.empty in
    let (fields',_) = env_field_decls decl.Ast.class_fields Env.empty in
    let typ = 
      { Types.class_name        = name;
	Types.class_fields      = fields';
	Types.class_constructor = env_constructor_decl decl.Ast.class_constructor;
(*	Types.class_main        = env_main_decl_opt decl.Ast.class_main;*)
	Types.class_methods     = mdecls' }
    in Env.add name typ env

(*  env_source_file : Ast.source_file -> Types.class_type Env -> 
                                                          Types.class_type Env *)
let env_source_file sf env =
  env_class_decl sf.Ast.source_file_decl env

(*  env_source_files : Ast.source_file list -> Types.class_type Env -> 
                                                          Types.class_type Env *)
let rec env_source_files sfs env = match sfs with
  | [] -> env
  | sf :: sfs -> env_source_files sfs (env_source_file sf env)

(* Ast.program -> Types.type_env *)
let env_program prog =
  env_source_files prog Env.empty

(* ************************************************************ *)

