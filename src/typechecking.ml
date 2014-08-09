(** Compiler phase to perform type checking of the program.
    It also performs type coercions and other transformations
    based on the types.
*)

module LAst = Linkingast
module TAst = Typecheckingast
module Env  = Types.Env

(*************************************************************************)
(** {2 Helper Functions }                                                *)
(*************************************************************************)

let rec typeexp_to_string typeexp = match typeexp with
  | Types.Void -> "void"
  | Types.Int  -> "int"
  | Types.Boolean -> "boolean"
  | Types.String  -> "String"
  | Types.Class name -> name
  | Types.Null -> "null"

let binop_to_string binop = match binop with
  | Ast.Plus   -> "plus"
  | Ast.Minus  -> "minus"
  | Ast.Times  -> "times"
  | Ast.Divide -> "divide"
  | Ast.Modulo -> "modulo"
  | Ast.Eq  -> "eq"
  | Ast.Ne  -> "ne"
  | Ast.Lt  -> "lt"
  | Ast.Le  -> "le"
  | Ast.Gt  -> "gt"
  | Ast.Ge  -> "ge"
  | Ast.And -> "and"
  | Ast.Or  -> "or"
  | Ast.Xor -> "xor"
  | Ast.Concat -> "+"

let unop_to_string unop = match unop with
  | Ast.Negate -> "negate"
  | Ast.Complement -> "complement"
  | Ast.CharToString -> "+(char)"

let convert_binop binop = match binop with
  | Ast.Plus   -> TAst.Plus
  | Ast.Minus  -> TAst.Minus
  | Ast.Times  -> TAst.Times
  | Ast.Divide -> TAst.Divide
  | Ast.Modulo -> TAst.Modulo
  | Ast.Eq     -> TAst.Eq
  | Ast.Ne     -> TAst.Ne
  | Ast.Lt     -> TAst.Lt
  | Ast.Le     -> TAst.Le
  | Ast.Gt     -> TAst.Gt
  | Ast.Ge     -> TAst.Ge
  | Ast.And    -> TAst.And
  | Ast.Or     -> TAst.Or
  | Ast.Xor    -> TAst.Xor
  | Ast.Concat -> TAst.Concat


(** Insert a conversion to string of the given expression.
      @param exp  the expression to coerce. 
*)
let coerce_to_string exp err =
  let mkunop uop = { exp with TAst.exp = TAst.Unop (uop, exp) } in

  match exp.TAst.exp_type with
    | Types.Void    -> err ()
    | Types.Int     -> mkunop TAst.IntToString
    | Types.Boolean -> mkunop TAst.BooleanToString
    | Types.String  -> 
       (* If the expression is not a constant or the result
	  of a string concatenation or conversion, it may
	  be null. Since null values must be coerced to the
	  string "null", we must insert a conversion to string
	  even though the value is already a string. *)
      (match exp.TAst.exp with
	| TAst.StringConst _
	| TAst.Binop _
	| TAst.Unop _ -> exp
	| _           -> mkunop TAst.ObjectToString)
    | Types.Class _
    | Types.Null   -> mkunop TAst.ObjectToString
      (* Null values are not subject to constant folding, so
	 just replacing by the string "null" could lead to
	 incorrect results. *)


(** Check if a value of one type can be assigned to a variable of another.
      @param to the type that was expected
      @param from the type that was given
      @return [true] if the assignment is legal,
              [false] otherwise.
*)
let assignable totype fromtype =
  if totype = fromtype
  then true
  else match (fromtype, totype) with
    | (Types.Null, Types.Class _)
    | (Types.Null, Types.String) -> true
    | _ -> false

let rec lookup_method mname methods pos = match methods with
  | [] ->  Error.error pos ("The method '" ^ mname ^ "' is not defined.")
  | meth::meths ->
    if meth.Types.method_name = mname
    then meth
    else lookup_method mname meths pos


(*************************************************************************)
(** {2 Error Functions }                                                 *)
(*************************************************************************)

(** Print a type error message for a binary operation.
      @param exp the expression containing a type error
*)
let binop_type_error lexp binop rexp pos =
  Error.error pos ("The operator " ^ (binop_to_string binop) ^
     " cannot be used with the types " ^ (typeexp_to_string lexp.TAst.exp_type) ^
                              " and " ^ (typeexp_to_string rexp.TAst.exp_type))
    

(** Print a type error message for a unary operation.
      @param exp the expression containing a type error
*)
let unop_type_error unop exp pos =
  Error.error pos ("The operator " ^ (unop_to_string unop) ^
        " cannot be used with the type " ^ (typeexp_to_string exp.TAst.exp_type))


(** Print a type error message for an assignment or argument passing.
      @param pos the position of the error
      @param to the type that was expected
      @param from the type that was given
*)
let assign_type_error totype fromtype pos =
  Error.error pos ("The type " ^ (typeexp_to_string fromtype) ^
                 " cannot be assigned to the type " ^ (typeexp_to_string totype))


(*************************************************************************)
(** {2 Type Checking Traversal }                                         *)
(*************************************************************************)

(*  tcheck_lvalue : Ast.lvalue -> Types.class_type Env -> Types.class_type ->
                                              Types.typeexp Env -> TAst.lvalue *)
let tcheck_lvalue lvalue tenv ctype lenv = 
  let pos = lvalue.Ast.lvalue_pos in

  let mklval lval typ = 
    { TAst.lvalue_pos = pos; TAst.lvalue_type = typ; TAst.lvalue = lval } in

  match lvalue.Ast.lvalue with
    | Ast.Local id ->
      let pos  = id.Ast.identifier_pos in
      let ltyp = Environment.lookup_env lenv "variable" id.Ast.identifier pos in
      mklval (TAst.Local id) ltyp
    | Ast.Field f  ->
      let fname  = f.Ast.identifier in
      let fields = ctype.Types.class_fields in
      let ftyp   = List.find (fun ft -> ft.Types.field_name = fname) fields in
      mklval (TAst.Field (f,ftyp)) ftyp.Types.field_type


(* tcheck_arg_list : Types.typeexp list -> Types.typeexp list -> 
                                                       Lexing.position -> unit *)
let rec tcheck_arg_list totypes fromtypes kind pos = 
  match (totypes, fromtypes) with
    | ([],[]) -> ()
    | (totyp::ttyps,fromtyp::ftyps) ->
      if assignable totyp fromtyp
      then tcheck_arg_list ttyps ftyps kind pos
      else assign_type_error totyp fromtyp pos
    | _ -> 
      Error.error pos ("Incorrect number of arguments for " ^ kind)


(*  tcheck_exp : Ast.exp -> Types.class_type Env -> Types.class_type -> 
                                                 Types.typeexp Env -> TAst.exp *)
let rec tcheck_exp exp tenv ctype lenv = 
  let pos = exp.Ast.exp_pos in

  let mkexp exp typ = 
    { TAst.exp_pos = pos; TAst.exp_type = typ; TAst.exp = exp } in

  match exp.Ast.exp with
    | Ast.Binop (exp1,op,exp2) ->
      let exp1' = tcheck_exp exp1 tenv ctype lenv in
      let exp2' = tcheck_exp exp2 tenv ctype lenv in
      (match (exp1'.TAst.exp_type, op, exp2'.TAst.exp_type) with
        (* Only occurs with + (char) exp
           Both arguments must be of string type *)
	| (Types.String,   Ast.Concat, Types.String) ->
	  mkexp (TAst.Binop (exp1',TAst.Concat,exp2')) Types.String
	(* String concatenation *)
	| (Types.String,   Ast.Plus,   _           )
	| (_,              Ast.Plus,   Types.String) ->
	  let err () = binop_type_error exp1' op exp2' exp.Ast.exp_pos in
	  let exp1' = coerce_to_string exp1' err in
	  let exp2' = coerce_to_string exp2' err in
	  mkexp (TAst.Binop (exp1',TAst.Concat,exp2')) Types.String
	(* Integer addition *)
	| (Types.Int,      Ast.Plus,   Types.Int)
	(* Integer operation *)
	| (Types.Int,      Ast.Minus,  Types.Int)
	| (Types.Int,      Ast.Times,  Types.Int)
	| (Types.Int,      Ast.Divide, Types.Int)
	| (Types.Int,      Ast.Modulo, Types.Int) ->
	  let op' = convert_binop op in
	  mkexp (TAst.Binop (exp1',op',exp2')) Types.Int

	| (Types.Int,      Ast.Eq,     Types.Int)
	| (Types.Int,      Ast.Ne,     Types.Int)
	| (Types.Boolean,  Ast.Eq,     Types.Boolean)
	| (Types.Boolean,  Ast.Ne,     Types.Boolean) ->
	  let op' = convert_binop op in
	  mkexp (TAst.Binop (exp1',op',exp2')) Types.Boolean

	| (Types.String,   Ast.Eq,     Types.String)
	| (Types.String,   Ast.Eq,     Types.Null)
	| (Types.Class _,  Ast.Eq,     Types.Null)
	| (Types.Null,     Ast.Eq,     Types.String)
	| (Types.Null,     Ast.Eq,     Types.Class _)
	| (Types.Null,     Ast.Eq,     Types.Null) ->
	  mkexp (TAst.Binop (exp1',TAst.Aeq,exp2')) Types.Boolean

	| (Types.String,   Ast.Ne,     Types.String)
	| (Types.String,   Ast.Ne,     Types.Null)
	| (Types.Class _,  Ast.Ne,     Types.Null)
	| (Types.Null,     Ast.Ne,     Types.String)
	| (Types.Null,     Ast.Ne,     Types.Class _)
	| (Types.Null,     Ast.Ne,     Types.Null) ->
	  mkexp (TAst.Binop (exp1',TAst.Ane,exp2')) Types.Boolean

	| (Types.Class lc, Ast.Eq,     Types.Class rc) ->
	  if lc <> rc
	  then binop_type_error exp1' op exp2' exp.Ast.exp_pos
	  else mkexp (TAst.Binop (exp1',TAst.Aeq,exp2')) Types.Boolean

	| (Types.Class lc, Ast.Ne,     Types.Class rc) ->
	  if lc <> rc
	  then binop_type_error exp1' op exp2' exp.Ast.exp_pos
	  else mkexp (TAst.Binop (exp1',TAst.Ane,exp2')) Types.Boolean

	| (Types.Int,      Ast.Lt,     Types.Int)
	| (Types.Int,      Ast.Le,     Types.Int)
	| (Types.Int,      Ast.Gt,     Types.Int)
	| (Types.Int,      Ast.Ge,     Types.Int)

	| (Types.Boolean,  Ast.And,    Types.Boolean)
	| (Types.Boolean,  Ast.Or,     Types.Boolean)
	| (Types.Boolean,  Ast.Xor,    Types.Boolean) ->
	  let op' = convert_binop op in
	  mkexp (TAst.Binop (exp1',op',exp2')) Types.Boolean
	    
	| _ -> 
	  binop_type_error exp1' op exp2' exp.Ast.exp_pos )

    | Ast.Unop (op,exp) ->
      let exp' = tcheck_exp exp tenv ctype lenv in
      (match (op, exp'.TAst.exp_type) with
	| (Ast.Negate,      Types.Int) -> 
	  mkexp (TAst.Unop (TAst.Negate,exp')) Types.Int
	| (Ast.Complement,  Types.Boolean) -> 
	  mkexp (TAst.Unop (TAst.Complement,exp')) Types.Boolean
	| (Ast.CharToString,Types.Int) ->
	  mkexp (TAst.Unop (TAst.CharToString,exp')) Types.String
	| _ -> unop_type_error op exp' pos)

    | Ast.IntConst i ->
      mkexp (TAst.IntConst i) Types.Int
    | Ast.StringConst s ->
      mkexp (TAst.StringConst s) Types.String
    | Ast.BooleanConst b ->
      mkexp (TAst.BooleanConst b) Types.Boolean
    | Ast.Null ->
      mkexp TAst.Null Types.Null
    | Ast.This ->
      mkexp TAst.This (Types.Class ctype.Types.class_name)
    | Ast.Invoke (exp,m,exps) ->
      let exp'  = tcheck_exp exp tenv ctype lenv in
      let exps' = tcheck_exps exps tenv ctype lenv in
      (match exp'.TAst.exp_type with
	| Types.Class cid ->
	  let pos     = m.Ast.identifier_pos in
	  let ctype   = Env.find cid tenv in
	  let methods = ctype.Types.class_methods in
	  let mtype   = lookup_method m.Ast.identifier methods pos in
	  let formals = mtype.Types.method_formals in
	  let actuals = List.map (fun e' -> e'.TAst.exp_type) exps' in
	  begin
	    tcheck_arg_list formals actuals "method" pos;
	    mkexp (TAst.Invoke (exp',m,exps',mtype)) mtype.Types.method_result;
	  end
	| _ -> Error.error exp.Ast.exp_pos 
                      "Only class types can be method invocation receivers" )
    | Ast.New (id,exps) ->
      let pos         = id.Ast.identifier_pos in
      let exps'       = tcheck_exps exps tenv ctype lenv in
      let ctype       = Env.find id.Ast.identifier tenv in
      let constructor = ctype.Types.class_constructor in
      let formals     = constructor.Types.constructor_formals in
      let actuals     = List.map (fun e' -> e'.TAst.exp_type) exps' in
      begin
	tcheck_arg_list formals actuals "constructor" pos;
	mkexp (TAst.New (id,exps',constructor)) (Types.Class id.Ast.identifier)
      end
    | Ast.Lvalue lval ->
      let lval' = tcheck_lvalue lval tenv ctype lenv in
      mkexp (TAst.Lvalue lval') lval'.TAst.lvalue_type
    | Ast.Assignment (lval,exp) ->
      let lval' = tcheck_lvalue lval tenv ctype lenv in
      let exp'  = tcheck_exp exp tenv ctype lenv in
      let lvalue_type = lval'.TAst.lvalue_type in
      let exp_type    = exp'.TAst.exp_type in
      if not (assignable lvalue_type exp_type)
      then
	assign_type_error lvalue_type exp_type exp.Ast.exp_pos
      else
	mkexp (TAst.Assignment (lval',exp')) lvalue_type
    | Ast.Print e ->
      let e' = tcheck_exp e tenv ctype lenv in
      if e'.TAst.exp_type = Types.Void
      then
	let pos = e.Ast.exp_pos in
	Error.error pos "Value to be printed must not be void"
      else
	mkexp (TAst.Print e') Types.Void
    | Ast.Read ->
      mkexp TAst.Read Types.Int

(*  tcheck_exps : Ast.exp list -> Types.class_type Env -> Types.class_type ->
                                            Types.typeexp Env -> TAst.exp list *)
and tcheck_exps exps tenv ctype lenv = match exps with
  | [] -> []
  | exp::exps ->
    let exp'  = tcheck_exp exp tenv ctype lenv in
    let exps' = tcheck_exps exps tenv ctype lenv in
    exp'::exps'
  
(*  tcheck_exp_opt : Ast.exp option -> Types.class_type Env -> 
                      Types.class_type -> Types.typeexp Env -> TAst.exp option *)
let tcheck_exp_opt exp_opt tenv ctype lenv = match exp_opt with
  | None -> None
  | Some exp ->
    let exp' = tcheck_exp exp tenv ctype lenv in
    Some exp'

(*  tcheck_stm : Ast.stm -> Types.class_type Env -> Types.class_type -> 
                                                 Types.typeexp Env -> TAst.stm *)
let rec tcheck_stm stm tenv ctype lenv = 
  let pos = stm.Ast.stm_pos in

  let mkstm stm = 
    { TAst.stm_pos = pos; TAst.stm = stm } in

  match stm.Ast.stm with
    | Ast.Exp e ->
      let e' = tcheck_exp e tenv ctype lenv in
      mkstm (TAst.Exp e')
    | Ast.IfThen (e,s) ->
      let e' = tcheck_exp e tenv ctype lenv in
      let s' = tcheck_stm s tenv ctype lenv in
      if e'.TAst.exp_type <> Types.Boolean
      then
	let pos = e'.TAst.exp_pos in
	Error.error pos "Condition must have type boolean"
      else
	mkstm (TAst.IfThen (e',s'))
    | Ast.IfThenElse (e,s1,s2) ->
      let e'  = tcheck_exp e tenv ctype lenv in
      let s1' = tcheck_stm s1 tenv ctype lenv in
      let s2' = tcheck_stm s2 tenv ctype lenv in
      if e'.TAst.exp_type <> Types.Boolean
      then
	let pos = e'.TAst.exp_pos in
	Error.error pos "Condition must have type boolean"
      else
	mkstm (TAst.IfThenElse (e',s1',s2'))
    | Ast.While (e,s) ->
      let e' = tcheck_exp e tenv ctype lenv in
      let s' = tcheck_stm s tenv ctype lenv in
      if e'.TAst.exp_type <> Types.Boolean
      then
	let pos = e'.TAst.exp_pos in
	Error.error pos "Condition must have type boolean"
      else
	mkstm (TAst.While (e',s'))
    | Ast.Empty ->
      mkstm TAst.Empty
    | Ast.Block stms ->
      let stms' = tcheck_stms stms tenv ctype lenv in
      mkstm (TAst.Block stms')

(*  tcheck_stms : Ast.stm list -> Types.class_type Env -> Types.class_type ->
                                            Types.typeexp Env -> TAst.stm list *)
and tcheck_stms stms tenv ctype lenv = match stms with
  | [] -> []
  | stm::stms ->
    let stm' = tcheck_stm stm tenv ctype lenv in
    let stms' = tcheck_stms stms tenv ctype lenv in
    stm'::stms'

(*  tcheck_return_stm : LAst.return_stm -> Types.class_type Env -> 
                              Types.class_type -> Types.typeexp Env ->
                                        Types.typeexp -> TAst.formals_and_body *)
let tcheck_return_stm return_stm tenv ctype lenv rettype =  
  let pos = return_stm.Ast.return_stm_pos in

  let mkretstm retstm = 
    { TAst.return_stm_pos = pos; TAst.return_stm = retstm } in

  match return_stm.Ast.return_stm with
    | Ast.VoidReturn ->
      if rettype <> Types.Void
      then
	Error.error pos "A non-void method must return a value"
      else
	mkretstm TAst.VoidReturn
    | Ast.ValueReturn e ->
      let e' = tcheck_exp e tenv ctype lenv in
      let etype = e'.TAst.exp_type in
      if not (assignable rettype etype)
      then 
	assign_type_error rettype etype pos
      else mkretstm (TAst.ValueReturn e')


(*  tcheck_formal_param : LAst.formal_param -> Types.class_type Env -> 
                    Types.typeexp Env -> TAst.formal_param * Types.typeexp Env *)
let tcheck_formal_param formal tenv lenv = 
  let (typ,id) = formal in
  let lenv'    = Env.add id.Ast.identifier typ lenv in
  (formal, lenv')

(*  tcheck_formal_params : LAst.formal_param list -> Types.class_type Env -> 
               Types.typeexp Env -> TAst.formal_param list * Types.typeexp Env *)
let rec tcheck_formal_params formals tenv lenv = match formals with
  | [] -> ([], lenv)
  | formal::formals ->
    let (formal',lenv')   = tcheck_formal_param formal tenv lenv in
    let (formals',lenv'') = tcheck_formal_params formals tenv lenv' in
    (formal'::formals',lenv'')

(*  tcheck_local_decl : LAst.local_decl -> Types.class_type Env -> 
                         Types.class_type -> Types.typeexp Env -> 
                                           TAst.local_decl * Types.typeexp Env *)
let tcheck_local_decl ldecl tenv ctype lenv = 
  let (typ,id,exp) = ldecl in
  let exp'         = tcheck_exp exp tenv ctype lenv  in
  let etype = exp'.TAst.exp_type in
  let pos   = exp'.TAst.exp_pos in
  let lenv' = Env.add id.Ast.identifier typ lenv in
  if not (assignable typ etype)
  then assign_type_error typ etype pos
  else ((typ,id,exp'), lenv')

(*  tcheck_local_decls : LAst.local_decl list -> Types.class_type Env -> 
                              Types.class_type -> Types.typeexp Env -> 
                                      TAst.local_decl list * Types.typeexp Env *)
let rec tcheck_local_decls ldecls tenv ctype lenv = match ldecls with
  | [] -> ([], lenv)
  | ldecl::ldecls ->
    let (ldecl',lenv')   = tcheck_local_decl ldecl tenv ctype lenv in
    let (ldecls',lenv'') = tcheck_local_decls ldecls tenv ctype lenv' in
    (ldecl'::ldecls',lenv'')


(*  tcheck_field_decl : LAst.field_decl -> Types.class_type Env -> 
                                           Types.class_type -> TAst.field_decl *)
let tcheck_field_decl fdecl tenv ctype =
  let exp_opt' =
    match tcheck_exp_opt fdecl.LAst.field_init tenv ctype Env.empty with
      | None -> None
      | Some e' ->
	let ftype = fdecl.LAst.field_type in
	let etype = e'.TAst.exp_type in
	let pos   = e'.TAst.exp_pos in
	if not (assignable ftype etype)
	then assign_type_error ftype etype pos
	else Some e' in
  { TAst.field_type = fdecl.LAst.field_type;
    TAst.field_name = fdecl.LAst.field_name;
    TAst.field_init = exp_opt' }
      
(*  tcheck_field_decls : LAst.field_decl list -> Types.class_type Env -> 
                                      Types.class_type -> TAst.field_decl list *)
let rec tcheck_field_decls fdecls tenv ctype = match fdecls with
  | [] -> []
  | fdecl::fdecls ->
    let fdecl'  = tcheck_field_decl fdecl tenv ctype in
    let fdecls' = tcheck_field_decls fdecls tenv ctype in
    fdecl'::fdecls'

(*  tcheck_formals_and_body : LAst.formals_and_body -> Types.class_type Env -> 
                    Types.class_type -> Types.typeexp -> TAst.formals_and_body *)
let tcheck_formals_and_body fab tenv ctype rettype = 
  let (formals',lenv) =
    tcheck_formal_params fab.LAst.formals tenv Env.empty in
  let (locals',lenv') = 
    tcheck_local_decls fab.LAst.locals tenv ctype lenv in
  let statements' = tcheck_stms fab.LAst.statements tenv ctype lenv' in
  let return'     = tcheck_return_stm fab.LAst.return tenv ctype lenv' rettype in
  { TAst.formals    = formals';
    TAst.locals     = locals';
    TAst.statements = statements';
    TAst.return     = return';
  }

(*  tcheck_formals_and_body_opt : LAst.formals_and_body option -> 
           Types.class_type Env -> Types.class_type -> Types.typeexp ->
                                                         TAst.formals_and_body *)
let tcheck_formals_and_body_opt fab tenv ctype rettype = match fab with
  | None -> None
  | Some fab ->
    Some (tcheck_formals_and_body fab tenv ctype rettype)

(*  tcheck_constructor_decl : LAst.constructor_decl -> Types.class_type Env -> 
                                    Types.class_type ->  TAst.constructor_decl *)
let tcheck_constructor_decl cdecl tenv ctype =
  let constructor_name = cdecl.LAst.constructor_name in
  if constructor_name.Ast.identifier <> ctype.Types.class_name
  then
    let pos = constructor_name.Ast.identifier_pos in
    Error.error pos "Constructor must have the same name as its enclosing class"
  else
    let formals_and_body = cdecl.LAst.constructor_formals_and_body in
    let formals_and_body' = 
      tcheck_formals_and_body formals_and_body tenv ctype Types.Void in
  { TAst.constructor_name             = cdecl.LAst.constructor_name;
    TAst.constructor_formals_and_body = formals_and_body';
  }

(*  tcheck_method_decl : LAst.method_decl -> Types.class_type Env -> 
                                         Types.class_type ->  TAst.method_decl *)
let tcheck_method_decl mdecl tenv ctype =
  let rettype = mdecl.LAst.method_return_type in
  let formals_and_body = mdecl.LAst.method_formals_and_body in
  let formals_and_body' =
    tcheck_formals_and_body formals_and_body tenv ctype rettype in
  { TAst.method_return_type      = rettype;
    TAst.method_name             = mdecl.LAst.method_name;
    TAst.method_formals_and_body = formals_and_body';
  }

(*  tcheck_method_decl : LAst.method_decl list -> Types.class_type Env -> 
                                         Types.class_type ->  TAst.method_decl *)
let rec tcheck_method_decls mdecls tenv ctype = match mdecls with
  | [] -> []
  | mdecl::mdecls ->
    let mdecl'  = tcheck_method_decl mdecl tenv ctype in
    let mdecls' = tcheck_method_decls mdecls tenv ctype in
    mdecl'::mdecls'

(*  tcheck_class_decl : LAst.class_decl -> Types.class_type Env -> string ->
                                                               TAst.class_decl *)
let tcheck_class_decl cdecl tenv filename =
  let classname = cdecl.LAst.class_name in
  let pos       = classname.Ast.identifier_pos in
  let expected_name = classname.Ast.identifier ^ ".java" in
  if expected_name <> Filename.basename filename
  then
    Error.error pos ("The public class '" ^ classname.Ast.identifier ^ "' " ^
			"must be declared in a file called " ^ expected_name)
  else
    let class_type   = 
      Environment.lookup_env tenv "class" classname.Ast.identifier pos in
    let fields'      = 
      tcheck_field_decls cdecl.LAst.class_fields tenv class_type in
    let constructor' = 
      tcheck_constructor_decl cdecl.LAst.class_constructor tenv class_type in
    let main'        = 
      tcheck_formals_and_body_opt cdecl.LAst.class_main tenv class_type Types.Void in
    let methods'     = 
      tcheck_method_decls cdecl.LAst.class_methods tenv class_type in
    { TAst.class_name        = classname;
      TAst.class_fields      = fields';
      TAst.class_constructor = constructor';
      TAst.class_main        = main';
      TAst.class_methods     = methods';
    }

(*  tcheck_src_file : Ast.source_file -> Types.class_type Env -> TAst.source_file *)
let tcheck_src_file src_file tenv =
  let filename = src_file.LAst.source_file_name in
  let cdecl' = tcheck_class_decl src_file.LAst.source_file_decl tenv filename in
  { TAst.source_file_name = filename;
    TAst.source_file_decl = cdecl';
  }

(*  tcheck_program : Ast.program -> Types.class_type Env -> TAst.program *)
let rec tcheck_program prog tenv = match prog with
  | [] -> []
  | src_file::src_files ->
    let src_file'  = tcheck_src_file src_file tenv in
    let src_files' = tcheck_program src_files tenv in
    src_file'::src_files'
