(** Compiler phase to generate Java bytecode for all
    method bodies.

    Code generation for a statement or local variable declaration
    leaves the stack height unchanged.

    Code generation for an expression pushes the result
    of the expression onto the stack if the type of
    the expression is non-void, or leaves the stack height
    unchanged if the type of the expression is void.
*)

module TAst = Typecheckingast
module RAst = Resourceast
module CAst = Codegenerationast
module Inst = Instruction

(************************************************************************)
(** {2 Helper Functions }                                               *)
(************************************************************************)

let codegen_method_sig_known msig numargs numreturns =
  { Inst.method_sig      = msig;
    Inst.method_nargs    = numargs;
    Inst.method_nreturns = numreturns
  }

let codegen_method_sig id base m =
  let mname   = id.Ast.identifier in
  let basesig = Types.cname_to_sig base in
  let argsigs = List.map Types.typeexp_to_sig m.Types.method_formals in
  let ressig  = Types.typeexp_to_sig m.Types.method_result in
  let numargs = List.length m.Types.method_formals in
  let numreturns = if m.Types.method_result = Types.Void then 0 else 1 in
  let msig =
    basesig ^ "/" ^ mname ^ "(" ^ (String.concat "" argsigs) ^ ")" ^ ressig in
  codegen_method_sig_known msig numargs numreturns

let codegen_constructor_sig base c =
    let basesig = Types.cname_to_sig base in
    let argsigs = List.map Types.typeexp_to_sig c.Types.constructor_formals in
    let numargs = List.length c.Types.constructor_formals in
    let msig    = basesig ^ "/<init>(" ^ (String.concat "" argsigs) ^ ")V" in
    codegen_method_sig_known msig numargs 0

let codegen_cond op = match op with 
  | TAst.Eq -> Inst.Eq
  | TAst.Ne -> Inst.Ne
  | TAst.Lt -> Inst.Lt
  | TAst.Le -> Inst.Le
  | TAst.Gt -> Inst.Gt
  | TAst.Ge -> Inst.Ge
  | TAst.Aeq -> Inst.Aeq
  | TAst.Ane -> Inst.Ane
  | _ -> raise (Error.InternalCompilerError "Illegal cond in binary operation")

(************************************************************************)
(** {2 Code Generation Traversal }                                      *)
(************************************************************************)

type info = { tenv             : Types.class_type Types.Env.t;
	      class_type       : Types.class_type;
	      nonstatic_fields : RAst.field_decl list }

(*  codegen_lvalue_read : RAst.lvalue -> info -> Inst.instruction list  *)
let rec codegen_lvalue_read lvalue info = match lvalue.RAst.lvalue with
  | RAst.Field (id,ftype) -> 
    let fieldname = id.Ast.identifier in
    let fieldtype = Types.typeexp_to_sig lvalue.RAst.lvalue_type in
    let basesig   = info.class_type.Types.class_name in
    [Inst.Iaload 0;
     Inst.Igetfield (basesig ^ "/" ^ fieldname ^ " " ^ fieldtype)]
  | RAst.Local (id,i) -> 
    (match lvalue.RAst.lvalue_type with
      | Types.Int
      | Types.Boolean -> [Inst.Iiload i] (* integer load *)
      | Types.String 
      | Types.Class _ -> [Inst.Iaload i] (* address load *)
      | _ -> raise (Error.InternalCompilerError "Illegal type of lvalue") )


(*  codegen_lvalue_write : RAst.lvalue -> info -> Inst.instruction list  *)
and codegen_lvalue_write lvalue info =
  match lvalue.RAst.lvalue with
    | RAst.Field (id,base) ->
      let fieldname = id.Ast.identifier in
      let fieldtype = Types.typeexp_to_sig lvalue.RAst.lvalue_type in
      let basesig   = info.class_type.Types.class_name in
      [Inst.Iaload(0);
       Inst.Iswap;
       Inst.Iputfield (basesig ^ "/" ^ fieldname ^ " " ^ fieldtype)]
    | RAst.Local (id,i) -> 
      (match lvalue.RAst.lvalue_type with
	| Types.Int
	| Types.Boolean  -> [Inst.Iistore i] (* integer store *)
	| Types.String 
	| Types.Class _  -> [Inst.Iastore i] (* address store *)
	| _ -> raise (Error.InternalCompilerError "Illegal type of lvalue") )


(*  codegen_exp : exp -> info -> instruction list  *)
and codegen_exp exp info = match exp.RAst.exp with
  | RAst.Binop (e1,op,e2) ->
    let ie1 = codegen_exp e1 info in
    let ie2 = codegen_exp e2 info in
    (match op with
      | TAst.Plus   -> List.concat [ie1; ie2; [Inst.Iiadd]]
      | TAst.Minus  -> List.concat [ie1; ie2; [Inst.Iisub]]
      | TAst.Times  -> List.concat [ie1; ie2; [Inst.Iimul]]
      | TAst.Divide -> List.concat [ie1; ie2; [Inst.Iidiv]]
      | TAst.Modulo -> List.concat [ie1; ie2; [Inst.Iirem]]
      | TAst.And    -> List.concat [ie1; ie2; [Inst.Iiand]]
      | TAst.Or     -> List.concat [ie1; ie2; [Inst.Iior]]
      | TAst.Xor    -> List.concat [ie1; ie2; [Inst.Iixor]]
      | TAst.Eq    
      | TAst.Ne    
      | TAst.Lt
      | TAst.Le
      | TAst.Gt
      | TAst.Ge
      | TAst.Aeq
      | TAst.Ane    ->
	let cond = codegen_cond op in
	let truel = Inst.make_label "true" in
	let endl = Inst.make_label "end" in
	List.concat [ie1;
		     ie2;
		     [Inst.Iifcmp(cond,truel);
		      Inst.Ildc_int 0l;
		      Inst.Igoto endl;
		      Inst.Ilabel truel;
		      Inst.Ildc_int 1l;
		      Inst.Ilabel endl]]
      | TAst.Concat ->
	List.concat [ie1;
		     ie2;
		     [Inst.Iinvokevirtual
			 (codegen_method_sig_known 
			    "java/lang/String/concat(Ljava/lang/String;)Ljava/lang/String;" 1 1)]]
    )
  | RAst.Unop (op,e) ->
    let ie = codegen_exp e info in
    (match op with
      | TAst.Negate     -> List.concat [ie; [Inst.Iineg]]
      | TAst.Complement ->
	  let truel = Inst.make_label "true" in
	  let endl = Inst.make_label "end" in
	  List.concat [ie; [Inst.Iif(Inst.Eq,truel);
			    Inst.Ildc_int 0l;
			    Inst.Igoto endl;
			    Inst.Ilabel truel;
			    Inst.Ildc_int 1l;
			    Inst.Ilabel endl]]
      | TAst.BooleanToString ->
	let msig = "java/lang/String/valueOf(Z)Ljava/lang/String;" in
	List.concat [ie; 
		     [Inst.Iinvokestatic (codegen_method_sig_known msig 1 1) ]]
      | TAst.IntToString ->
	let msig = "java/lang/String/valueOf(I)Ljava/lang/String;" in
	List.concat [ie; 
		     [Inst.Iinvokestatic (codegen_method_sig_known msig 1 1) ]]
      | TAst.CharToString ->
	let msig = "java/lang/String/valueOf(C)Ljava/lang/String;" in
	List.concat [ie; 
		     [Inst.Iinvokestatic (codegen_method_sig_known msig 1 1) ]]
      | TAst.ObjectToString ->
	let msig = 
	  "java/lang/String/valueOf(Ljava/lang/Object;)Ljava/lang/String;" in
	List.concat [ie; 
		     [Inst.Iinvokestatic (codegen_method_sig_known msig 1 1) ]]
    )
  | RAst.IntConst i     -> [Inst.Ildc_int i]
  | RAst.StringConst s  -> [Inst.Ildc_string s]
  | RAst.BooleanConst b -> [Inst.Ildc_int (if b then 1l else 0l)]
  | RAst.Null           -> [Inst.Iaconst_null]
  | RAst.This           -> [Inst.Iaload 0]
  | RAst.Invoke (e,id,es,mtype) ->
    let ie = codegen_exp e info in
    let ies = List.concat (List.map (fun e -> codegen_exp e info) es) in
    let recvtype = e.RAst.exp_type in
    let msig = (codegen_method_sig id (Types.typeexp_to_string recvtype) mtype) in
    let invokeinst = [Inst.Iinvokevirtual msig] in
    List.concat [ie;
		 ies;
		 invokeinst]
  | RAst.New (typ,es,c) ->
    let ies = List.concat (List.map (fun e -> codegen_exp e info) es) in
    let typesig = Types.typeexp_to_string exp.RAst.exp_type in
    List.concat [[Inst.Inew typesig;
		  Inst.Idup];
		 ies;
		 [Inst.Iinvokespecial (codegen_constructor_sig typesig c)]]
  | RAst.Lvalue lval ->
    codegen_lvalue_read lval info
  | RAst.Assignment (lval,e) ->
    let ie = codegen_exp e info in
    let writeinst = codegen_lvalue_write lval info in
    List.concat [ie;
		 [Inst.Idup];
		 writeinst]
  | RAst.Print e ->
    let ie      = codegen_exp e info in
    let argtype = (match e.RAst.exp_type with
      | Types.Int -> "I"
      | Types.Boolean -> "Z"
      | Types.String
      | Types.Class _ -> "Ljava/lang/Object;"
      | _ -> raise (Error.InternalCompilerError "Illegal print type") ) in
    let msig = "java/io/PrintStream/print(" ^ argtype ^ ")V" in
    List.concat [ie;
		 [Inst.Igetstatic "java/lang/System/out Ljava/io/PrintStream;";
		  Inst.Iswap;
		  Inst.Iinvokevirtual (codegen_method_sig_known msig 1 0)]]
  | RAst.Read ->
    (* Generate code as if System.in.read() was called. *)
    [Inst.Igetstatic "java/lang/System/in Ljava/io/InputStream;";
     Inst.Iinvokevirtual (codegen_method_sig_known "java/io/InputStream/read()I" 0 1)]

(*  codegen_stm : stm -> info -> instruction list  *)
let rec codegen_stm stm info = match stm.RAst.stm with
  | RAst.Exp e ->
    let ie = codegen_exp e info in
    (match e.RAst.exp_type with
      | Types.Void -> ie
      | _ -> List.concat [ie;
			  [Inst.Ipop]] )
  | RAst.IfThen (e,s) ->
    let falsel = Inst.make_label "false" in
    let ie = codegen_exp e info in
    let is = codegen_stm s info in
    List.concat
      [ie;
       [Inst.Iif (Inst.Eq,falsel)];
       is;
       [Inst.Ilabel falsel;]]
  | RAst.IfThenElse (e,s1,s2) ->
    let falsel = Inst.make_label "false" in
    let endifl = Inst.make_label "endif" in
    let ie  = codegen_exp e info in
    let is1 = codegen_stm s1 info in
    let is2 = codegen_stm s2 info in
    List.concat 
      [ie;
       [Inst.Iif (Inst.Eq,falsel)];
       is1;
       [Inst.Igoto endifl;
	Inst.Ilabel falsel];
       is2;
       [Inst.Ilabel endifl;]]
  | RAst.While (e,s) ->
    let loopl = Inst.make_label "loop" in
    let condl = Inst.make_label "cond" in
    let ie = codegen_exp e info in
    let is = codegen_stm s info in
    List.concat
      [[Inst.Igoto condl;
	Inst.Ilabel loopl];
       is;
       [Inst.Ilabel condl];
       ie;
       [Inst.Iif (Inst.Ne,loopl)]]
  | RAst.Empty -> []
  | RAst.Block b ->
    codegen_stm_list b info

(*  codegen_stm_list : stm list -> info -> instruction list  *)
and codegen_stm_list stms info =
  let inst_lists = List.map (fun stm -> codegen_stm stm info) stms in
  List.concat inst_lists
  
(*  codegen_return_stm : return_stm -> info -> instruction list  *)
let codegen_return_stm rstm info = match rstm.RAst.return_stm with
  | RAst.VoidReturn -> [Inst.Ireturn]
  | RAst.ValueReturn e ->
    let ie = codegen_exp e info in
    (match e.RAst.exp_type with
      | Types.Int
      | Types.Boolean -> 
	List.concat [ie;
		     [Inst.Iireturn]]
      | Types.String
      | Types.Class _
      | Types.Null ->
	List.concat [ie;
		     [Inst.Iareturn]]
      | Types.Void ->
	raise (Error.InternalCompilerError "Illegal type of return expression"))

(*  codegen_local_decl : RAst.local_decl -> info -> Inst.instruction list  *)
let codegen_local_decl ldecl info =
  let (typ,id,exp,offset) = ldecl in
  let is = codegen_exp exp info in
  match typ with
    | Types.Int
    | Types.Boolean -> 
      List.concat [is;
		   [Inst.Iistore offset]]  (* integer store *)
    | Types.Class _
    | Types.String ->
      List.concat [is;
		   [Inst.Iastore offset]]  (* address store *)
    | Types.Void
    | Types.Null ->
      raise (Error.InternalCompilerError "Illegal type of local initializer")

(* codegen_local_decls : RAst.local_decl list -> info -> Inst.instruction list  *)
let rec codegen_local_decls ldecls info = match ldecls with
  | [] -> []
  | ldecl::ldecls ->
    let ldecl_is  = codegen_local_decl ldecl info in
    let ldecls_is = codegen_local_decls ldecls info in
    ldecl_is @ ldecls_is

(*  codegen_field_decl : RAst.field_decl -> info -> CAst.field_decl  *)
let codegen_field_decl fdecl info =
  { CAst.field_type      = fdecl.RAst.field_type;
    CAst.field_name      = fdecl.RAst.field_name;
    CAst.field_signature = fdecl.RAst.field_signature; }

(*  codegen_field_decls : RAst.field_decl list -> info -> CAst.field_decl list  *)
let rec codegen_field_decls fdecls info = match fdecls with
  | [] -> []
  | fdecl::fdecls ->
    let fdecl'  = codegen_field_decl fdecl info in
    let fdecls' = codegen_field_decls fdecls info in
    fdecl'::fdecls'

(*  codegen_formals_and_body : RAst.formals_and_body -> info -> CAst.formals_and_body *)
let codegen_formals_and_body fab info =
  let local_init = codegen_local_decls fab.RAst.locals info in
  let body'      = codegen_stm_list fab.RAst.statements info in
  let return'    = codegen_return_stm fab.RAst.return info in
  let insts      = List.concat [local_init;
				body';
				return'] in
  { CAst.formals = fab.RAst.formals;
    CAst.body    = insts }

(*  codegen_formals_and_body_opt : RAst.formals_and_body option -> info
                                                      -> CAst.formals_and_body option *)
let codegen_formals_and_body_opt fab info = match fab with
  | None -> None
  | Some fab -> 
    let fab' = codegen_formals_and_body fab info in
    Some fab'

(*  codegen_constructor_decl : RAst.constructor_decl -> info -> CAst.constructor_decl *)
let codegen_constructor_decl cdecl info =
  let fab' = codegen_formals_and_body cdecl.RAst.constructor_formals_and_body info in
  let supercall = [Inst.Iaload 0;
		   Inst.Iinvokespecial 
		     (codegen_method_sig_known "java/lang/Object/<init>()V" 0 0)
		  ] in
  let field_init =
      List.fold_right (fun fdecl flist ->
	match fdecl.RAst.field_init with
	  | None -> flist
	  | Some e -> 
	    let ie = codegen_exp e info in
	    let fieldsig = fdecl.RAst.field_signature
	             ^ " " ^ (Types.typeexp_to_sig fdecl.RAst.field_type) in
	    List.concat [[Inst.Iaload 0];
			 ie;
			 [Inst.Iputfield fieldsig];
			 flist]) info.nonstatic_fields [] in
  let insts = List.concat [supercall;
			   field_init;
			   fab'.CAst.body] in
  { CAst.constructor_name             = cdecl.RAst.constructor_name;
    CAst.constructor_formals_and_body = { fab' with CAst.body = insts };
    CAst.constructor_signature        = cdecl.RAst.constructor_signature }

(*  codegen_method_decl : RAst.method_decl -> info -> CAst.method_decl *)
let codegen_method_decl mdecl info =
  let fab' = codegen_formals_and_body mdecl.RAst.method_formals_and_body info in
  { CAst.method_return_type      = mdecl.RAst.method_return_type;
    CAst.method_name             = mdecl.RAst.method_name;
    CAst.method_formals_and_body = fab';
    CAst.method_signature        = mdecl.RAst.method_signature }

(*  codegen_method_decls : RAst.method_decl list -> info -> CAst.method_decl list *)
let rec codegen_method_decls mdecls info = match mdecls with
  | [] -> []
  | mdecl::mdecls ->
    let mdecl' = codegen_method_decl mdecl info in
    let mdecls' = codegen_method_decls mdecls info in
    mdecl'::mdecls'

(*  codegen_class_decl : RAst.class_decl -> Types.class_type Env -> CAst.class_decl *)
let codegen_class_decl cdecl tenv =
  let class_name = cdecl.RAst.class_name in
  let class_type = 
    Environment.lookup_env tenv "class" class_name.Ast.identifier class_name.Ast.identifier_pos in
  let info    = { tenv             = tenv;
		  class_type       = class_type;
		  nonstatic_fields = cdecl.RAst.class_fields } in
  let fields' = codegen_field_decls cdecl.RAst.class_fields info in
  let main'   = codegen_formals_and_body_opt cdecl.RAst.class_main info in
  let mdecls' = codegen_method_decls cdecl.RAst.class_methods info in
  let cdecl'  = codegen_constructor_decl cdecl.RAst.class_constructor info in
  { CAst.class_name           = cdecl.RAst.class_name;
    CAst.class_fields         = fields';
    CAst.class_constructor    = cdecl';
    CAst.class_main           = main';
    CAst.class_methods        = mdecls';
    CAst.class_decl_signature = cdecl.RAst.class_decl_signature; }

(*  codegen_src_file : RAst.source_file -> Types.class_type Env -> CAst.source_file *)
let codegen_src_file src_file tenv =
  let cdecl' = codegen_class_decl src_file.RAst.source_file_decl tenv in
  { CAst.source_file_name = src_file.RAst.source_file_name;
    CAst.source_file_decl = cdecl' }

(*  codegen_src_files : RAst.source_file list -> Types.class_type Env -> CAst.source_file list *)
let rec codegen_src_files src_files tenv = match src_files with
  | [] -> []
  | src_file::src_files ->
    let src_file' = codegen_src_file src_file tenv in
    let src_files' = codegen_src_files src_files tenv in
    src_file'::src_files'

(*  codegen_program : RAst.program -> Types.class_type Env -> CAst.program  *)
let codegen_program prog tenv = codegen_src_files prog tenv
