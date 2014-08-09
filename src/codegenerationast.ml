(* ********************************************************************** *)
(* AST TYPE FROM CODEGENERATION                                           *)
(* ********************************************************************** *)

module TAst = Typecheckingast
module Inst = Instruction

type identifier = Ast.identifier

(* *************** Types *************** *)
type typeexp = Types.typeexp

(* *************** Class members *************** *)

type formal_param = typeexp * identifier * int (*NEW*)
(*type local_decl   = typeexp * identifier (* * exp *) * int (*NEW*)*)

type field_decl	=	
    { field_type      : typeexp;
      field_name      : identifier;
      (*field_init      : exp option;*)
      field_signature : string (*NEW*) }

type formals_and_body =
    { formals    : formal_param list;
(*      locals     : local_decl list; *)
      body       : Inst.instruction list; }

type constructor_decl =
    { constructor_name             : identifier;
      constructor_formals_and_body : formals_and_body;
      constructor_signature        : string (*NEW*) }

type main_decl = formals_and_body

type method_decl =
    { method_return_type      : typeexp;
      method_name             : identifier;
      method_formals_and_body : formals_and_body;
      method_signature        : string (*NEW*) }


(* *************** Class and source file *************** *)

type class_decl	=
    { class_name           : identifier;
      class_fields         : field_decl list;
      class_constructor    : constructor_decl;
      class_main           : main_decl option;
      class_methods        : method_decl list;
      class_decl_signature : string (*NEW*) }

type source_file = 
    { source_file_name : string;
      source_file_decl : class_decl }

type program = source_file list
