(* ********************************************************************** *)
(* AST TYPE FROM PARSETREE                                                *)
(* ********************************************************************** *)

type identifier = Ast.identifier

(* *************** Types *************** *)

type typeexp = Types.typeexp

(* *************** Operators *************** *)

type binop = Ast.binop
type unop = Ast.binop

(* *************** Expressions *************** *)

type lvalue = Ast.lvalue
type exp    = Ast.exp

(* *************** Blocks and statements *************** *)

type stm        = Ast.stm
type return_stm = Ast.return_stm

(* *************** Class members *************** *)

type formal_param = typeexp * identifier
type local_decl   = typeexp * identifier * exp

type field_decl	=	
    { field_type : typeexp;
      field_name : identifier;
      field_init : exp option; }

type formals_and_body =
    { formals    : formal_param list;
      locals     : local_decl list;
      statements : stm list;
      return     : return_stm; }

type constructor_decl =
    { constructor_name             : identifier;
      constructor_formals_and_body : formals_and_body; }

type main_decl = formals_and_body

type method_decl =
    { method_return_type      : typeexp;
      method_name             : identifier;
      method_formals_and_body : formals_and_body; }


(* *************** Class and source file *************** *)

type class_decl	=
    { class_name        : identifier;
      class_fields      : field_decl list;
      class_constructor : constructor_decl;
      class_main        : main_decl option;
      class_methods     : method_decl list; }

type source_file = 
    { source_file_name : string;
      source_file_decl : class_decl }

type program = source_file list
