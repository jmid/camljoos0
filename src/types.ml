(* Structure/signature of Java types. *)

(* ******************* Type Environment *********************** *)

module Env = Map.Make (String)


type identifier = string

type typeexp = 
  | Void
  | Int
  | Boolean
  | String
  | Class of identifier
  | Null

type field_type =
    { field_type     : typeexp;
      field_name     : identifier; }

type method_type =
    { method_result   : typeexp;
      method_name     : identifier;
      method_formals  : typeexp list; }

type constructor_type =
    { constructor_name    : identifier;
      constructor_formals : typeexp list; }

type class_type =
    { class_name        : identifier;
      class_fields      : field_type list;
      class_constructor : constructor_type;
(*      class_main        : method_type option;*)
      class_methods     : method_type list; }

(* map from identifier to named_type *)
(* type type_env = class_type Env.t *)

(* ******************** Helpers ******************** *)

let dots_to_slashes s =
  let dotregexp = Str.regexp "\\." in
  let ss = Str.split dotregexp s in
  String.concat "/" ss

let cname_to_sig cn = dots_to_slashes cn

let rec typeexp_to_string t = match t with
  | Void    -> "V"
  | Int     -> "I"
  | Boolean -> "Z"
  | String  -> "java.lang.String";
  | Class c -> (*cname_to_sig*) c
  | Null    ->
    raise (Error.InternalCompilerError "The null type has no signature")

let rec typeexp_to_sig t = match t with
  | Void
  | Int
  | Boolean -> typeexp_to_string t
  | String
  | Class _ -> let s = typeexp_to_string t in
	       "L" ^ (cname_to_sig s) ^ ";"
  | Null    ->
    raise (Error.InternalCompilerError "The null type has no signature")
