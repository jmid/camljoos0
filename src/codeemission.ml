(** Final compiler phase to emit the generated code for all classes and
    interfaces to jasmin files. *)

module LAst = Limitsast
module Inst = Instruction

(* ********************************************************************** *)
(* HELPER FUNCTIONS                                                       *)
(* ********************************************************************** *)

let output_line ch str =
  begin
    output_string ch str;
    output_char ch '\n';
    flush ch
  end

let output_newline ch =
  output_char ch '\n';
  flush ch

let make_sig method_name fullsig =
  method_name ^ (Str.string_after fullsig (String.index fullsig '('))

(* ********************************************************************** *)
(* CODE GENERATION TRAVERSAL                                              *)
(* ********************************************************************** *)

(*  emit_instruction : Inst.instruction -> out_channel -> unit  *)
let emit_instruction inst ch =
  output_line ch (Inst.to_asm inst)

(*  emit_instruction : Inst.instruction list -> out_channel -> unit  *)
let rec emit_instructions insts ch = match insts with
  | [] -> ()
  | inst::insts ->
    begin
      emit_instruction inst ch;
      emit_instructions insts ch
    end

(*  emit_field_decl : field_decl -> out_channel -> unit  *)
let emit_field_decl fdecl ch =
  let field_name = fdecl.LAst.field_name.Ast.identifier in
  let field_type = Types.typeexp_to_sig fdecl.LAst.field_type in
  output_line ch (".field protected \"" ^ field_name ^ "\" " ^ field_type)


(*  emit_field_decls : field_decl list -> out_channel -> unit  *)
let rec emit_field_decls fdecls ch = match fdecls with
  | [] -> ()
  | fdecl::fdecls ->
    begin
      emit_field_decl fdecl ch;
      emit_field_decls fdecls ch;
    end

(*  emit_formals_and_body : formals_and_body -> out_channel -> unit  *)
let emit_formals_and_body fab ch = 
  begin
    output_line ch (".limit stack " ^ (string_of_int fab.LAst.max_stack));
    output_line ch (".limit locals " ^ (string_of_int fab.LAst.max_locals));
    emit_instructions fab.LAst.body ch;
  end

(*  emit_main_opt : formals_and_body option -> out_channel -> unit  *)
let emit_main_opt fab ch = match fab with
  | None -> ()
  | Some fab ->
    begin
      output_newline ch;
      output_line ch ".method public static main([Ljava/lang/String;)V";
      output_line ch ".throws java/lang/Exception";
      emit_formals_and_body fab ch;
      output_line ch ".end method";
    end

(*  emit_constructor_decl : constructor_decl -> out_channel -> unit  *)
let emit_constructor_decl cdecl ch =
  let csig = cdecl.LAst.constructor_signature in
  let initsig = make_sig "<init>" csig in (* chop off full sig appropriately *)
  begin
    output_newline ch;
    output_line ch (".method public " ^ initsig);
    output_line ch ".throws java/lang/Exception";
    emit_formals_and_body cdecl.LAst.constructor_formals_and_body ch;
    output_line ch ".end method";
  end

(*  emit_method_decl : LAst.method_decl -> out_channel -> unit  *)
let emit_method_decl mdecl ch =
  let method_name = mdecl.LAst.method_name.Ast.identifier in
  let fullsig = mdecl.LAst.method_signature in
  let msig = make_sig method_name fullsig in (* chop off full sig appropriately *)
  begin
    output_newline ch;
    output_line ch (".method public " ^ msig);
    output_line ch ".throws java/lang/Exception";
    emit_formals_and_body mdecl.LAst.method_formals_and_body ch;
    output_line ch ".end method";
  end

(*  emit_method_decls : LAst.method_decl list -> out_channel -> unit  *)
let rec emit_method_decls mdecls ch = match mdecls with
  | [] -> ()
  | mdecl::mdecls ->
    begin
      emit_method_decl mdecl ch;
      emit_method_decls mdecls ch;
    end

(*  emit_class_decl : LAst.class_decl -> out_channel -> unit  *)
let emit_class_decl cdecl ch =
  begin
    output_line ch (".class public " ^ (cdecl.LAst.class_name.Ast.identifier));
    output_line ch (".super java/lang/Object");
    emit_field_decls cdecl.LAst.class_fields ch;
    emit_constructor_decl cdecl.LAst.class_constructor ch;
    emit_main_opt cdecl.LAst.class_main ch;
    emit_method_decls cdecl.LAst.class_methods ch;
  end

(*  emit_src_file : LAst.source_file -> unit  *)
let emit_src_file src_file =
  let src_name  = src_file.LAst.source_file_name in
  let file_name = (Filename.chop_extension src_name) ^ ".j" in
  try
    let ch = open_out file_name in
    begin
      output_line ch (".source " ^ (Filename.basename src_name));
      emit_class_decl src_file.LAst.source_file_decl ch;
      close_out ch
    end
  with Sys_error msg ->
    Error.error Lexing.dummy_pos ("Unable to open file " ^ msg)

(*  emit_src_files : LAst.source_file list -> unit  *)
let rec emit_src_files src_files = match src_files with
  | [] -> ()
  | src_file::src_files ->
    begin
      emit_src_file src_file;
      emit_src_files src_files;
    end

(*  emit_program : LAst.program -> unit  *)
let emit_program prog = emit_src_files prog
