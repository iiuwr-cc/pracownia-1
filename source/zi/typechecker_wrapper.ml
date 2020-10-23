open Zi_lib
open Iface

module MakeTypecheckerToolbox
    (LexerAndParser : LEXER_AND_PARSER)
    (ErrorReporter : Typechecker_errors.ERROR_REPORTER)
    (IC : INTERFACE_CHECKER) : TYPECHECKER_TOOLBOX = struct
  module Parser_wrapper = Parser_wrapper.Make (LexerAndParser)
  module ErrorReporter = ErrorReporter

  let dummy_interface = Ast.IdMap.empty

  let zi_interface_extension = ".zii"

  let zi_lib_dir = "zisdk/lib"

  let parse_and_check_interface ~loc ifile =
    match Parser_wrapper.parse_interface ifile with
    | Ok intf -> IC.check_interface intf
    | Error (loc, descr) ->
        ErrorReporter.report_other_error ~loc ~descr;
        dummy_interface

  let find_and_check_interface ~loc iname =
    let ifile = iname ^ zi_interface_extension in
    let (Ast.Location { file; _ }) = loc in
    let program_dir = Filename.dirname file in
    if Sys.file_exists (Filename.concat program_dir ifile) then
      parse_and_check_interface ~loc (Filename.concat program_dir ifile)
    else if Sys.file_exists (Filename.concat zi_lib_dir ifile) then
      parse_and_check_interface ~loc (Filename.concat zi_lib_dir ifile)
    else (
      ErrorReporter.report_other_error ~loc
        ~descr:(Printf.sprintf "Could not locate interface of %s module" iname);
      dummy_interface )
end

module WrapTypechecker
    (LexerAndParser : LEXER_AND_PARSER)
    (MakeInterfaceChecker : Zi_lib.Plugin.MAKE_INTERFACE_CHECKER)
    (MakeTypechecker : Zi_lib.Plugin.MAKE_TYPECHECKER) : TYPECHECKER_STEP =
struct
  let check_module mdef =
    let module ErrorReporter = Typechecker_errors.MakeErrorReporter () in
    let module InterfaceChecker = MakeInterfaceChecker (ErrorReporter) in
    let module Toolbox =
      MakeTypecheckerToolbox (LexerAndParser) (ErrorReporter) (InterfaceChecker)
    in
    let module Typechecker = MakeTypechecker (Toolbox) in
    ErrorReporter.wrap Typechecker.check_module mdef
end
