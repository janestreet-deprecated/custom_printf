(* OASIS_START *)
(* OASIS_STOP *)

let dispatch = function
  | Before_options ->
    Options.make_links := false
  | After_rules ->
    let tag = "pa_custom_printf" and file = "syntax/pa_custom_printf.cmo" in
    flag ["ocaml"; "compile"; tag] & S[A"-ppopt"; A file];
    flag ["ocaml"; "ocamldep"; tag] & S[A"-ppopt"; A file];
    flag ["ocaml"; "doc"; tag] & S[A"-ppopt"; A file];
    dep ["ocaml"; "ocamldep"; tag] [file];

    let env = BaseEnvLight.load () in
    let ver = BaseEnvLight.var_get "ocaml_version" env in
    let ver = Scanf.sscanf ver "%d.%d" (fun major minor -> (major, minor)) in
    if ver >= (4, 02) then begin
      flag ["ocaml"; "compile"; "use_macro"] & S[A"-ppopt"; A "-DOCAML_4_02"];
      flag ["ocaml"; "ocamldep"; "use_macro"] & S[A"-ppopt"; A "-DOCAML_4_02"];
    end

  | _ ->
    ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook)
