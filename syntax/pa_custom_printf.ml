open Camlp4.PreCast

open StdLabels

let failwithf loc fmt = Printf.ksprintf (fun s () -> Loc.raise loc (Failure s)) fmt

(* returns the index of the conversion spec (unless the end of string is reached) *)
let rec skip_over_format_flags fmt i =
  if i >= String.length fmt
  then `Eoi
  else match fmt.[i] with
  | '*' | '#' | '-' | ' ' | '+' | '_' | '0'..'9' | '.' ->
    skip_over_format_flags fmt (i + 1)
  | _ -> `Ok i

(* doesn't check to make sure the format string is well-formed *)
(* Formats with subformats are skipped for the following reasons:

   One is that they are hard to understand and not often used.

   Another is that subformats like "%(%{Module}%)" won't work, since it
   is impossible to produce a format of type [(Module.t -> 'a,...) format].
*)
let has_subformats (fmt:string) =
  let lim = String.length fmt - 1 in
  let rec loop i =
    if i > lim
    then false
    else
      if fmt.[i] = '%' then
        match skip_over_format_flags fmt (i + 1) with
        | `Eoi -> false
        | `Ok i ->
          match fmt.[i] with
          | '(' | ')' | '}' -> true
          | _ -> loop (i + 1)
      else loop (i + 1)
  in
  loop 0

(* returns a list of strings where even indexed elements are parts of the format string
   that the preprocessor won't touch and odd indexed elements are the contents of %{...}
   specifications. *)
let explode loc (s:string) =
  let len = String.length s in
  (* for cases where we can't parse the string with custom format specifiers, consider
     the string as a regular format string *)
  let as_normal_format_string = [s] in
  if has_subformats s
  then as_normal_format_string
  else
    let sub from to_ = String.sub s ~pos:from ~len:(to_ - from) in
    let rec loop acc from to_ =
      assert (List.length acc mod 2 = 0);
      if to_ >= len
      then List.rev (
        if from >= len
        then acc
        else sub from len :: acc
      )
      else
        if s.[to_] <> '%'
        then loop acc from (to_ + 1)
        else
          match skip_over_format_flags s (to_ + 1) with
          | `Eoi -> as_normal_format_string
          | `Ok i ->
            if s.[i] <> '{' then
              loop acc from (i + 1) (* skip the conversion spec *)
            else begin
              if to_ + 1 <> i then begin
                failwithf loc "Unexpected format flags before %%{} specification in %S" s ()
              end;
              let end_index =
                try Some (String.index_from s (to_ + 2) '}') with Not_found -> None
              in
              match end_index with
              | None -> as_normal_format_string
              | Some i ->
                let l =
                  sub (to_ + 2) i
                  :: sub from to_
                  :: acc
                in
                loop l (i + 1) (i + 1)
            end
    in
    loop [] 0 0

let gensym = Pa_type_conv.Gen.gensym ~prefix:"_custom_printf"

let processed_format_string ~exploded_format_string =
  let l =
    let rec loop = function
      | s1 :: _s2 :: l -> s1 :: "%s" :: loop l
      | [s1] -> [s1]
      | [] -> []
    in
    loop exploded_format_string
  in
  String.concat l ~sep:""

module Count : sig
  val num_args_of_format_string : string -> int
end = struct
  open CamlinternalFormatBasics

  type 'a seq =
    | Z : unit seq
    | S : 'a seq -> (_ -> 'a) seq

  let rec int_of_seq : type a. a seq -> int = function
    | S x -> int_of_seq x + 1
    | Z -> 0

  let num_args_of_format_string =
    let rec loop_fmt : type a b c d e f. (a, b, c, d, e, f) fmt -> f seq -> a seq = fun fmt k ->
      match fmt with
      | Char fmt ->
        S (loop_fmt fmt k)
      | Caml_char fmt ->
        S (loop_fmt fmt k)
      | String (padding, fmt) ->
        loop_padding padding (S (loop_fmt fmt k))
      | Caml_string (padding, fmt) ->
        loop_padding padding (S (loop_fmt fmt k))
      | Int (_, padding, precision, fmt) ->
        loop_padding padding (loop_precision precision (S (loop_fmt fmt k)))
      | Int32 (_, padding, precision, fmt) ->
        loop_padding padding (loop_precision precision (S (loop_fmt fmt k)))
      | Nativeint (_, padding, precision, fmt) ->
        loop_padding padding (loop_precision precision (S (loop_fmt fmt k)))
      | Int64 (_, padding, precision, fmt) ->
        loop_padding padding (loop_precision precision (S (loop_fmt fmt k)))
      | Float (_, padding, precision, fmt) ->
        loop_padding padding (loop_precision precision (S (loop_fmt fmt k)))
      | Bool fmt ->
        S (loop_fmt fmt k)
      | Flush fmt ->
        loop_fmt fmt k
      | String_literal (_, fmt) ->
        loop_fmt fmt k
      | Char_literal (_, fmt) ->
        loop_fmt fmt k
      | Format_arg (_, _fmtty, fmt) ->
        S (loop_fmt fmt k)
      | Format_subst (_, fmtty_rel, fmt) ->
        S (loop_fmtty_rel fmtty_rel (loop_fmt fmt k))
      | Alpha fmt ->
        S (S (loop_fmt fmt k))
      | Theta fmt ->
        S (loop_fmt fmt k)
      | Formatting_lit (_, fmt) ->
        loop_fmt fmt k
      | Formatting_gen (Open_tag (Format (tag_fmt, _)), fmt) ->
        loop_fmt tag_fmt (loop_fmt fmt k)
      | Formatting_gen (Open_box (Format (box_fmt, _)), fmt) ->
        loop_fmt box_fmt (loop_fmt fmt k)
      | Reader _ ->
        failwith "reader not supported (Reader)"
      | Scan_char_set _ ->
        failwith "reader not supported (Scan_char_set)"
      | Scan_get_counter _ ->
        failwith "reader not supported (Scan_get_counter)"
      | Ignored_param (_, _fmt) ->
        failwith "reader not supported (Ignored_param)"
      | End_of_format ->
        k
    and loop_padding : type a b. (a, b) padding -> b seq -> a seq = fun p k ->
      match p with
      | No_padding -> k
      | Lit_padding _ -> k
      | Arg_padding _ -> S k
    and loop_precision : type a b. (a, b) precision -> b seq -> a seq = fun p k ->
      match p with
      | No_precision -> k
      | Lit_precision _ -> k
      | Arg_precision -> S k
    and loop_fmtty_rel
      : type a1 b1 c1 d1 e1 f1
               a2 b2 c2 d2 e2 f2.
        (a1, b1, c1, d1, e1, f1,
         a2, b2, c2, d2, e2, f2) fmtty_rel -> f2 seq -> a2 seq = fun fmtty_rel k ->
      match fmtty_rel with
      | Char_ty fmtty_rel ->
        S (loop_fmtty_rel fmtty_rel k)
      | String_ty fmtty_rel ->
        S (loop_fmtty_rel fmtty_rel k)
      | Int_ty fmtty_rel ->
        S (loop_fmtty_rel fmtty_rel k)
      | Int32_ty fmtty_rel ->
        S (loop_fmtty_rel fmtty_rel k)
      | Nativeint_ty fmtty_rel ->
        S (loop_fmtty_rel fmtty_rel k)
      | Int64_ty fmtty_rel ->
        S (loop_fmtty_rel fmtty_rel k)
      | Float_ty fmtty_rel ->
        S (loop_fmtty_rel fmtty_rel k)
      | Bool_ty fmtty_rel ->
        S (loop_fmtty_rel fmtty_rel k)
      | Format_arg_ty (_fmtty, fmtty_rel) ->
        S (loop_fmtty_rel fmtty_rel k)
      | Format_subst_ty (_a, b, c) ->
        S (loop_fmtty_rel b (loop_fmtty_rel c k))
      | Alpha_ty fmtty_rel ->
        S (S (loop_fmtty_rel fmtty_rel k))
      | Theta_ty fmtty_rel ->
        S (loop_fmtty_rel fmtty_rel k)
      | Reader_ty _ ->
        failwith "reader not supported (Reader_ty)"
      | Ignored_reader_ty _ ->
        failwith "reader not supported (Ignored_reader_ty)"
      | End_of_fmtty ->
        k
    in
    fun s ->
      let CamlinternalFormat.Fmt_EBB fmt = CamlinternalFormat.fmt_ebb_of_string s in
      int_of_seq (loop_fmt fmt Z)
end

let num_args loc (format : string) =
  let format = processed_format_string ~exploded_format_string:(explode loc format) in
  Count.num_args_of_format_string format

let get_sexp_of_quote () =
  try
    Some (
      Syntax.Quotation.find "sexp_of" Syntax.Quotation.DynAst.expr_tag
    )
  with
    Not_found -> None

let chop_prefix s ~prefix =
  let prefix_len = String.length prefix in
  if String.length s >= prefix_len
    && String.sub s ~pos:0 ~len:prefix_len = prefix
  then Some (String.sub s ~pos:prefix_len ~len:(String.length s - prefix_len))
  else None

let string_to_expr _loc s =
  match chop_prefix s ~prefix:"sexp:" with
  | Some s ->
    begin match get_sexp_of_quote () with
    | None ->
      failwithf _loc "sexp converter is used in %S, but no \"sexp_of\" quotation could \
                      be found (include pa_sexp?)" s ()
    | Some sexp_of_quote ->
      let e = sexp_of_quote _loc None s in
      let arg = gensym () in
      <:expr<fun $lid:arg$ -> Sexplib.Sexp.to_string_hum ($e$ $lid:arg$)>>
    end
  | None ->
    match Gram.parse_string Syntax.opt_expr _loc s with
    | Ast.ExNil _loc -> <:expr<to_string>>
    | Ast.ExId (_loc,ident) ->
      let l = List.rev (Ast.list_of_ident ident []) in
      let l =
        match List.hd l with
        | Ast.IdUid (_,_) -> Ast.IdLid (_loc,"to_string") :: l
        | Ast.IdLid (_loc,_) as lid -> lid :: Ast.IdUid (_loc,"Format") :: List.tl l
        | _ -> assert false
      in
      Ast.ExId (_loc,Ast.idAcc_of_list (List.rev l))
    | _ ->
      failwithf _loc
        "string %S should be of the form sexp:<type>, <Module>, or <Module>.<identifier>"
        s ()

let apply_exprs _loc expr args =
  List.fold_left ~f:(fun expr arg ->
    <:expr< $expr$ $arg$ >>
  ) ~init:expr args

let abstract_lids _loc lids body =
  List.fold_right ~f:(fun lid body ->
    <:expr< fun $lid:lid$ -> $body$ >>
  ) ~init:body lids

let let_opts _loc lid_expr_list ~in_:body =
  List.fold_right ~f:(fun opt body ->
    match opt with
    | None -> body
    | Some (lid, expr) -> <:expr< let $lid:lid$ = $expr$ in $body$ >>
  ) ~init:body lid_expr_list

let name_to_preserve_side_effects arg =
  let must_be_named = function
    | Ast.ExNil _ (* This happens for [~label], which we treat the same as
                     [~label:any_identifier] *)
    | <:expr< $id:_$ >>
    | <:expr< $int:_$ >>
    | <:expr< $str:_$ >> (* naming a string can break typing when it is
                            actually a format string  *)
      -> false
    | _ -> true
  in
  let maybe_name expr =
    if must_be_named expr
    then
      let loc = Ast.loc_of_expr expr in
      let symbol = gensym () in
      Some (symbol, expr), <:expr@loc< $lid:symbol$ >>
    else None, expr
  in
  match arg with
  | Ast.ExLab (loc, name, expr) ->
    let binding, expr = maybe_name expr in
    binding, Ast.ExLab (loc, name, expr)
  | Ast.ExOlb (loc, name, expr) ->
    let binding, expr = maybe_name expr in
    binding, Ast.ExOlb (loc, name, expr)
  | expr ->
    maybe_name expr

let is_labeled_argument = function
  | Ast.ExLab (_,_,_)
  | Ast.ExOlb (_,_,_) -> true
  | _ -> false

let split_after_n_anon_args l ~n =
  let rec aux acc n l =
    if n = 0 then List.rev acc, l
    else
      match l with
      | [] -> invalid_arg "split"
      | hd :: tl ->
        let n = if is_labeled_argument hd then n else n - 1 in
        aux (hd :: acc) n tl
  in
  aux [] n l

let split_last l =
  match List.rev l with
  | [] -> invalid_arg "split_last"
  | h :: t -> List.rev t, h

let apply _loc fmt_string printf orig_args =
  let num_all_args = num_args _loc fmt_string in
  let let_bindings_opt, fun_bindings, args =
    let orig_unlabeled_args =
      List.filter orig_args ~f:(fun arg -> not (is_labeled_argument arg))
    in
    let num_fun_bindings = num_all_args - List.length orig_unlabeled_args in
    let let_bindings_opt, args =
      List.split (List.map orig_args ~f:name_to_preserve_side_effects)
    in
    if num_fun_bindings <= 0
    then
      let_bindings_opt, [], args
    else
      let fun_bindings = Array.to_list (Array.init num_fun_bindings ~f:(fun (_:int) -> gensym ())) in
      let_bindings_opt, fun_bindings, args @ List.map fun_bindings
        ~f:(fun s -> <:expr<$lid:s$>>)
  in
  let fmt_strings = explode _loc fmt_string in
  let processed_fmt_string =
    processed_format_string ~exploded_format_string:fmt_strings
  in
  let printf_binding_opt, printf = name_to_preserve_side_effects printf in
  let expr = <:expr< $printf$ $str:processed_fmt_string$>> in
  let applied_expr =
    let rec loop expr format_chunks args =
      match format_chunks with
      | [] ->
        (* [args] doesn't have to be empty:  Probably too many arguments are provided, and
           in that case, we also apply too many arguments and there should be a type error
           later.  But it could also be something like

               let f fmt = ksprintf fmt (fun s x y -> ...) in
               f !"%{Time}" time x y
        *)
        apply_exprs _loc expr args
      | normal_format :: rest ->
        let args_format, args = split_after_n_anon_args args ~n:(num_args _loc normal_format) in
        let expr = apply_exprs _loc expr args_format in
        match rest with
        | [] -> apply_exprs _loc expr args
        | custom_format :: format_chunks ->
          let args_format, args = split_after_n_anon_args args ~n:1 in
          let labeled_args, arg = split_last args_format in
          let expr = apply_exprs _loc expr labeled_args in
          let expr = <:expr< $expr$ ($string_to_expr _loc custom_format$ $arg$) >> in
          loop expr format_chunks args
    in
    loop expr fmt_strings args
  in
  let_opts _loc (printf_binding_opt :: let_bindings_opt)
    ~in_:(abstract_lids _loc fun_bindings applied_expr)

let f = object
  inherit Ast.map as super
  method! expr expr =
    let fmt_string = function
      (* camlp4 parses !"literal" as "literal".val  *)
      | <:expr< $str:s$.val >> -> Some s
      | _ -> None
    in
    let rec loop args = function
      | Ast.ExApp (_loc, e1, e2) ->
        begin
        match fmt_string e2 with
        | Some fmt_string ->
          apply _loc fmt_string e1 args
        | None -> loop (e2::args) e1
      end
      | _ -> expr
    in
    super#expr (loop [] expr)
end
in
AstFilters.register_str_item_filter f#str_item;
AstFilters.register_topphrase_filter f#str_item
