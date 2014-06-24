let sprintf = Printf.sprintf
let ksprintf = Printf.ksprintf
open Sexplib.Conv

module Time : sig
  type t
  val now : unit -> t
  val to_string : t -> string
  module Format : sig
    val to_sec_string : t -> string
  end
end = struct
  type t = string
  let now () = "Time.now ()"
  let to_string t = "[Time.to_string (" ^ t ^ ")]"
  module Format = struct
    let to_sec_string t = "[Time.Format.to_sec_string (" ^ t ^ ")]"
  end
end

module Zone : sig
  type t
  val machine_zone : unit -> t
  val to_string : t -> string
end = struct
  type t = string
  let machine_zone () = "Zone.machine_zone ()"
  let to_string t = "[Zone.to_string (" ^ t ^ ")]"
end

TEST = sprintf !"The time is %{Time} and the timezone is %{Zone}.\n"
  (Time.now ()) (Zone.machine_zone ())
  = "The time is [Time.to_string (Time.now ())] and the timezone is \
     [Zone.to_string (Zone.machine_zone ())].\n"

(* check the X.Format.y kinds of format and that arguments are not
   reversed somehow *)
TEST =
  let now = Time.now () in
  sprintf !"%{Time}, %{Time.to_sec_string}\n%!" now now
  = "[Time.to_string (Time.now ())], [Time.Format.to_sec_string (Time.now ())]\n"

(* testing what happens is the expression to the left of the format string
   is a bit complicated *)
TEST =
  let s = ksprintf (fun s ->
    (s ^ " foo")
  ) !"%{Time} bar" (Time.now ())
  in
  s = "[Time.to_string (Time.now ())] bar foo"

(* checking sexp: format *)
TEST "sexp conversion" =
  sprintf !"The pair is: %{sexp:int * string}" (4,"asdf")
  = "The pair is: (4 asdf)"

(* checking tricky formats *)
TEST =
  sprintf !"%d %%{foo" 3
  = "3 %{foo"
TEST =
  sprintf !"%d %.2%{foo" 3
  = "3 %{foo"
TEST =
  sprintf !"%d %.2%%{Time}" 3 (Time.now ())
  = "3 %[Time.to_string (Time.now ())]"
TEST =
  sprintf !"%d %%{%{Time}" 3 (Time.now ())
  = "3 %{[Time.to_string (Time.now ())]"

(* checking that when we eta expand, we do not change side effects *)
TEST =
  let side_effect1_happened = ref false in
  let side_effect2_happened = ref false in
  let _f : Zone.t -> string =
    (side_effect1_happened := true; sprintf) !"%{Time} %{Zone}"
      (side_effect2_happened := true; Time.now ())
  in
  !side_effect1_happened && !side_effect2_happened

TEST =
  let to_string () = "plop" in
  sprintf !"%{  }" () = "plop"

TEST_UNIT =
  let f ~labeled_arg:() fmt = ksprintf (fun _ -> ()) fmt in
  (* Check that it compiles with the labeled argument applied both before and after the
     format string *)
  f ~labeled_arg:() !"hello";
  f !"hello" ~labeled_arg:();
;;

TEST_UNIT =
  let after1 = Some () in
  let f ~before:() fmt = ksprintf (fun _ ?after1:_ () ~after2:() -> ()) fmt in
  f ~before:() ?after1 !"hello" () ~after2:();
  f ~before:() !"hello" ?after1 () ~after2:();
  f !"hello" ~before:() ?after1 () ~after2:();
  f !"hello" ?after1 ~before:() () ~after2:();
;;

TEST_UNIT =
  let f ~label:() fmt = ksprintf (fun _ -> ()) fmt in
  let r = ref 0 in
  let g = f !"%{Time}" ~label:(incr r) in
  g (Time.now ()); g (Time.now ());
  assert (!r = 1);
;;
