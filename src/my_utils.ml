(*
 * Copyright (C) 2012 Zhang Initiative Research Unit,
 * Advance Science Institute, RIKEN
 * 2-1 Hirosawa, Wako, Saitama 351-0198, Japan
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *)

(* to work in the toplevel
   #use "topfind";;
   #require "batteries";;
   #use "my_utils.ml";; *)

module A   = Array
module BF  = BatFile
module BL  = BatList
module BS  = BatString
module F   = Filename
module HT  = Hashtbl
module L   = List
module Mar = Marshal
module P   = Printf
module RNG = Random
module S   = String

(* ============================== Operators ===================== *)

(* function composition operator (chaining functions) *)
let ($@) f g x = f (g x)
(* in case the previous operator give problems while parsing *)
let compose f g x = f (g x)

let (|>) x f = f x

(* ============================== Conversion functions ===================== *)

(* ASCII to integer *)
let atoi = int_of_string

(* integer to ASCII *)
let itoa = string_of_int

let atof = float_of_string

let ftoa = string_of_float

let itof = float_of_int

(* truncate, does not round *)
let ftoi = int_of_float

(* ============================== I/O ============================== *)

(* like Python's readlines() *)
let string_list_of_file f =
  BL.of_enum (BF.lines_of f)

let list_to_file f to_string l =
  let out = open_out f in
  L.iter
    (fun elt -> P.fprintf out "%s" (to_string elt))
    l;
  close_out out

(* output a string list to a file, '\n' are added *)
let string_list_to_file f l =
  list_to_file f
    (fun s -> P.sprintf "%s\n" s)
    l

(* keep only lines satisfying p *)
let some_lines_of_file p f =
  L.filter p (string_list_of_file f)

(* call f on lines of file *)
let iter_on_lines_of_file f file =
  let input = open_in file in
  try
    while true do
      f (input_line input)
    done
  with _ -> close_in input

(* call f on lines of file that satisfy p *)
let iter_on_some_lines_of_file p f file =
  iter_on_lines_of_file
    (fun l -> if p l then f l else ())
    file

(* apply f on lines of file *)
let map_on_lines_of_file f file =
  let res = ref [] in
  iter_on_lines_of_file
    (fun l -> res := (f l) :: !res)
    file;
  L.rev !res

(* apply f on lines of file that satisfy p *)
let map_on_some_lines_of_file p f file =
  let res = ref [] in
  iter_on_lines_of_file
    (fun l ->
       if p l then
         res := (f l) :: !res
       else
         ())
    file;
  L.rev !res

(* ============================== Strings ============================== *)

(* split 's' using separator 'sep' *)
let str_split sep s =
  Str.split (Str.regexp_string sep) s

(* ============================== Arrays ============================= *)

(* float array sum *)
let sum_a a = A.fold_left (fun acc a -> acc +. a) 0. a

(* float array average *)
let average_a a = (sum_a a) /. (float_of_int (A.length a))

(* minimum of a float array *)
let min_a a =
  A.fold_left
    Pervasives.min      (* a fun   *)
    Pervasives.infinity (* a value *)
    a

(* maximum of a float array *)
let max_a a =
  A.fold_left
    Pervasives.max          (* a fun   *)
    Pervasives.neg_infinity (* a value *)
    a

(* ============================== Lists ============================== *)

let is_empty l =
  match l with
      [] -> true
    | _  -> false

(* remove first occurence of elt in lst *)
let remove_first elt lst =
  let rec loop l acc = match l with
    | []      -> lst
    | x :: xs ->
        if x = elt
        then BL.append (L.rev acc) xs
        else loop xs (x :: acc)
  in
  loop lst []

(* fold_left f on l while p is true *)
let rec fold_while f p acc l =
  match l with
      []      -> acc
    | x :: xs ->
        if p x then
          fold_while f p (f x :: acc) xs
        else
          acc

(* reversed tail rec. version of stdlib's List.combine *)
let rev_combine l1 l2 =
  let rec combine_priv l1 l2 acc =
    match l1, l2 with
      []       , []        -> acc
    | []       , _
    | _        , []        ->
        failwith "my_utils.ml: rev_combine: list with different lengths"
    | x1 :: xs1, x2 :: xs2 -> combine_priv xs1 xs2 ((x1, x2) :: acc)
  in
  combine_priv l1 l2 []

(* tail rec. version of stdlib's List.combine *)
let combine l1 l2 =
  L.rev (rev_combine l1 l2)

let combine3 l m n =
  let rec loop l1 l2 l3 acc =
    match l1, l2, l3 with
        [], [], [] -> L.rev acc
      | x :: xs, y :: ys, z :: zs ->
          loop xs ys zs ((x, y, z) :: acc)
      | _ -> failwith "my_utils.ml: combine3: different list lengths"
  in
  loop l m n []

let enumerate l =
  let rec enumerate_priv l i acc =
    match l with
        []            -> L.rev acc
      | elt :: others -> enumerate_priv others (i+1) ((i, elt) :: acc)
  in
  enumerate_priv l 0 []

(* (\* <=> stdlib's List.map but tail recursive *\) *)
(* BatList has a much more efficient version: use it instead of this one *)
(* let map f l = *)
(*   L.rev (L.rev_map f l) *)

(* prepend (rev 'pre') to 'post' *)
let rec rev_prepend pre post =
  match pre with
    []      -> post
  | x :: xs -> rev_prepend xs (x :: post)

(* add 'elt' to 'lst' if it is not already in *)
let add_if_not_mem elt lst =
  if L.mem elt lst then
    lst
  else
    elt :: lst

(* remove all occurences of 'elt' from 'lst' *)
let remove_all elt lst =
  L.filter ((!=) elt) lst

(* return the list of integers [start ; ... ; stop] *)
let list_of_ints start stop =
  let rec list_of_ints_priv start stop acc =
    if start = stop then
      stop :: acc
    else
      list_of_ints_priv start (stop - 1) (stop :: acc)
  in
  list_of_ints_priv start stop []

(* same as list_of_ints but convert them to floats afterwards *)
let list_of_floats start stop =
  BL.map float_of_int (list_of_ints start stop)

(* list of floats [start, start + step, ..., stop], without accumulation
   of floating point errors.
   step = (stop - start) / nb_steps *)
let discretize_range start nb_steps stop =
  let delta = stop -. start in
  let n     = itof nb_steps in
  let res   = ref []        in
  for i = nb_steps downto 0 do
    res := (((delta *. (itof i)) /. n) +. start) :: !res;
  done;
  !res

let shorter_list_first l1 l2 =
  compare (L.length l1) (L.length l2)

let longer_list_first l1 l2 =
  -(shorter_list_first l1 l2)

let string_of_list to_string sep l =
  "[" ^ String.concat sep (BL.map to_string l) ^ "]"

let rev_enrich_with f l =
  L.rev_map (fun x -> (f x, x)) l

exception Different_list_lengths;;

(* combine is not tail rec in the std lib *)
let combine lx ly =
  let rec combine_priv l1 l2 acc =
    match l1, l2 with
        x :: xs, y :: ys -> combine_priv xs ys ((x,y) :: acc)
      | []     , []      -> L.rev acc
      | _                -> raise Different_list_lengths
  in
  combine_priv lx ly []

let combine3 lx ly lz =
  let rec combine3_priv l1 l2 l3 acc =
    match l1, l2, l3 with
        x :: xs, y :: ys, z :: zs -> combine3_priv xs ys zs ((x,y,z) :: acc)
      | []     , []     , []      -> L.rev acc
      | _                         -> raise Different_list_lengths
  in
  combine3_priv lx ly lz []

exception List_length_not_multiple_of_three;;

let triplets_list_of_list l =
  let rec triplets_list_of_list_priv l acc =
    match l with
        x :: y :: z :: tail ->
          triplets_list_of_list_priv tail ((x,y,z) :: acc)
      | [] -> L.rev acc
      | _  -> raise List_length_not_multiple_of_three
  in
  triplets_list_of_list_priv l []

(* product of all elements of a float list *)
let fprod l = BL.reduce ( *. ) l

(* product of all elements of an int list *)
let prod l = BL.reduce ( * ) l

let average_l l =
  let rec loop lst (sum, count) =
    match lst with
    | []      -> sum /. count
    | x :: xs -> loop xs (sum +. x, count +. 1.)
  in
  loop l (0., 0.)

(* standard deviation (sigma) *)
let std_dev l =
  let avg = average_l l in
  sqrt (average_l (BL.map (fun x -> (x -. avg) *. (x -. avg)) l))

(* apply f to elements of l that satisfy p *)
let iter_on_some_elements p f l =
  L.iter
    (fun x -> if p x then f x else ())
    l

(* ============================== Sets ================================ *)

module IntSet =
  Set.Make
    (struct
      let compare = Pervasives.compare
      type t = int
    end)

let int_set_of_list l =
  L.fold_left
    (fun acc x -> IntSet.add x acc)
    IntSet.empty l

(* ============================== Time ================================ *)

let gettimeofday_fractional_part () =
  let tod     = Unix.gettimeofday() in
  let us, _s  = modf tod in
  us

(* ============================== Tuples ============================== *)

(* compare 2 pairs based on their 1st element *)
let compare_fst (i1, _) (i2, _) =
  compare i1 i2

(* compare 2 pairs based on their 2nd element *)
let compare_snd (_, i1) (_, i2) =
  compare i1 i2

(* swap a pair *)
let swap (i, j) = (j, i)

(* create a sorted triple from a, b and c *)
let sort_triple a b c =
  match L.sort compare [a; b; c] with
    one :: two :: three :: [] -> (one, two, three)
  | _                         ->
      failwith "my_utils.ml: sort_triple: the impossible happened"

(* ============================== Hash tables ============================== *)

(* return all the values of a given hash table, unsorted *)
let values hash_table =
  let add_value_to_list _key value acc =
    value :: acc
  in
  HT.fold add_value_to_list hash_table []

(* return all the keys of a given hash table, unsorted *)
let keys hash_table =
  let add_key_to_list key _value acc =
    key :: acc
  in
  HT.fold add_key_to_list hash_table []

(* return all the (key, value) pairs of a given hash table, unsorted *)
let key_values hash_table =
  let add_key_value_pair_to_list key value acc =
    (key, value) :: acc
  in
  HT.fold add_key_value_pair_to_list hash_table []

(* add key->value mapping to 'hash_table' if not already present *)
let add_if_not_present hash_table (key, value) =
  if HT.mem hash_table key then (* already here *)
    ()
  else (* not yet here *)
    HT.add hash_table key value

(* create a Hashtbl from a list of key-value pairs *)
let hashtbl_of key_value_pairs =
  let res = HT.create (L.length key_value_pairs) in
  let add (k, v) = HT.add res k v in
  L.iter add key_value_pairs;
  res

(* transform a Hashtbl of key->value into a Hashtbl of value->key *)
let reverse_binding ht =
  let res = HT.create (HT.length ht) in
  HT.iter (fun key value -> HT.add res value key) ht;
  res

(* hash table lookup with key being printed out in exception if not found *)
let find ht key key_printer =
  let res = try HT.find ht key with
      Not_found -> failwith ("my_utils.ml: find: HT lookup failed for: "
                             ^ (key_printer key)) in
  res

(* ============================== Maths ============================= *)

(* No Math.pi in Ocaml ?! *)
let pi       = 4.0 *. atan 1.0
let pi_div_2 = 2.0 *. atan 1.0

let is_NaN x =
  match classify_float x with
      FP_nan -> true
    | _      -> false

(* flipping parameters *)
let flip f x y = f y x

let nearest_int f =
  int_of_float (f +. 0.5)

let fabs x = if x < 0.0 then -.x
                        else   x

(* x =? v +/- epsilon *)
let around epsilon v x =
  (x >= v -. epsilon) &&
  (x <= v +. epsilon)

(* a famous function that does nothing *)
let identity x = x

let square x = x *. x

let cube x = x *. x *. x

let median_a xs =
  let n = A.length xs in
  A.sort compare xs;
  if n mod 2 = 1
  then xs.(n/2)
  else (xs.(n/2) +. xs.(n/2 - 1)) /. 2.0

let median_l xs =
  median_a (A.of_list xs)

(* significance of the correlation score c for a population of size n
   formula (14.6.2) for t at page 749 of numerical recipes 3rd ed. *)
let significance c n =
  let nf = float_of_int n in
  c *. sqrt (( nf -. 2.) /. (1. -. c*.c))

(* approximation of the complementary error function erfc(x),
   comes from the book "numerical recipes" 3rd edition *)
let erfcc x =
  let   z = fabs x          in
  let   t = 2. /. (2. +. z) in
  let ans = t *. exp
    (-.z *. z -. 1.26551223 +. t *.
              (  1.00002368 +. t *.
              (  0.37409196 +. t *.
              (  0.09678418 +. t *.
              (-.0.18628806 +. t *.
              (  0.27886807 +. t *.
              (-.1.13520398 +. t *.
              (  1.48851587 +. t *.
              (-.0.82215223 +. t *.
                 0.17087277))))))))) in
  if x >= 0.0 then ans
              else 2.0 -. ans

(* Pearson correlation coefficient for float arrays, cross validated with some
   Python implementation of it that I have *)
let pearson_a a1 a2 =
  let tiny = 1.0e-20     in
  let    n = A.length a1 in
  let   nf = itof n      in
  if n <> A.length a2 then
    failwith "my_utils.ml: pearson_a: arrays length differ"
  else
    let p      = average_a a1 in
    let q      = average_a a2 in
    let sum_xx = ref 0.       in
    let sum_yy = ref 0.       in
    let sum_xy = ref 0.       in
    let process x' y' =
      let x    = x' -. p in
      let y    = y' -. q in
      let xx   = x *. x  in
      let yy   = y *. y  in
      let xy   = x *. y  in
        sum_xx := !sum_xx +. xx;
        sum_yy := !sum_yy +. yy;
        sum_xy := !sum_xy +. xy;
    in
    for i = 0 to n - 1 do
      process a1.(i) a2.(i);
    done;
    let r = !sum_xy /. (sqrt(!sum_xx *. !sum_yy) +. tiny)        in
    let z = 0.5 *. log((1.0 +. r +. tiny) /. (1.0 -. r +. tiny)) in
    (* approximation of Student's t probability valid for large n *)
    let t = erfcc(fabs(z *. sqrt(nf -. 1.0)) /. 1.4142136)       in
    (r, t)

(* comes from biocaml, not me *)
let spearman_rank arr =
  let arr = Array.copy arr in
  let arr = Array.mapi (fun i a -> a,i) arr in
  Array.sort (fun (a,_) (b,_) -> Pervasives.compare a b) arr;
  let g _prev il ans =
    let count = List.length il in
    let n = count + (List.length ans) in
    let hi = float_of_int n in
    let lo = float_of_int (n - count + 1) in
    let rank = (hi +. lo) /. 2. in
    (List.map (fun i -> rank,i) il) @ ans
  in
  let f (prev, il, ans) (x,i) =
    let count = List.length il in
    if count = 0 then
      x, [i], ans
    else if x = prev then
      x, i::il, ans
    else
      x, [i], g prev il ans
  in
  let prev,il,ans = Array.fold_left f (0.,[],[]) arr in
  let ans = g prev il ans in
  let ans = List.sort (fun (_,a) (_,b) -> Pervasives.compare a b) ans in
  Array.of_list (List.map fst ans)

(* Spearman rank-order correlation coefficient *)
let spearman_a (a1:float array) (a2:float array) =
  pearson_a (spearman_rank a1) (spearman_rank a2)

let spearman_l l1 l2 =
  spearman_a (A.of_list l1) (A.of_list l2)

let pearson_l l1 l2 =
  pearson_a (A.of_list l1) (A.of_list l2)

let carbo_hodgkin_a a1 a2 =
  let n = A.length a1 in
  if n <> A.length a2 then
    failwith "my_utils.ml: carbo_idx_a: arrays length differ"
  else
    let sum_xy = ref 0. in
    let sum_x2 = ref 0. in
    let sum_y2 = ref 0. in
    for i = 0 to n - 1 do
      let x = a1.(i) in
      let y = a2.(i) in
      sum_xy := !sum_xy +. (x *. y);
      sum_x2 := !sum_x2 +. (x *. x);
      sum_y2 := !sum_y2 +. (y *. y);
    done;
    let carbo = !sum_xy /. ((sqrt !sum_x2) *. (sqrt !sum_y2)) in
    let hodgkin = (2. *. !sum_xy) /. (!sum_x2 +. !sum_y2) in
    (* enforce theoretical bounds [-1; 1] *)
    (min (max (-1.) carbo  ) 1.0,
     min (max (-1.) hodgkin) 1.0)

(* tests:
   carbo_hodgkin [2.; 3.] [1.; 1.] = (0.98058067569092, 0.66666666666666663);;
   carbo_hodgkin [2.; 3.] [4.; 6.] = (1., 0.8);;
   carbo_hodgkin [-2.; -3.] [4.; 6.] = (-1., -0.8);;
 *)
let carbo_hodgkin l1 l2 =
  carbo_hodgkin_a (A.of_list l1) (A.of_list l2)

(* initialize the RNG using system time in ms as the seed (returned) *)
let init_RNG () =
  let ms_int = ftoi (1000. *. gettimeofday_fractional_part()) in
  RNG.init ms_int;
  ms_int

(* ============================= System / UNIX ============================ *)

let getenv_or_fail variable_name failure_message =
  try Sys.getenv variable_name
  with Not_found -> failwith failure_message

exception Command_failed of string;;

(* run a command,
   ~debug  -> dry-run only
   ~strict -> raise (Command_failed msg) on non 0 exit status of the command *)
let run_command ?(debug = false) ?(strict = true) cmd =
  P.printf "running:\n%s\n%!" cmd;
  if debug then ()
  else
    let ret_code = Sys.command cmd in
    (* P.printf "ret_code: %d\n%!" ret_code; *)
    match ret_code with
        0 -> ()
      | _ -> if strict then raise (Command_failed ("command failed: " ^ cmd))

(* gzip a file using the given compression level
   return the compressed filename
   the original file is removed *)
let gzip filename comp_level =
  run_command (P.sprintf "gzip -f -%d %s" comp_level filename);
  (filename ^ ".gz")

(* gunzip a file
   return the uncompressed filename
   the original file can be kept but is removed by default *)
let gunzip ?(keep = false) filename =
  let res = F.chop_extension filename in
  let cmd =
    if keep
    then P.sprintf "gunzip -c %s > %s" filename res
    else P.sprintf "gunzip -f %s"      filename
  in
  run_command cmd;
  res

(* run the given command and return its output as a string *)
let get_command_output cmd =
  let cmd_out   = Unix.open_process_in cmd in
  let buff_size = 1024*1024                in
  let buff      = Buffer.create buff_size  in
  let _         =
    try
      while true do
        Buffer.add_string buff (input_line cmd_out);
        Buffer.add_char   buff '\n'; (* was stripped by input_line *)
      done;
    with
    | End_of_file -> ignore(Unix.close_process_in cmd_out)
    | exn         -> ignore(Unix.close_process_in cmd_out); raise exn
  in
  Buffer.contents buff

(* ============================== Files ============================== *)

(* get_extension "toto.txt" -> "txt"
   get_extension "toto"     -> ""    
   get_extension ""         -> ""    *)
let get_extension filename =
  let f = F.basename filename in
  match Str.split (Str.regexp_string ".") f with
      []  -> ""
    | [_] -> ""
    | l   -> L.hd (L.rev l)

(* check filename has one in a list of possible extensions *)
let enforce_any_file_extension filename possible_exts_list =
  let res =
    L.filter
      (fun ext -> F.check_suffix filename ext)
      possible_exts_list
  in
  match res with
    [] -> failwith ("my_utils.ml: enforce_any_file_extension: " ^
                    filename ^ " is not any of " ^
                    (string_of_list identity "; " possible_exts_list))
  | _  -> ()

let filename_with_different_extension filename previous_ext new_ext =
  (* don't chop silently if the extension is not the expected one,
     crash loud and early instead *)
  enforce_any_file_extension filename [previous_ext];
  (F.chop_suffix filename previous_ext) ^ new_ext

(* test if a valid gzip file extension is present in filename *)
let is_a_compressed_file filename =
  F.check_suffix filename ".gz"   ||
  F.check_suffix filename ".gzip" ||
  F.check_suffix filename ".Z"

(* marshal a value to a possibly compressed file, determined based on the
   extension of f *)
let marshal v f =
  let marshal_priv v f =
    let out = open_out f in
    Mar.to_channel out v [];
    close_out out
  in
  if is_a_compressed_file f then begin
    let uncompressed_f = F.chop_extension f in
    marshal_priv v uncompressed_f;
    ignore(gzip uncompressed_f 1);
  end else
    marshal_priv v f

(* unmarshal a value from a possibly compressed file *)
let unmarshal f =
  let unmarshal_priv f =
    let input = open_in f              in
    let v     = Mar.from_channel input in
    close_in input;
    v
  in
  if is_a_compressed_file f then
    let uncompressed_f = gunzip ~keep:true f           in
    let res            = unmarshal_priv uncompressed_f in
    Sys.remove uncompressed_f;
    res
  else
    unmarshal_priv f

(* ============================= Options ============================ *)

let get_some opt err_msg =
  match opt with
      Some x -> x
    | None   -> failwith err_msg

(* ============================= My error monad ======================== *)

(* the very badly named "return" in Haskell, it is just a constructor in fact *)
type ('a, 'b) result =
  | Result of 'a
  | Error  of 'b

(* "bind" in Haskell *)
let try_to_apply m f = match m with
  | Result r -> f r (* apply the function *)
  | Error  _ -> m   (* leave the value untouched and propagate it downstream *)

(* "bind" as an operator *)
let (>>=) m f = try_to_apply m f
