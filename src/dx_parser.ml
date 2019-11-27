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

module A    = Array
module At   = Atom
module BL   = BatList
module BS   = BatString
module ISet = My_utils.IntSet
module ITV  = Interval_tree
module L    = List
module MU   = My_utils
module PDB  = Pdb_parser
module Pqr  = Pqr
module V3   = Vector3

open Printf

type parsed_line =
    Grid      of int   * int   * int
  | Origin    of V3.t
  | Deltas    of float * float * float
  | Nb_values of int

let parse_object_1_line l =
  try Scanf.sscanf l "%s %d %s %s %s %d %d %d"
    (* "object 1 class gridpositions counts 129 129 129" *)
    (fun _object i _class _gridpositions _counts nbx nby nbz ->
       if i = 1 then
         Some (Grid (nbx, nby, nbz))
       else
         None)
  with _ -> None

let parse_origin_line l =
  try Scanf.sscanf l "%s %f %f %f"
    (* "origin -1.135820e+01 -2.901020e+01 5.456000e+00" *)
    (fun origin x y z ->
       if origin = "origin" then
         Some (Origin (V3.make x y z))
       else
         None)
  with _ -> None

let parse_delta_line l =
  try Scanf.sscanf l "%s %f %f %f"
    (* "delta 4.746422e-01 0.000000e+00 0.000000e+00" *)
    (fun delta dx dy dz ->
       if delta = "delta" then
         Some (Deltas (dx, dy, dz))
       else
         None)
  with _ -> None

let parse_object_3_line l =
  try Scanf.sscanf l "%s %d %s %s %s %s %s %d %s %d %s %s"
    (fun
       (*"object 3 class array type double rank 0 items 2146689 data follows"*)
       _obj i _class _array _type _double _rank _j _items k _data _follows ->
       if i = 3 then
         Some (Nb_values k)
       else
         None)
  with _ -> None

(* value lines always start with a numeric character or minus *)
let is_a_value_line s =
  (* Str.string_match (Str.regexp "^\\(-\\|[0-9]\\)") s 0 *)
  (* regexp was too slow :( *)
  if String.length s > 0 then
    let c = s.[0] in
    (c = '-' || (c >= '0' && c <= '9'))
  else
    false

let parse_line acc l =
  if (is_a_value_line l) || (BS.starts_with l "#") then
    acc (* ignore comments (and value lines for the moment) *)
  else
    match parse_object_1_line l with
        Some grid -> grid :: acc
      | _ -> match parse_origin_line l with
            Some vector3 -> vector3 :: acc
          | _ -> match parse_delta_line l with
                Some deltas -> deltas :: acc
              | _ -> match parse_object_3_line l with
                    Some k -> k :: acc
                  | _ -> acc

type dx_parser_state = {
  mutable nbx     : int; (* grid dimensions *)
  mutable nby     : int;
  mutable nbz     : int;
  mutable ox      : float; (* origin coordinates *)
  mutable oy      : float;
  mutable oz      : float;
  mutable dx      : float; (* grid deltas *)
  mutable dy      : float;
  mutable dz      : float;
  mutable nbxyz   : int; (* verif grid dimensions *)
  mutable ijk     : int; (* to fill charges at init *)
  (* i, j, k: indexes for reading grid values *)
  mutable i       : int;
  mutable j       : int;
  mutable k       : int;
  mutable charges : float array
}

(* check two dx objects have same dimensions and are in the same volume *)
let are_compatible dx_r1 dx_r2 =
  (!dx_r1.nbx   = !dx_r2.nbx  ) &&
  (!dx_r1.nby   = !dx_r2.nby  ) &&
  (!dx_r1.nbz   = !dx_r2.nbz  ) &&
  (!dx_r1.ox    = !dx_r2.ox   ) &&
  (!dx_r1.oy    = !dx_r2.oy   ) &&
  (!dx_r1.oz    = !dx_r2.oz   ) &&
  (!dx_r1.dx    = !dx_r2.dx   ) &&
  (!dx_r1.dy    = !dx_r2.dy   ) &&
  (!dx_r1.dz    = !dx_r2.dz   ) &&
  (!dx_r1.nbxyz = !dx_r2.nbxyz)

let state_incr state_r =
  let nbx, nby, nbz = !state_r.nbx, !state_r.nby, !state_r.nbz in
  let i  , j  , k   = !state_r.i  , !state_r.j  , !state_r.k   in
  (if k = nbz - 1 then
     if j = nby - 1 then
       if i = nbx - 1 then
         (!state_r.i <- 0 ;
          !state_r.j <- 0 ;
          !state_r.k <- 0 )
       else
         (!state_r.i <- i + 1 ;
          !state_r.j <- 0     ;
          !state_r.k <- 0     )
     else
       (!state_r.i <- i     ;
        !state_r.j <- j + 1 ;
        !state_r.k <- 0     )
   else
     (!state_r.i <- i     ;
      !state_r.j <- j     ;
      !state_r.k <- k + 1 )
  );
  !state_r.ijk <- !state_r.ijk + 1

let i2f = float_of_int

let x_pos_of_grid_coord state_r i = !state_r.ox +. !state_r.dx *. (i2f i)
let y_pos_of_grid_coord state_r j = !state_r.oy +. !state_r.dy *. (i2f j)
let z_pos_of_grid_coord state_r k = !state_r.oz +. !state_r.dz *. (i2f k)

let pos_of_grid_coord state_r i j k =
  V3.make
    (x_pos_of_grid_coord state_r i)
    (y_pos_of_grid_coord state_r j)
    (z_pos_of_grid_coord state_r k)

let index_of_ijk state_r i j k =
  let nby, nbz = !state_r.nby, !state_r.nbz in
  i*nby*nbz + j*nbz + k

let sprintf_charge state_r u =
  let i, j, k = !state_r.i, !state_r.j, !state_r.k in
  let pos     = pos_of_grid_coord state_r i j k    in
  V3.(sprintf "%f %f %f %f" pos.x pos.y pos.z u)

let process_parsed_line state_r l =
  match l with
    Grid (nbx, nby, nbz) ->
      !state_r.nbx <- nbx;
      !state_r.nby <- nby;
      !state_r.nbz <- nbz;
      Log.info (lazy (sprintf "nbx nby nbz nbxyz: %d %d %d %d"
                        nbx nby nbz (nbx * nby * nbz)))
  | Origin v3 ->
      let ox, oy, oz = V3.to_triplet v3 in
      !state_r.ox <- ox;
      !state_r.oy <- oy;
      !state_r.oz <- oz;
      Log.info (lazy (sprintf "origin: %s" (V3.to_string v3)))
  | Deltas (dx, dy, dz) ->
      if dx = 0.0 then
        if dy = 0.0 then
          if dz = 0.0 then
            failwith "dx dy dz all 0.0"
          else
            (!state_r.dz <- dz;
             Log.info (lazy (sprintf "dz: %f" dz)))
        else
          (!state_r.dy <- dy;
           Log.info (lazy (sprintf "dy: %f" dy)))
      else
        (!state_r.dx <- dx;
         Log.info (lazy (sprintf "dx: %f" dx)))
  | Nb_values k ->
      !state_r.nbxyz <- k;
      if k <> !state_r.nbx * !state_r.nby * !state_r.nbz then
        failwith "dx parser state error: nbx nby nbz <> nbxyz"
      else
        (Log.info (lazy (sprintf "values: %d" k));
         !state_r.charges <- A.make k 0.)

let process_value_line verbose state_r l =
  let values_s = Str.split (Str.regexp " ") l in
  L.iter
    (fun v_s -> begin
       let v   = float_of_string v_s in
       let ijk = !state_r.ijk        in
       !state_r.charges.(ijk) <- v;
       if verbose
       then printf "%s\n" (sprintf_charge state_r v)
       else ();
       state_incr state_r;
    end)
    values_s

(* lines after the 'object 3 ...' one are values, we don't need to read them
   in case we just want to know the grid dimensions and discretization *)
let parse_dx_file ?(partially = false) verbosity dx_file =
  MU.enforce_any_file_extension dx_file [".dx"];
  let lines =
    if partially then
      let input = open_in dx_file in
      let cond  = ref true        in
      let acc   = ref []          in
      (try
         while !cond do
           let l = input_line input in
           acc := l :: !acc;
           if BS.starts_with l "object 3 "
           then cond := false
           else ();
         done
       with _ -> close_in input);
      L.rev !acc
    else
      MU.string_list_of_file dx_file                    in
  let parsed  = L.rev (L.fold_left parse_line [] lines) in
  let state_r = ref { nbx     = 0  ;
                      nby     = 0  ;
                      nbz     = 0  ;
                      ox      = 0. ;
                      oy      = 0. ;
                      oz      = 0. ;
                      dx      = 0. ;
                      dy      = 0. ;
                      dz      = 0. ;
                      nbxyz   = 0  ;
                      ijk     = 0  ;
                      i       = 0  ;
                      j       = 0  ;
                      k       = 0  ;
                      charges = A.make 1 0. } in
  L.iter (process_parsed_line state_r) parsed;
  if partially then
    !state_r
  else begin
    MU.iter_on_some_elements
      is_a_value_line
      (process_value_line verbosity state_r)
      lines;
    (if !state_r.ijk <> !state_r.nbxyz then
       (* the expected number of values should have been read in now *)
       failwith (sprintf
                   "dx_parser.ml: parse_dx_file: read %d, expecting %d\n"
                   !state_r.ijk !state_r.nbxyz)
     else !state_r)
  end

(* ==================== ES map masks ==================== *)

let create_positive_mask state_r =
  A.make !state_r.nbxyz true

let create_negative_mask state_r =
  A.make !state_r.nbxyz false

let mask_op mask_1 mask_2 op =
  assert (A.length mask_1 = A.length mask_2);
  A.mapi
    (fun i a_bool -> op a_bool mask_2.(i))
    mask_1

(* mask_op over a list of masks *)
let masks_op masks op =
  let rec loop ms =
    match ms with
      []                 -> failwith "dx_parser.ml: masks_op: empty list"
    | [res]              -> res
    | m1 :: m2 :: others ->
        loop ((mask_op m1 m2 op) :: others)
  in
  loop masks

let charges_position_mask np state_r accept_point =
  let nbx, nby, nbz, nbxyz =
    !state_r.nbx,
    !state_r.nby,
    !state_r.nbz,
    !state_r.nbxyz in
  let grid_points = A.make nbxyz V3.origin in
  for i = 0 to nbx - 1 do
    for j = 0 to nby - 1 do
      for k = 0 to nbz - 1 do
        let index = index_of_ijk state_r i j k in
        grid_points.(index) <- pos_of_grid_coord state_r i j k
      done;
    done;
  done;
  Parmap.array_parmap ~ncores:np accept_point grid_points

(* unmask charges satisfying a given criterion *)
let charges_value_mask state_r criterion =
  let nbx, nby, nbz = !state_r.nbx, !state_r.nby, !state_r.nbz in
  let res           = create_negative_mask state_r             in
  for i = 0 to nbx - 1 do
    for j = 0 to nby - 1 do
      for k = 0 to nbz - 1 do
        let index = index_of_ijk state_r i j k in
        if criterion !state_r.charges.(index)
        then res.(index) <- true
        else ()
      done;
    done;
  done;
  res

let coordinate_intervals_of_atoms atoms =
  let i = ref 0 in
  L.fold_left
    (fun (acc_x, acc_y, acc_z) a ->
       let x_min, x_max,
           y_min, y_max,
           z_min, z_max = At.bounding_box a in
       let itv_x = ITV.new_interval x_min x_max !i in
       let itv_y = ITV.new_interval y_min y_max !i in
       let itv_z = ITV.new_interval z_min z_max !i in
       incr i;
       (itv_x :: acc_x, itv_y :: acc_y, itv_z :: acc_z))
    ([], [], []) atoms

let inside_MS_mask state_r =
  let nbx, nby, nbz, nbxyz = !state_r.nbx,
                             !state_r.nby,
                             !state_r.nbz,
                             !state_r.nbxyz in
  let grid_points = A.make nbxyz false in
  (* fill the mask *)
  for i = 0 to nbx - 1 do
    for j = 0 to nby - 1 do
      for k = 0 to nbz - 1 do
        let index = index_of_ijk state_r i j k in
        (* APBS puts 0.0 to grid points inside of the molecular surface *)
        grid_points.(index) <- (!state_r.charges.(index) = 0.0)
      done;
    done;
  done;
  grid_points

let inside_VdV_mask atoms state_r =
  let x_intervals, y_intervals, z_intervals =
    coordinate_intervals_of_atoms atoms       in
  let x_tree  = ITV.interval_tree x_intervals in
  let y_tree  = ITV.interval_tree y_intervals in
  let z_tree  = ITV.interval_tree z_intervals in
  let atoms_a = A.of_list atoms               in
  let nbx, nby, nbz, nbxyz = !state_r.nbx,
                             !state_r.nby,
                             !state_r.nbz,
                             !state_r.nbxyz in
  let grid_points = A.make nbxyz false      in
  let x_results   = A.make nbx ISet.empty   in
  let y_results   = A.make nby ISet.empty   in
  let z_results   = A.make nbz ISet.empty   in
  (* precompute all possible query results *)
  for i = 0 to nbx - 1 do
    let p_x = x_pos_of_grid_coord state_r i in
    x_results.(i) <- MU.int_set_of_list (ITV.data_of_query_result x_tree p_x);
  done;
  for j = 0 to nby - 1 do
    let p_y = y_pos_of_grid_coord state_r j in
    y_results.(j) <- MU.int_set_of_list (ITV.data_of_query_result y_tree p_y);
  done;
  for k = 0 to nbz - 1 do
    let p_z = z_pos_of_grid_coord state_r k in
    z_results.(k) <- MU.int_set_of_list (ITV.data_of_query_result z_tree p_z);
  done;
  (* fill the mask *)
  for i = 0 to nbx - 1 do
    let i_set = x_results.(i) in
    for j = 0 to nby - 1 do
      let ij_set = ISet.inter i_set y_results.(j) in
      for k = 0 to nbz - 1 do
        let ijk_set = ISet.inter ij_set z_results.(k) in
        let p       = pos_of_grid_coord state_r i j k in
        let index   = index_of_ijk state_r i j k      in
        grid_points.(index) <-
          ISet.exists
            (fun atom_id -> let a = atoms_a.(atom_id) in
                            At.point_is_in_atom p a)
            ijk_set;
      done;
    done;
  done;
  grid_points

let dilate atoms by =
  BL.map
    (fun a -> At.dilate a by)
    atoms

let erode atoms by =
  BL.map
    (fun a -> At.erode a by)
    atoms

let thick_skin_mask skin_half_thickness pqr_A state_r =
  let atoms_A         = Pqr.atoms_of_pqr_file pqr_A        in
  let dilated_atoms_A = dilate atoms_A skin_half_thickness in
  let eroded_atoms_A  = erode  atoms_A skin_half_thickness in
  let nearby_A        = inside_VdV_mask dilated_atoms_A state_r in
  let inside_A        = inside_VdV_mask eroded_atoms_A  state_r in
  mask_op nearby_A inside_A (fun a b -> a && (not b))

let around_but_not_inside_mask es_falloff_limit pqr_A state_r =
  let atoms_A         = Pqr.atoms_of_pqr_file pqr_A             in
  let dilated_atoms_A = dilate atoms_A es_falloff_limit         in
  let nearby_A        = inside_VdV_mask dilated_atoms_A state_r in
  let inside_A        = inside_MS_mask state_r                  in
  mask_op nearby_A inside_A (fun a b -> a && (not b))

(* useful alias *)
let surface_mask thickness pqr state_r =
  around_but_not_inside_mask thickness pqr state_r

let around_mask es_falloff_limit pqr_A state_r =
  let atoms_A         = Pqr.atoms_of_pqr_file pqr_A     in
  let dilated_atoms_A = dilate atoms_A es_falloff_limit in
  inside_VdV_mask dilated_atoms_A state_r

(* fake H atom PDB line *)
let pdb_line_of_vector3 ?(bfactor=0.0) v = V3.(
(* example:
   'ATOM     10  C3  MPT    10     103.646  58.223 -36.577    1   0.07' *)
  Printf.sprintf "ATOM          H  MPT      %12.3f%8.3f%8.3f%5d%7.2f"
    v.x v.y v.z 1 bfactor)

(* output a mask as a PDB file for pymol, the values in the state_r are used
   as a B-factor scaled from 0.0 to 90.0 *)
let mask_to_pdb mask state_r output_file =
  let out           = open_out output_file                     in
  let nbx, nby, nbz = !state_r.nbx, !state_r.nby, !state_r.nbz in
  let min_v         = MU.min_a !state_r.charges                in
  let max_v         = MU.max_a !state_r.charges                in
  for i = 0 to nbx - 1 do
    for j = 0 to nby - 1 do
      for k = 0 to nbz - 1 do
        let index = index_of_ijk state_r i j k in
        if mask.(index) then
          let pos = pos_of_grid_coord state_r i j k                        in
          let bf  =
            90. *. (!state_r.charges.(index) -. min_v) /. (max_v -. min_v) in
          fprintf out "%s\n" (pdb_line_of_vector3 pos ~bfactor:bf)
        else ()
      done;
    done;
  done;
  close_out out

(* dump to file x y z v of points positively masked *)
let dump mask state_r output_file =
  let out           = open_out output_file                     in
  let nbx, nby, nbz = !state_r.nbx, !state_r.nby, !state_r.nbz in
  for i = 0 to nbx - 1 do
    for j = 0 to nby - 1 do
      for k = 0 to nbz - 1 do
        let index = index_of_ijk state_r i j k in
        if mask.(index) then
          let pos     = pos_of_grid_coord state_r i j k in
          let str_pos = V3.to_string pos                in
          fprintf out "%s %f\n" str_pos !state_r.charges.(index)
        else ()
      done;
    done;
  done;
  close_out out

(* ==================== ES maps ==================== *)

(* correlate ES map values which are positively masked *)
let map_correl dx_r1 dx_r2 mask =
  let count  = ref 0          in
  let l1, l2 = ref [], ref [] in
  A.iteri
    (fun i not_masked ->
       if not_masked then begin
         l1 := !dx_r1.charges.(i) :: !l1;
         l2 := !dx_r2.charges.(i) :: !l2;
         incr count;
       end)
    mask;
  (!count,
   MU.spearman_l    !l1 !l2,
   MU.pearson_l     !l1 !l2,
   MU.carbo_hodgkin !l1 !l2)

(* write out ES value pairs which are positively masked *)
let output_unmasked_values dx_r1 dx_r2 mask f =
  let l1, l2 = ref [], ref [] in
  A.iteri
    (fun i not_masked ->
       if not_masked then begin
         l1 := !dx_r1.charges.(i) :: !l1;
         l2 := !dx_r2.charges.(i) :: !l2;
       end)
    mask;
  MU.list_to_file
    f
    (fun (x,y) -> sprintf "%f %f\n" x y)
    (MU.combine !l1 !l2)

(* print out the .dx values, except that those that are not masked are
   reset to 0.0 (for graphical illustration purpose) *)
let print_unmasked_reset_others dx_r mask =
  let j = ref 0 in
  A.iteri
    (fun i masked ->
       if not masked then
         printf "0.0 "
       else
         printf "%e " !dx_r.charges.(i);
       incr j;
       if !j = 3 then begin
         j := 0;
         printf "\n";
       end
    )
    mask

type neg_pos = Negative | Positive

let neg_pos_of_value v =
  if v > 0.0 then Positive
             else Negative

(* Jaccard score for 2 classes (negative or positive ES value) *)
let map_jaccard_2 dx_r1 dx_r2 mask =
  let a_and_b_card = ref 0 in
  let a_or_b_card  = ref 0 in
  A.iteri
    (fun i not_masked ->
      if not_masked then begin
        incr a_or_b_card;
        let a, b = neg_pos_of_value !dx_r1.charges.(i),
                   neg_pos_of_value !dx_r2.charges.(i) in
        match a, b with
          Negative, Negative
        | Positive, Positive -> incr a_and_b_card
        | _                  -> ()
      end else ())
    mask;
  (!a_and_b_card, !a_or_b_card)

type neg_hphob_pos = Neg | Hphob | Pos

let neg_hphob_pos_of_value neg_lim pos_lim v =
  if      v < neg_lim then Neg
  else if v > pos_lim then Pos
                      else Hphob

(* Jaccard score for 3 classes (negative or hydrophobic or positive) *)
let map_jaccard_3 neg_lim pos_lim dx_r1 dx_r2 mask =
  let a_and_b_card = ref 0 in
  let a_or_b_card  = ref 0 in
  A.iteri
    (fun i not_masked ->
      if not_masked then begin
        incr a_or_b_card;
        let a, b = neg_hphob_pos_of_value neg_lim pos_lim !dx_r1.charges.(i),
                   neg_hphob_pos_of_value neg_lim pos_lim !dx_r2.charges.(i) in
        match a, b with
          Neg  , Neg
        | Pos  , Pos
        | Hphob, Hphob -> incr a_and_b_card
        | _            -> ()
      end else ())
    mask;
  (!a_and_b_card, !a_or_b_card)
