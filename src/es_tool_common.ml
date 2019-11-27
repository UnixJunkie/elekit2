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

module BS  = BatString
module BL  = BatList
module Dx  = Dx_parser
module F   = Filename
module Log = Dolog.Log
module MU  = My_utils
module Opt = BatOption
module P   = Printf
module PDB = Pdb_parser
module V3  = Vector3

(* stuffs common to all es tools *)

(* "0:5:5" -> (0.0, 5, 5.0) *)
let process_scan_option min_steps_max_str =
  let atof  = MU.atof                            in
  let atoi  = MU.atoi                            in
  let split = MU.str_split ":" min_steps_max_str in
  match split with
    min :: nb_steps :: max :: [] ->
      let fmin, steps, fmax = atof min, atoi nb_steps, atof max in
      assert (fmin  >= 0.0  &&
              fmax  >  fmin &&
              steps >  0    &&
              fmax  <  10.0);
      (fmin, steps, fmax)
  | _ -> failwith ("process_scan_option: cannot process scan option: " ^
                     min_steps_max_str)

let dump_mask ?(gzip = true) verbose fallout_threshold pqr parsed_dx mask =
  let extension = ".mask" ^ (if gzip then ".gz" else "") in
  let mask_out  =
    MU.filename_with_different_extension
      pqr ".pqr" (P.sprintf "_%3.1f%s" fallout_threshold extension) in
  MU.marshal mask mask_out;
  if verbose then
    Dx.mask_to_pdb mask parsed_dx
      (MU.filename_with_different_extension mask_out extension ".pdb")
  else ();
  mask_out

let dump_mask_around_receptor verbose fallout_threshold pqr parsed_dx =
  let around_receptor = Dx.around_mask fallout_threshold pqr parsed_dx in
  ignore(dump_mask verbose fallout_threshold pqr parsed_dx around_receptor);
  around_receptor

let dump_mask_around_ligand
    ?(gzip = true) verbose fallout_threshold pqr parsed_dx =
  let around_ligand =
    Dx.around_but_not_inside_mask fallout_threshold pqr parsed_dx in
  ignore(
    dump_mask ~gzip verbose fallout_threshold pqr parsed_dx around_ligand
  );
  around_ligand

let dump_thick_surface_mask
    ?(gzip = true) verbose half_thickness pqr parsed_dx =
  let result = Dx.thick_skin_mask half_thickness pqr parsed_dx in
  ignore(dump_mask ~gzip verbose half_thickness pqr parsed_dx result);
  result

let print_apbs_dot_in_file f =
  Log.info "---";
  MU.iter_on_lines_of_file (Log.info "%s") f;
  Log.info "---"

let correct_apbs_input_file new_center maybe_pdie maybe_sdie
                            dot_pqr_in dot_in_in dot_in_out dx_out =
  let dx_out_basename  = F.chop_suffix dx_out ".dx" in
  let read_pqr_cmd     = "    mol pqr"              in
  let cgrid_center_cmd = "    cgcent"               in
  let fgrid_center_cmd = "    fgcent"               in
  (* protein dielectric constant *)
  let pdie_cmd         = "    pdie"                 in
  (* solvent dielectric constant *)
  let sdie_cmd         = "    sdie"                 in
  let _dim_cmd         = "    dime"                 in
  let change_pdie      = Opt.is_some maybe_pdie     in
  let change_sdie      = Opt.is_some maybe_sdie     in
  let input_l =
    BL.map
      (fun s -> V3.(
        if BS.starts_with s read_pqr_cmd
        then (P.sprintf "%s %s" read_pqr_cmd dot_pqr_in)
        else if BS.starts_with s cgrid_center_cmd
        then (P.sprintf "%s %f %f %f" cgrid_center_cmd
                new_center.x new_center.y new_center.z)
        else if BS.starts_with s fgrid_center_cmd
        then (P.sprintf "%s %f %f %f" fgrid_center_cmd
                new_center.x new_center.y new_center.z)
        else s)
        (* (\* generick processing *\) *)
        (* if BS.starts_with s read_pqr_cmd *)
        (* then (P.sprintf "%s %s" read_pqr_cmd dot_pqr_in) *)
        (* else if BS.starts_with s cgrid_center_cmd *)
        (* then (P.sprintf "    gcent %f %f %f" *)
        (*         new_center.x new_center.y new_center.z) *)
        (* (\* some hack for grid discretization *\) *)
        (* else if BS.starts_with s "    mg-auto" then " mg-manual" *)
        (* else if BS.starts_with s "    dime"    then " dime 225 225 225" *)
        (* else if BS.starts_with s "    cglen"   then " grid 0.45 0.45 0.45" *)
        (* else if BS.starts_with s "    fglen"   then "" *)
        (* else if BS.starts_with s "    fgcent"  then "" *)
        (* else s)) *)
      )
      (MU.string_list_of_file dot_in_in) in
  let input_l' =
    if not (change_pdie || change_sdie) then input_l
    else
      BL.map
        (fun s ->
           if      change_pdie && BS.starts_with s pdie_cmd
           then P.sprintf "%s %f" pdie_cmd (Opt.get maybe_pdie)
           else if change_sdie && BS.starts_with s sdie_cmd
           then P.sprintf "%s %f" sdie_cmd (Opt.get maybe_sdie)
           else s
        )
        input_l in
  MU.string_list_to_file dot_in_out
    (BL.append
       (BL.take_while
          (fun s -> not (BS.starts_with s "    calcenergy "))
          input_l')
       ["    calcenergy no"                            ;
        "    calcforce no"                             ;
        "    write smol dx " ^ dx_out_basename ^ ".ms" ;
        "    write pot dx "  ^ dx_out_basename         ;
        "end"                                          ;
        "quit"                                         ])

let sprintf_spearman (count, (spearman, spear_ttest)) =
  P.sprintf
    "N: %d Spear: %.3f Stt: %.3f eki: %.3f"
    count spearman spear_ttest ((float_of_int count) *. spearman)

let sprintf_corr_scores
    (count, (spearman, spear_ttest), (pearson, pears_ttest), (carb, hodg)) =
  P.sprintf
    "N: %d Spear: %.3f Stt: %.3f eki: %.3f \
     Pears: %.3f Ptt: %.3f Carb: %.3f Hodg: %.3f"
    count spearman spear_ttest ((float_of_int count) *. spearman)
    pearson pears_ttest carb hodg

let sprintf_jaccard prefix (inter_card, union_card) =
  P.sprintf "%s %d / %d = %.3f"
    prefix
    inter_card
    union_card
    ((MU.itof inter_card) /. (MU.itof union_card))

let sprintf_jaccard2 inter_union_cards =
  sprintf_jaccard "Js-/+:" inter_union_cards

let sprintf_jaccard3 inter_union_cards =
  sprintf_jaccard "Js-/hp/+:" inter_union_cards

let create_AB_pdb pdb_A_in pdb_B_in pdb_AB_out =
  let pdb_A_lines = MU.some_lines_of_file PDB.is_atom_or_hetatm pdb_A_in in
  let pdb_B_lines = MU.some_lines_of_file PDB.is_atom_or_hetatm pdb_B_in in
  MU.string_list_to_file
    pdb_AB_out
    (BL.append pdb_A_lines pdb_B_lines)
