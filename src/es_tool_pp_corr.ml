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

module A   = Array
module At  = Atom
module BL  = BatList
module BS  = BatString
module Dx  = Dx_parser
module F   = Filename
module L   = List
module MU  = My_utils
module P   = Printf
module Opt = BatOption
module PDB = Pdb_parser
module RNG = Random
module V3  = Vector3

open Pdb2pqr
open Apbs
open Es_tool_common

let (|>) = MU.(|>)

(* -------------------- CLI options management -------------------- *)

type options = { pdb_A      : string option ;
                 pdb_B      : string option ;
                 around_rec : float  option ;
                 verbose    : bool          }

let default_opts = ref { pdb_A      = None  ; (* mandatory *)
                         pdb_B      = None  ; (* mandatory *)
                         around_rec = None  ; (* mandatory *)
                         verbose    = false }

let set_pdb_A opts_r f  = opts_r := { !opts_r with pdb_A      = Some f  }
let set_pdb_B opts_r f  = opts_r := { !opts_r with pdb_B      = Some f  }
let set_arec  opts_r fa = opts_r := { !opts_r with around_rec = Some fa }
let set_v     opts_r () = opts_r := { !opts_r with verbose    = true    }

let read_args () =
  let spec_list =
    [("-p1"  , Arg.String (set_pdb_A default_opts), "rec.pdb"               );
     ("-p2"  , Arg.String (set_pdb_B default_opts), "lig.pdb"               );
     ("-c"   , Arg.Float  (set_arec  default_opts), "ES cutoff (A)"         );
     ("-v"   , Arg.Unit   (set_v     default_opts), "be verbose"            )]
  in
  let help = P.sprintf
    "example: %s -p1 rec_prot.pdb -p2 lig_prot.pdb -c 1.4 -v"
    Sys.argv.(0) in
  (if A.length Sys.argv = 1
   then Arg.usage spec_list help
   else Arg.parse spec_list ignore help);
  !default_opts

(* -------------------- main -------------------- *)

let main () =

  Log.set_log_level Log.INFO;
  Log.color_on();

  let opts = read_args() in
  (* A is the receptor, B is the ligand *)
  let pdb_A, pdb_B, around_rec, verbose =
    MU.get_some opts.pdb_A      "option -p1 missing",
    MU.get_some opts.pdb_B      "option -p2 missing",
    MU.get_some opts.around_rec "option -c  missing",
                opts.verbose                        in
  MU.enforce_any_file_extension pdb_A [".pdb"];
  MU.enforce_any_file_extension pdb_B [".pdb"];
  Log.info (lazy "creating A and B.pqr...");
  let pqr_A, pqr_B =
    pdb_to_pqr pdb_A,
    pdb_to_pqr pdb_B in
  Log.info (lazy "creating AB pqr...");
  let pdb_AB, pqr_AB =
    MU.filename_with_different_extension pdb_A ".pdb" "_AB.pdb",
    MU.filename_with_different_extension pdb_A ".pdb" "_AB.pqr" in
  create_AB_pdb pdb_A pdb_B pdb_AB;
  let apbs_in_AB    =
    MU.filename_with_different_extension pdb_A ".pdb" "_AB.in"  in
  let pdb_AB_to_pqr =
    pdb2pqr ^ " --ff=AMBER --chain --whitespace --apbs-input " ^
    pdb_AB ^ " " ^ pqr_AB in
  MU.run_command pdb_AB_to_pqr;
  (* all ES fields need to be in the same cartesian space so we
     constrain their origin to be at the center of the AB complex *)
  let dx_center = Pqr.center_of_pqr_file pqr_AB in
  let apbs_in_A, apbs_in_B, ms_out_A, ms_out_B =
    MU.filename_with_different_extension pdb_A ".pdb" ".in"    ,
    MU.filename_with_different_extension pdb_B ".pdb" ".in"    ,
    MU.filename_with_different_extension pdb_A ".pdb" ".ms.dx" , 
    MU.filename_with_different_extension pdb_B ".pdb" ".ms.dx" in
  let dx_out_A, dx_out_B =
    MU.filename_with_different_extension pdb_A ".pdb" ".dx" ,
    MU.filename_with_different_extension pdb_B ".pdb" ".dx" in
  correct_apbs_input_file
    dx_center None None pqr_A apbs_in_AB apbs_in_A dx_out_A;
  correct_apbs_input_file
    dx_center None None pqr_B apbs_in_AB apbs_in_B dx_out_B;
  Log.info (lazy "creating A.dx and B.dx...");
  let create_A_dx, create_B_dx =
    apbs ^ " " ^ apbs_in_A,
    apbs ^ " " ^ apbs_in_B in
  Log.info (lazy ("APBS commands in " ^ apbs_in_A));
  print_apbs_dot_in_file apbs_in_A;
  MU.run_command create_A_dx;
  Log.info (lazy ("APBS commands in " ^ apbs_in_B));
  print_apbs_dot_in_file apbs_in_B;
  MU.run_command create_B_dx;
  Log.info (lazy  "parsing molecular surfaces for A and B...");
  let ms_r_A, ms_r_B =
    ref (Dx.parse_dx_file false ms_out_A),
    ref (Dx.parse_dx_file false ms_out_B) in
  (* delete the molecular surface files after use to save space *)
(*   Sys.remove ms_out_A; *)
(*   Sys.remove ms_out_B; *)
  Log.info (lazy  "creating masks...");
  let mask1, mask2 =
    dump_mask_around_ligand verbose around_rec pqr_A ms_r_A ,
    dump_mask_around_ligand verbose around_rec pqr_B ms_r_B in
  let mask = Dx.masks_op [mask1; mask2] (&&) in
  try
    Log.info (lazy  "parsing dx files...");
    let dx_r_1 = ref (Dx.parse_dx_file false dx_out_A) in
    let dx_r_2 = ref (Dx.parse_dx_file false dx_out_B) in
    (if verbose then
        MU.filename_with_different_extension
          pdb_B ".pdb" ".final_mask.pdb" |> Dx.mask_to_pdb mask dx_r_2);
    Log.info (lazy  "correlating...");
    let corr_scores = Dx.map_correl dx_r_1 dx_r_2 mask in
    P.printf "%s %s\n" dx_out_B (sprintf_corr_scores corr_scores)
  with MU.Command_failed msg -> Log.warn (lazy msg)
;;

main()
