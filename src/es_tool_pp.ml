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

(* -------------------- CLI options management -------------------- *)

type options = { pdb_A      : string option ;
                 pdb_B      : string option ;
                 around_rec : float  option ;
                 around_lig : float  option ;
                 pdie       : float  option ;
                 sdie       : float  option ;
                 nprocs     : int           ;
                 verbose    : bool          }

let default_opts = ref { pdb_A      = None  ; (* mandatory *)
                         pdb_B      = None  ; (* mandatory *)
                         around_rec = None  ; (* mandatory *)
                         around_lig = None  ; (* mandatory *)
                         pdie       = None  ; (* APBS default is fine *)
                         sdie       = None  ; (* APBS default is fine *)
                         nprocs     = 1     ;
                         verbose    = false }

let set_pdb_A opts_r f  = opts_r := { !opts_r with pdb_A      = Some f  }
let set_pdb_B opts_r f  = opts_r := { !opts_r with pdb_B      = Some f  }
let set_arec  opts_r fa = opts_r := { !opts_r with around_rec = Some fa }
let set_alig  opts_r fa = opts_r := { !opts_r with around_lig = Some fa }
let set_pdie  opts_r f  = opts_r := { !opts_r with pdie       = Some f  }
let set_sdie  opts_r f  = opts_r := { !opts_r with sdie       = Some f  }
let set_np    opts_r np = opts_r := { !opts_r with nprocs     = np      }
let set_v     opts_r () = opts_r := { !opts_r with verbose    = true    }

let read_args () =
  let spec_list =
    [("-p1"  , Arg.String (set_pdb_A default_opts), "rec.pdb"                 );
     ("-p2"  , Arg.String (set_pdb_B default_opts), "lig.pdb"                 );
     ("-arec", Arg.Float  (set_arec  default_opts), "receptor ES cutoff (A)"  );
     ("-alig", Arg.Float  (set_alig  default_opts), "ligand ES cutoff (A)"    );
     ("-pdie", Arg.Float  (set_pdie  default_opts),
               "protein dielec. constant, in [2..20] usually"                 );
     ("-sdie", Arg.Float  (set_sdie  default_opts),
               "solvent dielec. constant, in [78..80] usually"                );
     ("-np"  , Arg.Int    (set_np    default_opts), "number of cores"         );
     ("-v"   , Arg.Unit   (set_v     default_opts), "be verbose"              )]
  in
  let help = P.sprintf
    "example: %s -p1 rec_prot.pdb -p2 lig_prot.pdb -arec 2.0 -alig 5.0 -v"
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
  let pdb_A, pdb_B, around_rec, around_lig, pdie, sdie, _nprocs, verbose =
    MU.get_some opts.pdb_A      "option -p1   missing",
    MU.get_some opts.pdb_B      "option -p2   missing",
    MU.get_some opts.around_rec "option -arec missing",
    MU.get_some opts.around_lig "option -alig missing",
                opts.pdie                             ,
                opts.sdie                             ,
                opts.nprocs                           ,
                opts.verbose                          in
  MU.enforce_any_file_extension pdb_A [".pdb"];
  MU.enforce_any_file_extension pdb_B [".pdb"];
  Log.info (lazy "creating A and B.pqr...");
  let pqr_A = pdb_to_pqr pdb_A in
  let pqr_B = pdb_to_pqr pdb_B in
  (* all ES fields need to be in the same cartesian space so we
     constrain their origin to be at the center of the receptor protein *)
  let dx_center = Pqr.center_of_pqr_file pqr_A in
  Log.info (lazy "creating AB pqr...");
  let pdb_AB        =
    MU.filename_with_different_extension pdb_A ".pdb" "_AB.pdb" in
  let pqr_AB        =
    MU.filename_with_different_extension pdb_A ".pdb" "_AB.pqr" in
  create_AB_pdb pdb_A pdb_B pdb_AB;
  let apbs_in_AB    =
    MU.filename_with_different_extension pdb_A ".pdb" "_AB.in"  in
  let pdb_AB_to_pqr =
    pdb2pqr ^ " --ff=AMBER --chain --whitespace --apbs-input " ^
    pdb_AB ^ " " ^ pqr_AB in
  MU.run_command pdb_AB_to_pqr;
  let apbs_in_B    =
    MU.filename_with_different_extension pdb_B ".pdb" ".in"    in
  let dx_out_B     =
    MU.filename_with_different_extension pdb_B ".pdb" ".dx"    in
  let ms_out_B     =
    MU.filename_with_different_extension pdb_B ".pdb" ".ms.dx" in
  correct_apbs_input_file
    dx_center pdie sdie pqr_B apbs_in_AB apbs_in_B dx_out_B;
  Log.info (lazy "creating B.dx...");
  let create_B_dx = apbs ^ " " ^ apbs_in_B in
  Log.info (lazy ("APBS commands used for the protein ligand: " ^ apbs_in_B));
  print_apbs_dot_in_file apbs_in_B;
  MU.run_command create_B_dx;
  Log.info (lazy "parsing B.dx...");
  let ms_r_B  = ref (Dx.parse_dx_file false ms_out_B) in
  let atoms_A = Pqr.atoms_of_pqr_file pqr_A           in
  let atoms_B = Pqr.atoms_of_pqr_file pqr_B           in
  if verbose then begin
    Log.info (lazy "dumping atoms_A and atoms_B.pdb...");
    let mask_A = Dx.inside_VdV_mask atoms_A ms_r_B in
    let mask_B = Dx.inside_VdV_mask atoms_B ms_r_B in
    Dx.mask_to_pdb mask_A ms_r_B "atoms_A.pdb";
    Dx.mask_to_pdb mask_B ms_r_B "atoms_B.pdb";
  end else ();
  Log.info (lazy "creating masks...");
  ignore(dump_mask_around_receptor verbose around_rec pqr_A ms_r_B);
  ignore(dump_mask_around_ligand   verbose around_lig pqr_B ms_r_B);
  Log.info (lazy ("masks created\n"))
;;

main()
