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
module Dx   = Dx_parser
module F    = Filename
module L    = List
module Mol2 = Mol2_parser
module MU   = My_utils
module P    = Printf
module Opt  = BatOption
module PDB  = Pdb_parser
module RNG  = Random
module V3   = Vector3

open Pdb2pqr
open Apbs
open Es_tool_common

let (|>) = MU.(|>)

(* -------------------- CLI options management -------------------- *)

type options = { pdb_A      : string option ;
                 pdb_B      : string option ;
                 around_lig : float  option ;
                 surface    : float  option ;
                 sdie       : float  option ;
                 nprocs     : int           ;
                 keep_dx    : bool          ;
                 verbose    : bool          }

let default_opts = ref { pdb_A      = None   ; (* mandatory *)
                         pdb_B      = None   ; (* mandatory *)
                         around_lig = None   ; (* mandatory *)
                         surface    = None   ; (* mandatory *)
                         sdie       = None   ; (* APBS default is fine *)
                         nprocs     = 1      ;
                         keep_dx    = false  ;
                         verbose    = false  }

let set_pdb_A opts_r f  = opts_r := { !opts_r with pdb_A      = Some f  }
let set_pdb_B opts_r f  = opts_r := { !opts_r with pdb_B      = Some f  }
let set_surf  opts_r f  = opts_r := { !opts_r with surface    = Some f  }
let set_sdie  opts_r f  = opts_r := { !opts_r with sdie       = Some f  }
let set_np    opts_r np = opts_r := { !opts_r with nprocs     = np      }
let set_keep  opts_r () = opts_r := { !opts_r with keep_dx    = true    }
let set_v     opts_r () = opts_r := { !opts_r with verbose    = true    }

let read_args () =
  let spec_list =
  [("-rec" , Arg.String (set_pdb_A    default_opts), "rec.pdb"          );
   ("-lig" , Arg.String (set_pdb_B    default_opts), "lig.mol2"         );
   ("-sdie", Arg.Float  (set_sdie     default_opts), "solvent dielec. \
             constant, in [78..80] usually for water"                   );
   ("-surf", Arg.Float  (set_surf     default_opts), "x half surface thickness(A)");
   ("-np"  , Arg.Int    (set_np       default_opts), "number of cores"  );
   ("-k"   , Arg.Unit   (set_keep     default_opts), "keep dx files"    );
   ("-v"   , Arg.Unit   (set_v        default_opts), "output final mask")]
  in
  let help = P.sprintf
    "example:\n\
     %s -rec 2b4j-receptor.pdb -lig ledgin1.mol2 -surf 0.38"
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
  (* A is the receptor protein, B is the ligand molecule *)
  let pdb_A, pdb_B, surface_thickness,
    sdie, nprocs, keep_dx, verbose =
    MU.get_some opts.pdb_A      "option -p1   missing",
    MU.get_some opts.pdb_B      "option -p2   missing",
    MU.get_some opts.surface    "option -surf missing",
                opts.sdie                             ,
                opts.nprocs                           ,
                opts.keep_dx                          ,
                opts.verbose                          in
  MU.enforce_any_file_extension pdb_A [".pdb" ];
  MU.enforce_any_file_extension pdb_B [".mol2"];
  let pqr_A       = pdb_to_pqr pdb_A             in
  let grid_center = Pqr.center_of_pqr_file pqr_A in
  let molecules   = Mol2.explode pdb_B           in
  let apbs_in_A, dx_out_A, ms_out_A =
    MU.filename_with_different_extension pdb_A ".pdb" ".in"    ,
    MU.filename_with_different_extension pdb_A ".pdb" ".dx"    ,
    MU.filename_with_different_extension pdb_A ".pdb" ".ms.dx" in
  (* some redundant processing of the first molecule is inevitable *)
  let in_model = "apbs_cmd_model.in"  in
  let pdb_B'   = fst (L.hd molecules) in
  let maybe_pqr_B =
    mol2_to_pqr ~gen_apbs_in_file:true ("../" ^ pdb_A) pdb_B' in
  if Opt.is_some maybe_pqr_B then
    let pqr_B = Opt.get maybe_pqr_B in
    let apbs_in_B, dx_out_B =
      MU.filename_with_different_extension pdb_B' ".mol2" ".in"    ,
      MU.filename_with_different_extension pdb_B' ".mol2" ".dx"    in
    P.printf "creating template commands for apbs in %s...\n%!" in_model;
    correct_apbs_input_file
      grid_center None sdie (F.basename pqr_B) apbs_in_B in_model
      (F.basename dx_out_B);
  else failwith ("FATAL: failure on first molecule: " ^ pdb_B');
  (* RECEPTOR ####################################################### *)
  P.printf "creating .in for %s...\n%!" pqr_A;
  correct_apbs_input_file
    grid_center None sdie pqr_A in_model apbs_in_A dx_out_A;
  P.printf "running apbs for %s...\n%!" pqr_A;
  let create_A_dx =
    P.sprintf
      "export OMP_NUM_THREADS=%d ; %s %s >> log.txt"
      nprocs apbs (F.basename apbs_in_A) in
  P.printf "creating .dx for %s...\n%!" pdb_A;
  P.printf "apbs commands used for the receptor: %s\n%!" apbs_in_A;
  print_apbs_dot_in_file apbs_in_A;
  MU.run_command create_A_dx;
  P.printf "creating masks for %s...\n%!" pdb_A;
  let ms_r_A = ref (Dx.parse_dx_file false ms_out_A) in
  (* delete the molecular surface file after use to save space *)
  Sys.remove ms_out_A;
  let receptor_surface =
    dump_thick_surface_mask
      ~gzip:false verbose surface_thickness pqr_A ms_r_A in
  let dx_r_1 = ref (Dx.parse_dx_file false dx_out_A) in
  (* molecules from a multiple molecules mol2 file are processed in parallel *)
  (* L.iter *)
  Parmap.pariter ~ncores:nprocs
    (fun (pdb_B', m_name) -> try
       P.printf "creating .pqr for %s...\n%!" pdb_B';
       let maybe_pqr_B =
         mol2_to_pqr ~gen_apbs_in_file:true ("../" ^ pdb_A) pdb_B' in
       (* abort current iteration on failure to create the pqr file *)
       if Opt.is_some maybe_pqr_B then
         (* LIGAND ######################################################### *)
         let pqr_B = Opt.get maybe_pqr_B in
         let apbs_in_B, dx_out_B, ms_out_B =
           MU.filename_with_different_extension pdb_B' ".mol2" ".in"    ,
           MU.filename_with_different_extension pdb_B' ".mol2" ".dx"    ,
           MU.filename_with_different_extension pdb_B' ".mol2" ".ms.dx" in
         P.printf "creating template commands for apbs in %s...\n%!" in_model;
         correct_apbs_input_file
           grid_center None sdie (F.basename pqr_B) apbs_in_B in_model
           (F.basename dx_out_B);
         P.printf "creating .in for %s...\n%!" pqr_B;
         MU.run_command ("cp " ^ in_model ^ " " ^ apbs_in_B);
         P.printf "running apbs for %s...\n%!" pqr_B;
         let dir         = F.dirname apbs_in_B in
         let create_B_dx =
           P.sprintf
             "cd %s; %s %s >> log.txt"
             dir apbs (F.basename apbs_in_B) in
         P.printf "creating .dx for %s...\n%!" pdb_B';
         P.printf "apbs commands used for the ligand: %s\n%!" apbs_in_B;
         print_apbs_dot_in_file apbs_in_B;
         MU.run_command create_B_dx;
         P.printf "creating masks for %s...\n%!" pdb_B';
         let ms_r_B = ref (Dx.parse_dx_file false ms_out_B) in
         (* delete the molecular surface file after use to save space *)
         Sys.remove ms_out_B;
         let ligand_surface =
           dump_thick_surface_mask verbose surface_thickness pqr_B ms_r_B in
         (* CORRELATION #################################################### *)
         Log.info "preparing mask...";
         let mask = Dx.masks_op [receptor_surface; ligand_surface] (&&) in
         Log.info "parsing dx files...";
         let dx_r_2 = ref (Dx.parse_dx_file false dx_out_B) in
         (if verbose then
             MU.filename_with_different_extension
               pdb_B' ".mol2" ".final_mask.pdb" |> Dx.mask_to_pdb mask dx_r_2);
         Log.info "correlating...";
         let corr_scores = Dx.map_correl dx_r_1 dx_r_2 mask in
         P.printf "%s %s mol: %s\n%!"
           dx_out_B
           (sprintf_corr_scores corr_scores)
           m_name;
         (* delete ligand molecule dx file after use to save space *)
         (if not keep_dx
          then Sys.remove dx_out_B)
      with MU.Command_failed msg -> Log.warn "%s" msg)
    (* molecules *)
    (Parmap.L molecules)
;;

main()
