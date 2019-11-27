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

(* -------------------- utils -------------------- *)

let correct_apbs_input_file dot_pqr_in dot_in_in dot_in_out dx_out =
  let dx_out_basename = F.basename (F.chop_suffix dx_out ".dx") in
  let read_pqr_cmd    = "    mol pqr"                           in
  let input_l =
    BL.map
      (fun s -> V3.(
         if BS.starts_with s read_pqr_cmd
         then (P.sprintf "%s %s" read_pqr_cmd (F.basename dot_pqr_in))
         else s))
      (MU.string_list_of_file dot_in_in) in
  MU.string_list_to_file dot_in_out
    (BL.append
       (BL.take_while
          (fun s -> not (BS.starts_with s "    calcenergy "))
          input_l)
       ["    calcenergy no"                            ;
        "    calcforce no"                             ;
        "    write smol dx " ^ dx_out_basename ^ ".ms" ;
        "    write pot dx "  ^ dx_out_basename         ;
        "end"                                          ;
        "quit"                                         ])

(* -------------------- CLI options management -------------------- *)

type options = { pdb_A      : string option ;
                 pdb_B      : string option ;
                 out        : string option ;
                 in_model   : string option ;
                 around_lig : float  option ;
                 scan_range : string option ;
                 nprocs     : int           ;
                 verbose    : bool          }

let default_opts = ref { pdb_A      = None   ; (* mandatory *)
                         pdb_B      = None   ; (* mandatory *)
                         out        = None   ; (* mandatory *)
                         in_model   = None   ; (* mandatory *)
                         around_lig = None   ; (* mandatory *)
                         scan_range = None   ;
                         nprocs     = 1      ;
                         verbose    = false  }

let set_pdb_A    opts_r f  = opts_r := { !opts_r with pdb_A      = Some f  }
let set_pdb_B    opts_r f  = opts_r := { !opts_r with pdb_B      = Some f  }
let set_out      opts_r f  = opts_r := { !opts_r with out        = Some f  }
let set_in_model opts_r f  = opts_r := { !opts_r with in_model   = Some f  }
let set_alig     opts_r fa = opts_r := { !opts_r with around_lig = Some fa }
let set_range    opts_r r  = opts_r := { !opts_r with scan_range = Some r  }
let set_np       opts_r np = opts_r := { !opts_r with nprocs     = np      }
let set_v        opts_r () = opts_r := { !opts_r with verbose    = true    }

let read_args () =
  let spec_list =
  [("-p1"  , Arg.String (set_pdb_A    default_opts), "rec.pdb"                );
   ("-p2"  , Arg.String (set_pdb_B    default_opts), "lig.mol2"               );
   ("-out" , Arg.String (set_out      default_opts), "out.files"              );
   ("-in"  , Arg.String (set_in_model default_opts), "PP.in"                  );
   ("-alig", Arg.Float  (set_alig     default_opts), "ligand ES cutoff (A)"   );
   ("-scan", Arg.String (set_range    default_opts),
    "titration parameters in the form: min:nb_steps:max (A)"                  );
   ("-np"  , Arg.Int    (set_np       default_opts), "number of cores"        );
   ("-v"   , Arg.Unit   (set_v        default_opts), "be verbose"             )]
  in
  let help = P.sprintf
    "example: %s -p1 rec_prot.pdb -p2 lig.mol2 -in PP.in -alig 5.0 -v"
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
  let pdb_A, pdb_B, out_files, in_model, around_lig, nprocs, verbose =
    MU.get_some opts.pdb_A      "option -p1   missing",
    MU.get_some opts.pdb_B      "option -p2   missing",
    MU.get_some opts.out        "option -out  missing",
    MU.get_some opts.in_model   "option -in   missing",
    MU.get_some opts.around_lig "option -alig missing",
                opts.nprocs                           ,
                opts.verbose                          in
  MU.enforce_any_file_extension pdb_A    [".pdb" ];
  MU.enforce_any_file_extension pdb_B    [".mol2"];
  MU.enforce_any_file_extension in_model [".in"  ];
  let molecules = Mol2.explode pdb_B in
  let completed = open_out out_files     in
  (* molecules from a multiple molecules mol2
     file are processed in parallel *)
  Parmap.pariter ~ncores:nprocs
    (fun (pdb_B', m_name) ->
       P.printf "creating .pqr for %s...\n%!" pdb_B';
       let maybe_pqr_B =
         mol2_to_pqr ("../" ^ pdb_A) pdb_B' in
       (* abort current iteration on failure to create the pqr file *)
       if Opt.is_some maybe_pqr_B then
         let pqr_B = Opt.get maybe_pqr_B in
         let apbs_in_B, dx_out_B, ms_out_B =
           MU.filename_with_different_extension pdb_B' ".mol2" ".in"    ,
           MU.filename_with_different_extension pdb_B' ".mol2" ".dx"    ,
           MU.filename_with_different_extension pdb_B' ".mol2" ".ms.dx" in
         P.printf "creating .in for %s...\n%!" pdb_B';
         correct_apbs_input_file pqr_B in_model apbs_in_B dx_out_B;
         let dir         = F.dirname apbs_in_B in
         let create_B_dx =
           P.sprintf
             "cd %s; %s %s >> log.txt"
             dir apbs (F.basename apbs_in_B) in
         P.printf "creating .dx for %s...\n%!" pdb_B';
         P.printf "APBS commands used for the SM ligand: %s\n%!" apbs_in_B;
         print_apbs_dot_in_file apbs_in_B;
         MU.run_command create_B_dx;
         P.printf "creating masks for %s...\n%!" pdb_B';
         let ms_r_B     = ref (Dx.parse_dx_file false ms_out_B) in
         (* delete the molecular surface .dx file after use to save space *)
         Sys.remove ms_out_B;
         ignore(dump_mask_around_ligand verbose around_lig pqr_B ms_r_B);
         let dx_out_B_gz = MU.gzip dx_out_B 1 in
         (* %! --> flush out *)
         P.fprintf completed "%s %s\n%!" dx_out_B_gz m_name)
    (Parmap.L molecules);
  close_out completed
;;

main()
