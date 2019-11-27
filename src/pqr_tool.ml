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

open Pqr

module A   = Array
module L   = List
module MU  = My_utils
module P   = Printf
module PDB = Pdb_parser

let main () =

  Log.set_log_level Log.INFO;
  Log.color_on();

  let argc = A.length Sys.argv in
  if argc <> 2 && argc <> 3 then begin
    FATAL_NP(log, P.sprintf
               "error: incorrect number of parameters\n\
                usage: %s molecule.pqr [random_seed]\n" Sys.argv.(0));
    (*                 0  1            2             3 *)
    exit 1;
  end else
    let pqr_file = Sys.argv.(1) in
    MU.enforce_any_file_extension pqr_file [".pqr"];
    let seed =
      if argc = 3 then
        let s = MU.atoi Sys.argv.(2) in
        RNG.init s;
        s
      else
        MU.init_RNG()
    in
    INFO_NP(log, P.sprintf "RNG seed: %d" seed);
    let lines = MU.some_lines_of_file PDB.is_atom_or_hetatm pqr_file in
    L.iter (MU.compose (P.printf "%s\n") process_pqr_line) lines;
;;

main()
