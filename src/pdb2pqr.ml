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

module F   = Filename
module L   = List
module MU  = My_utils
module P   = Printf
module PDB = Pdb_parser

(* wrapper for pdb2pqr *)

let pdb2pqr =
  MU.getenv_or_fail
    "PDB2PQR"
    "the PDB2PQR environment variable must point to pdb2pqr.py"

let pdb_to_pqr pdb_A =
  let pqr_A = MU.filename_with_different_extension pdb_A ".pdb" ".pqr" in
  let dir_A = F.dirname pdb_A                                          in
  let pdb_A_to_pqr =
    (* cd to where the file is in order to be isolated from other runs,
       each molecule should be in a different dir *)
    P.sprintf
      "cd %s; %s --ff=AMBER --chain --whitespace %s %s"
      dir_A pdb2pqr (F.basename pdb_A) (F.basename pqr_A) in
  MU.run_command pdb_A_to_pqr;
  pqr_A

(* kind of 'sed -i ...' *)
let keep_only_ligand_lines pqr_f =
  let lines = L.filter PDB.is_ligand (MU.string_list_of_file pqr_f) in
  match lines with
      [] -> false (* pdb2pqr did not manage to parameterize the ligand
                     as it is not present in the output pqr file *)
    | _  ->
      ignore(MU.string_list_to_file pqr_f lines);
      true

let mol2_to_pqr ?(gen_apbs_in_file = false) pdb_receptor mol2_ligand =
  let pqr_ligand  =
    MU.filename_with_different_extension mol2_ligand ".mol2" ".pqr" in
  let dir_ligand  = F.dirname mol2_ligand                           in
  let gen_in_file = if gen_apbs_in_file then "--apbs-input" else "" in
  try (MU.run_command ~strict:true
         (* cd to where the file is in order to be isolated from other runs,
            each molecule should be in a different dir *)
         (P.sprintf
            "cd %s; \
             %s --ligand=%s --ff=AMBER --chain --whitespace %s %s %s >> \
             log.txt"
            dir_ligand
            pdb2pqr
            (F.basename mol2_ligand)
            gen_in_file
            pdb_receptor
            (F.basename pqr_ligand));
       if keep_only_ligand_lines pqr_ligand
       then Some pqr_ligand
       else let _ = P.printf "pdb2pqr.ml: problem with %s\n%!" mol2_ligand in
            None)
  with MU.Command_failed msg ->
    P.printf "pdb2pqr.ml: exception on %s: %s\n%!" mol2_ligand msg;
    None
