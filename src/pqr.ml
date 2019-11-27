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

module At  = Atom
module BL  = BatList
module BS  = BatString
module L   = List
module MU  = My_utils
module PDB = Pdb_parser
module RNG = Random
module S   = String
module Sc  = Scanf
module V3  = Vector3

open Printf

let tiny_pos_charge = "  0.0001"
let tiny_neg_charge = " -0.0001"

(* replace charge in a line with a random very weak one *)
let process_pqr_line l =
  (* example PQR line:
     "ATOM      1  N   ASP A 685     -13.412   4.512  17.116  0.0782 1.8240"
      |                                                       |     |
      0                                                       qs    qe
  *)
  (* WARNING: this is quite fragile code here if the pqr format output by
              pdb2pqr changes *)
  let tokens = L.rev (BS.nsplit l " ") in
  match tokens with
      _r :: q :: _z :: _ ->
        (let qs     = BS.find l q   in
         let q_len  =  S.length q   in
         let qe     = qs + q_len    in
         let prefix =
           BS.strip ~chars:" "
             (S.sub l 0 qs)         in
         let suffix = BS.tail l qe in
           (* printf "%s\n" l; *)
           (* printf "%s\n" prefix; *)
           (* printf "%s\n" suffix; *)
         let new_charge =
           if RNG.bool()
           then tiny_pos_charge
           else tiny_neg_charge in
         sprintf "%s%s%s" prefix new_charge suffix)
    | _ -> failwith (sprintf "process_pqr_line: cannot parse: %s" l)

let xyz_of_pqr_line l =
  let atof   = MU.atof                                 in
  let tokens = L.rev (Str.split (Str.regexp "[ ]+") l) in
  (* printf "%s\n" (MU.string_of_list MU.identity "; " tokens); *)
  match tokens with
      _r :: _q :: z :: y :: x :: _ -> V3.make (atof x) (atof y) (atof z)
    | _ -> failwith (sprintf "xyz_of_pqr_line: cannot parse: %s" l)

let atoms_of_pqr_file f =
  MU.enforce_any_file_extension f [".pqr"];
  let pqr_lines = MU.some_lines_of_file PDB.is_atom_or_hetatm f in
  BL.map At.atom_of_pqr_line pqr_lines

let center_of_pqr_file f =
  let atom_coords =
    MU.map_on_some_lines_of_file PDB.is_atom_or_hetatm xyz_of_pqr_line f in
  let sum         =
    L.fold_left
      (fun acc xyz -> V3.add acc xyz)
      V3.origin atom_coords in
  V3.div sum (float_of_int (L.length atom_coords))
