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

(* compute Solvent Accessible Surface of each atom *)

module A  = Array
module At = Atom
module BL = BatList
module BS = BatString
module GS = Generalized_spiral
module HT = Hashtbl
module L  = List
module MU = My_utils
module P  = Printf
module V3 = Vector3

(* update neighbors of a1 and a2 if necessary *)
let update_neighbors_priv (i1, a1) (i2, a2) res =
  if At.clash a1 a2 then
    (res.(i1) <- a2 :: res.(i1);
     res.(i2) <- a1 :: res.(i2))
  else ()

let rec update_neighbors a1i1 a2i2_list res =
  match a2i2_list with
      [] -> ()
    | a2i2 :: others ->
        update_neighbors_priv a1i1 a2i2 res;
        update_neighbors a1i1 others res

let compute_neighbors_lists atoms =
  let n      = L.length atoms     in
  let res    = A.create n []      in
  let atomsi = MU.enumerate atoms in
  let rec process atomsi =
    match atomsi with
        []             -> ()
      | a1i1 :: others ->
          update_neighbors a1i1 others res;
          process others
  in
  process atomsi;
  res

(* table of lists of solvent accessible surface points per atom *)
let compute_surface_points unit_vectors atoms =
  let n   = L.length atoms in
  let res = A.create n []  in
  BL.iteri
    (fun i a -> res.(i) <- At.dot_surface a unit_vectors)
    atoms;
  res

let only_solvent_exposed_dots dots neighbor_atoms =
  L.filter
    (fun dot ->
       if L.exists (At.point_is_in_atom dot) neighbor_atoms then
         false
       else
         true)
    dots

let nb_solvent_exposed_dots dots neighbor_atoms =
  L.length (only_solvent_exposed_dots dots neighbor_atoms)

let string_of_xyz (x, y, z) =
  P.sprintf "%f %f %f" x y z
