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

open Sas

module PDB = Pdb_parser

let main () =

  Log.set_log_level Log.INFO;
  Log.color_on();

  if A.length Sys.argv <> 4 then
    (P.fprintf stderr "%s"
       ("error: incorrect number of parameters\n" ^
       (*           0                1                  2 *)
        "usage: " ^ Sys.argv.(0) ^ " nb_points_per_atom molecule.pdb" ^
       (* 3 *)
        " molecule.pqr\n");
     exit 1)
  else
    let water_r      = 1.4                                  in
    let nb_points    = MU.atoi Sys.argv.(1)                 in
    let pdb_file     = Sys.argv.(2)                         in
    let pqr_file     = Sys.argv.(3)                         in
    MU.enforce_any_file_extension pdb_file [".pdb"];
    MU.enforce_any_file_extension pqr_file [".pqr"];
    let unit_vectors = ref (GS.unit_vectors nb_points)      in
    let pqr_lines    =
      MU.some_lines_of_file
        PDB.is_atom_or_hetatm
        pqr_file                                            in
    let xyz_to_pdb_line =
      PDB.create_xyz_to_pdb_line pdb_file                   in
    let atoms'       =
      L.rev_map At.atom_of_pqr_line pqr_lines               in
    (* atoms are augmented by the water probe radius *)
    let atoms        =
      L.rev_map (MU.flip At.dilate water_r) atoms'          in
    let atoms_t      = A.of_list atoms                      in
    let n            = A.length atoms_t                     in
    let neighbors_t  = compute_neighbors_lists atoms        in
    let surface_points_t =
      compute_surface_points unit_vectors atoms             in
    (* let j = ref 0 in *)
    for i = 0 to n - 1 do
      (* L.iter *)
      (* (fun p -> *)
      (*    (P.printf "%s\n" (V3.make_pdb_line p !j); *)
      (*     MU.incr j)) *)
      (* output nb solvent exposed dots for atoms that are in the input PDB *)
      let xyz = At.xyz_of atoms_t.(i) in
      if HT.mem xyz_to_pdb_line xyz then
        P.printf "%d:%s\n"
          (nb_solvent_exposed_dots surface_points_t.(i) neighbors_t.(i))
          (HT.find xyz_to_pdb_line xyz)
      else () (* maybe H atom added by pdb2pqr.py *)
    done
;;

main()
