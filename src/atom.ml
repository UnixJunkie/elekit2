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
module L   = List
module V3  = Vector3

type atom = { position: V3.t  ; (* x, y, z in the PDB *)
              radius:   float } (* van der vals radius *)

let new_atom x y z r =
  { position = V3.make x y z ;
    radius   = r }

let dummy =
  new_atom 0. 0. 0. 1.

let atom_of_vector3 r v =
  { position = v ;
    radius   = r }

let atom_of_pqr_line l =
  let a2f    = float_of_string         in
  (* end of a pqr line: x y z q r *)
  let tokens = L.rev (Str.split (Str.regexp "[ ]+") l) in
  let res    =
    match tokens with
        rs :: _qs :: zs :: ys :: xs :: _ ->
          (try Some { position = V3.make (a2f xs) (a2f ys) (a2f zs) ;
                      radius   = a2f rs }
           with _ -> None)
      | _ -> None
  in
  match res with
      Some a -> a
    | None   -> failwith ("atom_of_pqr_line: could not parse: " ^ l)

let move atom displacement =
  { position = V3.add atom.position displacement ;
    radius   = atom.radius }

(* add r to radius *)
let dilate a r =
  { position = a.position    ;
    radius   = a.radius +. r }

(* remove r to radius if possible
   or set the radius to almost 0 if not *)
let erode a r =
  if a.radius > r then
    { position = a.position    ;
      radius   = a.radius -. r }
  else
    let epsilon = 0.01 in
    { position = a.position ;
      radius   = epsilon    }

let distance atom1 atom2 =
  V3.dist atom1.position atom2.position

let distance_to_point a p =
  V3.mag (V3.diff a.position p)

let squared_distance_to_point a p =
  V3.mag2 (V3.diff a.position p)

let point_is_in_atom p a =
  squared_distance_to_point a p <= a.radius *. a.radius

let point_within_d2_of_atom p d2 a =
  squared_distance_to_point a p <= d2

(* do they overlap? *)
let clash atom1 atom2 =
  let r1 = atom1.radius in
  let r2 = atom2.radius in
  distance atom1 atom2 <= r1 +. r2

(* return xmin, xmax, ymin, ymax, zmin, zmax *)
let bounding_box a = V3.(
  let x = a.position.x in
  let y = a.position.y in
  let z = a.position.z in
  let r = a.radius     in
  (x -. r, x +. r,
   y -. r, y +. r,
   z -. r, z +. r))

(* list surface points *)
let dot_surface a unit_vectors = V3.(
  let x = a.position.x in
  let y = a.position.y in
  let z = a.position.z in
  let r = a.radius     in
  let scale_then_translate v =
    add (make x y z) (mult v r)
  in
  List.rev_map scale_then_translate !unit_vectors)

let xyz_of a = V3.(
  a.position.x, a.position.y, a.position.z)

let string_of_atom atom = V3.(
  (string_of_float atom.position.x) ^ " " ^
  (string_of_float atom.position.y) ^ " " ^
  (string_of_float atom.position.z) ^ " " ^
  (string_of_float atom.radius))

let atom_of_string s = V3.(
  Scanf.sscanf s "%f %f %f %f"
    (fun x y z r -> { position = { x = x ;
                                   y = y ;
                                   z = z } ;
                      radius   = r }))
