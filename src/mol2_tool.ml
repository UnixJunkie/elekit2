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
module L    = List
module Mol2 = Mol2_parser
module P    = Printf

let main () =

  Log.set_log_level Log.INFO;
  Log.color_on();

  if A.length Sys.argv <> 2 then
    (P.fprintf stderr "%s"
       ("error: incorrect number of parameters\n" ^
        "usage: " ^ Sys.argv.(0) ^ " FILE.mol2\n");
     (*             0                1 *)
     exit 1)
  else
    let new_files = Mol2.explode Sys.argv.(1) in
    Log.info (lazy "Files created:");
    L.iter
      (fun (fn, _m_name) -> Log.info (lazy fn))
      new_files
;;

main()
