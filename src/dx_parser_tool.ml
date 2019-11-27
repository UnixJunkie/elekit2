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

open Printf
open Dx_parser

let main () =

  Log.set_log_level Log.INFO;
  Log.color_on();

  if A.length Sys.argv <> 2 then
    (fprintf stderr "%s"
       ("error: incorrect number of parameters\n" ^
        "usage: " ^ Sys.argv.(0) ^ " file.dx\n");
     (*             0                1       2 *)
     exit 1)
  else
    let dx_filename = Sys.argv.(1) in
    parse_dx_file true dx_filename
;;

main()
