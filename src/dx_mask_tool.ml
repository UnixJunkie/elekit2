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

module A   = Array
module Dx  = Dx_parser
module MU  = My_utils

let main () =

  Log.set_log_level Log.INFO;
  Log.color_on();

  if A.length Sys.argv <> 5 then
    (fprintf stderr "%s"
       ("will output values of the input dx file " ^
        "with unmasked values reset to 0.0\n" ^
        "error: incorrect number of parameters\n" ^
        "usage: " ^ Sys.argv.(0) ^ " es_grid.dx protrec.mask protlig.mask ligmol.mask\n");
     (*             0                1          2            3            4 *)
     exit 1)
  else
    let dx_f          = Sys.argv.(1) in
    let prot_rec_mask = Sys.argv.(2) in
    let prot_lig_mask = Sys.argv.(3) in
    let mol_lig_mask  = Sys.argv.(4) in
    MU.enforce_any_file_extension dx_f   [".dx"  ];
    MU.enforce_any_file_extension prot_rec_mask [".mask"];
    MU.enforce_any_file_extension prot_lig_mask [".mask"];
    MU.enforce_any_file_extension mol_lig_mask  [".mask"];
    let mask  = Dx.masks_op
      [MU.unmarshal prot_rec_mask;
       MU.unmarshal prot_lig_mask;
       MU.unmarshal mol_lig_mask] (&&) in
    let dx_r  = ref (Dx.parse_dx_file false dx_f) in
    Dx.print_unmasked_reset_others dx_r mask
;;

main()
