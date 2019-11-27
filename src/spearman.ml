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

module A  = Array
module L  = List
module MU = My_utils
module P  = Printf
module S  = Scanf

let main () =

  Log.set_log_level Log.INFO;
  Log.color_on();

  if A.length Sys.argv <> 3 then begin
    P.fprintf stderr "%s"
      ("error: incorrect number of parameters\n\
        usage: " ^ Sys.argv.(0) ^ " pearson|spearman data_pairs_file\n");
    (*             0                1                2               3 *)
    exit 1;
  end else begin
    let correl = Sys.argv.(1) in
    let input  = Sys.argv.(2) in
    let lines  = MU.string_list_of_file input in
    let l1, l2 = ref [], ref [] in
    L.iter
      (fun l ->
         match S.sscanf l "%f %f" (fun x y -> (x, y)) with
             (x, y) ->
               l1 := x :: !l1;
               l2 := y :: !l2)
      lines;
    match correl with
        "pearson"  ->
          let pears, signif = MU.pearson_l !l1 !l2  in
          P.printf "r: %f t: %f\n" pears signif
      | "spearman" ->
          let spear, signif = MU.spearman_l !l1 !l2 in
          P.printf "r: %f t: %f\n" spear signif
      | _ -> failwith ("unsupported correlation: " ^ correl)
  end
;;

main()
