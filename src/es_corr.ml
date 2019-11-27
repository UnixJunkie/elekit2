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

module A   = Array
module Dx  = Dx_parser
module L   = List
module MU  = My_utils
module P   = Printf

open Es_tool_common

let (|>) = MU.(|>)

let sprintf_spearman (count, (spearman, spear_ttest), (pearson, pears_ttest)) =
  P.sprintf
    "N: %d Spear: %.3f Stt: %.3f eki: %.3f \
     Pears: %.3f Ptt: %.3f"
    count spearman spear_ttest ((float_of_int count) *. spearman)
    pearson pears_ttest

let sprintf_jaccard prefix (inter_card, union_card) =
  P.sprintf "%s %d / %d = %.3f"
    prefix
    inter_card
    union_card
    ((MU.itof inter_card) /. (MU.itof union_card))

let sprintf_jaccard2 inter_union_cards =
  sprintf_jaccard "Js-/+:" inter_union_cards

let sprintf_jaccard3 inter_union_cards =
  sprintf_jaccard "Js-/hp/+:" inter_union_cards

(* -------------------- CLI options management -------------------- *)

type options = { plig_dx   : string option ;
                 plig_mask : string option ;
                 prec_mask : string option ;
                 mlig_dx   : string option ;
                 mlig_mask : string option ;
                 neg_lim   : float  option ; (* Jaccard 3 negative limit *)
                 pos_lim   : float  option ; (* Jaccard 3 positive limit *)
                 verbose   : bool          }

let default_opts = ref { plig_dx   = None        ; (* mandatory *)
                         plig_mask = None        ; (* mandatory *)
                         prec_mask = None        ; (* mandatory *)
                         mlig_dx   = None        ; (* mandatory *)
                         mlig_mask = None        ; (* mandatory *)
                         neg_lim   = Some (-1.0) ;
                         pos_lim   = Some   1.0  ;
                         verbose   = false       }

let set_plig_dx    opts_r f  = opts_r := { !opts_r with plig_dx    = Some f }
let set_plig_mask  opts_r f  = opts_r := { !opts_r with plig_mask  = Some f }
let set_prec_mask  opts_r f  = opts_r := { !opts_r with prec_mask  = Some f }
let set_mlig_dx    opts_r f  = opts_r := { !opts_r with mlig_dx    = Some f }
let set_mlig_mask  opts_r f  = opts_r := { !opts_r with mlig_mask  = Some f }
let set_neg_lim    opts_r f  = opts_r := { !opts_r with neg_lim    = Some f }
let set_pos_lim    opts_r f  = opts_r := { !opts_r with pos_lim    = Some f }
let set_v          opts_r () = opts_r := { !opts_r with verbose    = true   }

let read_args () =
  let spec_list =
  [("-d1", Arg.String (set_plig_dx    default_opts), "protein ligand dx"     );
   ("-m1", Arg.String (set_plig_mask  default_opts), "protein ligand mask"   );
   ("-m2", Arg.String (set_prec_mask  default_opts), "protein receptor mask" );
   ("-d3", Arg.String (set_mlig_dx    default_opts), "small mol. ligand dx"  );
   ("-m3", Arg.String (set_mlig_mask  default_opts), "small mol. ligand mask");
   ("-nl", Arg.Float  (set_neg_lim    default_opts), "Jaccard3 neg lim"      );
   ("-pl", Arg.Float  (set_pos_lim    default_opts), "Jaccard3 pos lim"      );
   ("-v" , Arg.Unit   (set_v          default_opts), "be verbose"            )]
  in
  let help = P.sprintf "example: %s " Sys.argv.(0) in
  (if A.length Sys.argv = 1
   then Arg.usage spec_list help
   else Arg.parse spec_list ignore help);
  !default_opts

(* -------------------- main -------------------- *)

let main () =

  Log.set_log_level Log.INFO;
  Log.color_on();

  let opts = read_args() in
  let dx1_f, mask1_f, mask2_f, dx3_f, mask3_f, neg_lim, pos_lim, _verbose =
  MU.get_some opts.plig_dx    "option -d1 missing" ,
  MU.get_some opts.plig_mask  "option -m1 missing" ,
  MU.get_some opts.prec_mask  "option -m2 missing" ,
  MU.get_some opts.mlig_dx    "option -d3 missing" ,
  MU.get_some opts.mlig_mask  "option -m3 missing" ,
  MU.get_some opts.neg_lim    "option -nl missing" ,
  MU.get_some opts.pos_lim    "option -pl missing" ,
              opts.verbose                         in
  MU.enforce_any_file_extension dx1_f   [".dx"     ];
  MU.enforce_any_file_extension dx3_f   [".dx.gz"  ];
  MU.enforce_any_file_extension mask1_f [".mask.gz"];
  MU.enforce_any_file_extension mask2_f [".mask.gz"];
  MU.enforce_any_file_extension mask3_f [".mask.gz"];
  Log.info (lazy ( "parsing lig_prot.dx..."));
  let dx_r_1   = ref (Dx.parse_dx_file false dx1_f) in
  Log.info (lazy ( "preparing mask..."));
  let mask1    = MU.unmarshal mask1_f                   in
  let mask2    = MU.unmarshal mask2_f                   in
  let mask3    = MU.unmarshal mask3_f                   in
  let mask     = Dx.masks_op [mask1; mask2; mask3] (&&) in
  try
    let dx_f   = MU.gunzip ~keep:true dx3_f        in
    let dx_r_2 = ref (Dx.parse_dx_file false dx_f) in
    Sys.remove dx_f; (* rm .dx file after use to save disk space *)
    Log.info (lazy ( "correlating..."));
    let scores = Dx.map_correl    dx_r_1 dx_r_2 mask                 in
    let jacc2  = Dx.map_jaccard_2 dx_r_1 dx_r_2 mask                 in
    let jacc3  = Dx.map_jaccard_3 neg_lim pos_lim dx_r_1 dx_r_2 mask in
    P.printf "%s %s %s %s\n"
      dx_f
      (sprintf_corr_scores scores)
      (sprintf_jaccard2 jacc2)
      (sprintf_jaccard3 jacc3)
  with MU.Command_failed msg -> Log.warn (lazy msg);
;;

main()
