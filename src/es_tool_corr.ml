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

(* -------------------- CLI options management -------------------- *)

type options = { plig_dx    : string option ;
                 plig_mask  : string option ;
                 prec_mask  : string option ;
                 dx_masks_f : string option ;
                 values_f   : string        ;
                 neg_lim    : float  option ; (* Jaccard 3 negative limit *)
                 pos_lim    : float  option ; (* Jaccard 3 positive limit *)
                 nprocs     : int           ;
                 verbose    : bool          }

let default_opts = ref { plig_dx    = None        ; (* mandatory *)
                         plig_mask  = None        ; (* mandatory *)
                         prec_mask  = None        ; (* mandatory *)
                         dx_masks_f = None        ; (* mandatory *)
                         values_f   = ""          ;
                         neg_lim    = Some (-1.0) ;
                         pos_lim    = Some   1.0  ;
                         nprocs     = 1           ;
                         verbose    = false       }

let set_plig_dx    opts_r f  = opts_r := { !opts_r with plig_dx    = Some f }
let set_plig_mask  opts_r f  = opts_r := { !opts_r with plig_mask  = Some f }
let set_prec_mask  opts_r f  = opts_r := { !opts_r with prec_mask  = Some f }
let set_dx_masks_f opts_r f  = opts_r := { !opts_r with dx_masks_f = Some f }
let set_values_f   opts_r f  = opts_r := { !opts_r with values_f   = f      }
let set_neg_lim    opts_r f  = opts_r := { !opts_r with neg_lim    = Some f }
let set_pos_lim    opts_r f  = opts_r := { !opts_r with pos_lim    = Some f }
let set_np         opts_r np = opts_r := { !opts_r with nprocs     = np     }
let set_v          opts_r () = opts_r := { !opts_r with verbose    = true   }

let read_args () =
  let spec_list =
  [("-d1", Arg.String (set_plig_dx    default_opts), "protein ligand dx"     );
   ("-m1", Arg.String (set_plig_mask  default_opts), "protein ligand mask"   );
   ("-m2", Arg.String (set_prec_mask  default_opts), "protein receptor mask" );
   ("-f" , Arg.String (set_dx_masks_f default_opts), "dx and masks list file");
   ("-o" , Arg.String (set_values_f   default_opts), "file for raw value \
                                                      pairs output"          );
   ("-nl", Arg.Float  (set_neg_lim    default_opts), "Jaccard3 neg lim"      );
   ("-pl", Arg.Float  (set_pos_lim    default_opts), "Jaccard3 pos lim"      );
   ("-np", Arg.Int    (set_np         default_opts), "number of cores"       );
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
  let dx1_f, mask1_f, mask2_f, dx_mask_list, neg_lim, pos_lim,
      values_f, nprocs, verbose =
  MU.get_some opts.plig_dx    "option -d1 missing" ,
  MU.get_some opts.plig_mask  "option -m1 missing" ,
  MU.get_some opts.prec_mask  "option -m2 missing" ,
  MU.get_some opts.dx_masks_f "option -f  missing" ,
  MU.get_some opts.neg_lim    "option -nl missing" ,
  MU.get_some opts.pos_lim    "option -pl missing" ,
              opts.values_f                        ,
              opts.nprocs                          ,
              opts.verbose                         in
  if (values_f <> "" && nprocs > 1)
  then failwith "-r not possible in multiprocessor mode";
  (* verbose --> save the final mask as a PDB *)
  MU.enforce_any_file_extension dx1_f   [".dx"     ];
  MU.enforce_any_file_extension mask1_f [".mask.gz"];
  MU.enforce_any_file_extension mask2_f [".mask.gz"];
  let dx_mask_files =
    MU.map_on_lines_of_file
      (fun s ->
         Scanf.sscanf s
           "%s %s %s"
           (fun dx_f mask_f m_name -> (dx_f, mask_f, m_name)))
      dx_mask_list in
  Log.info (lazy ( "parsing lig_prot.dx..."));
  let dx_r_1   = ref (Dx.parse_dx_file false dx1_f) in
  Log.info (lazy ( "preparing mask..."));
  let mask1    = MU.unmarshal mask1_f            in
  let mask2    = MU.unmarshal mask2_f            in
  let pre_mask = Dx.masks_op [mask1; mask2] (&&) in
  (* L.iter *)
  Parmap.pariter ~ncores:nprocs ~chunksize:1
    (fun (dx_f_gz, mask_f, m_name) ->
       MU.enforce_any_file_extension dx_f_gz [".dx.gz"  ];
       MU.enforce_any_file_extension mask_f  [".mask.gz"];
       let curr_mask = MU.unmarshal mask_f                    in
       let mask      = Dx.masks_op [pre_mask; curr_mask] (&&) in
       try
         let dx_f   = MU.gunzip ~keep:true dx_f_gz      in
         let dx_r_2 = ref (Dx.parse_dx_file false dx_f) in
         (if verbose then
             MU.filename_with_different_extension
               mask_f ".mask.gz" ".pdb" |> Dx.mask_to_pdb mask dx_r_2);
         Sys.remove dx_f; (* rm .dx file after use to save disk space *)
         if values_f <> "" then begin
           Log.info (lazy ( "writing value pairs out..."));
           Dx.output_unmasked_values dx_r_1 dx_r_2 mask values_f;
         end;
         Log.info (lazy ( "correlating..."));
         let corr_scores = Dx.map_correl dx_r_1 dx_r_2 mask              in
         let jacc2 = Dx.map_jaccard_2 dx_r_1 dx_r_2 mask                 in
         let jacc3 = Dx.map_jaccard_3 neg_lim pos_lim dx_r_1 dx_r_2 mask in
         P.printf "%s %s %s %s mol: %s\n"
           dx_f
           (sprintf_corr_scores corr_scores)
           (sprintf_jaccard2 jacc2)
           (sprintf_jaccard3 jacc3)
           m_name
       with MU.Command_failed msg -> Log.warn (lazy msg);
    )
    (* dx_mask_files *)
    (Parmap.L dx_mask_files)
;;

main()
