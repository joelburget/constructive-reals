(*

Copyright (C) 2015 The Android Open Source Project
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
   http://www.apache.org/licenses/LICENSE-2.0
It is also reproduced at the end of this file.
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
All material in this directory, except SlowCRTest.java and ConversionTest.java,
is also covered by one or both of the following:
---------------------------------------
Copyright © 1999, Silicon Graphics, Inc. -- ALL RIGHTS RESERVED
Permission is granted free of charge to copy, modify, use and distribute
this software  provided you include the entirety of this notice in all
copies made.
THIS SOFTWARE IS PROVIDED ON AN AS IS BASIS, WITHOUT WARRANTY OF ANY
KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, WITHOUT LIMITATION,
WARRANTIES THAT THE SUBJECT SOFTWARE IS FREE OF DEFECTS, MERCHANTABLE, FIT
FOR A PARTICULAR PURPOSE OR NON-INFRINGING.   SGI ASSUMES NO RISK AS TO THE
QUALITY AND PERFORMANCE OF THE SOFTWARE.   SHOULD THE SOFTWARE PROVE
DEFECTIVE IN ANY RESPECT, SGI ASSUMES NO COST OR LIABILITY FOR ANY
SERVICING, REPAIR OR CORRECTION.  THIS DISCLAIMER OF WARRANTY CONSTITUTES
AN ESSENTIAL PART OF THIS LICENSE. NO USE OF ANY SUBJECT SOFTWARE IS
AUTHORIZED HEREUNDER EXCEPT UNDER THIS DISCLAIMER.
UNDER NO CIRCUMSTANCES AND UNDER NO LEGAL THEORY, WHETHER TORT (INCLUDING,
WITHOUT LIMITATION, NEGLIGENCE OR STRICT LIABILITY), CONTRACT, OR
OTHERWISE, SHALL SGI BE LIABLE FOR ANY DIRECT, INDIRECT, SPECIAL,
INCIDENTAL, OR CONSEQUENTIAL DAMAGES OF ANY CHARACTER WITH RESPECT TO THE
SOFTWARE INCLUDING, WITHOUT LIMITATION, DAMAGES FOR LOSS OF GOODWILL, WORK
STOPPAGE, LOSS OF DATA, COMPUTER FAILURE OR MALFUNCTION, OR ANY AND ALL
OTHER COMMERCIAL DAMAGES OR LOSSES, EVEN IF SGI SHALL HAVE BEEN INFORMED OF
THE POSSIBILITY OF SUCH DAMAGES.  THIS LIMITATION OF LIABILITY SHALL NOT
APPLY TO LIABILITY RESULTING FROM SGI's NEGLIGENCE TO THE EXTENT APPLICABLE
LAW PROHIBITS SUCH LIMITATION.  SOME JURISDICTIONS DO NOT ALLOW THE
EXCLUSION OR LIMITATION OF INCIDENTAL OR CONSEQUENTIAL DAMAGES, SO THAT
EXCLUSION AND LIMITATION MAY NOT APPLY TO YOU.
These license terms shall be governed by and construed in accordance with
the laws of the United States and the State of California as applied to
agreements entered into and to be performed entirely within California
between California residents.  Any litigation relating to these license
terms shall be subject to the exclusive jurisdiction of the Federal Courts
of the Northern District of California (or, absent subject matter
jurisdiction in such courts, the courts of the State of California), with
venue lying exclusively in Santa Clara County, California.
--------------------------------------------
Copyright (c) 2001-2002, Hewlett-Packard Company -- ALL RIGHTS RESERVED
Permission is granted free of charge to copy, modify, use and distribute
this software  provided you include the entirety of this notice in all
copies made.
THIS SOFTWARE IS PROVIDED ON AN AS IS BASIS, WITHOUT WARRANTY OF ANY
KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, WITHOUT LIMITATION,
WARRANTIES THAT THE SUBJECT SOFTWARE IS FREE OF DEFECTS, MERCHANTABLE, FIT
FOR A PARTICULAR PURPOSE OR NON-INFRINGING.   HEWLETT-PACKARD ASSUMES
NO RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE.
SHOULD THE SOFTWARE PROVE DEFECTIVE IN ANY RESPECT,
HEWLETT-PACKARD ASSUMES NO COST OR LIABILITY FOR ANY
SERVICING, REPAIR OR CORRECTION.  THIS DISCLAIMER OF WARRANTY CONSTITUTES
AN ESSENTIAL PART OF THIS LICENSE. NO USE OF ANY SUBJECT SOFTWARE IS
AUTHORIZED HEREUNDER EXCEPT UNDER THIS DISCLAIMER.
UNDER NO CIRCUMSTANCES AND UNDER NO LEGAL THEORY, WHETHER TORT (INCLUDING,
WITHOUT LIMITATION, NEGLIGENCE OR STRICT LIABILITY), CONTRACT, OR
OTHERWISE, SHALL HEWLETT-PACKARD BE LIABLE FOR ANY DIRECT, INDIRECT, SPECIAL,
INCIDENTAL, OR CONSEQUENTIAL DAMAGES OF ANY CHARACTER WITH RESPECT TO THE
SOFTWARE INCLUDING, WITHOUT LIMITATION, DAMAGES FOR LOSS OF GOODWILL, WORK
STOPPAGE, LOSS OF DATA, COMPUTER FAILURE OR MALFUNCTION, OR ANY AND ALL
OTHER COMMERCIAL DAMAGES OR LOSSES, EVEN IF HEWLETT-PACKARD SHALL
HAVE BEEN INFORMED OF THE POSSIBILITY OF SUCH DAMAGES.
THIS LIMITATION OF LIABILITY SHALL NOT APPLY TO LIABILITY RESULTING
FROM HEWLETT-PACKARD's NEGLIGENCE TO THE EXTENT APPLICABLE
LAW PROHIBITS SUCH LIMITATION.  SOME JURISDICTIONS DO NOT ALLOW THE
EXCLUSION OR LIMITATION OF INCIDENTAL OR CONSEQUENTIAL DAMAGES, SO THAT
EXCLUSION AND LIMITATION MAY NOT APPLY TO YOU.
---------------------------------------------
                                 Apache License
                           Version 2.0, January 2004
                        http://www.apache.org/licenses/
   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION
   1. Definitions.
      "License" shall mean the terms and conditions for use, reproduction,
      and distribution as defined by Sections 1 through 9 of this document.
      "Licensor" shall mean the copyright owner or entity authorized by
      the copyright owner that is granting the License.
      "Legal Entity" shall mean the union of the acting entity and all
      other entities that control, are controlled by, or are under common
      control with that entity. For the purposes of this definition,
      "control" means (i) the power, direct or indirect, to cause the
      direction or management of such entity, whether by contract or
      otherwise, or (ii) ownership of fifty percent (50%) or more of the
      outstanding shares, or (iii) beneficial ownership of such entity.
      "You" (or "Your") shall mean an individual or Legal Entity
      exercising permissions granted by this License.
      "Source" form shall mean the preferred form for making modifications,
      including but not limited to software source code, documentation
      source, and configuration files.
      "Object" form shall mean any form resulting from mechanical
      transformation or translation of a Source form, including but
      not limited to compiled object code, generated documentation,
      and conversions to other media types.
      "Work" shall mean the work of authorship, whether in Source or
      Object form, made available under the License, as indicated by a
      copyright notice that is included in or attached to the work
      (an example is provided in the Appendix below).
      "Derivative Works" shall mean any work, whether in Source or Object
      form, that is based on (or derived from) the Work and for which the
      editorial revisions, annotations, elaborations, or other modifications
      represent, as a whole, an original work of authorship. For the purposes
      of this License, Derivative Works shall not include works that remain
      separable from, or merely link (or bind by name) to the interfaces of,
      the Work and Derivative Works thereof.
      "Contribution" shall mean any work of authorship, including
      the original version of the Work and any modifications or additions
      to that Work or Derivative Works thereof, that is intentionally
      submitted to Licensor for inclusion in the Work by the copyright owner
      or by an individual or Legal Entity authorized to submit on behalf of
      the copyright owner. For the purposes of this definition, "submitted"
      means any form of electronic, verbal, or written communication sent
      to the Licensor or its representatives, including but not limited to
      communication on electronic mailing lists, source code control systems,
      and issue tracking systems that are managed by, or on behalf of, the
      Licensor for the purpose of discussing and improving the Work, but
      excluding communication that is conspicuously marked or otherwise
      designated in writing by the copyright owner as "Not a Contribution."
      "Contributor" shall mean Licensor and any individual or Legal Entity
      on behalf of whom a Contribution has been received by Licensor and
      subsequently incorporated within the Work.
   2. Grant of Copyright License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      copyright license to reproduce, prepare Derivative Works of,
      publicly display, publicly perform, sublicense, and distribute the
      Work and such Derivative Works in Source or Object form.
   3. Grant of Patent License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      (except as stated in this section) patent license to make, have made,
      use, offer to sell, sell, import, and otherwise transfer the Work,
      where such license applies only to those patent claims licensable
      by such Contributor that are necessarily infringed by their
      Contribution(s) alone or by combination of their Contribution(s)
      with the Work to which such Contribution(s) was submitted. If You
      institute patent litigation against any entity (including a
      cross-claim or counterclaim in a lawsuit) alleging that the Work
      or a Contribution incorporated within the Work constitutes direct
      or contributory patent infringement, then any patent licenses
      granted to You under this License for that Work shall terminate
      as of the date such litigation is filed.
   4. Redistribution. You may reproduce and distribute copies of the
      Work or Derivative Works thereof in any medium, with or without
      modifications, and in Source or Object form, provided that You
      meet the following conditions:
      (a) You must give any other recipients of the Work or
          Derivative Works a copy of this License; and
      (b) You must cause any modified files to carry prominent notices
          stating that You changed the files; and
      (c) You must retain, in the Source form of any Derivative Works
          that You distribute, all copyright, patent, trademark, and
          attribution notices from the Source form of the Work,
          excluding those notices that do not pertain to any part of
          the Derivative Works; and
      (d) If the Work includes a "NOTICE" text file as part of its
          distribution, then any Derivative Works that You distribute must
          include a readable copy of the attribution notices contained
          within such NOTICE file, excluding those notices that do not
          pertain to any part of the Derivative Works, in at least one
          of the following places: within a NOTICE text file distributed
          as part of the Derivative Works; within the Source form or
          documentation, if provided along with the Derivative Works; or,
          within a display generated by the Derivative Works, if and
          wherever such third-party notices normally appear. The contents
          of the NOTICE file are for informational purposes only and
          do not modify the License. You may add Your own attribution
          notices within Derivative Works that You distribute, alongside
          or as an addendum to the NOTICE text from the Work, provided
          that such additional attribution notices cannot be construed
          as modifying the License.
      You may add Your own copyright statement to Your modifications and
      may provide additional or different license terms and conditions
      for use, reproduction, or distribution of Your modifications, or
      for any such Derivative Works as a whole, provided Your use,
      reproduction, and distribution of the Work otherwise complies with
      the conditions stated in this License.
   5. Submission of Contributions. Unless You explicitly state otherwise,
      any Contribution intentionally submitted for inclusion in the Work
      by You to the Licensor shall be under the terms and conditions of
      this License, without any additional terms or conditions.
      Notwithstanding the above, nothing herein shall supersede or modify
      the terms of any separate license agreement you may have executed
      with Licensor regarding such Contributions.
   6. Trademarks. This License does not grant permission to use the trade
      names, trademarks, service marks, or product names of the Licensor,
      except as required for reasonable and customary use in describing the
      origin of the Work and reproducing the content of the NOTICE file.
   7. Disclaimer of Warranty. Unless required by applicable law or
      agreed to in writing, Licensor provides the Work (and each
      Contributor provides its Contributions) on an "AS IS" BASIS,
      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
      implied, including, without limitation, any warranties or conditions
      of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
      PARTICULAR PURPOSE. You are solely responsible for determining the
      appropriateness of using or redistributing the Work and assume any
      risks associated with Your exercise of permissions under this License.
   8. Limitation of Liability. In no event and under no legal theory,
      whether in tort (including negligence), contract, or otherwise,
      unless required by applicable law (such as deliberate and grossly
      negligent acts) or agreed to in writing, shall any Contributor be
      liable to You for damages, including any direct, indirect, special,
      incidental, or consequential damages of any character arising as a
      result of this License or out of the use or inability to use the
      Work (including but not limited to damages for loss of goodwill,
      work stoppage, computer failure or malfunction, or any and all
      other commercial damages or losses), even if such Contributor
      has been advised of the possibility of such damages.
   9. Accepting Warranty or Additional Liability. While redistributing
      the Work or Derivative Works thereof, You may choose to offer,
      and charge a fee for, acceptance of support, warranty, indemnity,
      or other liability obligations and/or rights consistent with this
      License. However, in accepting such obligations, You may act only
      on Your own behalf and on Your sole responsibility, not on behalf
      of any other Contributor, and only if You agree to indemnify,
      defend, and hold each Contributor harmless for any liability
      incurred by, or claims asserted against, such Contributor by reason
      of your accepting any such warranty or additional liability.
   END OF TERMS AND CONDITIONS

*)
open Base
open Stdio

(* TODO:
  * rename functions to _exn / remove exceptions
  * stop using min_value
  * re-evaluate use of Int32
  * rewrite / document calculations
  * properly use radix when printing strings
*)

exception Arithmetic_error of string
exception Early_return
exception Precision_overflow

module Int32 = struct
  include Int32

  let two = of_int_exn 2
  let three = of_int_exn 3
  let four = of_int_exn 4
  let minus_four = of_int_exn (-4)
  let six = of_int_exn 6
  let ten = of_int_exn 10
  let minus_ten = of_int_exn (-10)
  let sixteen = of_int_exn 16
  let minus_twenty = of_int_exn (-20)
  let thirty = of_int_exn 30
  let fifty = of_int_exn 50
  let sixty = of_int_exn 60
  let hundred = of_int_exn 100
  let minus_52 = of_int_exn (-52)
  let minus_1000 = of_int_exn (-1000)
  let minus_1080 = of_int_exn (-1080)
end

module Z = struct
  include Z
  include Z.Compare
end

let big0 = Z.of_int 0
let big1 = Z.of_int 1
let bigm1 = Z.of_int (-1)
let big2 = Z.of_int 2
let bigm2 = Z.of_int (-2)
let big3 = Z.of_int 3
let big4 = Z.of_int 4
let big8 = Z.of_int 8
let big750 = Z.of_int 750
let bigm750 = Z.of_int (-750)

type series_init =
  { current_term : Z.t
  ; current_sum : Z.t
  ; calc_precision : int32
  ; max_trunc_error : Z.t
  ; p : int32
  }

type sum_iteration =
  { current_term : Z.t
  ; current_sum : Z.t
  }

type sum_scope_value = string * sum_iteration Queue.t

let sum_scope_enabled = ref false
let sum_scope_values : sum_scope_value Queue.t = Queue.create ()

let string_of_scaled_int scaled_int digits =
  let scaled_string : string = Z.(scaled_int |> abs |> to_string) in
  let result =
    if Int32.(digits = zero)
    then scaled_string
    else (
      let len = scaled_string |> String.length |> Int32.of_int_exn in
      let len, scaled_string =
        if Int32.(len <= digits)
        then (
          let z = String.make (Int.of_int32_exn Int32.(digits + one - len)) '0' in
          Int32.(digits + one), z ^ scaled_string)
        else len, scaled_string
      in
      let splitting_pos = Int.of_int32_exn Int32.(len - digits) in
      let whole = scaled_string |> String.subo ~pos:0 ~len:splitting_pos in
      let fraction = scaled_string |> String.subo ~pos:splitting_pos in
      whole ^ "." ^ fraction)
  in
  if Z.(scaled_int < zero) then "-" ^ result else result
;;

let big_shift_left bi i32 = Z.shift_left bi (Int.of_int32_exn i32)

let shift_left_allow_neg bi i =
  let open Int in
  if i < 0 then Z.shift_right bi (neg i) else Z.shift_left bi i
;;

let numbits = Z.numbits

let shift k n =
  let n' = Int32.to_int_exn n in
  if Int32.(n = zero)
  then k
  else if Int32.(n < zero)
  then Z.shift_right k (-n')
  else big_shift_left k n
;;

type base =
  { mutable min_prec : int32
        (** The smallest precision value with which the above has been called? *)
  ; mutable max_appr : Z.t (** The scaled approximation corresponding to min_prec *)
  ; mutable appr_valid : bool (** min_prec and max_val are valid *)
  }

let base_to_string { min_prec; max_appr; appr_valid } =
  Printf.sprintf
    "{ min_prec = %s; max_appr = %s; appr_valid = %b }"
    (Int32.to_string min_prec)
    (Z.to_string max_appr)
    appr_valid
;;

let new_base () = { min_prec = Int32.zero; max_appr = big0; appr_valid = false }

type cr =
  (* | SlowCR of int * int *)
  | IntCR of Z.t
  | AssumedIntCR of t
  | AddCR of t * t
  | ShiftedCR of t * int32
  | NegCR of t
  | SelectCR of selector
  | MultCR of t * t
  | InvCR of t
  | PrescaledExpCR of t
  | PrescaledCosCR of t
  | PrescaledLnCR of t
  | PrescaledAsinCR of t
  | SqrtCR of t
  | GlPiCR

and t =
  { base : base
  ; cr : cr
  }

and selector =
  { selector : t
  ; mutable selector_sign : int32
  ; op1 : t
  ; op2 : t
  }

let rec debug_cr_to_string = function
  | IntCR i -> Printf.sprintf "IntCR %s" (Z.to_string i)
  | AssumedIntCR op -> Printf.sprintf "AssumedIntCR (%s)" (debug_to_string op)
  | AddCR (op1, op2) ->
    Printf.sprintf "AddCR (%s, %s)" (debug_to_string op1) (debug_to_string op2)
  | ShiftedCR (op, shift) ->
    Printf.sprintf "ShiftedCR (%s, %li)" (debug_to_string op) shift
  | NegCR op -> Printf.sprintf "NegCR (%s)" (debug_to_string op)
  | SelectCR { selector; selector_sign; op1; op2 } ->
    Printf.sprintf
      "SelectCR (%s, %li, %s, %s)"
      (debug_to_string selector)
      selector_sign
      (debug_to_string op1)
      (debug_to_string op2)
  | MultCR (op1, op2) ->
    Printf.sprintf "MultCR (%s, %s)" (debug_to_string op1) (debug_to_string op2)
  | InvCR op -> Printf.sprintf "InvCR (%s)" (debug_to_string op)
  | PrescaledExpCR op -> Printf.sprintf "PrescaledExpCR (%s)" (debug_to_string op)
  | PrescaledCosCR op -> Printf.sprintf "PrescaledCosCR (%s)" (debug_to_string op)
  | PrescaledLnCR op -> Printf.sprintf "PrescaledLnCR (%s)" (debug_to_string op)
  | PrescaledAsinCR op -> Printf.sprintf "PrescaledAsinCR (%s)" (debug_to_string op)
  | SqrtCR op -> Printf.sprintf "SqrtCR (%s)" (debug_to_string op)
  | GlPiCR -> "GlPiCR"

and debug_to_string { base; cr } =
  Printf.sprintf "{ base = %s; cr = %s }" (base_to_string base) (debug_cr_to_string cr)
;;

type operator =
  | App
  | Negate
  | Mul
  | Div
  | Shift
  | Add

(* An odd amalgamation of Haskell and OCaml rules *)
let prec : operator -> int = function
  | App -> 10
  | Negate -> 8
  | Mul | Div -> 7
  | Shift -> 6
  | Add -> 5
;;

let rec pp' prec ppf { cr; _ } = pp_cr prec ppf cr

and pp_cr ambient_prec ppf = function
  | IntCR i -> Fmt.string ppf (Z.to_string i)
  | AssumedIntCR op -> pp' ambient_prec ppf op
  | AddCR (op1, op2) ->
    let prec' = prec Add in
    if Int.(prec' <= ambient_prec)
    then Fmt.pf ppf "@[<hv>(%a + %a)@]" (pp' prec') op1 (pp' prec') op2
    else Fmt.pf ppf "%a + %a" (pp' prec') op1 (pp' prec') op2
  | ShiftedCR (op, shift) ->
    let prec' = prec Shift in
    if Int.(prec' <= ambient_prec)
    then Fmt.pf ppf "@[<hv>(%a << %li)@]" (pp' prec') op shift
    else Fmt.pf ppf "%a << %li" (pp' prec') op shift
  | NegCR op ->
    let prec' = prec Negate in
    if Int.(prec' <= ambient_prec)
    then Fmt.pf ppf "@[<hv>(-%a)@]" (pp' prec') op
    else Fmt.pf ppf "-%a" (pp' prec') op
  | SelectCR { selector; selector_sign; op1; op2 }
  (* TODO: this style is inconsistent with the others *) ->
    Fmt.pf
      ppf
      "select(%a, %li, %a, %a)"
      (pp' 0)
      selector
      selector_sign
      (pp' 0)
      op1
      (pp' 0)
      op2
  | MultCR (op1, op2) ->
    let prec' = prec Mul in
    if Int.(prec' <= ambient_prec)
    then Fmt.pf ppf "@[<hv>(%a * %a)@]" (pp' prec') op1 (pp' prec') op2
    else Fmt.pf ppf "%a * %a" (pp' prec') op1 (pp' prec') op2
  | InvCR op ->
    let prec' = prec Div in
    if Int.(prec' <= ambient_prec)
    then Fmt.pf ppf "@[<hv>(1 / %a)@]" (pp' prec') op
    else Fmt.pf ppf "1 / %a" (pp' prec') op
  | PrescaledExpCR op -> Fmt.pf ppf "exp %a" (pp' (prec App)) op
  | PrescaledCosCR op -> Fmt.pf ppf "cos %a" (pp' (prec App)) op
  | PrescaledLnCR op -> Fmt.pf ppf "ln %a" (pp' (prec App)) op
  | PrescaledAsinCR op -> Fmt.pf ppf "asin %a" (pp' (prec App)) op
  | SqrtCR op -> Fmt.pf ppf "sqrt %a" (pp' (prec App)) op
  | GlPiCR -> Fmt.string ppf "pi"
;;

let pp = pp' 0

let big_signum : Z.t -> int32 =
 fun i ->
  let ( = ), ( > ) = Z.(( = ), ( > )) in
  if i = big0 then Int32.zero else if i > big0 then Int32.one else Int32.minus_one
;;

(* Check that precision is at least a factor of 8 from overflowing the int32 used to
 * hold the precision spec. *)
let check_prec n =
  let open Int32 in
  let high = shift_right n 28 in
  let high_shifted = shift_right n 29 in
  if high lxor high_shifted <> zero then raise Precision_overflow
;;

(* constructors *)

let of_cr cr = { base = new_base (); cr }
let add x y = of_cr (AddCR (x, y))

let shift_left x n =
  check_prec n;
  of_cr (ShiftedCR (x, n))
;;

let shift_right x n =
  check_prec n;
  of_cr (ShiftedCR (x, Int32.neg n))
;;

let assume_int x = of_cr (AssumedIntCR x)
let negate x = of_cr (NegCR x)
let subtract x y = of_cr (AddCR (x, negate y))
let multiply x y = of_cr (MultCR (x, y))
let inverse x = of_cr (InvCR x)
let divide x y = of_cr (MultCR (x, inverse y))
let ( + ) = add
let ( - ) = subtract
let ( * ) = multiply
let ( / ) = divide
let pi = of_cr GlPiCR
let half_pi = shift_right pi Int32.one
let sqrt x = of_cr (SqrtCR x)

let bound_log2 : int32 -> int32 =
 fun n ->
  let x = Float.of_int (Int.of_int32_exn Int32.(abs n + one)) in
  Int32.of_float (Float.round_up (Float.log x /. Float.log 2.0))
;;

let of_bigint : Z.t -> t = fun x -> of_cr (IntCR x)
let of_int : int -> t = fun x -> x |> Z.of_int |> of_bigint
let of_int32 : int32 -> t = fun x -> x |> Z.of_int32 |> of_bigint
let of_int64 : int64 -> t = fun x -> x |> Z.of_int64 |> of_bigint
let one = of_int 1
let minus_one = of_int (-1)
let two = of_int 2
let three = of_int 3

let of_float n =
  (* TODO: throw for NaN / infinite *)
  let negative = Float.ieee_negative n in
  let pre_mantissa : int64 = Int63.to_int64 (Float.ieee_mantissa n) in
  let exp = Int.(Float.ieee_exponent n - 1023) |> Int32.of_int_exn in
  let p1 = shift_left one exp in
  let p2 = shift_left one Int32.minus_52 in
  let mantissa = add one (multiply (of_int64 pre_mantissa) p2) in
  let result = multiply p1 mantissa in
  if negative then negate result else result
;;

(* end constructors *)

(* Multiply by 2**n, rounding result *)
let scale k n =
  if Int32.(n >= zero)
  then big_shift_left k n
  else (
    (* TODO: Seems like an odd way to do this if I'm understanding *)
    let big_add = Z.( + ) in
    let adj_k = big_add (shift k Int32.(n + one)) big1 in
    Z.shift_right adj_k 1)
;;

let sum_series
    : name:string -> init:series_init -> f:(n:int -> current_term:Z.t -> Z.t) -> Z.t
  =
 fun ~name ~init ~f ->
  let n = ref 0 in
  let current_term = ref init.current_term in
  let current_sum = ref init.current_sum in
  let sum_scope_value =
    Queue.of_list [ { current_term = !current_term; current_sum = !current_sum } ]
  in
  let { calc_precision; max_trunc_error; _ } = init in
  while Z.(abs !current_term >= max_trunc_error) do
    current_term := f ~n:!n ~current_term:!current_term;
    (current_sum := Z.(!current_sum + !current_term));
    if !sum_scope_enabled
    then
      Queue.enqueue
        sum_scope_value
        { current_term = !current_term; current_sum = !current_sum };
    Int.incr n
  done;
  if !sum_scope_enabled then Queue.enqueue sum_scope_values (name, sum_scope_value);
  scale !current_sum Int32.(calc_precision - init.p)
;;

(* Keep the entire sequence b[n] that we've calculated so far.

   Holds the min_prec and max_appr for b_{n+1} = sqrt{a_n b_n}. *)
let pi_b_precalculated : (int32 * Z.t) Queue.t = Queue.of_list [ Int32.zero, big0 ]

(* Initial entry unused *)

let set_or_update_pi_b ~n ~v =
  if Int.(Queue.length pi_b_precalculated = n)
  then Queue.enqueue pi_b_precalculated v
  else Queue.set pi_b_precalculated n v
;;

let rec signum_a : t -> int32 -> int32 =
 fun op a ->
  (* TODO if appr_valid then let quick_try = signum max_appr in if quick_try <> 0 then
     quick_try *)
  let needed_prec = Int32.(a - one) in
  let this_appr = get_appr op needed_prec in
  big_signum this_appr

and signum : t -> int32 = fun op -> signum' op Int32.minus_twenty

and signum' : t -> int32 -> int32 =
 fun op a ->
  (* TODO check_prec a *)
  let result = signum_a op a in
  if Int32.(result <> zero) then result else signum' op Int32.(a * Int32.two)

(* Give a scaled approximation accurate to 2**n. In other words, [value / (2 ** p)],
   rounded to an integer. *)
and approximate : t -> int32 -> Z.t =
 fun ({ cr; _ } as t) p ->
  match cr with
  | IntCR value -> scale value (Int32.neg p)
  | AssumedIntCR value ->
    if Int32.(p >= zero)
    then get_appr value p
    else scale (get_appr value Int32.zero) (Int32.neg p)
  | AddCR (op1, op2) ->
    scale Z.(get_appr op1 Int32.(p - two) + get_appr op2 Int32.(p - two)) Int32.(neg two)
  | ShiftedCR (op, count) -> get_appr op Int32.(p - count)
  | NegCR op -> Z.neg (get_appr op p)
  | SelectCR selector ->
    if Int32.(selector.selector_sign < zero)
    then get_appr selector.op1 p
    else if Int32.(selector.selector_sign > zero)
    then get_appr selector.op2 p
    else (
      let op1_appr = get_appr selector.op1 Int32.(p - one) in
      let op2_appr = get_appr selector.op2 Int32.(p - one) in
      let diff = Z.(abs (op1_appr - op2_appr)) in
      if Z.(diff <= big1)
      then scale op1_appr Int32.minus_one
      else if Int32.(signum selector.selector < zero)
      then (
        selector.selector_sign <- Int32.minus_one;
        scale op1_appr Int32.minus_one)
      else (
        selector.selector_sign <- Int32.one;
        scale op2_appr Int32.minus_one))
  | MultCR (x, y) -> approximate_mult_cr x y p
  | InvCR op -> approximate_inv_cr op p
  | PrescaledExpCR op -> approximate_prescaled_exp_cr op p
  | PrescaledCosCR op -> approximate_prescaled_cos_cr op p
  | PrescaledLnCR op -> approximate_prescaled_ln_cr op p
  | PrescaledAsinCR op -> approximate_prescaled_asin_cr op p
  | SqrtCR op -> approximate_sqrt_cr t op p
  | GlPiCR -> approximate_pi_cr p

and approximate_mult_cr : t -> t -> int32 -> Z.t =
 fun op1 op2 p ->
  try
    let half_prec = Int32.(shift_right p 1 - one) in
    let msd_op1 = msd op1 half_prec in
    let op1, op2, msd_op1 =
      if Int32.(msd_op1 = min_value)
      then (
        let msd_op2 = msd op2 half_prec in
        (* Product is small enough that zero will do as an approximation *)
        if Int32.(msd_op2 = min_value) then raise Early_return;
        (* Swap operands so larger is first *)
        op2, op1, msd_op2)
      else op1, op2, msd_op1
    in
    let prec2 = Int32.(p - msd_op1 - Int32.three) in
    let appr2 = get_appr op2 prec2 in
    if Z.(appr2 = zero) then raise Early_return;
    let msd_op2 = known_msd op2 in
    let prec1 = Int32.(p - msd_op2 - Int32.three) in
    let appr1 = get_appr op1 prec1 in
    let scale_digits = Int32.(prec1 + prec2 - p) in
    scale Z.(appr1 * appr2) scale_digits
  with
  | Early_return -> big0

(* Newton-Raphson? *)
and approximate_inv_cr op p =
  let open Int32 in
  let msd = iter_msd op Int32.min_value in
  let inv_msd = Int32.one - msd in
  let digits_needed = inv_msd - p - Int32.three in
  let prec_needed = msd - digits_needed in
  let log_scale_factor = neg p - prec_needed in
  if log_scale_factor < Int32.zero
  then big0
  else
    let open Z in
    let dividend = big_shift_left big1 log_scale_factor in
    let scaled_divisor = get_appr op prec_needed in
    let abs_scaled_divisor = abs scaled_divisor in
    let adj_dividend = dividend + shift_right abs_scaled_divisor 1 in
    let result = adj_dividend / abs_scaled_divisor in
    if scaled_divisor < zero then neg result else result

and approximate_prescaled_asin_cr op p =
  let open Int32 in
  if p >= Int32.two
  then big0
  else (
    let iterations_needed = (neg three * p / two) + four in
    let calc_precision = p - bound_log2 (two * iterations_needed) - four in
    let op_prec = p - three in
    let op_appr = get_appr op op_prec in
    let exp = ref 1 in
    let start_term_val = op_prec - calc_precision |> big_shift_left op_appr in
    let current_term = ref start_term_val in
    let current_sum = ref start_term_val in
    let current_factor = ref start_term_val in
    let max_trunc_error = big_shift_left big1 (p - four - calc_precision) in
    while Z.(abs !current_term >= max_trunc_error) do
      (exp := Int.(!exp + 2));
      let open Z in
      current_factor := !current_factor * of_int Int.(!exp - 2);
      current_factor := scale (!current_factor * op_appr) Int32.(op_prec + Int32.two);
      current_factor := !current_factor * op_appr;
      let divisor = of_int Int.(!exp - 1) in
      current_factor := !current_factor / divisor;
      current_factor := scale !current_factor Int32.(op_prec - Int32.two);
      current_term := !current_factor / of_int !exp;
      current_sum := !current_sum + !current_term
    done;
    scale !current_sum Int32.(calc_precision - p))

(* Uses the Gauss-Legendre algorithm: a_0 = 1 b_0 = 1 / sqrt 2 t_0 = 1 / 4

   a_{n+1} = (a_n + b_n) / 2
   b_{n+1} = sqrt (a_n + b_n)
   t_{n+1} = t_n - (2^n)(a_n - a_{n+1})^2

   pi ~ (a_{n+1} + b_{n+1})^2 / 4t_{n+1} *)
and approximate_pi_cr p =
  let pi_tolerance = big4 in
  let sqrt_half = sqrt (shift_right one Int32.one) in
  if Int32.(p >= zero)
  then scale big3 (Int32.neg p) (* quick rough approximation *)
  else (
    let extra_eval_prec =
      Int32.(
        of_float (Float.round_up (Float.log (to_float (neg p)) /. Float.log 2.0)) + ten)
    in
    let eval_prec = Int32.(p - extra_eval_prec) in
    let a = ref (big_shift_left big1 (Int32.neg eval_prec)) in
    let b = ref (get_appr sqrt_half eval_prec) in
    let t = ref (big_shift_left big1 Int32.(neg eval_prec - two)) in
    let n = ref 0 in
    while Z.(!a - !b - pi_tolerance > big0) do
      let next_a = Z.(shift_right (!a + !b) 1) in
      let a_diff = Z.(!a - next_a) in
      let b_prod =
        Z.(shift_right (!a * !b) (eval_prec |> Int32.neg |> Int.of_int32_exn))
      in
      let b_prod_as_cr = shift_right (of_bigint b_prod) (Int32.neg eval_prec) in
      let next_b_as_cr =
        if Int.(Queue.length pi_b_precalculated = !n + 1)
        then sqrt b_prod_as_cr
        else (
          (* Reuse previous approximation *)
          let min_prec, max_appr = Queue.get pi_b_precalculated Int.(!n + 1) in
          { base = { min_prec; max_appr; appr_valid = true }; cr = SqrtCR b_prod_as_cr })
      in
      let next_b = get_appr next_b_as_cr eval_prec in
      set_or_update_pi_b ~n:Int.(!n + 1) ~v:(p, scale next_b (Int32.neg extra_eval_prec));
      let next_t =
        (* shift distance is usually negative *)
        Z.(!t - shift_left_allow_neg (a_diff * a_diff) Int.(!n + of_int32_exn eval_prec))
      in
      a := next_a;
      b := next_b;
      t := next_t;
      Int.incr n
    done;
    let sum = Z.(!a + !b) in
    let result = Z.(shift_right (sum * sum / !t) 2) in
    scale result (Int32.neg extra_eval_prec))

(* Use Newton's algorithm to compute. *)
and approximate_sqrt_cr t op p =
  let open Int32 in
  (* Convervative estimate of number of significant bits in double precision computation *)
  let fp_prec = fifty in
  let fp_op_prec = sixty in
  let max_op_prec_needed = (two * p) - one in
  let msd = iter_msd op max_op_prec_needed in
  if msd <= max_op_prec_needed
  then big0
  else (
    let result_msd = msd / two in
    let result_digits = result_msd - p in
    if result_digits > fp_prec
    then (
      let appr_digits = (result_digits / two) + six in
      let appr_prec = result_msd - appr_digits in
      let prod_prec = two * appr_prec in
      let op_appr = get_appr op prod_prec in
      let last_appr = get_appr t appr_prec in
      let open Z in
      let prod_prec_scaled_numerator = (last_appr * last_appr) + op_appr in
      let scaled_numerator = scale prod_prec_scaled_numerator Int32.(appr_prec - p) in
      let shifted_result = scaled_numerator / last_appr in
      shift_right (shifted_result + big1) 1)
    else (
      let op_prec = (msd - fp_op_prec) land lnot Int32.one in
      let working_prec = op_prec - fp_op_prec in
      let scaled_bi_appr = big_shift_left (get_appr op op_prec) fp_op_prec in
      let scaled_appr = Z.to_float scaled_bi_appr in
      if Float.(scaled_appr < 0.0) then raise (Arithmetic_error "sqrt(negative)");
      let scaled_fp_sqrt = Float.sqrt scaled_appr in
      let scaled_sqrt = Z.of_float scaled_fp_sqrt in
      let shift_count = (working_prec / two) - p in
      shift scaled_sqrt shift_count))

(* Calculate the Taylor series for [ln (1 + x)]. *)
and approximate_prescaled_ln_cr op p =
  let open Int32 in
  if p >= Int32.zero
  then big0
  else (
    let iterations_needed = neg p in
    let op_prec = p - three in
    let op_appr = get_appr op op_prec in
    let calc_precision = p - bound_log2 (two * iterations_needed) - four in
    let max_trunc_error = big_shift_left big1 (p - four - calc_precision) in
    (* x_nth holds the value of x^(n+1) throughout. Initially just x. *)
    let x_nth_val = scale op_appr (op_prec - calc_precision) in
    let x_nth = ref x_nth_val in
    let init =
      { current_term = x_nth_val
      ; current_sum = x_nth_val
      ; calc_precision
      ; max_trunc_error
      ; p
      }
    in
    sum_series ~name:"ln" ~init ~f:(fun ~n ~current_term:_ ->
        let current_sign = if Int.(n % 2 = 1) then 1 else -1 in
        x_nth := scale Z.(!x_nth * op_appr) op_prec;
        Z.(!x_nth / of_int Int.((n + 2) * current_sign))))

(* Use a Taylor series to calculate cos x.

 [cos x = sum{n=0}{\inf}{x^{2n} / (2n)!}] *)
and approximate_prescaled_cos_cr op p =
  let open Int32 in
  if p >= Int32.one
  then big0
  else (
    let iterations_needed = (neg p / two) + four in
    let op_prec = p - two in
    let op_appr = get_appr op op_prec in
    let calc_precision = p - bound_log2 (two * iterations_needed) - four in
    let max_trunc_error = big_shift_left big1 (p - four - calc_precision) in
    let scaled_1 = big_shift_left big1 (neg calc_precision) in
    let init =
      { current_term = scaled_1
      ; current_sum = scaled_1
      ; calc_precision
      ; max_trunc_error
      ; p
      }
    in
    sum_series ~name:"cos" ~init ~f:(fun ~n ~current_term ->
        let n' = Int.(n * 2) in
        let rescale x = scale x op_prec in
        (* Multiply numerator by x twice, and denominator by next two terms of factorial. *)
        let numerator = Z.(neg (rescale (rescale (current_term * op_appr) * op_appr))) in
        let denominator = Z.of_int Int.((n' + 2) * (n' + 1)) in
        Z.(numerator / denominator)))

(* Use a taylor series to calculate [e^x]. Assumes |x| < 1/2.

   [e^x = sum{n=0}{\inf}{x^n / n!} = 1 + x + x^2 / 2! + x^3 / 3! + ...] *)
and approximate_prescaled_exp_cr op p =
  let open Int32 in
  if p >= Int32.one
  then big0
  else (
    let iterations_needed = (neg p / two) + two in
    let op_prec = p - three in
    let op_appr = get_appr op op_prec in
    let calc_precision = p - bound_log2 (two * iterations_needed) - four in
    let max_trunc_error = big_shift_left big1 (p - four - calc_precision) in
    let scaled_1 = big_shift_left big1 (neg calc_precision) in
    let init =
      { current_term = scaled_1
      ; current_sum = scaled_1
      ; calc_precision
      ; max_trunc_error
      ; p
      }
    in
    sum_series ~name:"exp" ~init ~f:(fun ~n ~current_term ->
        Z.(scale (current_term * op_appr) op_prec / of_int Int.(n + 1))))

(* Return [value / 2 ** prec] rounded to an integer. *)
and get_appr : t -> int32 -> Z.t =
 fun ({ base = { min_prec; max_appr; appr_valid }; _ } as op) precision ->
  check_prec precision;
  if appr_valid && Int32.(precision >= min_prec)
  then scale max_appr Int32.(min_prec - precision)
  else (
    let result = approximate op precision in
    op.base.min_prec <- precision;
    op.base.max_appr <- result;
    op.base.appr_valid <- true;
    result)

(* Position of the most significant digit. If [msd x = n] then [pow 2 (n - 1) < abs x <
   pow 2 (n + 1)] This version assumes that max_appr is valid and sufficiently removed
   from zero that the msd is determined. *)
and known_msd : t -> int32 =
 fun op ->
  let length =
    if Z.(op.base.max_appr >= zero)
    then op.base.max_appr |> numbits
    else op.base.max_appr |> Z.neg |> numbits
  in
  Int32.(op.base.min_prec + of_int_exn length - one)

(* Most significant digit. Returns [Int32.min_value] if the correct answer [< n].

 TODO: return option? *)
and msd : t -> int32 -> int32 =
 fun op n ->
  let { max_appr; appr_valid; _ } = op.base in
  if (not appr_valid)
     || (* in range [-1, 1] *)
     Z.(max_appr >= bigm1 && max_appr <= big1)
  then (
    let (_ : Z.t) = get_appr op Int32.(n - one) in
    let max_appr = op.base.max_appr in
    (* get new value after get_appr *)
    if Z.(abs max_appr <= big1)
    then Int32.min_value (* msd arbitrarily far to the right *)
    else known_msd op)
  else known_msd op

and iter_msd op n = iter_prec_msd op n Int32.zero

and iter_prec_msd op n prec =
  if Int32.(prec <= n + thirty)
  then msd op n
  else (
    let msd = msd op prec in
    if Int32.(msd <> min_value)
    then msd
    else (
      check_prec prec;
      iter_prec_msd op n Int32.((prec * three / two) - sixteen)))
;;

let compare_absolute x y ~absolute_tolerance =
  let needed_prec = Int32.(absolute_tolerance - one) in
  let x_appr = get_appr x needed_prec in
  let y_appr = get_appr y needed_prec in
  let open Z in
  if x_appr > y_appr + one then 1 else if x_appr < y_appr - one then -1 else 0
;;

let equal_within x y ~absolute_tolerance =
  Int.(compare_absolute x y ~absolute_tolerance = 0)
;;

let compare_known_unequal x y =
  let rec go x y ~absolute_tolerance =
    check_prec absolute_tolerance;
    let result = compare_absolute x y ~absolute_tolerance in
    if Int.(result <> 0) then result else go x y ~absolute_tolerance
  in
  go ~absolute_tolerance:Int32.minus_twenty x y
;;

let compare x y ~relative_tolerance ~absolute_tolerance =
  let x_msd = iter_msd x absolute_tolerance in
  let y_msd =
    iter_msd y (if Int32.(x_msd > absolute_tolerance) then x_msd else absolute_tolerance)
  in
  let max_msd = if Int32.(y_msd > x_msd) then y_msd else x_msd in
  if Int32.(max_msd = min_value)
  then 0
  else (
    check_prec relative_tolerance;
    let rel = Int32.(max_msd + relative_tolerance) in
    let abs_prec = if Int32.(rel > absolute_tolerance) then rel else absolute_tolerance in
    compare_absolute x y ~absolute_tolerance:abs_prec)
;;

let bigint_value op = get_appr op Int32.zero
let int_value op = op |> bigint_value |> Z.to_int

let float_value op =
  let op_msd = iter_msd op Int32.minus_1080 in
  if Int32.(op_msd = min_value)
  then 0.0
  else (
    let needed_prec = Int32.(op_msd - sixty) in
    let needed_prec' = Int32.to_int_exn needed_prec in
    let scaled_int = get_appr op needed_prec |> Z.to_float in
    let may_underflow = Int32.(needed_prec < minus_1000) in
    let negative = Float.ieee_negative scaled_int in
    let mantissa = Float.ieee_mantissa scaled_int in
    let orig_exp = Float.ieee_exponent scaled_int in
    let exp_adj = if may_underflow then Int.(needed_prec' + 96) else needed_prec' in
    if Int.(orig_exp + exp_adj >= 0x7ff)
    then if Float.(scaled_int < 0.0) then Float.neg_infinity else Float.infinity
    else (
      let exponent = Int.(orig_exp + exp_adj) in
      let result = Float.create_ieee_exn ~negative ~exponent ~mantissa in
      if may_underflow
      then (
        let two48 : float = Float.of_int Int.(shift_left 1 48) in
        result /. two48 /. two48)
      else result))
;;

(* more constructors (that use get_appr) *)
let select selector x y =
  of_cr
    (SelectCR
       { selector
       ; selector_sign = Int32.minus_twenty |> get_appr selector |> big_signum
       ; op1 = x
       ; op2 = y
       })
;;

let max x y = select (subtract x y) y x
let min x y = select (subtract x y) x y
let abs x = select x (negate x) x

let rec exp op =
  let low_prec = Int32.minus_ten in
  let rough_appr = get_appr op low_prec in
  if Z.(rough_appr > big2) || Z.(rough_appr < bigm2)
  then (
    let square_root = exp (shift_right op Int32.one) in
    multiply square_root square_root)
  else of_cr (PrescaledExpCR op)
;;

let e = exp one

let rec cos : t -> t =
 fun op ->
  let halfpi_multiples = get_appr (divide op pi) Int32.minus_one in
  let abs_halfpi_multiples = Z.abs halfpi_multiples in
  if Z.(abs_halfpi_multiples >= big2)
  then (
    let pi_multiples = scale halfpi_multiples Int32.minus_one in
    let adjustment = multiply pi (of_bigint pi_multiples) in
    if Z.(pi_multiples land big1 <> big0)
    then (* Odd number of pi multiples *)
      subtract op adjustment |> cos |> negate
    else (* Even number of pi multiples *)
      subtract op adjustment |> cos)
  else if Z.(abs (get_appr op Int32.minus_one) >= big2)
  then (
    (* cos(2x) = 2 * cos(x)^2 - 1 *)
    let cos_half = cos (shift_right op Int32.one) in
    subtract (shift_left (multiply cos_half cos_half) Int32.one) one)
  else of_cr (PrescaledCosCR op)
;;

let sin x = cos (subtract half_pi x)

let rec asin : t -> t =
 fun op ->
  let rough_appr = get_appr op Int32.minus_ten in
  if Z.(rough_appr > big750)
  then (* asin(x) = acos(sqrt(1 - x^2)) *)
    subtract one (multiply op op) |> sqrt |> acos
  else if Z.(rough_appr < bigm750)
  then (* asin(-x) = -asin(x) <=> asin(x) = -asin(-x) *)
    op |> negate |> asin |> negate
  else of_cr (PrescaledAsinCR op)

and acos x = subtract half_pi (asin x)

(* asin (subtract half_pi x) *)

let tan x = sin x / cos x

let atan n =
  let n2 = n * n in
  let abs_sin_atan = sqrt (n2 / (one + n2)) in
  let sin_atan = select n (negate abs_sin_atan) abs_sin_atan in
  asin sin_atan
;;

let low_ln_limit = big8
let high_ln_limit = Z.of_int Int.(16 + 8)
let scaled_4 = Z.of_int Int.(4 * 16)
let simple_ln op = of_cr (PrescaledLnCR (subtract op one))
let ten_ninths = divide (of_int 10) (of_int 9)
let twentyfive_twentyfourths = divide (of_int 25) (of_int 24)
let eightyone_eightyeths = divide (of_int 81) (of_int 80)
let ln2_1 = multiply (of_int 7) (simple_ln ten_ninths)
let ln2_2 = multiply two (simple_ln twentyfive_twentyfourths)
let ln2_3 = multiply three (simple_ln eightyone_eightyeths)
let ln2 = add (subtract ln2_1 ln2_2) ln2_3

let rec ln : t -> t =
 fun op ->
  let low_prec = Int32.minus_four in
  let rough_appr = get_appr op low_prec in
  if Z.(rough_appr < big0) then raise (Arithmetic_error "ln(negative)");
  if Z.(rough_appr <= low_ln_limit)
  then op |> inverse |> ln |> negate
  else if Z.(rough_appr >= high_ln_limit)
  then
    if Z.(rough_appr <= scaled_4)
    then (
      let quarter = op |> sqrt |> sqrt |> ln in
      shift_left quarter Int32.two)
    else (
      let extra_bits = Int32.(of_int_exn (numbits rough_appr) - three) in
      let scaled_result = shift_right op extra_bits |> ln in
      add scaled_result (of_int32 extra_bits |> multiply ln2))
  else simple_ln op
;;

(* end more constructors *)

let eval_to_string : ?digits:int32 -> ?radix:int32 -> t -> string =
 fun ?(digits = Int32.ten) ?(radix = Int32.ten) op ->
  let scaled_cr =
    if Int32.(radix = sixteen)
    then shift_left op Int32.(four * digits)
    else (
      let scale_factor = Z.(pow (of_int32 radix) (Int.of_int32_exn digits)) in
      multiply op (of_cr (IntCR scale_factor)))
  in
  let scaled_int = get_appr scaled_cr Int32.zero in
  if !sum_scope_enabled then printf "scaled int: %s\n" (Z.to_string scaled_int);
  (* TODO: radix *)
  string_of_scaled_int scaled_int digits
;;

(* to_string_float_rep *)

let bigint_value : t -> Z.t = fun op -> get_appr op Int32.zero

let%test_module "Calculator" =
  (module struct
    exception AssertionFailure of string

    let zero = of_int 0
    let one = of_int 1
    let minus_one = of_int (-1)
    let two = of_int 2
    let three = add two one
    let four = add two two
    let i38923 = of_int 38923
    let half = divide one two
    let thirteen = of_int 13
    let sqrt13 = sqrt thirteen
    let thousand = Z.of_int 1000
    let million = Z.of_int 1000000
    let print ?(digits = Int32.ten) cr = printf "%s\n" (eval_to_string cr ~digits)

    let%expect_test _ =
      print zero;
      [%expect {| 0.0000000000 |}]
    ;;

    let%expect_test _ =
      print one;
      [%expect {| 1.0000000000 |}]
    ;;

    let%expect_test _ =
      print three;
      [%expect {| 3.0000000000 |}]
    ;;

    let%expect_test _ =
      print four;
      [%expect {| 4.0000000000 |}]
    ;;

    let%expect_test _ =
      print (zero * one);
      [%expect {| 0.0000000000 |}]
    ;;

    let%expect_test _ =
      print i38923;
      [%expect {| 38923.0000000000 |}]
    ;;

    let%expect_test _ =
      print (inverse two);
      [%expect {| 0.5000000000 |}]
    ;;

    let%expect_test _ =
      print (inverse three);
      [%expect {| 0.3333333333 |}]
    ;;

    let%expect_test _ =
      print (shift_left one Int32.one);
      print (shift_left one Int32.minus_one);
      print (shift_right one Int32.one);
      print (shift_right one Int32.minus_one);
      [%expect
        {|
      2.0000000000
      0.5000000000
      0.5000000000
      2.0000000000 |}]
    ;;

    let%expect_test _ =
      print (shift_right two Int32.one);
      [%expect {| 1.0000000000 |}]
    ;;

    let%expect_test _ =
      print (one + one);
      [%expect {| 2.0000000000 |}]
    ;;

    let%expect_test _ =
      print pi ~digits:Int32.hundred;
      [%expect
        {| 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170680 |}]
    ;;

    let%expect_test _ =
      print (sqrt one);
      [%expect {| 1.0000000000 |}]
    ;;

    let%expect_test _ =
      print (sqrt two) ~digits:Int32.hundred;
      [%expect
        {| 1.4142135623730950488016887242096980785696718753769480731766797379907324784621070388503875343276415727 |}]
    ;;

    let%expect_test _ =
      print (sqrt four);
      [%expect {| 2.0000000000 |}]
    ;;

    let%expect_test _ =
      print half_pi;
      [%expect {| 1.5707963268 |}]
    ;;

    let%expect_test _ =
      print (asin one);
      [%expect {| 1.5707963268 |}]
    ;;

    let%expect_test _ =
      print (asin (negate one));
      [%expect {| -1.5707963268 |}]
    ;;

    let%expect_test _ =
      print (asin zero);
      [%expect {| 0.0000000000 |}]
    ;;

    let%expect_test _ =
      print (sin zero);
      [%expect {| 0.0000000000 |}]
    ;;

    let%expect_test _ =
      print (sin half_pi);
      print (sin pi);
      print (sin half);
      [%expect {|
      1.0000000000
      0.0000000000
      0.4794255386
    |}]
    ;;

    let%expect_test _ =
      print (of_float 0.684413);
      print (of_float 0.684414);
      [%expect {|
      0.6844130000
      0.6844140000 |}]
    ;;

    let%expect_test _ =
      print (sin one);
      print (sin two);
      print (sin three);
      [%expect {|
      0.8414709848
      0.9092974268
      0.1411200081 |}]
    ;;

    let%expect_test _ =
      print (asin (sin half));
      [%expect {| 0.5000000000 |}]
    ;;

    let%expect_test _ =
      print (cos one);
      print (cos three);
      [%expect {|
      0.5403023059
      -0.9899924966 |}]
    ;;

    let%expect_test _ =
      print (asin (sin one));
      [%expect {| 1.0000000000 |}]
    ;;

    let%expect_test _ =
      print (acos (cos one));
      [%expect {| 1.0000000000 |}]
    ;;

    let%expect_test _ =
      print (atan (tan one));
      [%expect {| 1.0000000000 |}]
    ;;

    let%expect_test _ =
      print (atan (tan (negate one)));
      [%expect {| -1.0000000000 |}]
    ;;

    let%expect_test _ =
      print (multiply sqrt13 sqrt13);
      [%expect {| 13.0000000000 |}]
    ;;

    let%expect_test _ =
      let tmp = exp (add pi (of_int (-123))) in
      print (subtract (ln tmp) pi);
      let tmp = exp (of_int (-3)) in
      print (ln tmp);
      [%expect {|
      -123.0000000000
      -3.0000000000 |}]
    ;;

    let check_appr_eq x y =
      if Float.(x -. y > 0.000001)
      then raise (AssertionFailure (Printf.sprintf "%f vs %f" x y))
    ;;

    let%test_unit _ =
      List.init 10 ~f:(fun i -> Int.((i * 2) - 10))
      |> List.map ~f:Float.of_int
      |> List.iter ~f:(fun n ->
             check_appr_eq (Float.sin n) (of_float n |> sin |> float_value);
             check_appr_eq (Float.cos n) (of_float n |> cos |> float_value);
             check_appr_eq (Float.exp n) (of_float n |> exp |> float_value);
             check_appr_eq
               (Float.asin (0.1 *. n))
               (of_float (0.1 *. n) |> asin |> float_value);
             check_appr_eq
               (Float.acos (0.1 *. n))
               (of_float (0.1 *. n) |> acos |> float_value);
             if Float.(n > 0.0)
             then check_appr_eq (Float.log n) (of_float n |> ln |> float_value))
    ;;

    let check_eq x y = equal_within x y ~absolute_tolerance:(Int32.of_int_exn (-50))

    let%test _ =
      let huge = of_bigint Z.(million * million * thousand) in
      check_eq (tan (atan huge)) huge
    ;;

    let%test _ =
      List.for_all
        ~f:Fn.id
        [ check_eq (shift_left one Int32.one) two
        ; check_eq (shift_right two Int32.one) one
        ; check_eq (one + one) two
        ; check_eq (max two one) two
        ; check_eq (min two one) one
        ; check_eq (abs one) one
        ; check_eq (abs (negate one)) one
        ; check_eq (of_int 4) four
        ; check_eq (negate one + two) one
        ; check_eq (two * two) four
        ; check_eq (shift_left (one / four) Int32.four) four
        ; check_eq (two / negate one) (negate two)
        ; check_eq (one / thirteen * thirteen) one
        ; check_eq (exp zero) one
        ; check_eq (ln e) one
        ; check_eq (sin half_pi) one
        ; check_eq (asin one) half_pi
        ; check_eq (asin (negate one)) (negate half_pi)
        ; check_eq (asin zero) zero
        ; check_eq (asin (sin half)) half
        ; check_eq (asin (sin one)) one
        ; check_eq (acos (cos one)) one
        ; check_eq (atan (tan one)) one
        ; check_eq (atan (tan minus_one)) minus_one
        ; check_eq (sqrt13 * sqrt13) thirteen
        ]
    ;;

  let print_approximation t i =
    let v = approximate t (Int32.of_int_exn i) in
    printf "%s %s\n" (Z.to_string v) (Z.format "%b" v)

  let%expect_test _ =
    print_approximation (of_int 5) (-10);
    print_approximation (of_int 5) 0;
    print_approximation (of_int 5) 1;
    print_approximation (of_int 5) 2;
    print_approximation (of_int 5) 3;
    print_approximation (of_int 5) 4;
    [%expect {|
      5120 1010000000000
      5 101
      3 11
      1 1
      1 1
      0 0
      |}]

      (* Can be used for debugging *)
  let _print_sum_scope f =
    sum_scope_enabled := true;
    print (f ());
    sum_scope_values
      |> Queue.to_list
      |> List.iter ~f:(fun (name, iterations) ->
        printf "%s:\n" name;
        iterations
          |> Queue.to_list
          |> List.iter ~f:(fun { current_term; current_sum } ->
          printf "current_sum: %s, current_term: %s\n"
            (Z.to_string current_sum) (Z.to_string current_term)
        )
         );
    sum_scope_enabled := false

         (*
  let%expect_test _ =
    print_sum_scope (fun () -> ln (of_float 0.5));
    print_sum_scope (fun () -> cos (of_float 0.5));
    [%expect]
         *)
  end)
;;
