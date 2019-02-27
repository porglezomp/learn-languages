(* Copyright (c) 2008, 2011, 2015, Adam Chlipala
 * 
 * This work is licensed under a
 * Three-clause BSD Licence
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * - The names of contributors may not be used to endorse or promote products
 *   derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *)

(* Types and notations presented in Chapter 6 *)

Set Implicit Arguments.
Set Asymmetric Patterns.


Notation "!" := (False_rec _ _) : specif_scope.
Notation "[ e ]" := (exist _ e _) : specif_scope.
Notation "x <== e1 ; e2" := (let (x, _) := e1 in e2)
(right associativity, at level 60) : specif_scope.

Local Open Scope specif_scope.
Delimit Scope specif_scope with specif.

Notation "'Yes'" := (left _ _) : specif_scope.
Notation "'No'" := (right _ _) : specif_scope.
Notation "'Reduce' x" := (if x then Yes else No) (at level 50) : specif_scope.

Notation "x || y" := (if x then Yes else Reduce y) (at level 50, left associativity) : specif_scope.

Section sumbool_and.
  Variables P1 Q1 P2 Q2 : Prop.

  Variable x1 : {P1} + {Q1}.
  Variable x2 : {P2} + {Q2}.

  Definition sumbool_and : {P1 /\ P2} + {Q1 \/ Q2} :=
    match x1 with
      | left HP1 =>
        match x2 with
          | left HP2 => left _ (conj HP1 HP2)
          | right HQ2 => right _ (or_intror _ HQ2)
        end
      | right HQ1 => right _ (or_introl _ HQ1)
    end.
End sumbool_and.

Infix "&&" := sumbool_and (at level 40, left associativity) : specif_scope.

Inductive maybe (A : Set) (P : A -> Prop) : Set :=
| Unknown : maybe P
| Found : forall x : A, P x -> maybe P.

Notation "{{ x | P }}" := (maybe (fun x => P)) : specif_scope.
Notation "??" := (Unknown _) : specif_scope.
Notation "[| x |]" := (Found _ x _) : specif_scope.

Notation "x <- e1 ; e2" := (match e1 with
                             | Unknown => ??
                             | Found x _ => e2
                           end)
(right associativity, at level 60) : specif_scope.

Notation "!!" := (inright _ _) : specif_scope.
Notation "[|| x ||]" := (inleft _ [x]) : specif_scope.

Notation "x <-- e1 ; e2" := (match e1 with
                               | inright _ => !!
                               | inleft (exist x _) => e2
                             end)
(right associativity, at level 60) : specif_scope.

Notation "e1 ;; e2" := (if e1 then e2 else ??)
  (right associativity, at level 60) : specif_scope.

Notation "e1 ;;; e2" := (if e1 then e2 else !!)
  (right associativity, at level 60) : specif_scope.


Section partial.
  Variable P : Prop.

  Inductive partial : Set :=
  | Proved : P -> partial
  | Uncertain : partial.
End partial.

Notation "[ P ]" := (partial P) : type_scope.

Notation "'Yes'" := (Proved _) : partial_scope.
Notation "'No'" := (Uncertain _) : partial_scope.

Local Open Scope partial_scope.
Delimit Scope partial_scope with partial.

Notation "'Reduce' v" := (if v then Yes else No) : partial_scope.
Notation "x || y" := (if x then Yes else Reduce y) : partial_scope.
Notation "x && y" := (if x then Reduce y else No) : partial_scope.
