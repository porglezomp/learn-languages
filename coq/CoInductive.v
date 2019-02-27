Add LoadPath ".".
Require Import Nat Arith Cpdt.CpdtTactics.

Set Implicit Arguments.
Set Asymmetric Patterns.

Definition var := nat.
Definition vars := var -> nat.
Definition set (vs : vars) (v1 : var) (n : nat) : vars :=
  fun v2 => if beq_nat v1 v2 then n else vs n.

Inductive exp : Set :=
| Const : nat -> exp
| Var : var -> exp
| Plus : exp -> exp -> exp
| Times : exp -> exp -> exp.

Fixpoint evalExp (vs : vars) (e : exp) : nat :=
  match e with
  | Const n => n
  | Var v => vs v
  | Plus e1 e2 => evalExp vs e1 + evalExp vs e2
  | Times e1 e2 => evalExp vs e1 * evalExp vs e2
  end.

Inductive cmd : Set :=
| Assign : var -> exp -> cmd
| Seq : cmd -> cmd -> cmd
| While : exp -> cmd -> cmd
| If : exp -> cmd -> cmd -> cmd.

CoInductive evalCmd : vars -> cmd -> vars -> Prop :=
| EvalAssign : forall vs v e,
    evalCmd vs (Assign v e) (set vs v (evalExp vs e))
| EvalSeq : forall vs1 vs2 vs3 c1 c2,
    evalCmd vs1 c1 vs2 ->
    evalCmd vs2 c2 vs3 ->
    evalCmd vs1 (Seq c1 c2) vs3
| EvalWhileFalse : forall vs e c,
    evalExp vs e = 0 ->
    evalCmd vs (While e c) vs
| EvalWhileTrue : forall vs1 vs2 vs3 e c,
    evalExp vs1 e <> 0 ->
    evalCmd vs1 c vs2 ->
    evalCmd vs2 (While e c) vs3 ->
    evalCmd vs1 (While e c) vs3
| EvalIfFalse : forall vs1 vs2 e c1 c2,
    evalExp vs1 e = 0 ->
    evalCmd vs1 c2 vs2 ->
    evalCmd vs1 (If e c1 c2) vs2
| EvalIfTrue : forall vs1 vs2 e c1 c2,
    evalExp vs1 e <> 0 ->
    evalCmd vs1 c1 vs2 ->
    evalCmd vs1 (If e c1 c2) vs2.

Section evalCmd_coind.
  Variable R : vars -> cmd -> vars -> Prop.

  Hypothesis AssignCase : forall vs1 vs2 v e,
      R vs1 (Assign v e) vs2 ->
      vs2 = set vs1 v (evalExp vs1 e).

  Hypothesis SeqCase : forall vs1 vs3 c1 c2,
      R vs1 (Seq c1 c2) vs3 ->
      exists vs2, R vs1 c1 vs2 /\ R vs2 c2 vs3.

  Hypothesis WhileCase : forall vs1 vs3 e c,
      R vs1 (While e c) vs3 ->
      (evalExp vs1 e = 0 /\ vs1 = vs3) \/
      exists vs2, evalExp vs1 e <> 0 /\ R vs1 c vs2 /\ R vs2 (While e c) vs3.

  Hypothesis IfCase : forall vs1 vs2 e c1 c2,
      R vs1 (If e c1 c2) vs2 ->
      (evalExp vs1 e = 0 /\ R vs1 c2 vs2) \/
      (evalExp vs1 e <> 0 /\ R vs1 c1 vs2).

  Theorem evalCmd_coind : forall vs1 c vs2, R vs1 c vs2 -> evalCmd vs1 c vs2.
    cofix; intros ? ? ? H; destruct c.
    - rewrite (AssignCase H); constructor.
    - destruct (SeqCase H) as [? [? ?]]; econstructor; eauto.
    - destruct (WhileCase H) as [[? ?] | [? [? [? ?]]]]; subst; econstructor; eauto.
    - destruct (IfCase H) as [[? ?] | [? ?]].
      + apply EvalIfFalse; auto.
      + apply EvalIfTrue; auto.
  Qed.
End evalCmd_coind.

Fixpoint optExp (e : exp) : exp :=
  match e with
  | Plus (Const 0) e => optExp e
  | Plus e (Const 0) => optExp e
  | Plus e1 e2 => Plus (optExp e1) (optExp e2)
  | Times (Const 0) _ => Const 0
  | Times _ (Const 0) => Const 0
  (* | Times (Const 1) e => optExp e *)
  (* | Times e (Const 1) => optExp e *)
  | Times e1 e2 => Times (optExp e1) (optExp e2)
  | e => e
  end.

Fixpoint optCmd (c : cmd) : cmd :=
  match c with
  | Assign v e => Assign v (optExp e)
  | Seq c1 c2 => Seq (optCmd c1) (optCmd c2)
  | While e c => While (optExp e) (optCmd c)
  | If e c1 c2 => If (optExp e) (optCmd c1) (optCmd c2)
  end.

Lemma optExp_plus_dist : forall vs e1 e2,
    evalExp vs (optExp (Plus e1 e2)) = evalExp vs (Plus (optExp e1) (optExp e2)).
  induction e1; induction e2; auto; induction n; simpl; auto; induction n0; simpl; auto.
Qed.

Lemma optExp_plus : forall vs e1 e2,
    evalExp vs (optExp (Plus e1 e2)) = evalExp vs (optExp e1) + evalExp vs (optExp e2).
  induction e1; induction e2; auto; induction n; simpl; auto; induction n0; simpl; auto.
Qed.

(*
Lemma optExp_times_const : forall vs e n,
    evalExp vs (optExp (Times e (Const n))) = evalExp vs (optExp e) * n.
  intros; induction e; simpl; crush.
Admitted.

Lemma optExp_const_times : forall vs e n,
    evalExp vs (optExp (Times (Const n) e)) = n * evalExp vs (optExp e).
Admitted.
*)

Lemma optExp_times : forall vs e1 e2,
    evalExp vs (optExp (Times e1 e2)) = evalExp vs (optExp e1) * evalExp vs (optExp e2).
  (* Hint Rewrite optExp_times_const optExp_const_times. *)
  induction e1; induction e2; auto; induction n; auto; induction n0; auto.
    (* rewrite ? optExp_times_const, ? optExp_const_times; auto. *)
Qed.

Lemma optExp_correct : forall vs e, evalExp vs (optExp e) = evalExp vs e.
  induction e; auto; (rewrite optExp_plus || rewrite optExp_times); rewrite IHe1, IHe2; auto.
  (*
  induction e; crush;
    repeat (match goal with
            | [ |- context[match ?E with Const _ => _ | Plus _ _ => _ | _ => _ end] ] =>
              destruct E
            | [ |- context[match ?E with O => _ | S _ => _ end] ] =>
              destruct E
            end; crush).
   *)
Qed.

Hint Rewrite optExp_correct.

Ltac finisher :=
  match goal with
  | [ H : evalCmd _ _ _ |- _ ] => ((inversion H; []) || (inversion H; [|])); subst
  end; crush; eauto 10.

Lemma optCmd_correct1 : forall vs1 c vs2, evalCmd vs1 c vs2 -> evalCmd vs1 (optCmd c) vs2.
  intros;
    apply (evalCmd_coind (fun vs1 c vs2 => exists c', evalCmd vs1 c' vs2 /\ c = optCmd c'));
    eauto; crush;
      match goal with
      | [ H : _ = optCmd ?E |- _ ] =>
        destruct E; simpl in *; discriminate || injection H; intros; subst
      end; finisher.
Qed.

Lemma optCmd_correct2 : forall vs1 c vs2, evalCmd vs1 (optCmd c) vs2 -> evalCmd vs1 c vs2.
  intros; apply (evalCmd_coind (fun vs1 c vs2 => evalCmd vs1 (optCmd c) vs2));
    crush; finisher.
Qed.

Theorem optCmd_correct : forall vs1 c vs2, evalCmd vs1 (optCmd c) vs2 <-> evalCmd vs1 c vs2.
  intuition; apply optCmd_correct1 || apply optCmd_correct2; assumption.
Qed.