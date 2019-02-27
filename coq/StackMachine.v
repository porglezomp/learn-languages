Add LoadPath ".".
Require Import Bool Arith List Cpdt.CpdtTactics.
Set Implicit Arguments.
Set Asymmetric Patterns.

Definition option_bind (a b : Type) (x : option a) (f : a -> option b) : option b :=
  match x with
  | Some x => f x
  | None => None
  end.

Notation "x >>= f" := (option_bind x f) (at level 90).

Section stack_machine.
  Inductive binop : Set := Plus | Times.
  Inductive exp : Set :=
  | Const : nat -> exp
  | Binop : binop -> exp -> exp -> exp.

  Definition binopDenote (b : binop) : nat -> nat -> nat :=
    match b with
    | Plus => plus
    | Times => mult
    end.

  Fixpoint expDenote (e : exp) : nat :=
    match e with
    | Const n => n
    | Binop op e1 e2 => binopDenote op (expDenote e1) (expDenote e2)
    end.

  Eval simpl in expDenote (Const 42).
  Eval simpl in expDenote (Binop Times (Const 6) (Const 7)).
  Eval simpl in expDenote (Binop Times (Binop Plus (Const 4) (Const 2)) (Const 7)).

  Inductive instr : Set :=
  | iConst : nat -> instr
  | iBinop : binop -> instr.

  Definition prog := list instr.
  Definition stack := list nat.

  Definition instrDenote (i : instr) (s : stack) : option stack :=
    match i with
    | iConst n => Some (n :: s)
    | iBinop op =>
      match s with
      | x :: y :: s => Some (binopDenote op x y :: s)
      | _ => None
      end
    end.

  Fixpoint progDenote (p : prog) (s : stack) : option stack :=
    match p with
    | nil => Some s
    | x :: xs => instrDenote x s >>= progDenote xs
    end.

  Fixpoint compile (e : exp) : prog :=
    match e with
    | Const n => iConst n :: nil
    | Binop op e1 e2 => compile e2 ++ compile e1 ++ iBinop op :: nil
    end.

  Eval simpl in compile (Binop Times (Binop Plus (Const 4) (Const 2)) (Const 7)).
  Eval simpl in progDenote (compile (Const 42)) nil.
  Eval simpl in
      let exp := Binop Times (Binop Plus (Const 4) (Const 2)) (Const 7) in
      let prog := compile exp in
      progDenote prog nil.

  Lemma compile_extend : forall e p s,
      progDenote (compile e ++ p) s = progDenote p (expDenote e :: s).
  Proof.
    induction e; crush.
    (*
  induction e; simpl.
  - (* Const n *)
    auto.
  - (* Binop op e1 e2 *)
    intros; rewrite app_assoc_reverse, IHe2, app_assoc_reverse, IHe1; reflexivity.
*)
  Qed.

  Theorem compile_correct : forall e, progDenote (compile e) nil = Some (expDenote e :: nil).
  Proof.
    intro e; rewrite (app_nil_end (compile e)), compile_extend; reflexivity.
  Qed.
End stack_machine.

(* *****************************************************************************
   A Typed Interpreter
 **************************************************************************** *)

Section typed_interpreter.
  Inductive type : Set := Nat | Bool.

  Inductive tbinop : type -> type -> type -> Set :=
  | TPlus : tbinop Nat Nat Nat
  | TTimes : tbinop Nat Nat Nat
  | TEq : forall t, tbinop t t Bool
  | TLt : tbinop Nat Nat Bool.

  Inductive texp : type -> Set :=
  | TNConst : nat -> texp Nat
  | TBConst : bool -> texp Bool
  | TBinop : forall t1 t2 t, tbinop t1 t2 t -> texp t1 -> texp t2 -> texp t.

  Definition typeDenote (t : type) : Set :=
    match t with
    | Nat => nat
    | Bool => bool
    end.

  Definition tbinopDenote t1 t2 res (t : tbinop t1 t2 res)
    : typeDenote t1 -> typeDenote t2 -> typeDenote res :=
    match t with
    | TPlus => plus
    | TTimes => mult
    | TEq Nat => beq_nat
    | TEq Bool => eqb
    | TLt => leb
    end.

  Fixpoint texpDenote t (e : texp t) : typeDenote t :=
    match e with
    | TNConst n => n
    | TBConst b => b
    | TBinop _ _ _ op e1 e2 => tbinopDenote op (texpDenote e1) (texpDenote e2)
    end.

  Definition tstack := list type.

  Inductive tinstr : tstack -> tstack -> Set :=
  | TiNConst : forall s, nat -> tinstr s (Nat :: s)
  | TiBConst : forall s, bool -> tinstr s (Bool :: s)
  | TiBinop : forall t1 t2 res s, tbinop t1 t2 res -> tinstr (t1 :: t2 :: s) (res :: s).

  Inductive tprog : tstack -> tstack -> Set :=
  | TNil : forall s, tprog s s
  | TCons : forall s1 s2 s3, tinstr s1 s2 -> tprog s2 s3 -> tprog s1 s3.

  Fixpoint vstack (ts : tstack) : Set :=
    match ts with
    | nil => unit
    | t :: ts => typeDenote t * vstack ts
    end.

  Definition tinstrDenote ts1 ts2 (i : tinstr ts1 ts2) : vstack ts1 -> vstack ts2 :=
    match i with
    | TiNConst _ n => fun s => (n, s)
    | TiBConst _ b => fun s => (b, s)
    | TiBinop _ _ _ _ op =>
      fun s => let '(x, (y, s)) := s in (tbinopDenote op x y, s)
    end.

  Fixpoint tprogDenote ts1 ts2 (p : tprog ts1 ts2) : vstack ts1 -> vstack ts2 :=
    match p with
    | TNil _ => fun s => s
    | TCons _ _ _ i p => fun s => tprogDenote p (tinstrDenote i s)
    end.

  Fixpoint tconcat ts1 ts2 ts3 (p1 : tprog ts1 ts2) : tprog ts2 ts3 -> tprog ts1 ts3 :=
    match p1 with
    | TNil _ => fun p2 => p2
    | TCons _ _ _ i p1 => fun p2 => TCons i (tconcat p1 p2)
    end.

  Infix "+:+" := tconcat (right associativity, at level 60).

  Fixpoint tcompile t (e : texp t) (ts : tstack) : tprog ts (t :: ts) :=
    match e with
    | TNConst n => TCons (TiNConst _ n) (TNil _)
    | TBConst b => TCons (TiBConst _ b) (TNil _)
    | TBinop _ _ _ op e1 e2 =>
      tcompile e2 _ +:+ tcompile e1 _ +:+ TCons (TiBinop _ op) (TNil _)
    end.

  Lemma tconcat_correct :
    forall ts1 ts2 ts3 (p1 : tprog ts1 ts2) (p2 : tprog ts2 ts3) (s : vstack ts1),
      tprogDenote (p1 +:+ p2) s = tprogDenote p2 (tprogDenote p1 s).
  Proof. induction p1; crush. Qed.

  Hint Rewrite tconcat_correct.

  Lemma tcompile_extend : forall t (e : texp t) ts (s : vstack ts),
      tprogDenote (tcompile e ts) s = (texpDenote e, s).
  Proof. induction e; crush. Qed.

  Hint Rewrite tcompile_extend.

  Theorem tcompile_correct : forall t (e : texp t),
      tprogDenote (tcompile e nil) tt = (texpDenote e, tt).
  Proof. crush. Qed.
End typed_interpreter.