Section ilist.
  Variable A : Set.

  Inductive ilist : nat -> Set :=
  | Nil : ilist O
  | Const : forall n, A -> ilist n -> ilist (S n).
End ilist.