Structure group :=
  {
    G  :> Set ;
    
    op : G -> G -> G where "x * y" := (op x y) ;
    assoc : forall (x y z : G), x * (y * z) = (x * y) * z ;

    e : G ;
    left_id : forall (x : G), e * x = x ;

    inv : G -> G ;
    left_inv : forall (x : G), inv x * x = e ;
  }.

(* Force implicits *)
Arguments e {g}.
Arguments op {g} _ _.
Arguments inv {g} _.
Arguments left_id {g} _.
Arguments left_inv {g} _.
Notation "x * y" := (op x y).

Theorem unique_id {gr : group} :
  forall (x y : gr), x * y = x -> y = e.
Proof.
  intros x y prf.
  apply f_equal with (f := fun z => inv x * z) in prf.
  rewrite assoc, left_inv, left_id in prf.
  trivial.
Qed.

Theorem right_inv {gr : group} :
  forall (x : gr), x * inv x = e.
Proof.
  intro x.
  assert (prf : x * inv x = x * e * inv x).
  rewrite <- assoc, left_id; trivial.
  rewrite <- (left_inv x), assoc, <- assoc in prf.
  exact (unique_id (x * inv x) (x * inv x) (sym_eq prf)).
Qed.

Theorem right_id {gr : group} :
  forall (x : gr), x * e = x.
Proof.
  intro x.
  pose (prf := f_equal (fun z => z * x) (right_inv x)) ; simpl in prf.
  rewrite <- assoc, left_inv, left_id in prf.
  trivial.
Qed.

Theorem inv_sym {gr : group} :
  forall (x y : gr), x * y = e -> y * x = e.
Proof.
  intros x y r_i.
  apply f_equal with (f := fun z => inv x * z * x) in r_i.
  rewrite assoc, right_id, left_inv, left_id in r_i.
  trivial.
Qed.

Theorem inv_is_unique {gr : group} :
  forall (x y : gr), x * y = e -> y = inv x.
Proof.
  intros x y y_is_inv.
  apply f_equal with (f := fun z => inv x * z) in y_is_inv.
  rewrite assoc, left_inv, left_id, right_id in y_is_inv.
  trivial.
Qed.